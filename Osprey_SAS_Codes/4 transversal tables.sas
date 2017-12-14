/* **************************** CREATING TRANSVERSAL TABLES **************************** */

/* **************************** */
/* Policy Tables for Policy Sea */
/* **************************** */
%MACRO POLICY_PSEA;
	
	/* Keep only FG (excl. G400, SERVUNIT = FG) and (In Force or Cancelled - STATCODE = IF
	and CA) records from CHDRPF 											  			 */
	PROC SORT
		DATA = EXTRACT.CHDRPF(KEEP  = servunit	chdrnum	 currfrom	currto	   tranno	   chdrstcdc cnttype
							  		  cownnum	cnttype	 occdate	cntbranch  rnltype	   nofrisks
							  		  dtecan	reptype	 zrepolno	repnum	   statreasn  statcode
	      					  		  agntnum	ccdate	 crdate		zendno	   zrenno stattran campaign
							  WHERE = (servunit = "FG" AND statcode IN ("IF", "CA")))
		OUT = CHDRPF;
		
		/* Sort in this order to ensure most recent is kept   							  */
	    BY chdrnum DESCENDING zrenno 	/* Here we first put most recent POI at the top   */
			DESCENDING tranno;			/* Finally we sort on transaction number - normally
										   this should be enough for a sort, but this is 
										   not captured chronologically (each change can
										   be recorded at different times)				  */
	RUN;
	
	/* Remove duplicates by taking latest tranno of Policy, Period of Insurance (zrenno)  
	and dates of effect, thus simply keeping the most recent information FOR EACH policy 
	FOR EACH POI																		  */
	PROC SORT
		NODUPKEYS
		DATA = CHDRPF
		OUT  = CHDRPF2;
		BY chdrnum DESCENDING zrenno	/* NODUPKEYS is sorting according to the vars	  */
			DESCENDING tranno;			/* chdrnum, zrenno and tranno 					  */
										/* then "combines them" and removes duplicates 	  */
	RUN;								/* from this combined "key" (it removes in a      */
										/* decending order, hence keeping the first)  	  */
	
	/* Clean-up Fields */
	DATA TRANSV.POLHISTORY_PSEA;
		
		SET CHDRPF2;    /* Working with CHDRPF2 from above (in WORK library) 			  */
		
	    /* Dates conversion */
	    FORMAT D_from  D_to  D_oricom  D_com  D_exp  D_cancel date9.; /* Create new date fields  
															              with correct format  */
	    D_from   = convert_date(currfrom);
		D_to     = convert_date(currto);
		D_oricom = convert_date(occdate);
		D_com    = convert_date(ccdate); 
		D_exp   = convert_date(crdate);
		IF STATCODE='CA' THEN D_cancel = convert_date(dtecan); ELSE D_cancel = &errdate;
		
	    /* Field Renaming */
	    RENAME agntnum =  AGENTID  COWNNUM =  OWNERID;
		LABEL  agntnum = "AGENTID" COWNNUM = "OWNERID";
		
	    /* Renewability */
	    IF rnltype IN ("M2", "02") THEN
			renewable = 0;		/* Creates variable "renewable" in data table 		  */
		ELSE					/* and sets its value according to the				  */
			renewable = 1;		/* conditions mentioned in the statement			  */
		
	    /* Clean-Up Replacements */
	    /* Remove trailing blanks and keep the number only if:
	          1 REPTYPE is RF or RX    - replacements occur only when REPTYPE is RF or RX
	          2 REPNUM is not: 
					2 a) CHDRNUM, and
					2 b) Length of REPNUM is exactly 8 with
						   2 b   i) first character is a letter 
						   2 b  ii) all the rest are digits 
						   2 b iii) Careful with GTOM (Structure might be different)
		   Same conditions apply on ZREPOLNO						 					  */
		
		/* Replacing blanks */
		chdrnum  = COMPRESS(chdrnum,  " ", "p"); /* Using COMPRESS since it removes       */
	    repnum   = COMPRESS(repnum,   " ", "p"); /* blanks as well as other special       */
		zrepolno = COMPRESS(zrepolno, " ", "p"); /* character types with option p.        */
		
		/* Clear if condition 2 a (above) is met 										  */
	    IF repnum   = chdrnum THEN repnum   = "";
		IF zrepolno = chdrnum THEN zrepolno = "";
		
		/* Clear if condition 1 (above) is not met - if it is anyhing other than 		  */
	    IF reptype NOT IN ("RF", "RX", "CH") THEN 
			DO;
				repnum   = "";
				zrepolno = "";
			END;
		
		/* Clear if condition 2 b (above) is met */
	    IF repnum NE "" AND (LENGTH(repnum) NE 8                         OR 
		    				 SUBSTR(repnum, 1, 1) NOT IN ("P", "Q", "S") OR
		    				 ANYALPHA(repnum, 2) NE 0) THEN 
			DO;
				repnum = "";
			END;
		
		/* Same as above */
	    IF zrepolno NE "" AND (LENGTH(zrepolno) NE 8                         OR 
							   SUBSTR(zrepolno, 1, 1) NOT IN ("P", "Q", "S") OR 
							   ANYALPHA(zrepolno, 2) NE 0) THEN
			
			DO;
				zrepolno = "";
			END;
		/* Small note on the first condition in both IF statements above:
		   	If the condition on the repnum not being blank is omitted, then the code will  
			run but the log will generate many errors. Indeed, if you use the substr  
			function, you need to ensure that the characters you’re extracting exist  	  */
		
		DROP currfrom  currto  rnltype  occdate  ccdate  crdate  
			 dtecan    statreasn;
		
	RUN;
	
	/* Create Policy Header table */ 
	PROC SORT 
		NODUPKEYS 
		DATA = TRANSV.POLHISTORY_PSEA 
		OUT  = TRANSV.POL_PSEA(KEEP = chdrnum	D_oricom  D_cancel	 chdrstcdc  ownerid  agentid 
									  cnttype	zrenno	  cntbranch	 repnum);
		BY chdrnum;
	RUN;
	
%MEND;

/* ***************************** */
/* Premium Tables for Policy Sea */
/* ***************************** */
%MACRO PREMIUM_PSEA(monthend);
	/* 1) sacscode filter */
	/* Consider Gross Prem (FG) and Co-insurance Prem      */
	/* we will consider RP (Re-insurance premium) in  	   */
	/* the next section, since it only needs to run   	   */
	/* once a month - it is a massive table			       */
	/* 2) batctrcde filter */
	/* Only consider real financial transactions      	   */
	DATA ZTRNPF;
		FORMAT RLDGACCT $8.;
		SET EXTRACT.ZTRNPF(KEEP  = batcactyr			batcactmn	 rldgacct	tranno	  ccdate    effdate
								   accnum				expiry_date	 batctrcde	sacscode  trandate  chdrstcdc 
								   tranamt01-tranamt05
						   WHERE = (sacscode IN ("FG", "CO") AND 
									batctrcde IN ("T405", "T409", "T413", "TA39", "T495", "T454"))); 
		
		RENAME rldgacct =  chdrnum  accnum =  agentid;
		LABEL  rldgacct = "chdrnum" accnum = "agentid";
		
		/* Calculations */
		IF (sacscode = "FG") THEN
			DO;
				GWPtotal = (tranamt01 - tranamt03); 			/* Gross Prem 			   */
				GWCtotal = (tranamt04 + tranamt05); /* Gross Commission 	   */
				CWPtotal = 0;									/* Co-insurance Prem 	   */
				CWCtotal = 0;									/* Co-insurance Commission */
			END;
		ELSE
			DO;
				GWPtotal = 0; 			 						
				GWCtotal = 0;            		  				
				CWPtotal = tranamt01 - tranamt03; 				
				CWCtotal = tranamt04 + tranamt05; 	
			END;
		
		YrM = batcactyr * 100 + batcactmn;
		
		/* Dates */
		FORMAT D_tran D_eff D_com D_exp date9.;
		
		D_eff  = convert_date(effdate);
		D_com  = convert_date(ccdate);
		D_exp  = convert_date(expiry_date);
		D_tran = convert_date(trandate);
		
		/* Map the transaction types */
		SELECT (batctrcde);					 /* Should actually be mapped 				   */
			WHEN ("T405")
				trantype = "NWBS";           /* New Business 	 						   */
			WHEN ("T413") 
				trantype = "RNWL"; 			 /* Renewal			 						   */
			WHEN ("T409") 
				trantype = "ENDO"; 			 /* Endorsement 			 				   */
			WHEN ("T454") 
				trantype = "CANC"; 			 /* Cancelation		 						   */
			WHEN ("TA39")
				trantype = "REIN"; 			 /* Reinstatement	 						   */
			OTHERWISE 
				trantype = "OTHR";	 		 /* Other			 						   */
		END;
		
		/* Drop these fields from active table ZTRNPF in WORK */
		DROP batcactyr	batcactmn			  effdate	ccdate	expiry_date 
			 trandate	tranamt01-tranamt05;
		
	RUN;
		
	/* ****************************************************************** */
	/* ************************* FIRST GET BASE ************************* */ 
	/* ****************************************************************** */
	PROC SORT 
		NODUPKEYS
		DATA = ZTRNPF(WHERE = (sacscode = "FG")) 
		OUT  = BASE(DROP = GWPtotal  GWCtotal  CWPtotal  CWCtotal  sacscode); 
		BY chdrnum tranno;
	RUN;
	
	/* ****************************************************************** */
	/* ********************** THEN AGGREGATE ZTRNPF ********************* */ 
	/* ****************************************************************** */
	/* Sums GWPtotal GWCtotal CWPtotal CWCtotal by chdrnum and tranno     */
	/* Since we took only FG transactions the filter is already leaving   */
	/* only one unique chdrnum and tranno combination, this aggregation   */
	/* mainly used to ensure that there are only one combination.         */
	/* ****************************************************************** */

	/* NWAY is used to cross variables and 
	   MISSING to keep data when one level of one of the CLASS variables is missing        
       Hence it considers the missing data as a seperate level and will combine all of 
	   the policynumbers with corresponding missing trannos for instance and sum these
	   groups.																		       */
	
	PROC SUMMARY 
		NWAY MISSING
		DATA = ZTRNPF;				 
		CLASS chdrnum  tranno;					    /* to summarise by the group chdrnum and 
													   tranno (i.e. calculate the sum for the 
													   combined key chdrnum + tranno) 		   */
		VAR GWPtotal  GWCtotal  CWPtotal  CWCtotal; /* the variables that the statistics 
												       should be performed on 				   */
		OUTPUT 
			OUT = ZTRNPF(DROP = _:)  	 		    /* define the output, remove the SAS-created */
					SUM = ;						    /* variables (everything that starts with 
													   a "_") and the summary that should be 
													   performed is the sum 				   */
	RUN;
	
	/* ****************************************************************** */
	/* *********** GET DETAIL OF GROSS TRANSACTIONS IN PREMPF *********** */
	/* ****************************************************************** */
	DATA PREMPF;
		
		SET EXTRACT.PREMPF(KEEP = chdrno  rskno  tranno  premcl  extr01-extr05);
		
		RENAME chdrno =  chdrnum;
		LABEL  chdrno = "chdrnum";
		
		gwp = extr01 - extr03;					/* Getting */
		gwc = extr04 + extr05;			/*  GROSS  */
		
		DROP extr01-extr05;
		
	RUN;
	
	PROC SUMMARY 
		NWAY MISSING
		DATA = PREMPF;
		CLASS chdrnum  tranno  rskno  premcl;
		VAR gwp  gwc;
		OUTPUT 
			OUT = PREMPF(DROP = _:) 
					SUM = ;
	RUN;
	
	/* ****************************************************************** */
	/* ****** GET DETAIL OF CO-INSURANCE TRANSACTIONS IN RPRMPF_CO ****** */
	/* ****************************************************************** */
	DATA RPRMPF;
		
		SET EXTRACT.RPRMPF_CO(KEEP = chdrno  rskno  tranno  premcl  extr01-extr05);
		
		RENAME chdrno =  chdrnum;
		LABEL  chdrno = "chdrnum";
		
		cwp = extr01 - extr03;				/*    Getting   */
		cwc = extr04 + extr05;		/* CO_INSURANCE */
		
		DROP extr01-extr05;
		
	RUN;
	
	PROC SUMMARY 
		NWAY MISSING
		DATA = RPRMPF;
		CLASS chdrnum  tranno  rskno  premcl;
		VAR cwp  cwc;
		OUTPUT 
			OUT = RPRMPF(DROP = _:) 
					SUM = ;
	RUN;
	
	/* ****************************************************************** */
	/* ************** MERGE GROSS AND CO-INSURANCE TABLES *************** */
	/* ****************************************************************** */
	DATA COMBINED;
		MERGE PREMPF(IN = a) 
			  RPRMPF(IN = b);
			BY chdrnum  tranno  rskno  premcl;
		
		IF a;
		
		IF NOT b THEN
			DO;
				cwp = 0;
				cwc = 0;
			END;
	RUN;
	
	/* Now we do a small check (not needed each time) to see if the number
	   of rows in the original PREMPF containing the gross premium matches
	   at least with the new combined data. Hence the number of rows in
	   PREMPF should equal the new table COMBINED					      */
	
	/* Aggregate the Gross premium and commission on cdhrnum and tranno in
	   order to finally recon with ztrnpf 								  */
	PROC SUMMARY 
		NWAY MISSING
		DATA = COMBINED;
		CLASS chdrnum  tranno; /* Since PREMPF is more granual than ZTRNPF, 
								 this aggrigation should make it match with 
								 ZTRNPF 								  */
		VAR gwp  gwc  cwp  cwc;
		OUTPUT
			OUT = COMBINED_SUM(DROP = _:) 
					SUM = ;
	RUN;
	
	/* ***************** RECON ON GROSS SUB ACCOUNTS ***************** */
	/* Creating 2 tables:
	   WORK.RECON_GR          - This table will contain all the information 
					            will be used in the next step             */
	/* RECON.PR_GR_PSEA_RECON - This table will not contain the 
							    proportion info                           */
	DATA RECON_GR 
		 RECON.PR_GR_PSEA_RECON(DROP = propGWP  propGWC  propCWP  propCWC); 
		
		MERGE ZTRNPF      (IN = a) 
			  COMBINED_SUM(IN = b);
			BY chdrnum tranno;
		
		IF a; /* LEFT JOIN - a is binary. Thus it means if a then output  */
		
		IF NOT b THEN 
			DO;
				gwp = 0;
				gwc = 0;
				cwp = 0;
				cwc = 0;
			END;
		
		IF ABS(GWPtotal - gwp) > 0.01 OR   /* If any one of these         */
		   ABS(GWCtotal - gwc) > 0.01 OR   /* conditions are then only    */
		   ABS(CWPtotal - cwp) > 0.01 OR   /* should the output be        */
		   ABS(CWCtotal - cwc) > 0.01 THEN /* created (thus only when a   */
										   /* missmatch occurs)           */
			DO;
				/* Else condition exists for each if, since if the
				   error exists for one particular total, then it might
				   be dividing by 0 for some others */
				IF gwp NE 0 THEN 
					propGWP = GWPtotal / gwp;
				ELSE 
					propGWP = 0; 

				IF gwc NE 0 THEN
					propGWC = GWCtotal / gwc;
				ELSE
					propGWC = 0;

				IF cwp NE 0 THEN 
					propCWP = CWPtotal / cwp;
				ELSE 
					propCWP = 0;

				IF cwc NE 0 THEN 
					propCWC = CWCtotal / cwc;
				ELSE
					propCWC = 0;
			
				OUTPUT;
			END;

	RUN;
	
	/* Creating the final premium table and we are reallocating the portion 
	   of  the data that did not match */
	DATA TRANSV.PR_GR_PSEA;
		MERGE BASE    (IN = a) 
		      COMBINED(IN = b) 
		      RECON_GR(IN = c KEEP = chdrnum  tranno  propGWP  propGWC  propCWP  propCWC);
			BY chdrnum tranno;
		
		IF a AND b;
		
		IF c THEN 
			DO; 
				gwp = ROUND(gwp * propGWP, 0.01);
				gwc = ROUND(gwc * propGWC, 0.01);
				cwp = ROUND(cwp * propCWP, 0.01);
				cwc = ROUND(cwc * propCWC, 0.01);
			END;
		
		DROP propGWP  propGWC  propCWP  propCWC;
		
	RUN;
	/* The number of rows in the combination missmatch (between BASE and COMBINED) because 
	   we did not apply the same filters on COMBINED (such as the financial transactions) */
	
	%IF (&monthend = 1) %THEN
		%DO;
			DATA ZTRNPF_RI;
				
				SET EXTRACT.ZTRNPF(KEEP  = batcactyr			batcactmn    rldgacct	tranno	  ccdate    effdate
										   accnum			    expiry_date  batctrcde	sacscode  trandate
										   tranamt01-tranamt05
								   WHERE = (sacscode IN ("RP") AND /* Now Re-insurance prem (RP) */					
											batctrcde IN ("T405", "T409", "T413", "TA39", "T495", "T454")));
				
				RENAME rldgacct =  chdrnum  accnum = accntid;
				LABEL  rldgacct = "chdrnum" accnum = "accntid";
				
				/* Calculations */
				RWPtotal = (tranamt01 - tranamt03); 			/* Re-insurance Prem       */
				RWCtotal = (tranamt04 + tranamt05); /* Re-insurance Commission */
				
				YrM = batcactyr * 100 + batcactmn;
				
				/* Dates */
				FORMAT D_tran  D_eff  D_com  D_exp date9.;
				
				D_eff  = convert_date(effdate);
				D_com  = convert_date(ccdate);
				D_exp  = convert_date(expiry_date);
				D_tran = convert_date(trandate);
				
				/* Map the transaction types */
				SELECT (batctrcde);					 /* Should actually be mapped */
					WHEN ("T405") 
						trantype = "NWBS"; 
					WHEN ("T413")
						trantype = "RNWL"; 
					WHEN ("T409") 
						trantype = "ENDO"; 
					WHEN ("T454") 
						trantype = "CANC"; 
					WHEN ("TA39") 
						trantype = "REIN"; 
					OTHERWISE     
						trantype = "OTHR";	 
				END;
				
				/* Drop these fields from active table ZTRNPF in WORK */
				DROP batcactyr	batcactmn			  effdate	ccdate	expiry_date 
					 trandate	tranamt01-tranamt05;
				RUN;
				
			RUN;
			
			PROC SUMMARY 
				NWAY MISSING
	            DATA = ZTRNPF_RI;                
	            CLASS chdrnum  tranno  accntid;                                           
	            VAR RWPtotal  RWCtotal; 
	            OUTPUT 
		            OUT = ZTRNPF_RI(DROP = _:) 
							SUM = ;       
            RUN;                            
			
			/* ****************************************************************** */
			/* ***** GET DETAIL OF RE-INSURANCE TRANSACTIONS IN PREMPF_RP ******* */
			/* ****************************************************************** */
			DATA RPRMPF_RI;
				
				SET EXTRACT.RPRMPF_RP(KEEP = chdrno	 rskno	tranno	premcl	extr01-extr05 
											 racc	 ritype); 
				
				RENAME chdrno =  chdrnum  racc =  accntid;
				LABEL  chdrno = "chdrnum" racc = "accntid";
				
				rwp = extr01 - extr03;
				rwc = extr04 + extr05;
				
				DROP extr01-extr05;
				
			RUN;
			
			/* Re-insurance Premium information at a more granular level 
			   specifically by accntid AND ritype as well                       */
			PROC SUMMARY
				NWAY MISSING
				DATA = RPRMPF_RI;
				CLASS chdrnum  tranno  rskno  premcl  accntid  ritype;
				VAR rwp rwc;
				OUTPUT 
					OUT = RPRMPF_RI(DROP = _:) 
							SUM = ;	
			RUN;
			
			/* Above table aggregated in the same way as before in order to
			   reconcile with ZTRNPF_RI */
			PROC SUMMARY
				NWAY MISSING
				DATA = RPRMPF_RI;
				CLASS chdrnum tranno accntid; /* Match with ZTRNPF_RI "grouping" */ 
				VAR rwp rwc;
				OUTPUT
					OUT = RPRMPF_RI_SUM(DROP = _:) 
							SUM = ;
			RUN;
			
			/* ***************** RECON ON GROSS SUB ACCOUNTS ***************** */
			DATA RECON_RI 
				 RECON.PR_RI_PSEA_RECON(DROP = propRWP  propRWC); 
				
				MERGE ZTRNPF_RI    (IN = a) 
					  RPRMPF_RI_SUM(IN = b);
					BY chdrnum tranno accntid;
				
				IF a; 
				
				IF NOT b THEN 
					DO;
						rwp = 0;
						rwc = 0;
					END;
				
				IF ABS(RWPtotal - rwp) > 0.01 OR 
				   ABS(RWCtotal - rwc) > 0.01 THEN
					
					DO;
						IF rwp NE 0 THEN propRWP = RWPtotal / rwp;
							ELSE propRWP = 0;
						IF rwc NE 0 THEN propRWC = RWCtotal / rwc;
							ELSE propRWC = 0;
						OUTPUT;
					END;
			RUN;
						
			/* Creating the final premium table and we are reallocating the portion of 
			   the data that did not match */
			DATA TRANSV.PR_RI_PSEA;
				MERGE BASE     (IN = a) 
				   	  RPRMPF_RI(IN = b) 
				      RECON_RI (IN = c KEEP = chdrnum  tranno  propRWP  propRWC  accntid); 
					  /* ritype removed in RECON_RI*/
					BY chdrnum tranno;
				
				IF a AND b;
				
				IF c THEN 
					DO; 
						rwp = round(rwp * propRWP, 0.01);
						rwc = round(rwc * propRWC, 0.01);
					END;
				
				DROP propRWP  propRWC;
				
			RUN;
			
		%END;
	
%MEND;

%macro POLICY_G400;

/*******POLICY HISTORY from GCHIPF*******/
proc sort data=EXTRACT.GCHIPF(keep=chdrnum effdate ccdate crdate agntnum CNTBRANCH chdrstcda tranno) out=GCHIPF;
by chdrnum ccdate descending effdate descending tranno;
run;

data TRANSV.POLHISTORY_G400;set GCHIPF;format D_com D_exp D_eff date9.;
D_com=convert_date(ccdate);D_exp=convert_date(crdate);D_eff=convert_date(effdate);
rename CHDRSTCDA=FUNDCODE agntnum=AGENTID CNTBRANCH=BRANCHCODE;label CHDRSTCDA="FUNDCODE" agntnum="AGENTID" CNTBRANCH="BRANCHCODE";
drop ccdate crdate effdate;run;	

/******POLICY HEADER from GCHD*******/

data gchd;set EXTRACT.GCHD(keep=chdrnum cnttype statcode occdate cownnum effdcldt campaign);
format D_cancel D_oricom date9. SEGMENT $3.;
rename statcode=LATEST_STATUS cownnum=OWNERID;label statcode="LATEST_STATUS" cownnum="OWNERID";
D_cancel=convert_date(effdcldt);D_oricom=convert_date(occdate);SEGMENT="AIS";
drop occdate effdcldt;
proc sort nodupkeys;by chdrnum;run;

/******MAP INFORMATION FROM POLICY HISTORY TO POLICY HEADER*******/

proc sort nodupkeys 
	data=TRANSV.POLHISTORY_G400(keep=chdrnum D_eff D_com D_exp agentid fundcode tranno) 
	out=HEADER;
	by chdrnum; 
run;

data TRANSV.POL_G400;merge gchd(in=a) HEADER(in=b drop=D_eff D_com tranno);by chdrnum;if a;
rename D_exp=D_latestexpiry;label D_exp="D_latestexpiry";run;

/******IDENTIFY REPLACEMENT NUMBER FROM GCHPPF*******/
proc sort nodupkeys data=EXTRACT.GCHPPF(keep=chdrnum refno) out=GCHPPF;by chdrnum;run;

data GCHPPF;set GCHPPF;format REPNO $8.;
	if length(compress(refno))>=8 then do; /*exclude cases of IH inherited from GH*/
		if substr(compress(refno),1,8) ne chdrnum and substr(compress(refno),1,2) in ("G","P") then REPNO=substr(compress(refno),1,8);
		else REPNO="NA";
	end;
	else do;
		REPNO="NA";
	end;
drop refno;
run;


%mend;
%macro PREMIUM_G400;
/*********************************************/
/*											 /*
/*			   G400 TRANSACTIONS			 /*
/*											 /*
/*********************************************/

data ztrnpf;
	set EXTRACT.ZTRNPF(keep=batcactyr batcactmn rldgacct tranno ccdate accnum rdocnum CNTBRANCH chdrstcdc
	expiry_date chdrstcda crate cnttype chdrstcdc chdrstcda batctrcde sacscode origcurr acctcurr trandate tranamt01-tranamt10 
	where=(((sacscode='GR' and batctrcde in ('T405','T409','T413','T44B','T454','T903','T913','T922','T927','T928','T931','TA39','B920','T934')) or sacscode='GO')));

	format D_tran D_comz D_expz date9. TRANTYPE $4. RITYPE $3.;
	D_tran=convert_date(trandate);
	D_comz=convert_date(ccdate);
	D_expz=convert_date(expiry_date);
/*Map the transaction types*/
	select (batctrcde);
	when ('T903') TRANTYPE='NWBS';
	when ('T913','T926','T934') TRANTYPE='CANC';
	when ('T922','T921','T927') TRANTYPE='ENDO';
	when ('T928') TRANTYPE='RNWL';
	otherwise TRANTYPE='OTHR';
	end;
/*Type of REINSURANCE AGREEMENT*/
	/*if substr(accnum,3,3)='PPP' or accnum in ('04661','04662','04663','04664','04665','04666','04669','04670') then 
	RITYPE="PPP";else */RITYPE="OTH";

	if sacscode='GR' then do;
		GWPtotal=(tranamt01-tranamt03);
		GWCtotal=(tranamt04+tranamt05+tranamt10);
		RWPtotal=0;
		RWCtotal=0;
	end;
	if sacscode='GO' then do;
		RWPtotal=(tranamt01-tranamt03);
		RWCtotal=(tranamt04);
		GWPtotal=0;
		GWCtotal=0;
		accnum="NA";
	end;

	YrM=batcactyr*100+batcactmn;billno=rdocnum*1;
	rename rldgacct=CHDRNUM accnum=AGENTID;label rldgacct="CHDRNUM" accnum="AGENTID";
	drop batcactyr batcactmn expiry_date ccdate trandate rdocnum;
run;

proc summary nway missing
	data=ztrnpf;
	class chdrnum billno sacscode TRANTYPE RITYPE D_tran D_comz D_expz AGENTID YrM crate chdrstcda chdrstcdc CNTBRANCH origcurr;
	var GWPtotal GWCtotal RWPtotal RWCtotal ;
	output out=ztrnpf2(drop=_:) sum=;
run;

proc summary nway missing
	data=ztrnpf2; 
	class sacscode chdrnum billno;
	var GWPtotal GWCtotal RWPtotal RWCtotal ;
	output out=check_z(drop=_:) sum=;
run;

/************GET DETAIL OF GROSS TRANSACTIONS IN GPMDPF************/

data GPMDPF;
	set EXTRACT.GPMDPF(keep=batctrcd billno chdrnum HEADCNTIND prodtyp planno dpntno mbrno prmfrdt prmtodt 
	pprem pemxtprm poaxtprm  
	where=(billno ne 0 and batctrcd in ('T405','T409','T413','T44B','T454','T903','T913','T922','T927','T928','T931','TA39','T919','T921','T934')));

	/*Date Formatting*/
	format D_from D_to date9.;
		D_from=convert_date(prmfrdt);
		D_to=convert_date(prmtodt);
	/*Calculation*/
		GWP=pprem+pemxtprm+poaxtprm;

	drop batctrcd pprem pemxtprm poaxtprm prmfrdt prmtodt;
run;

proc summary nway missing
	data=GPMDPF;
	class chdrnum billno HEADCNTIND mbrno dpntno prodtyp planno D_from D_to;
	var GWP;
	output out=GPMDPF2(drop=_:) sum=;
run;

proc summary nway missing
	data=GPMDPF2 ;
	class chdrnum billno;
	var GWP;
	output out=check_p(drop=_:) sum=;
run;

/************CHECKS OF DISCREPANCIES BETWEEN ZNTRPF & GPMDPF************/

data RECON.PR_GR_G400_RECON;
	merge check_z(drop=GWCtotal RWPtotal RWCtotal where=(sacscode='GR') in=a) check_p;
	by chdrnum billno;
	if a;

	if abs(GWP-GWPtotal)>0.1 ;
	drop sacscode GWP GWPtotal;
run;

data GPMDPF2;
	merge GPMDPF2 RECON.PR_GR_G400_RECON(in=b);
	by chdrnum billno;
	if not b;
run;

/************MERGE GPMDPF WITH GBIDPF TO GET THE COMMISSION RATES************/
proc sort data=GPMDPF2;by billno prodtyp planno;run;

proc summary nway missing
	data=EXTRACT.gbidpf(keep=billno prodtyp planno bprem bextprm bcomm);
	class billno prodtyp planno;
	var bprem bextprm bcomm;
	output out=gbidpf(drop=_TYPE_ _FREQ_) sum=;
run;

data GPMDPF2;
	merge GPMDPF2(in=a) gbidpf(in=b);
	by billno prodtyp planno;
	if a;
	if not b then do;
		GWC=0;
	end;
	else do;
		if bprem+bextprm=0 then GWC=0;
		else GWC=round(bcomm/(bprem+bextprm)*GWP,0.01);
	end;
	drop bprem bextprm bcomm;
run;

/************FINAL MERGE FOR GR TRANSACTIONS************/

proc sort 
	data=GPMDPF2; 
	by chdrnum billno;
run;

proc sort 
	data=ztrnpf2(drop=RITYPE RWPtotal RWCtotal where=(sacscode="GR")) 
	out=GROSS_TRAN(drop=sacscode);
	by chdrnum billno;
run;

data GROSS_TRAN;
	merge GROSS_TRAN(in=a) GPMDPF2(in=b);
	by chdrnum billno;
	if a;

	/*Adjustment on missing transactions from GPMD*/
	if not b then do;
		GWP=GWPtotal;GWC=GWCtotal;
		mbrno='00';planno='000';dpntno='00';prodtyp="NA";
		D_from=D_comz; D_to=D_expz;
		HEADCNTIND="N";
	end;
	GWP=GWP*crate;
	GWC=GWC*crate;
	drop GWPtotal GWCtotal /*D_comz D_expz*/;
run;

/************Map Premium Classes************/

data TRANSV.PR_GR_G400;
	length chdrnum $8. billno D_tran D_from D_to D_comz D_expz 8. trantype $4. agentid $8. yrm crate 8. chdrstcda chdrstcdc $3. origcurr $3. 
	headcntind $1. mbrno $5. dpntno $2. prodtyp $4. planno cnttype $3. GWP GWC 8.;
	format D_tran d_from d_to D_comz D_expz date9.;

	if _n_=1 then do;
		/*POLICY HEADER INFO*/
		declare hash polquali(dataset:'transv.pol_g400');polquali.definekey('chdrnum');polquali.definedata('cnttype');
		polquali.definedone();call missing(cnttype);
	end;

	set GROSS_TRAN;

	rc=polquali.find();
drop rc cnttype;
run;

%mend;

%MACRO PILLAR1;
	
	/* PILLAR 1 - PSEA */
	PROC SUMMARY 
		NWAY 
		DATA = TRANSV.PR_GR_PSEA(KEEP  = chdrnum	yrm		 D_tran	 D_eff  D_com	tranno 
										 trantype	agentid	 gwp	 cwp	gwc		cwc
								 WHERE = (INT(yrm / 100) >= %EVAL(&acc_yr - 4))); /* past 4 years only */
		CLASS chdrnum   D_com  tranno  yrm  trantype  agentid;
		VAR gwp  gwc  cwp  cwc;
		OUTPUT
			OUT = PSEA(DROP = _:) 
				SUM = ;
	RUN;
	
	DATA PSEA;
		MERGE PSEA(IN = a) 
			  TRANSV.POL_PSEA(KEEP = chdrnum  orig_chd  D_oricom);
			BY chdrnum;
		
		IF a;
	RUN;
	
	/* Temp start - until replacements are fixed */
	DATA PSEA;
		SET PSEA;
		orig_chd = chdrnum;
	RUN;
	/* Temp end */
	
	DATA PSEA;
		SET PSEA;
		BY chdrnum D_com;
		
		FORMAT trantype2 $2.;
		RETAIN trantype2;
		
		/* If this is the FIRST record of the policy AND it has never replaced, 
		   THEN it is a NB (New Business), ELSE RE (Renewal) */
		IF FIRST.chdrnum THEN 
			DO;
				IF (chdrnum = orig_chd AND trantype = "NWBS") OR 
				   (chdrnum = orig_chd AND D_com    = D_oricom) THEN 
					
					trantype2 = "NB";
					
				ELSE 
					trantype2 = "RE";
			END;
			
		/* Else it is NOT the FIRST record, but the previous record was an NB AND 
			we are on the same date of inception THEN NB, ELSE RE */
		ELSE 
			DO;	
				IF trantype2 = "NB" AND 
				   (NOT FIRST.D_com) THEN 
					trantype2 = "NB";
				ELSE 
					trantype2 = "RE";
			END;
		
		/* Calculate AXA GWP */
		gwp = gwp - cwp;
		gwc = gwc - cwc;
		
		DROP cwp cwc;
		
	RUN;
	
	/* CONSOLIDATE PSEA */
	%LET VAR = chdrnum  yrm  agentid  trantype  trantype2  gwp 
			   gwc;
	
	DATA PILLAR1;
		SET PSEA(IN = a KEEP = &VAR);
		FORMAT id $4.;
		
		id = "PSEA";
		
		SELECT (trantype2);
			WHEN ("NB") 
				DO;
					all_gwp_nb = gwp;
					all_gwc_nb = gwc;
					
					IF trantype IN ("NWBS", "RNWL") THEN 
						nbpol_nb = 1; /* Other trantypes is not real new business */
				END;			  	  /* business but instead changes in policy   */
			
			WHEN ("RE")
				DO;
					all_gwp_re = gwp;
					all_gwc_re = gwc;
					
					IF trantype IN ("NWBS", "RNWL") THEN 
						nbpol_re = 1;
				END;
				
			OTHERWISE;
			
		END;
		
		DROP gwp trantype trantype2;
		
	RUN;
	
	PROC STDIZE 
		REPONLY MISSING = 0 
		DATA = PILLAR1
		OUT  = PILLAR1_PSEA;
		VAR all_gwp_nb   all_gwp_re   all_gwc_nb   all_gwc_re   nbpol_nb   nbpol_re;
	RUN;
		
	PROC SUMMARY 
		NWAY
		DATA = PILLAR1_PSEA;
		CLASS id  chdrnum  yrm  agentid;
		VAR all_gwp_nb   all_gwp_re   all_gwc_nb   all_gwc_re   nbpol_nb   nbpol_re;
		OUTPUT 
			OUT = TRANSV.P1(DROP = _:) 
				SUM = ;
	RUN;
	
%MEND;
/* **************************** */
/* Claims Tables for Policy Sea */
/* **************************** */

/* Goal is similar to the policy tables
   one table for the the header (on overview) as well as one with the history
   that contains all transactions on the claims side */
%MACRO CLAIMS_QUANTI_PSEA;
	
	/*----------------------------------------------------------*/
	/* 							PART I 							*/
	/*----------------------------------------------------------*/
	
	/* CMOVPF (can be seen as similar to ZTRNPF)
	   Description: Original CMOVPF with merge of some adjustments on the dates */
	DATA CMOVPF;
		/* The extract of clmpfx clmcoy is in fact not needed since it is always 
		   CL and 1. Therefore the filter is also not important, it is just so that
		   we are certain. Since CL is the only claim information we care about.
		   However, validflag fitlers out only valid claims.				    */
		SET EXTRACT.CMOVPF(KEEP  = clmpfx	  clmcoy			claim		tranno	batcactyr  batcactmn 
								   batctrcde  transaction_date  validflag	chgbal	paymnt	   chgbal_ri
	 							   paymnt_ri  clstat			paycde		clrate	reqnno	   receipt
						   WHERE = (clmpfx = "CL" AND clmcoy = "1" AND validflag = "1"));
		
		FORMAT D_tran  D_clo date9.;
		FORMAT paymntid $9.;
		
		/* Need to fix this since transaction_date is in a format different to
	       what we are used to. Normally, dates are in the format yyyymmdd, here
		   we have it as yymmdd. Also if it is in 2000 - 2009 then it is ymmdd   */
		D_tran = convert_date(convert_dateb(transaction_date));
		
		/* Take care on the D_clo - if the claims was never closed then this will 
		   be an errordate - also if the claim has been closed it can be re-opened 
		   and the date can change thus the date is never unique per claim       */
		IF clstat = 2 THEN /* If it is 2 then the claim is closed */
			D_clo = D_tran;
		ELSE
			D_clo = &errdate;
		
		yrm = batcactyr * 100 + batcactmn;
		
		/* Here we join the receipt and reqnno numbers (payments and receivable info). 
		   This is done so that we can find out (in CHEQPF) who the entity was that we 
		   paid or that we recieved payment from.
			
		   coalescec - first non missing argument this could either be a payment or a 
		   receipt and we take the first of which occurs, if neither is there then NA 
		   here we loose the information on whether it is a payment or a reciept since 
		   we join these 2 fields - on recovery the amounts will be negative */
		paymntid = COMPRESS(COALESCEC(reqnno, receipt, "NA"));
		
		/* Rename amounts */
		* gpaytot = paymnt * clrate;
		* gmovtot = chgbal * clrate;
		RENAME paymnt =  gpaytot  chgbal =  gmovtot  paymnt_ri =  rpaytot  chgbal_ri =  rmovtot;
		LABEL  paymnt = "gpaytot" chgbal = "gmovtot" paymnt_ri = "rpaytot" chgbal_ri = "rmovtot";
		
		DROP transaction_date	batcactyr	batcactmn  clmpfx  clmcoy  validflag 
			 batctrcde			reqnno		receipt;
		
	RUN;
	
	/* ****************************************************************** */
	/* ************************* FIRST GET BASE ************************* */ 
	/* ****************************************************************** */
	PROC SORT
		NODUPKEYS
		DATA = CMOVPF(DROP = gpaytot  gmovtot  rpaytot  rmovtot)
		OUT  = BASE;
		BY claim tranno;
	RUN;
	
	/* ****************************************************************** */
	/* ********************** THEN AGGREGATE CMOVPF ********************* */ 
	/* ****************************************************************** */
	PROC SUMMARY 
		MISSING NWAY
		DATA = CMOVPF;
		CLASS claim  tranno;
		VAR gpaytot  gmovtot  rpaytot  rmovtot;
		OUTPUT 
			OUT = CMOVPF_SUM(DROP = _:) 
					SUM = ;
	RUN;
	
	/*----------------------------------------------------------*/
	/* 							PART II							*/
	/*----------------------------------------------------------*/
	
	/* CORRECTION SECTION TO BE ADDED HERE (if needed) *************************** */
	/* Here if there are missmappings on the date - only date since the amounts    */
	/*  gets updated at the very end of the gross section and reinsurance sections */  
	/*  seperately and/or amount of a claim it needs to be corrected this is done  */ 
	/*  from the excel mapping file that needs to be imported 					   */
	/* *************************************************************************** */
	
	/* ****************************************************************** */
	/* ************ GET DETAIL OF GROSS CLAIM INFO IN CTRNPF ************ */
	/* ****************************************************************** */
	DATA CTRNPF;
		/* Again the fields clmpfx clmcoy (and the filter on them) is not really 
	 	   neccesary but it is good practice */
    	SET EXTRACT.CTRNPF(KEEP  = clmpfx	clmcoy  claim  tranno  prcl  rscd
								   paymnt	chgbal
						   WHERE = (clmpfx = "CL" AND clmcoy = "1"));
		
		/* Create key for mapping based on this concatenation */
		prcl_rscd = COMPRESS(prcl||rscd);
		
		RENAME paymnt =  gpay  chgbal =  gmov;
		LABEL  paymnt = "gpay" chgbal = "gmov";
		
	RUN;
	
	/* ************************* CREATE TEMPLATE ************************ */
	/* Get the unique claims only (on premium class breakdown) */
	PROC SORT 
		NODUPKEYS 
		DATA = CTRNPF(KEEP = claim  prcl_rscd) 
		OUT  = CLASSES;
		BY claim  prcl_rscd;
	RUN;
 	
	/* For each claim get all of the trannos */
	PROC SORT 
		NODUPKEYS 
		DATA = CMOVPF(KEEP = claim  tranno) 
		OUT  = TRANNOS;
		BY claim  tranno;
	RUN;
	
	/* Combine to create the TEMPLATE */
	PROC SQL;
		CREATE TABLE TEMPLATE AS
		SELECT t1.*, 
			   t2.prcl_rscd
		FROM TRANNOS AS t1 
			 LEFT JOIN CLASSES AS t2 ON (t1.claim = t2.claim);
	QUIT;
	
	PROC SORT
		DATA = TEMPLATE;
		BY claim  tranno  prcl_rscd;
	RUN;
	/* ****************************************************************** */
	
	/* Remove records which does not exist in CMOVPF 
	   since we need to recon with CMOVPF        */
	PROC SORT
		DATA = CTRNPF;
		BY claim tranno;
	RUN;
       
	DATA CTRNPF; 
		MERGE BASE  (IN = a KEEP = claim  tranno) 
			  CTRNPF(IN = b);
			BY claim  tranno;
		
		IF a;
		
		IF NOT b THEN 
			DO;
				gpay = 0;
				gmov = 0;
			END;
	RUN;
	
	/* Aggregate on granular level */
 	PROC SUMMARY 
		MISSING NWAY
		DATA = CTRNPF;
		CLASS claim  tranno  prcl_rscd;
		VAR gpay  gmov;
		OUTPUT 
			OUT = CTRNPF(DROP = _:) 
					SUM = ;
	RUN;
	
	/* ****************************************************************** */
	/* ******* COMPLETE TEMPLATE WITH GRANULAR CLAIM INFORMATION ******** */
	/* ****************************************************************** */
	DATA CTRNPF;
		MERGE TEMPLATE(IN = a) 
			  CTRNPF  (IN = b);
			BY claim  tranno  prcl_rscd;
		
		IF a;
		
		IF NOT b THEN
			DO;
				gpay = 0;
				gmov = 0;
			END;
	RUN;
	
	/* ****************************************************************** */
	/* ********* AGGREGATE GROSS CLAIM INFO IN CTRNPF FOR RECON ********* */
	/* ****************************************************************** */
	/* Aggregate granular level of CTRNPF for CMOVPF recon */
	PROC SUMMARY 
		MISSING NWAY
		DATA = CTRNPF;
		CLASS claim  tranno;
		VAR gpay  gmov;
		OUTPUT 
			OUT = CTRNPF_SUM(DROP   = _type_ 
							 RENAME = (_freq_ = nbkeys)) /* in Summary freq is automatically created and
							 								here we would like to keep it - for creating 
							 								the reinsurance granular amounts section 	*/ 
					SUM = ;
	RUN;

	/*----------------------------------------------------------*/
	/* 						  PART III							*/
	/*----------------------------------------------------------*/
	
	/* ****************************************************************** */
	/* *********************** RECONCILE THE DATA *********************** */
	/* ****************************************************************** */
	DATA RECON.CL_GR_PSEA_RECON;
		MERGE CMOVPF_SUM(IN = a DROP = rpaytot  rmovtot) 
			  CTRNPF_SUM(IN = b DROP = nbkeys);
			BY claim tranno;
		
		IF a;
		
		IF NOT b THEN 
			DO;
				gpay = 0;
				gmov = 0;
			END;
		
		IF ABS(gpay - gpaytot) > 0.01 THEN
			err_pay = 1;
		ELSE
			err_pay = 0;
		
		IF ABS(gmov - gmovtot) > 0.01 THEN 
			err_mov = 1;
		ELSE
			err_mov = 0;
		
		IF (err_pay = 1 OR err_mov = 1);
		
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART IV							*/
	/*----------------------------------------------------------*/
	
	/* ****************************************************************** */
	/* ****** AGGREGATE REINSURANCE CLAIM INFO IN LTRNPF FOR RECON ****** */
	/* ****************************************************************** */
	DATA LTRNPF;
		SET EXTRACT.LTRNPF(KEEP  = clmpfx	  clmcoy	   claim	tranno	lactyp
								   paymnt_ri  chgbal_ri
						   WHERE = (clmpfx = "CL" AND clmcoy = "1"));
		
		IF lactyp = "I" THEN /* For co-insurance */ 
			DO;
				cpay = paymnt_ri;
				cmov = chgbal_ri;
				
				rpay = 0;
				rmov = 0;
			END;
		ELSE
			DO;
				cpay = 0;
				cmov = 0;
				
				rpay = paymnt_ri;
				rmov = chgbal_ri;
			END;
		
		DROP clmpfx  clmcoy  lactyp  paymnt_ri  chgbal_ri;
		
	RUN;
	
	/* Remove records which don"t exist in CMOVPF 
	   since we need to recon with CMOVPF        */	
	PROC SORT
		DATA = LTRNPF;
		BY claim tranno;
	RUN;
	
	DATA LTRNPF; 
		MERGE BASE(IN = a KEEP = claim  tranno) 
			  LTRNPF(IN = b);
			BY claim tranno;
		
		IF a;
		
		IF NOT b THEN 
			DO;
				cpay = 0;
				cmov = 0;
				
				rpay = 0;
				rmov = 0;
			END;
	RUN;
	
	/* Aggregate on granular level - here it is claim tranno
	   since this is the most granular level (and we need it
	   more granular we will merge this later)              */
	PROC SUMMARY 
		MISSING NWAY
		DATA = LTRNPF;
		CLASS claim  tranno;
		VAR cpay  cmov  rpay  rmov;
		OUTPUT 
			OUT = LTRNPF_SUM(DROP = _:) 
					SUM = ;
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART V							*/
	/*----------------------------------------------------------*/
	
	/* ****************************************************************** */
	/* *********************** RECONCILE THE DATA *********************** */
	/* ****************************************************************** */
	/* Recon - can only recon reinsurance total and not split by RI and CO,
	   since in the CMOVPF table we do not have the information on the reinsurance type. 
	   What this means is that we dont have an rpaytot and a cpaytot in CMOVPF_SUM, since
	   we are unable to split in CMOVPF between CO in RI. */
	DATA RECON.CL_RI_PSEA_RECON;
		MERGE CMOVPF_SUM(IN = a DROP = gmovtot  gpaytot)
			  LTRNPF_SUM(IN = b);
			BY claim tranno;
		
		IF a;
		
		IF NOT b THEN 
			DO;
				cpay = 0;
				cmov = 0;
				
				rpay = 0;
				rmov = 0;
			END;
		
		IF ABS(cpay + rpay - rpaytot) > 0.01 THEN
			err_pay = 1;
		ELSE
			err_pay = 0;
		
		IF ABS(cmov + rmov - rmovtot) > 0.01 THEN 
			err_mov = 1;
		ELSE
			err_mov = 0;
		
		IF (err_pay = 1 OR err_mov = 1);
		
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART VI							*/
	/*----------------------------------------------------------*/
	/* Aggregate LTRNPF */
	/* Note on RETAIN - it  */
	DATA LTRNPF_SUM2;
		SET LTRNPF_SUM;
		BY claim tranno; /* Not neccesary - since it is already sorted in the correct order. But useful for reading/ */
		/* each time a retain is used you need to first sort the data in the appropriate direction */
		RETAIN cpay_cum_tranno  cmov_cum_tranno  rpay_cum_tranno  rmov_cum_tranno 0;
		
		IF FIRST.claim THEN
			DO;
				cpay_cum_tranno = cpay;
				cmov_cum_tranno = cmov;
				
				rpay_cum_tranno = rpay; 
				rmov_cum_tranno = rmov;
			END;
		ELSE 
			DO;
				cpay_cum_tranno = cpay_cum_tranno + cpay;
				cmov_cum_tranno = cmov_cum_tranno + cmov;
				
				rpay_cum_tranno = rpay_cum_tranno + rpay; 
				rmov_cum_tranno = rmov_cum_tranno + rmov;
			END;
		
		RENAME cpay =  cpay_aggr  cmov =  cmov_aggr  rpay =  rpay_aggr  rmov =  rmov_aggr;
		LABEL  cpay = "cpay_aggr" cmov = "cmov_aggr" rpay = "rpay_aggr" rmov = "rmov_aggr";
		
	RUN;
	
	/* Aggregate CTRNPF */
	DATA CTRNPF_SUM2;
		SET CTRNPF_SUM;
		BY claim  tranno;
		
		RETAIN gpay_cum_tranno  gmov_cum_tranno 0;
		
		IF FIRST.claim THEN
			DO;
				gpay_cum_tranno = gpay;
				gmov_cum_tranno = gmov;
			END;
		ELSE 
			DO;
				gpay_cum_tranno = gpay_cum_tranno + gpay;
				gmov_cum_tranno = gmov_cum_tranno + gmov;
			END;
		
		RENAME gpay =  gpay_aggr  gmov =  gmov_aggr;
		LABEL  gpay = "gpay_aggr" gmov = "gmov_aggr";
		
	RUN;
	
	/* Merge */
	DATA QUANTI_PSEA;
		MERGE CTRNPF     (IN = a)
			  CTRNPF_SUM2(IN = b)
			  LTRNPF_SUM2(IN = c);
			BY claim tranno;
		
		IF a OR c; /* Join on either gross or reinsurance */
		
	RUN;
	
	/* Use this instead of making a condition on each NUMERIC field that if it is . you make it 0 */
	PROC STDIZE
		REPONLY MISSING = 0
		DATA = QUANTI_PSEA
		OUT  = QUANTI_PSEA;
		VAR _NUMERIC_;
	RUN;
	
	PROC SORT
		DATA = QUANTI_PSEA;
		BY claim  prcl_rscd  tranno;
	RUN;
	
	/* Proportion the reinsurance to granularity */
	/* Cumulate on gpay and gmov since this is all you know up to now */
	DATA QUANTI_PSEA2;
		SET QUANTI_PSEA;
		BY claim  prcl_rscd  tranno; /* Retain by this */
		RETAIN gpay_cum_pr_rsv		gmov_cum_pr_rsv
			   tmp_cpay_cum_pr_rsv	tmp_cmov_cum_pr_rsv 
			   tmp_rpay_cum_pr_rsv	tmp_rmov_cum_pr_rsv 0;
		
		IF (FIRST.claim OR FIRST.prcl_rscd) THEN
			DO;
				gpay_cum_pr_rsv = gpay;
				gmov_cum_pr_rsv = gmov;
			END;
		ELSE
			DO;
				gpay_cum_pr_rsv = gpay_cum_pr_rsv + gpay;
				gmov_cum_pr_rsv = gmov_cum_pr_rsv + gmov;
			END;
		
		IF gpay_cum_tranno NE 0 THEN
			prop_pay = gpay_cum_pr_rsv / gpay_cum_tranno;
		ELSE
			DO;
				IF nbkeys NE 0 THEN
					prop_pay = 1 / nbkeys;
				ELSE
					prop_pay = 1;
			END;
		
		IF gmov_cum_tranno NE 0 THEN
			prop_mov = gmov_cum_pr_rsv / gmov_cum_tranno;
		ELSE
			DO;
				IF nbkeys NE 0 THEN
					prop_mov = 1 / nbkeys;
				ELSE
					prop_mov = 1;
			END;
		
		cpay_cum_pr_rsv = prop_pay * cpay_cum_tranno;
		cmov_cum_pr_rsv = prop_mov * cmov_cum_tranno;
		
		rpay_cum_pr_rsv = prop_pay * rpay_cum_tranno;
		rmov_cum_pr_rsv = prop_mov * rmov_cum_tranno;
		
		/* Get incremental for CO and RI */
		IF (FIRST.claim OR FIRST.prcl_rscd) THEN
			DO;
				
				tmp_cpay_cum_pr_rsv = cpay_cum_pr_rsv;
				tmp_cmov_cum_pr_rsv = cmov_cum_pr_rsv;
				
				tmp_rpay_cum_pr_rsv = rpay_cum_pr_rsv;
				tmp_rmov_cum_pr_rsv = rmov_cum_pr_rsv;
				
				cpay = cpay_cum_pr_rsv;
				cmov = cmov_cum_pr_rsv;
				
				rpay = rpay_cum_pr_rsv;
				rmov = rmov_cum_pr_rsv;
				
			END;
		ELSE
			DO;		
					
				cpay = cpay_cum_pr_rsv - tmp_cpay_cum_pr_rsv;
				cmov = cmov_cum_pr_rsv - tmp_cmov_cum_pr_rsv;
				
				rpay = rpay_cum_pr_rsv - tmp_rpay_cum_pr_rsv;
				rmov = rmov_cum_pr_rsv - tmp_rmov_cum_pr_rsv;
				
				/* ORDER is important */
				/* temp gets updated after it is subtracted    */
				/* if it is before the result will alwasy be 0 */
				tmp_cpay_cum_pr_rsv = cpay_cum_pr_rsv;
				tmp_cmov_cum_pr_rsv = cmov_cum_pr_rsv;
				
				tmp_rpay_cum_pr_rsv = rpay_cum_pr_rsv;
				tmp_rmov_cum_pr_rsv = rmov_cum_pr_rsv;
				
			END;
		
		IF LAST.claim OR LAST.prcl_rscd THEN
			DO;
				IF gmov_cum_pr_rsv = 0 AND rmov_cum_pr_rsv NE 0 THEN
					rmov_cmov_error_flag = 1;
			END;
		
		KEEP claim	tranno	prcl_rscd	gpay	gmov	cpay 
			 cmov	rpay	rmov; 
		
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART VII							*/
	/*----------------------------------------------------------*/
	PROC SORT
		DATA = QUANTI_PSEA2;
		BY claim tranno prcl_rscd;
	RUN;

	/* Finalize bu adding qualitative information */
	DATA CL_QUANTI_PSEA;
		MERGE BASE      (IN = a)
			QUANTI_PSEA2(IN = b);
			By claim  tranno;
		
		IF a;
	
	RUN;
	
	/*Manual Adjustments from Mapping File : Cases where Balo of CLAMPF doesn't reconcile with the sum of CHGBAL in CMOVPF*/
	PROC SORT 
		DATA = MAPPINGS.ACT_CLAIMGROSS_PSEA; 
		BY claim tranno prcl_rscd;
	RUN;

	DATA TRANSV.CL_QUANTI_PSEA;
		MERGE CL_QUANTI_PSEA(in=a) MAPPINGS.ACT_CLAIMGROSS_PSEA(keep=claim tranno prcl_rscd gpay2 gmov2);
		BY claim tranno prcl_rscd;
		if a;
		if gpay2 ne . then gpay=gpay2*1;
		if gmov2 ne . then gmov=gmov2*1;
		DROP gpay2 gmov2;
	RUN;

	
%MEND;
/* **************************** */
/* Claims Tables for Policy Sea */
/* **************************** */

%MACRO CLAIMS_QUALI_PSEA;
	
	/* ****************************************************************** */
	/* **************************** GET BASE **************************** */ 
	/* ****************************************************************** */
	/* Create a base for Quali - only consider */
	PROC SORT 
		DATA = TRANSV.CL_QUANTI_PSEA(KEEP = claim  D_tran) 
		OUT  = BASEQUALI;
		BY claim  D_tran;
	RUN;
	
	/* Keep first registration date only */
	PROC SORT 
		NODUPKEYS 
		DATA = BASEQUALI(RENAME = (D_tran = D_reg));
		BY claim;
	RUN;
	
	/* ****************************************************************** */
	/* ******************** CLEAN QUALITATIVE TABLES ******************** */ 
	/* ****************************************************************** */
	/* CLAMPF - claim header main info */
	/* normally each row should be unique by claim */
	PROC SORT 
		NODUPKEYS
		DATA = EXTRACT.CLAMPF(KEEP  = claim		chdrstcda		chdrstcdc	agntnum	  clntnum  chdrnum
									  datrep	datocc			rskno		cnttype	  rsktyp   clmdsc
									  cedref	zrepclmno		assess		assessdt  solict   solictdt
									  clrvwdat	user_profile	clstat		clcurr	  subrec   id
									  mevent	proc_tran_date	validflag                    
							  WHERE = (validflag = "1")) 
		OUT  = CLAMPF;
		BY claim;
	RUN;
	
	/* CLXDPF - Extra claim info */
	/* normally each row should be unique by claim */
	PROC SORT 
		NODUPKEYS 
		DATA = EXTRACT.CLXDPF(KEEP  = claim		 ccdate		dteeff		crdate	 acstyp	 desc01 
									  desc02	 zwcmtpd	zmperfac	zclmprd	 indpti	 assessrdt 
									  solictrdt	 validflag
							  WHERE = (validflag = "1"))  
		OUT  = CLXDPF;
		BY claim;
	RUN;
	
	/* Merge with base and clean variables */
	DATA CL_QUALI_PSEA;
		MERGE BASEQUALI(IN = a)
			  CLAMPF   (IN = b)
			  CLXDPF   (IN = c);
			BY claim;
		
		IF a;
		
		/* Format dates */
		FORMAT D_rep  D_occ date9.;
		
		D_rep = convert_date(datrep);
		D_occ = convert_date(datocc);
		
		/* Appointment, reported & review dates */
		FORMAT D_app_A  D_app_S  D_review date9.;
		
		/* Some date issues - assessdt should not exceed todays date */
		/* it can also not be before 1990-05-01, since AXA did not   */
		/* operate in Thailand before this date */
		IF assessdt > &cal_ymd OR assessdt < 19900501 THEN 
			assessdt = 0;
		
		D_app_A  = convert_date(assessdt);
		D_app_S  = convert_date(solictdt);
		D_review = convert_date(clrvwdat);
		
		/* Fund code, Agent ID, Claimant ID */
		RENAME chdrstcda =  fundcode  agntnum =  agentid  clntnum =  claimantid;
		LABEL  chdrstcda = "fundcode" agntnum = "agentid" clntnum = "claimantid";
		
		/* Format Claim description */
		clmdesc = LOWCASE(TRIM(clmdsc));
		
		/* Claim Handler */
		RENAME user_profile =  examiner;
		LABEL  user_profile = "examiner";
		
		RENAME id =  officerid;
		LABEL  id = "officerid";
		
		/* Solicitor & Assessor */
		FORMAT solicitor  assessor $10.;
		
		assessor  = COALESCEC(assess, "NA");
		solicitor = COALESCEC(solict, "NA");
		
		DROP datrep	 datocc	 clmdsc	 assessdt		 solictdt  clrvwdat 
			 assess	 solict	 proc_tran_date;
		
	RUN;

	DATA TRANSV.CL_QUALI_PSEA;
		LENGTH CLAIM $8. CLSTAT $1. LATEST_STATUS $7. NATCAT 8.;
			IF _N_=1 THEN DO;
				/*Claim Status*/
				DECLARE HASH MAPSTAT(DATASET:'MAPPINGS.SYS_CLSTAT(RENAME=(CLSTAT_LDESC=LATEST_STATUS))');
				MAPSTAT.DEFINEKEY('CLSTAT');
				MAPSTAT.DEFINEDATA('LATEST_STATUS');MAPSTAT.DEFINEDONE();CALL MISSING(LATEST_STATUS);
				/*Nat Cat*/
				DECLARE HASH MAPNC(DATASET:'MAPPINGS.ACT_NATCAT');
				MAPNC.DEFINEKEY('CHDRSTCDC','MEVENT');
				MAPNC.DEFINEDATA('NATCAT');MAPNC.DEFINEDONE();CALL MISSING(NATCAT);
			END;
		SET CL_QUALI_PSEA;
		RC = MAPSTAT.FIND();RC = MAPNC.FIND(); IF RC NE 0 THEN NATCAT=0;
		DROP RC;
	RUN;
	
%MEND;

%macro CLAIMS_QUALI_G400;

/*CREATE CLAIMS QUALITATIVE INFORMATION @ OCCURENCE LEVEL (from GCLHPF)*/

data GCLHPF;
	set EXTRACT.GCLHPF(keep=clamnum gcoccno clrate chdrnum clntnum dtevisit GCFRPDTE gcsts gcdiagcd
	DTEREG datime mbrno PROVORG dpntno prodtyp planno DTEKNOWN RECVD_DATE CLAIMCUR DTEDCHRG
	rename=(clamnum=claim gcsts=clstat CLAIMCUR=CLCURR));

	tranno=gcoccno*1;
	label claim="CLAIM" tranno="TRANNO" clstat="CLSTAT" CLCURR="CLCURR";
	rename PROVORG=PROVIDERID CLNTNUM=CLAIMANTID;label PROVORG="PROVIDERID" CLNTNUM="CLAIMANTID";

	/*Dates Formatting*/
	format D_tempreg D_regoc D_visit D_rep D_tran D_clo D_firstknown D_recvd D_discharged date9.;
	D_tempreg=convert_date(DTEREG);D_regoc=D_tempreg;
	D_visit=convert_date(dtevisit);D_rep=convert_date(GCFRPDTE);D_tran=datepart(datime);
	D_firstknown=convert_date(DTEKNOWN);D_recvd=convert_date(RECVD_DATE);D_discharged=convert_date(DTEDCHRG);

	YRM=year(D_tran)*100+month(D_tran);

	/*Closing Status*/
	if clstat in ('CC','CR','CV') then D_clo=D_tran; else D_clo=&errdate;

	drop gcoccno DTEREG dtevisit GCFRPDTE DTEKNOWN RECVD_DATE DTEDCHRG datime;
run;

proc sort
	data=GCLHPF;
	by claim D_tran D_tempreg;
run;

data GCLHPF;
	set GCLHPF;
	by claim;
	format D_reg date9.;retain D_reg;

	/*If date of registration = 31DEC2999 then take the first date of transaction as date of registration*/
	if first.claim then do;
		if D_tempreg ne &errdate then D_reg=D_tempreg;
	/*If we don't have the date of registration, the best proxy is date reported 
	Note: Time stamp here is a date of latest modification...isn't a good idea to use it as a proxy for the date of registration*/
		else D_reg=D_rep;
	end;

	D_regoc=min(D_regoc,D_tran);

	drop D_tempreg;
run;

proc sort nodupkeys
	data=GCLHPF;
	by claim tranno;
run;

data TRANSV.CL_QUALI_OCC_G400;
	length CLAIM $8. TRANNO 8.;
	set GCLHPF(drop=D_tran D_reg yrm);
run;

/*IN GCLHPF:
  - Unique variables per claim: chdrnum clntnum mbrno dpntno prodtyp 
  - Not unique: D_visit D_rep D_tran D_reg planno status*/
proc sort nodupkeys 
	data=GCLHPF(keep=claim chdrnum CLAIMANTID mbrno dpntno prodtyp planno PROVIDERID gcdiagcd 
	D_reg D_visit D_rep tranno D_firstknown D_recvd CLCURR) 
	out=CL_QUALI_G400(drop=tranno rename=(D_visit=D_occ));
	by claim; /*We set date of occurrence as first date of visit*/
run;

/*FETCH THE CORRESPONDING PERIOD OF INSURANCE + QUALITATIVE VARIABLES FROM GCHIPF*/
proc sort nodupkeys 
	data=CL_QUALI_G400(keep=chdrnum D_occ) 
	out=OCCDATES;
	by chdrnum D_occ;
run;

/*Get POI in GCHIPF as well as Qualitative Information (contract type, LOB, agentID...)*/

proc sql;
	create table OCCDATES2 as
		select t1.*, t2.agentid, t2.fundcode, t2.branchcode, t2.d_com, t2.d_eff, t2.d_exp, t3.cnttype, t3.ownerid
		from OCCDATES as t1 
			left join transv.POLHISTORY_G400 as t2 
			on (t1.chdrnum=t2.chdrnum and t2.D_eff<=t1.D_occ<=t2.D_exp)
			left join transv.POL_G400 as t3
			on t1.chdrnum = t3.chdrnum;
quit;

proc sort data=OCCDATES2;
	by chdrnum D_occ descending D_eff;
run;
proc sort nodupkeys
	data=OCCDATES2;
	by chdrnum D_occ;
run;

proc sort  
	data=CL_QUALI_G400 
	out=CL_QUALI_G400;
	by chdrnum D_occ;
run;

data TRANSV.CL_QUALI_G400;
	merge CL_QUALI_G400(in=a) OCCDATES2;
	by chdrnum D_occ;
	if a;
run;
proc sort 
	data=TRANSV.CL_QUALI_G400;
	by claim;
run;

%mend;
%macro CLAIMS_QUANTI_G400;

/*******************************GROSS RESERVES DATA*******************************/

proc summary nway missing
	data=EXTRACT.GCREPF(keep=clamnum seqno outresv resvdate rename=(clamnum=claim seqno=tranno outresv=gbal));
	class claim tranno resvdate;
	var gbal;
	output out=GCREPF(drop=_TYPE_ _FREQ_) sum=;
run;

/*******************************CONVERT GROSS RESERVES IN INCREMENTAL*******************************/
data GCREPF;
	set GCREPF;
	by claim;
	retain temp;
		if first.claim then do;
			temp=gbal;gmov=gbal;
		end;
		else do;
			gmov=gbal-temp;temp=gbal;
		end;
	drop gbal temp;
run;

/*******************************RIO RESERVES DATA*******************************/

proc summary nway missing 
	data=EXTRACT.RIREPF(keep=clamnum seqno outresv resvdate rename=(clamnum=claim seqno=tranno outresv=rbal)) ;
	class claim tranno resvdate;
	var rbal;
	output out=RIREPF(drop=_TYPE_ _FREQ_) sum=;
run;

/*******************************CONVERT RIO RESERVES IN INCREMENTAL*******************************/

data RIREPF;
	set RIREPF;
	by claim;
	retain temp;
		if first.claim then do;
			temp=rbal;rmov=rbal;
		end;
		else do;
			rmov=rbal-temp;temp=rbal;
		end;
	drop rbal temp;
run;

/*******************************COMBINE GROSS AND RIO RESERVES DATA*******************************/

data RESV_G400;
	merge GCREPF(in=a) RIREPF(in=b);
	by claim tranno resvdate;
	if a or b;
		if a and not b then rmov=0;
		if b and not a then gmov=0;

	format D_tran date9.;D_tran=convert_date(resvdate);
	label claim="CLAIM" tranno="TRANNO";
	yrm=year(D_tran)*100+month(D_tran);
drop resvdate;
run;



proc sort
	data=RESV_G400;
	by yrm;
run;

proc sort nodupkeys data=closing_days(keep=yrm Date_of_Closing) out=calendar;by yrm;run;

/*Assign an accounting year and month according to the transaction date*/
data RESV_G400;
	merge RESV_G400(in=a) calendar;
	by yrm;
	if a;
		if D_tran>Date_of_Closing then do; 
			if month(D_tran)=12 then YRM=yrm+100-11;
			else YRM=yrm+1;
		end;
drop Date_of_Closing;
run;

proc sort
	data=RESV_G400;
	by claim YRM tranno;
run;


/*******************************PAYMENTS DATA*******************************/

data PAY_G400;
	set EXTRACT.RTRNPF(keep=rldgacct batcactyr batcactmn ACCTAMT sacscode sacstyp chdrstcdd glsign datime rdocnum
	where=((sacscode='GO' & sacstyp='CR') or (sacscode='GC' and sacstyp='PC' and chdrstcdd='ALL')));

	format D_tran date9.;D_tran=datepart(datime);
	format CLAIM $8.;CLAIM=substr(rldgacct,1,8);
	TRANNO=substr(rldgacct,10,2)*1;
	rename rdocnum=PAYMNTID;label rdocnum="PAYMNTID";
	YRM=batcactyr*100+batcactmn;

	if glsign='-' then ACCTAMT=-ACCTAMT;

	if (sacscode='GO' & sacstyp='CR') then do;
		rpay=ACCTAMT;gpay=0;
	end;
	else do;
		gpay=ACCTAMT;rpay=0;
	end;
	drop rldgacct sacscode sacstyp batcactyr batcactmn ACCTAMT datime chdrstcdd glsign;
run;

proc sort
	data=PAY_G400;
	by claim YRM tranno;
run;

/*******************************COMBINE PAYMENTS & RESERVES DATA*******************************/

data QUANTI_G400;
	set RESV_G400(in=a) PAY_G400;
	format PAYMNTID $9.;
	if a then PAYMNTID="NA";
run;

proc stdize 
	data=QUANTI_G400 reponly missing=0 
	out=QUANTI_G400;
	var gpay gmov rpay rmov;
run;

proc summary nway missing;
	class claim YRM tranno D_tran PAYMNTID;
	var gpay gmov rpay rmov;
	output out=QUANTI_G400(drop=_TYPE_ _FREQ_) sum=;
run;

/*OTHER CHANGING FIELDS (DATE OF CLOSE, STATUS OF THE CLAIM & DATE REGISTRATION FOR EACH OC)*/

%let yr=&acc_yr;
proc sort data=GCLHPF(keep=claim YRM D_tran CLSTAT D_clo TRANNO) out=VISIONS;by YRM claim TRANNO;
data VISIONS;merge VISIONS(in=a) calendar;by yrm;if a;
if D_tran>Date_of_Closing then do; 
	if month(D_tran)=12 then YRM=yrm+100-11;
	else YRM=yrm+1;
end;
drop Date_of_Closing;
proc sort out=VISIONS(drop=D_tran TRANNO);by claim descending YRM descending D_tran descending TRANNO;
proc sort nodupkeys;by claim descending YRM;run;

data VISIONS;set VISIONS;by claim;retain tempyrm;
if first.claim then do;
	Ubound=%eval(&acc_yr+1)*100+1;LBound=YRM;tempyrm=YRM;
end;
else do;Ubound=tempyrm;LBound=YRM;tempyrm=YRM;end;
if last.claim then LBound=199401;
drop tempyrm yrm;
run;	

proc sql;
create table TRANSV.CL_QUANTI_G400 as
select t1.*,t2.CLSTAT,t2.D_clo
from QUANTI_G400 as t1 inner join VISIONS as t2 /*inner join because some claims don't exist in GCLHPF => we delete*/
on t1.claim=t2.claim and t2.LBound<=t1.YRM<t2.UBound;
quit;
proc sort ;by claim tranno;run;

/*Take the registration date for each GCOCCNO and merge it back into QUANTI (by tranno which is same as GCOCCNO for
payment transactions. If REGDATE=error date then correct it by being the first transaction date of this GCOCCNO*/
proc sort data=GCLHPF(keep=claim tranno D_regoc D_tran YRM) out=REGDATES(drop=YRM);by claim tranno YRM D_tran;
proc sort nodupkeys;by claim tranno;
data REGDATES;set REGDATES;if D_regoc=&errdate then D_regoc=D_tran;drop D_tran;

data TRANSV.CL_QUANTI_G400;merge TRANSV.CL_QUANTI_G400(in=a) REGDATES(in=b);by claim tranno;if a;
if not b then D_regoc=&errdate; 
run;

%mend;

%MACRO PILLAR2;
	
	/* ****************************************************************** */	
	/* ***************** Get policy by vision per policy **************** */
	/* ****************************************************************** */
	/* ******** Premium Section ******* */
	/* ******************************** */
	/* Aggregate Premium */
	PROC SUMMARY
		NWAY MISSING
		DATA = TRANSV.PR_GR_PSEA;
    	CLASS chdrnum  D_com  D_exp  trantype;
      	VAR   gwp  cwp;
      	OUTPUT
			OUT = PREMIUM(DROP = _:) 
				SUM = ;
	RUN;
	
	/* Split GWP */
	DATA PREMIUM;
		/* Flag the issuances */
		SET PREMIUM;
		
		gwp = gwp - cwp;
		
		SELECT (trantype);
			WHEN ("NWBS", "RNWL")
				DO;
					gwp_issu = gwp;
					gwp_canc = 0;
					gwp_rein = 0;
				END;
			
			WHEN ("CANC")
				DO; 
					gwp_issu = 0;
					gwp_canc = gwp;
					gwp_rein = 0;
				END;
			
			WHEN ("REIN")
				DO; 
					gwp_issu = 0;
					gwp_canc = 0;
					gwp_rein = gwp;
				END;
			
			OTHERWISE 
				DO;
					gwp_issu = 0;
					gwp_canc = 0;
					gwp_rein = 0;
				END;
		END;
		
	RUN;
	
	/* Horizontalize */
	PROC SUMMARY
    	NWAY MISSING
		DATA = PREMIUM;
    	CLASS chdrnum  d_com  d_exp;
    	VAR   gwp  gwp_issu  gwp_canc  gwp_rein;
    	OUTPUT 
			OUT = PREMIUM(DROP = _:)
				SUM = ;
	RUN;
	
	/* ******** Policy Section ******** */
	/* ******************************** */
	/* Filter */
	PROC SORT
		DATA = TRANSV.POLHISTORY_PSEA(KEEP = chdrnum  D_com  D_exp  zrenno  tranno  renewable agentid stattran)
		OUT  = POLHISTO;
      	BY chdrnum  D_com  D_exp   DESCENDING  tranno;
    RUN;
	
	/* Dedupelicate */
    PROC SORT
		NODUPKEYS
		DATA = POLHISTO;
    	BY chdrnum  D_com  D_exp;
    RUN;
	
	/* ************ Combine *********** */
	/* ******************************** */
    DATA COMBINE;
      	MERGE PREMIUM (IN = a) 
			  POLHISTO(IN = b);
      	BY chdrnum  D_com  D_exp;
      	IF a AND b;
    RUN;
	
	PROC SORT
		DATA = COMBINE;
		BY chdrnum  zrenno  tranno;
	RUN;
	
	/* ************** Base ************ */
	/* ******************************** */
	DATA BASE_PSEA;
		SET COMBINE;
		BY chdrnum  zrenno;
		RETAIN gwp_poi gwp_canc_poi gwp_rein_poi gwp_issu_poi 0;
		/* Calculate the GWP for each split */
		IF FIRST.chdrnum OR FIRST.zrenno THEN
			DO;	
				gwp_poi      = gwp;
				gwp_issu_poi = gwp_issu;
	            gwp_canc_poi = gwp_canc;
	            gwp_rein_poi = gwp_rein;
			END;
		ELSE
			DO;
				gwp_poi      = gwp_poi + gwp;
	            gwp_issu_poi = gwp_issu_poi + gwp_issu;
	            gwp_canc_poi = gwp_canc_poi + gwp_canc;
	            gwp_rein_poi = gwp_rein_poi + gwp_rein;
			END;
		/* Once end of grouping is reached - output */
		IF LAST.chdrnum OR LAST.zrenno; 
		/* Get true cancelation amount */
		gwp_canc_poi = gwp_canc_poi + gwp_rein_poi; 
		
		DROP tranno  gwp  gwp_issu  gwp_canc  gwp_rein  gwp_rein_poi;
		
	RUN;
	
	/* Merge between base and policy header to get additional information */
	DATA BASE_PSEA;
		MERGE BASE_PSEA      (IN = a)
		 	  TRANSV.POL_PSEA(IN = b  KEEP = chdrnum  chdrstcdc  cntbranch  cnttype  D_cancel);
			BY chdrnum;
		
		IF a;
	RUN;
	
	PROC SORT 
		DATA = BASE_PSEA;
		BY chdrnum DESCENDING zrenno;
	RUN;
	
	/* ****************************************************************** */	
	/* ************************ Prepare KPI Data ************************ */
	/* ****************************************************************** */
	DATA TRANSV.P2;
		SET BASE_PSEA;
		BY chdrnum DESCENDING zrenno;
		
		RETAIN new_gwp_ee  new_gwp_ie  annual_factor  nextagt  nextpol  nextline 
			   nextbranch  nextcnt;
		
		FORMAT new_branch                $2.
			   new_cnttype  new_line     $3. 
			   new_pol 	    new_agentid  $8.  
			   D_end 	                 date9.;
		
		abnormal = 0;
		
		/* Initialize Variables */
		/* Underwriting (UW) KPIs */
		pc_rnw     = 1; 						/* Policy renewed */
		gwp_rnw_ee = gwp_issu_poi;				/* GWP from renewed policies, excluding endorsements */
		gwp_rnw_ie = gwp_poi - gwp_canc_poi;	/* GWP from renewed policies, including endorsements */
		/* Initialize renewal information fully and will correct if not applicable in following section */
		pc_nbz     = 0; 						/* Policy new business (incepts for the first time) */
		gwp_nbz_ee = 0;							/* GWP from new policies, excluding endorsements */
		gwp_nbz_ie = 0;							/* GWP from new policies, including endorsements */
		
		/* Cancelations */
		pc_exp_rnabl_canc = 0;					/* Renewable policy cancelled   */
		s_rnabl_gwp_canc  = 0;					/* GWP of above - before expiry */
		
		pc_exp_nonrn_canc = 0;					/* Non-renewable policy cancelled */
		s_nonrn_gwp_canc  = 0;					/* GWP of above - before expiry */
		
		/* Expired */
		pc_exp_nonrn_expi = 0;					/* Policy Expired, wasn't Renewable */
		s_nonrn_gwp_expi  = 0;					/* GWP of above - before expiry */
		
		/* Lapse */
		pc_exp_rnabl_laps = 0;					/* Policy Expired, was Renewable, and Lapsed */
		s_rnabl_gwp_laps  = 0;					/* GWP of above - before expiry */
		
		/* Price effect */
		pc_exp_rnabl_renw   = 0;				/* Policy Expired, was Renewable, and was Renewed */
		gwp_renw_old_365    = 0;				/* Annualized GWP of renewed policies, including all endorsements just BEFORE the renewal */
		gwp_renw_new_ee     = 0;				/* GWP of renewed policies, excluding endorsements, just AFTER the renewal */
		gwp_renw_new_ie     = 0;				/* GWP of renewed policies, including endorsements, just AFTER the renewal */
		gwp_renw_new_365_ee = 0;				/* Annualized GWP of renewed policies, excluding endorsements, just AFTER the renewal */
		gwp_renw_new_365_ie = 0;				/* Annualized GWP of renewed policies, including endorsements, just AFTER the renewal */
		
		pc_exp_rnabl_repl   = 0;				/* Policy Expired, was Renewable, and was Replaced */
		gwp_repl_old_365    = 0;				/* Annualized GWP of replaced policies, including all endorsements just BEFORE the renewal */
		gwp_repl_new_ee     = 0;				/* GWP of replaced policies, excluding endorsements, just AFTER the renewal */
		gwp_repl_new_ie     = 0;				/* GWP of replaced policies, including endorsements, just AFTER the renewal */
		gwp_repl_new_365_ee = 0;				/* Annualized GWP of replaced policies, excluding endorsements, just AFTER the renewal */
		gwp_repl_new_365_ie = 0;				/* Annualized GWP of replaced policies, including endorsements, just AFTER the renewal */
		
		IF (FIRST.chdrnum) THEN /* When replacements are fixed we need to add here */
			DO;
				/* Cancel Section */
				/* Exposure (EXP) KPIs */
				IF (D_com <= D_cancel <= D_exp) THEN 
					DO; /* Cancelled contract */
						IF renewable = 1 THEN
							DO;
								pc_exp_rnabl_canc = 1;					    
								s_rnabl_gwp_canc  = gwp_poi - gwp_canc_poi;  						
							END;
						ELSE
							DO;
								pc_exp_nonrn_canc = 1;						 
								s_nonrn_gwp_canc  = gwp_poi - gwp_canc_poi;  /* GWP of above - before expiry   */
							END;
					END;
				
				/* Lapse Section */
				/* If the expiry date has been reached AND the policy has not cancelled,
				   THEN we consider the policy has lapsed OR succesfully expired if it is non-renewable.
				   Else, EXP KPIs will be 0 */
				IF D_exp <= INTNX("MONTH", MDY(&acc_mth, 1, &acc_yr), 0, "END") AND /* end of current month (exipery happened before today) */
				   (pc_exp_rnabl_canc = 0 AND pc_exp_nonrn_canc = 0) THEN /* Here lapse occurred (and it is not cancelled) */
					 
					DO;
						IF renewable = 1 THEN 
							DO;
								pc_exp_rnabl_laps = 1;
								s_rnabl_gwp_laps  = gwp_poi;
							END;
						ELSE
							DO;
								pc_exp_nonrn_expi = 1;
								s_nonrn_gwp_expi  = gwp_poi;	
							END;
					END;
				
				/* FIELDS TO BE RETAINED */
				new_gwp_ee = gwp_rnw_ee;
				new_gwp_ie = gwp_rnw_ie;
				
				annual_factor = 366.25 / (D_exp - D_com + 1); /* 366.25 since thailand cover starts and ends on the same day (not the day before) */
				
				/* Renewed */
				nextpol    = chdrnum;
				nextline   = chdrstcdc;
				nextbranch = cntbranch;
				nextcnt    = cnttype;
				nextagt    = agentid;
				
				/* Did not renew */
				new_pol     = "NA";
				new_line    = "NA";
				new_branch  = "NA";
				new_cnttype = "NA";
				new_agentid = "NA";
				
			END;
		ELSE /* Policy renewed or replaced */
			DO;
				IF renewable = 0 THEN 
					abnormal = 1;
				
				/* Update previous policy info to current (in iteration step) */
				new_pol     = nextpol;
				new_line    = nextline;
				new_branch  = nextbranch;
				new_cnttype = nextcnt;
				new_agentid = nextagt;
			
				IF chdrnum = nextpol THEN /* applicable only when replacements occur */
					DO; /* For renewals */
						pc_exp_rnabl_renw   = 1;
						gwp_renw_old_365    = gwp_poi * 366.25 / (D_exp - D_com + 1);
						gwp_renw_new_ee     = new_gwp_ee;
						gwp_renw_new_ie     = new_gwp_ie;
						gwp_renw_new_365_ee = new_gwp_ee * annual_factor;
						gwp_renw_new_365_ie = new_gwp_ie * annual_factor;
					END;
				ELSE 
					DO; /* For replacements */
						pc_exp_rnabl_repl   = 1;
						gwp_repl_old_365    = gwp_poi * 366.25 / (D_exp - D_com + 1);
						gwp_repl_new_ee     = new_gwp_ee;
						gwp_repl_new_ie     = new_gwp_ie;
						gwp_repl_new_365_ee = new_gwp_ee * annual_factor;
						gwp_repl_new_365_ie = new_gwp_ie * annual_factor;
					END;
				
				/* FIELDS TO BE RETAINED */
				new_gwp_ee = gwp_rnw_ee;
				new_gwp_ie = gwp_rnw_ie;
				
				annual_factor = 366.25 / (D_exp - D_com + 1); 
				
				nextpol    = chdrnum;
				nextline   = chdrstcdc;
				nextbranch = cntbranch;
				nextcnt    = cnttype;
				nextagt    = agentid;

			END;
		
		/* Last record - (sorted BY latest inception - first time step of policy journey) is NEW BUSINESS */
		IF (LAST.chdrnum) THEN 
			DO;
				/* UW KPIs */
				pc_rnw     = 0;
				gwp_rnw_ee = 0;
				gwp_rnw_ie = 0;
				
				pc_nbz     = 1;
				gwp_nbz_ee = gwp_issu_poi;
				gwp_nbz_ie = gwp_poi - gwp_canc_poi;
			END;
		
		/* Create date end, which is when the policy ended - both cancel and expire */
		IF (pc_exp_rnabl_canc = 0 AND pc_exp_nonrn_canc = 0) THEN 
			D_end = D_exp;
		ELSE 
			D_end = D_cancel;
		
		/* Calculation of various KPIs */
		gwp_rn_old_365     = gwp_renw_old_365 + gwp_repl_old_365;
		
		gwp_rn_new_ie      = gwp_renw_new_ie + gwp_repl_new_ie;
		gwp_rn_new_ee      = gwp_renw_new_ee + gwp_repl_new_ee;
		
		gwp_rn_new_365_ie  = gwp_renw_new_365_ie + gwp_repl_new_365_ie;
		gwp_rn_new_365_ee  = gwp_renw_new_365_ee + gwp_repl_new_365_ee;
		
		s_rnabl_gwp_renw   = pc_exp_rnabl_renw * (gwp_rnw_ie + gwp_nbz_ie);
		
		s_rnabl_gwp_repl   = pc_exp_rnabl_repl * (gwp_rnw_ie + gwp_nbz_ie);
		
		DROP new_gwp_ee			  new_gwp_ie		   annual_factor		nextpol			     nextline		  nextcnt 
			 nextagt 		  	  nextbranch		   gwp_poi			    gwp_issu_poi 	     /*gwp_canc_poi*/	  D_cancel		  
			 gwp_renw_old_365     gwp_repl_old_365	   gwp_renw_new_ie	    gwp_repl_new_ie	     gwp_renw_new_ee   gwp_repl_new_ee
			 gwp_renw_new_365_ie  gwp_repl_new_365_ie  gwp_renw_new_365_ee  gwp_repl_new_365_ee;
	RUN;
	
%MEND;

%MACRO PILLAR2_PIF;
	
	 /* TABLE OF QUALITATIVE INFORMATION FROM POLICY HEADER */
	DATA QUALI;
		SET TRANSV.POL_PSEA(KEEP = chdrnum  cnttype  chdrstcdc  cntbranch  ownerid);
	RUN;
	
	/* COMPUTE PIF FROM PILLAR 2 */
	/* Setting parameters */
	%LET interval  = MONTH;
	%LET class_var = chdrnum  agentid  cnttype  chdrstcdc  cntbranch;
	%LET start_pif = %SYSFUNC(MDY(1, 1, %EVAL(&acc_yr - 4))); /* 1 Jan for accounting year - 4 */
   	
	DATA _NULL_;
		CALL SYMPUT("end_pif",   INTNX("MONTH", MDY(&acc_mth, 1, &acc_yr), 0, "END"));
		CALL SYMPUT("start_pif", INTNX("MONTH", &start_pif, 0, "END"));
	RUN;
	
	/* Compute the number of steps required to RUN all months */
	DATA _NULL_;
		CALL SYMPUT("n", INTCK("&interval", &start_pif, &end_pif) + 1);
	RUN;
	
	%PUT &n &end_pif &start_pif;
	
	DATA PIF;
		SET TRANSV.P2(KEEP  = &class_var  gwp_nbz_ie  gwp_rnw_ie  D_com  D_exp  D_end  
					  WHERE = (D_com <= &end_pif AND D_end >= &start_pif));
	RUN;
	
	%LET PIF = ;
	
	%DO i = 1 %TO &n;
		
		DATA _NULL_;
			CALL SYMPUT("pif_date", INTNX("&interval", &end_pif, -%EVAL(&i - 1), "END"));
		RUN;
		
		%LET yrm = %EVAL(%SYSFUNC(YEAR(&pif_date)) * 100 + %SYSFUNC(MONTH(&pif_date)));
		
		DATA PIF&yrm;
			SET PIF(WHERE = (D_com <= &pif_date <= D_end));
			pif    = 1; /* all policies gets assigned as In Force */
			gw_pif = gwp_nbz_ie + gwp_rnw_ie;
			yrm    = &yrm;
			
			IF D_end NE D_exp AND D_end = &pif_date THEN /* 1) If D_end = D_exp then it means it is an expiration, hence no cancelation 2) consider cancelation only if D_end = &pif_date */
				DELETE;								 	 /* Remove the policies that should not be considered as in force */
			
			KEEP &class_var  yrm  D_com  D_exp  pif  gw_pif;
			
		RUN;
		
		%LET PIF = &PIF  PIF&yrm;
	
	%END;
	
	DATA TRANSV.P2_PIF;
		LENGTH cntbranch			$2.
			   cnttype 				$3.    
			   agentid 				$8.  
			   chdrnum  ownerid 	$8.  
			   chdrstcdc 			$30.;
				
		SET &PIF;
		
		IF _N_ = 1 THEN 
			DO;
				DECLARE HASH h1(DATASET: "QUALI");
				
				h1.DEFINEKEY("chdrnum");
				h1.DEFINEDATA("cntbranch", "chdrstcdc", "cnttype", "ownerid");
				h1.DEFINEDONE();
				
				CALL MISSING(cntbranch, chdrstcdc, cnttype, ownerid);
			END;
			
		rc = h1.FIND();
		
		DROP rc;
	RUN;
	
	PROC DELETE 
		DATA = &PIF;
	RUN;
	
%MEND;
%MACRO PILLAR3_PSEA;

/****************************************************************************************
*																						*
*																						*
*								P3 TABLE: GWP & RISK EXPOSURE 							*
*					(KEY: CHDRNUM, RSKNO, TRANNO, ZRENNO, D_from, RSKNO) 				*
*																						*
*																						*
****************************************************************************************/


/****************************************************************************************
*																						*
*					(1) SUMMARIZE PREMIUM TABLE BY RSKNO & TRANNO 						*
*																						*
****************************************************************************************/	

PROC SUMMARY 
	NWAY MISSING
	DATA=TRANSV.PR_GR_PSEA(WHERE=(YRM<=&acc_yrm));
	CLASS CHDRNUM TRANNO RSKNO D_eff D_exp;
	VAR GWP CWP GWC CWC;
	OUTPUT OUT=PREM(drop=_:) SUM=;
RUN;

/****************************************************************************************
*																						*
*		(2) INNER JOIN PREMIUM SUMMARY POLHISTORY DATES (MERGE KEY: CHDRNUM TRANNO) 	*
*																						*
****************************************************************************************/	

PROC SORT 
	DATA = TRANSV.POLHISTORY_PSEA(KEEP = CHDRNUM TRANNO ZRENNO D_cancel)
	OUT = POLHIST;
	BY CHDRNUM TRANNO;
RUN;

DATA PREM2;
	MERGE PREM(in=a) POLHIST;
	BY CHDRNUM TRANNO;
	IF A;
	GWP = GWP - CWP; 
	GWC = GWC - CWC;
RUN;

PROC SUMMARY NWAY MISSING
	DATA = PREM2;
	CLASS CHDRNUM RSKNO ZRENNO D_eff D_exp;
	VAR GWP GWC;
	OUTPUT OUT=PREM2(DROP=_: WHERE=( ABS(GWP)>0 )) SUM=;
RUN;

/********************************************************************************************************************
*																													*
*		(3) IN THE NEWLY MERGED TABLE, INDEX THE DIFFERENT PERIODS (CHDRNUM * RSKNO * ZRENNO * D_from * D_to) 		*
*									AND CALCULATE THE DAILY RATES FOR EACH PERIOD 									*
*																													*
********************************************************************************************************************/	

DATA PREM3;
	SET PREM2;
	BY CHDRNUM RSKNO ZRENNO;
	RETAIN PERIOD;
	DAILY_RATE_P = GWP / (D_exp - D_eff + 1); 
	DAILY_RATE_C = GWC / (D_exp - D_eff + 1);
		IF FIRST.CHDRNUM OR FIRST.RSKNO OR FIRST.ZRENNO THEN PERIOD = 1; ELSE PERIOD = PERIOD+1;
	DROP GWP GWC;
RUN;

/********************************************************************************************************************
*																													*
*	(4) TO SPLIT THE DIFFERENT PERIODS THE RIGHT AMOUNT OF TIMES, GET LIST OF ALL POSSIBLE 							*
*									EFFECTIVE DATES WITHIN EACH PERIOD 												*
*																													*																													*
********************************************************************************************************************/

DATA LISTDATES;
	SET PREM3(KEEP=CHDRNUM RSKNO ZRENNO D_eff D_exp);
	FORMAT D_temp DATE9.;
	D_temp = D_eff; OUTPUT LISTDATES;
	D_temp = D_exp; OUTPUT LISTDATES;
	DROP D_eff D_exp;
RUN;
PROC SORT NODUPKEYS 
	DATA=LISTDATES;
	BY CHDRNUM RSKNO ZRENNO D_temp;
RUN;

/*********************************************************************************
*																				 *
*	(5) GET THE NUMBER OF PERIODS FOR EACH POLICY x RISK PERIOD (ZRENNO). 		 *
*				WHEN THERE IS ONLY 1, WE WON'T SPLIT THE DATES. 				 *
*					   MAP THE COUNT TO THE LISTDATES TABLE 					 *
*																				 *																													*
*********************************************************************************/

DATA NBPERIOD; 
	SET PREM3(KEEP=CHDRNUM RSKNO ZRENNO PERIOD); 
	BY CHDRNUM RSKNO ZRENNO;
	PERIODCOUNT=PERIOD;
	IF LAST.CHDRNUM OR LAST.RSKNO OR LAST.ZRENNO; 
RUN;
DATA LISTDATES2;
	MERGE LISTDATES(IN=A) NBPERIOD(KEEP=CHDRNUM RSKNO ZRENNO PERIODCOUNT);
	BY CHDRNUM RSKNO ZRENNO;
	IF A;
RUN;

/*********************************************************************************
*																				 *
*	(6) IN THE PREMIUM SUMMARY TABLE, MAP THE DIFFERENT EFFECTIVE DATES ON 		 *
*		THE PERIODS WHERE THE TOTAL COUNT OF PERIODS PER ZRENNO IS MORE THAN 1 	 *
*	. 				(RQ: THIS WILL NATURALLY DUPLICATE THE RECORDS) 			 *
*																				 *
*********************************************************************************/

PROC SQL;
	CREATE TABLE PREM4 AS
	SELECT T1.*, T2.D_temp
	FROM PREM3 AS T1 LEFT JOIN LISTDATES2 AS T2
	ON T1.CHDRNUM=T2.CHDRNUM AND T1.RSKNO=T2.RSKNO AND T1.ZRENNO=T2.ZRENNO 
	AND (((T1.D_eff<T2.D_temp<T1.D_exp) AND PERIODCOUNT=1) OR ((T1.D_eff<=T2.D_temp<=T1.D_exp) AND PERIODCOUNT>1));
QUIT;

PROC SORT;
	BY CHDRNUM RSKNO ZRENNO PERIOD D_temp;
RUN;

/*********************************************************************************
*																				 *
*		(7) SPLIT THE RECORDS BY ALL THE DIFFERENT EFFECTIVE DATES 				 *
*						WHICH WERE MAPPED IN STEP 6 		 					 *
*																				 *
*********************************************************************************/

DATA PREM5;
	SET PREM4;
	BY CHDRNUM RSKNO ZRENNO PERIOD;
	RETAIN TEMPDATE;
	FORMAT D_from D_to DATE9.;
	IF FIRST.CHDRNUM OR FIRST.RSKNO OR FIRST.ZRENNO OR FIRST.PERIOD THEN DO;
		IF LAST.ZRENNO THEN DO;
			D_from = D_eff;  D_to = D_exp; OUTPUT;
		END;
		ELSE DO;
			D_from = D_eff; D_to = D_temp; OUTPUT;
			TEMPDATE = D_temp; 
		END;
	END;
	ELSE DO;
		IF D_temp = (TEMPDATE + 1) THEN DO; 
			D_from = D_temp; D_to = D_temp; OUTPUT;
		END;
		ELSE DO;
			D_from = TEMPDATE + 1; D_to = D_temp - 1; OUTPUT;
			D_from = D_temp; D_to = D_temp; OUTPUT;
		END;
		TEMPDATE = D_temp; 
	END;
	KEEP CHDRNUM RSKNO ZRENNO D_from D_to DAILY_RATE_P DAILY_RATE_C;
RUN;
PROC SUMMARY NWAY DATA=PREM5;
	CLASS CHDRNUM RSKNO ZRENNO D_from D_to;
	VAR DAILY_RATE_P DAILY_RATE_C;
	OUTPUT OUT=PREM5(DROP=_:) SUM=;
RUN;

/************************************************************************************
*																				  	*
*  (8) FOR EACH RECORD IN OUR NEW PREM5, WE'D LIKE A TRANNO TO BE ABLE TO MERGE   	*
*						WITH ANY RISK TABLE LATER ON.							  	*
*		WE FIND THE CORRESPONDING TRANNO IN RISKPF BASED ON THE EFFECTIVE DATES	  	* 
*  (D_from IN PREM5 MATCHING DTEEFF IN RISKPF (OR THE CLOSEST MATCH IN THE ZRENNO).	*
*				FOR THE ONES WHERE THERE IS NO POSSIBLE MATCHING TRANNO,			*
*						  WE OUTPUT THE RECORD IN ERROR TABLE.						*
*																				  	*
************************************************************************************/

PROC SORT 
	DATA = EXTRACT.RISKPF(KEEP=CHDRNO TRANNO RSKNO DTEATT DTEEFF DTETER DATIME RECFORMAT)
	OUT  = RISKPF(DROP=DATIME);
	BY CHDRNO TRANNO RSKNO DESCENDING DATIME;
RUN;
PROC SORT NODUPKEYS
	OUT  = RISKPF;
	BY CHDRNO TRANNO RSKNO;
RUN;

DATA POLHIST_RISK;
	MERGE RISKPF(IN=A) POLHIST(IN=B RENAME=(CHDRNUM=CHDRNO));
	BY CHDRNO TRANNO;
	IF A AND B;
	FORMAT D_starteff DATE9.;
	D_starteff = CONVERT_DATE(DTEEFF);
	KEEP CHDRNO RSKNO ZRENNO TRANNO D_starteff DTETER RECFORMAT;
RUN;

PROC SORT
	DATA = POLHIST_RISK; 
	BY CHDRNO RSKNO ZRENNO DESCENDING D_starteff DESCENDING DTETER DESCENDING TRANNO;
RUN;

/*We Map the closest transaction of RISKPF to our PREM TABLE. If not a perfect match (D_starteff NE D_from)
then we move on to the previous D_starteff.
We also map the lob and date of cancellation (useful when we calculate exposure) from Policy Header*/

DATA PREM6 ERROR;
	LENGTH CHDRNUM $8. RSKNO ZRENNO D_from D_to D_starteff D_cancel TRANNO 8. RECFORMAT $10.;
	FORMAT D_from D_to D_starteff D_cancel DATE9.;
	IF _N_=1 THEN DO;
		DECLARE HASH DATEMAP(dataset:'POLHIST_RISK(RENAME=(CHDRNO=CHDRNUM))', MULTIDATA:'Y');
	 	DATEMAP.DEFINEKEY('CHDRNUM','RSKNO','ZRENNO');
		DATEMAP.DEFINEDATA('D_starteff','TRANNO','RECFORMAT');
		DATEMAP.DEFINEDONE();
		CALL MISSING(D_starteff,TRANNO,RECFORMAT);
		DECLARE HASH CANCMAP(dataset:'TRANSV.POL_PSEA');
	 	CANCMAP.DEFINEKEY('CHDRNUM');
		CANCMAP.DEFINEDATA('D_cancel');
		CANCMAP.DEFINEDONE();
		CALL MISSING(D_cancel);
	END;
	SET PREM5;
	RC1 = DATEMAP.FIND();RC2 = CANCMAP.FIND(); 
	DO WHILE (RC1=0);
		IF D_from >= D_starteff THEN DO; OUTPUT PREM6; LEAVE; END;
		DATEMAP.HAS_NEXT(RESULT: HAVE_MORE);
		IF NOT HAVE_MORE THEN OUTPUT ERROR;
		RC1 = DATEMAP.FIND_NEXT();
	END;
KEEP CHDRNUM ZRENNO RSKNO D_from D_to D_cancel TRANNO DAILY_RATE_P DAILY_RATE_C;
RUN;

/**********************************************************************************
*																				  *
*  (9) FOR SEVERAL RECORDS, WE MAY NOT HAVE FOUND ANY POSSIBLE CANDIDATE 		  *
*						FOR A TRANNO WITHIN THE SAME POI. 						  *
*			WE THEN MAP THE INFO FROM THE FIRST RECORD OF POLHIST_RISK.			  *
*																				  *
**********************************************************************************/

PROC SQL; 
	CREATE TABLE ERROR2 AS
	SELECT T1.*, T2.TRANNO, T2.D_starteff
	FROM ERROR(DROP=TRANNO) AS T1 LEFT JOIN POLHIST_RISK AS T2
	ON T1.CHDRNUM=T2.CHDRNO AND T1.RSKNO=T2.RSKNO;
QUIT;

PROC SORT 
	DATA = ERROR2;
	BY CHDRNUM RSKNO ZRENNO D_from D_to DESCENDING D_starteff;
RUN;

PROC SORT NODUPKEYS 
	DATA = ERROR2
	OUT = ERROR2(DROP=D_starteff); 
	BY CHDRNUM RSKNO ZRENNO D_from D_to;
RUN;

/**********************************************************************************
*																				  *
* (10) FINAL TABLE IS THE CONCATENATION OF THE PREVIOUS AND THE CORRECTED ERRORS. *
*																				  *
**********************************************************************************/

DATA TRANSV.P3_PSEA; SET PREM6 ERROR2; RUN;
PROC SORT; BY CHDRNUM RSKNO ZRENNO D_from; RUN;


/*ACCURACY CHECKS: EXAMPLE ON 201606 YTD*/
/*

%LET Lbound = mdy(1,1,2016);
%LET Ubound = mdy(6,30,2016);
%LET Eyear  = mdy(12,31,2016);

DATA GEP201606_NEW; 
	SET TRANSV.P3_PSEA(WHERE=(D_from<=&UBound AND D_to>=&LBound));
	GEP_NEW = (min(D_to, &Ubound) - max(D_from, &Lbound) + 1) * DAILY_RATE_P;
	EXP_NEW = ((min(D_cancel, &Ubound) - max(D_from, &Lbound))*(D_from <= D_cancel <= D_to) 
		  + (min(D_to, &Ubound) - max(D_from, &Lbound) + 1)*(D_cancel > D_to)) / (&EYEAR - &LBOUND + 1);
RUN;
PROC SUMMARY NWAY 
	DATA=GEP201606_NEW;
	CLASS CHDRNUM RSKNO;
	VAR GEP_NEW EXP_NEW;
OUTPUT OUT=GEP201606_NEW_SUM(DROP=_:) SUM=;
RUN;

DATA GEP201606_OLD;
	MERGE TRANSV.PR_GR_PSEA(IN=A KEEP=CHDRNUM RSKNO D_eff D_exp yrm GWP CWP 
	WHERE=(D_exp>=&Lbound AND D_eff<=&Ubound AND yrm<=&acc_yrm)) 
	TRANSV.POL_PSEA(KEEP=CHDRNUM CHDRSTCDC);
	BY CHDRNUM;
	IF A;
	POI_DAYS = D_exp - D_eff + 1;
	GEP_OLD=(min(D_exp,&Ubound)-max(D_eff,&Lbound)+1)/POI_DAYS*(GWP-CWP);
RUN;
PROC SUMMARY NWAY 
	DATA=GEP201606_OLD;
	CLASS CHDRNUM RSKNO;
	VAR GEP_OLD;
OUTPUT OUT=GEP201606_OLD_SUM(DROP=_:) SUM=;
RUN;

DATA ERRORS; 
	MERGE GEP201606_OLD_SUM(IN=A) GEP201606_NEW_SUM(IN=B) ;
	BY CHDRNUM RSKNO; 
	IF (A OR B);
RUN;
*/

%MEND;
%MACRO PILLAR3_PSEA_MONTHLY;

	%let earn_start=%sysfunc(mdy(1,1,%eval(&acc_yr-4)));
	DATA _null_;call symput('earn_end',intnx("month",mdy(&acc_mth,1,&acc_yr),0,'end'));run;
	%let earn_start_yrm=%eval(%sysfunc(year(&earn_start))*100+%sysfunc(month(&earn_start)));
	%let earn_end_yrm=%eval(%sysfunc(year(&earn_end))*100+%sysfunc(month(&earn_end)));

	/*Compute the number of steps required to run all months*/
	%let interval=month;
	data _null_;call symput('n',intck("&interval",&earn_start,&earn_end)+1);run;

	%put &earn_start_yrm &earn_end_yrm &n;

	%LET PSEA=;

		%DO I=1 %to &N;
	
			DATA _null_;
				call symput('Ubound',intnx("&interval",&earn_end,-%eval(&i-1),'end'));
				call symput('Lbound',intnx("&interval",&earn_end,-%eval(&i-1),'beginning'));
			run;
			%LET YearLength = %sysfunc(mdy(12,31,%sysfunc(year(&Ubound)))) - %sysfunc(mdy(1,1,%sysfunc(year(&Ubound)))) + 1;

			%LET YRM=%eval(%sysfunc(year(&Ubound))*100+%sysfunc(month(&Ubound)));

			DATA PSEA&yrm;
				SET TRANSV.P3_PSEA(WHERE=(D_from<=&Ubound AND D_to>=&Lbound));
				GEP = (min(D_to, &Ubound) - max(D_from, &Lbound) + 1) * DAILY_RATE_P;
				GEC = (min(D_to, &Ubound) - max(D_from, &Lbound) + 1) * DAILY_RATE_C;
				EXP = ((min(D_cancel, &Ubound) - max(D_from, &Lbound))*(D_from <= D_cancel <= D_to)*(D_cancel > &Lbound) 
	  			+ (min(D_to, &Ubound) - max(D_from, &Lbound) + 1)*(D_cancel > D_to)) / %eval(&YearLength);
				RIF = (D_from <= D_cancel <= D_to)*(D_from <= &Ubound < D_cancel) + 
					  (D_cancel > D_to)*(D_from <= &Ubound <= D_to);
				YRM = &YRM;
			RUN;
			PROC SUMMARY NWAY MISSING DATA=PSEA&yrm;
			CLASS CHDRNUM RSKNO ZRENNO TRANNO YRM;
			VAR GEP GEC EXP RIF;
			OUTPUT OUT=PSEA&yrm(DROP=_:) SUM=;
			RUN;

		%LET PSEA=&PSEA PSEA&yrm;

		%END;	

		DATA TRANSV.P3_PSEA_MONTHLY; SET &PSEA; RUN;
		PROC DELETE DATA = &PSEA; 
RUN;

/****************************************
*										*
*		ADD PRODUCTION ALLOWANCE		*
*										*
****************************************/
/*
PROC IMPORT DATAFILE="C:\LOCAL\l-vie\Asia\SWAT Thaïland\Reporting\02 Input\PA 201301-201612.xlsx" OUT=PA_DATA
DBMS=EXCEL REPLACE; RUN;

PROC SUMMARY NWAY MISSING 
	DATA=PA_DATA;
	CLASS ACCOUNTING_PERIOD contract_type RISK_type agent_id;
	VAR Product_Allowance_Profit_commiss;
	OUTPUT OUT=PA_DATA_SUM(DROP=_:) SUM=;
RUN;

PROC SUMMARY NWAY MISSING 
	DATA=TRANSV.P3_PSEA_MONTHLY;
	CLASS YRM CNTTYPE RSKTYP AGENTID;
	VAR GEP;
	OUTPUT OUT=P3_SUM(DROP=_:) SUM=;
RUN;

PROC SQL;
	CREATE TABLE MAPPA AS
	SELECT T1.YRM, T1.CNTTYPE, T1.RSKTYP, T1.AGENTID, COALESCE(T2.Product_Allowance_Profit_commiss/GEP,0) AS PA_RATE
	FROM TEST AS T1 INNER JOIN PA_DATA_SUM AS T2
	ON T1.YRM=T2.ACCOUNTING_PERIOD AND T1.CNTTYPE=T2.contract_type AND T1.RSKTYP=T2.RISK_type AND T1.AGENTID=T2.AGENT_ID;
QUIT;

DATA TRANSV.P3_PSEA_MONTHLY;
	LENGTH CHDRNUM $8. RSKNO ZRENNO TRANNO 8. AGENTID $8. CHDRSTCDC CNTTYPE RSKTYP $3. YRM GEP GEC PA EXP RIF PA_RATE 8.;
	IF _N_=1 THEN DO;
		DECLARE HASH PRODALL(DATASET:'MAPPA');PRODALL.DEFINEKEY('YRM','CNTTYPE','RSKTYP','AGENTID');
		PRODALL.DEFINEDATA('PA_RATE');PRODALL.DEFINEDONE();CALL MISSING(PA_RATE);
	END;
	SET TRANSV.P3_PSEA_MONTHLY;
	RC = PRODALL.FIND();
	PA = COALESCE(PA_RATE*GEP,0);
	DROP PA_RATE RC;
RUN;*/

%MEND;
%MACRO PILLAR3_PSEA_12RM;

	/*PREPARE THE CLAIMS*/
	PROC SORT NODUPKEYS 
		DATA=TRANSV.CL_QUALI_PSEA(KEEP=CLAIM CHDRNUM RSKNO D_occ CHDRSTCDC NATCAT) 
		OUT=CLAIMQUALI;
		BY CLAIM;
	RUN;

	/*Get the Mapping of Trannos from P3*/
	PROC SORT NODUPKEYS 
		DATA = TRANSV.P3_PSEA(KEEP=CHDRNUM RSKNO D_from D_to ZRENNO TRANNO)
		OUT = TRANNOMAP;
		BY CHDRNUM RSKNO D_from;
	RUN;

	PROC SQL;
		CREATE TABLE CLAIMQUALI2 AS
		SELECT T1.CLAIM, T1.CHDRNUM, T1.RSKNO, T1.CHDRSTCDC, T1.NATCAT, (YEAR(T1.D_occ)*100+MONTH(T1.D_occ)) AS LOSS_YRM, 
		T2.ZRENNO, T2.TRANNO
		FROM CLAIMQUALI AS T1 INNER JOIN TRANNOMAP AS T2
		ON T1.CHDRNUM=T2.CHDRNUM AND T1.RSKNO=T2.RSKNO AND T2.D_from<=T1.D_occ<=T2.D_to;
	QUIT;
	PROC SORT NODUPKEYS
		DATA=CLAIMQUALI2;
		BY CLAIM;
	RUN;

	PROC SUMMARY NWAY MISSING
		DATA=TRANSV.CL_QUANTI_PSEA(KEEP=CLAIM YRM TRANNO D_tran CLSTAT GPAY GMOV);
		CLASS CLAIM TRANNO D_tran YRM CLSTAT;
		VAR GPAY GMOV;
		OUTPUT OUT=CLAIMQUANTI(DROP=_: RENAME=(TRANNO=TRANNO_CL)) SUM=;
	RUN;

	DATA CLAIM;
		MERGE CLAIMQUALI2(IN=A) CLAIMQUANTI(IN=B);
		BY CLAIM;
		IF A AND B;
	RUN;

	/*START OF LOOPS*/
	%LET P3_START=%sysfunc(mdy(1,1,%eval(&acc_yr-2)));
	%LET P3_END=%sysfunc(mdy(&acc_mth,1,&acc_yr));

	/*Compute the number of steps required to run all months*/
	%LET interval=month;
	data _null_;call symput('n',intck("&interval",&P3_start,&P3_end)+1);run;

	%LET PREMIUM=; %LET CLAIM=;

		%DO I=1 %to &N;

		DATA _null_;
			call symput('vision',intnx("&interval",&P3_end,-%eval(&i-1),'end'));
			call symput('Ubound',intnx("&interval",&P3_end,-%eval(&i-1)-2,'end'));
			call symput('Lbound',intnx("&interval",&P3_end,-%eval(&i-1)-13,'beginning'));
		run;

		%let vision_yrm=%eval(%sysfunc(year(&vision))*100+%sysfunc(month(&vision)));
		%let Ubound_yrm=%eval(%sysfunc(year(&Ubound))*100+%sysfunc(month(&Ubound)));
		%let Lbound_yrm=%eval(%sysfunc(year(&Lbound))*100+%sysfunc(month(&Lbound)));

		%put &vision_yrm &Ubound_yrm &Lbound_yrm;

		/*12 RM PREMIUM FROM P3_MONTHLY*/
		PROC SUMMARY NWAY MISSING 
			DATA=TRANSV.P3_PSEA_MONTHLY(WHERE=(&Lbound_yrm <= YRM <= &Ubound_yrm));
			CLASS CHDRNUM RSKNO ZRENNO TRANNO;
			VAR GEP GEC EXP RIF;
			OUTPUT OUT=TEMPPR(DROP=_:) SUM=;
		RUN;
		DATA PREMIUM&vision_yrm; SET TEMPPR; YRM=&vision_yrm; RUN;

		/*12 RM CLAIMS - SPLIT BETWEEN ATTRITIONAL/CATNAT/LARGE + EXCLUDE CLAIMS CLOSED @ NIL FROM THE COUNT*/
		DATA TEMPCL; 
			SET CLAIM(WHERE=(YRM<=&VISION_YRM AND (&Lbound_yrm <= LOSS_YRM <= &Ubound_yrm)));
			BY CLAIM TRANNO_CL D_tran;
			RETAIN GPAYULT GMOVULT;
			IF FIRST.CLAIM THEN DO;
				GPAYULT=GPAY;GMOVULT=GMOV;
			END;
			ELSE DO;
				GPAYULT=GPAYULT+GPAY;GMOVULT=GMOVULT+GMOV;
			END;
			IF LAST.CLAIM;
		RUN;
		DATA TEMPCL; 
			LENGTH CLAIM $8. CHDRSTCDC $3. THRESHOLD 8.;
			IF _N_=1 THEN DO;
				DECLARE HASH MAPLC(DATASET:'MAPPINGS.ACT_LARGECLAIM'); MAPLC.DEFINEKEY('CHDRSTCDC');MAPLC.DEFINEDATA('THRESHOLD');
				MAPLC.DEFINEDONE();CALL MISSING(THRESHOLD);
			END;
			SET TEMPCL;
			RC=MAPLC.FIND();
				IF NATCAT = 1 THEN DO;
					GINC_NC = GPAYULT+GMOVULT; GINC_L = 0; GINC_A = 0;
					NBCLAIM_NC = 1; NBCLAIM_L = 0; NBCLAIM_A = 0; 
				END;
				ELSE IF (THRESHOLD NE . AND (GPAYULT+GMOVULT)>THRESHOLD) THEN DO;
					GINC_NC = 0; GINC_L = GPAYULT+GMOVULT; GINC_A = 0;
					NBCLAIM_NC = 0; NBCLAIM_L = 1; NBCLAIM_A = 0;
				END;
				ELSE DO;
					GINC_NC = 0; GINC_L = 0; GINC_A = GPAYULT+GMOVULT;
					NBCLAIM_NC = 0; NBCLAIM_L = 0; NBCLAIM_A = 1;
				END;
			IF CLSTAT='2' AND ABS(GPAYULT+GMOVULT)<0.01 THEN DO; 
				NBCLAIM_NC = 0; NBCLAIM_L = 0; NBCLAIM_A = 0; 
			END;
			YRM = &VISION_YRM; 
			KEEP CHDRNUM RSKNO ZRENNO TRANNO YRM GINC_A GINC_L GINC_NC NBCLAIM_A NBCLAIM_L NBCLAIM_NC;
		RUN;
		PROC SUMMARY NWAY MISSING;
			CLASS CHDRNUM RSKNO ZRENNO TRANNO YRM;
			VAR GINC_A GINC_L GINC_NC NBCLAIM_A NBCLAIM_L NBCLAIM_NC;
			OUTPUT OUT=CLAIM&vision_yrm(DROP=_:) SUM=;
		RUN;

		%LET PREMIUM=&PREMIUM PREMIUM&vision_yrm;
		%LET CLAIM=&CLAIM CLAIM&vision_yrm;

		%END;	

		DATA P3_12RM;
			SET &PREMIUM &CLAIM;
		RUN;

		PROC STDIZE REPONLY MISSING=0
			OUT=P3_12RM;
			VAR _NUMERIC_;
		RUN;
		
		PROC SUMMARY NWAY MISSING;
			CLASS CHDRNUM RSKNO ZRENNO TRANNO YRM;
			VAR GEP GEC EXP RIF GINC_A GINC_L GINC_NC NBCLAIM_A NBCLAIM_L NBCLAIM_NC;
			OUTPUT OUT=P3_PSEA_12RM(DROP=_:) SUM=;
		RUN;
		
		PROC DELETE DATA=&PREMIUM &CLAIM; RUN;


DATA TRANSV.P3_PSEA_12RM;
LENGTH CHDRNUM $8. TRANNO RSKNO 8. CNTTYPE CHDRSTCDC $3. AGENTID $8. CAMPAIGN $6. CHDRSTCDC_LDESC $30. AGENT_NAME $50. CHANNEL $20.;
LENGTH LOB $35. AGENT $60.;
		IF _N_=1 THEN DO;
			/*Policy Details from Transv.POLHISTORY_PSEA*/
			DECLARE HASH POLMAP(DATASET:'TRANSV.POLHISTORY_PSEA'); POLMAP.DEFINEKEY('CHDRNUM','TRANNO'); 
			POLMAP.DEFINEDATA('D_com','CNTTYPE','CHDRSTCDC','AGENTID','CAMPAIGN');
			POLMAP.DEFINEDONE(); CALL MISSING(D_com, CNTTYPE, CHDRSTCDC, AGENTID, CAMPAIGN);
			/*Lob Mapping based on CHDRSTCDC*/
			DECLARE HASH LOBMAP(DATASET:'MAPPINGS.SYS_CHDRSTCDCPSEA'); LOBMAP.DEFINEKEY('CHDRSTCDC'); 
			LOBMAP.DEFINEDATA('CHDRSTCDC_LDESC');
			LOBMAP.DEFINEDONE(); CALL MISSING(CHDRSTCDC_LDESC);
			/*Intermediary Mapping from Finance*/
			DECLARE HASH DCMAP(DATASET:'MAPPINGS.ACT_INTERM(RENAME=(AGENT_CODE=AGENTID))'); DCMAP.DEFINEKEY('AGENTID'); 
			DCMAP.DEFINEDATA('AGENT_NAME','CHANNEL');
			DCMAP.DEFINEDONE(); CALL MISSING(AGENT_NAME,CHANNEL);
		END;
	SET P3_PSEA_12RM;
	RC = POLMAP.FIND(); RC = LOBMAP.FIND(); RC = DCMAP.FIND();
	LOB=COMPBL(CHDRSTCDC||"-"||CHDRSTCDC_LDESC);
	AGENT=COMPBL(AGENT_NAME||"-"||AGENTID);
	UW_YEAR=YEAR(D_com);
	DROP RC CHDRSTCDC CHDRSTCDC_LDESC D_com AGENT_NAME AGENTID;
RUN;


%MEND;

%MACRO RISK_G400;

/*MEMBERS TABLE*/
%macro risk_dates_members(inp_table,out_table,measure);

proc sort 
	data=&inp_table 
	out=temp(keep=chdrnum mbrno dpntno prodtyp planno dteatt effdate dtetrm EMLOAD &measure);
	by chdrnum mbrno dpntno prodtyp planno EMLOAD descending effdate descending tranno;
run;

data &out_table;
	set temp;
	by chdrnum mbrno dpntno prodtyp planno EMLOAD descending effdate;
	retain prevdate;

	format D_from D_to date9.;

	if (first.chdrnum or first.mbrno or first.dpntno or first.prodtyp or first.planno or first.EMLOAD) then do;
		if not (last.chdrnum or last.mbrno or last.dpntno or last.prodtyp or last.planno or first.EMLOAD) then prevdate=max(effdate,dteatt);
		D_from=convert_date(max(effdate,dteatt));
		if dtetrm not in (99999999,0) then D_to=convert_date(dtetrm)-1;else D_to=&errdate;
		if D_to<D_from then delete;
	end;
	else do;
		D_to=convert_date(prevdate)-1;
		D_from=convert_date(max(effdate,dteatt));
		if prevdate<=effdate then delete;else prevdate=effdate;
	end;
drop dteatt effdate dtetrm prevdate;
run;

%mend;

%risk_dates_members(extract.gxhipf(where=(headno="")),gxhipf,);

/*MAP QUALI INFO FROM THE POLICY AND REMOVE PERIODS WHICH HAVE NO GWP*/

proc summary nway missing
	data=TRANSV.PR_GR_G400(keep=chdrnum D_from D_to gwp);
	class chdrnum D_from D_to;
	var gwp;
	output out=EFFECTIVE_DATES(drop=_: where=(round(gwp,0.01) ne 0)) sum=;
run;

proc summary nway missing
	data=EFFECTIVE_DATES;
	class chdrnum;
	var D_from D_to;
	output out=EFFECTIVE_DATES2(drop=_:) min(D_from)=D_eff max(D_to)=D_exp;
run;

data GXHIPF2;
length CHDRNUM SUBSNUM $8. MBRNO $5. DPNTNO $2. CLNTNUM $8.;
length D_eff D_exp 8;
format D_eff D_exp date9.;
length CNTTYPE $3.;
if _n_=1 then do;
     declare hash h1(dataset:'EFFECTIVE_DATES2');
	 h1.definekey('CHDRNUM');
	 h1.definedata('D_eff','D_exp');
	 h1.definedone();
	 call missing(D_eff,D_exp);
	 declare hash h2(dataset:'EXTRACT.GMHDPF');
	 h2.definekey('CHDRNUM','MBRNO','DPNTNO');
	 h2.definedata('CLNTNUM');
	 h2.definedone();
	 call missing(CLNTNUM);
	 declare hash h3(dataset:'TRANSV.POL_G400');
	 h3.definekey('CHDRNUM');
	 h3.definedata('CNTTYPE');
	 h3.definedone();
	 call missing(CNTTYPE);
end;
  set GXHIPF;
  rc=h1.find();
  if rc=0 then do;
  	if D_eff>D_to or D_exp<D_from then delete;
  	if D_from<=D_exp<D_to then D_to=D_exp;
	if D_from<D_eff<=D_to then D_from=D_eff;
  end;
  rc=h2.find();
  rc=h3.find();
  if rc=0 then do;HEADCNTIND='M';subsnum="NA";LIVES=1;if DPNTNO="00" then EMPLOYEES=1;else EMPLOYEES=0;output;end;
  rename CLNTNUM=MEMBERID;;label CLNTNUM="MEMBERID";
drop rc D_eff D_exp;
run;


proc sql;
	create table TRANSV.MEMBERS_G400(where=(D_starteff<=D_endeff) drop=D_from D_to) as
	select t1.*,t2.AGENTID,t2.D_exp,t2.D_eff,
	max(t1.D_from,t2.D_eff) as D_starteff format=date9.,
	min(t1.D_to,t2.D_exp) as D_endeff format=date9.
	from gxhipf2 as t1 inner join TRANSV.POLHISTORY_G400(keep=chdrnum AGENTID D_eff D_exp) as t2
	on t1.chdrnum=t2.chdrnum and (t2.D_eff<=t1.D_from<=t2.D_exp);
quit;

%mend;


*%POLICY_PSEA;
*%PREMIUM_PSEA(&monthend);
*POLICY_G400;
*%PILLAR1;
*%CLAIMS_QUANTI_PSEA;
*%CLAIMS_QUALI_PSEA;
*%CLAIMS_QUALI_G400;
*%CLAIMS_QUANTI_G400;
*%PILLAR2;
*%RISK_PSEA;
*%PILLAR2_PIF;
*%PILLAR3_PSEA;
*%PILLAR3_PSEA_MONTHLY;
%PILLAR3_PSEA_12RM;
*%RISK_G400;

