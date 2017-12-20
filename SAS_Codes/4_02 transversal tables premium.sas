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
		LENGTH rldgacct $8.;
		SET EXTRACT.ZTRNPF(KEEP  = batcactyr			batcactmn	 rldgacct	tranno	  ccdate    effdate
								   accnum				expiry_date	 batctrcde	sacscode  trandate  chdrstcdc 
								   tranamt01-tranamt05  batcbrn
						   WHERE = (sacscode  IN ("FG",   "CO") AND 
									batctrcde IN ("T405", "T409", "T413", "TA39", "T495", "T454"))); 

		RENAME rldgacct =  chdrnum   accnum =  agentid ;
		LABEL  rldgacct = "chdrnum"  accnum = "agentid";
	RUN;
	/*Batch Month adjustment 201702*/
	PROC SORT DATA =ZTRNPF;
		BY CHDRNUM TRANNO;
	RUN;

	PROC SORT DATA = mappings.act_ztrn_adj OUT = ZTRN_ADJ;
		BY CHDRNUM TRANNO;
	RUN;

	DATA ZTRNPF;
		MERGE ZTRNPF(IN = a) ZTRN_ADJ;
		BY CHDRNUM TRANNO;
		IF a;
	RUN;

	DATA ZTRNPF;
		SET ZTRNPF;	
		/* Initialize */
		gwptotal = 0;    /* Gross Prem 			    */
		gwctotal = 0;    /* Gross Commission 	    */
		cwptotal = 0;    /* Co-insurance Prem 	    */
		cwctotal = 0;    /* Co-insurance Commission */

		/* Calculations */
		IF (sacscode = "FG")    THEN    DO;    gwptotal = tranamt01 - tranamt03;    gwctotal = tranamt04 + tranamt05;    END;
		ELSE                            DO;    cwptotal = tranamt01 - tranamt03;    cwctotal = tranamt04 + tranamt05;    END;
		
		yrm = batcactyr * 100 + batcactmn;
		
		/* Dates */
		FORMAT d_tran  d_eff  d_com  d_exp    date9.;
		
		d_tran = convert_date(trandate);
		d_eff  = convert_date(effdate);
		d_com  = convert_date(ccdate);
		d_exp  = convert_date(expiry_date);
			
		/* Drop these fields from active table ZTRNPF in WORK */
		DROP batcactyr  batcactmn  effdate  ccdate  expiry_date 
			 trandate   tranamt01-tranamt05;
	RUN;
	
	/* Map Transaction Types */
	/* Note - the mapping is limited to the filter, so the mapping is only needed for the
	   transaction types applied in the filter */
	PROC SQL;
   		CREATE TABLE ZTRNPF AS 
   		SELECT t1.*, 
               t2.trantype
      		FROM ZTRNPF t1
           		LEFT JOIN MAPPINGS.ACT_TRANTYPE t2 ON 
					(t1.batctrcde = t2.batctrcde);
	QUIT;
	
	/* ****************************************************************** */
	/* ************************* FIRST GET BASE ************************* */ 
	/* ****************************************************************** */
	PROC SORT 
		NODUPKEYS
		DATA = ZTRNPF(WHERE = (sacscode = "FG")) 
		OUT  = BASE(DROP = gwptotal  gwctotal  cwptotal  cwctotal  sacscode); 
		BY chdrnum  tranno;
	RUN;
	
	/* ****************************************************************** */
	/* ********************** THEN AGGREGATE ZTRNPF ********************* */ 
	/* ****************************************************************** */
	/* Sums gwptotal gwctotal cwptotal cwctotal by chdrnum and tranno     */
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
		CLASS chdrnum  tranno;					    /* to summarise by the group chdrnum and tranno (i.e. calculate the sum for the combined key chdrnum + tranno) */
		VAR gwptotal  gwctotal  cwptotal  cwctotal; /* the variables that the statistics should be performed on */
		OUTPUT 
			OUT = ZTRNPF(DROP = _:)  	 		    /* define the output, remove the SAS-created */
				SUM = ;						   	    /* variables (everything that starts with a "_") and the summary that should be performed is the sum */
	RUN;
	
	/* ****************************************************************** */
	/* *********** GET DETAIL OF GROSS TRANSACTIONS IN PREMPF *********** */
	/* ****************************************************************** */
	DATA PREMPF;
		SET EXTRACT.PREMPF(KEEP = chdrno  rskno  tranno  premcl  extr01-extr05);
		
		RENAME chdrno =  chdrnum;
		LABEL  chdrno = "chdrnum";
		
		gwp = extr01 - extr03;    gwc = extr04 + extr05;    /* Getting gross */
		
		DROP extr01-extr05;
	RUN;
	
	PROC SUMMARY 
		NWAY MISSING
		DATA = PREMPF;
		CLASS chdrnum  tranno  rskno  premcl;
		VAR   gwp  gwc;
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
		
		cwp = extr01 - extr03;    cwc = extr04 + extr05;    /*Getting CO INSURANCE */
		
		DROP extr01-extr05;
	RUN;
	
	PROC SUMMARY 
		NWAY MISSING
		DATA = RPRMPF;
		CLASS chdrnum  tranno  rskno  premcl;
		VAR   cwp  cwc;
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
		
		IF NOT b    THEN    DO;    cwp = 0;    cwc = 0;    END;
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
				gwp = 0;    gwc = 0;
				cwp = 0;    cwc = 0;
			END;
		
		IF ABS(gwptotal - gwp) > 0.01 OR   /* If any one of these         */
		   ABS(gwctotal - gwc) > 0.01 OR   /* conditions are then only    */
		   ABS(cwptotal - cwp) > 0.01 OR   /* should the output be        */
		   ABS(cwctotal - cwc) > 0.01 THEN /* created (thus only when a   */
										   /* missmatch occurs)           */
			DO;
				/* Else condition exists for each if, since if the
				   error exists for one particular total, then it might
				   be dividing by 0 for some others */
				IF gwp NE 0    THEN    propGWP = gwptotal / gwp;    ELSE    propGWP = 0; 
				IF gwc NE 0    THEN    propGWC = gwctotal / gwc;    ELSE    propGWC = 0;
				IF cwp NE 0    THEN    propCWP = cwptotal / cwp;    ELSE    propCWP = 0;
				IF cwc NE 0    THEN    propCWC = cwctotal / cwc;    ELSE    propCWC = 0;
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
				gwp = ROUND(gwp * propGWP, 0.01);    gwc = ROUND(gwc * propGWC, 0.01);
				cwp = ROUND(cwp * propCWP, 0.01);    cwc = ROUND(cwc * propCWC, 0.01);
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
										   tranamt01-tranamt05  batcbrn
								   WHERE = (sacscode  IN ("RP") AND /* Now Re-insurance prem (RP) */					
											batctrcde IN ("T405", "T409", "T413", "TA39", "T495", "T454")));
				
				RENAME rldgacct =  chdrnum   accnum =  accntid ;
				LABEL  rldgacct = "chdrnum"  accnum = "accntid";
				
				/* Calculations */
				rwptotal = (tranamt01 - tranamt03); /* Re-insurance Prem       */
				rwctotal = (tranamt04 + tranamt05); /* Re-insurance Commission */
				
				yrm = batcactyr * 100 + batcactmn;
				
				/* Dates */
				FORMAT d_tran  d_eff  d_com  d_exp   date9.;
				
				d_tran = convert_date(trandate);
				d_eff  = convert_date(effdate);
				d_com  = convert_date(ccdate);
				d_exp  = convert_date(expiry_date);
				
				/* Drop these fields from active table ZTRNPF in WORK */
				DROP batcactyr  batcactmn            effdate  ccdate  expiry_date 
					 trandate   tranamt01-tranamt05;
				RUN;
			RUN;
			
			/* Mapping transaction type */
			PROC SQL;
			 	CREATE TABLE ZTRNPF_RI AS 
			  	SELECT t1.*, 
			           t2.trantype
			     	FROM ZTRNPF_RI t1
			          	LEFT JOIN MAPPINGS.ACT_TRANTYPE t2 ON 
							(t1.batctrcde = t2.batctrcde);
			QUIT;
			
			PROC SUMMARY 
				NWAY MISSING
	            DATA = ZTRNPF_RI;                
	            CLASS chdrnum  tranno  accntid;                                           
	            VAR rwptotal  rwctotal; 
	            OUTPUT 
		            OUT = ZTRNPF_RI(DROP = _:) 
						SUM = ;       
            RUN;                            
			
			/* ****************************************************************** */
			/* ***** GET DETAIL OF RE-INSURANCE TRANSACTIONS IN PREMPF_RP ******* */
			/* ****************************************************************** */
			DATA RPRMPF_RI;
				SET EXTRACT.RPRMPF_RP(KEEP = chdrno	 rskno    tranno  premcl  extr01-extr05 
											 racc    ritype); 
				
				RENAME chdrno =  chdrnum   racc =  accntid;
				LABEL  chdrno = "chdrnum"  racc = "accntid";
				
				rwp = extr01 - extr03;    rwc = extr04 + extr05;
				
				DROP extr01-extr05;
			RUN;
			
			/* Re-insurance Premium information at a more granular level 
			   specifically by accntid AND ritype as well                       */
			PROC SUMMARY
				NWAY MISSING
				DATA = RPRMPF_RI;
				CLASS chdrnum  tranno  rskno  premcl  accntid  ritype;
				VAR   rwp  rwc;
				OUTPUT 
					OUT = RPRMPF_RI(DROP = _:) 
						SUM = ;	
			RUN;
			
			/* Above table aggregated in the same way as before in order to
			   reconcile with ZTRNPF_RI */
			PROC SUMMARY
				NWAY MISSING
				DATA = RPRMPF_RI;
				CLASS chdrnum  tranno  accntid; /* Match with ZTRNPF_RI "grouping" */ 
				VAR   rwp  rwc;
				OUTPUT
					OUT = RPRMPF_RI_SUM(DROP = _:) 
						SUM = ;
			RUN;
			
			/* ***************** RECON ON GROSS SUB ACCOUNTS ***************** */
			DATA RECON_RI 
				 RECON.PR_RI_PSEA_RECON(DROP = propRWP  propRWC); 
				
				MERGE ZTRNPF_RI    (IN = a) 
					  RPRMPF_RI_SUM(IN = b);
					BY chdrnum  tranno  accntid;
				
				IF a; 
				
				IF NOT b THEN    DO;    rwp = 0;    rwc = 0;    END;
				
				IF ABS(rwptotal - rwp) > 0.01 OR 
				   ABS(rwctotal - rwc) > 0.01 THEN
					DO;
						IF rwp NE 0    THEN    propRWP = rwptotal / rwp;    ELSE    propRWP = 0;
						IF rwc NE 0    THEN    propRWC = rwctotal / rwc;    ELSE    propRWC = 0;
						OUTPUT;
					END;
			RUN;
						
			/* Creating the final premium table and we are reallocating the portion of 
			   the data that did not match */
			DATA TRANSV.PR_RI_PSEA;
				MERGE BASE     (IN = a) 
				   	  RPRMPF_RI(IN = b) 
				      RECON_RI (IN = c KEEP = chdrnum  tranno  propRWP  propRWC  accntid); 
					  /* ritype removed in RECON_RI */
					BY chdrnum  tranno;
				
				IF a AND b;
				
				IF c    THEN    DO;    rwp = round(rwp * propRWP, 0.01);    rwc = round(rwc * propRWC, 0.01);    END;
				
				DROP propRWP  propRWC;
			RUN;
		%END;
%MEND;
