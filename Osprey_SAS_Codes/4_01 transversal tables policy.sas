/* ********************************* CREATING TRANSVERSAL TABLES ********************************* */
/* Policy Tables for Policy Sea */
/* **************************** */
%MACRO POLICY_PSEA;
	/*
		Keep only
			1) FG, hence exclude G400, set servunit = FG which is Fire and General; and
	   		2) In Force or Cancelled - statcode = IF and CA records from CHDRPF
	*/
	PROC SORT
		DATA = EXTRACT.CHDRPF(KEEP  = servunit   chdrnum	 currfrom  currto	  tranno	 chdrstcdc
							  		  cownnum    cnttype	 occdate   rnltype	  nofrisks   campaign
							  		  dtecan     reptype	 zrepolno  repnum	  statcode   cntbranch
	      					  		  agntnum    ccdate      crdate	   zendno     zrenno     stattran 
							  WHERE = (servunit = "FG"  AND  statcode IN ("IF", "CA")))
		OUT = CHDRPF;
		
		/* Sort in this order to ensure most recent is kept - zrenno is included since
		   tranno is not neccesarily chronological										  */
	    BY chdrnum   DESCENDING  zrenno   DESCENDING  currto   DESCENDING  tranno;
	RUN;
	
	/* Remove duplicates by taking keeping the most recent information FOR EACH policy
	FOR EACH POI																		  */
	PROC SORT
		NODUPKEYS						/* NODUPKEYS is sorting according to the vars	  */
		DATA = CHDRPF					/* chdrnum, zrenno and tranno 					  */
		OUT  = CHDRPF2;					/* then "combines them" and removes duplicates 	  */
		BY chdrnum  DESCENDING zrenno	/* from this combined "key" (it removes in a      */
					DESCENDING currto	/* decending order, hence keeping the first)  	  */
			        DESCENDING tranno;
	RUN;
	
	/* Clean-up Fields */
	DATA TRANSV.POLHISTORY_PSEA;
		FORMAT  chdrnum  replnum  compnum    $8.;
		SET CHDRPF2;    /* Working with CHDRPF2 from above (in WORK library) */
		
		/* Adding PSEA flag */
		FORMAT  id   $4.;
		id = "PSEA";    /* Important when G400 is added */
		
		/* **************** */
	    /* Dates conversion */
		/* **************** */
	    FORMAT d_from  d_to  d_oricom  d_com  d_exp  d_cancel    date9.;     /* Create new date fields with correct format  							*/
		
	    d_from   = convert_date(currfrom);    /* 1) Effective dates on policy version - thus could be for each policy version throughout the year, hence
			  										there could be MANY CHANGES per policy 																*/
		d_to     = convert_date(currto);	  /* 2) Same as above, just the date until the change is effective. 										*/
		d_oricom = convert_date(occdate);     /* 3) Original commencement date for current policy version it does not look at the original dates for
			  										replaced contracts, thus there is ONLY ONE for each policy version 									*/
		d_com    = convert_date(ccdate);      /* 4) Commencement date, date on which policy is renewed (or new  business) this is the start of the POI 	*/
		d_exp    = convert_date(crdate);      /* 5) Renewal date, if not renewed then expired 															*/
		IF statcode = 'CA'    THEN    d_cancel = convert_date(dtecan);    ELSE    d_cancel = &errdate;   /* 6) Date of cancelation 						*/
		
	    /* Field Renaming */
	    RENAME agntnum =  agentid   cownnum =  ownerid;
		LABEL  agntnum = "agentid"  cownnum = "ownerid";
		
	    /* Renewability */
	    IF rnltype IN ("M2", "02") THEN
			renewable = 0;		/* Creates variable "renewable" in data table 		  */
		ELSE					/* and sets its value according to the				  */
			renewable = 1;		/* conditions mentioned in the statement (M2 or O2)	  */
		
	    /* Clean-Up Replacements */
	    /* Remove trailing blanks and keep the number only if:
	          1 REPTYPE is RF or RX    - replacements occur only when REPTYPE is RF or RX
	          2 REPNUM is not:
					2 a) CHDRNUM, and
					2 b) Length of REPNUM is exactly 8 with
						   2 b   i) first character is a letter (P, Q or S)
						   2 b  ii) all the rest are digits
						   2 b iii) Careful with GTOM (Structure might be different)
		   Same conditions apply on ZREPOLNO						 					  */
		
		/* Replacing blanks */
		chdrnum  = COMPRESS(chdrnum,  " ", "p"); /* Using COMPRESS since it removes       */
	    repnum   = COMPRESS(repnum,   " ", "p"); /* blanks as well as other special       */
		zrepolno = COMPRESS(zrepolno, " ", "p"); /* character types with option p.        */
		
		/* Clear if condition 1 (above) is not met - if it is anything other than 		  */
	    IF reptype NOT IN ("RF", "RX", "CH")     THEN     DO;    repnum   = "";    zrepolno = "";    END;
		
		/* Construct regular expression to match criteria of valid replacement number     */
		/* The condition being, as string starting with P, Q or S, followed immediatly
		   by 7 digits.																	  */
		IF _n_ = 1    THEN    re = PRXPARSE('([PQS][[:digit:]]{7})');
		RETAIN re;
		/* This combination of combining _N_ and RETAIN when used with the PRXPARSE 	
		   function is good programming technique since you avoid executing the function
		   for each iteration of the DATA step.											  */
		
		/* Fixing repnum based on conditions imposed */
		IF repnum NE "" THEN
			DO;
				IF PRXMATCH(re, repnum)    THEN    repnum = PRXPOSN(re, 0, repnum);
				ELSE                               repnum = "";
			END;
		
		/* Fixing zrepolno based on conditions imposed */
		IF zrepolno NE "" AND chdrstcdc = "001" THEN
			DO;
				IF PRXMATCH(re, zrepolno)    THEN    zrepolno = PRXPOSN(re, 0, zrepolno);
				ELSE                                 zrepolno = "";
			END;
		ELSE 
			zrepolno = "";
		
		replnum = repnum;
		compnum = zrepolno;
		
		/* The replacement number if the replacement number equals the contract number - this cannot be a replacement */
	    IF repnum   = chdrnum    THEN    repnum   = "";
		IF zrepolno = chdrnum    THEN    zrepolno = "";
		
		DROP currfrom  currto    rnltype   occdate  ccdate  crdate
			 dtecan    zrepolno  repnum;
	RUN;
	
	/* Create Policy Header table */
	PROC SORT
		NODUPKEYS
		DATA = TRANSV.POLHISTORY_PSEA
		OUT  = TRANSV.POL_PSEA(KEEP = chdrnum  d_oricom  d_cancel  chdrstcdc  ownerid    agentid
									  cnttype  zrenno    replnum   compnum    cntbranch  id);
		BY chdrnum;
	RUN;
	
	/* ****************************************************************** */
	/* *********************** REPLACEMENT SECTION ********************** */ 
	/* ****************************************************************** */
	/* Get Replacements only */
	PROC SORT
		DATA = TRANSV.POL_PSEA(KEEP  = replnum  chdrnum  d_oricom  ownerid  chdrstcdc
							   WHERE = (replnum NE ""))
		OUT  = REPS_ONLY;
		BY replnum;
	RUN;
	
	/* Rename the variables that will be merged later */
	DATA REPS_ONLY;
	 	FORMAT chdrstcdc_2										$3.
			   rep_2        latest_replacement  ownerid_2       $8.
			   d_oricom_2 				       				 date9.;
		SET REPS_ONLY;
		
		rep_2              = chdrnum;
		d_oricom_2         = d_oricom;
		ownerid_2          = ownerid;
		chdrstcdc_2        = chdrstcdc;
		latest_replacement = replnum;
		
		DROP replnum  d_oricom  chdrnum  ownerid  chdrstcdc;
	RUN;
	
	/* Initialize cur_com and latest_replacement */
	DATA REPS;
		SET TRANSV.POL_PSEA(KEEP = chdrnum  d_oricom  ownerid  chdrstcdc);
		FORMAT latest_replacement     		   $8.
			   cur_com   					date9.
			   update_count         			8.;
		
		latest_replacement = chdrnum;
		cur_com            = d_oricom;
		update_count       = 0;
	RUN;
	
	/* Loops to iteratively update each row until most recent
	   contract is found */
	%MACRO DOLOOP;
		%DO i = 1 %TO 15;	
			/* Update the current contract number fields */
			PROC SQL;
				CREATE TABLE WORK.REPS AS 
				SELECT t1.*,
			           t2.chdrstcdc_2, 
			           t2.rep_2, 
			           t2.ownerid_2, 
			           t2.d_oricom_2
					FROM WORK.REPS t1
						LEFT JOIN WORK.REPS_ONLY t2 ON 
							(t1.latest_replacement  = t2.latest_replacement 	AND 
							 d_oricom_2            >= cur_com 					AND 
							 ownerid_2              = ownerid 					AND 
							 chdrstcdc_2            = chdrstcdc);
			QUIT;
			
			DATA REPS;
				SET REPS;
				IF rep_2 NE "" THEN
					DO;
						latest_replacement = rep_2;
						cur_com            = d_oricom_2; 
						/* maybe update ownerid as well */
						update_count = update_count + 1;
						
						IF latest_replacement = chdrnum THEN   /* Not real replacement */
							DO;
								cur_com      = d_oricom;
								update_count = update_count - 1; /* Fix counter */
							END;
					END;
				
				DROP chdrstcdc_2  rep_2  ownerid_2  d_oricom_2;
			RUN;
		%END;
		
		/* Get old policies at top */
		PROC SORT
			DATA = REPS
			OUT  = REPS;
			BY chdrnum  DESCENDING  cur_com;
		RUN;
		/* Dedupe */
		PROC SORT
			NODUPKEYS
			DATA = REPS
			OUT  = REPS;
			BY chdrnum;
		RUN;
	%MEND;
	
	%DOLOOP;
	
	/* Sort by occurance date in group */
	PROC SORT
		DATA = REPS(DROP = cur_com)
		OUT  = REPS;
		BY latest_replacement  d_oricom;
	RUN;
	
	/* Update the original contract number fields */
	DATA REPS2;
		SET REPS;
		FORMAT d_first_com  date9.;
		BY latest_replacement;
		
		RETAIN orig_pol  d_first_com;
		
		IF FIRST.latest_replacement THEN
			DO;
				orig_pol    = chdrnum;
				d_first_com = d_oricom;    
			END;
	RUN;
	
	/* Merge */
	PROC SQL;
	   CREATE TABLE TRANSV.POL_PSEA AS 
	   SELECT t1.*,
	          t2.orig_pol, 
	          t2.latest_replacement,
			  t2.d_first_com
	      FROM TRANSV.POL_PSEA t1
	           INNER JOIN WORK.REPS2 t2 ON 
					(t1.chdrnum = t2.chdrnum);
	QUIT;
	
	PROC SQL;
	   CREATE TABLE TRANSV.POLHISTORY_PSEA AS 
	   SELECT t1.*,
	          t2.orig_pol, 
	          t2.latest_replacement,
			  t2.d_first_com,
			  t2.update_count
	      FROM TRANSV.POLHISTORY_PSEA t1
	           INNER JOIN WORK.REPS2 t2 ON 
					(t1.chdrnum = t2.chdrnum);
	QUIT;
%MEND;

