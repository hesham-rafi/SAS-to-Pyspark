%MACRO PILLAR3_PSEA_12RM;
	/* Prepare Claims */
	PROC SORT
		NODUPKEYS
		DATA = TRANSV.CL_QUALI_PSEA(KEEP = claim  chdrnum  rskno  d_occ  chdrstcdc  natcat)
		OUT  = CLAIMQUALI;
		BY claim;
	RUN;
	
	/* Get the mapping of trannos from P3 */
	PROC SORT
		DATA = TRANSV.P3_PSEA(KEEP = chdrnum  rskno  d_from  d_to  zrenno  tranno rsktabl)
		OUT  = TRANNOMAP;
		BY chdrnum  rskno  d_from;
	RUN;
	
	/* Add trannos and zrennos */
	PROC SQL;
		CREATE TABLE CLAIMQUALI2 AS
		SELECT T1.claim,
			   T1.chdrnum,
			   T1.rskno,
			   T1.chdrstcdc,
			   T1.natcat,
		       (YEAR(T1.d_occ) * 100 + MONTH(T1.d_occ)) AS loss_yrm,
			   T2.zrenno,
			   T2.tranno,
			   T2.d_from,
			   T2.rsktabl
			FROM CLAIMQUALI AS T1
				LEFT JOIN TRANNOMAP AS T2 ON
					T1.chdrnum = T2.chdrnum  			AND
					T1.rskno   = T2.rskno    			AND
					T2.d_from <= T1.d_occ <= T2.d_to;
	QUIT;
	
	PROC SORT
		DATA = CLAIMQUALI2
		OUT  = CLAIMQUALI2;
 		BY claim  zrenno  d_from;
    RUN;
	
    PROC SORT
		NODUPKEYS 
		DATA = CLAIMQUALI2
		OUT  = CLAIMQUALI2(DROP = d_from);
    	BY claim;
    RUN;
	
	PROC SUMMARY
		NWAY MISSING
		DATA = TRANSV.CL_QUANTI_PSEA(KEEP = claim  yrm  tranno  d_tran  clstat  gpay  gmov);
		CLASS claim  tranno  d_tran  yrm  clstat;
		VAR   gpay  gmov;
		OUTPUT
			OUT = CLAIMQUANTI(DROP   = _:
							  RENAME = (tranno = tranno_cl))
				SUM = ;
	RUN;
	
	DATA CLAIM;
		MERGE CLAIMQUALI2(IN = A)
			  CLAIMQUANTI(IN = B);
			BY claim;
		
		IF A AND B;
	RUN;
	
	/* START OF LOOPS */
	%LET study_start = %EVAL((&acc_yr - &period) * 100 + 1);
	
	%LET p3_start = %SYSFUNC(MDY(1, 1, %EVAL(&acc_yr - (&period + 2)))); /* Need to add 2 for rolling 12 months, with reporting delay */
	%LET p3_end   = %SYSFUNC(MDY(&acc_mth, 1, &acc_yr));
	
	/* e.g. p3_start = 2017-(5+2) = 2010-01-01; p3_end = 2017-01-01*/
	
	/* Compute the number of steps required to run all months */
	DATA _NULL_;
		CALL SYMPUT('n', INTCK("&interval", &p3_start, &p3_end) + 1);
	RUN;
	
	%LET PREMIUM = ;
	%LET CLAIM   = ;
	
	%DO i = 1 %TO &n;
		DATA _null_;
			CALL SYMPUT('vision',  INTNX("&interval", &p3_end, -%EVAL(&i - 1),                 'end'));
			CALL SYMPUT('l_bound', INTNX("&interval", &p3_end, -%EVAL(&i - 1) - (11 + &delay), 'beginning'));
			CALL SYMPUT('u_bound', INTNX("&interval", &p3_end, -%EVAL(&i - 1) - &delay,        'end'));
		RUN;
		
		%LET vision_yrm  = %EVAL(%SYSFUNC(YEAR(&vision))  * 100 + %SYSFUNC(MONTH(&vision)));
		%LET u_bound_yrm = %EVAL(%SYSFUNC(YEAR(&u_bound)) * 100 + %SYSFUNC(MONTH(&u_bound)));
		%LET l_bound_yrm = %EVAL(%SYSFUNC(YEAR(&l_bound)) * 100 + %SYSFUNC(MONTH(&l_bound)));
		
		/* 12 RM premium FROM P3_MONTHLY */
		PROC SUMMARY
			NWAY MISSING
			DATA = TRANSV.P3_PSEA_MONTHLY(WHERE = (&l_bound_yrm <= yrm <= &u_bound_yrm));
			CLASS chdrnum  rskno  zrenno  tranno rsktabl;
			VAR   gep  gec  exp  rif;
				OUTPUT
					OUT = TEMPPR(DROP = _:)
						SUM = ;
		RUN;
		
		DATA PREMIUM&vision_yrm;
			SET TEMPPR;
			yrm = &vision_yrm;
		RUN;
		
		/* 12 RM CLAIMS - SPLIT BETWEEN ATTRITIONAL/CATNAT/LARGE + EXCLUDE CLAIMS CLOSED @ NIL FROM THE COUNT */
		DATA TEMPCL;
			SET CLAIM(WHERE = ((&l_bound_yrm <= loss_yrm <= &u_bound_yrm) AND  /* If claim occurred in the period of investigation  */
							   yrm <= &vision_yrm)); 						   /* Allow for development over reporting delay */
			BY claim  tranno_cl  d_tran;
			
			RETAIN gpay_ult  gmov_ult;
			
			IF FIRST.claim THEN
				DO;
					gpay_ult = gpay;
					gmov_ult = gmov;
				END;
			ELSE
				DO;
					gpay_ult = gpay_ult + gpay;
					gmov_ult = gmov_ult + gmov;
				END;
			
			IF LAST.claim;
		RUN;
		
		DATA TEMPCL;
			LENGTH chdrstcdc 	$3.
				   claim 		$8.
				   threshold 	 8.;
			
			SET TEMPCL;
			
			IF _n_ = 1 THEN
				DO;
					DECLARE HASH MAPLC(DATASET: 'MAPPINGS.ACT_CLAIM_LARGECLAIM');
					MAPLC.DEFINEKEY('chdrstcdc');
					MAPLC.DEFINEDATA('threshold');
					MAPLC.DEFINEDONE();
					CALL MISSING(threshold);
				END;
			
			rc = MAPLC.FIND();
			
			/* Initialize all variables */
			ginc_nc    = 0;		ginc_l    = 0;		ginc_a    = 0;
			nbclaim_nc = 0;		nbclaim_l = 0;		nbclaim_a = 0; 
			
			/* Populate */
			IF natcat = 1 THEN
				DO;
					ginc_nc    = gpay_ult + gmov_ult;
					nbclaim_nc = 1;
				END;
			ELSE IF (threshold NE . AND (gpay_ult + gmov_ult) > threshold) THEN
				DO;
					ginc_l    = gpay_ult + gmov_ult;
					nbclaim_l = 1;
				END;
			ELSE
				DO;
					ginc_a    = gpay_ult + gmov_ult;
					nbclaim_a = 1;
				END;
			
			IF clstat = '2' AND ABS(gpay_ult + gmov_ult) < 0.01 THEN /* if claim is closed (2) and nothing was paid, then claim count should also be 0 */
				DO;
					nbclaim_nc = 0;		nbclaim_l = 0;		nbclaim_a = 0;
				END;
			
			yrm = &vision_yrm;
			
			KEEP chdrnum  rskno    zrenno     tranno     yrm    rsktabl      ginc_a
				 ginc_l   ginc_nc  nbclaim_a  nbclaim_l  nbclaim_nc;
		RUN;
		
		PROC SUMMARY
			NWAY MISSING;
			CLASS chdrnum  rskno  zrenno  tranno  yrm rsktabl;
			VAR   ginc_a  ginc_l  ginc_nc  nbclaim_a  nbclaim_l  nbclaim_nc;
			OUTPUT
				OUT = claim&vision_yrm(DROP = _:)
					SUM = ;
		RUN;
		
		%LET PREMIUM = &premium  premium&vision_yrm;
		%LET CLAIM   = &claim    claim&vision_yrm;
	%END;
	
	DATA P3_12RM;
		SET &PREMIUM  &CLAIM;
	RUN;
	
	PROC DELETE
		DATA = &PREMIUM &CLAIM;
	RUN;
	
	PROC STDIZE
		REPONLY MISSING = 0
		OUT = P3_12RM;
		VAR _NUMERIC_;
	RUN;
	
	PROC SUMMARY
		NWAY MISSING
		DATA = P3_12RM;
		CLASS chdrnum  rskno  zrenno  tranno  yrm rsktabl;
		VAR   gep      gec        exp        rif          ginc_a  ginc_l
			  ginc_nc  nbclaim_a  nbclaim_l  nbclaim_nc;
			OUTPUT
				OUT = P3_PSEA_12RM(DROP = _:)
					SUM = ;
	RUN;
	
	DATA TRANSV.P3_PSEA_12RM;
		SET P3_PSEA_12RM;
		LENGTH cnttype          chdrstcdc 	 $3.
			   /*campaign 					 $6.*/
			   chdrnum          agentid 	 $8.
			   tranno           rskno 		  8.
			   /*channel 					$20.
			   chdrstcdc_ldesc  			$30.
			   lob 							$35.*/
			   /*agent_name 				$50.
			   agent 						$60.*/;
		
		IF _n_ = 1 THEN
			DO;
				/* Policy Details from Transv.POLHISTORY_PSEA */
				DECLARE HASH POLMAP(DATASET: 'TRANSV.POLHISTORY_PSEA');
				POLMAP.DEFINEKEY('chdrnum', 'tranno');
				POLMAP.DEFINEDATA(/*'d_com',*/ 'cnttype', 'chdrstcdc', 'agentid'/*, 'campaign'*/);
				POLMAP.DEFINEDONE();
				CALL MISSING(/*d_com,*/ cnttype, chdrstcdc, agentid/*, campaign*/);
				
				/* Lob Mapping based on chdrstcdc */
				/*DECLARE HASH LOBMAP(DATASET: 'MAPPINGS.SYS_CHDRSTCDCPSEA');
				LOBMAP.DEFINEKEY('chdrstcdc');
				LOBMAP.DEFINEDATA('chdrstcdc_ldesc');
				LOBMAP.DEFINEDONE();
				CALL MISSING(chdrstcdc_ldesc);*/
				
				/* Intermediary Mapping (from Finance) */
				/*DECLARE HASH DCMAP(DATASET:'MAPPINGS.ACT_INTERM(RENAME = (agent_code = agentid))');
				DCMAP.DEFINEKEY('agentid');
				DCMAP.DEFINEDATA('agent_name', 'channel');
				DCMAP.DEFINEDONE();
				CALL MISSING(agent_name, channel);*/
			END;
		
		rc = POLMAP.FIND();
		/*rc = LOBMAP.FIND();
		rc = DCMAP.FIND();
		
		lob   = COMPBL(chdrstcdc  || "-" || chdrstcdc_ldesc);
		agent = COMPBL(agent_name || "-" || agentid);
		
		uw_year = YEAR(d_com);*/
		
		DROP rc  /*chdrstcdc  chdrstcdc_ldesc  d_com  agent_name  agentid*/;
	RUN;
	
	/*DATA TRANSV.P3_PSEA_12RM;
		SET TRANSV.P3_PSEA_12RM(WHERE = (yrm GE &study_start));
	RUN;*/
%MEND;
