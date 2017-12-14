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
    	CLASS CHDRNUM TRANNO TRANTYPE;
      	VAR GWP CWP;
      	OUTPUT OUT = PREMIUM(DROP = _:) SUM = ;
	RUN;
	
	/* Split GWP */
	DATA PREMIUM;
		/* Flag the issuances */
		SET PREMIUM;
		GWP_POI = GWP-CWP;
		GWP_ISSU_POI=(GWP_POI)*(TRANTYPE IN ("NB", "RN"));
		GWP_CANC_POI=(GWP_POI)*(TRANTYPE IN ("CA", "RE"));
		DROP CWP;
	RUN;
	
	/*Map the POI from POLHISTORY to the premium transactions*/
	PROC SORT NODUPKEYS
		DATA = TRANSV.POLHISTORY_PSEA(KEEP = CHDRNUM ZRENNO TRANNO)
		OUT  = MAP_ZRENNO;
      	BY CHDRNUM TRANNO;
    RUN;

	/*Inner Join: if there is a mismatch, it can only be due to delay between the extractions of ZTRN & CHDR*/
	DATA PREMIUM2;
		MERGE PREMIUM(IN=A) MAP_ZRENNO(IN=B);
		BY CHDRNUM TRANNO;
		IF A AND B;
	RUN;

	PROC SUMMARY NWAY;
		CLASS CHDRNUM ZRENNO;
		VAR GWP_POI GWP_ISSU_POI GWP_CANC_POI;
		OUTPUT OUT=PREMIUM3(DROP=_:) SUM=;
	RUN;

	/* ******** Policy Section ******** */
	/* ******************************** */
	/* From POLHISTORY, keep only the latest transaction for each POI (ZRENNO) - by descending D_to & descending TRANNO*/
	PROC SORT
		DATA = TRANSV.POLHISTORY_PSEA(KEEP = CHDRNUM D_com D_exp D_to ZRENNO TRANNO RENEWABLE D_cancel CHDRSTCDC CNTTYPE 
		ORIG_POL LATEST_REPLACEMENT CNTBRANCH AGENTID)
		OUT  = LIST_POI;
      	BY CHDRNUM ZRENNO DESCENDING D_to DESCENDING TRANNO;
    RUN;
	PROC SORT NODUPKEYS
		OUT=LIST_POI(DROP=D_to);
		BY CHDRNUM ZRENNO;
	RUN;

	/*Map Premium for each POI*/
	DATA LIST_POI2;
		MERGE LIST_POI(IN=A) PREMIUM3(IN=B);
		BY CHDRNUM ZRENNO;
		IF A AND B;
	RUN;
	PROC SORT;
		BY ORIG_POL DESCENDING CHDRNUM DESCENDING ZRENNO;
	RUN;
	
	/* ****************************************************************** */	
	/* ************************ Prepare KPI Data ************************ */
	/* ****************************************************************** */
	DATA TRANSV.P2;
		SET LIST_POI2;
		BY ORIG_POL DESCENDING CHDRNUM DESCENDING ZRENNO;
		
		RETAIN NEW_GWP_EE NEW_GWP_IE ANNUAL_FACTOR NEXTAGT NEXTPOL NEXTLINE NEXTBRANCH NEXTCNT;
		
		FORMAT NEW_BRANCH                $2.
			   NEW_CNTTYPE NEW_LINE		 $3. 
			   NEW_POL NEW_AGENTID 		 $8.  
			   D_end 	                 date9.;
		
		ABNORMAL = 0;
		
		/* Initialize Variables */
		/* Underwriting (UW) KPIs */
		PC_RNW     = 1; 						/* Policy renewed */
		GWP_RNW_EE = GWP_ISSU_POI;				/* GWP from renewed policies, excluding endorsements */
		GWP_RNW_IE = GWP_POI - GWP_CANC_POI;	/* GWP from renewed policies, including endorsements */
		/* Initialize renewal information fully and will correct if not applicable in following section */
		PC_NBZ     = 0; 						/* Policy new business (incepts for the first time) */
		GWP_NBZ_EE = 0;							/* GWP from new policies, excluding endorsements */
		GWP_NBZ_IE = 0;							/* GWP from new policies, including endorsements */
		
		/* Cancellations */
		PC_EXP_RNABL_CANC = 0;					/* Renewable policy cancelled   */	
		S_RNABL_GWP_CANC  = 0;					/* GWP of above - before expiry */
		
		PC_EXP_NONRN_CANC = 0;					/* Non-renewable policy cancelled */
		S_NONRN_GWP_CANC  = 0;					/* GWP of above - before expiry */
		
		/* Expired */
		PC_EXP_NONRN_EXPI = 0;					/* Policy Expired, wasn't Renewable */
		S_NONRN_GWP_EXPI  = 0;					/* GWP of above - before expiry */
		
		/* Lapse */
		PC_EXP_RNABL_LAPS = 0;					/* Policy Expired, was Renewable, and Lapsed */
		S_RNABL_GWP_LAPS  = 0;					/* GWP of above - before expiry */
		
		/* Price effect */
		PC_EXP_RNABL_RENW   = 0;				/* Policy Expired, was Renewable, and was Renewed */
		GWP_RENW_OLD_365    = 0;				/* Annualized GWP of renewed policies, including all endorsements just BEFORE the renewal */
		GWP_RENW_NEW_EE     = 0;				/* GWP of renewed policies, excluding endorsements, just AFTER the renewal */
		GWP_RENW_NEW_IE     = 0;				/* GWP of renewed policies, including endorsements, just AFTER the renewal */
		GWP_RENW_NEW_365_EE = 0;				/* Annualized GWP of renewed policies, excluding endorsements, just AFTER the renewal */
		GWP_RENW_NEW_365_IE = 0;				/* Annualized GWP of renewed policies, including endorsements, just AFTER the renewal */
		
		PC_EXP_RNABL_REPL   = 0;				/* Policy Expired, was Renewable, and was Replaced */
		GWP_REPL_OLD_365    = 0;				/* Annualized GWP of replaced policies, including all endorsements just BEFORE the renewal */
		GWP_REPL_NEW_EE     = 0;				/* GWP of replaced policies, excluding endorsements, just AFTER the renewal */
		GWP_REPL_NEW_IE		= 0;				/* GWP of replaced policies, including endorsements, just AFTER the renewal */
		GWP_REPL_NEW_365_EE = 0;				/* Annualized GWP of replaced policies, excluding endorsements, just AFTER the renewal */
		GWP_REPL_NEW_365_IE = 0;				/* Annualized GWP of replaced policies, including endorsements, just AFTER the renewal */
		
		IF (FIRST.ORIG_POL) THEN /* When replacements are fixed we need to add here */
			DO;
				/* Cancel Section */
				/* Exposure (EXP) KPIs */
				IF (D_com <= D_cancel <= D_exp) THEN 
					DO; /* Cancelled contract */
						IF RENEWABLE = 1 THEN
							DO;
								PC_EXP_RNABL_CANC = 1;					    
								S_RNABL_GWP_CANC  = GWP_POI - GWP_CANC_POI;  						
							END;
						ELSE
							DO;
								PC_EXP_NONRN_CANC = 1;						 
								S_NONRN_GWP_CANC  = GWP_POI - GWP_CANC_POI;  /* GWP of above - before expiry   */
							END;
					END;
				
				/* Lapse Section */
				/* If the expiry date has been reached AND the policy has not cancelled,
				   THEN we consider the policy has lapsed OR succesfully expired if it is non-renewable.
				   Else, EXP KPIs will be 0 */
				IF D_exp <= INTNX("MONTH", MDY(&acc_mth, 1, &acc_yr), 0, "END") AND /* end of current month (exipery happened before today) */
				   (PC_EXP_RNABL_CANC = 0 AND PC_EXP_NONRN_CANC = 0) THEN /* Here lapse occurred (and it is not cancelled) */
					 
					DO;
						IF RENEWABLE = 1 THEN 
							DO;
								PC_EXP_RNABL_LAPS = 1;
								S_RNABL_GWP_LAPS  = GWP_POI;
							END;
						ELSE
							DO;
								PC_EXP_NONRN_EXPI = 1;
								S_NONRN_GWP_EXPI  = GWP_POI;	
							END;
					END;
				
				/* FIELDS TO BE RETAINED */
				NEW_GWP_EE = GWP_RNW_EE;
				NEW_GWP_IE = GWP_RNW_IE;
				
				ANNUAL_FACTOR = 366.25 / (D_exp - D_com + 1); /* 366.25 since thailand cover starts and ends on the same day (not the day before) */
				
				/* Renewed */
				NEXTPOL    = CHDRNUM;
				NEXTLINE   = CHDRSTCDC;
				NEXTBRANCH = CNTBRANCH;
				NEXTCNT    = CNTTYPE;
				NEXTAGT    = AGENTID;
				
				/* Did not renew */
				NEW_POL     = "NA";
				NEW_LINE    = "NA";
				NEW_BRANCH  = "NA";
				NEW_CNTTYPE = "NA";
				NEW_AGENTID = "NA";
				
			END;
		ELSE /* Policy renewed or replaced */
			DO;
				IF RENEWABLE = 0 THEN ABNORMAL = 1;
				
				/* Update previous policy info to current (in iteration step) */
				NEW_POL     = NEXTPOL;
				NEW_LINE    = NEXTLINE;
				NEW_BRANCH  = NEXTBRANCH;
				NEW_CNTTYPE = NEXTCNT;
				NEW_AGENTID = NEXTAGT;
			
				IF CHDRNUM = NEXTPOL THEN /* applicable only when replacements occur */
					DO; /* For renewals */
						PC_EXP_RNABL_RENW   = 1;
						GWP_RENW_OLD_365    = GWP_POI * 366.25 / (D_exp - D_com + 1);
						GWP_RENW_NEW_EE     = NEW_GWP_EE;
						GWP_RENW_NEW_IE     = NEW_GWP_IE;
						GWP_RENW_NEW_365_EE = NEW_GWP_EE * ANNUAL_FACTOR;
						GWP_RENW_NEW_365_IE = NEW_GWP_IE * ANNUAL_FACTOR;
					END;
				ELSE 
					DO; /* For replacements */
						PC_EXP_RNABL_REPL   = 1;
						GWP_REPL_OLD_365    = GWP_POI * 366.25 / (D_exp - D_com + 1);
						GWP_RENW_NEW_EE     = NEW_GWP_EE;
						GWP_RENW_NEW_IE     = NEW_GWP_IE;
						GWP_REPL_NEW_365_EE = NEW_GWP_EE * ANNUAL_FACTOR;
						GWP_REPL_NEW_365_IE = NEW_GWP_IE * ANNUAL_FACTOR;
					END;
				
				/* FIELDS TO BE RETAINED */
				NEW_GWP_EE = GWP_RNW_EE;
				NEW_GWP_IE = GWP_RNW_IE;
				
				ANNUAL_FACTOR = 366.25 / (D_exp - D_com + 1); 
				
				NEXTPOL    = CHDRNUM;
				NEXTLINE   = CHDRSTCDC;
				NEXTBRANCH = CNTBRANCH;
				NEXTCNT    = CNTTYPE;
				NEXTAGT    = AGENTID;

			END;
		
		/* Last record - (sorted BY latest inception - first time step of policy journey) is NEW BUSINESS */
		IF (LAST.ORIG_POL) THEN 
			DO;
				/* UW KPIs */
				PC_RNW     = 0;
				GWP_RNW_EE = 0;
				GWP_RNW_IE = 0;
				
				PC_NBZ     = 1;
				GWP_NBZ_EE = GWP_ISSU_POI;
				GWP_NBZ_IE = GWP_POI - GWP_CANC_POI;
			END;
		
		/* Create date end, which is when the policy ended - both cancel and expire */
		IF (PC_EXP_RNABL_CANC = 0 AND PC_EXP_NONRN_CANC = 0) THEN 
			D_end = D_exp;
		ELSE 
			D_end = D_cancel;
		
		/* Calculation of various KPIs */
		GWP_RN_OLD_365 = GWP_RENW_OLD_365 + GWP_REPL_OLD_365;
		
		GWP_RN_NEW_IE 	   = GWP_RENW_NEW_IE + GWP_REPL_NEW_IE;
		GWP_RN_NEW_EE      = GWP_RENW_NEW_EE + GWP_REPL_NEW_EE;
		
		GWP_RN_NEW_365_IE  = GWP_RENW_NEW_365_IE + GWP_REPL_NEW_365_IE;
		GWP_RN_NEW_365_EE  = GWP_RENW_NEW_365_EE + GWP_REPL_NEW_365_EE;
		
		S_RNABL_GWP_RENW   = PC_EXP_RNABL_RENW * (GWP_RNW_IE + GWP_NBZ_IE);
		
		S_RNABL_GWP_REPL   = PC_EXP_RNABL_REPL * (GWP_RNW_IE + GWP_NBZ_IE);
		
		DROP NEW_GWP_EE			  NEW_GWP_IE		   ANNUAL_FACTOR		NEXTPOL			     NEXTLINE		  NEXTCNT 		
			 NEXTAGT 		  	  NEXTBRANCH		   GWP_POI			    GWP_ISSU_POI 	     /*GWP_CANC_POI*/	  D_CANCEL	
			 GWP_RENW_OLD_365     GWP_REPL_OLD_365	   GWP_RENW_NEW_IE	    GWP_REPL_NEW_IE	     GWP_RENW_NEW_EE   GWP_REPL_NEW_EE								
			 GWP_RENW_NEW_365_IE  GWP_REPL_NEW_365_IE  GWP_RENW_NEW_365_EE  GWP_REPL_NEW_365_EE;
	RUN;

%MEND
