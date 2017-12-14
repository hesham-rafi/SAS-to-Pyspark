/* **************************** */
/* Claims Tables for Policy Sea */
/* **************************** */

%MACRO CLAIMS_QUALI_PSEA;
	/* ****************************************************************** */
	/* **************************** GET BASE **************************** */ 
	/* ****************************************************************** */
	/* Create a base for Quali - only consider */
	PROC SORT 
		DATA = TRANSV.CL_QUANTI_PSEA(KEEP = claim  d_tran d_clo) 
		OUT  = BASEQUALI;
		BY claim  d_tran;
	RUN;
	
	/* Keep first registration date only */
	PROC SORT 
		NODUPKEYS 
		DATA = BASEQUALI(DROP = d_clo RENAME = (D_tran = D_reg))
		OUT = BASEQUALIREG;
		BY claim;
	RUN;

	/* Keep closing date only */
	DATA BASEQUALICLO;
		SET BASEQUALI;
		BY claim  D_tran;
		IF last.claim;
		DROP d_tran;
	RUN;
	
	/*combine closing and registration date*/
	DATA BASEQUALI1;
		MERGE 	BASEQUALIREG( IN = a)
				BASEQUALICLO ( IN = b);
		BY claim;
		IF a and b;
	RUN;
	
	/* ****************************************************************** */
	/* ******************** CLEAN QUALITATIVE TABLES ******************** */ 
	/* ****************************************************************** */
	/* CLAMPF - claim header main info */
	/* normally each row should be unique by claim */
	PROC SORT 
		NODUPKEYS
		DATA = EXTRACT.CLAMPF(KEEP  = claim	    chdrstcda	  chdrstcdc  agntnum   clntnum  chdrnum
									  datrep    datocc		  rskno	     cnttype   rsktyp   clmdsc
									  cedref    zrepclmno	  assess	 assessdt  solict   solictdt
									  clrvwdat  user_profile  clstat	 clcurr    subrec   id
									  mevent    validflag     coppn  	 servbr    TPARTY          
							  WHERE = (validflag = "1"))
		OUT  = CLAMPF;
		BY claim;
	RUN;
	
	/* CLXDPF - Extra claim info */
	/* normally each row should be unique by claim */
	PROC SORT 
		NODUPKEYS 
		DATA = EXTRACT.CLXDPF(KEEP  = claim	   ccdate     dteeff  crdate  acstyp  xdesc01 
									  xdesc02  validflag
							  WHERE = (validflag = "1"))
		OUT  = CLXDPF;
		BY claim;
	RUN;
	
	/* Merge with base and clean variables */
	DATA CL_QUALI_PSEA;
		MERGE BASEQUALI1(IN = a)
			  CLAMPF   (IN = b)
			  CLXDPF   (IN = c);
			BY claim;
		
		IF a;
		
		/* Format dates */
		/* Reported, occurred appointment & review dates */
		FORMAT d_rep  d_occ  d_app_a  d_app_s  d_review  d_com  d_exp  d_eff  d_clo  date9.;
		
		d_com = convert_date(ccdate);
		d_exp = convert_date(crdate); 
		d_eff = convert_date(dteeff);

		d_rep = convert_date(datrep);
		d_occ = convert_date(datocc);
		
		/* Some date issues - assessdt should not exceed todays date */
		/* it can also not be before 1990-05-01, since AXA did not   */
		/* operate in Thailand before this date */
		IF assessdt > &cal_ymd OR assessdt < 19900501 THEN 
			assessdt = 0;
		
		d_app_a  = convert_date(assessdt);
		d_app_s  = convert_date(solictdt);
		d_review = convert_date(clrvwdat);
		
		/* Fund code, Agent ID, Claimant ID */
		RENAME chdrstcda =  fundcode    agntnum =  agentid    clntnum =  claimantid;
		LABEL  chdrstcda = "fundcode"   agntnum = "agentid"   clntnum = "claimantid";
		
		/* Format Claim description */
		clmdesc1 = LOWCASE(TRIM(clmdsc));
		clmdesc2 = LOWCASE(TRIM(xdesc01));
		clmdesc3 = LOWCASE(TRIM(xdesc02));
		
		/* Claim Handler */
		RENAME user_profile =  examiner;
		LABEL  user_profile = "examiner";
		
		RENAME id =  officerid;
		LABEL  id = "officerid";
		
		/* Solicitor & Assessor */
		FORMAT solicitor  assessor $10.;
		
		assessor  = COALESCEC(assess, "NA");
		solicitor = COALESCEC(solict, "NA");
		
		DROP 	datrep    datocc     clmdsc    validflag  
			    clrvwdat  assess     solict    solictdt assessdt
				ccdate	  dteeff     crdate    xdesc01    xdesc02;
	RUN;
	
	/* Add mappings */
	DATA TRANSV.CL_QUALI_PSEA;
		LENGTH clstat           $1.
			   latest_status    $7.
			   claim            $8.   
			   natcat            8.;
		
		SET CL_QUALI_PSEA;
		
		IF _n_ = 1 THEN 
			DO;
				/* Claim Status */
				DECLARE HASH MAPSTAT(DATASET: 'MAPPINGS.SYS_CLSTAT(RENAME = (clstat_ldesc = latest_status))');
				MAPSTAT.DEFINEKEY('clstat');
				MAPSTAT.DEFINEDATA('latest_status');
				MAPSTAT.DEFINEDONE();
				CALL MISSING(latest_status);
				
				/* Nat Cat */
				DECLARE HASH MAPNC(DATASET: 'MAPPINGS.ACT_CLAIM_NATCAT');
				MAPNC.DEFINEKEY('chdrstcdc', 'mevent');
				MAPNC.DEFINEDATA('natcat');
				MAPNC.DEFINEDONE();
				CALL MISSING(natcat);
			END;
		
		rc = MAPSTAT.FIND();
		rc = MAPNC.FIND(); 
		
		IF rc NE 0 THEN 
			natcat = 0;
		
		DROP rc clstat;
	RUN;
%MEND;
