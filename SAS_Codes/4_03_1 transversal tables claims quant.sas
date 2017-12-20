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
		SET EXTRACT.CMOVPF(KEEP  = clmpfx            clmcoy     claim   tranno  batcactyr  batcactmn 
								   transaction_date  validflag  chgbal  paymnt  chgbal_ri  paymnt_ri
	 							   clstat            paycde     clrate  reqnno  receipt    batctrcde
						   WHERE = (clmpfx    = "CL"  AND 
								    clmcoy    = "1"   AND 
									validflag = "1"));
		
		FORMAT d_tran    d_clo   date9.
		       paymntid          $9.;
		
		/* Need to fix this since transaction_date is in a format different to
	       what we are used to. Normally, dates are in the format yyyymmdd, here
		   we have it as yymmdd. Also if it is in 2000 - 2009 then it is ymmdd   */
		d_tran = convert_date(convert_dateb(transaction_date));
		
		/* Take care on the d_clo - if the claims was never closed then this will 
		   be an errordate - also if the claim has been closed it can be re-opened 
		   and the date can change thus the date is never unique per claim       */
		IF    clstat = 2   THEN   d_clo = d_tran; /* If it is 2 then the claim is closed */
		ELSE                      d_clo = &errdate;
		
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
		RENAME paymnt =  gpaytot    chgbal =  gmovtot    paymnt_ri =  rpaytot    chgbal_ri =  rmovtot;
		LABEL  paymnt = "gpaytot"   chgbal = "gmovtot"   paymnt_ri = "rpaytot"   chgbal_ri = "rmovtot";
		
		DROP transaction_date  batcactyr  batcactmn  clmpfx  clmcoy  validflag 
			 reqnno            receipt;
	RUN;
	
	/* ****************************************************************** */
	/* ************************* FIRST GET BASE ************************* */ 
	/* ****************************************************************** */
	PROC SORT
		NODUPKEYS
		DATA = CMOVPF(DROP = gpaytot  gmovtot  rpaytot  rmovtot)
		OUT  = BASE;
		BY claim  tranno;
	RUN;
	
	/* ****************************************************************** */
	/* ********************** THEN AGGREGATE CMOVPF ********************* */ 
	/* ****************************************************************** */
	PROC SUMMARY 
		MISSING NWAY
		DATA = CMOVPF;
		CLASS claim    tranno;
		VAR   gpaytot  gmovtot  rpaytot  rmovtot;
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
    	SET EXTRACT.CTRNPF(KEEP  = clmpfx  clmcoy  claim  tranno  prcl  rscd
								   paymnt  chgbal
						   WHERE = (clmpfx = "CL"   AND 
									clmcoy = "1"));
		
		/* Create key for mapping based on this concatenation */
		prcl_rscd = COMPRESS(prcl||rscd);
		
		RENAME paymnt =  gpay    chgbal =  gmov;
		LABEL  paymnt = "gpay"   chgbal = "gmov";
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
			 LEFT JOIN CLASSES AS t2 ON 
				(t1.claim = t2.claim);
	QUIT;
	
	PROC SORT
		DATA = TEMPLATE
		OUT  = TEMPLATE;
		BY claim  tranno  prcl_rscd;
	RUN;
	/* ****************************************************************** */
	
	/* Remove records which does not exist in CMOVPF 
	   since we need to recon with CMOVPF        */
	PROC SORT
		DATA = CTRNPF
		OUT  = CTRNPF;
		BY claim tranno;
	RUN;
       
	DATA CTRNPF; 
		MERGE BASE  (IN = a KEEP = claim  tranno) 
			  CTRNPF(IN = b);
			BY claim  tranno;
		
		IF a;
		
		IF NOT b THEN    DO;    gpay = 0;    gmov = 0;    END;
	RUN;
	
	/* Aggregate on granular level */
 	PROC SUMMARY 
		MISSING NWAY
		DATA = CTRNPF;
		CLASS claim  tranno  prcl_rscd;
		VAR   gpay  gmov;
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
		
		IF NOT b THEN    DO;    gpay = 0;    gmov = 0;    END;			
	RUN;
	
	/* ****************************************************************** */
	/* ********* AGGREGATE GROSS CLAIM INFO IN CTRNPF FOR RECON ********* */
	/* ****************************************************************** */
	/* Aggregate granular level of CTRNPF for CMOVPF recon */
	PROC SUMMARY 
		MISSING NWAY
		DATA = CTRNPF;
		CLASS claim  tranno;
		VAR   gpay  gmov;
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
			BY claim  tranno;
		
		IF a;
		
		IF NOT b THEN    DO;    gpay = 0;    gmov = 0;    END;
		
		IF ABS(gpay - gpaytot) > 0.01    THEN    err_pay = 1;    ELSE    err_pay = 0;
		IF ABS(gmov - gmovtot) > 0.01    THEN    err_mov = 1;    ELSE    err_mov = 0;
		
		IF (err_pay = 1 OR err_mov = 1);
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART IV							*/
	/*----------------------------------------------------------*/
	/* ****************************************************************** */
	/* ****** AGGREGATE REINSURANCE CLAIM INFO IN LTRNPF FOR RECON ****** */
	/* ****************************************************************** */
	DATA LTRNPF;
		SET EXTRACT.LTRNPF(KEEP  = clmpfx     clmcoy     claim  tranno  lactyp
								   paymnt_ri  chgbal_ri
						   WHERE = (clmpfx = "CL" AND clmcoy = "1"));
		
	   	/* For co-insurance */ 
		IF lactyp = "I" THEN    DO;    cpay = paymnt_ri;    cmov = chgbal_ri;    rpay = 0;            rmov = 0;            END;	
		ELSE 				    DO;    cpay = 0;            cmov = 0;            rpay = paymnt_ri;    rmov = chgbal_ri;    END;
		
		DROP clmpfx  clmcoy  lactyp  paymnt_ri  chgbal_ri;
	RUN;
	
	/* Remove records which don"t exist in CMOVPF 
	   since we need to recon with CMOVPF        */	
	PROC SORT
		DATA = LTRNPF;
		BY claim  tranno;
	RUN;
	
	DATA LTRNPF; 
		MERGE BASE  (IN = a   KEEP = claim  tranno) 
			  LTRNPF(IN = b);
			BY claim tranno;
		
		IF a;
		
		IF NOT b THEN    DO;    cpay = 0;    cmov = 0;    rpay = 0;    rmov = 0;    END;
		
	RUN;
	
	/* Aggregate on granular level - here it is claim tranno
	   since this is the most granular level (and we need it
	   more granular we will merge this later)              */
	PROC SUMMARY 
		MISSING NWAY
		DATA = LTRNPF;
		CLASS claim  tranno;
		VAR   cpay  cmov  rpay  rmov;
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
		MERGE CMOVPF_SUM(IN = a   DROP = gmovtot  gpaytot)
			  LTRNPF_SUM(IN = b);
			BY claim tranno;
		
		IF a;
		
		IF NOT b THEN    DO;    cpay = 0;    cmov = 0;    rpay = 0;    rmov = 0;    END;
		
		IF ABS(cpay + rpay - rpaytot) > 0.01    THEN    err_pay = 1;    ELSE    err_pay = 0;
		IF ABS(cmov + rmov - rmovtot) > 0.01    THEN    err_mov = 1;    ELSE    err_mov = 0;
		
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
		RETAIN cpay_cum_tranno  cmov_cum_tranno  rpay_cum_tranno  rmov_cum_tranno   0;
		
		IF FIRST.claim THEN
			DO;
				cpay_cum_tranno = cpay;                      cmov_cum_tranno = cmov;
				rpay_cum_tranno = rpay;                      rmov_cum_tranno = rmov;
			END;
		ELSE 
			DO;
				cpay_cum_tranno = cpay_cum_tranno + cpay;    cmov_cum_tranno = cmov_cum_tranno + cmov;
				rpay_cum_tranno = rpay_cum_tranno + rpay;    rmov_cum_tranno = rmov_cum_tranno + rmov;
			END;
		
		RENAME cpay =  cpay_aggr    cmov =  cmov_aggr    rpay =  rpay_aggr    rmov =  rmov_aggr;
		LABEL  cpay = "cpay_aggr"   cmov = "cmov_aggr"   rpay = "rpay_aggr"   rmov = "rmov_aggr";
	RUN;
	
	/* Aggregate CTRNPF */
	DATA CTRNPF_SUM2;
		SET CTRNPF_SUM;
		BY claim  tranno;
		
		RETAIN gpay_cum_tranno  gmov_cum_tranno   0;
		
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
		
		RENAME gpay =  gpay_aggr    gmov =  gmov_aggr;
		LABEL  gpay = "gpay_aggr"   gmov = "gmov_aggr";
	RUN;
	
	/* Merge */
	DATA QUANTI_PSEA;
		MERGE CTRNPF     (IN = a)
			  CTRNPF_SUM2(IN = b)
			  LTRNPF_SUM2(IN = c);
			BY claim  tranno;
		
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
		RETAIN gpay_cum_pr_rsv  gmov_cum_pr_rsv   tmp_cpay_cum_pr_rsv  tmp_cmov_cum_pr_rsv  tmp_rpay_cum_pr_rsv  tmp_rmov_cum_pr_rsv   0;
		
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
				IF nbkeys NE 0 THEN    prop_pay = 1 / nbkeys;
				ELSE                   prop_pay = 1;
			END;
		
		IF gmov_cum_tranno NE 0 THEN
			prop_mov = gmov_cum_pr_rsv / gmov_cum_tranno;
		ELSE
			DO;
				IF nbkeys NE 0 THEN    prop_mov = 1 / nbkeys;
				ELSE                   prop_mov = 1;
			END;
		
		cpay_cum_pr_rsv = prop_pay * cpay_cum_tranno;            cmov_cum_pr_rsv = prop_mov * cmov_cum_tranno;
		rpay_cum_pr_rsv = prop_pay * rpay_cum_tranno;            rmov_cum_pr_rsv = prop_mov * rmov_cum_tranno;
		
		/* Get incremental for CO and RI */
		IF (FIRST.claim OR FIRST.prcl_rscd) THEN
			DO;
				tmp_cpay_cum_pr_rsv = cpay_cum_pr_rsv;           tmp_cmov_cum_pr_rsv = cmov_cum_pr_rsv;
				tmp_rpay_cum_pr_rsv = rpay_cum_pr_rsv;           tmp_rmov_cum_pr_rsv = rmov_cum_pr_rsv;
				
				cpay = cpay_cum_pr_rsv;                          cmov = cmov_cum_pr_rsv;
				rpay = rpay_cum_pr_rsv;                          rmov = rmov_cum_pr_rsv;				
			END;
		ELSE
			DO;		
				cpay = cpay_cum_pr_rsv - tmp_cpay_cum_pr_rsv;    cmov = cmov_cum_pr_rsv - tmp_cmov_cum_pr_rsv;
				rpay = rpay_cum_pr_rsv - tmp_rpay_cum_pr_rsv;    rmov = rmov_cum_pr_rsv - tmp_rmov_cum_pr_rsv;
				
				/* ORDER is important */
				/* temp gets updated after it is subtracted    */
				/* if it is before the result will alwasy be 0 */
				tmp_cpay_cum_pr_rsv = cpay_cum_pr_rsv;           tmp_cmov_cum_pr_rsv = cmov_cum_pr_rsv;
				tmp_rpay_cum_pr_rsv = rpay_cum_pr_rsv;           tmp_rmov_cum_pr_rsv = rmov_cum_pr_rsv;
			END;
		
		IF LAST.claim OR LAST.prcl_rscd THEN
			DO;
				IF gmov_cum_pr_rsv = 0  AND  rmov_cum_pr_rsv NE 0   THEN    rmov_cmov_error_flag = 1;
			END;
		
		KEEP claim  tranno  prcl_rscd  gpay  gmov  cpay 
			 cmov   rpay    rmov; 
	RUN;
	
	/*----------------------------------------------------------*/
	/* 						  PART VII							*/
	/*----------------------------------------------------------*/
	PROC SORT
		DATA = QUANTI_PSEA2;
		BY claim  tranno  prcl_rscd;
	RUN;
	
	/* Finalize by adding qualitative information */
	DATA CL_QUANTI_PSEA;
		MERGE BASE        (IN = a)
			  QUANTI_PSEA2(IN = b);
			By claim  tranno;
		
		IF a;
	RUN;
	
	/* Manual Adjustments from Mapping File :
		Cases where Balo of CLAMPF doesn't reconcile
		with the sum of CHGBAL in CMOVPF			*/
	PROC SORT
		DATA = MAPPINGS.ACT_CLAIM_TRAN_ADJ;
		BY claim  tranno  prcl_rscd;
	RUN;
	
	DATA TRANSV.CL_QUANTI_PSEA;
		MERGE CL_QUANTI_PSEA(IN = a)
			  MAPPINGS.ACT_CLAIM_TRAN_ADJ(KEEP = claim  tranno  prcl_rscd  gpay gmov cpay cmov rpay rmov);
			BY claim  tranno  prcl_rscd;
		
		IF a;
		
	RUN;
%MEND;
