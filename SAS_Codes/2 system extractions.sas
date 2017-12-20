/* ********************************* AS400 EXTRACTIONS ********************************* */
/*   This step is to extract the correct tables and the fields along with those tables   */
/*   lease see document 01 AS400 Table Notes.docx for a full description of each table   */
/* ************************************************************************************* */

/* PREMIUM */
%MACRO PREMIUM(monthend);
	/* Transaction table - All premium information at policy level but not at risk level */
	DATA EXTRACT.ZTRNPF;
		SET AS400B.ZTRNPF(KEEP = batcactyr  batcactmn  rldgacct             tranno     ccdate   effdate 
								 accnum	    rdocnum	   expiry_date          chdrstcda  crate    batctrcde
		 						 sacscode   sacstyp06  origcurr             datime     cnttype  chdrstcdc 
		 						 acctcurr   trandate   tranamt01-tranamt06  tranamt10  batcbrn);
	RUN;
	
	/* Annual premium details - breakdown of gross premiums at risk level more granular 
	than ZTRNPF (only for PSEA not G400) 												 */
	DATA EXTRACT.PREMPF;
		SET AS400B.PREMPF(KEEP = chdrno	 rskno   tranno  premcl  sacscode  extr01-extr05
								 extr10	 datime);
	RUN;
	
	/* Extract to get the premium received date */
	DATA EXTRACT.RTRNPF;
		SET AS400B.RTRNPF(KEEP = rldgacct  tranno  batctrcde  trandate);
	RUN;
	
	/* Reinsurance Annual premium details - Co-Insurance only 
	insurance and coinsurance level of premium transactions ONLY */
	DATA EXTRACT.RPRMPF_CO;
		SET AS400B.RPRMPF(KEEP  = chdrno    rskno          tranno  premcl  racc  ritype 
								  sacscode  extr01-extr05  extr10  datime
						  WHERE = (sacscode = "CO"));
	RUN;
	
	%IF &monthend = 1 %THEN 
		%DO;
			/* Reinsurance Annual premium details - Reinsurance Only */
			DATA EXTRACT.RPRMPF_RP;
				SET AS400B.RPRMPF(KEEP  = chdrno    rskno          tranno  premcl  racc  ritype 
										  sacscode  extr01-extr05  extr10  datime
								  WHERE = (sacscode = "RP"));
			RUN;
		%END;
%MEND;

/* POLICY RISK */
/* All information related to the risk attributed to a particular policy (i.e. pricing
   details)																				 */
%MACRO POLICY_RISK(monthend);
	/* Contract Header - Fire and General contracts only (Non-Life/P&C/General Insurance, 
	   captured by filtering SERVUNIT on FG) This is Policy Sea							 
	   Contract Header captures any and every change and will cause a new row to be 
	   created, note that not all changes will result in new policy number				 */
	DATA EXTRACT.CHDRPF;
		SET AS400B.CHDRPF(KEEP  = chdrnum    cnttype    tranno    currfrom   currto     occdate 
								  ccdate     statreasn  statcode  rnltype    repnum     cownnum
								  cntbranch	 agntnum    payplan   campaign   chdrstcda  chdrstcdc
								  mplnum     coppn      cotype    covernt    zrenno     zendno
								  chdrpfx    chdrcoy    zrepolno  dtecan     statdate   crdate
								  servunit   reptype    datime    validflag  rnldurn    quoteno
								  stattran   nofrisks);
	RUN;	
	
	/* Contract Header Extra Details */
	DATA EXTRACT.CHEXPF;
		SET AS400B.CHEXPF;
	RUN;
	
	/* Private Motor Vehicle General Risk */
	DATA EXTRACT.RPMTPF;
		SET AS400B.RPMTPF;
	RUN;
	
	/* Coverage information Motor - such as sum insured of TTPI */
	DATA EXTRACT.SMCPPF; 
		SET AS400B.SMCPPF(DROP = user_profile  datime  valid_flag);
	RUN;

	/* Named Driver - A table with all drivers and their info for each policy */
	DATA EXTRACT.NDRVPF; 
		SET AS400B.NDRVPF(KEEP = chdrno	 rskno   dteeff   tranno    zsno        clntnum
								 year    datime  cltdobx  zdemerit  valid_flag  dteter
								 cltsex);
	RUN;
	
			/* Generic Risk Header */
	DATA EXTRACT.RISKPF;
	   	SET AS400B.RISKPF(KEEP = chdrno  rskno   rsktyp  tranno     dteatt  dteeff 
										 dteter  totsil  totpre  recformat  datime);
	RUN;

	/* PA&TA General Risk */
	DATA EXTRACT.PPIAPF;
		SET AS400B.PPIAPF;
	RUN;

	/* Coverage information PA&TA - such as Sum Insured of Death */
	DATA EXTRACT.SPIAPF;
		SET AS400B.SPIAPF;
	RUN;
	
	
%MEND;

/* CLAIMS */
%MACRO CLAIMS(monthend);
	/* Claims Movement Transaction Details */
	DATA EXTRACT.CMOVPF;
		SET AS400B.CMOVPF(KEEP = clmpfx     clmcoy            claim      tranno     batcactyr  batcactmn 
								 batctrcde  transaction_date  validflag  balo       chgbal     paymnt
		 						 clrate     balo_ri           chgbal_ri  paymnt_ri  clstat     paycde 
								 reqnno     receipt);
	RUN;
	
	/* Claims Transaction Reserve Dissection */
	DATA EXTRACT.CTRNPF;
		SET AS400B.CTRNPF(KEEP = clmpfx	 clmcoy	 claim  tranno  prcl  rscd 
								 paymnt	 chgbal	 balo);
	RUN;
	
	/* Claims Loss Recovery Transaction Detail */
	DATA EXTRACT.LTRNPF;
		SET AS400B.LTRNPF(KEEP = clmpfx	   clmcoy  claim  tranno  lacnum     lacpfx 
								 laccoy	   lactyp  coppn  cotype  paymnt_ri  chgbal_ri
		 						 balo_ri);
	RUN;

	/* Claim Header Details Record */
	DATA EXTRACT.CLAMPF;
		SET AS400A.CLAMVIEW(KEEP = claim  tranno     chdrstcda  chdrstcdc       agntnum    clntnum 
								 chdrnum  datrep     datocc     rskno           cnttype    rsktyp 
								 clmdsc   zrepclmno  cedref     assess     		assessdt   coppn
								 solict   solictdt   clrvwdat   user_profile    clstat     clcurr 
								 mevent   id         subrec     proc_tran_date  validflag  servbr TPARTY);
	RUN;
	
	/* Claim Extra Detail Physical File - extract from AS400C since decoding is not correct in b (Thai language issue) */
	DATA EXTRACT.CLXDPF;
		SET AS400C.CLXDTH(KEEP = claim      ccdate     dteeff    crdate   acstyp  xdesc01
								 xdesc02    zwcmtpd    zmperfac  zclmprd  indpti  assessrdt
		  						 solictrdt  validflag);
	RUN;
	
	/* Claim Statistics For Rpmt File (Private) */
	DATA EXTRACT.CPMTPF;
		SET AS400C.CPMTTH;
	RUN;

	DATA EXTRACT.CMXTPF;
		SET AS400b.CMXTPF;
	RUN;
	
	/*non motor*/
	DATA EXTRACT.CPATPF;
		SET AS400B.CPATPF;
	RUN;

	DATA EXTRACT.CFIRPF;
		SET AS400B.CFIRPF;
	RUN;

	DATA EXTRACT.CPIAPF;
		SET AS400B.CPIAPF;
	RUN;

	DATA EXTRACT.CWSCPF;
		SET AS400B.CWSCPF;
	RUN;

	/*marine*/
	DATA EXTRACT.CCGOPF;
		SET AS400C.CCGOTH;
	RUN;
	
	DATA EXTRACT.CHULPF;
		SET AS400B.CHULPF;
	RUN;

	/*third party motor*/
	DATA EXTRACT.TPDTPF;
		SET AS400A.TPDTTH;
	RUN;

	/*KFK Details*/
	DATA EXTRACT.CKFKPF;
		SET AS400B.CKFKPF;
	RUN;

	/*recovery*/
	DATA EXTRACT.CLRCPF;
		SET AS400B.CLRCPF;
	RUN;

	/*Claim reinsurance*/
	DATA EXTRACT.CLRIPF;
		SET AS400B.CLRIPF;
	RUN;
 
%MEND;

/* MAPPINGS */
%MACRO MAPPINGS(monthend);
	/* Client & Intermediaries Header Physical File */
	DATA EXTRACT.CLNTPF;
		SET AS400A.CLNTTHAI(KEEP = 	clntpfx   clntcoy    clntnum     xsurname    xgivname  xlsurname
								 	xlgivname secuityno  clttype     cltsex      cltpcode  cltdob
								 	natlty    ethorig    marryd      mailing     ctrycode  xcltaddr01-xcltaddr05 
		 						 	dirmail   addrtype   cltphone01  cltphone02  cltstat	xsalutl
								 	statcode  cltstat	 );
	RUN;
	
	/* Client Extra Details */
	DATA EXTRACT.CLEXPF;
		SET AS400B.CLEXPF;
	RUN;
	
	/* Agent header */
	DATA EXTRACT.AGNTPF;
		SET AS400B.AGNTPF;
	RUN;
	
	DATA EXTRACT.ZYAMPF;
		SET AS400B.ZYAMPF;
	RUN;

	/* Desc Extra Data Screen/Reference File (Others - Mappings) */
	DATA EXTRACT.DESCPF;
		SET AS400B.DESCPF;
	RUN;
	
	/* Physical File: Smart Table Reference Da */
	DATA EXTRACT.ITEMPF;
		SET AS400B.ITEMPF;
	RUN;
	
	/* Physical File: Payment Details (Payment IDs) */
	DATA EXTRACT.CHEQPF;
		SET AS400B.CHEQPF;
	RUN;
	
	/* Physical File: Table Description File */
	DATA EXTRACT.DTAHPF;
		SET AS400B.DTAHPF;
	RUN;
	
	/* Physical File: Field Description File */
	DATA EXTRACT.FLDDPF;
		SET AS400B.FLDDPF;
	RUN;
%MEND;

/* ********************************* LIST OF DOWNLOADS ********************************* */
LIBNAME AS400B ODBC DSN 			= ATLDTA 
			   UID 					= &Axaprod_username 
			   PASSWORD				= &Axaprod_password 
			   READ_ISOLATION_LEVEL	= RU 
			   READBUFF				= 10000 
			   BULKLOAD				= yes 
			   SCHEMA				= "ATLDTA";

LIBNAME AS400C ODBC DSN 			= ATLDTA 
			   UID 					= &Axaprod_username 
			   PASSWORD				= &Axaprod_password 
			   READ_ISOLATION_LEVEL	= RU 
			   READBUFF				= 10000 
			   BULKLOAD				= yes 
			   SCHEMA				= "ATHQRY2";

LIBNAME AS400A ODBC DSN 			= ATLDTA 
			   UID 					= &Axaprod_username 
			   PASSWORD				= &Axaprod_password 
			   READ_ISOLATION_LEVEL	= RU 
			   READBUFF				= 10000 
			   BULKLOAD				= yes 
			   SCHEMA				= "ATHQRY";

%PREMIUM(&monthend);
%POLICY_RISK(&monthend);
%CLAIMS(&monthend);
%MAPPINGS(&monthend);

/*LIBNAME AS400B CLEAR;
LIBNAME AS400C CLEAR;
LIBNAME AS400A CLEAR;*/
