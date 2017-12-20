%MACRO PILLAR2_SUMMARIES;

/*List of Trannos*/
PROC SORT
	DATA=TRANSV.POLHISTORY_PSEA(KEEP=CHDRNUM TRANNO ZRENNO D_from D_to)
	OUT=POLHIST;
	BY CHDRNUM TRANNO;
RUN;

PROC SORT 
	DATA = EXTRACT.RISKPF(KEEP=CHDRNO TRANNO DATIME)
	OUT  = RISKPF(DROP=DATIME);
	BY CHDRNO TRANNO DESCENDING DATIME;
RUN;
PROC SORT NODUPKEYS
	OUT  = RISKPF;
	BY CHDRNO TRANNO;
RUN;

DATA TRANNOLIST;
	MERGE RISKPF(IN=A RENAME=(CHDRNO=CHDRNUM)) POLHIST(IN=B);
	BY CHDRNUM TRANNO;
	IF A AND B;
RUN;
PROC SORT;
	BY CHDRNUM ZRENNO;
RUN;


/*
*********************************************************
*														*
*					Underwriting KPIs					*
*														*
*********************************************************
*/

%LET UW_KPI  = PC_RNW PC_NBZ GWP_NBZ_EE GWP_NBZ_IE GWP_RNW_EE GWP_RNW_IE;
%LET EXP_KPI = PC_EXP_: S_RNABL_: S_NONRN_: GWP_RN_OLD_365 GWP_RN_NEW_365_EE;

DATA UW;
	SET TRANSV.P2(KEEP=CHDRNUM ZRENNO D_com D_exp CHDRSTCDC &UW_KPI 
	WHERE=(YEAR(D_com)>=%eval(&acc_yr-4)));
	YRM = YEAR(D_com)*100+MONTH(D_com);
RUN;
PROC SUMMARY NWAY;
	CLASS CHDRNUM ZRENNO YRM D_com D_exp CHDRSTCDC;
	VAR &UW_KPI;
	OUTPUT OUT=UW(DROP=_:) SUM=;
RUN;
/*Append Relevant Tranno : The first one of each POI/ZRENNO*/
PROC SORT 
	DATA=TRANNOLIST
	OUT=TRANNOLIST_UW;
	BY CHDRNUM ZRENNO D_from TRANNO;
RUN;
PROC SORT NODUPKEYS;
	BY CHDRNUM ZRENNO;
RUN;

DATA UW2;
	MERGE UW(IN=A) TRANNOLIST_UW(KEEP=CHDRNUM ZRENNO TRANNO);
	BY CHDRNUM ZRENNO;
	IF A;
RUN;

/*
*********************************************************
*														*
*						Expiring KPIs					*
*														*
*********************************************************
*/

DATA EXP;
	SET TRANSV.P2(KEEP=CHDRNUM ZRENNO D_com D_exp CHDRSTCDC &EXP_KPI 
	WHERE=(YEAR(D_exp)>=%eval(&acc_yr-4)));
	/*Specific to Thailand : we take the expiry period as the date of expiry because renewal will be on same date*/
	YRM = YEAR(D_exp)*100+MONTH(D_exp);
RUN;
PROC SUMMARY NWAY;
	CLASS CHDRNUM ZRENNO YRM D_com D_exp CHDRSTCDC;
	VAR &EXP_KPI;
	OUTPUT OUT=EXP(DROP=_:) SUM=;
RUN;

/*Append Relevant Tranno : The last one of each POI/ZRENNO*/
PROC SORT 
	DATA=TRANNOLIST
	OUT=TRANNOLIST_EXP;
	BY CHDRNUM ZRENNO DESCENDING D_to DESCENDING TRANNO;
RUN;
PROC SORT NODUPKEYS;
	BY CHDRNUM ZRENNO;
RUN;

DATA EXP2;
	MERGE EXP(IN=A) TRANNOLIST_EXP(KEEP=CHDRNUM ZRENNO TRANNO);
	BY CHDRNUM ZRENNO;
	IF A;
RUN;

/*
*********************************************************
*														*
*			  Combine Expiring, UW & PIF KPIs			*
*														*
*********************************************************
*/

DATA P2_UWEXP;
	SET UW2 EXP2;
RUN;
PROC STDIZE REPONLY MISSING=0 
	OUT=P2_UWEXP;
	VAR _NUMERIC_;
RUN;
PROC SUMMARY NWAY MISSING;
	CLASS CHDRNUM ZRENNO TRANNO YRM D_com D_exp CHDRSTCDC;
	VAR &UW_KPI &EXP_KPI;
	OUTPUT OUT=TRANSV.P2_UWEXP(DROP=_:) SUM=;
RUN;

/*
*********************************************************
*														*
*					Policies in Force					*
*														*
*********************************************************
*/

%LET START_PIF=%sysfunc(MDY(1,1,%EVAL(&acc_yr-4)));

DATA _NULL_;
	CALL SYMPUT('end_PIF',INTNX("month",MDY(&acc_mth,1,&acc_yr),0,'end'));
	CALL SYMPUT('start_PIF',INTNX("month",&start_PIF,0,'end'));
RUN;

/*Compute the number of steps required to run all months*/
DATA _NULL_;
	CALL SYMPUT('N',INTCK("month",&start_PIF,&end_PIF)+1);
RUN;

%PUT &N &end_PIF &start_PIF;

DATA PIF;
	SET TRANSV.P2(KEEP=CHDRNUM ZRENNO D_com D_exp D_end CHDRSTCDC GWP_NBZ_IE GWP_RNW_IE 
	WHERE=(D_com<=&end_PIF and D_end>=&start_PIF));
RUN;

	%LET PIF=;
	%DO I=1 %TO &N;

		DATA _NULL_;
			CALL SYMPUT('PIF_date',INTNX("month",&end_PIF,-%eval(&i-1),'end'));
		RUN;

		%LET YRM=%eval(%sysfunc(YEAR(&PIF_date))*100+%sysfunc(MONTH(&PIF_date)));

		DATA PIF&YRM;
			SET PIF(where=(D_com<=&PIF_date<=D_end));
			FORMAT PIF_date DATE9.;
				PIF=1;
				GWPIF=GWP_NBZ_IE+GWP_RNW_IE;
				YRM=&YRM;
				PIF_date=&PIF_date;
			IF D_end NE D_exp AND D_end=&PIF_date THEN DELETE;
		KEEP CHDRNUM ZRENNO YRM D_com D_exp CHDRSTCDC PIF GWPIF PIF_date;
		RUN;

	%LET PIF=&PIF PIF&yrm;

	%END;

DATA PIF;
	SET &PIF;
RUN;

/*Append Relevant Tranno : The one where the last day of the InForce yrm is included between the bounds of the policy*/
PROC SQL;
	CREATE TABLE PIF2 AS SELECT T1.*,T2.TRANNO,T2.D_from
	FROM PIF AS T1 LEFT JOIN TRANNOLIST AS T2
	ON T1.CHDRNUM=T2.CHDRNUM AND T1.ZRENNO=T2.ZRENNO AND T2.D_from<=T1.PIF_date;
QUIT;
PROC SORT DATA=PIF2;
	BY CHDRNUM ZRENNO YRM DESCENDING D_from;
RUN;
PROC SORT NODUPKEYS
	OUT=TRANSV.P2_PIF(DROP=D_from PIF_date);
	BY CHDRNUM ZRENNO YRM;
RUN;

%MEND;
