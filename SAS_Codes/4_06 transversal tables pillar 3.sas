/****************************************************************************************
*																						*
*																						*
*						P3 TABLE: GWP & RISK EXPOSURE 									*
*			(KEY: chdrnum, rskno, tranno, zrenno, D_from, rskno) 						*
*																						*
*																						*
****************************************************************************************/

/****************************************************************************************
*																						*
*						(1) SUMMARIZE PREMIUM TABLE BY rskno & tranno 					*
*																						*
****************************************************************************************/

PROC SUMMARY
	NWAY MISSING
	DATA = TRANSV.PR_GR_PSEA(WHERE = (yrm <= &acc_yrm));  /* Remove 'future' rows - not in scope         */
	CLASS  chdrnum  tranno  rskno  d_eff  d_exp  trantype /* The summary is by riskno and tranno only    */
	 	   yrm;									  		  /* the additional variables is added just      */
	VAR    gwp  cwp  gwc  cwc;							  /* for additional information - for example    */
	OUTPUT												  /* tranno will change with trantype (at least) */
		OUT = PREM(DROP = _:) 							  /* but not the other way round                 */
			SUM = ;
RUN;

/****************************************************************************************
*																						*
*		(2) INNER JOIN PREMIUM SUMMARY POLHISTORY DATES (MERGE KEY: chdrnum tranno) 	*
*																						*
****************************************************************************************/

PROC SORT
	DATA = TRANSV.POLHISTORY_PSEA(KEEP = chdrnum   tranno  zrenno  d_cancel)
	OUT = POLHIST;
	BY chdrnum  tranno;
RUN;

DATA PREM2;
	MERGE PREM(IN = a)
		  POLHIST;
		BY chdrnum  tranno;
	
	IF a;
	
	gwp = gwp - cwp;
	gwc = gwc - cwc;
RUN;

PROC SUMMARY
	NWAY MISSING
	DATA = PREM2;
	CLASS chdrnum  rskno  zrenno  d_eff  d_exp;
	VAR   gwp  gwc;
	OUTPUT
	 	OUT = PREM2(DROP  = _:
		 			WHERE = (ABS(gwp) > 0.01))
				SUM = ;
RUN;

/********************************************************************************************************************
*																													*
*	(3) IN THE NEWLY MERGED TABLE, INDEX THE DIFFERENT PERIODS (chdrnum * rskno * zrenno * d_from * d_to) 			*
*							AND CALCULATE THE DAILY RATES FOR EACH PERIOD 											*
*																													*
********************************************************************************************************************/

DATA PREM3;
	SET PREM2;
	BY chdrnum  rskno  zrenno;
	RETAIN period;
	
	daily_rate_p = gwp / (d_exp - d_eff + 1);
	daily_rate_c = gwc / (d_exp - d_eff + 1);
	
	IF FIRST.chdrnum  OR
	   FIRST.rskno    OR
	   FIRST.zrenno   THEN
		 period = 1;
	ELSE
		period = period + 1;
	
	DROP gwp  gwc;
RUN;

/********************************************************************************************************************
*																													*
*		(4) TO SPLIT THE DIFFERENT PERIODS THE RIGHT AMOUNT OF TIMES, GET LIST OF ALL POSSIBLE 						*
*								EFFECTIVE DATES WITHIN EACH PERIOD 													*
*																													*
********************************************************************************************************************/

DATA LISTDATES;
	SET PREM3(KEEP = chdrnum  rskno  zrenno  d_eff  d_exp);
	FORMAT  d_temp  date9.;
	
	d_temp = d_eff;
	OUTPUT LISTDATES;
	
	d_temp = d_exp;
	OUTPUT LISTDATES;
	
	DROP d_eff  d_exp;
RUN;

PROC SORT
	NODUPKEYS
	DATA = LISTDATES;
	BY chdrnum  rskno  zrenno  d_temp;
RUN;

/*********************************************************************************
*																				 *
*	(5) GET THE NUMBER OF PERIODS FOR EACH POLICY x RISK PERIOD (zrenno). 		 *
*	 		WHEN THERE IS ONLY 1, WE WON'T SPLIT THE DATES. 					 *
*				   MAP THE COUNT TO THE LISTDATES TABLE 	 					 *
*																				 *
*********************************************************************************/

DATA NBPERIOD;
	SET PREM3(KEEP = chdrnum  rskno  zrenno  period);
	BY chdrnum  rskno  zrenno;
	periodcount = period;
	
	IF LAST.chdrnum OR
	   LAST.rskno   OR
	   LAST.zrenno;
RUN;

DATA LISTDATES2;
	MERGE LISTDATES(IN = a)
		  NBPERIOD(KEEP = chdrnum  rskno  zrenno  periodcount);
		BY chdrnum  rskno  zrenno;
	
	IF a;
RUN;

/*********************************************************************************
*																				 *
*	(6) IN THE PREMIUM SUMMARY TABLE, MAP THE DIFFERENT EFFECTIVE DATES ON 		 *
*		THE PERIODS WHERE THE TOTAL COUNT OF PERIODS PER zrenno IS MORE THAN 1 	 *
* 				(RQ: THIS WILL NATURALLY DUPLICATE THE RECORDS) 			 	 *
*																				 *
*********************************************************************************/

PROC SQL;
	CREATE TABLE PREM4 AS
	SELECT T1.*,
		   T2.d_temp
	FROM PREM3 AS T1
		LEFT JOIN LISTDATES2 AS T2
			ON T1.chdrnum = T2.chdrnum AND
			   T1.rskno   = T2.rskno   AND
			   T1.zrenno  = T2.zrenno  AND
				 (((T1.d_eff <  T2.d_temp <  T1.d_exp) AND T2.periodcount = 1) OR
				 (( T1.d_eff <= T2.d_temp <= T1.d_exp) AND T2.periodcount > 1));
QUIT;

PROC SORT;
	BY chdrnum  rskno  zrenno  period  d_temp;
RUN;

/*********************************************************************************
*																				 *
*		(7) SPLIT THE RECORDS BY ALL THE DIFFERENT EFFECTIVE DATES 				 *
*						WHICH WERE MAPPED IN STEP 6 		 					 *
*																				 *
*********************************************************************************/

DATA PREM5;
	SET PREM4;
	BY chdrnum  rskno  zrenno  period;
	
	RETAIN TEMPDATE;
	
	FORMAT  d_from  d_to  date9.;
	
	IF FIRST.chdrnum  OR
	   FIRST.rskno    OR
	   FIRST.zrenno   OR
	   FIRST.period   THEN
		DO;
	 		IF LAST.zrenno THEN /* If there is only 1 row */
				DO;
			 		d_from = d_eff;
					d_to   = d_exp;
				 	OUTPUT;
				END;
			ELSE
				DO;
			   		d_from = d_eff;
				 	d_to   = d_temp;
					OUTPUT;
			     	tempdate = d_temp; /* Store this in memory for the next iteration */
				END;
		END;
	ELSE
		DO;
			IF d_temp = (tempdate + 1) THEN /* Will rarely happen */
				DO;
					d_from = d_temp;
					d_to   = d_temp;
					OUTPUT;
				END;
			ELSE
				DO;
					d_from = tempdate + 1; /* Correct start date */
					d_to   = d_temp - 1;   /* Correct end date   */
					OUTPUT;
					
					d_from = d_temp; /* Day on which    */
					d_to   = d_temp; /* change occurred */
					OUTPUT;
				END;
			tempdate = d_temp;
		END;
	
	KEEP chdrnum        rskno  zrenno  d_from  d_to  daily_rate_p
	     daily_rate_c;
	
RUN;

PROC SUMMARY
	NWAY MISSING
	DATA = PREM5;
	CLASS chdrnum  rskno  zrenno  d_from  d_to;
	VAR   daily_rate_p  daily_rate_c;
	OUTPUT
		OUT = PREM5(DROP = _:)
			SUM = ;
RUN;

/************************************************************************************
*																				  	*
*  (8) FOR EACH RECORD IN OUR NEW PREM5, WE'D LIKE A tranno TO BE ABLE TO MERGE   	*
*							WITH ANY RISK TABLE LATER ON.							*
*		WE FIND THE CORRESPONDING tranno IN RISKPF BASED ON THE EFFECTIVE DATES	  	*
*  (d_from IN PREM5 MATCHING dteeff IN RISKPF (OR THE CLOSEST MATCH IN THE zrenno).	*
*			FOR THE ONES WHERE THERE IS NO POSSIBLE MATCHING tranno,				*
*					  WE OUTPUT THE RECORD IN ERROR TABLE.							*
*																				  	*
************************************************************************************/

PROC SORT
	DATA = EXTRACT.RISKPF(KEEP = chdrno  tranno  rskno       dteatt  dteeff  dteter
								 datime  rsktyp  recformat)
	OUT  = RISKPF(DROP = datime);
	BY chdrno  tranno  rskno   DESCENDING  datime;
RUN;

PROC SORT
	NODUPKEYS
	OUT = RISKPF;
	BY chdrno  tranno  rskno;
RUN;

DATA POLHIST_RISK;
	MERGE RISKPF (IN = a)
		  POLHIST(IN = b   RENAME = (chdrnum = chdrno));
		BY chdrno tranno;
	
	IF a AND b;
	
	FORMAT  d_starteff  date9.;
	
	d_starteff = CONVERT_DATE(dteeff);
	
	KEEP chdrno  rskno       zrenno  tranno  d_starteff  dteter
		 rsktyp  recformat;
RUN;

PROC SORT
	DATA = POLHIST_RISK;
	BY chdrno  rskno  zrenno   DESCENDING   d_starteff    DESCENDING   dteter    DESCENDING   tranno;
RUN;

/**************************************************************************************************************
*  We Map the closest transaction of RISKPF to our PREM TABLE. If not a perfect match (d_starteff NE d_from)  *
*							 then we move on to the previous d_starteff.									  *
* 		We also map the lob and date of cancellation (useful when we calculate exposure) from Policy Header   *
**************************************************************************************************************/
DATA PREM6   /* Creates 2 */
	 ERROR;  /* tables... */
	 
	/* Initialize */
	LENGTH chdrstcdc  rsktyp 														   $3.
		   rsktabl 																	   $4.
		   chdrnum 																	   $8.
		   rskno      zrenno  d_from      d_to      d_starteff  d_cancel  tranno        8.
		   recformat 																  $10.;
	FORMAT d_from     d_to    d_starteff  d_cancel                                  date9.;
	
	/* Set start merge */
	SET PREM5;
	
	IF _n_ = 1 THEN
		DO;
			DECLARE HASH DATEMAP(DATASET: 'POLHIST_RISK(RENAME = (chdrno = chdrnum))', MULTIDATA: 'Y');
	 		DATEMAP.DEFINEKEY('chdrnum', 'rskno', 'zrenno');
			DATEMAP.DEFINEDATA('d_starteff', 'tranno', 'rsktyp', 'recformat');
			DATEMAP.DEFINEDONE();
			CALL MISSING(d_starteff, tranno, rsktyp, recformat);
			
			DECLARE HASH CANCMAP(DATASET: 'TRANSV.POL_PSEA');
	 		CANCMAP.DEFINEKEY('chdrnum');
			CANCMAP.DEFINEDATA('d_cancel', 'chdrstcdc');
			CANCMAP.DEFINEDONE();
			CALL MISSING(d_cancel, chdrstcdc);
		END;
	
	rc1 = DATEMAP.FIND();
	rc2 = CANCMAP.FIND();
	
	rsktabl = SUBSTR(recformat, 1, LENGTH(recformat) - 3);
	
	DO WHILE (rc1 = 0);
		IF d_from >= d_starteff THEN
		 	DO;
				OUTPUT PREM6;
				LEAVE;
			END;
		
		DATEMAP.HAS_NEXT(RESULT: HAVE_MORE);
		
		IF NOT HAVE_MORE THEN
			OUTPUT ERROR;
		
		rc1 = DATEMAP.FIND_NEXT();
		
	END;
	
	KEEP chdrnum  zrenno 	 rskno  	   d_from  		 d_to    d_cancel
		 tranno   chdrstcdc  daily_rate_p  daily_rate_c  rsktyp  rsktabl;
RUN;

/**********************************************************************************
*																				  *
*	  (9) FOR SEVERAL RECORDS, WE MAY NOT HAVE FOUND ANY POSSIBLE CANDIDATE 	  *
*						FOR A tranno WITHIN THE SAME POI. 						  *
*			WE THEN MAP THE INFO FROM THE FIRST RECORD OF POLHIST_RISK.			  *
*																				  *
**********************************************************************************/

PROC SQL;
	CREATE TABLE ERROR2 AS
	SELECT T1.*,
		   T2.tranno,
		   T2.d_starteff
	FROM ERROR(DROP = tranno) AS T1
		LEFT JOIN POLHIST_RISK AS T2
			ON T1.chdrnum = T2.chdrno AND
			   T1.rskno   = T2.rskno;
QUIT;

PROC SORT
	DATA = ERROR2;
	BY chdrnum  rskno  zrenno  d_from  d_to   DESCENDING   d_starteff;
RUN;

PROC SORT
	NODUPKEYS
	DATA = ERROR2
	OUT = ERROR2(DROP = d_starteff);
	BY chdrnum  rskno  zrenno  d_from  d_to;
RUN;

/**********************************************************************************
*																				  *
* (10) FINAL TABLE IS THE CONCATENATION OF THE PREVIOUS AND THE CORRECTED ERRORS. *
*																				  *
**********************************************************************************/

DATA TRANSV.P3_PSEA;
	SET PREM6
	ERROR2;
RUN;

PROC SORT;
	BY chdrnum  rskno  zrenno  d_from;
RUN;








/* ACCURACY CHECKS: EXAMPLE ON 201606 YTD */
/*

%LET l_bound = mdy(1,   1, 2016);
%LET u_bound = mdy(6,  30, 2016);
%LET e_year  = mdy(12, 31, 2016);

DATA GEP201606_NEW;
	SET TRANSV.P3_NONHEALTH(WHERE = (d_from <= &u_bound AND
	 								 d_to   >= &l_bound));
	
	gep_new = (min(d_to, &u_bound) - max(d_from, &l_bound) + 1) * daily_rate_p;
	
	exp_new = ((min(d_cancel, &u_bound) - max(d_from, &l_bound)) * (d_from <= d_cancel <= d_to) +
			  (min(d_to, &u_bound) - max(d_from, &l_bound) + 1) * (d_cancel > d_to))
									/
						(&e_year - &l_bound + 1);
RUN;

PROC SUMMARY
	NWAY MISSING
	DATA = GEP201606_NEW;
	CLASS chdrnum  rskno;
	VAR   gep_new  exp_new;
	OUTPUT
	 	OUT = GEP201606_NEW_SUM(DROP = _:)
			SUM = ;
RUN;

DATA GEP201606_OLD;
	MERGE TRANSV.PR_GR_PSEA(IN = a  KEEP  = chdrnum  rskno  d_eff  d_exp  yrm  gwp  cwp
									WHERE = (d_exp >= &l_bound  AND  d_eff <= &u_bound  AND  yrm <= &acc_yrm))
        TRANSV.POL_PSEA(KEEP = chdrnum  chdrstcdc);
		BY chdrnum;
	
	IF A;
	
	poi_days = d_exp - d_eff + 1;
	gep_old  = (min(d_exp, &u_bound) - max(d_eff, &l_bound) + 1)
							    	/
				 		  poi_days * (gwp - cwp);
RUN;

PROC SUMMARY
	NWAY MISSING
	DATA = GEP201606_OLD;
	CLASS chdrnum  rskno;
	VAR   gep_old;
	OUTPUT
	 	OUT = GEP201606_OLD_SUM(DROP = _:)
			SUM = ;
RUN;

DATA ERRORS;
	MERGE GEP201606_OLD_SUM(IN = a)
	 	  GEP201606_NEW_SUM(IN = b);
		BY chdrnum  rskno;
	IF (a OR b);
RUN;

*/
