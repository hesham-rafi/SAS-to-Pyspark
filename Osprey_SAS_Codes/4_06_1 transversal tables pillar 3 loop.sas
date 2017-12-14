/* LOOP ON DIFFERENT YRMs */
%MACRO LOOP_YRM;
	/* ******************************************************************************************** */
	/* Date and Range section */
	/* ********************** */
	%LET earn_start = %SYSFUNC(MDY(1, 1, %EVAL(&acc_yr - (&period + 2)))); /* Need to add 2 for rolling 12 months and reporting delay */
	DATA _NULL_;
		CALL SYMPUT('earn_end', INTNX("MONTH", MDY(&acc_mth, 1, &acc_yr), 0, 'END'));
	RUN;
	
	%LET earn_start_yrm = %EVAL(%SYSFUNC(YEAR(&earn_start)) * 100 + %SYSFUNC(MONTH(&earn_start)));
	%LET earn_end_yrm   = %EVAL(%SYSFUNC(YEAR(&earn_end))   * 100 + %SYSFUNC(MONTH(&earn_end)));
	
	/* Compute the number of steps required to RUN all months */	
	DATA _NULL_;
		CALL SYMPUT('n', INTCK("&interval", &earn_start, &earn_end) + 1);
	RUN;
	
	%LET PSEA = ;
	
	%DO i = 1 %TO &n;
		
		DATA _NULL_;
			CALL SYMPUT('u_bound', INTNX("&interval", &earn_end, -%EVAL(&i - 1), 'END'));
			CALL SYMPUT('l_bound', INTNX("&interval", &earn_end, -%EVAL(&i - 1), 'BEGINNING'));
		RUN;
		
		%LET year_length = %SYSFUNC(MDY(12, 31, %SYSFUNC(YEAR(&u_bound)))) - %SYSFUNC(MDY(1, 1, %SYSFUNC(YEAR(&u_bound)))) + 1;
		%LET yrm         = %EVAL(%SYSFUNC(YEAR(&u_bound)) * 100 + %SYSFUNC(MONTH(&u_bound)));
		
		DATA PSEA&yrm;
			SET TRANSV.P3_PSEA(WHERE = (d_from <= &u_bound AND d_to >= &l_bound));
			
			gep = (MIN(d_to, &u_bound) - MAX(d_from, &l_bound) + 1) * daily_rate_p;
			gec = (MIN(d_to, &u_bound) - MAX(d_from, &l_bound) + 1) * daily_rate_c;
			
			IF d_cancel < &l_bound THEN /* cancel before current period */
				DO;
					exp = 0; /* If cancelled before then no exposure */
					rif = 0; /* and it cannot be in force */
				END;
			ELSE IF d_cancel > &u_bound THEN /* cancel after current period */
				DO;
					exp = (&u_bound - MAX(d_from, &l_bound)) / %EVAL(&year_length);
					IF (d_from <= &u_bound) AND (d_to > &u_bound) THEN /* Needs to at least overlap with the current period */
						rif = 1;
					ELSE
						rif = 0;
				END;
			ELSE 	/* cancel in current period */
				DO;
					exp = (MIN(d_to, &u_bound) - MAX(d_from, &l_bound) + 1) / %EVAL(&year_length);
					rif = 0; /* if cancelled in the current period, then it is no longer in force */
				END;
			
			/* Risk In Force */
			/*rif = (d_from   <= d_cancel <= d_to) * (d_from <= &u_bound <  d_cancel) + 
				   (d_cancel > d_to)              * (d_from <= &u_bound <= d_to);*/
			
			yrm = &yrm;
		RUN;
		
		PROC SUMMARY 
			NWAY MISSING
			DATA = PSEA&yrm;
			CLASS chdrnum  rskno  zrenno  tranno  rsktyp  rsktabl chdrstcdc yrm;	 /* recformat vs chdrstcdc recformat shows where  */
			VAR   gep  gec  exp  rif;									  			/* the risk comes from. it is more granular      */
			OUTPUT									 					  			/* (for instance travel - you can see exactly    */
				OUT = PSEA&yrm(DROP = _:) 								  			/* which component is related to the risk)       */
					SUM = ;
		RUN;
		
		%LET PSEA = &PSEA PSEA&yrm;
	%END;
	
	DATA TRANSV.P3_PSEA_MONTHLY; 
		SET &PSEA; 
	RUN;
	
	PROC DELETE 
		DATA = &PSEA; 
	RUN;
%MEND;
