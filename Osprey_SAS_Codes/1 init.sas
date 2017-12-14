/* ************************************ SET OPTIONS ************************************ */

/* This step is to set the main global variables and custom functions to be used 		 */

/* compress length of data, particularly effective with medium to large datasets	     */
OPTION COMPRESS = BINARY;       
/* enables to create functions to automate repetitive tasks (convert dates for example)	 */
OPTIONS CMPLIB  = WORK.MYFUNCS; 
/* specifies that the SAS session will become active again after the specified command is 
executed, useful when you launch excel automatically from SAS: no need to press EXIT	 */
OPTIONS NOXWAIT;                							   

/* **************************** DEFINE INPUT/OUTPUT FOLDERS **************************** */
/* Set input folder location - for Mapping/Calendar files */
%LET input_folder  = &rootfolder.\02 Input;  
/* Set output folder location - for reporting */
%LET output_folder = &rootfolder.\03 Output; 

/* ************************************ DEFINE FILES *********************************** */
/* Import Closing Days defined in the Excel file */
PROC IMPORT 
	DATAFILE = "&input_folder\Calendar.xlsx" 
	OUT      = CLOSING_DAYS 
	DBMS     = EXCEL REPLACE; 
	SHEET    = ""; 
RUN;

/* ****************************** GLOBAL MACRO-VARIABLES ******************************* */
/* Dates */

/* Date used to indicate an error occurred */
%LET errdate = MDY(12, 31, 2999);					 													
/* Get current year, month, day and hour */
%LET cal_yr  = %SYSFUNC(YEAR (%SYSFUNC(TODAY()))); 	 
%LET cal_mth = %SYSFUNC(MONTH(%SYSFUNC(TODAY())));	 
%LET cal_day = %SYSFUNC(DAY  (%SYSFUNC(TODAY())));     
%LET hour    = %SYSFUNC(HOUR (%SYSFUNC(TIME ())));	 	
/* Get the format to yyyymm -> 2016 * 100 + 6 = 201600 + 6 = 201606 */
%LET cal_yrm = %EVAL(&cal_yr * 100 + &cal_mth); 	 													
/* Get current ymd */
%LET cal_ymd = %EVAL(&cal_yrm * 100 + &cal_day);

%LET monthend = 0; /* month end is defined so that some parts of code will be run on     */
			       /* month end only and other parts of code will not run on month end   */

/********************/
/* Accounting month */
/********************/
/* This is to find the current and previous accounting month as well as to determine
   if the current day should be considered as month end. For example:
   	   If we have: Current month = 2016/08 and Closing day = 2016/08/25
       	   1) Current day = 2016/08/04:
				Accounting month => 2016/08	
				Last Closing(LC) => 2016/07
				Month End 		 => 0 (NO)
       	   2) Current day = 2016/08/26:

				Accounting month => 2016/08	(Before Noon) 
				Last Closing(LC) => 2016/07	(Before Noon)
				Month End 		 => 1 (YES)	(Before Noon) 

							 	 vs.

				Accounting month => 2016/09	(After Noon)
				Last Closing(LC) => 2016/08	(After Noon)
				Month End 		 => 0 (NO)	(After Noon)

       	   3) Current day = 2016/08/29:
				Accounting month => 2016/09	
				Last Closing(LC) => 2016/08
				Month End 		 => 0 (NO)
*/

DATA _NULL_;
	/* Work with dataset "CLOSING_DAYS" imported from Excel. yrm and date_of_closing 
	   comes from CLOSING_DAYS in Excel, this only selects the row where it is the 
	   current accounting month 														 */
	SET CLOSING_DAYS(KEEP  = yrm date_of_closing   
					 WHERE = (yrm = &cal_yrm));   	 
	                                               
	/* Initialize variables */
	incryr     = 0; /* 1) for current accounting */
	incrmth    = 0; /*    period */ 
	
	incryr_lc  = 0; /* 2) for previous accounting */ 
	incrmth_lc = 0; /*    period */ 
	
	IF &cal_day <= DAY(date_of_closing) OR 
				(&cal_day = DAY(date_of_closing) + 1 AND &hour <= 23) THEN
		DO;
			/* Month end occurs the day after the closing date */
			IF &cal_day = (DAY(date_of_closing) + 1) THEN 
				CALL SYMPUTX("monthend", 1);
			ELSE 
				CALL SYMPUTX("monthend", 0);
			
			IF &cal_mth = 1 THEN 
				DO; 
					incryr_lc  = -1; 
					incrmth_lc = 11; 
				END;
			ELSE 
				DO;
					incrmth_lc = -1;
				END; 
		END;
	ELSE
		DO;
			/* If we are past Day 1, then latest closing is based on current calendar 
			   month 																	 */
			IF &cal_mth = 12 THEN 
				DO; 
					incryr  = 1; 
					incrmth = -11;
				END; 
			ELSE 
				DO;
					incrmth = 1;
				END;
		END;
	
	CALL SYMPUTX("acc_yr",   incryr  + &cal_yr);
	CALL SYMPUTX("acc_mth",  incrmth + &cal_mth); 
	CALL SYMPUTX("acc_yrm", (incryr  + &cal_yr) * 100 + (incrmth + &cal_mth));
	
	CALL SYMPUTX("lc_yr",    incryr_lc  + &cal_yr);
	CALL SYMPUTX("lc_mth",   incrmth_lc + &cal_mth);
	CALL SYMPUTX("lc_yrm",  (incryr_lc  + &cal_yr) * 100 + (incrmth_lc + &cal_mth));
RUN;

/* ********************************** HELPER FUNCTIONS ********************************* */
%INCLUDE "&sascode.\02 Sub\01_Helper_Functions\ARRAY.sas";

%INCLUDE "&sascode.\02 Sub\01_Helper_Functions\DO_OVER.sas";

%INCLUDE "&sascode.\02 Sub\01_Helper_Functions\NUMLIST.sas";

/* to convert dates that is in the format yyyymmdd to an appropriate format */ 
PROC FCMP 
	OUTLIB = WORK.MYFUNCS.MATH;
	
	FUNCTION convert_date(x);
		IF x IN (99999999, ., 0) THEN 
			a = &errdate;
		ELSE 
			/* x can be numeric or character input
			   COMPRESS first converts the input to
			   character type, then SUBSTR takes the
			   part of the string that is the 
		       year, month or day. Multiplying this
			   by 1 will convert it to a numeric type
			   MDY needs its input to be numeric.  */
			a = COALESCE(MDY(SUBSTR(COMPRESS(x), 5, 2) * 1, 
							 SUBSTR(COMPRESS(x), 7, 2) * 1, 
							 SUBSTR(COMPRESS(x), 1, 4) * 1), 
						 &errdate);
		RETURN (a);
	ENDSUB;
RUN;

PROC FCMP 
	OUTLIB = WORK.MYFUNCS.MATH;
	
	FUNCTION convert_dateb(x);
		IF LENGTH(COMPRESS(x)) = 6 & SUBSTR(COMPRESS(x), 1, 1) > 7 THEN 
			a = 19000000 + x;
		ELSE 
			a = 20000000 + x;
		RETURN (a);
	ENDSUB;
RUN;

PROC FCMP 
	OUTLIB = WORK.MYFUNCS.MATH;
	
	FUNCTION convert_datenum(x);
		a = YEAR(x) * 10000 + MONTH(x) * 100 + DAY(x);
		RETURN (a); 
	ENDSUB; 
RUN;

/* ********************************** DEFINE LIBRARIES ********************************* */
/* This copies the template folder structure */
%SYSEXEC XCOPY   "&prefix.\Template"  "&prefix.\&acc_yr.\&acc_yrm." /E /I /Y /R /C; 

/* Set current libraries */                                                         
LIBNAME EXTRACT  "&prefix.\&acc_yr.\&acc_yrm.\01 Main\01 Extractions";
LIBNAME MAPPINGS "&prefix.\&acc_yr.\&acc_yrm.\01 Main\02 Mappings";
LIBNAME TRANSV	 "&prefix.\&acc_yr.\&acc_yrm.\01 Main\03 Transversal";
LIBNAME RECON    "&prefix.\&acc_yr.\&acc_yrm.\01 Main\04 Reconciliation";
LIBNAME MOTOR    "&prefix.\&acc_yr.\&acc_yrm.\02 Sub\MOTOR";

/* Set last closing libraries */
LIBNAME BEXTRACT "&prefix.\&LC_yr.\&LC_yrm.\01 Main\01 Extractions";
LIBNAME BTRANSV	 "&prefix.\&LC_yr.\&LC_yrm.\01 Main\03 Transversal";

 