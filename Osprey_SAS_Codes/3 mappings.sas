/* ********************************** SYSTEM MAPPINGS ********************************** */
%MACRO DESCPF_MAP(table, name, ID);
	
	DATA MAPPINGS.SYS_&name.&ID.;
		/* Extract the window table with description fields, for selected table */
		SET EXTRACT.DESCPF(KEEP  = desctabl  descitem  language  desccoy  shortdesc  longdesc		  
						   WHERE = (desctabl =  "&table" AND 
									descitem NE ""       AND 
									desccoy  =  "1"      AND 
									language =  "E")); 
		
		/* Rename the item (and the label) description from the system to 
		   user defined name */
		RENAME descitem  =  &name    shortdesc =  &name._sdesc    longdesc  =  &name._ldesc;
		LABEL  descitem  = "&name" 	 shortdesc = "&name._sdesc"   longdesc  = "&name._ldesc";
		/* LABEL is just the name that is displayed, whereas RENAME changes the field naming
		   properties (for calling that varaible somehwere in the code) */
			
		DROP desctabl;
	RUN;
	
	PROC SORT 
		NODUPKEYS;
		BY &name;
	RUN;
%MEND;

%MACRO ITEMPF_MAP(table, key, field, start, length);
	DATA MAPPINGS.SYS_&field.;
		/* Extract the window table with description fields, for selected table 		  */
		SET EXTRACT.ITEMPF(KEEP  = itemtabl  itemitem  genarea  				  
						   WHERE = (itemtabl =  "&table"  AND  
									itemitem NE "")); 
		
		/* Renaming and re-labelling as in descpf_map macro   */
		RENAME itemitem =  &key;	
		LABEL  itemitem = "&key";
		/* Taking only a substring of the specified field to which the key will be mapped */				
		&field = SUBSTR(genarea, &start, &length);
		
		KEEP itemitem  &field;
	RUN;
	
	PROC SORT 
		NODUPKEYS;
		BY &key;
	RUN;
%MEND;

/* ********************************* INTERNAL MAPPINGS ********************************* */
%MACRO EXCEL_MAIN(input_file, sheet, sasname);
	PROC IMPORT 
		DATAFILE = "&input_folder\&input_file"
		OUT      = MAPPINGS.ACT_&sasname 
		DBMS     = excel REPLACE;
		SHEET    = "&sheet";
	RUN;
	
	/* Initialize the dropping variale */
	%LET dropping_levs = '';
	
	/* Drop empty Rows where the missing values are spaces */
	OPTIONS MISSING = ' ';
	DATA MAPPINGS.ACT_&sasname;
   		SET MAPPINGS.ACT_&sasname;
   		IF MISSING(CATS(OF _ALL_)) THEN 
			DELETE;
	RUN;
	
	/* Drop empty Rows where the missing values are blank */
	OPTIONS MISSING = '';
	DATA MAPPINGS.ACT_&sasname;
   		SET MAPPINGS.ACT_&sasname;
   		IF MISSING(CATS(OF _ALL_)) THEN 
			DELETE;
	RUN;
	
	/* Drop blank columns */
	ODS SELECT NONE;
	ODS OUTPUT NLEVELS = TEMP_DATA;
	
	PROC FREQ 
		DATA = MAPPINGS.ACT_&sasname  NLEVELS;
		TABLES _ALL_;
	RUN;
	
	/* Check if empty columns exist */
	DATA _NULL_;
 		dsid  = OPEN('TEMP_DATA');
 		check = VARNUM(dsid, 'nnonmisslevels');
 		IF check = 0 THEN 
			CALL SYMPUT("empty_cols", 0); /* No empty cols     */
		ELSE 
			CALL SYMPUT("empty_cols", 1); /* Empty cols exists */
	RUN;
	
	%IF &empty_cols. = 1 %THEN
		%DO;
			ODS SELECT NONE;
			PROC SQL NOPRINT;
				SELECT tablevar 
				INTO : dropping_levs  SEPARATED BY ' '
					FROM TEMP_DATA 
			   			WHERE nnonmisslevels = 0;
			QUIT;
				
			DATA MAPPINGS.ACT_&sasname;
	 			SET MAPPINGS.ACT_&sasname(DROP = &dropping_levs);
			RUN;
		%END;
%MEND;

/* ********************************* EXTERNAL MAPPINGS ********************************* */
/* INTERMEDIARIES MAPPING   */
/* Owner: Finance (Yanqing) */
/* Agent mapping exmple     */ 
/* 
PROC IMPORT 
		DATAFILE = "O:\MonthEnd\RMIS\Agent_Mapping\SG-TMPT_REF_CUSTMR2v1.44.xls" 
	    OUT      = MAPPINGS.OTH_source_code 
	    DBMS     = EXCEL2000 REPLACE; 
		SHEET    = "Source Code";
RUN;
*/

/* *** DESCPF_MAP mapping example *** */ 
*%DESCPF_MAP(T3681, CNTTYPE);
/* *** ITEMPF_MAP mapping example *** */ 
*%ITEMPF_MAP(T3640, PREMCL, MAS_CLASS, 4, 3);

/* Branch mapping */ 
%DESCPF_MAP(T4678, CLSTAT,              );

%DESCPF_MAP(T3681, CNTTYPE,         PSEA);
%DESCPF_MAP(T3597, CHDRSTCDC,       PSEA);
%DESCPF_MAP(T4677, RSKTYP,          PSEA);
%DESCPF_MAP(T3640, PREMCL,          PSEA);
%DESCPF_MAP(TR824, EVENT_LOSSTYPE,  PSEA);
%DESCPF_MAP(TR825, EVENT_CAUSETYPE, PSEA);
%DESCPF_MAP(T8793, MCITYPE,             );
%DESCPF_MAP(T4663, VEHCLS,              );
%DESCPF_MAP(T8787, USEFOR,              );
%DESCPF_MAP(TU585, ZRSNCD,              );
%DESCPF_MAP(T4660, OCCUPATION_BIZ,      );
%DESCPF_MAP(T4661, EXCESSTYPE,          );
%DESCPF_MAP(T3645, COUNTRYCODE,         );
%DESCPF_MAP(T8308, VEHICLE_BODY,        );
%DESCPF_MAP(T4997, CLAUSES,             );
%DESCPF_MAP(T4616, CAUSE_TYPE,          );
%DESCPF_MAP(T4617, LOSS_TYPE,           );
%DESCPF_MAP(T4676, PAY_CODE,            );
%DESCPF_MAP(T3645, NATIONALITY,         );
%DESCPF_MAP(T3644, OCCUP_CD,            );
%DESCPF_MAP(T4697, CNTRSKTYP,           );
%DESCPF_MAP(T3692, AGTYPE,              );
%DESCPF_MAP(T3617, STATREASN,           );
%DESCPF_MAP(T4681, PRCLRSCD,            );
%DESCPF_MAP(T1692, BRANCH,              );
%DESCPF_MAP(TR830, FAULT,				);
%DESCPF_MAP(T4989, MARINE_TYPE,			);
%DESCPF_MAP(T4651, PAYMENTCODE,			);
%DESCPF_MAP(T3628, CLNTSTAT,			);
%DESCPF_MAP(T4634, MARINE_REC,			);
%DESCPF_MAP(TR4E1, MARINE_TIMELOS,		);
%DESCPF_MAP(T4989, MARINE_SHIPMENT,		);


/*SYSTEM*/
%EXCEL_MAIN(ADM Mapping.xlsm, 000, sys_LOB);
%EXCEL_MAIN(ADM Mapping.xlsm, 001, sys_branch);

/*CLAIM*/
%EXCEL_MAIN(ADM Mapping.xlsm, 100, claim_map);
%EXCEL_MAIN(ADM Mapping.xlsm, 101, claim_rescode);
%EXCEL_MAIN(ADM Mapping.xlsm, 102, Claim_tran_adj);
%EXCEL_MAIN(ADM Mapping.xlsm, 103, claim_inc);
%EXCEL_MAIN(ADM Mapping.xlsm, 111, CLAIM_NATCAT);
%EXCEL_MAIN(ADM Mapping.xlsm, 112, CLAIM_LARGECLAIM);
/* TRANSVERSAL */
%EXCEL_MAIN(ADM Mapping.xlsm, 300, TRANTYPE);

/*PREMIUM*/
%EXCEL_MAIN(ADM Mapping.xlsm, 301, ZTRN_ADJ);
/*CHANNEL*/
%EXCEL_MAIN(ADM Mapping.xlsm, 400, source_code);
/*CLIENTS*/
%EXCEL_MAIN(ADM Mapping.xlsm, 501, Client_gender);

/* MOTOR */
%EXCEL_MAIN(ADM Mapping.xlsm, 700, veh_cov);
%EXCEL_MAIN(ADM Mapping.xlsm, 701, veh_mam);
%EXCEL_MAIN(ADM Mapping.xlsm, 702, veh_oic);
%EXCEL_MAIN(ADM Mapping.xlsm, 703, veh_age);
%EXCEL_MAIN(ADM Mapping.xlsm, 704, veh_sum);
%EXCEL_MAIN(ADM Mapping.xlsm, 705, veh_cap);
%EXCEL_MAIN(ADM Mapping.xlsm, 706, veh_sea);
%EXCEL_MAIN(ADM Mapping.xlsm, 707, veh_ton);
%EXCEL_MAIN(ADM Mapping.xlsm, 708, veh_pla);
%EXCEL_MAIN(ADM Mapping.xlsm, 709, veh_ncd);
%EXCEL_MAIN(ADM Mapping.xlsm, 710, mot_exe_prem);
%EXCEL_MAIN(ADM Mapping.xlsm, 711, Client_ageband);



/* External - Agent mapping */
PROC IMPORT 
	OUT      = MAPPINGS.ACT_INTERM
 	DATAFILE = "P:\RMIS\Reference Tables\Agent Mapping\Agent Mapping.xlsm" 
 	DBMS     = EXCEL REPLACE;
    SHEET    = "Agent mapping"; 
    GETNAMES = YES;
    MIXED    = NO;
    SCANTEXT = YES;  	 	
    USEDATE  = YES;
    SCANTIME = YES;
RUN; 	

