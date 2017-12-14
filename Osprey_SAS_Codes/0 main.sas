/* ------------------------------------------------------------------------------------------------------------ */
/* See document 02 OSPREY SAS Notes.docx for a full description of project OSPREY and the SAS code that follows */
/* ------------------------------------------------------------------------------------------------------------ */

/* ******************************************** SETUP FOR SAS CODE ******************************************** */
%LET rootfolder       = A:\Actuarial\6.Data\3.SAS code\0_OSPREY; 			 /* Code, input and output location */
%LET sascode          = &rootfolder.\01 SAS Code;							 /* Specific code folder            */
%LET prefix           = D:\01 OSPREY\02 ACTLIB;    							 /* Library root folders location   */
%INCLUDE "&sascode.\01 Transversal\1 init.sas";								 /* Initialize */

/* ********************************************** MACRO-VARIABLES ********************************************* */
%LET Axaprod_username = tcstheer;								 	/* UserIDs 								 	     					 */
%LET Axaprod_password = asdfghj9; 				   					/* Passwords 									 					 */
%LET period           = 5;   					   					/* period of investigation - i.e past x years 	 					 */
%LET delay            = 2;   					   					/* reporting delay (in months)                	 					 */
%LET interval         = MONTH;					   					/* Reporting interal choice, options: HOUR/DAY/WEEK/MONTH/QTR/YEAR	 */
%ARRAY(claims_years, VALUES = 1 3 5 10);						    /* Number of past claims to be added to the data, e.g. 1, 3, 5 years */

/* ******************************************* RUN SYSTEM EXTRACTIONS ***************************************** */
%INCLUDE "&sascode.\01 Transversal\2 system extractions.sas";

/* ************************************************ RUN MAPPING *********************************************** */
%INCLUDE "&sascode.\01 Transversal\3 mappings.sas";

/* ****************************************** RUN TRANSVERSAL TABLES ****************************************** */
%INCLUDE "&sascode.\01 Transversal\4_01 transversal tables policy.sas";
%POLICY_PSEA;

%INCLUDE "&sascode.\01 Transversal\4_02 transversal tables premium.sas";
%PREMIUM_PSEA(&monthend);

%INCLUDE "&sascode.\01 Transversal\4_03_1 transversal tables claims quant.sas";
%INCLUDE "&sascode.\01 Transversal\4_03_2 transversal tables claims qual.sas";
%CLAIMS_QUANTI_PSEA;
%CLAIMS_QUALI_PSEA;

%INCLUDE "&sascode.\01 Transversal\4_04 transversal tables pillar 1.sas";
%PILLAR1;

%INCLUDE "&sascode.\01 Transversal\4_05_1 transversal tables pillar 2.sas";
%INCLUDE "&sascode.\01 Transversal\4_05_2 transversal tables pillar 2 summaries.sas";
%PILLAR2;
%PILLAR2_SUMMARIES;

%INCLUDE "&sascode.\01 Transversal\4_06 transversal tables pillar 3.sas";
%INCLUDE "&sascode.\01 Transversal\4_06_1 transversal tables pillar 3 loop.sas";
%INCLUDE "&sascode.\01 Transversal\4_06_2 transversal tables pillar 3 12 rm.sas";
%LOOP_YRM;
%PILLAR3_PSEA_12RM;

%INCLUDE"&sascode.\01 Transversal\4_07 transversal client.sas"
%CUSTOMERS;

%INCLUDE"&sascode.\01 Transversal\4_08 transversal agent.sas"
%AGENTS;

/* ******************************************* UPDATE SERVER VISION ******************************************* */
%INCLUDE "&sascode.\01 Transversal\5 replace current vision on server.sas";
