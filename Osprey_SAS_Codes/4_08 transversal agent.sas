%MACRO AGENTS;

	DATA agent;
		SET extract.agntpf ( KEEP = agntnum clntnum agntbr validflag START_DATE DATEEND where = (validflag = '1'));
		
		FORMAT d_start d_end date9.;

		d_start = convert_date(START_DATE);
		d_end = convert_date(DATEEND);
		RENAME agntnum = agentid;
		LABEL agntnum = "agentid";

		DROP validflag START_DATE DATEEND; 

	RUN;

	PROC SQL;
		CREATE TABLE agent1 (DROP = CLNTNUM) AS SELECT a.*, b.clntnum, b.client as agent_name
		FROM agent AS a LEFT JOIN transv.clients AS b 
		ON a.clntnum = b.CLNTNUM;
	QUIT;

	PROC SQL;
		CREATE TABLE agent2 AS SELECT a.*, b.agntnum as agentid, b.STCDA as SOURCE_CODE
		FROM agent1 AS a LEFT JOIN extract.zyampf AS b 
		ON a.agentid = b.agntnum;
	QUIT;

	DATA agent2;
		SET agent2;
			SOURCE_CODE = UPCASE(SOURCE_CODE);
	RUN;

	PROC SQL;
		CREATE TABLE agent3 AS SELECT a.*, b.*
		FROM agent2 AS a LEFT JOIN mappings.act_source_code AS b 
		ON a.SOURCE_CODE = b.SOURCE_CODE;
	QUIT;

	DATA TRANSV.AGENTS;
		SET agent3;
		FORMAT CHANNEL $20.;

		IF CHANNEL_GROUP in ('General Agents' 'Local Broker' 'TISCO' 'Direct Others' 'FLD' 'Other Agents') THEN DO;
			IF agntbr = '10' THEN CHANNEL = TRIM(CHANNEL_GROUP)!!" (BKK)";
			ELSE CHANNEL = TRIM(CHANNEL_GROUP)!!" (UPC)";
			END;
		ELSE DO;
			CHANNEL = TRIM(CHANNEL_GROUP);
			END;

		IF agentid in ('BD039' 'VQ226') THEN CHANNEL = 'Digital';
			
			RENAME AGNTBR = AGENT_BRANCH;
			LABEL STCDA = 'SOURCE_CODE'
				  AGNTBR = 'AGENT_BRANCH';
	RUN;

	PROC SORT DATA = TRANSV.AGENTS;
	BY agentid;
	RUN;

%MEND;
