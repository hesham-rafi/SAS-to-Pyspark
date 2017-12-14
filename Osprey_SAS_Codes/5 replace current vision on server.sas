/* Remove and create the same directory - hence it will replace content */
DATA _NULL_;
	x 'rmdir A:\Actuarial\6.Data\1.Database\01_ACTLIB /s/q';
	x 'md A:\Actuarial\6.Data\1.Database\01_ACTLIB';  
RUN;
/* Update folder with new content */
%SYSEXEC XCOPY   "&prefix.\&acc_yr.\&acc_yrm."  "A:\Actuarial\6.Data\1.Database\01_ACTLIB" /E /I /Y /R /C;