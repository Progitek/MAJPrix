$PBExportHeader$majprix.sra
$PBExportComments$Generated Application Object
forward
global type majprix from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
string is_odbc
end variables

global type majprix from application
string appname = "majprix"
string appruntimeversion = "22.1.0.2828"
end type
global majprix majprix

on majprix.create
appname="majprix"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on majprix.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;string ls_sql, ls_odbc, ls_filestring
int li_filenum

is_odbc = commandline
//open(w_slapshot)

SQLCA.DBMS       = 'ODBC'
SQLCA.AutoCommit = True
SQLCA.LOCK		  = "0"

if is_odbc = '' then
	FileDelete("C:\ii4net\dentitek\odbcnom.bat")
	FileDelete("c:\ii4net\dentitek\output.txt")
	li_FileNum = FileOpen("C:\ii4net\dentitek\odbcnom.bat", TextMode!, Write!, LockWrite!, Replace!)
	FileWriteEx(li_FileNum, "c:\ii4net\dentitek\dblocate.exe >c:\ii4net\dentitek\output.txt")
	FileClose(li_FileNum)
	sleep(2)
	run("C:\ii4net\dentitek\odbcnom.bat",Minimized!)
	sleep(20)
	li_FileNum = FileOpen("c:\ii4net\dentitek\output.txt", Linemode!)
   FileReadEx(li_FileNum, ls_filestring)
	FileReadEx(li_FileNum, ls_filestring)
	FileReadEx(li_FileNum, ls_filestring)
	FileReadEx(li_FileNum, ls_filestring)
	ls_odbc = trim(left(ls_filestring,20))
	SQLCA.DbParm  = "ConnectString='DSN="+ ls_odbc +";UID=dba;PWD=ii4pr0g1+3k01',ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'"
else
	SQLCA.DbParm  = "ConnectString='DSN="+ is_odbc +";UID=dba;PWD=ii4pr0g1+3k01',ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'"
end if
//SQLCA.DbParm  = "ConnectString='DSN=dentitekdev;UID=dba;PWD=ii4clmam',ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'"


connect using SQLCA;
if sqlca.sqlcode <> 0 then
	MessageBox ("Impossible de se connecter à la base de données / Cannot Connect to Database", sqlca.sqlerrtext)
	return
end if

// Authentifier la connection pour la version 11

ls_sql = "SET TEMPORARY OPTION CONNECTION_AUTHENTICATION='Company=Progitek;Application=Progitek;Signature=000fa55157edb8e14d818eb4fe3db41447146f1571g7cf6b1c162e7a4e4925570fc8104c82ac6d466c6'"
execute immediate :ls_sql using SQLCA;
if sqlca.sqlcode <> 0 then
	MessageBox ("Validation d'authentification", sqlca.sqlerrtext)
	return
end if

open(w_majprix)



end event

