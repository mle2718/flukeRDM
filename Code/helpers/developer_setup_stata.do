/* small helper to setup up users files for the data directory.
Ideally all data is stored somewhere in "Data" inside the repo but not committed
Most people will store data there.
The processed data takes up alot of space, so whoever processes the data will need to store it elsewhere.
*/

assert inlist("$user", "LCH", "TP", "ML", "KB")

if inlist("$user","LCH") {
	global sfdatadir "E:\Lou_projects\flukeRDM\2028_mgt_cycle"
} 
else if inlist("$user","TP", "ML","KB"){
	global sfdatadir "${here}\Data\2028_mgt_cycle"
}
/* make this directory if it doesn't exist.*/
capture mkdir $sfdatadir 


display "Hello $user.  Use the global gfdatadir in place of \${here}\Data\YYYY mgmt cycle)."
display "The value of datadir is: $sfdatadir"


