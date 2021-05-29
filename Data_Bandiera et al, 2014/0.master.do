*============================================================================================================
* Date: Feb, 2019
* Project: ELA Uganda
*  
* Objective: This program estimates all tables and results included in the ELA paper.
*
* Data used: Uganda ELA Panel wide.dta
*

*============================================================================================================

*Setting roots and directories of the data
clear all
set more off


/*                       Setting paths and installing packages    */
if              "`c(username)'" == "Oriana"    {
                        global user "/Users/Oriana/Dropbox/ELA Replication/files for AEJ/"
                }
else    if       "`c(username)'" == "YOUR NAME" {
                        global user "YOUR DIRECTORY"
                }
    
			
global Do "${user}/do files"	
global Data "${user}/dta"	
cd "$user"


do "$Do/1.Creation.do"
do "$Do/2.T1_2_3.do"
do "$Do/3.T4_5_6_A3.do"
do "$Do/4.T7_8_F4_FA2.do"
do "$Do/5.TA1_2_4_5_6_7_8.do"
do "$Do/6.F1_2_3.do"
