*------------------------------------------------------------------------------*
**# Global Directory: 
*------------------------------------------------------------------------------*
** replace by 
global Folder "[YOUR PATH HERE!!]/Replication_File"
cd "$Folder/"

*------------------------------------------------------------------------------*
**# Install packages
*------------------------------------------------------------------------------*
*ssc install ivreg2 
*ssc install ftools, replace
*ssc install reghdfe
*ssc install require
*ssc install ivreghdfe
*ssc install ranktest
*ssc install estout

*------------------------------------------------------------------------------*
**# Set preferences:
*------------------------------------------------------------------------------*
net install scheme-modern, ///
	from("https://raw.githubusercontent.com/mdroste/stata-scheme-modern/master/")
set scheme Modern, perm

*------------------------------------------------------------------------------*
**# Create folders
*------------------------------------------------------------------------------*
capture mkdir "$Folder/2_Code/3_Output/"
capture mkdir "$Folder/2_Code/3_Output/Graphs"
capture mkdir "$Folder/2_Code/3_Output/Graphs/GK_2015"
capture mkdir "$Folder/2_Code/3_Output/Graphs/Romers_2004"
capture mkdir "$Folder/2_Code/3_Output/Graphs/Romers_2010"
capture mkdir "$Folder/2_Code/3_Output/Graphs/JST_2024"

*------------------------------------------------------------------------------*
**# Main text Do Files
*------------------------------------------------------------------------------*
do "$Folder/2_Code/1_Main/1_Decomposition.do"
do "$Folder/2_Code/1_Main/2_Multipliers_Pre_1952.do"
do "$Folder/2_Code/1_Main/3_Multipliers_Post_1952.do"

*------------------------------------------------------------------------------*
**# Appendix Do files
*------------------------------------------------------------------------------*

do "$Folder/2_Code/2_Appendix/1_Simulation.do"
do "$Folder/2_Code/2_Appendix/2_Gertler and Karadi (2015).do"
do "$Folder/2_Code/2_Appendix/3_Romer and Romer (2004).do"
do "$Folder/2_Code/2_Appendix/4_Romer and Romer (2010).do"
do "$Folder/2_Code/2_Appendix/5_Jordà, Sing, Taylor (2024).do"
do "$Folder/2_Code/2_Appendix/6_Robustness.do"
