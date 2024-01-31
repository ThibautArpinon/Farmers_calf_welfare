// Data Analysis Master File

clear all 


/*----------------------------------------------------------------------------*/
/* Working directory  					 					                  */
/*----------------------------------------------------------------------------*/

cd "/Users/ThibautArpinon_1/Desktop/Codes_data/"


/*----------------------------------------------------------------------------*/
/* Set seed for reproducible results 										  */
/*----------------------------------------------------------------------------*/

set seed 123456 


/*----------------------------------------------------------------------------*/
/* Install packages 										                  */
/*----------------------------------------------------------------------------*/

// ssc install tabout
// ssc install estout


/*----------------------------------------------------------------------------*/
/* Import data										                          */ 
/*----------------------------------------------------------------------------*/
use "Data/survey_SVO_clean.dta"


// The data used here was cleaned for another project. (see README file)

// Therefore,the data is already relatively clean when uploading the file "survey_SVO_clean.dta". 



/*----------------------------------------------------------------------------*/
/* Create addiitonal variables for the analysis								  */ 
/*----------------------------------------------------------------------------*/

// generate variable - both view calves and grazing 

//gen picture = . 

// "Treatment group"
/* Question: You now have two choices of what to see. You can either see a picture of Friesian bull calves on a 
lorry or see a blank screen for 5 seconds. Which one do you prefer? */ 

replace picture = 0 if view_picture == 0 /*view blank screen*/ 
replace picture = 1 if view_picture == 1 /*view calves*/ 


// "Control group"
/* Question: You now have two choices of what to see. You can either see a picture of grazing dairy
cows or see a blank screen for 5 seconds. Which one do you prefer? */ 
replace picture = 2 if view_grazing == 1 /*view grazing*/ 
replace picture = 3 if view_grazing == 2 /*view blank screen*/ 

table picture

/* 
----------------------
  picture |      Freq.
----------+-----------
        0 |        106 --> Chose to view blank screen instead of calves
        1 |        393 --> Chose to view calves instead of blank screen 
        2 |         46 --> Chose to view grazing cows instead of blank screen
        3 |          1 --> Chose to blank screen instead of grazing cows
----------------------
*/ 


// Crosstabulation 2x2 

// Create treatment variable 

gen treatment_info = . 

// Replace treatment equals 0 if the participant was in the control group (i.e., view cows or blank-screen)
replace treatment_info = 0 if picture == 2
replace treatment_info = 0 if picture == 3

// Replace treatment equals 1 if the participant was in the treated group (i.e., view calves or blank-screen)
replace treatment_info = 1 if picture == 0
replace treatment_info = 1 if picture == 1

// Defining label 
la var treatment_info "Group"
la def treatment_info 0 "Control" 1 "Treatment"
la val treatment_info treatment_info


// Create variable for if participants chose to see blank screen  

gen chose_see_image = . 

/* Replace chose_see_image equals 0 if the participant decided to view the blank screen
and this regardless of the treatment  */ 
replace chose_see_image = 0 if picture == 0
replace chose_see_image = 0 if picture == 3

/* Replace chose_see_image equals 1 if the participant decided to view the image
and this regardless of the treatment  */
replace chose_see_image = 1 if picture == 1
replace chose_see_image = 1 if picture == 2

// Defining label 
la var chose_see_image "Chose to see the image"
la def chose_see_image 0 "No" 1 "Yes" 
la val chose_see_image chose_see_image


// Putting variable of year survey filled in as variable 

gen year_dummy = .
replace year_dummy = 0 if year == 2020
replace year_dummy = 1 if year == 2021


// Save modifications
save "Data/modified_data.dta", replace

clear all





/*----------------------------------------------------------------------------*/
/* Summary statistics										                  */ 
/*----------------------------------------------------------------------------*/

/******************************************************************************/
/* IMPORTANT NOTE: the summary statistics (Table 1) was created directly on 
LaTeX. All the numbers are retrievable in the code below*/
/******************************************************************************/

use "Data/modified_data.dta"

// First restrict the summary statistics to only treated (499), thus deleting control group observations

drop if treatment_info == 0 // N = 499

// Recover summary statistics for total sample
su herd uaa calfmort calfproportion surplus_export age_dummy gender_dummy educ_dummy


// Recover summary depending on decision to view calves on a lorry or blank screen
bys chose_see_image: su herd uaa calfmort calfproportion surplus_export age_dummy gender_dummy educ_dummy 


// Testing for differences between the two groups

// First look whether data is normally distributed
// If yes --> t-test
// if data skewed --> Mann-Whitney 
// If doubt do both tests and compare the results according to distribution 

histogram herd, by(chose_see_image) 
ttest herd, by(chose_see_image)
ranksum herd, by(chose_see_image)
// --> Report t-test


histogram uaa, by(chose_see_image)
ttest uaa, by(chose_see_image)
ranksum uaa, by(chose_see_image)
// --> Report t-test



histogram calfmort, by(chose_see_image)
ttest calfmort, by(chose_see_image)
ranksum calfmort, by(chose_see_image)
// --> Report t-test


histogram calfprop, by(chose_see_image)
ttest calfprop, by(chose_see_image)
ranksum calfprop, by(chose_see_image)
// --> Report t-test


histogram surplus_export, by(chose_see_image) // Heavily skewed left
// Big difference here between t-test and Mann-Whitney
ttest surplus_export, by(chose_see_image)
ranksum surplus_export, by(chose_see_image)
// --> Report Mann Whitney


tab age_dummy chose_see_image, exact 
tab age_dummy chose_see_image, chi2 

tab gender_dummy chose_see_image, exact 
tab gender_dummy chose_see_image, chi2 

tab educ_dummy chose_see_image, exact 
tab educ_dummy chose_see_image, chi2 


prtest chose_see_image == 0.5


clear all





/*----------------------------------------------------------------------------*/
/* Information avoidance										              */ 
/*----------------------------------------------------------------------------*/


/******************************************************************************/
/* IMPORTANT NOTE: the regression (Table B.1) was created and transfered on LaTeX.
The coefficient figure (Figure 4) was created on Numbers using the file "Graphs".
All the numbers are retrievable in the code below*/
/******************************************************************************/

use "Data/modified_data.dta"


drop if treatment_info == 0 // N = 499

// Logit model 
logistic chose_see_image herd uaa calfmort calfproportion surplus_export  gender_dummy age_dummy educ_dummy year_dummy
logit chose_see_image herd uaa calfmort calfproportion surplus_export  gender_dummy age_dummy educ_dummy year_dummy

probit chose_see_image herd uaa calfmort calfproportion surplus_export  gender_dummy age_dummy educ_dummy year_dummy


quietly logit chose_see_image herd uaa calfmort calfproportion surplus_export  gender_dummy age_dummy educ_dummy year_dummy  

// Logistic model to obtain odds ratio (whoch corresponds to the exponential of logit coeff)
eststo raw
eststo or
//


// The table is saved in the file "Tables"
esttab raw or using "Tables/Logisitic_Model_1.tex", replace eform(0 1) b(3) se(3) stats(r2_p ll, labels("Pseudo R-square" "Log-likelihood")) ///
star(* 0.10 ** 0.05 *** 0.01) nonumbers mtitles ("Logit coeff." "Odds ratio") ///
coeflab(herd "Herd size" uaa "UAA (hectares)" calfmort "Calf mortality (\%)" ///
calfproportion "Calf housing (\%)" surplus_export "Export surplus calves (\%)" gender_dummy "Female (=1)" ///
age_dummy "Over age 46 (=1)" educ_dummy "3rd educ. level or higher (=1)" year_dummy "Year survey 2021 (=1)" ///
_cons "Constant") ///
title(Logistic regression of choice to view the image or blank screen) 

 
 
// Relationship between education and decision to view
tabulate educ chose_see_image
 tab educ chose_see_image , matcell(foo)
// As matrix to graph, copy to numbers
mat li foo



// Relationship between age and decision to view 
 tabulate age chose_see_image
 tab age chose_see_image , matcell(foo)
// As matrix to graph, copy to numbers
mat li foo



// Relationship between age and decision to view 
tabulate gender_dummy chose_see_image
tab gender_dummy chose_see_image , matcell(foo)

// As matrix to graph, copy to numbers
mat li foo
 
 

 
/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not										          */ 
/*----------------------------------------------------------------------------*/


/******************************************************************************/
/* IMPORTANT NOTE: the figures (Figures 5 and 6) were created on Numbers using 
the file "Graphs". All the numbers are retrievable in the code below*/
/******************************************************************************/

clear all 

use "Data/modified_data.dta"


drop if treatment_info == 0 // N = 499


// Analysis for those who chose to see the picture 

// Care about animals
table view_picture_reason1 

// Guilt
table view_picture_reason2

// Bias info 
table view_picture_reason3

// Other 
table view_picture_reason4


// Table of all participants who responded yes to each reason 
tabstat view_picture_reason*, s(sum) col(s)

// Freq 
di 284/393
di 42/393
di 89/393 
di 35/393


// Analysis for those who chose to not see the picture 

// Not interested
table view_nopicture_reason1 

// Biased info
table view_nopicture_reason2

// Expect guilt
table view_nopicture_reason3

// Not my business
table view_nopicture_reason4

// Already know
table view_nopicture_reason5

// Expect sadness
table view_nopicture_reason6

// Other
table view_nopicture_reason7



// Table of all participants who responded yes to each reason 
tabstat view_nopicture_reason*, s(sum) col(s)
// Freq 
di 9/106
di 25/106
di 16/106
di 9/106
di 54/106
di 29/106
di 4/106



 
/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to calf mortality			      */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in calf mortality among the different reasons given for viewing (or not) the image

// T-tests 
ttest calfmort, by(view_picture_reason1)
ttest calfmort, by(view_picture_reason2)
ttest calfmort, by(view_picture_reason3)
// --> No differences


// Regression for the reasons not to view the picture 
reg calfmort view_nopicture_reason1 view_nopicture_reason2 view_nopicture_reason3 view_nopicture_reason4 view_nopicture_reason5 /*
*/  view_nopicture_reason6 view_nopicture_reason7
// --> No differences


// T-tests 
ttest calfmort, by(view_nopicture_reason1)
ttest calfmort, by(view_nopicture_reason2)
ttest calfmort, by(view_nopicture_reason3)
ttest calfmort, by(view_nopicture_reason4)
ttest calfmort, by(view_nopicture_reason5)
ttest calfmort, by(view_nopicture_reason6)
// --> No differences



/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to surplus export			      */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in calf mortality among the different reasons given for viewing (or not) the image

// T-tests 
ttest surplus_export, by(view_picture_reason1)
ttest surplus_export, by(view_picture_reason2)
ttest surplus_export, by(view_picture_reason3)
// --> No differences


// T-tests 
ttest surplus_export, by(view_nopicture_reason1)
ttest surplus_export, by(view_nopicture_reason2)
ttest surplus_export, by(view_nopicture_reason3)
ttest surplus_export, by(view_nopicture_reason4)
ttest surplus_export, by(view_nopicture_reason5)
ttest surplus_export, by(view_nopicture_reason6)
// --> No differences


/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to calf housing			      */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in calf mortality among the different reasons given for viewing (or not) the image

// T-tests 
ttest calfproportion, by(view_picture_reason1)
ttest calfproportion, by(view_picture_reason2)
ttest calfproportion, by(view_picture_reason3)



// T-tests 
ttest calfproportion, by(view_nopicture_reason1)
ttest calfproportion, by(view_nopicture_reason2)
ttest calfproportion, by(view_nopicture_reason3)
ttest calfproportion, by(view_nopicture_reason4)
ttest calfproportion, by(view_nopicture_reason5)
ttest calfproportion, by(view_nopicture_reason6)


/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to gender 			              */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in gender among the different reasons given for viewing (or not) the image


// Reasons to view 
tab gender_dummy view_picture_reason1, exact
tab gender_dummy view_picture_reason2, exact
tab gender_dummy view_picture_reason3, exact


// Reasons not to view
tab gender_dummy view_nopicture_reason1, exact
tab gender_dummy view_nopicture_reason2, exact
tab gender_dummy view_nopicture_reason3, exact
tab gender_dummy view_nopicture_reason4, exact
tab gender_dummy view_nopicture_reason5, exact
tab gender_dummy view_nopicture_reason6, exact


/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to age 			              */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in gender among the different reasons given for viewing (or not) the image

tab age_dummy 


// Reasons to view 
tab age_dummy view_picture_reason1, exact
tab age_dummy view_picture_reason2, exact
tab age_dummy view_picture_reason3, exact
// No differences


// Reasons not to view
tab age_dummy view_nopicture_reason1, exact
tab age_dummy view_nopicture_reason2, exact
tab age_dummy view_nopicture_reason3, exact
tab age_dummy view_nopicture_reason4, exact
tab age_dummy view_nopicture_reason5, exact
tab age_dummy view_nopicture_reason6, exact


/*----------------------------------------------------------------------------*/
/* Reasons for engaging or not, with respect to education 			          */ 
/*----------------------------------------------------------------------------*/

// Here we explore whether there are differences in gender among the different reasons given for viewing (or not) the image

tab educ_dummy 


// Reasons to view 
tab educ_dummy view_picture_reason1, exact
tab educ_dummy view_picture_reason2, exact
tab educ_dummy view_picture_reason3, exact
// No differences


// Reasons not to view
tab educ_dummy view_nopicture_reason1, exact
tab educ_dummy view_nopicture_reason2, exact
tab educ_dummy view_nopicture_reason3, exact
tab educ_dummy view_nopicture_reason4, exact
tab educ_dummy view_nopicture_reason5, exact
tab educ_dummy view_nopicture_reason6, exact



/*----------------------------------------------------------------------------*/
/* Treatment effect 			                                              */ 
/*----------------------------------------------------------------------------*/
 
/******************************************************************************/
/* IMPORTANT NOTE: the code here displqys the analysis for Tables 2 and 3. The 
tables were made directly on LaTeX. All the numbers are retrievable in the code
below*/
/******************************************************************************/ 
 
 
clear all 

use "Data/modified_data.dta"


// 2X2 table with frequencies
tabulate chose_see_image treatment_info, row

// Export 2X2 table to file "Tables" (to obtain base, then modified design by hand in LaTeX
tabout chose_see_image treatment_info using "Tables/testtable.tex", c(row) replace ///
style(tex) f(2p 2p) font(bold) body



// Running hypothesis testing on the sample
/* With those test we are comparing the percentages 
(i.e., comparison of observed value vs. expected value if there was no treatment effect */ 

// With those tests, the hypothesis testing is setup as follows: 
// H0: there is no treatment effect
// H1: there is a treatment effect

// Display 2x2 table with Fisher Exact test
// Fisher Exact test is more appropriate due to extremely low number of observations in cell control/no
tabulate chose_see_image treatment_info, exact // -> p-value: 0.000


// -------------------------------------- // 
// Also possible with Chi Square but calculation might be incorrect due to low number of observations in cell control/no
// Display 2x2 table with Chi Square test 
//tabulate chose_blank_screen treatment_info, chi2 // -> p-value: 0.002
// -------------------------------------- // 


// Now that there is an effect, looking into odds ratio 
// Reporting the odds ratio with a logit regression
logistic chose_see_image treatment_info
logit chose_see_image treatment_info


test _b[treatment_info]=0

local sign_treatment_info = sign(_b[treatment_info])  

display "H_0: coef<=0  p-value = " 1-normal(`sign_treatment_info'*sqrt(r(chi2)))

display "H_0: coef>=0  p-value = " normal(`sign_treatment_info'*sqrt(r(chi2)))



// Regression with controls
logistic chose_see_image treatment_info uaa calfmort calfproportion surplus_export herd gender_dummy age_dummy educ_dummy year_dummy  

// The treatment effect holds with the inclusion of controls


gen treat_uaa= uaa*treatment_info
gen treat_calfmort= calfmort*treatment_info
gen treat_calfproportion= calfproportion*treatment_info
gen treat_surplus_export= surplus_export*treatment_info
gen treat_herd= herd*treatment_info
gen treat_gender= gender_dummy*treatment_info
gen treat_age= age_dummy*treatment_info
gen treat_educ= educ_dummy*treatment_info



logistic chose_see_image treatment_info treat_uaa treat_calfmort treat_calfproportion treat_surplus_export ///
treat_herd treat_gender treat_age treat_educ   





/*----------------------------------------------------------------------------*/
/* Bootstrap robustness check 			                                      */ 
/*----------------------------------------------------------------------------*/

// Resampling by bootstrapping to obtain the bootstrapped standard error, storing estimations for each new sample
bootstrap b=exp(_b[treatment_info]), reps(10000) seed(123456) saving("Data/bootstrap_oddratio.dta", replace): logit chose_see_image treatment_info
estat bootstrap, all

// Use the bootstrapped estimates
use "bootstrap_oddratio.dta", replace

// Histogram of bootstrapped values, exported to file "Graphs"
histogram b, bin(100) xtitle("Bootstrapped odds ratio") frequency color(eltblue) scheme(s1color) xlab(0(0.1)0.8, format(%3.1f))
graph export "Graphs/Bootsrapped_odd_ratio.png", replace


/* END */ 




 
 
 
 
 
 
 



