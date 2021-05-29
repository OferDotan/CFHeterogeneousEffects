use "$Data/ELA.dta", clear


***** Table T4, T5, T6 & TA3 *****


local SUMMARY "iga control_body aspiration"

local IGA "Entrep_total any_iga selfempl empl Expenditure_totDF "

local CHILDBEARING_MARRIAGE "M_children partner"

local RAPE "R_sexunwilling"

local KNOWLEDGE "sex_pregnancy Rhiv_skillsALT"

local CONTRACEPTION "always_condom other_contraceptive"

local EMPOWERMENT "empowerment"

local MARRIAGE "M_idealmarry_ageF M_idealmarry_ageM"

local CHILD_BEARING "M_idealbaby_no M_idealbaby_ageF"

local ASPIRATIONS "M_idealdaught_marry M_idealson_marry"

local INCOME "income_year_ind"

local EDUCATION "E_Denrolled study_hours back_school"

*local WELFARE "worry_job Expenditure_totDF "

*local CONSUMPTION "Expenditure_totDF"


*** Defining global topics ***

global topics      "SUMMARY IGA CHILDBEARING_MARRIAGE RAPE KNOWLEDGE CONTRACEPTION EMPOWERMENT MARRIAGE CHILD_BEARING ASPIRATIONS INCOME EDUCATION"


*** Options ***

* CONTROLS
global controls "age"

* BRANCH DUMMIES
*xi, prefix(_B) i.branch_name



*** Preparing the loops ***

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of varlist _B* {
    global branches "$branches `var'"
}

replace Rincind_empl=. if follow_up==0
replace Qincind_empl=. if endline==0

sum incind_empl 
sum Rincind_empl
sum Qincind_empl
reg Rincind_empl incind_empl _B* age treatment if panel==1, cluster(villid)
reg Qincind_empl incind_empl _B* age treatment if panel==1, cluster(villid)

replace Rincind_selfempl=. if follow_up==0
replace Qincind_selfempl=. if endline==0

tab incind_selfempl if panel==1 & age!=. & Qincind_selfempl!=. & treatment==0
sum incind_selfempl if panel==1 & age!=. & Qincind_selfempl!=. & treatment==0

sum Rincind_selfempl
sum Rincind_selfempl if Rselfempl==1
sum Rincind_selfempl if Rselfempl==0
reg Rincind_selfempl incind_selfempl _B* age treatment if panel==1, cluster(villid)

sum Qincind_selfempl
sum Qincind_selfempl if Qselfempl==1
sum Qincind_selfempl if Qselfempl==0
reg Qincind_selfempl incind_selfempl _B* age treatment if panel==1, cluster(villid)



*** ITT Regressions ***

foreach var of global varsestimation {
    
	sum `var' if treatment==0 & panel==1 & age!=. & Q`var'!=.
    global m_`var'=r(mean)
    global sd_`var'=r(sd)
    betaformat m_`var'
    betaformat sd_`var'
	
	count if panel==1 & R`var'!=. & age!=.
    global N_R`var'=r(N)
    numformat N_R`var'
	
	count if panel==1 & Q`var'!=. & age!=.
    global N_Q`var'=r(N)
    numformat N_Q`var'

}

** ITT Midline - ANCOVA
foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1, cluster(villid)
	getreg treatment, name(ittIC`var')
}

** ITT Endline - ANCOVA
foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1, cluster(villid)
	getreg treatment, name(ittICQ`var')
}

** ITT Midline - NO CONTROLS
foreach v of global varsestimation  {
	reg R`v' treatment if panel==1 & age!=., cluster(villid)
	getreg treatment, name(ittFOL`v') 
}

** ITT Endline - NO CONTROLS
foreach v of global varsestimation  {
	reg Q`v' treatment if panel==1 & age!=., cluster(villid)
	getreg treatment, name(ittEND`v')
}


*** Lee Bounds Estimations

*Estimate lee bounds without controls

* The lee bounds treat the sample after attrition as a selected sample
* Need to select an indicator of the selectivity for each sample
* In the data we may have 2 types of missing information:
	* Model 1: Missing information at follow up (where the variable follow_up indicates the resulting selected sample)
	* Model 2: Missing information at endline (where the variable endline indicates the resulting selected sample)

tab endline follow_up, m

*** Lee bounds using FOLLOW UP as the selection variable (Lee Bounds for girls present at baseline and attriting at midline/follow-up) ***

foreach v of global varsestimation {

* Estimate Lee Bounds
leebounds R`v' treatment if age!=., selec(follow_up)

*Getting coefficients 
mat A = e(b)
local N1 = A[1,1]
local N2 = A[1,2]
local N3 = e(depvar)

global lowerM1`v'=`N1'
global upperM1`v'=`N2'

betaformat lowerM1`v'
betaformat upperM1`v'

*Getting SE
mat S = e(V)
local S1 = S[1,1]
local S2 = S[2,2]
local SD1=sqrt(`S1')
local SD2=sqrt(`S2')

global sd_lowerM1`v'=`SD1'
global sd_upperM1`v'=`SD2'

betaformat sd_lowerM1`v'
betaformat sd_upperM1`v'

*Getting P-values
local zstat1 = (`N1') / `SD1'
local pvalue1 = 2 * normprob(-abs(`zstat1'))

local zstat2 = (`N2') / `SD2'
local pvalue2 = 2 * normprob(-abs(`zstat2'))

*Generating significance levels for lower bounds
if `pvalue1'<0.01{
	local star1 ***
}
if `pvalue1'>0.01 & `pvalue1'<0.05{
	local star1 **
}
if `pvalue1'>0.05 & `pvalue1'<0.1{
	local star1 *
}
if `pvalue1'>0.1{
	local star1 ""
}
 
*Generating significance levels for upper bounds
if `pvalue2'<0.01{
	local star2 ***
}
if `pvalue2'>0.01 & `pvalue2'<0.05{
	local star2 **
}
if `pvalue2'>0.05 & `pvalue2'<0.1{
	local star2 *
}
if `pvalue2'>0.1{
	local star2 ""
}

global pstar_lowerM1`v'="`star1'"
global pstar_upperM1`v'="`star2'"

}


*** Lee bounds using ENDLINE as the selection variable (Lee Bounds for girls present at baseline and only attriting at endline) ***

foreach v of global varsestimation {

gen XXX`v'=Q`v'
replace XXX`v'=. if panel==0

* Estimate Lee Bounds
leebounds XXX`v' treatment if age!=., selec(endline) 

*Getting coefficients 
mat A = e(b)
local N1 = A[1,1]
local N2 = A[1,2]
local N3 = e(depvar)

global lowerM2`v'=`N1'
global upperM2`v'=`N2'

betaformat lowerM2`v'
betaformat upperM2`v'

*Getting SE
mat S = e(V)
local S1 = S[1,1]
local S2 = S[2,2]
local SD1=sqrt(`S1')
local SD2=sqrt(`S2')

global sd_lowerM2`v'=`SD1'
global sd_upperM2`v'=`SD2'

betaformat sd_lowerM2`v'
betaformat sd_upperM2`v'

*Getting P-values
local zstat1 = (`N1') / `SD1'
local pvalue1 = 2 * normprob(-abs(`zstat1'))

local zstat2 = (`N2') / `SD2'
local pvalue2 = 2 * normprob(-abs(`zstat2'))

*Generating stars for lower bounds
if `pvalue1'<0.01{
	local star1 ***
}
if `pvalue1'>0.01 & `pvalue1'<0.05{
	local star1 **
}
if `pvalue1'>0.05 & `pvalue1'<0.1{
	local star1 *
}
if `pvalue1'>0.1{
	local star1 ""
}
 
*Generating stars for upper bounds
if `pvalue2'<0.01{
	local star2 ***
}
if `pvalue2'>0.01 & `pvalue2'<0.05{
	local star2 **
}
if `pvalue2'>0.05 & `pvalue2'<0.1{
	local star2 *
}
if `pvalue2'>0.1{
	local star2 ""
}

global pstar_lowerM2`v'="`star1'"
global pstar_upperM2`v'="`star2'"

drop XXX`var'

}


*** Generating the output ***

cap drop topic 
cap drop var 
cap drop mean 
cap drop N 
gen topic=""
gen var=""
gen mean=""
gen N=""
gen ITT_IC=""
gen ITT_ICQ=""
gen ittFOL=""
gen ittEND=""
gen lowerM1=""
gen upperM1=""
gen lowerM2=""
gen upperM2=""
gen e1=""
gen e2=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "D`x'"=="D`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace mean="${m_`x'}" in `i'
            qui replace mean="[${sd_`x'}]" in `j'
			
			qui replace N="${N_R`x'} / ${N_Q`x'}" in `i'
			
			qui replace ITT_IC="${beta_ittIC`x'}${pstar_ittIC`x'}" in `i'
            qui replace ITT_IC="${se_ittIC`x'}" in `j'
			
			qui replace ITT_ICQ="${beta_ittICQ`x'}${pstar_ittICQ`x'}" in `i'
            qui replace ITT_ICQ="${se_ittICQ`x'}" in `j'
			
            qui replace ittFOL="${beta_ittFOL`x'}${pstar_ittFOL`x'}" in `i'
            qui replace ittFOL="${se_ittFOL`x'}" in `j'
			
			qui replace ittEND="${beta_ittEND`x'}${pstar_ittEND`x'}" in `i'
            qui replace ittEND="${se_ittEND`x'}" in `j'
			
            qui replace lowerM1="${lowerM1`x'}${pstar_lowerM1`x'}" in `i'
            qui replace lowerM1="(${sd_lowerM1`x'})" in `j'
            
			qui replace upperM1="${upperM1`x'}${pstar_upperM1`x'}" in `i'
            qui replace upperM1="(${sd_upperM1`x'})" in `j'
			
			qui replace lowerM2="${lowerM2`x'}${pstar_lowerM2`x'}" in `i'
            qui replace lowerM2="(${sd_lowerM2`x'})" in `j'
            
			qui replace upperM2="${upperM2`x'}${pstar_upperM2`x'}" in `i'
            qui replace upperM2="(${sd_upperM2`x'})" in `j'
           
			local i=`i'+4
            local p=1
            continue, break
        }
    }
    if `p'==0 {
        if `i'==1 local i=1
        else local i=`i'+2
        qui replace topic="`var'" in `i'
    }
}

foreach var of varlist itt* {
    qui replace `var'="" if `var'=="." | `var'=="(.)"
}

replace var="IGA Index" if var=="iga"
replace var="Welfare Index" if var=="welfare"
replace var="Control Over the Body Index" if var=="control_body"
replace var="Aspiration Index" if var=="aspiration"

*browse variables of interest 
br topic var mean N e1 ITT_IC ITT_ICQ ittFOL ittEND e2 lowerM1 upperM1 lowerM2 upperM2




