use "$Data/ELA.dta", clear


***** Table T1 *****

local EMPOWERMENT "age E_Denrolled M_children partner"

local IGA "Entrep_total selfempl empl worry_job"

local CHILDBEARING "R_sexunwilling sex_pregnancy Rhiv_skillsALT always_condom other_contraceptive"

local MARRIAGE "empowerment M_idealmarry_ageF"


*** Defining global topics ***

global topics      "EMPOWERMENT IGA CHILDBEARING MARRIAGE"

global comparison "treatment"   /* 'treatment', 'rich', 'below16', "rural" */


*** Preparing the loops ***

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of global varsestimation {

    reg `var' $comparison, cluster(villid)
    getreg $comparison, name(d`var')
	
	sum `var' if $comparison==1
    local sd1_`var'=r(sd)
	global sd1_`var'=r(sd)
	local Var1_`var'=r(Var)
    local mean1_`var'=r(mean)
	global mean1_`var'=r(mean)
	betaformat sd1_`var'
	betaformat mean1_`var'
        
    sum `var' if $comparison==0
    local sd0_`var'=r(sd)
	global sd0_`var'=r(sd)
	local Var0_`var'=r(Var)
    local mean0_`var'=r(mean)
	global mean0_`var'=r(mean)
	betaformat sd0_`var'
	betaformat mean0_`var'
        
    global nd_`var'=(`mean1_`var''-`mean0_`var'')/(sqrt(`Var1_`var''+`Var0_`var''))
    betaformat nd_`var'

}


*** Generating the output ***

cap drop topic var
gen topic=""
gen var=""
gen mean1=""
gen mean0=""
gen diff=""
gen norm_diff=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "`x'"=="`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace mean1="${mean1_`x'}" in `i'
            qui replace mean1="[${sd1_`x'}]" in `j'
			
			qui replace mean0="${mean0_`x'}" in `i'
            qui replace mean0="[${sd0_`x'}]" in `j'
			
			qui replace diff="${beta_d`x'}${pstar_d`x'}" in `i'
            qui replace diff="${se_d`x'}" in `j'
			
			qui replace norm_diff="${nd_`x'}" in `i'
           
			local i=`i'+2
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


*browse variables of interest 
br topic var mean1 mean0 diff norm_diff

cap drop topic var mean0 mean1 diff norm_diff




***** Table T2 *****

gen follow_endline=0
replace follow_endline=1 if follow_up==1 & endline==1

* Different levels of attrition & set of control variables

*local attrition "follow_up" /* Tracked Between Baseline and Midline */
*local attrition "endline" /* Tracked Between Baseline and Endline */
local attrition "follow_endline" /* Tracked Between Baseline, Midline and Endline */

local control_variables_attrit "age E_Denrolled partner M_children"
local control_variables_attrit2 "igaALL control_bodyALL aspirationALL"

xi, prefix(_B) i.branch_name


* Preparing the regressions

global interact ""
foreach x in `control_variables_attrit' {
    gen `x'INT=`x'*treatment
    global interact "$interact `x'INT"
}
global interact2 ""
foreach x in `control_variables_attrit2' {
    gen `x'INT2=`x'*treatment
    global interact2 "$interact2 `x'INT2"
}

reg `attrition' treatment, cluster(villid)
getreg treatment, name(t1)
getreg _cons, name(c1)
sum `attrition' if e(sample)
global mean_att1=r(mean)
betaformat mean_att1

reg `attrition' treatment _B*, cluster(villid)
getreg treatment, name(t2)
getreg _cons, name(c2)
sum `attrition' if e(sample)
global mean_att2=r(mean)
betaformat mean_att2

reg `attrition' treatment `control_variables_attrit' `control_variables_attrit2' $interact $interact2 _B*, cluster(villid)
getreg treatment, name(t4b)
getreg _cons, name(c4b)
foreach x in `control_variables_attrit' `control_variables_attrit2' $interact $interact2 {
    getreg `x', name(`x'4b)
}
sum `attrition' if e(sample)
global mean_att4b=r(mean)
betaformat mean_att4b
test $interact $interact2
global F4b=r(F)
betaformat F4b
test $interact $interact2
global Fp4b=r(p)
betaformat Fp4b


*** Generating the output ***

local i=1
qui gen var=""
gen mod1=""
gen mod2=""
gen mod3=""
gen mod3b=""
gen mod4=""
gen mod4b=""
gen mod5=""

local i=3
foreach x in `control_variables_attrit' `control_variables_attrit2' {
    local j=`i'+1
    qui replace var="`x'" in `i'
    qui replace mod3="${beta_`x'3}${pstar_`x'3}" in `i'
    qui replace mod3="${se_`x'3}" in `j'
	qui replace mod3b="${beta_`x'3b}${pstar_`x'3b}" in `i'
    qui replace mod3b="${se_`x'3b}" in `j'
    qui replace mod4="${beta_`x'4}${pstar_`x'4}" in `i'
    qui replace mod4="${se_`x'4}" in `j'
	qui replace mod4b="${beta_`x'4b}${pstar_`x'4b}" in `i'
    qui replace mod4b="${se_`x'4b}" in `j'
    qui replace mod5="${beta_`x'5}${pstar_`x'5}" in `i'
    qui replace mod5="${se_`x'5}" in `j'
    local i=`i'+2
}

*local i=3
foreach x in $interact $interact2 {
    local j=`i'+1
    qui replace var="`x'" in `i'
    qui replace mod4="${beta_`x'4}${pstar_`x'4}" in `i'
    qui replace mod4="${se_`x'4}" in `j'
	qui replace mod4b="${beta_`x'4b}${pstar_`x'4b}" in `i'
    qui replace mod4b="${se_`x'4b}" in `j'
    qui replace mod5="${beta_`x'5}${pstar_`x'5}" in `i'
    qui replace mod5="${se_`x'5}" in `j'
    local i=`i'+2
}
local e=`i'+1
local outcome_mean=`i'+4
local obs=`i'+6
local f=`i'+7

local k=1
qui replace var="Treatment" in 1
qui replace var="Constant" in `i'
qui replace mod4="${F4}/${Fp4}" in `f'
qui replace mod4b="${F4b}/${Fp4b}" in `f'
while `k'<=5 {
    qui replace mod`k'="${beta_t`k'}${pstar_t`k'}" in 1
    qui replace mod`k'="${se_t`k'}" in 2
    qui replace mod`k'="${beta_c`k'}${pstar_c`k'}" in `i'
    qui replace mod`k'="${se_c`k'}" in `e'
    qui replace mod`k'="${N_t`k'}" in `obs'
	qui replace mod`k'="${mean_att`k'}" in `outcome_mean'
	if `k'==3 | `k'==4 {
		qui replace mod`k'b="${beta_t`k'b}${pstar_t`k'b}" in 1
		qui replace mod`k'b="${se_t`k'b}" in 2
		qui replace mod`k'b="${beta_c`k'b}${pstar_c`k'b}" in `i'
		qui replace mod`k'b="${se_c`k'b}" in `e'
		qui replace mod`k'b="${N_t`k'b}" in `obs'
		qui replace mod`k'b="${mean_att`k'b}" in `outcome_mean'
	}
    local k=`k'+1
}

browse var mod1 mod2 mod4b
cap drop var mod1 mod2 mod3 mod3b mod4 mod3b mod5






***** Table T3 *****

cap drop both_training
gen both_training=.
replace both_training=0 if lifeskillMOREfew==1 | lifeskillMOREfew==0
replace both_training=0 if livelihoodMOREfew==1 | livelihoodMOREfew==0
replace both_training=1 if livelihoodMOREfew==1 & lifeskillMOREfew==1

cap drop Qboth_training
gen Qboth_training=.
replace Qboth_training=0 if QlifeskillMOREfew==1 | QlifeskillMOREfew==0
replace Qboth_training=0 if QlivelihoodMOREfew==1 | QlivelihoodMOREfew==0
replace Qboth_training=1 if QlivelihoodMOREfew==1 & QlifeskillMOREfew==1

gen Roften3WEEK=often3WEEK 
gen Roften1WEEK=often1WEEK
gen RlifeskillMOREfew=lifeskillMOREfew 
gen RlivelihoodMOREfew=livelihoodMOREfew 
gen Rboth_training=both_training


*** Defining global topics ***

local CLUB_DESCRIPTIVES "C_clubheard C_clubparticipateIMP C_stillgoing often3WEEK often1WEEK lifeskillMOREfew livelihoodMOREfew both_training"

***Preparing the loops ***

global topics "CLUB_DESCRIPTIVES"

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of global varsestimation {

	* At follow-up

    reg R`var' treatment if follow_up==1 & age!=., cluster(villid)
    getreg treatment, name(dR`var')
	
	sum R`var' if follow_up==1 & age!=. & treatment==1
    local sd1_R`var'=r(sd)
	global sd1_R`var'=r(sd)
	local Var1_R`var'=r(Var)
    local mean1_R`var'=r(mean)
	global mean1_R`var'=r(mean)
	betaformat sd1_R`var'
	betaformat mean1_R`var'
        
    sum R`var' if follow_up==1 & age!=. & treatment==0
    local sd0_R`var'=r(sd)
	global sd0_R`var'=r(sd)
	local Var0_R`var'=r(Var)
    local mean0_R`var'=r(mean)
	global mean0_R`var'=r(mean)
	betaformat sd0_R`var'
	betaformat mean0_R`var'
        
    global nd_R`var'=(`mean1_R`var''-`mean0_R`var'')/(sqrt(`Var1_R`var''+`Var0_R`var''))
    betaformat nd_R`var'

	* At endline

	reg Q`var' treatment if endline==1 & age!=. & panel==1, cluster(villid)
    getreg treatment, name(dQ`var')
	
	sum Q`var' if endline==1 & age!=. & treatment==1 & panel==1
    local sd1_Q`var'=r(sd)
	global sd1_Q`var'=r(sd)
	local Var1_Q`var'=r(Var)
    local mean1_Q`var'=r(mean)
	global mean1_Q`var'=r(mean)
	betaformat sd1_Q`var'
	betaformat mean1_Q`var'
        
    sum Q`var' if endline==1 & age!=. & treatment==0 & panel==1
    local sd0_Q`var'=r(sd)
	global sd0_Q`var'=r(sd)
	local Var0_Q`var'=r(Var)
    local mean0_Q`var'=r(mean)
	global mean0_Q`var'=r(mean)
	betaformat sd0_Q`var'
	betaformat mean0_Q`var'
        
    global nd_Q`var'=(`mean1_Q`var''-`mean0_Q`var'')/(sqrt(`Var1_Q`var''+`Var0_Q`var''))
    betaformat nd_Q`var'
	
}

*** Generating the output ***

gen topic=""
gen var=""
gen mean1_mid=""
gen mean0_mid=""
gen diff_mid=""
gen norm_diff_mid=""
gen e1=""
gen mean1_end=""
gen mean0_end=""
gen diff_end=""
gen norm_diff_end=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "`x'"=="`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace mean1_mid="${mean1_R`x'}" in `i'
            qui replace mean1_mid="[${sd1_R`x'}]" in `j'
			
			qui replace mean0_mid="${mean0_R`x'}" in `i'
            qui replace mean0_mid="[${sd0_R`x'}]" in `j'
			
			qui replace diff_mid="${beta_dR`x'}${pstar_dR`x'}" in `i'
            qui replace diff_mid="${se_dR`x'}" in `j'
			
			qui replace norm_diff_mid="${nd_R`x'}" in `i'
			
			qui replace mean1_end="${mean1_Q`x'}" in `i'
            qui replace mean1_end="[${sd1_Q`x'}]" in `j'
			
			qui replace mean0_end="${mean0_Q`x'}" in `i'
            qui replace mean0_end="[${sd0_Q`x'}]" in `j'
			
			qui replace diff_end="${beta_dQ`x'}${pstar_dQ`x'}" in `i'
            qui replace diff_end="${se_dQ`x'}" in `j'
			
			qui replace norm_diff_end="${nd_Q`x'}" in `i'
           
			local i=`i'+2
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

*browse variables of interest 
br topic var mean1_mid mean0_mid diff_mid norm_diff_mid e1 mean1_end mean0_end diff_end norm_diff_end

cap drop topic var mean1_mid mean0_mid diff_mid norm_diff_mid e1 mean1_end mean0_end diff_end norm_diff_end




