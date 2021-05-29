 use "$Data/ELA.dta", clear

***** Table TA1 *****

local DEMOGRAPHICS "age E_Denrolled back_school M_children partner"

local IGA "Entrep_total selfempl empl worry_job Expenditure_totDF"

local CONTROL "R_sexunwilling sex_pregnancy Rhiv_skillsALT always_condom other_contraceptive"

local ASPIRATIONS "empowerment M_idealmarry_ageF M_idealbaby_no M_idealbaby_ageF M_idealdaught_marry M_idealson_marry"


*** Defining global topics ***

global topics      "DEMOGRAPHICS IGA CONTROL ASPIRATIONS"


*** Preparing the loops ***

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of global varsestimation {

    reg `var' treatment if panel==1 & age!=. & Q`var'!=., cluster(villid)
    getreg treatment, name(d`var')
	
	reg `var' treatment, cluster(villid)
    getreg treatment, name(db`var')
	
	sum `var' if treatment==1 & panel==1 & age!=. & Q`var'!=.
    local sd1_`var'=r(sd)
	global sd1_`var'=r(sd)
	local Var1_`var'=r(Var)
    local mean1_`var'=r(mean)
	global mean1_`var'=r(mean)
	betaformat sd1_`var'
	betaformat mean1_`var'
        
    sum `var' if treatment==0 & panel==1 & age!=. & Q`var'!=.
    local sd0_`var'=r(sd)
	global sd0_`var'=r(sd)
	local Var0_`var'=r(Var)
    local mean0_`var'=r(mean)
	global mean0_`var'=r(mean)
	betaformat sd0_`var'
	betaformat mean0_`var'
	
	sum `var' if treatment==1
    local sdb1_`var'=r(sd)
	global sdb1_`var'=r(sd)
	local Varb1_`var'=r(Var)
    local meanb1_`var'=r(mean)
	global meanb1_`var'=r(mean)
	betaformat sdb1_`var'
	betaformat meanb1_`var'
        
    sum `var' if treatment==0
    local sdb0_`var'=r(sd)
	global sdb0_`var'=r(sd)
	local Varb0_`var'=r(Var)
    local meanb0_`var'=r(mean)
	global meanb0_`var'=r(mean)
	betaformat sdb0_`var'
	betaformat meanb0_`var'
        
    global nd_`var'=(`mean1_`var''-`mean0_`var'')/(sqrt(`Var1_`var''+`Var0_`var''))
    betaformat nd_`var'
	
	global ndb_`var'=(`meanb1_`var''-`meanb0_`var'')/(sqrt(`Varb1_`var''+`Varb0_`var''))
    betaformat ndb_`var'

}


*** Generating the output ***

foreach var in topic var mean1 mean0 diff norm_diff e1 {
	cap drop `var'
}
gen topic=""
gen var=""
gen mean1=""
gen mean0=""
gen diff=""
gen norm_diff=""
gen diff_base=""
gen norm_diff_base=""
gen e1=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "D`x'"=="D`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace mean1="${meanb1_`x'}" in `i'
            qui replace mean1="[${sdb1_`x'}]" in `j'
			
			qui replace mean0="${meanb0_`x'}" in `i'
            qui replace mean0="[${sdb0_`x'}]" in `j'
			
			qui replace diff="${beta_d`x'}${pstar_d`x'}" in `i'
            qui replace diff="${se_d`x'}" in `j'
			
			qui replace norm_diff="${nd_`x'}" in `i'
			
			qui replace diff_base="${beta_db`x'}${pstar_db`x'}" in `i'
            qui replace diff_base="${se_db`x'}" in `j'
			
			qui replace norm_diff_base="${ndb_`x'}" in `i'
           
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
br topic var mean1 mean0 diff_base norm_diff_base e1 diff norm_diff






***** Table TA2 *****

gen HHBRACloan=HHF_loanbrac
replace HHBRACloan=1 if HHBRACloan>0 & HHBRACloan!=.
replace HHBRACloan=0 if HHF_loan==0
gen QHHBRACloan=1

local DEMOGRAPHICS "age E_Denrolled empowerment"

local IGA "Entrep_total selfempl empl satisfaction_income worry_job"

local CONTROL "M_children partner R_sexunwilling sex_pregnancy Rhiv_skillsALT always_condom"

local INDICES "iga control_body aspiration"

local LOAN "HHBRACloan"

*** Defining global topics ***
global topics      "DEMOGRAPHICS IGA CONTROL INDICES LOAN"

*** Preparing the loops ***
global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

gen member=RC_clubparticipateIMP

foreach var of global varsestimation {

    reg `var' member if panel==1, cluster(villid)
    getreg member, name(d`var')
	
	sum `var' if member==1 & panel==1
    local sd1_`var'=r(sd)
	global sd1_`var'=r(sd)
	local Var1_`var'=r(Var)
    local mean1_`var'=r(mean)
	global mean1_`var'=r(mean)
	betaformat sd1_`var'
	betaformat mean1_`var'
        
    sum `var' if member==0 & panel==1
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

drop topic var mean1 mean0 diff norm_diff
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

tab member if panel==1

*browse variables of interest 
br topic var mean1 mean0 diff norm_diff





***** Table TA4 *****

* Pooled Regressions

save help.dta, replace
keep if endline==1 & panel==1
keep id iga control_body aspiration treatment _B* age Qiga Qcontrol_body Qaspiration villid
rename Qiga Xiga
rename Qcontrol_body Xcontrol_body
rename Qaspiration Xaspiration
save help1.dta, replace 

use help.dta, clear
keep if panel==1 & endline==1
keep id iga control_body aspiration treatment _B* age Riga Rcontrol_body Raspiration villid
rename Riga Xiga 
rename Rcontrol_body Xcontrol_body
rename Raspiration Xaspiration
gen time=0

append using help1.dta
replace time=1 if time==.
drop if age==.

gen treatment1=treatment
replace treatment1=0 if time==1
gen treatment2=treatment
replace treatment2=0 if time==0

local INDICES "iga control_body aspiration"

global topics "INDICES"

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of global varsestimation  {
	reg X`var' `var' treatment1 treatment2 time age _B*, cluster(villid)
}

erase help1.dta

* Returning to the original data set used
use help.dta, clear
erase help.dta





***** Table TA5 & TA6 *****

* Heterogeneity analysis

local SUMMARY "iga control_body aspiration"

local IGA "Entrep_total selfempl empl"

local WELFARE "worry_job Expenditure_totDF "

local EDUCATION "E_Denrolled study_hours back_school"

local CHILDBEARING_MARRIAGE "M_children partner"

local RAPE "R_sexunwilling"

local KNOWLEDGE "sex_pregnancy Rhiv_skillsALT"

local CONTRACEPTION "always_condom other_contraceptive"

local EMPOWERMENT "empowerment"

local MARRIAGE "M_idealmarry_ageF M_idealmarry_ageM"

local CHILD_BEARING "M_idealbaby_no M_idealbaby_ageF"

local ASPIRATIONS "M_idealdaught_marry M_idealson_marry"


*** Defining global topics ***

global topics      "SUMMARY EDUCATION"


*** Preparing the loops ***

global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}


* ITT Heterogeneity - ANCOVA

* rural

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & rural==1, cluster(villid)
	getreg treatment, name(ittMr1`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & rural==1, cluster(villid)
	getreg treatment, name(ittEr1`var')
}

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & rural==0, cluster(villid)
	getreg treatment, name(ittMr0`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & rural==0, cluster(villid)
	getreg treatment, name(ittEr0`var')
}


* rich

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & rich==1, cluster(villid)
	getreg treatment, name(ittMa1`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & rich==1, cluster(villid)
	getreg treatment, name(ittEa1`var')
}

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & rich==0, cluster(villid)
	getreg treatment, name(ittMa0`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & rich==0, cluster(villid)
	getreg treatment, name(ittEa0`var')
}


* young

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & below16==1, cluster(villid)
	getreg treatment, name(ittMy1`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & below16==1, cluster(villid)
	getreg treatment, name(ittEy1`var')
}

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & below16==0, cluster(villid)
	getreg treatment, name(ittMy0`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & below16==0, cluster(villid)
	getreg treatment, name(ittEy0`var')
}


* low marriage age (parental)

gen marryyoung=.
replace marryyoung=1 if HHM_idealmarry_ageF<25 & HHM_idealmarry_ageF!=.
replace marryyoung=0 if HHM_idealmarry_ageF>=25 & HHM_idealmarry_ageF!=.


foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & marryyoung==1, cluster(villid)
	getreg treatment, name(ittMm1`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & marryyoung==1, cluster(villid)
	getreg treatment, name(ittEm1`var')
}

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & marryyoung==0, cluster(villid)
	getreg treatment, name(ittMm0`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & marryyoung==0, cluster(villid)
	getreg treatment, name(ittEm0`var')
}

* man responsible for earning income (parental)

gen manincome=.
replace manincome=1 if HHM_whoshouldearn==1 & HHM_whoshouldearn!=.
replace manincome=0 if HHM_whoshouldearn!=1 & HHM_whoshouldearn!=.


foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & manincome==1, cluster(villid)
	getreg treatment, name(ittMi1`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & manincome==1, cluster(villid)
	getreg treatment, name(ittEi1`var')
}

foreach var of global varsestimation  {
	reg R`var' `var' _B* $controls treatment if panel==1 & manincome==0, cluster(villid)
	getreg treatment, name(ittMi0`var')
}

foreach var of global varsestimation  {
	reg Q`var' `var' _B* $controls treatment if panel==1 & manincome==0, cluster(villid)
	getreg treatment, name(ittEi0`var')
}



*** Generating the output ***

foreach var in topic var e1 e2 {
	cap drop `var'
}
gen topic=""
gen var=""
gen ITT_MIDrural=""
gen ITT_ENDrural=""
gen ITT_MIDurban=""
gen ITT_ENDurban=""
gen e1=""
gen ITT_MIDrich=""
gen ITT_ENDrich=""
gen e2=""
gen ITT_MIDpoor=""
gen ITT_ENDpoor=""
gen e3=""
gen ITT_MIDyoung=""
gen ITT_ENDyoung=""
gen e4=""
gen ITT_MIDold=""
gen ITT_ENDold=""
gen e5=""
gen ITT_MIDmarryy=""
gen ITT_ENDmarryy=""
gen e6=""
gen ITT_MIDmarryo=""
gen ITT_ENDmarryo=""
gen e7=""
gen ITT_MIDincomem=""
gen ITT_ENDincomem=""
gen e8=""
gen ITT_MIDincomew=""
gen ITT_ENDincomew=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "D`x'"=="D`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace ITT_MIDrural="${beta_ittMr1`x'}${pstar_ittMr1`x'}" in `i'
            qui replace ITT_MIDrural="${se_ittMr1`x'}" in `j'
			
			qui replace ITT_ENDrural="${beta_ittEr1`x'}${pstar_ittEr1`x'}" in `i'
            qui replace ITT_ENDrural="${se_ittEr1`x'}" in `j'
			
            qui replace ITT_MIDurban="${beta_ittMr0`x'}${pstar_ittMr0`x'}" in `i'
            qui replace ITT_MIDurban="${se_ittMr0`x'}" in `j'
			
			qui replace ITT_ENDurban="${beta_ittEr0`x'}${pstar_ittEr0`x'}" in `i'
            qui replace ITT_ENDurban="${se_ittEr0`x'}" in `j'
			
            qui replace ITT_MIDrich="${beta_ittMa1`x'}${pstar_ittMa1`x'}" in `i'
            qui replace ITT_MIDrich="${se_ittMa1`x'}" in `j'
            
			qui replace ITT_ENDrich="${beta_ittEa1`x'}${pstar_ittEa1`x'}" in `i'
            qui replace ITT_ENDrich="${se_ittEa1`x'}" in `j'
			
			qui replace ITT_MIDpoor="${beta_ittMa0`x'}${pstar_ittMa0`x'}" in `i'
            qui replace ITT_MIDpoor="${se_ittMa0`x'}" in `j'
            
			qui replace ITT_ENDpoor="${beta_ittEa0`x'}${pstar_ittEa0`x'}" in `i'
            qui replace ITT_ENDpoor="${se_ittEa0`x'}" in `j'
            
			qui replace ITT_MIDyoung="${beta_ittMy1`x'}${pstar_ittMy1`x'}" in `i'
            qui replace ITT_MIDyoung="${se_ittMy1`x'}" in `j'
            
			qui replace ITT_ENDyoung="${beta_ittEy1`x'}${pstar_ittEy1`x'}" in `i'
            qui replace ITT_ENDyoung="${se_ittEy1`x'}" in `j'
			
			qui replace ITT_MIDold="${beta_ittMy0`x'}${pstar_ittMy0`x'}" in `i'
            qui replace ITT_MIDold="${se_ittMy0`x'}" in `j'
            
			qui replace ITT_ENDold="${beta_ittEy0`x'}${pstar_ittEy0`x'}" in `i'
            qui replace ITT_ENDold="${se_ittEy0`x'}" in `j'

			qui replace ITT_MIDmarryy="${beta_ittMm1`x'}${pstar_ittMm1`x'}" in `i'
            qui replace ITT_MIDmarryy="${se_ittMm1`x'}" in `j'

			qui replace ITT_ENDmarryy="${beta_ittEm1`x'}${pstar_ittEm1`x'}" in `i'
            qui replace ITT_ENDmarryy="${se_ittEm1`x'}" in `j'
            
			qui replace ITT_MIDmarryo="${beta_ittMm0`x'}${pstar_ittMm0`x'}" in `i'
            qui replace ITT_MIDmarryo="${se_ittMm0`x'}" in `j'

			qui replace ITT_ENDmarryo="${beta_ittEm0`x'}${pstar_ittEm0`x'}" in `i'
            qui replace ITT_ENDmarryo="${se_ittEm0`x'}" in `j'

			qui replace ITT_MIDincomem="${beta_ittMi1`x'}${pstar_ittMi1`x'}" in `i'
            qui replace ITT_MIDincomem="${se_ittMi1`x'}" in `j'

			qui replace ITT_ENDincomem="${beta_ittEi1`x'}${pstar_ittEi1`x'}" in `i'
            qui replace ITT_ENDincomem="${se_ittEi1`x'}" in `j'
            
			qui replace ITT_MIDincomew="${beta_ittMi0`x'}${pstar_ittMi0`x'}" in `i'
            qui replace ITT_MIDincomew="${se_ittMi0`x'}" in `j'

			qui replace ITT_ENDincomew="${beta_ittEi0`x'}${pstar_ittEi0`x'}" in `i'
            qui replace ITT_ENDincomew="${se_ittEi0`x'}" in `j'
           
            
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

replace var="IGA Index" if var=="iga"
replace var="Welfare Index" if var=="welfare"
replace var="Control Over the Body Index" if var=="control_body"
replace var="Aspiration Index" if var=="aspiration"

*browse variables of interest 
br topic var ITT_MIDrural ITT_MIDurban ITT_ENDrural ITT_ENDurban e1 ITT_MIDrich ITT_MIDpoor ITT_ENDrich ITT_ENDpoor e3 ITT_MIDyoung ITT_MIDold ITT_ENDyoung ITT_ENDold e4 ITT_MIDmarryy ITT_MIDmarryo ITT_ENDmarryy ITT_ENDmarryo e5 ITT_MIDincomem ITT_MIDincomew ITT_ENDincomem ITT_ENDincomew








***** Table TA7 *****

* Social desirability score

gen SDSscore=Attitude
gen T_SDSscore=treatment*SDSscore

reg Riga iga _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)
reg Qiga iga _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)

reg Rcontrol_body control_body _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)
reg Qcontrol_body control_body _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)

reg Raspiration aspiration _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)
reg Qaspiration aspiration _B* $controls treatment SDSscore T_SDSscore if panel==1, cluster(villid)
