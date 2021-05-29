use "$Data/ELA.dta", clear



***** Figure 2 *****

* Indices at baseline

twoway histogram igaALL || kdensity igaALL
twoway histogram control_bodyALL || kdensity control_bodyALL
twoway histogram aspirationALL || kdensity aspirationALL
d
corr igaALL control_bodyALL aspirationALL







***** Figure 3 *****

* Spider graphs
gen Entreprenship=""
replace Entreprenship="Run own business" in 1
replace Entreprenship="Identify business opportunities" in 2
replace Entreprenship="Obtain credit" in 3
replace Entreprenship="Save to invest" in 4
replace Entreprenship="Manage employees" in 5
replace Entreprenship="Manage financials" in 6
replace Entreprenship="Bargain - inputs" in 7
replace Entreprenship="Bargain - outputs" in 8
replace Entreprenship="Protect assets" in 9
replace Entreprenship="Collecting money" in 10

local outcome "Entreprenship"

* Choose 'R' or 'Q' for midline or endline
local round "Q"

gen zero=.
gen graphtreatment=.
gen cimax=.
gen cimin=.

local n=1
if "`outcome'"=="Entreprenship" local m=10

while `n'<=`m' {
	
	
	reg `round'`outcome'_`n' `outcome'_`n' treatment $controls _B*  if panel==1 & age!=., cluster(villid) 
	
	global cimin=_b[treatment] - invttail(e(df_r),0.025)*_se[treatment]
    global cimax=_b[treatment] + invttail(e(df_r),0.025)*_se[treatment]
    
    global treatment=_b[treatment]

    qui replace graphtreatment=$treatment in `n'
    qui replace cimax=$cimax in `n'
    qui replace cimin=$cimin in `n'
	
	replace zero=0 in `n'

    local n=`n'+1
}

drop if `outcome'==""
egen minr=rowmin(graphtreatment cimax cimin)
egen min=min(minr)
egen maxr=rowmax(graphtreatment cimax cimin)
egen max=max(maxr)
sum max
local maxhelp=r(mean)
local max=round(r(mean),0.1)
if `max'<`maxhelp' local max=`max'+0.1
sum min
local minhelp=r(mean)
local min=round(r(mean),0.1)
if `min'>`minhelp' local min=`min'+0.1

if abs(`max')>=abs(`min') local bound=`max'
if abs(`max')<abs(`min') local bound=`min'
local bound=abs(`bound')
replace min=0-`bound'
local min=0-`bound'
local max=0+`bound'

*outout
radar Entreprenship graphtreatment zero cimax cimin, r(`min' `min' 0 `max') lc(red black blue blue) lp(solid solid dash dash) lw(thick medthick medium medium) labsize(*.7) note("") graphregion(color(white)) legend(label(1 ITT) label(3 95% Confidence Interval)) legend(order(1 3))





