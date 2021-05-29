use "$Data/ELA.dta", clear
*** Table T7, AT8, Figure 4 & A2 *****

* ITT
regress Riga treatment iga _B* $controls if panel==1, cluster(villid) 
regress Rcontrol_body treatment control_body _B* $controls if panel==1, cluster(villid)
regress Raspiration treatment aspiration _B* $controls if panel==1, cluster(villid)

regress Qiga treatment iga _B* $controls  if panel==1, cluster(villid) 
regress Qcontrol_body treatment control_body _B* $controls if panel==1, cluster(villid) 
regress Qaspiration treatment aspiration _B* $controls if panel==1, cluster(villid) 

* 2SLS
ivregress 2sls Riga iga _B* $controls (RC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first
ivregress 2sls Rcontrol_body control_body _B* $controls (RC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first
ivregress 2sls Raspiration aspiration _B* $controls (RC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first

ivregress 2sls Qiga iga _B* $controls (QC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first
ivregress 2sls Qcontrol_body control_body _B* $controls (QC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first
ivregress 2sls Qaspiration aspiration _B* $controls (QC_clubparticipateIMP=treatment) if panel==1, cluster(villid) first

* Spillover
reg Riga iga _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==1)), cluster(villid)
reg Riga iga _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==0)), cluster(villid)
reg Qiga iga _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==1)), cluster(villid)
reg Qiga iga _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==0)), cluster(villid)

reg Rcontrol_body control_body _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==1)), cluster(villid)
reg Rcontrol_body control_body _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==0)), cluster(villid)
reg Qcontrol_body control_body _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==1)), cluster(villid)
reg Qcontrol_body control_body _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==0)), cluster(villid)

reg Raspiration aspiration _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==1)), cluster(villid)
reg Raspiration aspiration _B* $controls treatment if panel==1 & ((treatment==0 & RC_clubparticipateIMP==0) | (treatment==1 & RC_clubparticipateIMP==0)), cluster(villid)
reg Qaspiration aspiration _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==1)), cluster(villid)
reg Qaspiration aspiration _B* $controls treatment if panel==1 & ((treatment==0 & QC_clubparticipateIMP==0) | (treatment==1 & QC_clubparticipateIMP==0)), cluster(villid)


***** MTE *****

gen Rpartclub=RC_clubparticipateIMP
gen Qpartclub=QC_clubparticipateIMP

gen square=dist_nearclub*dist_nearclub
gen dist_treat=dist_nearclub*treatment
gen sq_treat=square*treatment

mtefe  Riga iga _B* $controls (Rpartclub=dist_nearclub square dist_treat sq_treat treatment), first trimsupport(0.01)
dprobit Rpartclub dist_nearclub square treatment dist_treat sq_treat iga _B* $controls if e(sample)
probit Rpartclub dist_nearclub square treatment dist_treat sq_treat iga _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*

mtefe  Qiga iga _B* $controls (Qpartclub=dist_nearclub square dist_treat sq_treat treatment), trimsupport(0.01) prte(dist_nearclub) first
dprobit Qpartclub dist_nearclub square treatment dist_treat sq_treat iga _B* $controls if e(sample)
probit Qpartclub dist_nearclub square treatment dist_treat sq_treat iga _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*

mtefe  Rcontrol_body control_body _B* $controls (Rpartclub=dist_nearclub square dist_treat sq_treat treatment), trimsupport(0.01) prte(dist_nearclub) first
dprobit Rpartclub dist_nearclub square treatment dist_treat sq_treat control_body _B* $controls if e(sample)
probit Rpartclub dist_nearclub square treatment dist_treat sq_treat control_body _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*

mtefe  Qcontrol_body control_body _B* $controls (Qpartclub=dist_nearclub square dist_treat sq_treat treatment), trimsupport(0.01) prte(dist_nearclub) first
dprobit Qpartclub dist_nearclub square treatment dist_treat sq_treat control_body _B* $controls if e(sample)
probit Qpartclub dist_nearclub square treatment dist_treat sq_treat control_body _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*

mtefe  Raspiration aspiration _B* $controls (Rpartclub=dist_nearclub square dist_treat sq_treat treatment), trimsupport(0.01) prte(dist_nearclub) first
dprobit Rpartclub dist_nearclub square treatment dist_treat sq_treat aspiration _B* $controls if e(sample)
probit Rpartclub dist_nearclub square treatment dist_treat sq_treat aspiration _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*

mtefe  Qaspiration aspiration _B* $controls (Qpartclub=dist_nearclub square dist_treat sq_treat treatment), trimsupport(0.01) prte(dist_nearclub) first
dprobit Qpartclub dist_nearclub square treatment dist_treat sq_treat aspiration _B* $controls if e(sample)
probit Qpartclub dist_nearclub square treatment dist_treat sq_treat aspiration _B* $controls if e(sample)
predict phat if e(sample)
gen phat_1=.
replace phat_1=phat if Rpartclub==1
gen phat_0=.
replace phat_0=phat if Rpartclub==0
sum phat*
drop phat*




/*
* SUPPORT REGRESSIONS (ONLY USED IN TEXT, NO CORRESPONDING TABLE *

tobit Rincind_selfempl incind_selfempl _B* age i.treatment if panel==1, vce(cluster villid) ll(0)
margins, dydx(treatment) predict(pr(0,.))
margins, dydx(treatment) predict(e(0,.))

tobit Qincind_selfempl incind_selfempl _B* age i.treatment if panel==1, vce(cluster villid) ll(0)
margins, dydx(treatment) predict(pr(0,.))
margins, dydx(treatment) predict(e(0,.))

replace Rhsworked_year_empl=. if follow_up==0
replace Qhsworked_year_empl=. if endline==0

replace Rhsworked_year_self=. if follow_up==0
replace Qhsworked_year_self=. if endline==0

sum hsworked_year_self if panel==1 & age!=. & Qincind_selfempl!=. & treatment==0

reg Rhsworked_year_empl hsworked_year_empl _B* age treatment if panel==1, cluster(villid)
reg Qhsworked_year_empl hsworked_year_empl _B* age treatment if panel==1, cluster(villid)

reg Rhsworked_year_self hsworked_year_self _B* age treatment if panel==1, cluster(villid)
reg Qhsworked_year_self hsworked_year_self _B* age treatment if panel==1, cluster(villid)

tobit Rhsworked_year_self hsworked_year_self _B* age i.treatment if panel==1, vce(cluster villid) ll(0)
margins, dydx(treatment) predict(pr(0,.))
margins, dydx(treatment) predict(e(0,.))

tobit Qhsworked_year_self hsworked_year_self _B* age i.treatment if panel==1, vce(cluster villid) ll(0)
margins, dydx(treatment) predict(pr(0,.))
margins, dydx(treatment) predict(e(0,.))
*/




***** Table T8 *****

* Mediation Analysis

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

cap drop both_trainingIMP
gen both_trainingIMP=.
replace both_trainingIMP=both_training
replace both_trainingIMP=0 if both_training==.
gen Qboth_trainingIMP=.
replace Qboth_trainingIMP=Qboth_training
replace Qboth_trainingIMP=0 if Qboth_training==.

cap drop any_training
gen any_training=.
replace any_training=1 if lifeskillMOREfew==1 | lifeskillMOREfew==0
replace any_training=1 if livelihoodMOREfew==1 | livelihoodMOREfew==0
replace any_training=1 if livelihoodMOREfew==1 & lifeskillMOREfew==1

cap drop Qany_training
gen Qany_training=.
replace Qany_training=1 if QlifeskillMOREfew==1 | QlifeskillMOREfew==0
replace Qany_training=1 if QlivelihoodMOREfew==1 | QlivelihoodMOREfew==0
replace Qany_training=1 if QlivelihoodMOREfew==1 & QlifeskillMOREfew==1

cap drop any_trainingIMP
gen any_trainingIMP=.
replace any_trainingIMP=any_training
replace any_trainingIMP=0 if any_training==.
gen Qany_trainingIMP=.
replace Qany_trainingIMP=any_training
replace Qany_trainingIMP=0 if any_training==.

* Choose the outcome

local outcome "iga"
*local outcome "control_body"
*local outcome "aspiration"

b1x2 R`outcome' if panel==1, x1all(`outcome' _B* $controls treatment) x2all(lifeskillMOREfewIMP livelihoodMOREfewIMP) x2delta(g1=lifeskillMOREfewIMP : g2=livelihoodMOREfewIMP) x1only(treatment) cluster(villid)
b1x2 Q`outcome' if panel==1, x1all(`outcome' _B* $controls treatment) x2all(QlifeskillMOREfewIMP QlivelihoodMOREfewIMP) x2delta(g1=QlifeskillMOREfewIMP : g2=QlivelihoodMOREfewIMP) x1only(treatment) cluster(villid)







