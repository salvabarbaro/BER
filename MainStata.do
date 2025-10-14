use "Main_data_set_replication.dta", clear
*---Replace vacdev with correct computation
replace vacdev = (exp(lnvac) - exp(lnvacfed)) / exp(lnvacfed)

capture erase "Results\maincoefs.csv"
capture erase "Results\allcoefs.csv"
est drop _all
capture log close
log using "Results\Main.smcl", replace
******* Testing the proportional odds assumption *******
omodel logit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength east berlin date if (wave == 2 | wave == 3)

eststo, title(model1): ologit index lninc lnincfed if (wave == 2 | wave == 3), or cluster(date) nolog
eststo, title(model2): ologit index lninc lnincfed date if (wave == 2 | wave == 3), or cluster(date) nolog
eststo, title(model3): ologit index lninc lnincfed i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog
eststo, title(model4): ologit index lninc lnincfed att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog
eststo, title(model5): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog

* Without NRW & Bavaria
eststo, title(NoNRWBav): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3) & nrwbayern == 0, or cluster(date) nolog

eststo, title(EastBerlin): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength east berlin i.weeknr if (wave == 2 | wave == 3) & nrwbayern == 0, or cluster(date) nolog

* Robustness check: Fixed-effects (conditional) ordered logistic regression
eststo, title(fixed1): feologit index lninc lnincfed date if wave == 2 | wave == 3, or group(state) nolog
eststo, title(fixed2): feologit index lninc lnincfed lnvac lnvacfed prevac date if wave == 2 | wave == 3, or group(state) nolog

* Zooming in on the week around MPKs (3 days before an MPK till 3 days after)
eststo, title(weekMPK): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength date if (wave == 2 | wave == 3) & weekMPK == 1, or cluster(date) nolog

* Up till 3 days after a MPK
eststo, title(AfterMPK): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength date if (wave == 2 | wave == 3) & dayssinceMPK <= 3, or cluster(date) nolog

* Deviations instead of logs
eststo, title(deviations): ologit index incdev vacdev att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog


esttab using "Results\maincoefs.csv", csv b(3) se(3) ar(3)  nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) eform ///
mtitles(model1 model2 model3 model4 model5 NoNRWBav EastBerlin fixed1 fixed2 weekMPK AfterMPK deviations) prehead("Ordered logit - All results are in odds ratios" "Significance: * = .1 / ** = .05 / *** = .01") keep(lninc lnincfed lnvac lnvacfed incdev vacdev) replace
esttab using "Results\allcoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) eform ///
mtitles(model1 model2 model3 model4 model5 NoNRWBav EastBerlin fixed1 fixed2 weekMPK AfterMPK deviations) prehead("Ordered logit - All results are in odds ratios" "Significance: * = .1 / ** = .05 / *** = .01") replace

esttab using "Results\Table_maincoefs_confidence_intervals.txt", star(* .1 ** .05 *** .01) cells(b(fmt(3) star) ci(fmt(3) par)) nonotes gaps eform ///
mtitles(model1 model2 model3 model4 model5 NoNRWBav EastBerlin fixed1 fixed2 weekMPK AfterMPK deviations) prehead("Ordered logit - All results are in odds ratios" "Significance: * = .1 / ** = .05 / *** = .01") keep(lninc lnvac incdev vacdev) replace
esttab using "Results\Table_allcoefs_confidence_intervals.txt", cells(b(fmt(3) star) ci(fmt(3) par)) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) eform ///
mtitles(model1 model2 model3 model4 model5 NoNRWBav EastBerlin fixed1 fixed2 weekMPK AfterMPK deviations) prehead("Ordered logit - All results are in odds ratios" "Significance: * = .1 / ** = .05 / *** = .01") keep(lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength date) replace
est drop _all

eststo, title(model4): ologit index lninc lnincfed att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog
eststo, title(model5): ologit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength i.weeknr if (wave == 2 | wave == 3), or cluster(date) nolog
esttab using "Results\Table with full results_confidence_intervals.txt", cells(b(fmt(3) star) ci(fmt(3) par)) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) eform ///
mtitles(model4 model5) prehead("Ordered logit - All results are in odds ratios" "Significance: * = .1 / ** = .05 / *** = .01") keep(lninc lnvac att_t_fed FKM21 econ_strength) replace

log close
translate "Results\Main.smcl" "Results\Main.pdf"
erase "Results\Main.smcl"
