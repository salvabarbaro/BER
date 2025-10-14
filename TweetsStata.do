use "Tweets_replication.dta", clear
capture erase "Results\Tweets_maincoefs.csv"
capture erase "Results\Tweets_allcoefs.csv"
est drop _all

capture log close
log using "Results\Tweets per day.smcl", replace
gen dow = dow(date)
sum tweetsday
eststo, title(weekMPK_dow): reg tweetsday weekMPK date i.dow, robust
eststo, title(3daysbeforeMPK_dow): reg tweetsday beforeMPK date i.dow, robust
eststo, title(dayMPKtill3daysafter_dow): reg tweetsday afterMPK date i.dow, robust

esttab using "Results\Tweets_maincoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(weekMPK_dow 3daysbeforeMPK_dow dayMPKtill3daysafter_dow) prehead("Correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01") keep(beforeMPK weekMPK weekMPK afterMPK) replace

esttab using "Results\Tweets_allcoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(weekMPK_dow 3daysbeforeMPK_dow dayMPKtill3daysafter_dow) prehead("Correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01")  replace
est drop _all
log close
translate "Results\Tweets per day.smcl" "Results\Tweets per day.pdf"
erase "Results\Tweets per day.smcl"