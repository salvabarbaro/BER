use "Tweets_replication.dta", clear
capture erase "Results\Tweets_maincoefs.csv"
capture erase "Results\Tweets_allcoefs.csv"
est drop _all

capture log close
log using "Results\Tweets per day.smcl", replace
sum tweetsday
eststo, title(dayMPK): reg tweetsday MPK date, robust
eststo, title(weekMPK): reg tweetsday weekMPK date, robust
eststo, title(3daysbeforeMPK): reg tweetsday beforeMPK date, robust
eststo, title(dayMPKtill3daysafter): reg tweetsday afterMPK date, robust

esttab using "Results\Tweets_maincoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(dayMPK weekMPK 3daysbeforeMPK dayMPKtill3daysafter) prehead("NOT correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01") keep(MPK beforeMPK weekMPK weekMPK afterMPK) replace

esttab using "Results\Tweets_allcoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(dayMPK weekMPK 3daysbeforeMPK dayMPKtill3daysafter) prehead("NOT Correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01")  replace
est drop _all

gen dow = dow(date)
eststo, title(dayMPK_dow): reg tweetsday MPK date i.dow, robust
eststo, title(weekMPK_dow): reg tweetsday weekMPK date i.dow, robust
eststo, title(3daysbeforeMPK_dow): reg tweetsday beforeMPK date i.dow, robust
eststo, title(dayMPKtill3daysafter_dow): reg tweetsday afterMPK date i.dow, robust

esttab using "Results\Tweets_maincoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(dayMPK_dow weekMPK_dow 3daysbeforeMPK_dow dayMPKtill3daysafter_dow) prehead("Correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01") keep(MPK beforeMPK weekMPK weekMPK afterMPK) append

esttab using "Results\Tweets_allcoefs.csv", csv b(3) se(3) ar(3) nonotes star(* .1 ** .05 *** .01) nogaps stats(N, fmt(0 3)) ///
mtitles(dayMPK_dow weekMPK_dow 3daysbeforeMPK_dow dayMPKtill3daysafter_dow) prehead("Correcting for day of week - Number of statements (tweets) per day" "Significance: * = .1 / ** = .05 / *** = .01")  append
est drop _all
log close
translate "Results\Tweets per day.smcl" "Results\Tweets per day.pdf"