use "data.dta", clear

* compute the different patterns of choice and label them
gen preferencePatternStrict = .
replace preferencePatternStrict = 1 if choice == 0 & valuationClose >= valuationDistant
replace preferencePatternStrict = 2 if choice == 1 & valuationClose <= valuationDistant
replace preferencePatternStrict = 3 if choice == 0 & valuationClose < valuationDistant
replace preferencePatternStrict = 4 if choice == 1 & valuationClose > valuationDistant

label define preferencePatternStrictL ///
    1 "Consistent omega_s" ///
    2 "Consistent omega_x" ///
    3 "Standard reversal" ///
    4 "Counter reversal"

label values preferencePatternStrict preferencePatternStrictL
    
* frequencies and proportions of patterns + McNemar's tests
* for each treatment, for each amount, and at the aggregate
* also controlling for knowledge and social distance
* = Section 4.1, Table 1, Figure 2 and Table 2

* CAREFUL! sometimes there are no standard or counter reversals
* in this case the f matrix will have missing values, which leads to the mcci command to fail, hence the capture
* in that case, we need to redo the computation by hand with mcci, using 0 instead of the missing value
* using mcc would have been nice but the data would need to be in a completely different shape

foreach treatment in 1 2 {
    display "TREATMENT `treatment'"
    foreach amounts in 1 2 3 4 5 {
        display "---AMOUNT `amounts'"
        tabulate preferencePatternStrict if treatment == `treatment' & amounts == `amounts', matcell(f)
        capture mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

        display "---AMOUNT `amounts' with knowledge"
        tabulate preferencePatternStrict if treatment == `treatment' & amounts == `amounts' & knowledgeClose > 2 & knowledgeDistant > 2, matcell(f)
        capture mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

        display "---AMOUNT `amounts' with knowledge and weak social distance confirmed"
        tabulate preferencePatternStrict if treatment == `treatment' & amounts == `amounts' & knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose >= IOSDistant, matcell(f)
        capture mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

        display "---AMOUNT `amounts' with knowledge and strong social distance confirmed"
        tabulate preferencePatternStrict if treatment == `treatment' & amounts == `amounts' & knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose > IOSDistant, matcell(f)
        capture mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'
    }
    display "---AGGREGATE"
    tabulate preferencePatternStrict if treatment == `treatment', matcell(f)
    mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

    display "---AGGREGATE with knowledge"
    tabulate preferencePatternStrict if treatment == `treatment' & knowledgeClose > 2 & knowledgeDistant > 2, matcell(f)
    mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

    display "---AGGREGATE with knowledge and weak social distance confirmed"
    tabulate preferencePatternStrict if treatment == `treatment' & knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose >= IOSDistant, matcell(f)
    mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'

    display "---AGGREGATE with knowledge and strong social distance confirmed"
    tabulate preferencePatternStrict if treatment == `treatment' & knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose > IOSDistant, matcell(f)
    mcci `=f[1,1]' `=f[3,1]' `=f[4,1]' `=f[2,1]'
}


* testing the IOS
* = Section 4.2, Figure 3
preserve
collapse (first) IOSClose (first) IOSDistant, by(treatment id) 
bysort treatment: summarize IOSClose IOSDistant
bysort treatment: signrank IOSClose = IOSDistant
restore


* conditional logit model
* = Section 5, Table 3

preserve

rename amounts pair
generate amountClose = 5
generate amountDistant = pair + 5
generate choiceFromValuation = .
replace choiceFromValuation = 1 if valuationDistant > valuationClose
replace choiceFromValuation = 0 if valuationDistant < valuationClose
egen subjectid = group(id)
egen subjectpairid = group(id pair)
reshape long valuation IOS orderValuation orderIOS knowledge orderKnowledge amount, i(subjectpairid) j(option) string
rename choice choicelong
generate choice = (choicelong == 0 & amount == 5) | (choicelong == 1 & amount != 5)
rename choiceFromValuation choiceFromValuationlong
generate choiceFromValuation = (choiceFromValuationlong == 0 & amount == 5) | (choiceFromValuationlong == 1 & amount != 5)

* Faculty
clogit choice amount IOS if treatment == 1, group(subjectpairid) vce(cluster subjectid)
clogit choiceFromValuation amount IOS if treatment == 1, group(subjectpairid) vce(cluster subjectid)
* Charity
clogit choice amount IOS if treatment == 2, group(subjectpairid) vce(cluster subjectid)
clogit choiceFromValuation amount IOS if treatment == 2, group(subjectpairid) vce(cluster subjectid)

restore


* getting the conditional reversal rates (reply to Reviewer 2, second-round R&R)
* = Appendix 2, Table 1
egen num_chosen_distant = sum(choice), by(id)
generate num_chosen_close = 5 - num_chosen_distant
generate std_rev = preferencePatternStrict == 3
generate counter_rev = preferencePatternStrict == 4
egen num_std_rev = sum(std_rev), by(id)
egen num_counter_rev = sum(counter_rev), by(id)

generate rate_std_rev = num_std_rev / num_chosen_close
generate rate_counter_rev = num_counter_rev / num_chosen_distant

preserve

collapse (first)    num_chosen_close num_chosen_distant ///
                    num_std_rev num_counter_rev ///
                    rate_std_rev rate_counter_rev ///
                    IOSClose IOSDistant ///
                    knowledgeClose knowledgeDistant, by(treatment id)

format rate_std_rev rate_counter_rev %5.4f

by treatment: summarize rate_std_rev rate_counter_rev, format
by treatment: summarize rate_std_rev rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2, format
by treatment: summarize rate_std_rev rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose >= IOSDistant, format
by treatment: summarize rate_std_rev rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose > IOSDistant, format

bysort treatment: signrank rate_std_rev = rate_counter_rev
bysort treatment: signrank rate_std_rev = rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2
bysort treatment: signrank rate_std_rev = rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose >= IOSDistant
bysort treatment: signrank rate_std_rev = rate_counter_rev if knowledgeClose > 2 & knowledgeDistant > 2 & IOSClose > IOSDistant

restore