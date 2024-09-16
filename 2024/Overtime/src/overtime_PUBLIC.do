*********************************
* NO TAX ON OVERTIME
* The Budget Lab
* Ernie Tedeschi
* Last Updated: 16 September 2024
*
* Analyzes CPS overtime data and
* creates microdata file for
* use with a tax simulator.
*********************************

clear all
set seed 5151980

global asecdir 	"/asecdir"
global asecfile	"asecmaster.dta"
global epidir	"/epidir"
global epifile	"epimaster.dta"


*********************************
* STEP 1: PULL IN IPUMS ASEC DATA
* For random draw of weeks worked data

frame create asecdata
frame change asecdata

	cd "$asecdir"
	use year wkswork1 occly ftotval asecwt if year==2023 using "$asecfile"
	
* create occupational and family income cateogies for the random draw
	egen occcat = cut(occly), at(0,10,441,961,1241,1561,1981,2061,2181,2556,2971,3551,3656,3961,4161,4256,4656,4966,5941,6131,6951,7641,8991,9431,9761,9831,9999) icodes
	egen faminc = cut(ftotval), at(-99999,5000,7500,10000,12500,15000,20000,25000,30000,35000,40000,50000,60000,75000,100000,150000,9999999) icodes
	replace faminc = faminc + 1

	keep if wkswork1 > 0 & !missing(wkswork1)

* Within cell, sort by income_cont <- continuous income measure
	sort year faminc occcat wkswork1

* Within cell, weight_norm = weight/sum(weight)
	bysort faminc occcat: egen double t_asecwt=total(asecwt)
	gen double norm_asecwt=asecwt/t_asecwt

* Within cell, weight_cum = cumulative sum of weight_norm
	bysort faminc occcat: gen double cum_norm_asecwt=sum(norm_asecwt)

	save "weeksworked.dta", replace

*********************************
* STEP 2: PULL IN EPI ORG DATA
* For all other analysis
	
frame change default

	cd "$epidir"
	use "$epifile", clear

	gen date = mofd(mdy(month, 1, year))
	keep if year==2023

* generate hourly
	gen hourly = paidhre==1
	gen lf = inrange(dlfstat,1,4)

* weights
	gen weight = orgwgt
	gen annualweight = orgwgt*52/12

* children (for tax purposes)
	ren ownchild numkids
	replace numkids = 0 if missing(numkids)

* family identifier
	egen familyid = group(hhid famid)
	sort familyid pulineno
	gen famhead = familyid != familyid[_n-1]

* merge in EAP codes from Appendix A of 2016 reg
	merge m:1 occ18 using "eap_codes.dta", nogen keep(master match)
	gsort occ18
	replace eap_code = eap_code[_n-1] if missing(eap_code) & !missing(eap_code[_n-1])

* impute Census family income
	gen censusfamilyincome = .
	replace censusfamilyincome = runiform(0,4999) if faminc == 1
	replace censusfamilyincome = runiform(5000,7499) if faminc == 2
	replace censusfamilyincome = runiform(7500,9999) if faminc == 3
	replace censusfamilyincome = runiform(10000,12499) if faminc == 4
	replace censusfamilyincome = runiform(12500,14999) if faminc == 5
	replace censusfamilyincome = runiform(15000,19999) if faminc == 6
	replace censusfamilyincome = runiform(20000,24999) if faminc == 7
	replace censusfamilyincome = runiform(25000,29999) if faminc == 8
	replace censusfamilyincome = runiform(30000,34999) if faminc == 9
	replace censusfamilyincome = runiform(35000,39999) if faminc == 10
	replace censusfamilyincome = runiform(40000,49999) if faminc == 11
	replace censusfamilyincome = runiform(50000,59999) if faminc == 12
	replace censusfamilyincome = runiform(60000,74999) if faminc == 13
	replace censusfamilyincome = runiform(75000,99999) if faminc == 14
	replace censusfamilyincome = runiform(100000,149999) if faminc == 15
	replace censusfamilyincome = rpareto(1.161,150000) if faminc == 16

	sort familyid pulineno
	replace censusfamilyincome = censusfamilyincome[_n-1] if familyid==familyid[_n-1]

* FSLA INELIGIBLE
* Note: like DOL, we assume most federal workers will be ineligible. 
	gen inelig = unemp==1 | (emp==1 & !inrange(cow1,2,5))

* make clergy ineligible
	replace inelig = 1 if emp==1 & inlist(occ18,2040,2050,2060)										

* add back TVA, LOC, and Post Office to eligibility per DOL 
	replace inelig = 0 if emp==1 & ind17==6370
	replace inelig = 0 if emp==1 & cow1==1 & ind17==570 & inlist(statefips,21,47,28,1,13,37,51)
	replace inelig = 0 if emp==1 & cow1==1 & ind17==6770 & inlist(statefips,11)

* FSLA EXEMPT
* Based on DOL
	gen 	exempt = 		inlist(occ18, 3010, 3040,3060,3120) 				// physicians
	replace	exempt = 1 if 	inlist(occ18,2100) 									// lawyers
	replace exempt = 1 if	inrange(occ18,2200,2550) & inlist(ind17,7860, 7870)	// teachers
	replace exempt = 1 if	inlist(occ18,2000) & inlist(ind17,7860, 7870)		// academic admi
	replace exempt = 1 if	inlist(occ18,230) & inlist(ind17,7860, 7870)		// eductional admin
	replace	exempt = 1 if 	inlist(occ18,4950) 									// outside sales
	replace	exempt = 1 if 	inlist(ind17,170,180,290) 							// agg
	replace exempt = 1 if 	inlist(ind17,280) & inlist(occ18,6100,9310)			// fisherman
	replace exempt = 1 if 	inlist(occ18,2810,2830,2840,2850,2860) & cbsasize==0			// small town newspapers
	replace exempt = 1 if	(occ18==3600 | occ18==4610) & (ind17==8170 | ind17==8370 | ind17==9290) // companions
	replace exempt = 1 if	inlist(occ18,110,1000,1010,1020,1040,1060,1100,1110,5800,7900) & wage>=27.63 & !missing(wage)	// computer workers
	replace exempt = 1 if	inlist(occ18,110,1000,1010,1020,1040,1060,1100,1110,5800,7900) & weekpay>=684 & !missing(weekpay)	// computer workers
	replace exempt = 1 if 	inlist(ind17,6170) & inlist(occ18,7110,7200,7210,7220,9130)			// motor carrier workers
	replace exempt = 1 if 	inlist(occ18,9200,9230,9240,9260)			// rail carrier employees
	replace exempt = 1 if 	inlist(ind17,6090) & inlist(occ18,9300,9310,9570)			// seamen
	replace exempt = 1 if	inlist(occ18,4750,4760,7110,7150,7160,7200,7210,7220,7240,7260) & (ind17==4670 | ind17==4680) // salesmen & mechanics

* generate finalhours to be used for analysis
* to ensure that OT hours never exceeds weekly pay, we use a uniform hours measure for everyone 
* for calculating weekly earnings
	gen finalhours = hourslw1 if hourslw1 > 0 & !missing(hourslw1)
	replace finalhours = hoursu1 if missing(finalhours) & hoursu1>0 & !missing(hoursu1)

* EAP probability calculations
	gen g_alpha = 0
	gen g_beta = 0

	forvalues x=1/4 {
		qui sum weekpay if weekpay>684 & eap_code==`x' & hourly==0 [iw=orgwgt]
		replace g_alpha = (r(mean)/r(sd))^2 if eap_code==`x' & hourly==0 & !missing(weekpay) & weekpay>684 & inelig==0 & exempt==0
		replace g_beta = r(Var)/r(mean) if eap_code==`x' & hourly==0 & !missing(weekpay) & weekpay>684 & inelig==0 & exempt==0
	}

	gen eap_prob = gammap(g_alpha, weekpay/g_beta)
	sum eap_prob
	browse eap_code g_alpha g_beta weekpay eap_prob

* recenter gamma probabilities to DOL ranges
	qui sum eap_prob if eap_code==1
	replace eap_prob = 0.9 + (eap_prob - r(min))*0.1/(r(max)-r(min)) if eap_code==1
	qui sum eap_prob if eap_code==2
	replace eap_prob = 0.5 + (eap_prob - r(min))*0.4/(r(max)-r(min)) if eap_code==2
	qui sum eap_prob if eap_code==3
	replace eap_prob = 0.1 + (eap_prob - r(min))*0.4/(r(max)-r(min)) if eap_code==3
	qui sum eap_prob if eap_code==4
	replace eap_prob = 0 + (eap_prob - r(min))*0.1/(r(max)-r(min)) if eap_code==4
	replace eap_prob = 0 if eap_code == 0 

* Modify probabilities for HCE individuals 
	replace eap_prob = 1 if eap_code==1 & weekpay>=100000/52 & !missing(eap_prob)
	replace eap_prob = 0.95 if eap_code==2 & weekpay>=100000/52 & !missing(eap_prob)
	replace eap_prob = 0.592 if eap_code==3 & weekpay>=100000/52 & !missing(eap_prob)
	replace eap_prob = 0.15 if eap_code==4 & weekpay>=100000/52 & !missing(eap_prob)

* Calculate probability of being salaried FSLA
* Note: random binomial function returns error at p=1 & p=0, so need to 
* manually replace those probabilities
	gen salaried_fsla = 1 - rbinomial(1,eap_prob)
	replace salaried_fsla = 0 if eap_prob==1
	replace salaried_fsla = 1 if eap_prob==0

* Categories of OT
	gen otalways_hourly = emp == 1 & hoursu1 > 40 & !missing(hoursu1) & hourly==1 & exempt==0 & inelig==0
	gen otsome_hourly = emp == 1 & hoursu1 <=40 & hourslw1 > 40 & !missing(hourslw1) & hourly==1 & exempt==0 & inelig==0
	gen otelig_hourly = emp==1 & hourly==1 & otalways_hourly==0 & otsome_hourly==0 & inelig==0 & exempt==0
	gen otinelig_hourly = emp==1 & hourly==1 & (inelig==1 | exempt==1)

	gen otsome_salaried = emp == 1 & ((weekpay<684) | (weekpay>=684 & salaried_fsla==1)) & hourly==0 & exempt==0 & hoursu1 <= 40 & hourslw1 > 40 & !missing(hourslw1) & inelig==0
	gen otalways_salaried = emp == 1 & ((weekpay<684) | (weekpay>=684 & salaried_fsla==1)) & hourly==0 & exempt==0 & hoursu1 > 40 & !missing(hoursu1) & inelig==0
	gen otelig_salaried = emp==1 & hourly==0 & otalways_salaried==0 & otsome_salaried==0 & inelig==0 & exempt==0  & ((weekpay<684) | (weekpay>=684 & salaried_fsla==1))
	gen otinelig_salaried = emp==1 & hourly==0 & inelig

	gen otcath = 4 if emp==1 & hourly==1
	replace otcath = 1 if otalways_hourly == 1
	replace otcath = 2 if otsome_hourly == 1
	replace otcath = 3 if otelig_hourly == 1

	gen otcats = 4 if emp==1 & hourly==0
	replace otcats = 1 if otalways_salaried == 1
	replace otcats = 2 if otsome_salaried == 1
	replace otcats = 3 if otelig_salaried == 1

* final wage and weekly earnings calculations
	gen finalwage = wageotc if wageotc > 0 & !missing(wageotc)
	replace finalwage = wage if missing(finalwage) & wage > 0 & !missing(wage)

	gen total_weekly_earnings = 0
	replace total_weekly_earnings = finalwage*finalhours if !missing(finalwage) & !missing(finalhours)
	
* ot_weekly_earnings calculation
	gen ot_weekly_earnings = 0
	replace ot_weekly_earnings = wage*1.5*(finalhours - 40) if otalways_hourly == 1 & finalhours > 40
	replace ot_weekly_earnings = wage*1.5*(finalhours - 40) if otsome_hourly == 1 & finalhours > 40
	replace ot_weekly_earnings = wage*1.5*(finalhours - 40) if otalways_salaried == 1 & finalhours > 40
	replace ot_weekly_earnings = wage*1.5*(finalhours - 40) if otsome_salaried == 1 & finalhours > 40
	
* flsa eligible
	gen flsa_eligible = 0
	replace flsa_eligible = 1 if otalways_hourly==1 | otsome_hourly==1
	replace flsa_eligible = 2 if otalways_salaried==1 | otsome_salaried==1

	gen employed = emp
	gen hourly_wage = wage

* weeks works random draw from ASEC
	egen occcat = cut(occ18), at(0,10,441,961,1241,1561,1981,2061,2181,2556,2971,3551,3656,3961,4161,4256,4656,4966,5941,6131,6951,7641,8991,9431,9761,9831,9999) icodes
	gen double cum_norm_asecwt = runiform()
	nearmrg faminc occcat using "weeksworked.dta", upper nearvar(cum_norm_asecwt)

* create tax data
	keep year age female married employed weight annualweight numkids flsa_eligible total_weekly_earnings ot_weekly_earnings familyid censusfamilyincome hourly_wage hourly wkswork1
	drop if missing(age)
	sort familyid
	save otdata, replace


