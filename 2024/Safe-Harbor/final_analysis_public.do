*****************************
* SAFE HARBOR CRP ANALYSIS  *
* Ernie Tedeschi		    *
* Yale Budget Lab			*
* This version: 17 Apr 2024 *
*****************************

**********
* SETTINGS

	clear all

	global datafile		".\data\merged_data.dta"
	
	global vdemvars		year country_text_id v2x_polyarchy v2x_freexp_altinf v2x_frassoc_thick ///
						v2x_suffr v2xel_frefair v2x_elecoff v2xcl_rol v2x_jucon v2xlg_legcon ///
						v2x_cspart v2xdd_dd v2xel_locelec v2xel_regelec v2xdl_delib v2xeg_eqprotec ///
						v2xeg_eqaccess v2xeg_eqdr
	
	global wd			"C:\Location\Of\Data\"

	
*******************
* LOAD & CLEAN DATA

	cd "$wd"
	use "$datafile"
	
** vdem: fix a few missing values
	egen tempid = group(country_text_id)
	xtset tempid year

	replace v2xlg_legcon = (L.v2xlg_legcon + F.v2xlg_legcon)/2 if missing(v2xlg_legcon)
	replace v2xel_locelec = L.v2xel_locelec if missing(v2xel_locelec)

** code oecd

	gen origgdpshare = gdpshare
	gen oecd = 		inlist(country_text_id, "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK") | ///
					inlist(country_text_id, "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR") | ///
					inlist(country_text_id, "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL") | ///
					inlist(country_text_id, "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR") | ///
					inlist(country_text_id, "GBR", "USA")
	
	gen oecdnonusa = oecd == 1 & !inlist(country_text_id, "USA")

	gen zerorisk = 	inlist(country_text_id,"USA","CAN","AUS","DNK","DEU","NLD","NZL","NOR","SGP") | ///
					inlist(country_text_id, "SWE","CHE")

	gen eurozone =	inlist(country_text_id, "AUT", "BEL", "HRV", "CYP", "EST", "FIN", "FRA", "DEU") | ///
					inlist(country_text_id, "GRC", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT") | ///
					inlist(country_text_id, "SVK", "SVN", "ESP")

	gen g7 =		inlist(country_text_id, "CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA") 
	
	gen pol_icrg = 	icrg_govstab + icrg_socioecon + icrg_invprof + icrg_intconf + icrg_extconf + icrg_corruption + icrg_militarypol + icrg_reltens + icrg_laworder + icrg_ethnictensions + icrg_accountability + icrg_bureaucraticquality	
	gen econ_icrg = icrg_gdphead + icrg_gdpgrowth + icrg_inflation + icrg_budbal + icrg_cacc
	gen fin_icrg = icrg_debt + icrg_debtserv + icrg_caxgs + icrg_intliq + icrg_fxstab

** preliminary graphs

*** FIGURE 7: icrg graph

	pause on
	preserve
	keep if g7==1
	keep year country_text_id pol_icrg
	keep if !missing(pol_icrg)
	sort country_text_id year
	egen tempid = group(country_text_id)
	drop country_text_id
	reshape wide pol_icrg, j(tempid) i(year)
	tsline pol_icrg*, ///
		title("{bf:ICRG Composite Political Risk for the G7}", justification(left) position(11)) ///
		subtitle("Score (out of 100)" "Higher = Safer", justification(left) position(11) color(gray)) ///
		legend(order(1 "CAN" 2 "DEU" 3 "FRA" 4 "GBR" 5 "ITA" 6 "JPN" 7 "USA")) ///
		xtitle("") ytitle("") xsize(7.5) ///
		lc(. . . . . . black) lw(. . . . . . thick) ///
		note("Source: PRS Group, Yale Budget Lab analysis.", color(gray))
	restore	

*** FIGURE 8: V-DEM
	pause on
	preserve
	keep if g7==1
	keep year country_text_id v2x_polyarchy
	keep if !missing(v2x_polyarchy)
	sort country_text_id year
	egen tempid = group(country_text_id)
	drop country_text_id
	reshape wide v2x_polyarchy, j(tempid) i(year)
	tsline v2x_polyarchy* if year>=1980, ///
		title("{bf:V-Dem Electoral Democracy Index for the G7}", justification(left) position(11)) ///
		subtitle("Score (0-1)" "Higher = Stronger democratic characteristics", justification(left) position(11) color(gray)) ///
		legend(order(1 "CAN" 2 "DEU" 3 "FRA" 4 "GBR" 5 "ITA" 6 "JPN" 7 "USA")) ///
		xtitle("") ytitle("") xsize(7.5) ///
		lc(. . . . . . black) lw(. . . . . . thick) ///
		note("Source: V-Dem, Yale Budget Lab analysis.", color(gray)) 
	browse year v2x_polyarchy* if year>=1980
	pause
	restore	

*** FIGURE 9: WGI
	pause on
	preserve
	keep if g7==1
	keep year country_text_id wgi_*
	sort country_text_id year
	egen tempid = group(country_text_id)
	drop country_text_id
	gen wgi_sum = (wgi_vae + wgi_pve + wgi_gee + wgi_rqe + wgi_rle + wgi_cce)/10
	keep wgi_sum year tempid
	reshape wide wgi_sum, j(tempid) i(year)
	tsline wgi_sum* if year>=1997, ///
		title("{bf:Composite WGI Index for the G7}", justification(left) position(11)) ///
		subtitle("Sum of individual indices, rescaled to (0-1)" "Higher = Stronger governance", justification(left) position(11) color(gray)) ///
		legend(order(1 "CAN" 2 "DEU" 3 "FRA" 4 "GBR" 5 "ITA" 6 "JPN" 7 "USA")) ///
		xtitle("") ytitle("") xsize(7.5) ///
		lc(. . . . . . black) lw(. . . . . . thick) ///
		note("Source: World Bank, Yale Budget Lab analysis.", color(gray)) 
	browse year wgi_sum* if year>=1997
	pause
	restore	
	
** preliminary table

	gen 	tablecat = .
	replace	tablecat = 1 if inlist(country_text_id, "USA")
	replace tablecat = 2 if inlist(country_text_id, "CAN")
	replace tablecat = 3 if inlist(country_text_id, "GBR")
	replace tablecat = 4 if inlist(country_text_id, "FRA")
	replace tablecat = 5 if inlist(country_text_id, "DEU")
	replace tablecat = 6 if inlist(country_text_id, "ITA")
	replace tablecat = 7 if inlist(country_text_id, "JPN")
	replace tablecat = 8 if tablecat==. & eurozone ==1
	replace tablecat = 9 if tablecat==. & oecd == 1
	replace tablecat = 10 if inlist(country_text_id, "CHN")
	replace tablecat = 11 if tablecat==. 
	
	table tablecat [pw=gdpshare] if year==2023, con(mean crp_damodaran mean v2x_polyarchy sum origgdpshare)

** drop overall imf for analysis (since it's already a composite)

	drop imf_fdi
	
** impute missing values	
*** imf: carry forward 1 year
	foreach v of varlist imf_* { // 
		replace `v'=L.`v' if !missing(L.`v') & missing(`v') & year>2021
	}

*** wgi: use ols
	foreach v of varlist wgi_* { 
		reg `v' v2x* icrg_* imf_* i.tempid
		predict temp_`v'
		replace `v'= L.`v' + D.temp_`v' if year==2023 & missing(`v')
		drop temp_`v'
	}
	drop tempid

** build a strongly balanced panel
	gen pancheck_icrg = 0
	gen pancheck_v2x = 0
	gen pancheck_imf = 0

	foreach v of varlist v2x* {
		replace pancheck_v2x = pancheck_v2x + 1 if !missing(`v')
	}

	foreach v of varlist icrg* {
		replace pancheck_icrg = pancheck_icrg + 1 if !missing(`v')
	}

	foreach v of varlist imf* {
		replace pancheck_imf = pancheck_imf + 1 if !missing(`v')
	}

	gen pancheck_total = pancheck_v2x == 17 & pancheck_icrg == 22 & pancheck_imf == 6
	keep if inrange(year, 2002, 2023)
	egen pancheck_country = total(pancheck_total), by(country_text_id)
	keep if pancheck_country == 22

	egen xid = group(country_text_id)
	xtset xid year

** renormalize gdpshare
	replace gdpshare = 0 if missing(gdpshare)
	egen tempt = total(gdpshare), by(year)
	replace gdpshare = 100*gdpshare/tempt
	drop tempt

**********
* ANALYSIS

** PCA 1: Political/Institutional Index
	pca v2x* wgi_* icrg_govstab icrg_intconf icrg_extconf icrg_corruption icrg_militarypol icrg_reltens icrg_laworder icrg_ethnictensions icrg_accountability icrg_bureaucraticquality, comp(1)
	predict pca1_*		

** PCA 2: Financial/Economic Index
	pca  icrg_socioecon icrg_invprof icrg_debt icrg_fxstab icrg_debtserv icrg_caxgs icrg_intliq icrg_inflation icrg_gdphead icrg_gdpgrowth icrg_budbal icrg_cacc imf_*, comp(1)
	predict pca2_*	

** reorthoganalize 
	pca pca1_1 pca2_1, comp(2)
	predict pcaf_*

** recast so both measure risk (up is riskier)
	replace pcaf_1 = -1*pcaf_1

** Poisson Regression of CRP and PCAs
*** save original pcas 
	gen o_pcaf_1 = pcaf_1
	gen o_pcaf_2 = pcaf_2

*** regression, simulate political effect
	poisson crp_damodaran pcaf_* if country_text_id!="USA"
	replace pcaf_2 = 0
	predict pol_effect
	replace pcaf_2 = o_pcaf_2

*** regression, simulate financial effect
	poisson crp_damodaran pcaf_* if country_text_id!="USA"
	replace pcaf_1 = 0
	predict fin_effect
	replace pcaf_1 = o_pcaf_1	

*** gdp-weighted effects for oecd
	preserve 
	gcollapse (mean) pol_effect fin_effect if oecdnonusa==1 [pw=gdpshare], by(year)
	tempfile oecdmean
	ren pol_effect pol_effect_oecd
	ren fin_effect fin_effect_oecd
	save `oecdmean', replace
	restore

*** gdp-weighted effects for damodaran zero-risk countries
	preserve 
	gcollapse (mean) pol_effect fin_effect if zerorisk==1 & !inlist(country_text_id, "USA")  [pw=gdpshare], by(year)
	tempfile zrmean
	ren pol_effect pol_effect_zr
	ren fin_effect fin_effect_zr
	save `zrmean', replace
	restore

*** FIGURES 10 & 11 
	pause on
	preserve
	keep if inlist(country_text_id, "USA", "CAN")
	keep year xid *_effect
	reshape wide *_effect, j(xid) i(year)
	merge 1:1 year using `oecdmean', nogen keep(master match)
	merge 1:1 year using `zrmean', nogen keep(master match)
	gen pol_spread = pol_effect104 - pol_effect17
	gen fin_spread = fin_effect104 - fin_effect17
	gen pol_spread_oecd = pol_effect104 - pol_effect_oecd
	gen fin_spread_oecd = fin_effect104 - fin_effect_oecd
	gen pol_spread_zr = pol_effect104 - pol_effect_zr
	gen fin_spread_zr = fin_effect104 - fin_effect_zr
	
	foreach v of varlist pol_spread pol_spread_zr pol_spread_oecd fin_spread fin_spread_zr fin_spread_oecd {
	    qui sum `v' if inrange(year, 2016,2016)
		gen b_`v' = `v'-r(mean)
	}
	tsline pol_spread pol_spread_zr, tline(2016, lc(red)) ///
		title("{bf:US Political/Institutional Risk Spread}", justification(left) position(11)) ///
		subtitle("Percentage points, country risk premium-equivalent" "Higher = More Risk", justification(left) position(11) color(gray)) ///
		legend(order(1 "Against Canada" 2 "Against All Low-Risk* **")) ///
		xtitle("") ytitle("") xsize(7.5) ///
		note("* GDP-weighted spread" "** Countries with zero-risk in Damodoran (2023): Canada, Australia, Denmark," "Germany, Netherlands, New Zealand, Norway, Singapore, Sweden, and Switzerland." "Source: V-Dem, PRS Group, World Bank, IMF, Damodaran (2023), Yale Budget Lab analysis.", color(gray))
	browse year pol_spread pol_spread_zr
	pause
	tsline b_pol_spread b_pol_spread_zr, tline(2016, lc(red)) ///
		title("{bf:US Political/Institutional Risk Spread, Difference from 2016}", justification(left) position(11)) ///
		subtitle("Percentage points, country risk premium-equivalent" "Higher = More Risk" "2016 = 0", justification(left) position(11) color(gray)) ///
		legend(order(1 "Against Canada" 2 "Against All Low-Risk* **")) ///
		xtitle("") ytitle("") xsize(7.5) ///
		note("* GDP-weighted spread" "** Countries with zero-risk in Damodoran (2023): Canada, Australia, Denmark," "Germany, Netherlands, New Zealand, Norway, Singapore, Sweden, and Switzerland." "Source: V-Dem, PRS Group, World Bank, IMF, Damodaran (2023), Yale Budget Lab analysis.", color(gray))
	browse year b_pol_spread b_pol_spread_zr
	restore

**********
* END CODE
	