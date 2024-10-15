************************************************
* MEASURING AUTO LOAN INTEREST IN THE 2022 SCF *
* Ernie Tedeschi					           *
* The Budget Lab at Yale University            *
* Last Updated: 15 October 2024                *
************************************************

* Download the full public SCF data set for 2022 here: https://www.federalreserve.gov/econres/files/scf2022s.zip

global datadir "C:\data\scf\"

clear all
set maxvar 11000

cd "$datadir"

use "p22i6.dta"

keep y1 yy1 x2219 x2319 x2419 x7170 x2218 x2318 x2418 x7169

foreach v of varlist x2219 x2319 x2419 x7170 {
    replace `v'=0 if `v'<0
}

gen carinterest = (x2219*x2218/10000)+(x2319*x2318/10000)+(x2419*x2418/10000)+(x7170*x7169/10000)

keep y1 yy1 carinterest

save scf2022_carinterest, replace