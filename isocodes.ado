*! version 1.0   Leo Ahrens   leo@ahrensmail.de

program define isocodes

*-------------------------------------------------------------------------------
* syntax
*-------------------------------------------------------------------------------

#delimit ;

syntax varlist(min=1 max=1 string), gen(string)    [
KEEPRegion(string) keepiso3n(numlist) keepiso3c(string) keepiso2c(string) 
NOLabel NOSort slow

] ;
#delimit cr

*-------------------------------------------------------------------------------
* error messages
*-------------------------------------------------------------------------------

local gen_substr = subinstr(subinstr("`gen'","cntryname","",.),"iso3c","",.)
if subinstr(subinstr(subinstr("`gen_substr'","iso2c","",.),"iso3n","",.)," ","",.)!="" {
	di as error "gen() only accepts {it:iso3n}, {it:iso3c}, {it:iso2c}, and {it:cntryname}."
	exit 498
}

local keepr_substr = subinstr(subinstr(subinstr("`keepregion'","oecd","",.),"eu","",.),"emu","",.)
local keepr_substr2 = subinstr(subinstr(subinstr("`keepr_substr'","africa","",.),"america","",.),"asia","",.)
if subinstr(subinstr(subinstr("`keepr_substr2'","europe","",.),"oceania","",.)," ","",.)!="" {
	di as error "keepregion() only accepts {it:oecd}, {it:eu}, {it:emu}, {it:africa}, {it:america}, {it:asia}, {it:europe}, and {it:oceania}."
	exit 498
}
	
*-------------------------------------------------------------------------------
* prep ados & country dataset 
*-------------------------------------------------------------------------------

quietly {
	
if "`slow'"!="" {
	local lvlsof levelsof
	local ggen egen
}
else {
	capture which gtools
	if _rc==111 {
		ssc install gtools, replace 
		gtools, upgrade
	}
	local lvlsof glevelsof
	local ggen gegen
}

cap describe using "`c(sysdir_plus)'i/countrycodes.dta", short varl
local ccodes_data_varl = r(varlist)
if _rc | !strpos("`ccodes_data_varl'","version1") {
	net set other `c(sysdir_plus)'i
	net get countrycodes, from("https://raw.githubusercontent.com/leojahrens/isocodes/master") replace
}

*-------------------------------------------------------------------------------
* prep dataset
*-------------------------------------------------------------------------------

qui count if `varlist'==""
if r(N)>0 {
	noisily di "There are " r(N) " missings in the {it:`varlist'} variable, which cannot be considered."
}
foreach __var in `gen' {
	cap confirm variable `__var'
	if !_rc {
		drop `__var'
		noisily di "A variable named `__var' already exists in the dataset. It has been dropped and replaced."
	}
}
preserve 
clonevar __ogcountry = `varlist'

replace `varlist' = subinstr(subinstr(subinstr(lower(`varlist'),":"," ",.),"'"," ",.),"-"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist',","," ",.),"/"," ",.),"("," ",.),")"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist',"."," ",.),"&"," ",.)," republics"," ",.)," the"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," of"," ",.),"of "," ",.)," of "," ",.)," the "," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," republic"," ",.),"republic "," ",.)," republic "," ",.),"the "," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," rep"," ",.),"rep "," ",.)," rep "," ",.)," ","",.)

`ggen' __tag = tag(`varlist')
keep if __tag==1 & `varlist'!=""
keep `varlist' __ogcountry

*-------------------------------------------------------------------------------
* assign country codes
*-------------------------------------------------------------------------------

// list of variables in attached dataset
local countrylist__ cntryname iso2c iso3c penn cow unctad marc name2 name3 name4 name5 name6 name7 cntryname_copy

// merge with all variants
local __cname__ "`varlist'"
foreach __cvar__ in `countrylist__' {
	if "`__cvar__'"!="`gen'" {
		rename `__cname__' `__cvar__'
		merge 1:m `__cvar__' using "`c(sysdir_plus)'i/countrycodes", nogen keepusing(`gen') keep(1 3)
		foreach ind in `gen' {
			if "`__cvar__'"!="`ind'" {
				if "`ind'"=="iso3n" {
					local mihelp !mi(`ind'_`__cvar__')
				}
				else {
					local mihelp `"`ind'_`__cvar__'!="""'
				}
				rename `ind' `ind'_`__cvar__'
				if "`ind'"!="iso3n" replace `ind'_`__cvar__' = "" if `ind'_`__cvar__'=="__mi"
				count if `mihelp'
				local `ind'_`__cvar__'_nm = r(N)	
			}
		}
		local __cname__ `__cvar__'
	}
}

// find out version with closest match
foreach ind in `gen' {
	local `ind'__nm = 0
	foreach __cvar__ in `countrylist__' {
		if "`__cvar__'"!="`ind'" {
			if ``ind'_`__cvar__'_nm'>``ind'__nm' {
				local `ind'_highestno "`__cvar__'"
				local `ind'__nm = ``ind'_`__cvar__'_nm'
			}
		}
	}
	rename `ind'_``ind'_highestno' `ind'
}

// use that as baseline and fill with values from others
foreach ind in `gen' {
	foreach __cvar__ in `countrylist__' {
		if "`__cvar__'"!="``ind'_highestno'" & "`__cvar__'"!="`ind'" {
			if "`ind'"=="iso3n" {
				local mihelp !mi(`ind')
			}
			else {
				local mihelp `"`ind'=="""'
			}
			count if `mihelp'
			if r(N)>0 {
				replace `ind' = `ind'_`__cvar__' if mi(`ind') & !mi(`ind'_`__cvar__')
			}
			else {
				continue, break
			}
		}
	}
}

// clean
rename `__cname__' `varlist'
keep `varlist' __ogcountry `gen'

// fillup with regular expressions
local anymiss = 0
foreach ind in `gen' {
	count if mi(`ind')
	if r(N)>0 local anymiss = 1 
}

if `anymiss'==1 {
	tempfile __ogdata __fillup
	save `__ogdata', replace 

	dis `"`gen'"'
	foreach ind in `gen' {
		local indgenmiss `indgenmiss' | mi(`ind')
	}
	keep if 1==2 `indgenmiss'
	keep `varlist' __ogcountry
	gen __cfill__ = ""
	local nameofvarlist `varlist'

	
	
	local shc replace __cfill__ =
	local shn if regexm(`varlist',

	`shc' "afghanistan" `shn' "(?i).*afg(h)?an.*")
	`shc' "aland islands" `shn' "(?i).*[aå]?land.*") & regexm(`varlist',"(?i).*is.*") & !(regexm(`varlist',"(?i).*bouvet|virgin|cape|cayman|mayen.*")) & !(regexm(`varlist',"(?i).*marshal|norfolk|mariana|solomon|sandwich.*")) & !(regexm(`varlist',"(?i).*caicos|outlying|wallis.*"))
	`shc' "albania" `shn' "(?i).*alban.*")
	`shc' "algeria" `shn' "(?i).*alger.*")
	`shc' "american samoa" `shn' "(?i).*samo.*") & regexm(`varlist',"(?i).*amer|us.*")
	`shc' "andorra" `shn' "(?i).*andor.*")
	`shc' "angola" `shn' "(?i).*angol.*")
	`shc' "anguilla" `shn' "(?i).*angu.*")
	`shc' "antarctica" `shn' "(?i).*arctica.*")
	`shc' "antigua and barbuda" `shn' "(?i).*antig.*")
	`shc' "argentina" `shn' "(?i).*argen.*")
	`shc' "armenia" `shn' "(?i).*armen.*")
	`shc' "aruba" `shn' "(?i).*arub.*")
	`shc' "australia" `shn' "(?i).*austra.*")
	`shc' "austria" `shn' "(?i).*austri.*")
	`shc' "azerbaijan" `shn' "(?i).*azer.*") | regexm(`varlist',"(?i).*bai[jd]?.*")
	`shc' "bahamas" `shn' "(?i).*baha.*")
	`shc' "bahrain" `shn' "(?i).*bahr.*")
	`shc' "bangladesh" `shn' "(?i).*bang.*")
	`shc' "barbados" `shn' "(?i).*barba.*")
	`shc' "belarus" `shn' "(?i).*bela.*") | regexm(`varlist',"(?i).*byelo.*")
	`shc' "belgium" `shn' "(?i).*belgi.*")
	`shc' "belize" `shn' "(?i).*beliz.*") | (regexm(`varlist',"(?i).*hond.*") & regexm(`varlist',"(?i).*brit|uk.*"))
	`shc' "benin" `shn' "(?i).*beni|dahom.*")
	`shc' "bermuda" `shn' "(?i).*berm.*")
	`shc' "bhutan" `shn' "(?i).*b(h)?ut.*")
	`shc' "bolivia" `shn' "(?i).*bolivi.*")
	`shc' "bonaire, sint eustatius and saba" `shn' "(?i).*bona.*") & regexm(`varlist',"(?i).*eust.*") & regexm(`varlist',"(?i).*sab.*") | (regexm(__cfill__,"(?i).*carib.*") & regexm(`varlist',"(?i).*nether.*"))
	`shc' "bosnia and herzegovina" `shn' "(?i).*bos.*") & regexm(`varlist',"(?i).*her.*")
	`shc' "botswana" `shn' "(?i).*bots.*")
	`shc' "bouvet island" `shn' "(?i).*bouv.*")
	`shc' "brazil" `shn' "(?i).*brazi.*")
	`shc' "british indian ocean territory" `shn' "(?i).*indi.*") & regexm(`varlist',"(?i).*ocean.*")
	`shc' "british virgin islands" `shn' "(?i).*virg.*") & regexm(`varlist',"(?i).*is.*") & regexm(`varlist',"(?i).*brit|uk.*")
	`shc' "brunei" `shn' "(?i).*brun|darus.*")
	`shc' "bulgaria" `shn' "(?i).*bulg.*")
	`shc' "burkina faso" `shn' "(?i).*burk|volt.*")
	`shc' "burundi" `shn' "(?i).*buru.*")
	`shc' "cabo verde" `shn' "(?i).*cabo|cape.*") & regexm(`varlist',"(?i).*verd.*")
	`shc' "cambodia" `shn' "(?i).*cambo|kampu.*")
	`shc' "cameroon" `shn' "(?i).*camer.*")
	`shc' "macao" `shn' "(?i).*macao|macau.*")
	

	keep if __cfill__!=""
	rename __cfill__ cntryname
	foreach ind in `gen' {
		if "`ind'"!="cntryname" merge m:1 cntryname using "`c(sysdir_plus)'i/countrycodes", nogen keepusing(`ind') keep(1 3)
	}
	noisily di "The following strings were matched with a degree of uncertainty. Please check if this is correct."
	rename __ogcountry OriginalVar
	noisily list OriginalVar `gen'
	drop OriginalVar
	foreach ind in `gen' {
		rename `ind' `ind'_regex
		local regexkeep `regexkeep' `ind'_regex
	}
	save `__fillup', replace 
	use `__ogdata', clear
	merge 1:1 `varlist' using `__fillup', nogen keepusing(`regexkeep') keep(1 3)
	foreach ind in `gen' {
		replace `ind' = `ind'_regex if mi(`ind') & !mi(`ind'_regex)
		drop `ind'_regex
	}
}

// fill up further from other gen() variables
if wordcount("`gen'")>1 {
	foreach ind in `gen' {
		count if mi(`ind')
		if r(N)>0 {
			keep `varlist' __ogcountry `gen'
			rename `ind' `ind'_temp
			local not_`ind' = subinstr("`gen'","`ind'","",.)
			foreach notind in `not_`ind'' {
				merge 1:m `notind' using "`c(sysdir_plus)'i/countrycodes", nogen keepusing(`ind') keep(1 3)
				replace `ind'_temp = `ind' if mi(`ind'_temp) & !mi(`ind')
				drop `ind'
			}
			rename `ind'_temp `ind'
		}
	}
}

// capitalize strings for cntryname 
if strpos("`gen'","cntryname") {
	merge 1:m cntryname using "`c(sysdir_plus)'i/countrycodes", nogen keepusing(cntryname_uc) keep(1 3)
	drop cntryname
	rename cntryname_uc cntryname
}

if strpos("`gen'","iso2c") | strpos("`gen'","iso3c") {
	foreach ind in `gen' {
		if !inlist("`ind'","iso3n","cntryname") {
			replace `ind' = upper(`ind')
		}
	}
}

// count missings
foreach ind in `gen' {
	count if mi(`ind')
	if r(N)>0 {
		local `ind'miss = r(N)
	}
	else {
		local `ind'miss = 0
	}
}


*-------------------------------------------------------------------------------
* restrict sample
*-------------------------------------------------------------------------------

if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" {
	
	if "`keepregion'"!="" {
		local __oecd_countries__ 36 40 56 124 152 203 208 233 246 250 276 300 348 352 372 ///
		376 380 392 410 428 440 442 484 528 554 578 616 620 703 705 724 752 756 792 826 840
		
		local __eu_countries__ 40 56 100 191 196 203 208 233 246 250 276 300 348 372 380 ///
		428 440 442 470 528 616 620 642 703 705 724 752 826
		
		local __emu_countries__ 40 56 196 233 246 250 276 300 372 380 428 442 470 528 620 703 705 724
		
		local __africa_countries__ 12 24 72 86 108 120 132 140 148 174 175 178 180 204 226 ///
		231 232 260 262 266 270 288 324 384 404 426 430 434 450 454 466 478 480 504 508 516 562 ///
		566 624 638 646 654 678 686 690 694 706 710 716 728 729 732 748 768 788 800 818 834 854 894
		
		local __america_countries__ 28 32 44 52 60 68 74 76 84 92 124 136 152 170 188 192 212 214 ///
		218 222 238 239 254 304 308 312 320 328 332 340 388 474 484 500 531 533 534 535 558 591 600 ///
		604 630 652 659 660 662 663 666 670 740 780 796 840 850 858 862
		
		local __asia_countries__ 4 31 48 50 51 64 96 104 116 144 156 196 268 275 344 356 360 ///
		364 368 376 392 398 400 408 410 414 417 418 422 446 458 462 496 512 524 586 608 626 634 ///
		682 702 704 760 762 764 784 792 795 860 887
		
		local __europe_countries__ 8 20 40 56 70 100 112 191 203 208 233 234 246 248 250 276 292 300 ///
		336 348 352 372 380 428 438 440 442 470 492 498 499 528 578 616 620 642 643 674 688 703 705 ///
		724 744 752 756 804 807 826 831 832 833
		
		local __oceania_countries__ 16 36 90 162 166 184 242 258 296 316 334 520 540 548 554 570 574 ///
		580 581 583 584 585 598 612 772 776 798 876 882

		gen __keeeep__ = 0
		foreach utut in `keepregion' {
			foreach lklk of local __`utut'_countries__ {
				replace __keeeep__ = 1 if iso3n==`lklk'
			}
		}
	}

	if "`keepiso3n'"!="" {
		cap confirm variable __keeeep__
		if _rc gen __keeeep__ = .
		foreach lklkl of numlist `keepiso3n' {
			replace __keeeep__ = 1 if iso3n==`lklkl'
		}
	}

	if "`keepiso2c'"!="" {
		cap confirm variable __keeeep__
		if _rc gen __keeeep__ = .
		foreach lklkl in `keepiso2c' {
			replace __keeeep__ = 1 if iso2c=="`lklkl'"
		}
	}

	if "`keepiso3c'"!="" {
		cap confirm variable __keeeep__
		if _rc gen __keeeep__ = .
		foreach lklkl in `keepiso3c' {
			replace __keeeep__ = 1 if iso3c=="`lklkl'"
		}
	}

	count if __keeeep__!=1 
	local __keeeep__count = r(N)
	if `__keeeep__count'>0 {
		`lvlsof' `varlist' if __keeeep__!=1, local(_asd) clean separate(; )
		noisily di `__keeeep__count' " countries are dropped by the keep options: `_asd'"
		drop if __keeeep__!=1 
	}
}

*-------------------------------------------------------------------------------
* iso label
*-------------------------------------------------------------------------------

if strpos("`gen'","iso3n") & "`nolabel'"=="" {
	cap label drop __isolabels__
	label define __isolabels__  4 "Afghanistan" 8 "Albania" 10 "Antarctica" 12 "Algeria" 16 "American Samoa" 20 "Andorra" 24 "Angola" 28 "Antigua and Barbuda" 31 "Azerbaijan" 32 "Argentina" 36 "Australia" 40 "Austria" 44 "Bahamas" 48 "Bahrain" 50 "Bangladesh" 51 "Armenia" 52 "Barbados" 56 "Belgium" 60 "Bermuda" 64 "Bhutan" 68 "Bolivia" 70 "Bosnia and Herzegovina" 72 "Botswana" 74 "Bouvet Island" 76 "Brazil" 84 "Belize" 86 "British Indian Ocean Territory" 90 "Solomon Islands" 92 "British Virgin Islands" 96 "Brunei Darussalam" 100 "Bulgaria" 104 "Myanmar" 108 "Burundi" 112 "Belarus" 116 "Cambodia" 120 "Cameroon" 124 "Canada" 132 "Cabo Verde" 136 "Cayman Islands" 140 "Central African Republic" 144 "Sri Lanka" 148 "Chad" 152 "Chile" 156 "China" 158 "Taiwan" 162 "Christmas Island" 166 "Cocos (Keeling) Islands" 170 "Colombia" 174 "Comoros" 175 "Mayotte" 178 "Congo" 180 "Democratic Republic of the Congo" 184 "Cook Islands" 188 "Costa Rica" 191 "Croatia" 192 "Cuba" 196 "Cyprus" 203 "Czechia" 204 "Benin" 208 "Denmark" 212 "Dominica" 214 "Dominican Republic" 218 "Ecuador" 222 "El Salvador" 226 "Equatorial Guinea" 231 "Ethiopia" 232 "Eritrea" 233 "Estonia" 234 "Faroe Islands" 238 "Falkland Islands" 239 "South Georgia and the South Sandwich Islands" 242 "Fiji" 246 "Finland" 248 "Åland Islands" 250 "France" 254 "French Guiana" 258 "French Polynesia" 260 "French Southern Territories" 262 "Djibouti" 266 "Gabon" 268 "Georgia" 270 "Gambia" 275 "State of Palestine" 276 "Germany" 288 "Ghana" 292 "Gibraltar" 296 "Kiribati" 300 "Greece" 304 "Greenland" 308 "Grenada" 312 "Guadeloupe" 316 "Guam" 320 "Guatemala" 324 "Guinea" 328 "Guyana" 332 "Haiti" 334 "Heard Island and McDonald Islands" 336 "Holy See" 340 "Honduras" 344 "Hong Kong" 348 "Hungary" 352 "Iceland" 356 "India" 360 "Indonesia" 364 "Iran" 368 "Iraq" 372 "Ireland" 376 "Israel" 380 "Italy" 384 "Côte d'Ivoire" 388 "Jamaica" 392 "Japan" 398 "Kazakhstan" 400 "Jordan" 404 "Kenya" 408 "North Korea" 410 "South Korea" 414 "Kuwait" 417 "Kyrgyzstan" 418 "Laos" 422 "Lebanon" 426 "Lesotho" 428 "Latvia" 430 "Liberia" 434 "Libya" 438 "Liechtenstein" 440 "Lithuania" 442 "Luxembourg" 446 "Macao" 450 "Madagascar" 454 "Malawi" 458 "Malaysia" 462 "Maldives" 466 "Mali" 470 "Malta" 474 "Martinique" 478 "Mauritania" 480 "Mauritius" 484 "Mexico" 492 "Monaco" 496 "Mongolia" 498 "Moldova" 499 "Montenegro" 500 "Montserrat" 504 "Morocco" 508 "Mozambique" 512 "Oman" 516 "Namibia" 520 "Nauru" 524 "Nepal" 528 "Netherlands" 531 "Curaçao" 533 "Aruba" 534 "Sint Maarten" 535 "Bonaire, Sint Eustatius and Saba" 540 "New Caledonia" 548 "Vanuatu" 554 "New Zealand" 558 "Nicaragua" 562 "Niger" 566 "Nigeria" 570 "Niue" 574 "Norfolk Island" 578 "Norway" 580 "Northern Mariana Islands" 581 "US Minor Outlying Islands" 583 "Micronesia" 584 "Marshall Islands" 585 "Palau" 586 "Pakistan" 591 "Panama" 598 "Papua New Guinea" 600 "Paraguay" 604 "Peru" 608 "Philippines" 612 "Pitcairn" 616 "Poland" 620 "Portugal" 624 "Guinea-Bissau" 626 "Timor-Leste" 630 "Puerto Rico" 634 "Qatar" 638 "Réunion" 642 "Romania" 643 "Russia" 646 "Rwanda" 652 "Saint Barthélemy" 654 "Saint Helena" 659 "Saint Kitts and Nevis" 660 "Anguilla" 662 "Saint Lucia" 663 "Saint Martin" 666 "Saint Pierre and Miquelon" 670 "Saint Vincent and the Grenadines" 674 "San Marino" 678 "Sao Tome and Principe" 682 "Saudi Arabia" 686 "Senegal" 688 "Serbia" 690 "Seychelles" 694 "Sierra Leone" 702 "Singapore" 703 "Slovakia" 704 "Vietnam" 705 "Slovenia" 706 "Somalia" 710 "South Africa" 716 "Zimbabwe" 724 "Spain" 728 "South Sudan" 729 "Sudan" 732 "Western Sahara" 740 "Suriname" 744 "Svalbard and Jan Mayen Islands" 748 "Eswatini" 752 "Sweden" 756 "Switzerland" 760 "Syria" 762 "Tajikistan" 764 "Thailand" 768 "Togo" 772 "Tokelau" 776 "Tonga" 780 "Trinidad and Tobago" 784 "United Arab Emirates" 788 "Tunisia" 792 "Turkey" 795 "Turkmenistan" 796 "Turks and Caicos Islands" 798 "Tuvalu" 800 "Uganda" 804 "Ukraine" 807 "North Macedonia" 818 "Egypt" 826 "United Kingdom" 831 "Guernsey" 832 "Jersey" 833 "Isle of Man" 834 "Tanzania" 840 "United States" 850 "US Virgin Islands" 854 "Burkina Faso" 858 "Uruguay" 860 "Uzbekistan" 862 "Venezuela" 876 "Wallis and Futuna Islands" 882 "Samoa" 887 "Yemen" 894 "Zambia"
	lab val iso3n __isolabels__
}

*-------------------------------------------------------------------------------
* merge back to dataset, sort & order
*-------------------------------------------------------------------------------

if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" local __keeploc __keeeep__
drop `varlist'
rename __ogcountry `varlist'
keep `varlist' `gen' `__keeploc'
tempfile cmerge 
save `cmerge', replace 
restore
merge m:1 `varlist' using `cmerge', nogen keepusing(`gen' `__keeploc') keep(1 3)
if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" {
	keep if __keeeep__==1 
	drop __keeeep__
}
order `varlist' `gen'
if "`nosort'"=="" {
	foreach __time__ in qyear year quarter month day {
		capture confirm variable `__time__'
		if !_rc local isocodesyear `isocodesyear' `__time__'
	}
	gsort `varlist' `isocodesyear'
}

*-------------------------------------------------------------------------------
* report missings
*-------------------------------------------------------------------------------

foreach ind in `gen' {
	if ``ind'miss'>0 {
		`lvlsof' `varlist' if mi(`ind'), local(_ztrw) clean separate(; )
		noisily di "`ind' codes could not be assigned to ``ind'miss' countries: `_ztrw'"
	}
}



}
end




















