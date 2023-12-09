*! version 1.1   Leo Ahrens   leo@ahrensmail.de

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

cap describe using "`c(sysdir_plus)'i/isocodes.dta", short varl
local ccodes_data_varl = r(varlist)
if _rc | !strpos("`ccodes_data_varl'","version1") {
	net set other `c(sysdir_plus)'i
	net get isocodes, from("https://raw.githubusercontent.com/leojahrens/isocodes/master") replace
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
		merge 1:m `__cvar__' using "$cpath/isocodes.dta", nogen keepusing(`gen') keep(1 3)
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
	dis "`ind'"
	local `ind'__nm = 0
	foreach __cvar__ in `countrylist__' {
		if "`__cvar__'"!="`ind'" {
			if ``ind'_`__cvar__'_nm'>``ind'__nm' {
				local `ind'_highestno "`__cvar__'"
				local `ind'__nm = ``ind'_`__cvar__'_nm'
			}
		}
	}
	if "`ind'_``ind'_highestno'"!="`ind'_" {
		rename `ind'_``ind'_highestno' `ind'
		local `ind'_highnoproceed "yes"
	}
	else {
		rename `ind'_cntryname `ind'
	}	
}

// use that as baseline and fill with values from others
foreach ind in `gen' {
	if "``ind'_highnoproceed'"=="yes" {
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

	foreach ind in `gen' {
		local indgenmiss `indgenmiss' | mi(`ind')
	}
	keep if 1==2 `indgenmiss'
	keep `varlist' __ogcountry
	gen __cfill__ = ""
	local nameofvarlist `varlist'

	local shc replace __cfill__ =
	local shn if regexm(`varlist',

	`shc' "afghanistan" `shn' "afghan")
	`shc' "albania" `shn' "albania")
	`shc' "antarctica" `shn' "antarctica")
	`shc' "algeria" `shn' "algeria")
	`shc' "americansamoa" `shn' "americ") & regexm(`varlist',"samoa")
	`shc' "andorra" `shn' "andorra")
	`shc' "angola" `shn' "angola")
	`shc' "antiguaandbarbuda" `shn' "antigua")
	`shc' "azerbaijan" `shn' "azerbaijan")
	`shc' "argentina" `shn' "argentin")
	`shc' "australia" `shn' "australia")
	`shc' "austria" `shn' "^(?!.*hungary).*austria|austri.*emp")
	`shc' "bahamas" `shn' "bahamas")
	`shc' "bahrain" `shn' "bahrain")
	`shc' "bangladesh" `shn' "bangladesh|^(?=.*east).*paki?stan")
	`shc' "armenia" `shn' "armenia")
	`shc' "barbados" `shn' "barbados")
	`shc' "belgium" `shn' "^(?!.*luxem).*belgium")
	`shc' "bermuda" `shn' "bermuda")
	`shc' "bhutan" `shn' "bhutan")
	`shc' "bolivia" `shn' "bolivia")
	`shc' "bosniaandherzegovina" `shn' "herzegovina|bosnia")
	`shc' "botswana" `shn' "botswana|bechuana")
	`shc' "bouvetisland" `shn' "bouvet")
	`shc' "brazil" `shn' "brazil")
	`shc' "belize" `shn' "belize|^(?=.*british).*honduras")
	`shc' "britishindianoceanterritory" `shn' "british.?indian.?ocean")
	`shc' "solomonislands" `shn' "solomon")
	`shc' "britishvirginislands" `shn' "(?i).*virg.*") & regexm(`varlist',"(?i).*is.*") & regexm(`varlist',"(?i).*brit|uk.*")
	`shc' "brunei" `shn' "brunei")
	`shc' "bulgaria" `shn' "bulgaria")
	`shc' "myanmar" `shn' "myanmar|burma")
	`shc' "burundi" `shn' "burundi")
	`shc' "belarus" `shn' "belarus|byelo")
	`shc' "cambodia" `shn' "cambodia|kampuchea|khmer")
	`shc' "cameroon" `shn' "cameroon")
	`shc' "canada" `shn' "canada")
	`shc' "caboverde" `shn' "verde")
	`shc' "caymanislands" `shn' "cayman")
	`shc' "centralafrican" `shn' "centr") & regexm(`varlist',"afr")
	`shc' "srilanka" `shn' "sri.?lanka|ceylon")
	`shc' "chad" `shn' "chad")
	`shc' "chile" `shn' "chile")
	`shc' "china" `shn' "china") & !regexm(`varlist',"taiw") & !regexm(`varlist',"hong") & !regexm(`varlist',"maca")
	`shc' "taiwan" `shn' "taiwan|taipei|formosa|^(?!.*peo)(?=.*rep).*china")
	`shc' "christmasisland" `shn' "christmas")
	`shc' "cocosislands" `shn' "cocos|keeling")
	`shc' "colombia" `shn' "colombia")
	`shc' "comoros" `shn' "comoro")
	`shc' "mayotte" `shn' "mayotte")
	`shc' "congo" `shn' "congo") & !regexm(`varlist',"dem")
	`shc' "democraticcongo" `shn' "congo") & regexm(`varlist',"dem") | regexm(`varlist',"kinshasa") | regexm(`varlist',"zaire")
	`shc' "cookislands" `shn' "cook")
	`shc' "costarica" `shn' "costa.?rica")
	`shc' "croatia" `shn' "croatia")
	`shc' "cuba" `shn' "cuba")
	`shc' "cyprus" `shn' "cyprus")
	`shc' "czechoslovakia" `shn' "czechoslovakia")
	`shc' "czech" `shn' "czech|czechia|bohemia")
	`shc' "benin" `shn' "benin|dahome")
	`shc' "denmark" `shn' "denmark")
	`shc' "dominica" `shn' "dominica(?!n)")
	`shc' "dominican" `shn' "dominican")
	`shc' "ecuador" `shn' "ecuador")
	`shc' "elsalvador" `shn' "el.?salvador")
	`shc' "equatorialguinea" `shn' "guine.*eq|eq.*guine|^(?=.*span).*guinea")
	`shc' "ethiopia" `shn' "ethiopia|abyssinia")
	`shc' "eritrea" `shn' "eritrea")
	`shc' "estonia" `shn' "estonia")
	`shc' "faroeislands" `shn' "faroe|faeroe")
	`shc' "falklandislands" `shn' "falkland|malvinas")
	`shc' "southgeorgiaandsouthsandwichislands" `shn' "south.?georgia|sandwich")
	`shc' "fiji" `shn' "fiji")
	`shc' "finland" `shn' "finland")
	`shc' "alandislands" `shn' "^[å|a]land")
	`shc' "france" `shn' "france") | regexm(`varlist',"french") & !regexm(`varlist',"dep") & !regexm(`varlist',"martinique") & !regexm(`varlist',"guiana") & !regexm(`varlist',"guyana") & !regexm(`varlist',"polynes") & !regexm(`varlist',"territ") & !regexm(`varlist',"martin")& !regexm(`varlist',"maarten")
	`shc' "frenchguiana" `shn' "french.?gu(y|i)ana")
	`shc' "frenchpolynesia" `shn' "french.?polynesia|tahiti")
	`shc' "frenchsouthernterritories" `shn' "french.?southern")
	`shc' "djibouti" `shn' "djibouti")
	`shc' "gabon" `shn' "gabon")
	`shc' "georgia" `shn' "^(?!.*south).*georgia")
	`shc' "gambia" `shn' "gambia")
	`shc' "palestine" `shn' "palestin|gaza|west.?bank")
	`shc' "germany" `shn' "german") & !regexm(`varlist',"east") & !regexm(`varlist',"federal")
	`shc' "germandemocratic" `shn' "german.?democratic.?rep|democratic.?rep.*germany|east.germany|germany.*east")
	`shc' "ghana" `shn' "ghana|gold.?coast")
	`shc' "gibraltar" `shn' "gibraltar")
	`shc' "kiribati" `shn' "kiribati")
	`shc' "greece" `shn' "greece|hellenic|hellas")
	`shc' "greenland" `shn' "greenland")
	`shc' "grenada" `shn' "grenada")
	`shc' "guadeloupe" `shn' "guadeloupe")
	`shc' "guam" `shn' "guam")
	`shc' "guatemala" `shn' "guatemala")
	`shc' "guinea" `shn' "^(?!.*eq)(?!.*span)(?!.*bissau)(?!.*portu)(?!.*new).*guinea")
	`shc' "guyana" `shn' "^guyana|british.?gu(y|i)ana")
	`shc' "haiti" `shn' "haiti")
	`shc' "heardislandandmcdonaldislands" `shn' "heard.*mcdonald")
	`shc' "vaticancity" `shn' "holy.?see|vatican|papal.?st")
	`shc' "honduras" `shn' "^(?!.*brit).*honduras")
	`shc' "hongkong" `shn' "hong.?kong")
	`shc' "hungary" `shn' "^(?!.*austr).*hungary")
	`shc' "iceland" `shn' "iceland")
	`shc' "india" `shn' "india(?!.*ocea)")
	`shc' "indonesia" `shn' "indonesia")
	`shc' "iran" `shn' "iran|persia")
	`shc' "iraq" `shn' "iraq|mesopotamia")
	`shc' "ireland" `shn' "^(?!.*north).*ireland")
	`shc' "israel" `shn' "israel")
	`shc' "italy" `shn' "italy|italian.?republic")
	`shc' "côtedivoire" `shn' "ivoire|ivory")
	`shc' "jamaica" `shn' "jamaica")
	`shc' "japan" `shn' "japan")
	`shc' "kazakhstan" `shn' "kazak")
	`shc' "jordan" `shn' "jordan")
	`shc' "kenya" `shn' "kenya|british.?east.?africa|east.?africa.?prot")
	`shc' "northkorea" `shn' "korea.*people|dprk|d.p.r.k|korea.+(d.p.r|dpr|north|dem.*rep.*)|(d.p.r|dpr|north|dem.*rep.*).+korea")
	`shc' "southkorea" `shn' "^(?!.*d.*p.*r)(?!.*democrat)(?!.*dem.*rep)(?!.*people)(?!.*north).*korea(?!.*d.*p.*r)(?!.*dem.*rep)")
	`shc' "kuwait" `shn' "kuwait")
	`shc' "kyrgyzstan" `shn' "kyrgyz|kirghiz")
	`shc' "laos" `shn' "lao")
	`shc' "lebanon" `shn' "lebanon")
	`shc' "lesotho" `shn' "lesotho|basuto")
	`shc' "latvia" `shn' "latvia")
	`shc' "liberia" `shn' "liberia")
	`shc' "libya" `shn' "libya")
	`shc' "liechtenstein" `shn' "liechtenstein")
	`shc' "lithuania" `shn' "lithuania")
	`shc' "luxembourg" `shn' "^(?!.*belg).*luxem")
	`shc' "macao" `shn' "maca(o|u)")
	`shc' "madagascar" `shn' "madagascar|malagasy")
	`shc' "malawi" `shn' "malawi|nyasa")
	`shc' "malaysia" `shn' "malaysia")
	`shc' "maldives" `shn' "maldive")
	`shc' "mali" `shn' "mali") & !regexm(`varlist',"somal")
	`shc' "malta" `shn' "malta")
	`shc' "martinique" `shn' "martinique")
	`shc' "mauritania" `shn' "mauritania")
	`shc' "mauritius" `shn' "mauritius")
	`shc' "mexico" `shn' "mexic")
	`shc' "monaco" `shn' "monaco")
	`shc' "mongolia" `shn' "mongolia")
	`shc' "moldova" `shn' "moldov|b(a|e)ssarabia")
	`shc' "montenegro" `shn' "^(?!.*serbia).*montenegro")
	`shc' "montserrat" `shn' "montserrat")
	`shc' "morocco" `shn' "morocco|maroc")
	`shc' "mozambique" `shn' "mozambique")
	`shc' "oman" `shn' "oman|trucial")
	`shc' "namibia" `shn' "namibia")
	`shc' "nauru" `shn' "nauru")
	`shc' "nepal" `shn' "nepal")
	`shc' "netherlands" `shn' "^(?!.*ant)(?!.*carib).*netherlands|holland")
	`shc' "netherlandsantilles" `shn' "netherlands.antil|dutch.antil")
	`shc' "curaçao" `shn' "^(?!.*bonaire).*cura(c|ç)ao")
	`shc' "aruba" `shn' "^(?!.*bonaire).*aruba")
	`shc' "sintmaarten" `shn' "^(?!.*martin)(?!.*saba).*maarten")
	`shc' "bonairesinteustatiusandsaba" `shn' "^(?=.*bonaire).*eustatius|^(?=.*carib).*netherlands|bes.?islands")
	`shc' "newcaledonia" `shn' "new.?caledonia")
	`shc' "vanuatu" `shn' "vanuatu|new.?hebrides")
	`shc' "newzealand" `shn' "new.?zealand")
	`shc' "nicaragua" `shn' "nicaragua")
	`shc' "niger" `shn' "niger(?!ia)")
	`shc' "nigeria" `shn' "nigeria")
	`shc' "niue" `shn' "niue")
	`shc' "norfolkisland" `shn' "norfolk")
	`shc' "norway" `shn' "norway")
	`shc' "northernmarianaislands" `shn' "mariana")
	`shc' "usminoroutlyingislands" `shn' "minor.?outlying.?is")
	`shc' "micronesia" `shn' "fed.*micronesia|micronesia.*fed")
	`shc' "marshallislands" `shn' "marshall")
	`shc' "palau" `shn' "palau")
	`shc' "pakistan" `shn' "^(?!.*east).*paki?stan")
	`shc' "panama" `shn' "panama")
	`shc' "papuanewguinea" `shn' "papua|new.?guinea")
	`shc' "paraguay" `shn' "paraguay")
	`shc' "peru" `shn' "peru")
	`shc' "philippines" `shn' "philippines")
	`shc' "pitcairn" `shn' "pitcairn")
	`shc' "poland" `shn' "poland")
	`shc' "portugal" `shn' "portugal")
	`shc' "guinea-bissau" `shn' "bissau|^(?=.*portu).*guinea")
	`shc' "timorleste" `shn' "^(?=.*leste).*timor|^(?=.*east).*timor")
	`shc' "puertorico" `shn' "puerto.?rico")
	`shc' "qatar" `shn' "qatar")
	`shc' "réunion" `shn' "r(e|é)union")
	`shc' "romania" `shn' "r(o|u|ou)mania")
	`shc' "russia" `shn' "russia|soviet.?union|u\.?s\.?s\.?r|socialist.?republics")
	`shc' "rwanda" `shn' "rwanda")
	`shc' "saintbarthélemy" `shn' "barth(e|é)lemy")
	`shc' "sainthelena" `shn' "helena")
	`shc' "saintkittsandnevis" `shn' "kitts|nevis")
	`shc' "anguilla" `shn' "anguill?a")
	`shc' "saintlucia" `shn' "lucia")
	`shc' "saintmartin" `shn' "saint.martin.*FR|^(?=.*collectivity).*martin|^(?=.*france).*martin(?!ique)|^(?=.*french).*martin(?!ique)")
	`shc' "saintpierreandmiquelon" `shn' "miquelon")
	`shc' "saintvincentandgrenadines" `shn' "vincent")
	`shc' "sanmarino" `shn' "san.?marino")
	`shc' "saotomeandprincipe" `shn' "s(a|ã)o.?tom(e|é)")
	`shc' "saudiarabia" `shn' "sa\w*.?arabia")
	`shc' "senegal" `shn' "senegal")
	`shc' "serbia" `shn' "^(?!.*monte).*serbia")
	`shc' "seychelles" `shn' "seychell")
	`shc' "sierraleone" `shn' "sierra")
	`shc' "singapore" `shn' "singapore")
	`shc' "slovakia" `shn' "^(?!.*cze).*slovak")
	`shc' "vietnam" `shn' "^(?!south)(?!republic).*viet.?nam(?!.*south)|democratic.republic.of.vietnam|socialist.republic.of.viet.?nam|north.viet.?nam|viet.?nam.north")
	`shc' "slovenia" `shn' "slovenia")
	`shc' "somalia" `shn' "somalia")
	`shc' "southafrica" `shn' "south") & regexm(`varlist',"afric")
	`shc' "zimbabwe" `shn' "zimbabwe|^(?!.*northern).*rhodesia")
	`shc' "spain" `shn' "spain")
	`shc' "southsudan" `shn' "sudan") & regexm(`varlist',"south")
	`shc' "sudan" `shn' "^(?!.*s(?!u)).*sudan") & !regexm(`varlist',"south")
	`shc' "westernsahara" `shn' "western.sahara")
	`shc' "suriname" `shn' "surinam|dutch.?gu(y|i)ana")
	`shc' "svalbardandjanmayen" `shn' "svalbard")
	`shc' "eswatini" `shn' "swaziland|eswatini")
	`shc' "sweden" `shn' "sweden")
	`shc' "switzerland" `shn' "switz|swiss")
	`shc' "syria" `shn' "syria")
	`shc' "tajikistan" `shn' "tajik")
	`shc' "thailand" `shn' "thailand|siam")
	`shc' "togo" `shn' "togo")
	`shc' "tokelau" `shn' "tokelau")
	`shc' "tonga" `shn' "tonga")
	`shc' "trinidadandtobago" `shn' "trinidad|tobago")
	`shc' "unitedarabemirates" `shn' "emirates|^u\.?a\.?e\.?$|united.?arab.?em")
	`shc' "tunisia" `shn' "tunisia")
	`shc' "turkey" `shn' "turkey|t(ü|u)rkiye")
	`shc' "turkmenistan" `shn' "turkmen")
	`shc' "turksandcaicosislands" `shn' "turks")
	`shc' "tuvalu" `shn' "tuvalu")
	`shc' "uganda" `shn' "uganda")
	`shc' "ukraine" `shn' "ukrain")
	`shc' "northmacedonia" `shn' "macedonia|fyrom")
	`shc' "sovietunion" `shn' "ussr")
	`shc' "egypt" `shn' "egypt")
	`shc' "unitedkingdom" `shn' "united.?kingdom|britain|^u\.?k\.?$")
	`shc' "guernsey" `shn' "guernsey")
	`shc' "jersey" `shn' "jersey")
	`shc' "isleman" `shn' "isle") & regexm(`varlist',"man")
	`shc' "tanzania" `shn' "tanzania")
	`shc' "unitedstates" `shn' "united") & regexm(`varlist',"state") & !regexm(`varlist',"island")
	`shc' "unitedstatesvirginislands" `shn' "virg") & regexm(`varlist',"island") & !regexm(`varlist',"brit") & !regexm(`varlist',"uk") & !regexm(`varlist',"u.k")
	`shc' "burkinafaso" `shn' "burkina|upper.?volta")
	`shc' "uruguay" `shn' "uruguay")
	`shc' "uzbekistan" `shn' "uzbek")
	`shc' "venezuela" `shn' "venezuela")
	`shc' "wallisandfutuna" `shn' "futuna|wallis")
	`shc' "samoa" `shn' "^(?!.*amer).*samoa")
	`shc' "yemen" `shn' "yemen")
	`shc' "yugoslavia" `shn' "yugoslavia")
	`shc' "zambia" `shn' "zambia|northern.?rhodesia")
	
	keep if __cfill__!=""
	rename __cfill__ cntryname
	foreach ind in `gen' {
		if "`ind'"!="cntryname" merge m:1 cntryname using "$cpath/isocodes.dta", nogen keepusing(`ind') keep(1 3)
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
				merge 1:m `notind' using "$cpath/isocodes.dta", nogen keepusing(`ind') keep(1 3)
				replace `ind'_temp = `ind' if mi(`ind'_temp) & !mi(`ind')
				drop `ind'
			}
			rename `ind'_temp `ind'
		}
	}
}

// capitalize strings for cntryname 
if strpos("`gen'","cntryname") {
	merge 1:m cntryname using "$cpath/isocodes.dta", nogen keepusing(cntryname_uc) keep(1 3)
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




















