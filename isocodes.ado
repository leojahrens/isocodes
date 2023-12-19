*! version 1.3   Leo Ahrens   leo@ahrensmail.de

program define isocodes
version 14.0

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

local keepr_substr = subinstr(subinstr(subinstr(subinstr("`keepregion'","oecd","",.),"eu","",.),"emu","",.)," ","",.)
if "`keepr_substr'"!="" {
	di as error "keepregion() only accepts {it:oecd}, {it:eu}, and {it:emu}"
	exit 498
}
	
*-------------------------------------------------------------------------------
* prep ados & country dataset 
*-------------------------------------------------------------------------------

quietly {

// gtools yes/no
if "`slow'"!="" {
	local lvlsof levelsof
	local ggen egen
	local gduplicates duplicates
	local isid isid
	local sort sort
}
else {
	capture which gtools
	if _rc==111 {
		ssc install gtools, replace 
		gtools, upgrade
	}
	local lvlsof glevelsof
	local ggen gegen
	local gduplicates gduplicates
	local isid gisid
	local sort gsort
}

// get current version of the country strings dataset 
cap describe using "`c(sysdir_plus)'i/isocodes.dta", short varl
local ccodes_data_varl = r(varlist)
if _rc | !strpos("`ccodes_data_varl'","version5") {
	net set other "`c(sysdir_plus)'i"
	net get isocodes, from("https://raw.githubusercontent.com/leojahrens/isocodes/master") replace
}

*-------------------------------------------------------------------------------
* prep dataset
*-------------------------------------------------------------------------------

// report missings in the country string
qui count if `varlist'==""
if r(N)>0 {
	noisily di "-------"
	noisily di "Your input variable {it:`varlist'} has " r(N) " missing values. They cannot be matched. Consider dropping or recoding them."
	if "`keepregion'`keepiso3n'`keepiso3c'`keepiso2c'"!="" noisily di "You drop countries not included in keep{it:x}(). Observations with missings in {it:`varlist'} are dropped by definition. Replace these values of {it:`varlist'} if they include actual countries"
}

// check for variables with same names as in gen()
foreach __var in `gen' {
	cap confirm variable `__var'
	if !_rc {
		noisily di "-------"
		drop `__var'
		noisily di "A variable named `__var' already exists in the dataset. It has been dropped and replaced."
	}
}

// reduce dataset to minimal obs&vars
preserve 
`ggen' __tag = tag(`varlist')
keep if __tag==1 & `varlist'!=""
keep `varlist'

// create stripped version of the input country string
clonevar __ogcountry = `varlist'
replace `varlist' = subinstr(subinstr(subinstr(lower(`varlist'),":"," ",.),"'"," ",.),"-"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist',","," ",.),"/"," ",.),"("," ",.),")"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist',"."," ",.),"&"," ",.)," republics"," ",.)," the"," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," of"," ",.),"of "," ",.)," of "," ",.)," the "," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," republic"," ",.),"republic "," ",.)," republic "," ",.),"the "," ",.)
replace `varlist' = subinstr(subinstr(subinstr(subinstr(`varlist'," rep"," ",.),"rep "," ",.)," rep "," ",.)," ","",.)

*-------------------------------------------------------------------------------
* match country codes with attached dataset
*-------------------------------------------------------------------------------

// list of variables in attached dataset
local countrylist__ cntryname iso2c iso3c wb3c wb2c ifs penn eurostat ecb cow unctad marc  unhcr ioc name2 name3 name4 name5 name6 name7 cntryname_copy

// extend gen() list if further vars are required for keepx()
if "`keepregion'`keepiso3n'`keepiso3c'`keepiso2c'"!="" {
	local oggen `gen'
	foreach keepopt in iso3n iso3c iso2c {
		if "`keep`keepopt''"!="" & !strpos("`gen'","`keepopt'") local gen `gen' `keepopt'
	}
	if "`keepregion'"!="" & !strpos("`gen'","iso3n") local gen `gen' iso3n
}

// merge input string with all country names/codes in attached dataset
local __cname__ "`varlist'"
foreach __cvar__ in `countrylist__' {
	if "`__cvar__'"!="`gen'" {
		rename `__cname__' `__cvar__'
		merge m:m `__cvar__' using "`c(sysdir_plus)'i/isocodes.dta", nogen keepusing(`gen') keep(1 3)
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
				count if `mihelp' 		// count non-missing values for below
				local `ind'_`__cvar__'_nm = r(N)	
			}
		}
		local __cname__ `__cvar__'
	}
}

// find out what variable from the attached dataset gives closest match to input string
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
	if "`ind'_``ind'_highestno'"!="`ind'_" {
		rename `ind'_``ind'_highestno' `ind'
		local `ind'_highnoproceed "yes"
	}
	else {
		rename `ind'_cntryname_copy `ind'
	}	
}

// use best match as baseline & fill missings with matches from other vars in attchd data
foreach ind in `gen' {
	if "``ind'_highnoproceed'"=="yes" { // only if there was at least one match
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

*-------------------------------------------------------------------------------
* match missings with regular expressions
*-------------------------------------------------------------------------------

// check if there are non matches
foreach ind in `gen' {
	count if mi(`ind')
	if r(N)>0 local anymiss "yes"
}

// proceed with reg ex if yes
if "`anymiss'"=="yes" {
	
	// save & restrict data to missings
	tempfile __ogdata __fillup
	save `__ogdata', replace 
	foreach ind in `gen' {
		local indgenmiss `indgenmiss' | mi(`ind')
	}
	keep if 1==2 `indgenmiss'
	keep `varlist' __ogcountry
	
	// gen new country var and match with regular expressions
	gen __cfill__ = ""
	gen __maybewrong = 0
	local nameofvarlist `varlist'

	local shc replace __cfill__ =
	local shn regexm(`varlist',

	`shc' "afghanistan" if `shn'"afghan")
	`shc' "albania" if `shn'"albania")
	`shc' "antarctica" if `shn'"antarctica")
	`shc' "algeria" if `shn'"algeria")
	`shc' "americansamoa" if (`shn'"am|us|u.s") & `shn'"samoa")) & !`shn'"west|wsamoa")
	`shc' "andorra" if `shn'"andorra")
	`shc' "angola" if `shn'"angola")
	`shc' "antiguaandbarbuda" if `shn'"antigua|barbuda")
	`shc' "azerbaijan" if `shn'"azerbai(j|dsch)an")
	`shc' "argentina" if `shn'"argentin")
	`shc' "australia" if `shn'"australia")
	`shc' "austria" if `shn'"austria") | `shn'"^(?!.*hungary).*austria|austri.*emp")
	`shc' "bahamas" if `shn'"bahamas")
	`shc' "bahrain" if `shn'"bahrain")
	`shc' "bangladesh" if `shn'"bangladesh|^(?=.*east).*paki?stan")
	`shc' "armenia" if `shn'"armenia")
	`shc' "barbados" if `shn'"barbados")
	`shc' "belgium" if `shn'"^(?!.*luxem).*belgium")
	`shc' "bermuda" if `shn'"bermuda")
	`shc' "bhutan" if `shn'"bhutan")
	`shc' "bolivia" if `shn'"bolivia")
	`shc' "bosniaandherzegovina" if `shn'"herzegovina|bosnia")
	`shc' "botswana" if `shn'"botswana|bechuana")
	`shc' "bouvetisland" if `shn'"bouvet")
	`shc' "brazil" if `shn'"brazil")
	`shc' "belize" if `shn'"belize") | (`shn'"honduras") & `shn'"brit|uk"))
	`shc' "britishindianoceanterritory" if `shn'"british.?indian.?ocean") | `shn'"chagos.?island|archipel")
	`shc' "solomonislands" if `shn'"solomon")
	`shc' "britishvirginislands" if `shn'"virgin") & `shn'"island") & !`shn'"us|states|united|americ")
	`shc' "brunei" if `shn'"brunei")
	`shc' "bulgaria" if `shn'"bulgaria")
	`shc' "myanmar" if `shn'"myanmar|burma")
	`shc' "burundi" if `shn'"burundi")
	`shc' "belarus" if `shn'"belarus|byelo")
	`shc' "cambodia" if `shn'"cambodia|kampuchea|khmer")
	`shc' "cameroon" if `shn'"cameroon")
	`shc' "canada" if `shn'"canada")
	`shc' "caboverde" if `shn'"verde")
	`shc' "caymanislands" if `shn'"cayman")
	`shc' "centralafrican" if `shn'"central") & `shn'"african") & !`shn'"economic|monetary|north|south|east")
	`shc' "srilanka" if `shn'"sri.?lanka|ceylon")
	`shc' "chad" if `shn'"chad")
	`shc' "chile" if `shn'"chile")
	`shc' "china" if `shn'"china") & !`shn'"taiw|hongmaca|commodity|emde")
	`shc' "taiwan" if `shn'"taiwan|taipei|formosa")
	`shc' "christmasisland" if `shn'"christmas")
	`shc' "cocosislands" if `shn'"cocos|keeling")
	`shc' "colombia" if `shn'"colombia")
	`shc' "comoros" if `shn'"comoro")
	`shc' "mayotte" if `shn'"mayotte|maore|mahor[eé]")
	`shc' "congo" if `shn'"congo") & !`shn'"dem|dr")
	`shc' "democraticcongo" if (`shn'"congo") & `shn'"dem|dr")) | `shn'"kinshasa|zaire")
	`shc' "cookislands" if `shn'"cook")
	`shc' "costarica" if `shn'"costa.?rica")
	`shc' "croatia" if `shn'"croatia")
	`shc' "cuba" if `shn'"cuba")
	`shc' "cyprus" if `shn'"cyprus") & !`shn'"turk|northern")
	`shc' "czechoslovakia" if `shn'"czechoslovak")
	`shc' "czech" if `shn'"czech|czechia|bohemia") & !`shn'"czechoslovak")
	`shc' "benin" if `shn'"benin|dahome")
	`shc' "denmark" if `shn'"denmark")
	`shc' "dominica" if `shn'"dominica(?!n)")
	`shc' "dominican" if `shn'"dominican")
	`shc' "ecuador" if `shn'"ecuador")
	`shc' "elsalvador" if `shn'"el.?salvador")
	`shc' "equatorialguinea" if `shn'"guine.*eq|eq.*guine|^(?=.*span).*guinea")
	`shc' "ethiopia" if `shn'"ethiopia|abyssinia")
	`shc' "eritrea" if `shn'"eritrea")
	`shc' "estonia" if `shn'"estonia")
	`shc' "faroeislands" if `shn'"faroe|faeroe")
	`shc' "falklandislands" if `shn'"falkland|malvinas")
	`shc' "southgeorgiaandsouthsandwichislands" if `shn'"south.?georgia|sandwich")
	`shc' "fiji" if `shn'"fiji")
	`shc' "finland" if `shn'"finland")
	`shc' "alandislands" if `shn'"^(?i)[å|a]land")
	`shc' "france" if `shn'"france|french") & !`shn'"dep|gu[yi]ana|polynes|territ|martin|maarten|island|colon")
	`shc' "frenchguiana" if `shn'"french|france") & `shn'"gu[yi]ana")
	`shc' "frenchpolynesia" if `shn'"french|france") & `shn'"polynesia|tahiti")
	`shc' "frenchsouthernterritories" if `shn'"french.?southern")
	`shc' "djibouti" if `shn'"djibouti")
	`shc' "gabon" if `shn'"gabon")
	`shc' "georgia" if `shn'"^(?!.*south).*georgia")
	`shc' "gambia" if `shn'"gambia")
	`shc' "palestine" if `shn'"palestin|gaza|west.?bank")
	`shc' "germany" if `shn'"german") & (!`shn'"east|soviet|dem") | `shn'"federal"))
	`shc' "germandemocratic" if `shn'"german") & `shn'"east|soviet|democratic") & !`shn'"fed")
	`shc' "ghana" if `shn'"ghana|gold.?coast")
	`shc' "gibraltar" if `shn'"gibraltar")
	`shc' "kiribati" if `shn'"kiribati")
	`shc' "greece" if `shn'"greece|hellenic|hellas")
	`shc' "greenland" if `shn'"greenland")
	`shc' "grenada" if `shn'"grenada")
	`shc' "guadeloupe" if `shn'"guadeloupe|guadelope|calucaera|karukera")
	`shc' "guam" if `shn'"guam")
	`shc' "guatemala" if `shn'"guatemala")
	`shc' "guinea" if `shn'"guinea") & !`shn'"eq|span|bissau|portu|new")
	`shc' "guyana" if `shn'"gu[yi]]ana") & !`shn'"french|france|british|uk|u.k")
	`shc' "haiti" if `shn'"haiti")
	`shc' "heardislandandmcdonaldislands" if `shn'"heard.*m?.cdonald")
	`shc' "vaticancity" if `shn'"holy.?see|vatican|papal.?st")
	`shc' "honduras" if `shn'"honduras") & !`shn'"brit|uk|u.k")
	`shc' "hongkong" if `shn'"hong.?kong")
	`shc' "hungary" if `shn'"^(?!.*austr).*hungary")
	`shc' "iceland" if `shn'"iceland")
	`shc' "india" if `shn'"india(?!.*ocea)")
	`shc' "indonesia" if `shn'"indonesia")
	`shc' "iran" if `shn'"iran|persia")
	`shc' "iraq" if `shn'"iraq|mesopotamia")
	`shc' "ireland" if `shn'"^(?!.*north).*ireland")
	`shc' "israel" if `shn'"israel")
	`shc' "italy" if `shn'"italy|italian")
	`shc' "côtedivoire" if `shn'"ivoire|ivory")
	`shc' "jamaica" if `shn'"jamaica")
	`shc' "japan" if `shn'"japan")
	`shc' "kazakhstan" if `shn'"kazak")
	`shc' "jordan" if `shn'"jordan")
	`shc' "kenya" if `shn'"kenya|british.?east.?africa|east.?africa.?prot")
	`shc' "northkorea" if `shn'"korea.*people|dprk|d.p.r.k|korea.+(d.p.r|dpr|north)|(d.p.r|dpr|north).+korea") & !`shn'"south")
	`shc' "southkorea" if `shn'"korea") & !`shn'"north|dpr|d.p.r|democrat|people|nkorea|korea")
	`shc' "kuwait" if `shn'"kuwait")
	`shc' "kyrgyzstan" if `shn'"kyrgyz|kirghiz")
	`shc' "laos" if `shn'"lao")
	`shc' "lebanon" if `shn'"lebanon")
	`shc' "lesotho" if `shn'"lesotho|basuto")
	`shc' "latvia" if `shn'"latvia")
	`shc' "liberia" if `shn'"liberia")
	`shc' "libya" if `shn'"libya")
	`shc' "liechtenstein" if `shn'"liechtenstein|lichtenstein")
	`shc' "lithuania" if `shn'"lith")
	`shc' "luxembourg" if `shn'"luxe[mn]") & !`shn'"belg")
	`shc' "macao" if `shn'"maca(o|u)")
	`shc' "madagascar" if `shn'"madagascar|malagasy")
	`shc' "malawi" if `shn'"malawi|nyasa")
	`shc' "malaysia" if `shn'"malaysia")
	`shc' "maldives" if `shn'"maldive")
	`shc' "mali" if `shn'"mali") & !`shn'"somal")
	`shc' "malta" if `shn'"malta")
	`shc' "martinique" if `shn'"martinique")
	`shc' "mauritania" if `shn'"mauritania")
	`shc' "mauritius" if `shn'"mauritius")
	`shc' "mexico" if `shn'"mexic")
	`shc' "monaco" if `shn'"monaco")
	`shc' "mongolia" if `shn'"mongolia")
	`shc' "moldova" if `shn'"moldov|b(a|e)ssarabia")
	`shc' "montenegro" if `shn'"^(?!.*serbia).*montenegro")
	`shc' "montserrat" if `shn'"montserrat")
	`shc' "morocco" if `shn'"morocco|maroc")
	`shc' "mozambique" if `shn'"mo(z|c|ç)ambique")
	`shc' "oman" if `shn'"oman|trucial")
	`shc' "namibia" if `shn'"namibia")
	`shc' "nauru" if `shn'"nauru")
	`shc' "nepal" if `shn'"nepal")
	`shc' "netherlands" if `shn'"netherlands|holland") & !`shn'"ant|carib")
	`shc' "netherlandsantilles" if `shn'"antil") & `shn'"dutch|netherland|holland") & !`shn'"former|previou")
	`shc' "curaçao" if `shn'"cura(c|ç)ao") & !`shn'"maarten|martin|bonaire|eustatius|saba|netherland|dutch")
	`shc' "aruba" if `shn'"aruba") & !`shn'"bonaire")
	`shc' "sintmaarten" if `shn'"maarten") & !`shn'"martin|saint|saba|cura(c|ç)ao|french|france")
	`shc' "bonairesinteustatiusandsaba" if `shn'"^(?=.*bonaire).*eustatius|^(?=.*carib).*netherlands|bes.?islands")
	`shc' "newcaledonia" if `shn'"new.?caledonia")
	`shc' "vanuatu" if `shn'"vanuatu|new.?hebrides")
	`shc' "newzealand" if `shn'"new.?zealand")
	`shc' "nicaragua" if `shn'"nicaragua")
	`shc' "niger" if `shn'"niger(?!ia)")
	`shc' "nigeria" if `shn'"nigeria")
	`shc' "niue" if `shn'"niue")
	`shc' "norfolkisland" if `shn'"norfolk")
	`shc' "norway" if `shn'"norway")
	`shc' "northernmarianaislands" if `shn'"mariana")
	`shc' "usminoroutlyingislands" if `shn'"minor.?outl.*is|us.*outlying.*is")
	`shc' "micronesia" if `shn'"fed.*micronesia|micronesia.*fed")
	`shc' "marshallislands" if `shn'"marshall")
	`shc' "palau" if `shn'"palau")
	`shc' "pakistan" if `shn'"^(?!.*east).*paki?stan")
	`shc' "panama" if `shn'"panama")
	`shc' "papuanewguinea" if `shn'"papua|new.?guinea|pnguinea")
	`shc' "paraguay" if `shn'"paraguay")
	`shc' "peru" if `shn'"peru")
	`shc' "philippines" if `shn'"philippines")
	`shc' "pitcairn" if `shn'"pitcairn")
	`shc' "poland" if `shn'"poland")
	`shc' "portugal" if `shn'"portugal")
	`shc' "guinea-bissau" if `shn'"bissau|^(?=.*portu).*guinea")
	`shc' "timorleste" if `shn'"^(?=.*leste).*timor|^(?=.*east).*timor|timoreast")
	`shc' "puertorico" if `shn'"puerto.*rico")
	`shc' "qatar" if `shn'"qatar")
	`shc' "réunion" if `shn'"r(e|é)union")
	`shc' "romania" if `shn'"r(o|u|ou)mania")
	`shc' "russia" if `shn'"russia|soviet.?union|u\.?s\.?s\.?r|socialist.?republics") & !`shn'"prussia")
	`shc' "rwanda" if `shn'"rwanda")
	`shc' "saintbarthélemy" if `shn'"barth(e|é)lemy")
	`shc' "sainthelena" if `shn'"helena")
	`shc' "saintkittsandnevis" if `shn'"kitts|nevis")
	`shc' "anguilla" if `shn'"anguill?a")
	`shc' "saintlucia" if `shn'"lucia")
	`shc' "saintmartin" if `shn'"saint.martin.*FR|^(?=.*collectivity).*martin|^(?=.*france).*martin(?!ique)|^(?=.*french).*martin(?!ique)")
	`shc' "saintpierreandmiquelon" if `shn'"miquelon")
	`shc' "saintvincentandgrenadines" if `shn'"vincent")
	`shc' "sanmarino" if `shn'"san.?marino")
	`shc' "saotomeandprincipe" if `shn'"s(a|ã)o.*tom(e|é)") | ustrregexm(`varlist',"s.o.*tom.*pr.ncipe")
	`shc' "saudiarabia" if `shn'"sa\w*.?arabia")
	`shc' "senegal" if `shn'"senegal")
	`shc' "serbia" if `shn'"^(?!.*monte).*serbia")
	`shc' "seychelles" if `shn'"seychell")
	`shc' "sierraleone" if `shn'"sierra")
	`shc' "singapore" if `shn'"singapore")
	`shc' "slovakia" if `shn'"^(?!.*cze).*slovak")
	`shc' "vietnam" if `shn'"^(?!south).*viet.?nam(?!.*south)|democratic.vietnam|socialist.viet.?nam|north.viet.?nam|viet.?nam.north")
	`shc' "slovenia" if `shn'"slovenia")
	`shc' "somalia" if `shn'"somalia")
	`shc' "southafrica" if ((`shn'"south(?!e)") & `shn'"afric")) | `shn'"safrica")) & !`shn'"west|economic|monetary|north|east")
	`shc' "zimbabwe" if `shn'"zimbabwe|^(?!.*northern).*rhodesia")
	`shc' "spain" if `shn'"spain")
	`shc' "southsudan" if `shn'"sudan") & `shn'"south")
	`shc' "sudan" if `shn'"^(?!.*s(?!u)).*sudan") & !`shn'"south")
	`shc' "westernsahara" if `shn'"western") & `shn'"sahara")
	`shc' "suriname" if `shn'"surinam|dutch.?gu(y|i)ana")
	`shc' "svalbardandjanmayen" if `shn'"svalbard")
	`shc' "eswatini" if `shn'"swaziland|eswatini")
	`shc' "sweden" if `shn'"sweden")
	`shc' "switzerland" if `shn'"switz|swiss")
	`shc' "syria" if `shn'"syria")
	`shc' "tajikistan" if `shn'"tajik") | `shn'"tadschikistan")
	`shc' "thailand" if `shn'"thailand|siam")
	`shc' "togo" if `shn'"togo")
	`shc' "tokelau" if `shn'"tokelau")
	`shc' "tonga" if `shn'"tonga")
	`shc' "trinidadandtobago" if (`shn'"trinidad") & `shn'"tob")) | (`shn'"trin") & `shn'"tobago"))
	`shc' "unitedarabemirates" if `shn'"emirates|^u\.?a\.?e\.?$|united.?arab.?em")
	`shc' "tunisia" if `shn'"tunisia")
	`shc' "turkey" if `shn'"turkey|t(ü|u)rkiye")
	`shc' "turkmenistan" if `shn'"turkmen")
	`shc' "turksandcaicosislands" if `shn'"turks") | (`shn'"island") & `shn'"caicos"))
	`shc' "tuvalu" if `shn'"tuvalu")
	`shc' "uganda" if `shn'"uganda")
	`shc' "ukraine" if `shn'"ukrain")
	`shc' "northmacedonia" if `shn'"macedonia|fyrom")
	`shc' "sovietunion" if `shn'"ussr")
	`shc' "egypt" if `shn'"egypt")
	`shc' "unitedkingdom" if `shn'"united.?kingdom|britain")
	`shc' "guernsey" if `shn'"guernsey")
	`shc' "jersey" if `shn'"jersey")
	`shc' "isleman" if `shn'"isle") & `shn'"man")
	`shc' "tanzania" if `shn'"tanzania")
	`shc' "unitedstates" if (`shn'"usa|^us$") | (`shn'"united") & `shn'"states"))) & !(`shn'"^(?=.*bonaire).*eustatius|^(?=.*carib).*netherlands|bes.?islands") | `shn'"virg") | `shn'"outlying"))
	`shc' "unitedstatesvirginislands" if `shn'"virgin") & `shn'"island") & !`shn'"brit|u.k|uk|english")
	`shc' "burkinafaso" if `shn'"burkina|upper.?volta")
	`shc' "uruguay" if `shn'"uruguay")
	`shc' "uzbekistan" if `shn'"uzbek")
	`shc' "venezuela" if `shn'"venezuela")
	`shc' "wallisandfutuna" if `shn'"futuna|fortuna|wallis")
	`shc' "samoa" if `shn'"^(?!.*amer).*samoa")
	`shc' "yemen" if `shn'"yemen")
	`shc' "yugoslavia" if `shn'"yugoslavia") | `shn'"serbia.*montenegro") 
	`shc' "zambia" if `shn'"zambia|northern.?rhodesia")
	
	// mark likely region/aggregate observations
	replace __maybewrong = 1 if `shn'"monetary|economic|economies|import|average|total|world|developing|industrialia") | (`shn'"income") & `shn'"high|mid|low"))
	`shc' "" if `shn'"monetary|economic|economies|import|average|total|world|developing|industrialia") | (`shn'"income") & `shn'"high|mid|low"))
	count if __maybewrong==1 
	if r(N)>0 local onewrong = "yes"

	// restrict dataset to successful matches
	count if __cfill__!=""
	if r(N)>0 local cfillcheck "yes"
	keep if __cfill__!="" | __maybewrong==1
	
	// merge gen() list for all reg ex matches
	rename __cfill__ cntryname
	cap rename cntryname_uc __aa_
	local counthere =1
	foreach ind in `gen' {
		if `counthere'==1 local addogcntryname cntryname_uc
		if `counthere'!=1 local addogcntryname
		if "`ind'"!="cntryname" local addogcntryname `addogcntryname' `ind'
		if "`addogcntryname'"!="" merge m:1 cntryname using "`c(sysdir_plus)'i/isocodes.dta", nogen keepusing(`addogcntryname') keep(1 3)
		local ++counthere
	}
	
	// report reg ex matches 
	rename cntryname_uc _match
	cap rename __aa_ cntryname_uc 
	rename __ogcountry _input
	if "`cfillcheck'"=="yes" {
		noisily di  "-------"
		noisily di "The countries listed below were matched with uncertainty. Please check if correct."
		noisily list _input _match if __maybewrong!=1
	}
	drop _input _match

	// store reg ex matches and merge them to matches from above
	foreach ind in `gen' {
		rename `ind' `ind'_regex
		local regexkeep `regexkeep' `ind'_regex
	}
	if "`onewrong'"=="yes" local regexkeep `regexkeep' __maybewrong
	save `__fillup', replace 
	use `__ogdata', clear
	merge m:1 `varlist' using `__fillup', nogen keepusing(`regexkeep') keep(1 3)
	foreach ind in `gen' {
		replace `ind' = `ind'_regex if mi(`ind') & !mi(`ind'_regex)
		drop `ind'_regex
	}
}

*-------------------------------------------------------------------------------
* finalize match dataset
*-------------------------------------------------------------------------------

// match remaining missings based on other gen() variables
if wordcount("`gen'")>1 {
	local morethanone "yes"
	foreach ind in `gen' {
		count if mi(`ind')
		if r(N)>0 {
			if "`onewrong'"=="yes" local onemore __maybewrong
			keep `varlist' __ogcountry `gen' `onemore'
			rename `ind' `ind'_temp
			local not_`ind' = subinstr("`gen'","`ind'","",.)
			foreach notind in `not_`ind'' {
				merge m:m `notind' using "`c(sysdir_plus)'i/isocodes.dta", nogen keepusing(`ind') keep(1 3)
				replace `ind'_temp = `ind' if mi(`ind'_temp) & !mi(`ind')
				drop `ind'
			}
			rename `ind'_temp `ind'
		}
	}
}

// capitalize strings
if strpos("`gen'","cntryname") {
	merge m:m cntryname using "`c(sysdir_plus)'i/isocodes.dta", nogen keepusing(cntryname_uc) keep(1 3)
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

*-------------------------------------------------------------------------------
* restrict sample
*-------------------------------------------------------------------------------

if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" {
	
	gen __keeeep__ = .
	
	if "`keepregion'"!="" {
		local __oecd_countries__ 36 40 56 124 152 203 208 233 246 250 276 300 348 352 372 ///
		376 380 392 410 428 440 442 484 528 554 578 616 620 703 705 724 752 756 792 826 840
		
		local __eu_countries__ 40 56 100 191 196 203 208 233 246 250 276 300 348 372 380 ///
		428 440 442 470 528 616 620 642 703 705 724 752 826
		
		local __emu_countries__ 40 56 196 233 246 250 276 300 372 380 428 442 470 528 620 703 705 724

		
		foreach utut in `keepregion' {
			foreach lklk of local __`utut'_countries__ {
				replace __keeeep__ = 1 if iso3n==`lklk'
			}
		}
	}

	if "`keepiso3n'"!="" {
		foreach lklkl of numlist `keepiso3n' {
			replace __keeeep__ = 1 if iso3n==`lklkl'
		}
	}

	if "`keepiso2c'"!="" {
		foreach lklkl in `keepiso2c' {
			replace __keeeep__ = 1 if iso2c=="`lklkl'"
		}
	}

	if "`keepiso3c'"!="" {
		foreach lklkl in `keepiso3c' {
			replace __keeeep__ = 1 if iso3c=="`lklkl'"
		}
	}
	
	local gen `oggen'

}

*-------------------------------------------------------------------------------
* final checks
*-------------------------------------------------------------------------------

// count missings
if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" {
	local __keepif & __keeeep__==1
	local __keeploc __keeeep__
}
if "`onewrong'"=="yes" {
	local __keepif `__keepif' & __maybewrong!=1
	local __keeploc `__keeploc' __maybewrong
}
foreach ind in `gen' {
	count if mi(`ind') `__keepif'
	if r(N)>0 {
		local `ind'miss = r(N)
	}
	else {
		local `ind'miss = 0
	}
}

// check for duplicates
foreach ind in `gen' {
	cap `isid' `ind' if !mi(`ind') `__keepif'
	if _rc {
		rename __ogcountry _input
		`sort' `ind'
		`gduplicates' tag `ind' if !mi(`ind') `__keepif', gen(_dupl_`ind')
		noisily di  "-------"
		noisily dis "The countries listed below were matched more than once. Please check if correct."
		noisily list _input `ind' if _dupl_`ind'==1 `__keepif'
		rename _input __ogcountry
	}
}

*-------------------------------------------------------------------------------
* iso label
*-------------------------------------------------------------------------------

if strpos("`gen'","iso3n") & "`nolabel'"=="" {
	cap label drop __isolabels__
	label define __isolabels__  4 "Afghanistan" 8 "Albania" 10 "Antarctica" 12 "Algeria" 16 "American Samoa" 20 "Andorra" 24 "Angola" 28 "Antigua and Barbuda" 31 "Azerbaijan" 32 "Argentina" 36 "Australia" 40 "Austria" 44 "Bahamas" 48 "Bahrain" 50 "Bangladesh" 51 "Armenia" 52 "Barbados" 56 "Belgium" 60 "Bermuda" 64 "Bhutan" 68 "Bolivia" 70 "Bosnia and Herzegovina" 72 "Botswana" 74 "Bouvet Island" 76 "Brazil" 84 "Belize" 86 "British Indian Ocean Territory" 90 "Solomon Islands" 92 "British Virgin Islands" 96 "Brunei Darussalam" 100 "Bulgaria" 104 "Myanmar" 108 "Burundi" 112 "Belarus" 116 "Cambodia" 120 "Cameroon" 124 "Canada" 132 "Cabo Verde" 136 "Cayman Islands" 140 "Central African Republic" 144 "Sri Lanka" 148 "Chad" 152 "Chile" 156 "China" 158 "Taiwan" 162 "Christmas Island" 166 "Cocos (Keeling) Islands" 170 "Colombia" 174 "Comoros" 175 "Mayotte" 178 "Congo" 180 "Democratic Republic of the Congo" 184 "Cook Islands" 188 "Costa Rica" 191 "Croatia" 192 "Cuba" 196 "Cyprus" 203 "Czechia" 204 "Benin" 208 "Denmark" 212 "Dominica" 214 "Dominican Republic" 218 "Ecuador" 222 "El Salvador" 226 "Equatorial Guinea" 231 "Ethiopia" 232 "Eritrea" 233 "Estonia" 234 "Faroe Islands" 238 "Falkland Islands" 239 "South Georgia and the South Sandwich Islands" 242 "Fiji" 246 "Finland" 248 "Åland Islands" 250 "France" 254 "French Guiana" 258 "French Polynesia" 260 "French Southern Territories" 262 "Djibouti" 266 "Gabon" 268 "Georgia" 270 "Gambia" 275 "State of Palestine" 276 "Germany" 288 "Ghana" 292 "Gibraltar" 296 "Kiribati" 300 "Greece" 304 "Greenland" 308 "Grenada" 312 "Guadeloupe" 316 "Guam" 320 "Guatemala" 324 "Guinea" 328 "Guyana" 332 "Haiti" 334 "Heard Island and McDonald Islands" 336 "Holy See" 340 "Honduras" 344 "Hong Kong" 348 "Hungary" 352 "Iceland" 356 "India" 360 "Indonesia" 364 "Iran" 368 "Iraq" 372 "Ireland" 376 "Israel" 380 "Italy" 384 "Côte d'Ivoire" 388 "Jamaica" 392 "Japan" 398 "Kazakhstan" 400 "Jordan" 404 "Kenya" 408 "North Korea" 410 "South Korea" 414 "Kuwait" 417 "Kyrgyzstan" 418 "Laos" 422 "Lebanon" 426 "Lesotho" 428 "Latvia" 430 "Liberia" 434 "Libya" 438 "Liechtenstein" 440 "Lithuania" 442 "Luxembourg" 446 "Macao" 450 "Madagascar" 454 "Malawi" 458 "Malaysia" 462 "Maldives" 466 "Mali" 470 "Malta" 474 "Martinique" 478 "Mauritania" 480 "Mauritius" 484 "Mexico" 492 "Monaco" 496 "Mongolia" 498 "Moldova" 499 "Montenegro" 500 "Montserrat" 504 "Morocco" 508 "Mozambique" 512 "Oman" 516 "Namibia" 520 "Nauru" 524 "Nepal" 528 "Netherlands" 531 "Curaçao" 533 "Aruba" 534 "Sint Maarten" 535 "Bonaire, Sint Eustatius and Saba" 540 "New Caledonia" 548 "Vanuatu" 554 "New Zealand" 558 "Nicaragua" 562 "Niger" 566 "Nigeria" 570 "Niue" 574 "Norfolk Island" 578 "Norway" 580 "Northern Mariana Islands" 581 "US Minor Outlying Islands" 583 "Micronesia" 584 "Marshall Islands" 585 "Palau" 586 "Pakistan" 591 "Panama" 598 "Papua New Guinea" 600 "Paraguay" 604 "Peru" 608 "Philippines" 612 "Pitcairn" 616 "Poland" 620 "Portugal" 624 "Guinea-Bissau" 626 "Timor-Leste" 630 "Puerto Rico" 634 "Qatar" 638 "Réunion" 642 "Romania" 643 "Russia" 646 "Rwanda" 652 "Saint Barthélemy" 654 "Saint Helena" 659 "Saint Kitts and Nevis" 660 "Anguilla" 662 "Saint Lucia" 663 "Saint Martin" 666 "Saint Pierre and Miquelon" 670 "Saint Vincent and the Grenadines" 674 "San Marino" 678 "Sao Tome and Principe" 682 "Saudi Arabia" 686 "Senegal" 688 "Serbia" 690 "Seychelles" 694 "Sierra Leone" 702 "Singapore" 703 "Slovakia" 704 "Vietnam" 705 "Slovenia" 706 "Somalia" 710 "South Africa" 716 "Zimbabwe" 724 "Spain" 728 "South Sudan" 729 "Sudan" 732 "Western Sahara" 740 "Suriname" 744 "Svalbard and Jan Mayen Islands" 748 "Eswatini" 752 "Sweden" 756 "Switzerland" 760 "Syria" 762 "Tajikistan" 764 "Thailand" 768 "Togo" 772 "Tokelau" 776 "Tonga" 780 "Trinidad and Tobago" 784 "United Arab Emirates" 788 "Tunisia" 792 "Turkey" 795 "Turkmenistan" 796 "Turks and Caicos Islands" 798 "Tuvalu" 800 "Uganda" 804 "Ukraine" 807 "North Macedonia" 818 "Egypt" 826 "United Kingdom" 831 "Guernsey" 
	label define __isolabels__ 832 "Jersey" 833 "Isle of Man" 834 "Tanzania" 840 "United States" 850 "US Virgin Islands" 854 "Burkina Faso" 858 "Uruguay" 860 "Uzbekistan" 862 "Venezuela" 876 "Wallis and Futuna Islands" 882 "Samoa" 887 "Yemen" 894 "Zambia", add
	lab val iso3n __isolabels__
}

*-------------------------------------------------------------------------------
* merge back to original dataset, sort & order, report
*-------------------------------------------------------------------------------

// finalize merged data
drop `varlist'
rename __ogcountry `varlist'
keep `varlist' `gen' `__keeploc'
tempfile cmerge 
save `cmerge', replace 

// merge to original data
restore
merge m:1 `varlist' using `cmerge', nogen keepusing(`gen' `__keeploc') keep(1 3)

// report on "probably aggregates" countries
if "`onewrong'"=="yes" {
	`lvlsof' `varlist' if __maybewrong==1, local(_sbwrong) clean separate(", ")
	noisily di  "-------"
	noisily dis "The observations listed hereafter partly contain references to ISO3166 countries. However, these are not encoded because they appear to address country aggregates rather than individual countries. Correct the input string ({it:`varlist'}) before executing {it:isocodes} if this is wrong: `_sbwrong'"  
	*drop if __maybewrong==1
	local possintrotext " (that were not covered above in the suspected country aggregates)"
}

// report on dropped countries
if "`keepiso3c'`keepiso2c'`keepiso3n'`keepregion'"!="" {
	count if __keeeep__!=1
	local __keeeep__count = r(N)
	if `__keeeep__count'>0 {
		`lvlsof' `varlist' if __keeeep__!=1, local(_asd) clean separate(", ")
		noisily di  "-------"
		noisily di "Countries dropped by the keep options: `_asd'"
		count if __keeeep__!=1
		drop if __keeeep__!=1
	}
}

// sort & order
order `varlist' `gen'
if "`nosort'"=="" {
	foreach __time__ in qyear year quarter month day {
		capture confirm variable `__time__'
		if !_rc local isocodesyear `isocodesyear' `__time__'
	}
	
	`sort' `varlist' `isocodesyear'
}

// last report
gen __nonm = 0
foreach ind in `gen' {
	if "`onewrong'"=="yes" {
		local addthos & __maybewrong!=1
		*local addthos2 & __maybewrong!=1
	}
	replace __nonm=1 if !mi(`ind') `addthos'
}
count if __nonm==0
if r(N)>0 {
	`lvlsof' `varlist' if __nonm==0 `addthos', local(_remmiss) clean separate(", ")
	noisily dis "------"
	noisily dis "The following observations of the input variable {it:`varlist'} couldn't be matched: `_remmiss'"
	noisily di "Remember that isocodes only covers countries in ISO3166."	
}
cap drop __nonm
cap drop __keeeep__
cap drop __maybewrong



}
end




















