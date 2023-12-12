{smcl}
{title:help isocodes}

{p2colset 5 18 20 2}{...}
{p2col :{cmd:isocodes}}Match English country codes and names{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{p 8 15 2} {cmd:isocodes}
{it:cvar}
{cmd:,} gen() [options] {p_end}

{p 8 15 2}where {it:cvar} is a string variable containing country names/codes and gen() defines the outputted variables

{marker opt_summary}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}

{syntab:Output variables}
{synopt :{opt gen(str)}}Choose the output variable(s). Can contain {it:iso3n}, {it:iso2c}, and/or {it:iso3c} for short ISO 3166 codes; and/or {it:cntryname} for full English country names.{p_end}

{syntab:Sample restriction}
{synopt :{opt keepr:egion(str)}}Restrict sample to the specified country group. May contain {it:oecd}, {it:eu}, and/or {it:emu}.{p_end}
{synopt :{opt keepiso3n(num)}}Restrict sample to countries specified by a list of ISO 3166 numeric codes.{p_end}
{synopt :{opt keepiso2c(str)}}Restrict sample to countries specified by a list of ISO 3166 alpha-2 codes.{p_end}
{synopt :{opt keepiso3c(str)}}Restrict sample to countries specified by a list of ISO 3166 alpha-3 codes.{p_end}

{syntab:Other}
{synopt :{opt nol:abel}}Do not label ISO 3166 numeric codes, i.e. the variable created by gen(iso3n).{p_end}
{synopt :{opt slow}}Use Stata native commands instead of Gtools.{p_end}

