*==============================================================================
* classificatie_objecten.do
*
* Reproduceert in Stata de objectclassificatie uit de R-pipeline: projecttype,
* uitkomst en de groepsindeling uit issue #28. Aan het eind wordt de
* beslismatrix uit het rapport opnieuw opgebouwd.
*
* De matrix is geen lijst om over te typen: hij is het RESULTAAT van elf
* geordende regels. Wie de regels overneemt, krijgt alle 31 combinaties er
* gratis bij, ook de combinaties die in een volgend extract nieuw opduiken.
*
* Bij: R/02_classify.R in jipclaassens/Stikstof
* Draait op: afstand_n2000_vergunningen.dta en afstand_n2000_nieuwbouw.dta
*==============================================================================

version 17
clear all
set more off

cd "C:\Users\u657113\OneDrive - Radboud Universiteit\projects\planrealisatie\"
local AF ".\analyses\analysis_stikstof\analysis_files"

*------------------------------------------------------------------------------
* 0. Vlag voor CBS-nieuwbouw op OBJECTniveau
*------------------------------------------------------------------------------
* Niet isnieuwbouw gebruiken: dat is een pandkenmerk uit Pand_VergunningDuur.
* Wat we nodig hebben is: staat dit verblijfsobject in het CBS-nieuwbouwbestand?

use "`AF'\afstand_n2000_nieuwbouw.dta", clear
keep vbo_bag_nr
duplicates drop
gen byte cbs_nieuwbouw = 1
tempfile nieuwbouwvlag
save `nieuwbouwvlag'

*------------------------------------------------------------------------------
* 1. Basisbestand: een rij per verblijfsobject met een bouwvergunning
*------------------------------------------------------------------------------
use "`AF'\afstand_n2000_vergunningen.dta", clear

merge 1:1 vbo_bag_nr using `nieuwbouwvlag', keep(master match) nogenerate
replace cbs_nieuwbouw = 0 if missing(cbs_nieuwbouw)

* LET OP. Dit bestand mist de circa 43.000 objecten die WEL in het
* CBS-nieuwbouwbestand staan maar geen vergunningrecord hebben (24,5% van de
* nieuwbouw in 2012, ~1,5% vanaf 2015). Die horen er voor tellingen bij; zie
* hoofdstuk 2 van het rapport. Wil je ze meenemen, append ze dan hier met
* append using "`AF'\afstand_n2000_nieuwbouw.dta", keep(vbo_bag_nr pand_bag_nr ///
*     dist2n2000_m gemeentecode provinciecode mutatiemaand)
* en zet cbs_nieuwbouw = 1 voor de toegevoegde rijen. De regels hieronder
* werken dan gewoon door; die objecten hebben geen last_vbo_status en belanden
* via regel (a) meteen in gerealiseerde nieuwbouw.

*------------------------------------------------------------------------------
* 2. Hulpvariabelen: de statusgroepen uit de BAG
*------------------------------------------------------------------------------
* Vier verzamelingen, elk één keer gedefinieerd. Voorkomt dat dezelfde lijst
* statussen op vijf plekken in de code wordt overgetypt.

gen byte vbo_in_voorraad  = inlist(last_vbo_status, "Verblijfsobject_in_gebruik",  ///
                                   "Verblijfsobject_in_gebruik_niet_ingemeten",    ///
                                   "Verblijfsobject_buiten_gebruik",               ///
                                   "Verbouwing_verblijfsobject")

gen byte pand_in_voorraad = inlist(last_pand_status, "Pand_in_gebruik",            ///
                                   "Pand_in_gebruik_niet_ingemeten",               ///
                                   "Pand_buiten_gebruik")

gen byte pand_pipeline    = inlist(last_pand_status, "Bouwvergunning_verleend",    ///
                                   "Bouw_gestart")

gen byte pand_gesloopt    = inlist(last_pand_status, "Pand_gesloopt",              ///
                                   "Sloopvergunning_verleend")

* Kerndiscriminator nieuwbouw versus renovatie. Negatief = het gebouw stond er al.
gen int bouwjaar_delta = pand_bouwjaar - vergunningjaar

*------------------------------------------------------------------------------
* 3. AS 1: projecttype, alleen op informatie die bij de vergunning al vaststond
*------------------------------------------------------------------------------
* Mag NIET afhangen van of er is opgeleverd: filteren op de uitkomst is hier
* niet onschuldig, want niet-realisatie is juist het veronderstelde effect.

gen byte projecttype = .

* (1) administratieve correctie: het object heeft nooit bestaan
replace projecttype = 1 if missing(projecttype) &                                 ///
    (last_vbo_status == "Verblijfsobject_ten_onrechte_opgevoerd" |                ///
     last_pand_status == "Pand_ten_onrechte_opgevoerd")

* (2) verbouwing: het gebouw stond er al bij de vergunning
*     !missing() is essentieel. In Stata is een ontbrekende waarde GROTER dan
*     elk getal, dus zonder die guard telt een leeg bouwjaar mee als delta > -6.
replace projecttype = 2 if missing(projecttype) &                                 ///
    (last_pand_status == "Verbouwing_pand" |                                      ///
     (!missing(bouwjaar_delta) & bouwjaar_delta <= -6))

* (3) de rest is een nieuwbouwvergunning
replace projecttype = 3 if missing(projecttype)

label define projecttype_lbl 1 "administratieve fout" 2 "verbouwing" 3 "nieuwbouw"
label values projecttype projecttype_lbl

*------------------------------------------------------------------------------
* 4. AS 2: uitkomst, wat er met het OBJECT is gebeurd
*------------------------------------------------------------------------------
* Realisatie wordt bepaald op last_vbo_status, niet op de pandstatus: de
* CBS-voorraaddefinitie werkt op objectniveau, en de twee reeksen spreken
* elkaar in ruim vijfduizend gevallen tegen.
*
* En let op: oplevering_eerste_datum is een PANDkenmerk. Van de 24.122 panden
* met meerdere objecten heeft er nul meer dan één opleverdatum. Hem als bewijs
* voor een specifiek object gebruiken zet 19.014 objecten ten onrechte op
* gerealiseerd. Hij wordt hieronder alleen gebruikt om te beslissen of een
* GEBOUW ooit af is gekomen, en dat is wel een pandvraag.

gen byte uitkomst        = .
gen byte uitkomst_ambigu = 0

* (a) staat in de CBS-nieuwbouwreeks: het hardste realisatiebewijs dat er is.
*     Gaat voor op alles, ook als het pand later is gesloopt of het object is
*     ingetrokken; dat zijn gebeurtenissen NA realisatie en worden vlaggen.
replace uitkomst = 1 if missing(uitkomst) & cbs_nieuwbouw == 1

* (b) object staat zelf in de voorraad, maar telt niet als nieuwbouw
replace uitkomst = 2 if missing(uitkomst) & vbo_in_voorraad == 1

* (c) object is expliciet niet gerealiseerd
replace uitkomst = 3 if missing(uitkomst) &                                       ///
    inlist(last_vbo_status, "Niet_gerealiseerd_verblijfsobject",                  ///
                            "Verblijfsobject_ten_onrechte_opgevoerd")

* (d) object bestaat administratief maar is nog niet in gebruik: de pijplijn
replace uitkomst = 4 if missing(uitkomst) & last_vbo_status == "Verblijfsobject_gevormd"

* (e) "ingetrokken" is dubbelzinnig: het dekt zowel "vergunning ingetrokken,
*     nooit gebouwd" als "bestond en is opgegaan in een ander object". Hier
*     beslist de pandstatus wel, want de vraag is nu wat er met het GEBOUW is
*     gebeurd. Alles wat via deze regel wordt toegewezen krijgt een vlag.
gen byte ing = missing(uitkomst) & last_vbo_status == "Verblijfsobject_ingetrokken"

replace uitkomst = 2 if ing & pand_in_voorraad == 1                               // gebouw staat er
replace uitkomst = 3 if ing & (pand_pipeline == 1 |                               ///
                               last_pand_status == "Niet_gerealiseerd_pand")      // nooit gebouwd
replace uitkomst = 2 if ing & pand_gesloopt == 1 & !missing(oplevering_eerste_datum)
replace uitkomst = 3 if ing & pand_gesloopt == 1 &  missing(oplevering_eerste_datum)
replace uitkomst_ambigu = 1 if ing & !missing(uitkomst)
drop ing

* (f) rest
replace uitkomst = 5 if missing(uitkomst)

label define uitkomst_lbl 1 "gerealiseerd, nieuwbouw" 2 "gerealiseerd, overig"    ///
    3 "uitval" 4 "pijplijn" 5 "onbepaald"
label values uitkomst uitkomst_lbl

*------------------------------------------------------------------------------
* 5. De vier groepen uit issue #28
*------------------------------------------------------------------------------
gen byte uitkomstgroep = 4
replace uitkomstgroep = 1 if uitkomst == 1
replace uitkomstgroep = 2 if uitkomst == 3
replace uitkomstgroep = 3 if uitkomst == 4

label define uitkomstgroep_lbl 1 "1. echte nieuwbouw" 2 "2. gestrand"                  ///
    3 "3. pijplijn" 4 "4. overig"
label values uitkomstgroep uitkomstgroep_lbl

*------------------------------------------------------------------------------
* 5b. Zes groepen: het complete plaatje
*------------------------------------------------------------------------------
* uitkomstgroep beschrijft alleen de nieuwbouwvergunningen. Verbouwing en
* administratieve ruis vallen buiten de analyses, maar zonder die twee telt
* het overzicht niet op tot alle woonobjecten. Eerst splitsen op projecttype
* (as 1, ex ante), dan binnen de nieuwbouw op uitkomst (as 2).

gen byte objectgroep = uitkomstgroep
replace objectgroep = 5 if projecttype == 2      // verbouwing
replace objectgroep = 6 if projecttype == 1      // administratieve ruis

label define objectgroep_lbl 1 "1. echte nieuwbouw" 2 "2. gestrand"                   ///
    3 "3. pijplijn" 4 "4. overig" 5 "5. verbouwing" 6 "6. administratieve ruis"
label values objectgroep objectgroep_lbl

*------------------------------------------------------------------------------
* 6. Vlaggen: informatie die niet in een van beide assen past
*------------------------------------------------------------------------------
gen byte later_gesloopt = pand_gesloopt

gen byte pand_vbo_inconsistent =                                                  ///
    (last_pand_status == "Niet_gerealiseerd_pand" & vbo_in_voorraad == 1) |       ///
    (pand_in_voorraad == 1 & last_vbo_status == "Niet_gerealiseerd_verblijfsobject")

* grijze band -1 tot -5 in de bouwjaar-delta: te weinig voor renovatie,
* te veel voor een normale nieuwbouwvolgorde
gen byte vlag_delta_grijs = !missing(bouwjaar_delta) &                            ///
    bouwjaar_delta < 0 & bouwjaar_delta > -6

*------------------------------------------------------------------------------
* 7. Ex-ante steekproef
*------------------------------------------------------------------------------
* Filteren op AS 1, meten op AS 2. Nooit selecteren op uitkomst.

gen byte in_sample = projecttype == 3 &                                           ///
    last_vbo_gebruiksdoel == "woon" &                                             ///
    !missing(dist2n2000_m) & !missing(provinciecode)

* Universum voor het overzicht: alle woonobjecten met een bruikbare locatie,
* dus ook verbouwingen en administratieve ruis. Ruimer dan in_sample.
gen byte in_universum = last_vbo_gebruiksdoel == "woon" &                         ///
    !missing(dist2n2000_m) & !missing(provinciecode)

*------------------------------------------------------------------------------
* 8. Controle: het complete overzicht in zes groepen
*------------------------------------------------------------------------------
* Dit is de tabel "Alle woonobjecten in zes groepen" uit het rapport. De eerste
* vier groepen samen zijn in_sample;
* groep 5 en 6 vallen buiten de analyses maar staan erbij zodat het optelt.

tab objectgroep if in_universum

* en de vier groepen binnen de nieuwbouwvergunningen (de beslismatrix telt hierop op)
tab uitkomstgroep if in_sample

* welke groepen zitten in welke analyse?
tab objectgroep if in_universum & in_sample     // vergunningenanalyse: groep 1 tot 4
tab objectgroep if in_universum & cbs_nieuwbouw == 1 & projecttype != 1   // nieuwbouwanalyse

*------------------------------------------------------------------------------
* 9. De beslismatrix uit het rapport
*------------------------------------------------------------------------------
* Elke combinatie van pandstatus, objectstatus en CBS-nieuwbouw met minstens
* 200 objecten, gegroepeerd per uitkomst. De regel "rest" per groep vangt de
* kleine cellen, zodat de matrix optelt tot de totalen uit stap 8.

preserve
keep if in_sample

collapse (count) n = vbo_bag_nr (max) ambigu = uitkomst_ambigu                    ///
                                (max) gesloopt = later_gesloopt,                  ///
    by(uitkomstgroep cbs_nieuwbouw last_pand_status last_vbo_status)

* kleine cellen samenvatten in één regel per groep
gen byte klein = n < 200
gen str40 pandstatus = cond(klein, "overige combinaties (< 200)", last_pand_status)
gen str40 objectstatus = cond(klein, "", last_vbo_status)
replace cbs_nieuwbouw = . if klein
collapse (sum) n (max) ambigu gesloopt, by(uitkomstgroep klein cbs_nieuwbouw pandstatus objectstatus)

gsort uitkomstgroep klein -n
list uitkomstgroep cbs_nieuwbouw pandstatus objectstatus n ambigu gesloopt,             ///
    sepby(uitkomstgroep) noobs abbreviate(14)

* subtotalen ter controle: deze moeten gelijk zijn aan stap 8
tabstat n, by(uitkomstgroep) stats(sum) format(%12.0fc)
restore

*------------------------------------------------------------------------------
* 10. Wegschrijven
*------------------------------------------------------------------------------
compress
save "`AF'\objecten_geclassificeerd.dta", replace

*==============================================================================
* Waarom deze volgorde
*==============================================================================
* De regels zijn geordend en de eerste die past wint. Dat is wat de matrix
* genereert. Drie principes zitten erin:
*
*  1. Sloop is geen realisatiestatus. Een object dat CBS als nieuwbouw telde en
*     waarvan het pand later is gesloopt, blijft gerealiseerd (regel a vóór de
*     pandregels), met de vlag later_gesloopt.
*  2. De objectstatus wint van de pandstatus voor de vraag of dít object er is
*     gekomen. Daarmee gaan 2.209 objecten van "gerealiseerd" naar "gestrand"
*     en 2.294 naar "pijplijn".
*  3. "Ingetrokken" is dubbelzinnig en wordt op de pandstatus beslist, met een
*     vlag zodat je het in een gevoeligheidsanalyse kunt omdraaien:
*     tab uitkomstgroep if in_sample & uitkomst_ambigu == 0
*
* En let op de twee assen. objectgroep mengt ze bewust voor het overzicht: groep 5
* en 6 komen van AS 1 (wat voor project was dit, ex ante), groep 1 tot 4 van
* AS 2 (wat is ermee gebeurd, ex post). Filteren doe je op AS 1, meten op
* AS 2. Wie op groep 1 filtert en dat een steekproef noemt, selecteert op de
* uitkomst, en dat is precies wat je hier niet moet doen.
*==============================================================================
