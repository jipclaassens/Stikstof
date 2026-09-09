## =====================================================================
## 02_classify.R -- typering en steekproefselectie op VBO-niveau
##
## Input : _derived/vbo_base.rds   (uit 01_build_vbo.R)
## Output: _derived/vbo_analyse.rds
##
## Hoort bij issue #28: "We willen echte nieuwbouw en gestaakte (als er wel
## iets is vergund, maar uiteindelijk niet gerealiseerd) objecten meenemen.
## En daarnaast is er ook veel in de pijplijn, daar willen we ook iets mee."
##
## ---------------------------------------------------------------------
## Waarom drie assen in plaats van een enkele status
## ---------------------------------------------------------------------
## De Stata-versie perst alles in een variabele met zeven niveaus en
## selecteert daarop. Dat mengt drie dingen die uit elkaar moeten:
##
##   1. WAT VOOR PROJECT was dit?   -- bekend bij vergunningverlening,
##                                     niet beinvloed door stikstof
##   2. WAT IS ERMEE GEBEURD?       -- de uitkomst die we willen meten
##   3. WAT WETEN WE NOG NIET?      -- censurering
##
## Filteren doe je op as 1. Meten doe je op as 2. Selecteren op as 2 is
## selecteren op de uitkomst, en niet-realisatie is juist het
## veronderstelde effect van stikstof.
##
## ---------------------------------------------------------------------
## Welke bron is waarvoor gezaghebbend
## ---------------------------------------------------------------------
## De BAG kent twee statusreeksen die elkaar kunnen tegenspreken:
##
##   realisatie van een OBJECT   -> vbo_status  (CBS-voorraaddefinitie,
##                                  docs/BAG-dataschoning-inzichten.md sec. 4)
##   lot van het GEBOUW          -> last_pand_status
##   datums (vergunning, bouwstart, oplevering) -> PAND-niveau
##
## Die laatste regel is makkelijk mis te gaan. oplevering_eerste_datum
## komt uit Pand_VergunningDuur en is gekoppeld op pand_bag_nr: van de
## 24.122 panden met meerdere verblijfsobjecten heeft er nul meer dan een
## opleverdatum. Een opleverdatum is dus GEEN bewijs dat een specifiek
## object is gerealiseerd, alleen dat het pand is opgeleverd. Hem toch als
## objectbewijs gebruiken zette 19.014 objecten ten onrechte op
## "gerealiseerd", terwijl hun eigen status niet-gerealiseerd, gevormd,
## ingetrokken of ten onrechte opgevoerd was.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))

kop("02_classify.R")

d <- read_derived("vbo_base")

## ---------------------------------------------------------------------
## 1. As 1: projecttype -- alleen op ex-ante informatie
## ---------------------------------------------------------------------
## Mag NIET afhangen van of er uiteindelijk is opgeleverd.
##
##  - "ten onrechte opgevoerd": administratieve correctie, het object
##    heeft nooit bestaan. Registratiefout, geen uitkomst.
##  - pand_bouwjaar - vergunningjaar <= -6: het gebouw stond er al, dus
##    de vergunning ging over een verbouwing. oorspronkelijkBouwjaar is
##    een verplicht pandattribuut dat al BIJ vergunningverlening wordt
##    ingevuld (docs sec. 5), dus bekend op t=0.
##  - last_pand_status == "Verbouwing_pand": status sinds BAG 2.0,
##    bedoeld voor reeds opgeleverde objecten.
##
## duur_negatief (oplevering voor vergunning) gebruikt wel de
## opleverdatum en is dus post-treatment; die zit hier bewust niet in
## het type maar als losse vlag.

d[, projecttype := NA_character_]

d[is.na(projecttype) &
    (vbo_status == "Verblijfsobject_ten_onrechte_opgevoerd" |
     last_pand_status == "Pand_ten_onrechte_opgevoerd"),
  projecttype := "admin_fout"]

d[is.na(projecttype) &
    (last_pand_status == "Verbouwing_pand" |
     (!is.na(bouwjaar_delta) & bouwjaar_delta <= PARAMS$reno_delta)),
  projecttype := "verbouwing"]

d[is.na(projecttype), projecttype := "nieuwbouw"]

## ---------------------------------------------------------------------
## 2. As 2: uitkomst -- wat is er met het OBJECT gebeurd
## ---------------------------------------------------------------------

d[, vbo_in_voorraad := vbo_status %chin% VBO_IN_VOORRAAD]
d[, pand_opgeleverd := !is.na(oplevering_eerste_datum)]   # PAND-niveau
d[, later_gesloopt  := last_pand_status %chin% PAND_GESLOOPT]

d[, uitkomst := NA_character_]
d[, uitkomst_ambigu := FALSE]

## (a) Staat in de CBS-nieuwbouwreeks: met een mutatiemaand aan de
##     woningvoorraad toegevoegd. Het hardste realisatiebewijs dat er is,
##     en het gaat voor op alles -- ook als het pand later is gesloopt of
##     het object later is ingetrokken. Dat zijn gebeurtenissen NA
##     realisatie; die worden vlaggen, geen aparte uitkomst.
d[is.na(uitkomst) & cbs_nieuwbouw == TRUE, uitkomst := "gerealiseerd_nieuwbouw"]

## (b) Object staat zelf in de voorraad, maar telt niet als nieuwbouw:
##     toevoegingen, transformaties, splitsingen.
d[is.na(uitkomst) & vbo_in_voorraad == TRUE, uitkomst := "gerealiseerd_overig"]

## (c) Object is expliciet niet gerealiseerd.
d[is.na(uitkomst) &
    vbo_status %chin% c("Niet_gerealiseerd_verblijfsobject",
                        "Verblijfsobject_ten_onrechte_opgevoerd"),
  uitkomst := "uitval"]

## (d) Object bestaat administratief maar is nog niet in gebruik:
##     de pijplijn uit issue #28.
d[is.na(uitkomst) & vbo_status == "Verblijfsobject_gevormd",
  uitkomst := "pipeline"]

## (e) "Verblijfsobject_ingetrokken" is dubbelzinnig: het dekt zowel
##     "vergunning ingetrokken, nooit gebouwd" als "bestond en is
##     opgegaan in een ander object". Hier beslist de PANDSTATUS wel,
##     want de vraag is nu wat er met het gebouw is gebeurd -- precies
##     waar de pandreeks over gaat. Alles wat via deze regel wordt
##     toegewezen krijgt de vlag uitkomst_ambigu, zodat je er in een
##     gevoeligheidsanalyse op kunt terugvallen.
ing <- is.na(d$uitkomst) & d$vbo_status == "Verblijfsobject_ingetrokken"

##   pand staat in de voorraad -> gebouw is er, object is samengevoegd
d[ing & last_pand_status %chin% PAND_IN_VOORRAAD,
  c("uitkomst", "uitkomst_ambigu") := .("gerealiseerd_overig", TRUE)]

##   pand nog in de vergunningfase of niet gerealiseerd -> nooit gebouwd.
##   Mediaan vergunningjaar 2023-2024 voor de vergunde groep: te recent
##   om gebouwd en alweer ingetrokken te zijn.
d[ing & last_pand_status %chin% c(PAND_PIPELINE, "Niet_gerealiseerd_pand"),
  c("uitkomst", "uitkomst_ambigu") := .("uitval", TRUE)]

##   pand gesloopt -> hier is de pand-opleverdatum wel het juiste bewijs,
##   want de vraag is of het gebouw ooit af is gekomen
d[ing & later_gesloopt & pand_opgeleverd,
  c("uitkomst", "uitkomst_ambigu") := .("gerealiseerd_overig", TRUE)]
d[ing & later_gesloopt & !pand_opgeleverd,
  c("uitkomst", "uitkomst_ambigu") := .("uitval", TRUE)]

d[is.na(uitkomst), uitkomst := "onbepaald"]

## De drie categorieen uit issue #28, plus wat er buiten valt
d[, uitkomstgroep := fcase(
  uitkomst == "gerealiseerd_nieuwbouw", "1_nieuwbouw",
  uitkomst == "uitval",                 "2_gestrand",
  uitkomst == "pipeline",               "3_pijplijn",
  default =                             "4_overig")]

## Zes groepen voor het beschrijvende overzicht. Eerst splitsen op
## projecttype (as 1, ex ante), dan binnen de nieuwbouwvergunningen op
## uitkomst (as 2). Verbouwing en administratieve ruis vallen buiten de
## analyses, maar horen wel in het overzicht: zonder die twee telt het
## plaatje niet op tot alle woonobjecten.
d[, objectgroep := fcase(
  projecttype == "admin_fout", "6_admin_ruis",
  projecttype == "verbouwing", "5_verbouwing",
  uitkomstgroep == "1_nieuwbouw",    "1_nieuwbouw",
  uitkomstgroep == "2_gestrand",     "2_gestrand",
  uitkomstgroep == "3_pijplijn",     "3_pijplijn",
  default =                    "4_overig")]

## ---------------------------------------------------------------------
## 3. Vlaggen -- informatie die niet in een van beide assen past
## ---------------------------------------------------------------------

## Pandstatus en objectstatus spreken elkaar tegen. Wordt gemeten in
## plaats van weggepoetst; dit was de aanleiding voor de hele exercitie.
d[, pand_vbo_inconsistent :=
    (last_pand_status == "Niet_gerealiseerd_pand" & vbo_in_voorraad) |
    (last_pand_status %chin% PAND_IN_VOORRAAD &
       vbo_status == "Niet_gerealiseerd_verblijfsobject")]

## Oplevering voor de eerste vergunning -- zie toelichting bij as 1
d[, vlag_duur_negatief := !is.na(duur_negatief) & duur_negatief]

## Grijze band -1..-5 in de bouwjaar-delta: te weinig voor renovatie, te
## veel voor een normale nieuwbouwvolgorde. Openstaand punt in docs sec. 6.
d[, vlag_delta_grijs := !is.na(bouwjaar_delta) &
    bouwjaar_delta < 0 & bouwjaar_delta > PARAMS$reno_delta]

## ---------------------------------------------------------------------
## 4. Steekproefvlaggen
## ---------------------------------------------------------------------
## Ex-ante steekproef: alles wat als woningbouwvergunning is afgegeven,
## ongeacht de afloop. Universe voor vergunningen, uitval en duur.

d[, in_sample := projecttype == "nieuwbouw" &
                 vbo_gebruiksdoel == "woon" &
                 !is.na(dist2n2000_m) &
                 !is.na(provinciecode)]

## Universum voor het beschrijvende overzicht: alle woonobjecten met een
## bruikbare locatie, dus ook verbouwingen en administratieve ruis. Ruimer
## dan in_sample, dat alleen de nieuwbouwvergunningen bevat.
d[, in_universum := vbo_gebruiksdoel == "woon" &
                    !is.na(dist2n2000_m) &
                    !is.na(provinciecode)]

d[, in_sample_verg := in_sample & !is.na(vergunning_jaar) &
    vergunning_jaar >= PARAMS$jaar_min & vergunning_jaar <= PARAMS$jaar_max]

## Voor analyses op realisatiejaar is het CBS-mutatiebestand zelf de
## uitkomst: alleen administratieve schoning en de jaargrens. 2026 valt
## af (halve jaargang, mutaties t/m juli).
d[, in_sample_nwb := cbs_nieuwbouw & projecttype != "admin_fout" &
    !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiejaar) &
    mutatiejaar >= PARAMS$jaar_min & mutatiejaar <= PARAMS$jaar_max]

## ---------------------------------------------------------------------
## 5. Censurering
## ---------------------------------------------------------------------
## Het extract is een momentopname. "Niet opgeleverd" betekent iets
## anders voor een cohort uit 2015 dan voor een uit 2024, en het verschil
## loopt niet gelijk op tussen nabij en ver (zie 03_checks.R CHECK 3).

EXTRACT_DATUM <- as.Date(EXTRACT, format = "%Y%m%d")

d[, opvolgtijd_dagen    := as.integer(EXTRACT_DATUM - vergunning_datum)]
d[, opvolgtijd_jaar     := opvolgtijd_dagen / 365.25]
d[, duur_tot_oplevering := as.integer(oplevering_eerste_datum - vergunning_datum)]

## Uitkomst binnen een vast venster van n jaar na vergunning; NA zolang
## het venster niet vol is, zodat cohorten vergelijkbaar blijven.
uitkomst_binnen <- function(dt, jaren) {
  grens <- jaren * 365.25
  gerealiseerd <- !is.na(dt$duur_tot_oplevering) & dt$duur_tot_oplevering <= grens
  vol <- dt$opvolgtijd_dagen >= grens
  fifelse(!vol & !gerealiseerd, NA, gerealiseerd)
}
for (j in c(3, 4, 5)) {
  d[, paste0("gerealiseerd_binnen_", j, "jr") := uitkomst_binnen(.SD, j)]
}

## ---------------------------------------------------------------------
## 6. Overzicht
## ---------------------------------------------------------------------

kop("Projecttype x uitkomst (objecten met gebruiksdoel woon)")
print(dcast(d[vbo_gebruiksdoel == "woon", .N, by = .(projecttype, uitkomst)],
            projecttype ~ uitkomst, value.var = "N", fill = 0))

kop("De drie categorieen uit issue #28, binnen de ex-ante steekproef")
s <- d[in_sample == TRUE]
print(s[, .(N = .N, pct = round(100 * .N / nrow(s), 2)), by = uitkomstgroep][order(uitkomstgroep)])
cat(sprintf("\ntoegewezen via de ambigue ingetrokken-regel: %s (%.2f%%)\n",
            nfmt(sum(s$uitkomst_ambigu)), 100 * mean(s$uitkomst_ambigu)))

kop("Steekproefomvang")
cat(sprintf("objecten totaal                   : %s\n", nfmt(nrow(d))))
cat(sprintf("in_sample (ex ante, woon)         : %s\n", nfmt(sum(d$in_sample))))
cat(sprintf("in_sample_verg (%d-%d)         : %s\n", PARAMS$jaar_min,
            PARAMS$jaar_max, nfmt(sum(d$in_sample_verg))))
cat(sprintf("in_sample_nwb  (%d-%d)         : %s\n", PARAMS$jaar_min,
            PARAMS$jaar_max, nfmt(sum(d$in_sample_nwb))))

kop("Vlaggen binnen de ex-ante steekproef")
for (v in c("pand_vbo_inconsistent", "later_gesloopt", "uitkomst_ambigu",
            "vlag_duur_negatief", "vlag_delta_grijs")) {
  cat(sprintf("  %-24s %s\n", v, nfmt(sum(d[[v]] & d$in_sample))))
}

write_derived(d, "vbo_analyse")
