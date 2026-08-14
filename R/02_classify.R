## =====================================================================
## 02_classify.R -- typering en steekproefselectie op VBO-niveau
##
## Input : _derived/vbo_base.rds   (uit 01_build_vbo.R)
## Output: _derived/vbo_analyse.rds
##
## Hoort bij issue #28: "Hoe gaan we de uiteindelijke data filteren, wat
## is ruis en wat is relevant voor onze analyse?"
##
## ---------------------------------------------------------------------
## Het probleem met een enkele `status`-variabele
## ---------------------------------------------------------------------
## De Stata-versie perst alles in een variabele `status` met zeven
## niveaus, en selecteert daar vervolgens op (`keep if inlist(status,2,4,7)`).
## Dat mengt drie dingen die uit elkaar moeten:
##
##   1. WAT VOOR PROJECT was dit?      -- bekend bij vergunningverlening,
##                                        niet beinvloed door stikstof
##   2. WAT IS ERMEE GEBEURD?          -- de uitkomst die we willen meten
##   3. WAT WETEN WE NOG NIET?         -- censurering
##
## Selecteren op (2) is selecteren op de uitkomst. Dat is hier niet
## onschuldig, want niet-realisatie is juist het veronderstelde effect
## van stikstof. Het huidige filter is bovendien intern asymmetrisch:
## status 6 (gerealiseerd, geen nieuwbouw) valt af, maar status 7 (nog
## niet opgeleverd) blijft -- terwijl daar de toekomstige 6'en in zitten.
## Naarmate een vergunningcohort ouder wordt lekt 7 naar 6 en valt weg.
## Dat is per constructie een jaarafhankelijke uitval.
##
## Daarom drie aparte assen:
##
##   projecttype   ex ante: nieuwbouw / verbouwing / admin_fout
##   uitkomst      ex post: gerealiseerd_nieuwbouw / gerealiseerd_overig /
##                          uitval / pipeline / onbepaald
##   vlaggen       later_gesloopt, pand_vbo_inconsistent, ...
##
## Filteren doe je op `projecttype`. Meten doe je op `uitkomst`.
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
## Mag NIET afhangen van of er uiteindelijk is opgeleverd. Gebruikte
## bronnen en waarom die ex ante zijn:
##
##  - "ten onrechte opgevoerd": administratieve correctie, het object
##    heeft nooit bestaan. Geen uitkomst maar een registratiefout.
##  - pand_bouwjaar - vergunningjaar <= -6: het gebouw stond er al, dus
##    de vergunning ging over een verbouwing. oorspronkelijkBouwjaar is
##    een verplicht pandattribuut dat al BIJ vergunningverlening wordt
##    ingevuld (zie docs/BAG-dataschoning-inzichten.md sec. 5), dus dit
##    is bekend op t=0.
##  - last_pand_status == "Verbouwing_pand": status sinds BAG 2.0,
##    bedoeld voor reeds opgeleverde objecten.
##
## `duur_negatief` (oplevering voor vergunning) gebruikt WEL de
## opleverdatum en is dus strikt genomen post-treatment. Die zit hier
## bewust niet in het type, maar als losse vlag. Het gaat om weinig
## gevallen; 03_checks.R rapporteert hoeveel.

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
## 2. As 2: uitkomst -- wat is er met het object gebeurd
## ---------------------------------------------------------------------
## Realisatie wordt bepaald op `vbo_status`, niet op de pandstatus.
## Een pand kan meerdere VBO's hebben en de pandstatus is de laatste
## stand van het hele pand; de CBS-voorraaddefinitie werkt op VBO-niveau
## (docs/BAG-dataschoning-inzichten.md sec. 4). Dat lost meteen de
## inconsistentie op waar dit onderzoek mee begon: 2.691 VBO's met
## pandstatus "Niet_gerealiseerd_pand" staan zelf op "in gebruik".

d[, vbo_in_voorraad := vbo_status %in% VBO_IN_VOORRAAD]
d[, heeft_oplevering := !is.na(oplevering_eerste_datum)]

d[, uitkomst := NA_character_]

## (a) Gerealiseerd en door CBS als nieuwbouw geteld. Dit is de harde
##     realisatie: het object staat in de CBS-nieuwbouwreeks met een
##     mutatiemaand. Gaat voor op alles, ook als het pand later is
##     gesloopt -- gesloopt-na-oplevering is een aparte vlag, geen
##     ander soort uitkomst.
d[is.na(uitkomst) & cbs_nieuwbouw == TRUE, uitkomst := "gerealiseerd_nieuwbouw"]

## (b) Gerealiseerd, maar niet als nieuwbouw geteld: toevoegingen,
##     transformaties, splitsingen. Wel in de voorraad gekomen.
d[is.na(uitkomst) & (vbo_in_voorraad | heeft_oplevering),
  uitkomst := "gerealiseerd_overig"]

## (c) Uitval: vergund, uiteindelijk niet gerealiseerd. Strikt op
##     VBO-niveau en zonder opleverdatum, zodat objecten die alsnog in
##     de voorraad zitten hier niet in belanden.
d[is.na(uitkomst) &
    vbo_status %in% c("Niet_gerealiseerd_verblijfsobject",
                      "Verblijfsobject_ingetrokken") &
    !heeft_oplevering,
  uitkomst := "uitval"]

## (d) Pijplijn: vergund, object bestaat administratief, nog niet
##     opgeleverd en niet ingetrokken. Dit is issue #28's "veel in de
##     pijplijn, daar willen we ook iets mee".
d[is.na(uitkomst) &
    (vbo_status == "Verblijfsobject_gevormd" |
     last_pand_status %in% PAND_PIPELINE),
  uitkomst := "pipeline"]

d[is.na(uitkomst), uitkomst := "onbepaald"]

## ---------------------------------------------------------------------
## 3. Vlaggen -- informatie die niet in een van beide assen past
## ---------------------------------------------------------------------

## Pand gesloopt of sloopvergunning: orthogonaal aan realisatie.
d[, later_gesloopt := last_pand_status %in% c("Pand_gesloopt",
                                              "Sloopvergunning_verleend")]

## Pandstatus en VBO-status spreken elkaar tegen. Dit is de "ruis" die
## de aanleiding was; hij wordt gemeten in plaats van weggepoetst.
d[, pand_vbo_inconsistent :=
    (last_pand_status == "Niet_gerealiseerd_pand" & vbo_in_voorraad) |
    (last_pand_status %in% PAND_IN_VOORRAAD &
       vbo_status == "Niet_gerealiseerd_verblijfsobject")]

## Oplevering voor de eerste vergunning -- zie toelichting bij as 1
d[, vlag_duur_negatief := !is.na(duur_negatief) & duur_negatief]

## Grijze band -1..-5 in de bouwjaar-delta: te weinig om renovatie te
## zijn, te veel voor een normale nieuwbouwvolgorde. Apart taggen was
## een openstaand punt in docs/BAG-dataschoning-inzichten.md sec. 6.
d[, vlag_delta_grijs := !is.na(bouwjaar_delta) &
    bouwjaar_delta < 0 & bouwjaar_delta > PARAMS$reno_delta]

## ---------------------------------------------------------------------
## 4. Steekproefvlaggen
## ---------------------------------------------------------------------
## Ex-ante steekproef: alles wat als woningbouwvergunning is afgegeven,
## ongeacht de afloop. Dit is de universe voor elke analyse van
## vergunningen, uitval en duur.

d[, in_sample := projecttype == "nieuwbouw" &
                 vbo_gebruiksdoel == "woon" &
                 !is.na(dist2n2000_m) &
                 !is.na(provinciecode)]

## Voor analyses op vergunningjaar moet er ook een vergunningjaar zijn
d[, in_sample_verg := in_sample & !is.na(vergunning_jaar) &
    vergunning_jaar >= PARAMS$jaar_min & vergunning_jaar <= PARAMS$jaar_max]

## Voor analyses op realisatiejaar: het CBS-mutatiebestand is zelf de
## uitkomst, dus hier geldt alleen de administratieve schoning en de
## jaargrens. 2026 valt af (halve jaargang, mutaties t/m juli).
d[, in_sample_nwb := cbs_nieuwbouw & projecttype != "admin_fout" &
    !is.na(dist2n2000_m) & !is.na(provinciecode) &
    !is.na(mutatiejaar) &
    mutatiejaar >= PARAMS$jaar_min & mutatiejaar <= PARAMS$jaar_max]

## ---------------------------------------------------------------------
## 5. Censurering
## ---------------------------------------------------------------------
## Het extract is een momentopname. Een vergunning uit 2024 kan nog niet
## zijn opgeleverd, dus "niet opgeleverd" betekent iets anders voor een
## cohort uit 2015 dan voor een uit 2024. Zonder vast opvolgvenster meet
## een vergelijking van uitvalpercentages over vergunningjaren vooral de
## afstand tot de extractdatum.

EXTRACT_DATUM <- as.Date(EXTRACT, format = "%Y%m%d")

d[, opvolgtijd_dagen := as.integer(EXTRACT_DATUM - vergunning_datum)]
d[, opvolgtijd_jaar  := opvolgtijd_dagen / 365.25]

## Tijd tot oplevering (NA als niet opgeleverd)
d[, duur_tot_oplevering := as.integer(oplevering_eerste_datum - vergunning_datum)]

## Uitkomst binnen een vast venster van n jaar na vergunning; NA als het
## venster nog niet vol is. Hiermee zijn cohorten onderling vergelijkbaar.
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

kop("Projecttype x uitkomst (alle VBO's met gebruiksdoel woon)")
print(dcast(d[vbo_gebruiksdoel == "woon", .N, by = .(projecttype, uitkomst)],
            projecttype ~ uitkomst, value.var = "N", fill = 0))

kop("Steekproefomvang")
cat(sprintf("VBO's totaal                      : %s\n", nfmt(nrow(d))))
cat(sprintf("in_sample (ex ante, woon)         : %s\n", nfmt(sum(d$in_sample))))
cat(sprintf("in_sample_verg (%d-%d)         : %s\n", PARAMS$jaar_min,
            PARAMS$jaar_max, nfmt(sum(d$in_sample_verg))))
cat(sprintf("in_sample_nwb  (%d-%d)         : %s\n", PARAMS$jaar_min,
            PARAMS$jaar_max, nfmt(sum(d$in_sample_nwb))))

kop("Vlaggen binnen de ex-ante steekproef")
cat(sprintf("pand/VBO-status inconsistent      : %s\n",
            nfmt(sum(d$pand_vbo_inconsistent & d$in_sample))))
cat(sprintf("later gesloopt                    : %s\n",
            nfmt(sum(d$later_gesloopt & d$in_sample))))
cat(sprintf("oplevering voor vergunning        : %s\n",
            nfmt(sum(d$vlag_duur_negatief & d$in_sample))))
cat(sprintf("bouwjaar-delta in grijze band     : %s\n",
            nfmt(sum(d$vlag_delta_grijs & d$in_sample))))

write_derived(d, "vbo_analyse")
