## =====================================================================
## 01_build_vbo.R -- drie GeoDMS-exports -> een bestand op VBO-niveau
##
## Input  (uit cfg/main/Analyse/N2000_Analyse.dms):
##   Object_BouwvergunningVerleend_<extract>.csv  1 rij per VBO met vergunning
##   Object_Nieuwbouw_<extract>.csv               1 rij per VBO dat CBS als
##                                                nieuwbouw telt (met mutatiemaand)
##   Pand_VergunningDuur_<extract>.csv            1 rij per PAND met de tijdlijn
##
## Output: _derived/vbo_base.rds -- 1 rij per VBO
##
## Ontwerpkeuzes, expliciet omdat ze in de Stata-versie impliciet waren:
##
##  (a) De ruggengraat is de VERENIGING van de VBO's uit het vergunningen-
##      en het nieuwbouwbestand, niet alleen het vergunningenbestand.
##      43.000 nieuwbouw-VBO's hebben geen vergunningrecord (24,5% in 2012,
##      ~1,5% vanaf 2015). Die stilzwijgend laten wegvallen bij een
##      merge maakt de koppelgraad tot een verborgen, jaarafhankelijk
##      selectiecriterium. Ze blijven staan, met vlag `bron_vergunning`.
##
##  (b) Project-id = gemeente + vergunningdocumentnummer, niet
##      vbo_documentnummer_rel. Reden: het documentnummer staat in BEIDE
##      objectbestanden, dus ook de niet-gekoppelde VBO's krijgen een
##      echt project-id. En _rel is te grof: 1.308 _rel-waarden lopen over
##      meerdere gemeenten heen (documentnummers als "RB 040/07" worden
##      hergebruikt). Andersom splitst de nieuwe sleutel geen enkel
##      bestaand _rel-project (geverifieerd in 03_checks.R).
##
##  (c) Pandkenmerken komen uitsluitend uit Pand_VergunningDuur.
##      `pand_status` in het nieuwbouwbestand is een SNAPSHOT op de
##      mutatiemaand (wijkt in 697.094 van de 884.382 rijen af van de
##      eindstatus) en is dus iets anders dan `last_pand_status`.
##      Beide worden bewaard, met verschillende namen.
## =====================================================================

## Werkt zowel via Rscript als via source() in RStudio
.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))

kop("01_build_vbo.R -- extract %s", EXTRACT)

stopifnot(all(file.exists(unlist(FILES))))

## ---------------------------------------------------------------------
## 1. Inlezen
## ---------------------------------------------------------------------

message("Inlezen Object_BouwvergunningVerleend ...")
bvv <- fread(FILES$vergunning)
setnames(bvv, tolower(names(bvv)))

message("Inlezen Object_Nieuwbouw ...")
nwb <- fread(FILES$nieuwbouw)
setnames(nwb, tolower(names(nwb)))

message("Inlezen Pand_VergunningDuur ...")
pnd <- fread(FILES$pand_duur)
setnames(pnd, tolower(names(pnd)))

## Sleutels moeten uniek zijn -- zo niet, dan is het extract kapot
stopifnot(!anyDuplicated(bvv$vbo_bag_nr))
stopifnot(!anyDuplicated(nwb$vbo_bag_nr))
stopifnot(!anyDuplicated(pnd$pand_bag_nr))

## ---------------------------------------------------------------------
## 2. Provinciecode opschonen (staat als "PV20" / "null" in de export)
## ---------------------------------------------------------------------

schoon_prv <- function(dt) {
  dt[, prv_code := sub("^PV", "", prv_code)]
  dt[prv_code %in% c("null", ""), prv_code := NA_character_]
  dt[, provinciecode := suppressWarnings(as.integer(prv_code))]
  dt[, prv_naam := gsub("_", "-", gsub("'", "", prv_naam))]
  dt[prv_naam %in% c("null", ""), prv_naam := NA_character_]
  setnames(dt, "prv_naam", "provincienaam")
  dt[, prv_code := NULL]
  invisible(dt)
}
invisible(lapply(list(bvv, nwb, pnd), schoon_prv))

## ---------------------------------------------------------------------
## 3. Vergunningenbestand: VBO-kenmerken
## ---------------------------------------------------------------------

setnames(bvv, "last_pand_bouwvergunningberleend_datum", "bv_pand_datum")  # typo in de export
bvv[, bv_pand_datum := as_datum(bv_pand_datum)]

verg <- bvv[, .(
  vbo_bag_nr,
  pand_bag_nr,
  x, y,
  dist2n2000_m,
  gemeentecode          = gm_code,
  provinciecode, provincienaam,
  vbo_gebruiksdoel      = last_vbo_gebruiksdoel,
  vbo_status            = last_vbo_status,
  documentnummer        = vbo_documentnummer,
  documentnummer_rel    = vbo_documentnummer_rel,   # oude project-id, alleen ter controle
  cluster_rel,
  bv_pand_datum,
  bron_vergunning       = TRUE
)]

## ---------------------------------------------------------------------
## 4. Nieuwbouwbestand: realisatiemoment volgens CBS
## ---------------------------------------------------------------------

nwb[, mutatiemaand_d := as_maand(mutatiemaand)]

nieuw <- nwb[, .(
  vbo_bag_nr,
  pand_bag_nr,
  x, y,
  dist2n2000_m,
  gemeentecode          = gm_code,
  provinciecode, provincienaam,
  documentnummer        = vbo_documentnummer,
  mutatiemaand          = mutatiemaand_d,
  mutatiejaar           = as.integer(substr(mutatiemaand, 1, 4)),
  vbo_oppervlakte,
  pand_status_mutatie   = pand_status,      # snapshot, NIET de eindstatus
  cbs_nieuwbouw         = TRUE
)]

## ---------------------------------------------------------------------
## 5. Ruggengraat: vereniging van beide objectbestanden
## ---------------------------------------------------------------------

alleen_nieuw <- nieuw[!vbo_bag_nr %in% verg$vbo_bag_nr]
message(sprintf("VBO's alleen in nieuwbouwbestand (geen vergunningrecord): %s",
                nfmt(nrow(alleen_nieuw))))

vbo <- rbindlist(list(
  verg,
  alleen_nieuw[, .(vbo_bag_nr, pand_bag_nr, x, y, dist2n2000_m, gemeentecode,
                   provinciecode, provincienaam, documentnummer,
                   bron_vergunning = FALSE)]
), fill = TRUE)
vbo[is.na(bron_vergunning), bron_vergunning := FALSE]

## nieuwbouwkenmerken erbij
vbo <- merge(vbo,
             nieuw[, .(vbo_bag_nr, mutatiemaand, mutatiejaar, vbo_oppervlakte,
                       pand_status_mutatie, cbs_nieuwbouw)],
             by = "vbo_bag_nr", all.x = TRUE)
vbo[is.na(cbs_nieuwbouw), cbs_nieuwbouw := FALSE]

## Object_Nieuwbouw is de CBS-nieuwbouwreeks voor WONINGEN: 99,9% van de
## gekoppelde VBO's heeft gebruiksdoel "woon". Voor de VBO's zonder
## vergunningrecord is het gebruiksdoel onbekend; die krijgen "woon" met
## een vlag, zodat de keuze zichtbaar blijft in plaats van weg te vallen.
vbo[, gebruiksdoel_afgeleid := is.na(vbo_gebruiksdoel) & cbs_nieuwbouw]
vbo[gebruiksdoel_afgeleid == TRUE, vbo_gebruiksdoel := "woon"]

## ---------------------------------------------------------------------
## 6. Pandtijdlijn erbij (m:1 op pand_bag_nr)
## ---------------------------------------------------------------------

for (v in c("bv_eerste_datum", "bv_laatste_datum", "bouwstart_eerste_datum",
            "oplevering_eerste_datum", "last_pand_status_datum")) {
  pnd[[v]] <- as_datum(pnd[[v]])
}

pand <- pnd[, .(
  pand_bag_nr,
  bv_eerste_datum, bv_laatste_datum,
  bouwstart_eerste_datum, oplevering_eerste_datum,
  vergunningjaar,
  pand_bouwjaar,
  last_pand_status,
  last_pand_status_datum,
  is_censored           = as.logical(iscensored),
  is_nieuwbouw_pand     = as.logical(isnieuwbouw),
  bouwjaar_mismatch     = as.logical(bouwjaar_mismatch),
  duur_verg_tot_oplevering, duur_verg_tot_bouwstart, duur_bouwstart_tot_oplev,
  duur_negatief         = as.logical(duur_negatief),
  duur_onrealistisch    = as.logical(duur_onrealistisch),
  bron_pandduur         = TRUE
)]

vbo <- merge(vbo, pand, by = "pand_bag_nr", all.x = TRUE)
vbo[is.na(bron_pandduur), bron_pandduur := FALSE]

## ---------------------------------------------------------------------
## 7. Afgeleide sleutels en variabelen
## ---------------------------------------------------------------------

## Project-id. Documentnummers zijn uniek binnen een gemeente, niet
## landelijk -- zie ontwerpkeuze (b) bovenaan.
vbo[, projectid := paste(gemeentecode, documentnummer)]
vbo[is.na(documentnummer) | documentnummer == "",
    projectid := paste0("__los_", vbo_bag_nr)]   # nooit poolen op een lege sleutel
vbo[, projectomvang := .N, by = projectid]

## Vergunningjaar op VBO-niveau. bv_eerste_datum is de eerste vergunning
## op het pand; voor VBO's zonder pandrecord valt hij terug op de
## pand-vergunningdatum uit het vergunningenbestand.
vbo[, vergunning_datum := fifelse(!is.na(bv_eerste_datum), bv_eerste_datum, bv_pand_datum)]
vbo[, vergunning_jaar  := as.integer(format(vergunning_datum, "%Y"))]

## Afstandsklassen en blootstelling
vbo[, afstandsklasse := afstandsklasse(dist2n2000_m)]
vbo[, mol_woning     := mol_per_woning(dist2n2000_m)]
vbo[, depositie_project := projectomvang * mol_woning]

for (dr in PARAMS$drempels) {
  vbo[, paste0("kl", dr) := as.integer(!is.na(dist2n2000_m) & dist2n2000_m <= dr)]
}

## Bouwjaar-delta: de kerndiscriminator nieuwbouw vs. renovatie.
## Negatief = het gebouw stond er al. Zie docs/BAG-dataschoning-inzichten.md sec. 5-6.
vbo[, bouwjaar_delta := pand_bouwjaar - vergunningjaar]

setcolorder(vbo, c("vbo_bag_nr", "pand_bag_nr", "projectid", "projectomvang",
                   "gemeentecode", "provinciecode", "provincienaam",
                   "dist2n2000_m", "afstandsklasse",
                   "vbo_gebruiksdoel", "vbo_status", "last_pand_status",
                   "vergunning_datum", "vergunning_jaar",
                   "mutatiemaand", "mutatiejaar", "cbs_nieuwbouw"))
setkey(vbo, vbo_bag_nr)

## ---------------------------------------------------------------------
## 8. Wegschrijven
## ---------------------------------------------------------------------

kop("Resultaat")
cat(sprintf("VBO's totaal            : %s\n", nfmt(nrow(vbo))))
cat(sprintf("  met vergunningrecord  : %s\n", nfmt(sum(vbo$bron_vergunning))))
cat(sprintf("  in CBS-nieuwbouw      : %s\n", nfmt(sum(vbo$cbs_nieuwbouw))))
cat(sprintf("  met pandtijdlijn      : %s\n", nfmt(sum(vbo$bron_pandduur))))
cat(sprintf("panden                  : %s\n", nfmt(uniqueN(vbo$pand_bag_nr))))
cat(sprintf("projecten               : %s\n", nfmt(uniqueN(vbo$projectid))))

write_derived(vbo, "vbo_base")
