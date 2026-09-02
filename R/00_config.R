## =====================================================================
## 00_config.R -- paden, parameters, hulpfuncties
## Project: Stikstof & woningbouw (RERQ)
##
## Wordt door elk script ge-source't. Bevat geen analyse.
## =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

## ---------------------------------------------------------------------
## 1. Paden
## ---------------------------------------------------------------------
## De ruwe GeoDMS-exports staan NIET in deze repo (te groot, en het zijn
## afgeleide data). Ze worden geproduceerd door cfg/main/Analyse/N2000_Analyse.dms
## en weggeschreven naar de Teams/OneDrive-map hieronder.
##
## Zet desgewenst een omgevingsvariabele STIKSTOF_DATA om dit te overrulen.

PATHS <- list(
  repo    = Sys.getenv("STIKSTOF_REPO", "C:/ProjDir/Stikstof"),
  raw     = Sys.getenv("STIKSTOF_DATA",
              "D:/Radboud Universiteit/Woningbouw RU-VU - General/data/data_natuur/input_woningen"),
  eib     = Sys.getenv("STIKSTOF_EIB",
              "D:/Radboud Universiteit/Woningbouw RU-VU - General/data/data_natuur/stikstofdepositie_eib.xlsx"),
  derived = Sys.getenv("STIKSTOF_DERIVED", "C:/ProjDir/Stikstof/R/_derived"),
  output  = Sys.getenv("STIKSTOF_OUTPUT",  "C:/ProjDir/Stikstof/R/_output")
)

dir.create(PATHS$derived, showWarnings = FALSE, recursive = TRUE)
dir.create(PATHS$output,  showWarnings = FALSE, recursive = TRUE)

## ---------------------------------------------------------------------
## 2. Welk extract
## ---------------------------------------------------------------------
## Alle drie de exports moeten van dezelfde GeoDMS-run komen: de
## bestandsdatum in de naam is de exportdatum, BAG_file_date staat in de
## bijbehorende .xml (20260730 -> BAG van 20260710).

EXTRACT <- "20260730"

FILES <- list(
  pand_duur  = file.path(PATHS$raw, sprintf("Pand_VergunningDuur_%s.csv",            EXTRACT)),
  nieuwbouw  = file.path(PATHS$raw, sprintf("Object_Nieuwbouw_%s.csv",               EXTRACT)),
  vergunning = file.path(PATHS$raw, sprintf("Object_BouwvergunningVerleend_%s.csv",  EXTRACT))
)

## ---------------------------------------------------------------------
## 3. Analyseparameters
## ---------------------------------------------------------------------

PARAMS <- list(
  ## Behandelmoment: PAS-uitspraak Raad van State, 29 mei 2019
  treatment_jaar   = 2019,
  referentie_jaar  = 2018,

  ## Waarnemingsvenster. 2026 is een halve jaargang (mutaties t/m 2026-07)
  ## en moet er altijd uit. 2024 is het eerste jaar onder de Omgevingswet
  ## en de Wkb-"knip" -> zie CHECK in 03_checks.R en de gevoeligheidsanalyse.
  jaar_min         = 2012,
  jaar_max         = 2025,
  wkb_breuk_jaar   = 2024,

  ## Renovatie-drempel: pand_bouwjaar - vergunningjaar <= -6 betekent dat
  ## het gebouw er al stond. Zie docs/BAG-dataschoning-inzichten.md sec. 6.
  ##
  ## Waarom -6 (issue #26)? Het is een EIGEN WERKAFSPRAAK, geen norm uit CBS
  ## of BAG: de spiegel van het plausibiliteitsvenster |delta| > 5 in
  ## `bouwjaar_mismatch` (N2000_Analyse.dms, ingevoerd onder "//adhoc tests").
  ## Presenteer hem ook zo, en suggereer geen bron die er niet is.
  ##
  ## Hij ligt wel ongeveer op de empirische knik: op pandniveau storten de
  ## aantallen in van 3.639 (delta -1) naar 210 (-5) en worden daarna vlak, en
  ## het aandeel "Pand_in_gebruik" springt van 15-37% bij -1..-5 naar 60-98%
  ## vanaf -6 -- pas daar zijn het bestaande, bewoonde panden (= renovatie).
  ##
  ## En hij is inert: robuustheidscheck -4 / -6 / -9 geeft op 250 m -0,2398 /
  ## -0,2394 / -0,2399 bij een standaardfout van 0,088; de regel helemaal
  ## uitzetten scheelt 0,004. Zie docs sec. 8 voor de volledige tabellen.
  reno_delta       = -6,

  ## Afstandsklassen: breedte in meters en bovengrens (alles daarboven in
  ## een restklasse). Komt overeen met n2000_afstandsklasse_250 in Stata.
  klasse_breedte   = 250,
  klasse_max_m     = 10000,

  ## Drempels voor de "geraakt"-dummies, in meters
  drempels         = c(250, 500, 1000, 2500, 5000),

  ## EIB-depositiecurve: ln(mol/woning) = a + b * ln(afstand)
  ## Geschat in 01b_eib_curve.R op stikstofdepositie_eib.xlsx.
  eib_a            = 6.411439,
  eib_b            = -1.737805
)

## VBO-statussen die volgens de CBS-voorraaddefinitie meetellen in de
## woningvoorraad. Let op: "buiten gebruik" telt mee.
## Bron: CBS, Afleiding van Voorraad woningen (StatLine 81955);
##       docs/BAG-dataschoning-inzichten.md sec. 4.
VBO_IN_VOORRAAD <- c(
  "Verblijfsobject_in_gebruik",
  "Verblijfsobject_in_gebruik_niet_ingemeten",
  "Verblijfsobject_buiten_gebruik",
  "Verbouwing_verblijfsobject"
)

## Pandstatussen waarbij het pand in de voorraad zit
PAND_IN_VOORRAAD <- c(
  "Pand_in_gebruik",
  "Pand_in_gebruik_niet_ingemeten",
  "Pand_buiten_gebruik"
)

## Pandstatussen die op de pijplijn duiden
PAND_PIPELINE <- c(
  "Bouwvergunning_verleend",
  "Bouw_gestart"
)

## Pandstatussen die op sloop duiden. Sloop is orthogonaal aan realisatie:
## een gebouw kan zijn opgeleverd en later gesloopt, of nooit zijn gebouwd
## terwijl het oude pand wel is gesloopt. Zie 02_classify.R regel (e).
PAND_GESLOOPT <- c(
  "Pand_gesloopt",
  "Sloopvergunning_verleend"
)

## ---------------------------------------------------------------------
## 4. Hulpfuncties
## ---------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## GeoDMS schrijft datums als YYYYMMDD-integer, maanden als YYYYMM.
as_datum <- function(x) as.Date(as.character(x), format = "%Y%m%d")
as_maand <- function(x) as.Date(paste0(as.character(x), "01"), format = "%Y%m%d")

## Afstand -> klasse-index (1 = 0-250 m, ...), restklasse bovenaan
afstandsklasse <- function(m, breedte = PARAMS$klasse_breedte,
                           max_m = PARAMS$klasse_max_m) {
  k <- pmax(1L, as.integer(ceiling(m / breedte)))
  top <- as.integer(max_m / breedte) + 1L
  k[!is.na(m) & m > max_m] <- top
  k
}

afstandsklasse_labels <- function(breedte = PARAMS$klasse_breedte,
                                  max_m = PARAMS$klasse_max_m) {
  n <- as.integer(max_m / breedte)
  c(sprintf("%d-%d m", (0:(n - 1)) * breedte, (1:n) * breedte),
    sprintf("> %d m", max_m))
}

## Depositie per woning volgens de EIB-curve
mol_per_woning <- function(afstand_m) {
  exp(PARAMS$eib_a + PARAMS$eib_b * log(afstand_m))
}

## Nederlandse duizendtalscheiding zonder de format()-waarschuwing
nfmt <- function(x) formatC(as.numeric(x), format = "d", big.mark = ".",
                            decimal.mark = ",")

## Consistent lezen/schrijven van tussenbestanden
derived_path <- function(name) file.path(PATHS$derived, paste0(name, ".rds"))
write_derived <- function(dt, name) {
  saveRDS(dt, derived_path(name), compress = FALSE)
  message(sprintf("  -> %s  (%s rijen, %s kolommen)",
                  basename(derived_path(name)), nfmt(nrow(dt)), ncol(dt)))
  invisible(dt)
}
read_derived <- function(name) {
  p <- derived_path(name)
  if (!file.exists(p)) stop("Ontbreekt: ", p, "\nDraai eerst het script dat dit maakt.")
  readRDS(p)
}

## Kopje in de log
kop <- function(...) cat("\n", strrep("=", 70), "\n", sprintf(...), "\n",
                        strrep("=", 70), "\n", sep = "")
