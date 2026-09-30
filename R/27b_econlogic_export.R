## =====================================================================
## 27b_econlogic_export.R -- vaste export voor het herontwikkelingspaper
##                           (EconLogicPaper, Redevelopment issue 30)
##
## Input : _derived/pijplijn/ (27a_pijplijn_export.py); analyse/zaken.csv
##         in de datamap van de bekendmakingen; _derived/aanvragen_titels.csv
##         (09a_titels.py); _derived/teksten_vlaggen.csv (24a_teksten.py);
##         _derived/vbo_base.rds; _derived/bestemming/ en
##         bestemming_punten.csv (28a_bestemming.py);
##         _derived/kennisgeving_dossier.csv (31a_kennisgevingen.py);
##         bron/plannen/ruimtelijkeplannen_plannen_20260701.csv
## Output: <VERGUNNINGEN_DATA>/export_econlogic_<yyyymmdd>/ met zaak.csv,
##         pand_zaak.csv, bag_gepland.csv, vbo_bag_datums.csv,
##         bestemming_punt_20131231.csv, bestemming_punt_nu.csv,
##         plan_klein_punt.csv en LEESMIJ.md
##
## De sleutels zijn zaak, pand, VBO of RD-punt, niet site_id: de sitevorming
## in EconLogicPaper verandert, deze bestanden niet. De regels voor nieuwbouw,
## sloop en vervanging staan in 27_regels.R, die voor de bestemming per punt
## in 28_regels.R: dezelfde als in 27_pijplijn_sites.R en 28_bestemming_sites.R.
## Puntkomma, datums als jjjj-mm-dd, vlaggen 0 of 1, leeg is onbekend.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
source(file.path(.here, "27_regels.R"))
source(file.path(.here, "28_regels.R"))
suppressPackageStartupMessages(library(bit64))
options(width = 200)
t0 <- Sys.time()
UIT <- file.path(PATHS$verg, sprintf("export_econlogic_%s", format(Sys.Date(), "%Y%m%d")))
dir.create(UIT, showWarnings = FALSE)
datum <- function(v) if (inherits(v, "IDate")) v else as.IDate(substr(as.character(v), 1, 10))
id16 <- function(v) sprintf("%016.0f", as.double(v))
als01 <- function(dt, kol) for (k in kol) set(dt, j = k, value = as.integer(dt[[k]]))
nlf <- function(n) trimws(formatC(as.numeric(n), big.mark = ".", format = "f", digits = 0))
schrijf <- function(dt, naam) {
  ## een lege tekst is onbekend: als NA schrijven, anders zet fwrite er "" neer
  for (k in names(dt)) if (is.character(dt[[k]])) set(dt, i = which(dt[[k]] == ""), j = k, value = NA_character_)
  fwrite(dt, file.path(UIT, naam), sep = ";", na = "", dateTimeAs = "ISO")
  cat(sprintf("%-28s %12s rijen\n", naam, nlf(nrow(dt))))
  data.table(bestand = naam, rijen = nrow(dt), kolommen = paste(names(dt), collapse = ";"))
}
TEL <- list()

## ---- 1. zaken -----------------------------------------------------------
PL <- file.path(PATHS$derived, "pijplijn")
zk <- fread(file.path(PL, "pijplijn_zaak.csv"), sep = ";", encoding = "UTF-8",
            colClasses = list(character = c("creator", "sleutelsoort", "sleutel", "uitkomst", "locatietype", "eerste_identifier", "titels")))
for (v in c("eerste_datum", "datum_aanvraag", "datum_uitkomst")) set(zk, j = v, value = datum(zk[[v]]))
za <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
            select = c("zaak_id", "gemeente", "datum_aanvraag", "uitgebreid", "woningen"))
za[, datum_aanvraag := datum(datum_aanvraag)]
za <- merge(za, fread(file.path(PATHS$derived, "aanvragen_titels.csv"), select = c("zaak_id", "soort")), by = "zaak_id", all.x = TRUE)
## zaken.csv en de export horen bij dezelfde zaakvorming (zoals in 27_pijplijn_sites.R)
chk <- merge(zk[, .(zaak_id, a1 = datum_aanvraag, g1 = creator)], za[, .(zaak_id, a2 = datum_aanvraag, g2 = gemeente)], by = "zaak_id")
gelijk <- chk[, mean(a1 == a2 & g1 == g2, na.rm = TRUE)]
cat(sprintf("zaken.csv tegen de export: %.3f%% met dezelfde gemeente en aanvraagdatum\n", 100 * gelijk))
stopifnot(gelijk > 0.999)
zk[za, on = "zaak_id", `:=`(uitgebreid = i.uitgebreid, soort = i.soort, woningen = i.woningen)]
rm(za, chk); invisible(gc())
zaak_vlaggen(zk)
tv <- fread(file.path(PATHS$derived, "teksten_vlaggen.csv"), sep = ";")
setnames(tv, c("teksten", "woningen"), c("n_teksten", "tekst_woningen"))
zk <- merge(zk, tv, by = "zaak_id", all.x = TRUE, sort = FALSE)
zk[, tekst := as.integer(!is.na(n_teksten))]
als01(zk, c("nieuwbouw", "sloop_woning", "nieuwbouw_echt", "vervanging"))
zk[, titels := NULL]
setorder(zk, zaak_id)
TEL$zaak <- schrijf(zk, "zaak.csv")
N_ZAAK <- list(nieuwbouw = zk[, sum(nieuwbouw)], sloop_woning = zk[, sum(sloop_woning)], nieuwbouw_echt = zk[, sum(nieuwbouw_echt)],
               vervanging = zk[, sum(vervanging)], met_tekst = zk[, sum(tekst)])
rm(zk, tv); invisible(gc())

## ---- 2. panden aan zaken: ongewijzigd ------------------------------------
stopifnot(file.copy(file.path(PL, "pijplijn_pand_zaak.csv"), file.path(UIT, "pand_zaak.csv"), overwrite = TRUE))
pz <- fread(file.path(UIT, "pand_zaak.csv"), sep = ";", colClasses = list(character = "pand_id"))
stopifnot(all(nchar(pz$pand_id) == 16L), !anyDuplicated(pz$pand_id))
TEL$pand_zaak <- data.table(bestand = "pand_zaak.csv", rijen = nrow(pz), kolommen = paste(names(pz), collapse = ";"))
cat(sprintf("%-28s %12s rijen\n", "pand_zaak.csv", nlf(nrow(pz))))
rm(pz)

## ---- 3. en 4. de BAG: geplande objecten en datums per VBO ----------------
vb <- readRDS(file.path(PATHS$derived, "vbo_base.rds"))
g <- vb[last_pand_status %chin% c("Bouwvergunning_verleend", "Bouw_gestart", "Niet_gerealiseerd_pand") & !is.na(x) & !is.na(y) &
          !is.na(vergunning_datum) & vergunning_datum >= as.IDate("2012-01-01"),
        .(vbo_id = id16(vbo_bag_nr), pand_id = id16(pand_bag_nr), status = last_pand_status, verg_datum = as.IDate(vergunning_datum),
          x = as.integer(x), y = as.integer(y))]
stopifnot(all(nchar(g$vbo_id) == 16L), !anyDuplicated(g$vbo_id))
TEL$bag_gepland <- schrijf(g, "bag_gepland.csv")
d <- vb[!is.na(vergunning_datum), .(vbo_id = id16(vbo_bag_nr), verg_datum = as.IDate(vergunning_datum),
                                    bouwstart_datum = as.IDate(bouwstart_eerste_datum), oplevering_datum = as.IDate(oplevering_eerste_datum),
                                    documentnummer)]
stopifnot(all(nchar(d$vbo_id) == 16L), !anyDuplicated(d$vbo_id))
TEL$vbo_bag_datums <- schrijf(d, "vbo_bag_datums.csv")
N_BAG <- list(gepland = g[, .N, by = status], met_vergunning = nrow(d))
rm(vb, g, d); invisible(gc())

## ---- 5. bestemming per punt, eind 2013 en nu ----------------------------
B <- file.path(PATHS$derived, "bestemming")
EB <- lees("Enkelbestemming"); BA <- lees("Bouwaanduiding"); MV <- lees("Maatvoering"); BV <- lees("Bouwvlak")
pt <- fread(file.path(PATHS$derived, "bestemming_punten.csv"), sep = ";")
per_punt <- function(S) {
  p <- S$punt[, .(pid, plan, plandatum = datum, hoofdgroep, woningtype_orde = as.integer(orde), bouwhoogte = hoogte,
                  in_bouwvlak = as.integer(in_bouwvlak))]
  we <- S$mv[!is.na(wooneenheden), .(wooneenheden = max(wooneenheden)), by = pid]
  p[we, on = "pid", wooneenheden := i.wooneenheden]
  p <- pt[p, on = "pid", nomatch = 0L]
  setorder(p, pid)
  p[, .(x, y, plan, plandatum, hoofdgroep, woningtype_orde, bouwhoogte, wooneenheden, in_bouwvlak)]
}
S14 <- stand(as.IDate("2013-12-31"))
TEL$b14 <- schrijf(per_punt(S14), "bestemming_punt_20131231.csv")
rm(S14); invisible(gc())
SNU <- stand(as.IDate("2026-07-01"), nu = TRUE)
TEL$bnu <- schrijf(per_punt(SNU), "bestemming_punt_nu.csv")
rm(SNU); invisible(gc())

## ---- 6. kleine plannen per punt (onder 10 ha), met de ontwerpdatum --------
plannen <- fread(file.path(PATHS$verg, "bron", "plannen", "ruimtelijkeplannen_plannen_20260701.csv"), sep = ";", encoding = "UTF-8",
                 select = c("identificatie", "opp_ha"))
plannen <- unique(plannen, by = "identificatie")
kg <- fread(file.path(PATHS$derived, "kennisgeving_dossier.csv"), sep = ";", colClasses = list(character = c("dossier", "ontwerp_kg")))
## zoals 28_bestemming_sites.R deel C: elk vlak van de enkelbestemming met een datum; de planstatus erbij, zodat een
## geconsolideerde versie (geen nieuw besluit) apart te houden is
alle <- EB$paren[EB$vlakken[!is.na(datum), .(fid, plan, datum, planstatus, dossier)], on = "fid", nomatch = 0L]
alle[plannen, on = .(plan = identificatie), opp_ha := i.opp_ha]
alle <- unique(alle[!is.na(opp_ha) & opp_ha < 10, .(pid, plan, datum, opp_ha, planstatus, dossier)])
alle[dossier == "" | is.na(dossier), dossier := sub("-[^-]*$", "", plan)]
alle[kg, on = "dossier", ontwerpdatum := as.IDate(i.ontwerp_kg)]
alle <- pt[alle, on = "pid", nomatch = 0L]
setorder(alle, pid, datum, plan)
TEL$klein <- schrijf(alle[, .(x, y, plan, datum, opp_ha, ontwerpdatum, planstatus)], "plan_klein_punt.csv")
N_KLEIN <- list(plannen = uniqueN(alle$plan), met_ontwerpdatum = alle[, uniqueN(plan[!is.na(ontwerpdatum)])],
                planstatus = alle[, .(plannen = uniqueN(plan)), by = planstatus][order(-plannen)])
rm(alle, EB, BA, MV, BV); invisible(gc())

## ---- 7. LEESMIJ ---------------------------------------------------------
git <- tryCatch(system2("git", c("-C", shQuote(.here), "rev-parse", "--short", "HEAD"), stdout = TRUE), error = function(e) NA_character_)
db <- file.info(file.path(PATHS$verg, "vergunningen.sqlite"))$mtime
tel <- rbindlist(TEL)
leesmij <- c(
  sprintf("# Export vergunningen en bestemming voor het herontwikkelingspaper (%s)", format(Sys.Date())),
  "",
  sprintf("Gemaakt door `R/27b_econlogic_export.R` in de repo Stikstof (commit %s, branch rapport-3.1) op %s. Voor Redevelopment issue 30 (EconLogicPaper, `R/11_permits.R`).",
          git[1], format(Sys.time(), "%Y-%m-%d %H:%M")),
  "Puntkomma als scheidingsteken, UTF-8, datums als jjjj-mm-dd, vlaggen 0 of 1, leeg is onbekend. Coördinaten in RD (EPSG:28992).",
  "",
  "## Bronnen en stand",
  "",
  sprintf("- Bekendmakingen: `vergunningen.sqlite` in de datamap, stand %s (de zaakvorming van 26 september 2026). Het herstel van `zaak.py` (commit 10ea670, inzichten hoofdstuk 31) is daarop nog niet toegepast. Na een herbouw verandert `zaak_id`; de stabiele sleutel is `eerste_identifier` (de eerste publicatie van de zaak, het KOOP-nummer), met `creator`, `sleutelsoort` en `sleutel`.",
          format(db, "%Y-%m-%d %H:%M")),
  "- BAG: `R/_derived/vbo_base.rds` uit de GeoDMS-export van het extract van 30 juli 2026 (BAG van 10 juli 2026).",
  "- Bestemming: de PDOK-download van 1 juli 2026 (Enkelbestemming, Bouwaanduiding, Maatvoering, Bouwvlak), gekoppeld door `R/28a_bestemming.py` aan de punten van `R/_derived/bestemming_punten.csv`. Die punten komen uit de juli-export van het herontwikkelingspaper (`perobject_Nederland_20260710`, alleen de bestaande opstal van de sites van toen). De export van 25 september 2026 op OVSRV06 heeft extra objecten (onveranderde niet-woonvoorraad); die punten staan hier niet in. Voor het hoofdmodel (sites met woningen vooraf) volstaat de juli-set.",
  "- Ontwerpdatum: `R/_derived/kennisgeving_dossier.csv` (`R/31a_kennisgevingen.py`), de eerste kennisgeving van een ontwerp per plandossier uit de officiële bekendmakingen; bekend voor 63 tot 90 procent van de plannen die van 2014 tot en met 2023 werden vastgesteld.",
  "",
  "## Bestanden",
  "",
  "- `zaak.csv`: een rij per gemeentelijke zaak, de kolommen van `pijplijn_zaak.csv` (`R/27a_pijplijn_export.py`) zonder de titels, met `uitgebreid` en `woningen` uit `analyse/zaken.csv`, `soort` uit `R/09a_titels.py` (met de woordgrens van 28 september 2026) en de vlaggen `nieuwbouw`, `sloop_woning`, `nieuwbouw_echt` en `vervanging` volgens `R/27_regels.R`. Daarnaast de vlaggen uit de volledige tekst (`R/24a_teksten.py`): `tekst` (1 als de volledige tekst is opgehaald), `n_teksten`, `natura`, `stikstof`, `soorten`, `vvgb`, `vvgb_raad`, `vvgb_prov`, `afwijken`, `milieu`, `brandveilig` en `tekst_woningen` (het aantal woningen uit de tekst); leeg zonder tekst. De tekstoogst dekt de aanvragen voor nieuwe woningen zonder aantal in de titel en de aanvragen met een gekoppeld nieuw BAG-pand (prioriteit 1 tot en met 3), niet alle zaken.",
  "- `pand_zaak.csv`: gelijk aan `pijplijn_pand_zaak.csv`: elk BAG-pand aan een zaak (`koppel_bag.py`), met afstand, aantal woningen en de begindatums van de pandstatussen; `pand_id` als tekst van 16 cijfers. Een pand hangt aan een zaak, niet per se aan de bouwaanvraag.",
  "- `bag_gepland.csv`: VBO's met als laatste pandstatus Bouwvergunning_verleend, Bouw_gestart of Niet_gerealiseerd_pand en een vergunningdatum vanaf 1 januari 2012 (deel D van `R/27_pijplijn_sites.R`).",
  "- `vbo_bag_datums.csv`: elk VBO in `vbo_base` met een vergunningdatum: de eerste datums van bouwstart en oplevering, en het documentnummer.",
  "- `bestemming_punt_20131231.csv` en `bestemming_punt_nu.csv`: per punt met een plan het vlak van de enkelbestemming met de laatste plandatum op of voor 31 december 2013, of voor 1 juli 2026 zonder vervallen of historische versies (`R/28_regels.R`); `woningtype_orde` 1 vrijstaand, 2 twee-aaneen, 3 aaneengebouwd, 4 gestapeld (de ruimste bouwaanduiding van hetzelfde plan op dat punt); `bouwhoogte` en `wooneenheden` het maximum van de maatvoering van hetzelfde plan op dat punt; `in_bouwvlak` 1 als het punt in een bouwvlak van dat plan ligt. De download bevat vooral de huidige plannen: de stand van 2013 dekt ongeveer de helft van de sites, die van 2012 14 tot 23 procent (inzichten hoofdstuk 38).",
  "- `plan_klein_punt.csv`: elk plan onder 10 hectare (oppervlakte uit de planlijst van de PDOK-download) waarvan een vlak van de enkelbestemming het punt raakt, met de plandatum, de ontwerpdatum uit de kennisgevingen en de planstatus. Zoals in deel C van `R/28_bestemming_sites.R` staan ook geconsolideerde versies erin; een geconsolideerde versie is geen nieuw besluit en is aan `planstatus` te herkennen.",
  "",
  "## Aantallen",
  "",
  "| Bestand | Rijen | Kolommen |", "|---|---|---|",
  tel[, sprintf("| %s | %s | %s |", bestand, nlf(rijen), kolommen)],
  "",
  sprintf("Zaken: nieuwbouw %s, sloop van een woning %s, nieuwbouw_echt %s, vervanging %s, met volledige tekst %s.",
          nlf(N_ZAAK$nieuwbouw), nlf(N_ZAAK$sloop_woning), nlf(N_ZAAK$nieuwbouw_echt),
          nlf(N_ZAAK$vervanging), nlf(N_ZAAK$met_tekst)),
  sprintf("Geplande objecten: %s. VBO's met een vergunningdatum: %s.",
          N_BAG$gepland[, paste(sprintf("%s %s", status, nlf(N)), collapse = ", ")], nlf(N_BAG$met_vergunning)),
  sprintf("Kleine plannen: %s, waarvan %s met een ontwerpdatum; naar planstatus: %s.", nlf(N_KLEIN$plannen),
          nlf(N_KLEIN$met_ontwerpdatum), N_KLEIN$planstatus[, paste(sprintf("%s %s", planstatus, nlf(plannen)), collapse = ", ")])
)
writeLines(leesmij, file.path(UIT, "LEESMIJ.md"), useBytes = TRUE)
cat(sprintf("Klaar in %.0f sec: %s\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), UIT))
