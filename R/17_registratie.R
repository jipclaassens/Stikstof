## =====================================================================
## 17_registratie.R -- verschoof het registratiemoment van de BAG-status
##                     Bouwvergunning verleend onder de Omgevingswet?
##
## Input : _derived/vbo_analyse.rds
##         _derived/pand_zaak.csv (17a_pand_zaak.py, uit de koppeling van
##         BAG-panden aan zaken in het vergunningenspoor)
## Output: _output/registratie_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## 13_binnenring.R laat zien dat er onder de Wkb geen registraties wegvallen.
## Wel veranderde de regel voor het moment van registratie: onder de
## Omgevingswet telt voor gevolgklasse 1 (vooral grondgebonden woningen) het
## akkoord op de bouwmelding, die uiterlijk vier weken voor de bouw wordt
## gedaan, en voor gevolgklasse 2 en 3 de technische bouwvergunning
## (factsheet VNG en Waarderingskamer, oktober 2023); sinds september 2025 is
## de vergunning voor de omgevingsplanactiviteit weer het standaardbron-
## document. Als gemeenten zich aan de regel van 2023 hielden, schuift de
## BAG-datum voor eengezinswoningen in 2024 en 2025 op naar de bouwstart en
## weg van het gepubliceerde vergunningbesluit. Drie toetsen:
##   1. de tijd van Bouwvergunning verleend tot Bouw gestart, per halfjaar
##      van vergunning en woningtype (aandeel gestart binnen 60 en 90 dagen);
##   2. de BAG-datum tegenover de publicatie van de uitkomst van de zaak in
##      de gemeentelijke bekendmakingen (koppel_bag.py: binnen 100 meter, de
##      BAG-datum van 14 dagen voor de aanvraag tot 180 dagen na de uitkomst);
##   3. of die verschuiving in de binnenste kilometer anders is dan verder
##      dan 5 kilometer, binnen gemeente en periode.
## En de piek van juni 2026: een inhaalslag van oudere vergunningen of een
## maand met veel besluiten?
##
## Woningtype zoals in 13_binnenring.R: meergezins als het pand drie of meer
## woonobjecten telt in het objectenbestand, anders eengezins. Eenheid is
## het pand: de registratie gebeurt per pand.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("registratie_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
d <- read_derived("vbo_analyse")
R <- list()
EINDE <- as.Date(EXTRACT, "%Y%m%d")   # datum van het BAG-extract

f_pz <- file.path(PATHS$derived, "pand_zaak.csv")
if (!file.exists(f_pz)) {
  if (system2("py", c("-3.13", shQuote(normalizePath(file.path(.here, "17a_pand_zaak.py"))),
                      shQuote(normalizePath(f_pz, mustWork = FALSE)), shQuote(normalizePath(PATHS$verg)))) != 0)
    stop("17a_pand_zaak.py faalde")
}

vbo_per_pand <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
d <- merge(d, vbo_per_pand, by = "pand_bag_nr", all.x = TRUE)
d[, type := fifelse(n_pand >= 3L, "meergezins", "eengezins")]
halfjaar <- function(datum) paste0(format(datum, "%Y"), fifelse(as.integer(format(datum, "%m")) <= 6L, "H1", "H2"))

## pandniveau: de vroegste datums van het pand en de kleinste afstand
s <- d[in_sample == TRUE & !is.na(vergunning_datum) & vergunning_datum >= as.Date("2019-01-01")]
p <- s[, .(bvv = min(vergunning_datum), start = min(bouwstart_eerste_datum), opl = min(oplevering_eerste_datum),
           type = type[1L], dist = min(dist2n2000_m), gm = gemeentecode[1L], objecten = .N,
           x = mean(x), y = mean(y)), by = pand_bag_nr]
p[, `:=`(hj = halfjaar(bvv), dagen_start = as.integer(start - bvv), volg = as.integer(EINDE - bvv))]
p[, ring := fifelse(dist <= 1000, "0 tot 1 km", fifelse(dist <= 2500, "1 tot 2,5 km",
            fifelse(dist <= 5000, "2,5 tot 5 km", "verder dan 5 km")))]
p[, periode := fifelse(bvv < as.Date("2021-07-01"), "voor juli 2021",
               fifelse(bvv < as.Date("2024-01-01"), "juli 2021 tot 2024",
               fifelse(bvv < as.Date("2025-09-01"), "2024 tot september 2025", "vanaf september 2025")))]
## pand_bag_nr is integer64: zonder de namespace van bit64 geeft as.character()
## onzin (zie 01_build_vbo.R); met de namespace geladen werkt de S3-methode
stopifnot(requireNamespace("bit64", quietly = TRUE))
p[, pand_id := as.character(pand_bag_nr)]
stopifnot(all(nchar(p$pand_id) <= 16L))
p[, pand_id := paste0(strrep("0", 16L - nchar(pand_id)), pand_id)]
## een bouwstart voor de vergunningdatum komt vrijwel niet voor, omdat de BAG
## de statussen van een pand in volgorde registreert; daarmee is een
## inhaalslag dus niet te zien (sectie 4 gebruikt de bekendmakingen)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), panden = nrow(p),
               bouwstart_voor_vergunning = mean(!is.na(p$start) & p$start < p$bvv))

## ---------------------------------------------------------------------
## 1. Van vergunning tot bouwstart
## ---------------------------------------------------------------------
kop("1. Aandeel panden met Bouw gestart binnen 60 en 90 dagen na Bouwvergunning verleend")
## alleen panden met minstens 90 dagen tussen vergunning en extract
R$start <- p[volg >= 90, .(panden = .N, objecten = sum(objecten), zonder_start = mean(is.na(start)),
                          binnen60 = mean(!is.na(dagen_start) & dagen_start <= 60),
                          binnen90 = mean(!is.na(dagen_start) & dagen_start <= 90)), by = .(type, hj)]
setorder(R$start, type, hj)
print(R$start[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. BAG-datum tegenover de publicatie van het besluit
## ---------------------------------------------------------------------
kop("2. BAG-vergunningdatum tegenover de publicatie van de uitkomst van de gekoppelde zaak")
pz <- fread(f_pz, sep = ";", colClasses = list(character = c("pand_id", "verg_begin", "datum_aanvraag", "datum_uitkomst")))
pz <- pz[!is.na(datum_uitkomst) & nchar(datum_uitkomst) >= 10L]
pz[, uitk := as.Date(substr(datum_uitkomst, 1L, 10L))]
setorder(pz, pand_id, -uitk)
R$meta$pand_zaak_dubbel <- sum(duplicated(pz$pand_id))
pz <- pz[!duplicated(pand_id)]
p[, zaak := pand_id %in% pz$pand_id]   # gekoppeld aan een zaak, met of zonder uitkomst
p <- merge(p, pz[, .(pand_id, uitk, uitkomst)], by = "pand_id", all.x = TRUE)
p[, na_publ := as.integer(bvv - uitk)]   # dagen van publicatie van de uitkomst tot de BAG-datum
k <- p[bvv >= as.Date("2021-07-01") & bvv <= as.Date("2026-06-30")]   # de landelijke bekendmakingen beginnen juli 2021
R$publicatie <- k[, .(panden = .N, gekoppeld = mean(!is.na(uitk)), verleend = mean(uitkomst %in% "verleend"),
                      rond14 = mean(abs(na_publ[uitkomst %in% "verleend"]) <= 14),
                      later30 = mean(na_publ[uitkomst %in% "verleend"] > 30),
                      eerder14 = mean(na_publ[uitkomst %in% "verleend"] < -14),
                      mediaan = as.double(median(na_publ[uitkomst %in% "verleend"]))), by = .(type, hj)]
setorder(R$publicatie, type, hj)
print(R$publicatie[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 3. Binnenste kilometer tegenover verder dan 5 kilometer
## ---------------------------------------------------------------------
kop("3. Per ring en periode, en binnen gemeente en periode: binnenste kilometer tegenover verder dan 5 km")
R$ring <- k[uitkomst %in% "verleend" & periode != "voor juli 2021",
            .(panden = .N, later30 = mean(na_publ > 30)), by = .(type, periode, ring)]
R$ring <- merge(R$ring, p[volg >= 90 & bvv >= as.Date("2021-07-01") & bvv <= as.Date("2026-06-30"),
                          .(panden_start = .N, binnen60 = mean(!is.na(dagen_start) & dagen_start <= 60)), by = .(type, periode, ring)],
                by = c("type", "periode", "ring"), all = TRUE)
setorder(R$ring, type, periode, ring)
print(R$ring[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
## lineair kansmodel met vaste effecten gemeente x periode en ring; de termen
## zijn de verandering van het verschil binnen tegenover ver ten opzichte van
## juli 2021 tot 2024, in procentpunten. Gemeente x periode en niet x halfjaar:
## bij meergezins hebben maar een handvol gemeente-halfjaren panden in beide
## ringen, en dan rust de schatting op een paar cellen
ring_did <- function(dt, uitkomst) {
  dt <- dt[ring %in% c("0 tot 1 km", "verder dan 5 km")]
  dt[, `:=`(binnen = as.integer(ring == "0 tot 1 km"), gm_per = paste(gm, periode),
            b_ow = as.integer(ring == "0 tot 1 km" & periode == "2024 tot september 2025"),
            b_s25 = as.integer(ring == "0 tot 1 km" & periode == "vanaf september 2025"))]
  m <- feols(as.formula(sprintf("%s ~ b_ow + b_s25 | gm_per + binnen", uitkomst)), data = dt, cluster = ~gm, notes = FALSE)
  ct <- coeftable(m)
  gemengd <- dt[, .(b = uniqueN(binnen)), by = gm_per][b == 2L, .N]
  ## een term zonder variatie binnen gemeente x periode valt weg: dan NA
  rbindlist(lapply(c(b_ow = "2024 tot september 2025", b_s25 = "vanaf september 2025"), function(x) data.table(term = x)))[
    , `:=`(pp = NA_real_, se = NA_real_, p = NA_real_, n = nobs(m), cellen_beide_ringen = gemengd)][
    , c("pp", "se", "p") := {
        rn <- c("b_ow", "b_s25")
        list(ifelse(rn %in% rownames(ct), 100 * ct[match(rn, rownames(ct)), 1], NA_real_),
             ifelse(rn %in% rownames(ct), 100 * ct[match(rn, rownames(ct)), 2], NA_real_),
             ifelse(rn %in% rownames(ct), ct[match(rn, rownames(ct)), 4], NA_real_))
      }][]
}
k[, later30 := as.integer(na_publ > 30)]
kk <- p[volg >= 90 & bvv >= as.Date("2021-07-01") & bvv <= as.Date("2026-06-30")]
kk[, binnen60 := as.integer(!is.na(dagen_start) & dagen_start <= 60)]
R$ring_did <- rbindlist(lapply(c("eengezins", "meergezins"), function(ty) rbind(
  cbind(type = ty, uitkomst = "later dan 30 dagen na publicatie", ring_did(k[uitkomst %in% "verleend" & type == ty], "later30")),
  cbind(type = ty, uitkomst = "bouwstart binnen 60 dagen", ring_did(kk[type == ty], "binnen60")))))
print(R$ring_did[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 4. De piek van juni 2026
## ---------------------------------------------------------------------
kop("4. Juni 2026: vergunde objecten, gemeenten, dagen en koppeling")
o <- d[in_sample == TRUE & !is.na(vergunning_datum) & vergunning_datum >= as.Date("2025-01-01")]
o[, mnd := format(vergunning_datum, "%Y-%m")]
print(o[, .N, by = mnd][order(mnd)])
jun <- o[mnd == "2026-06"]
g <- jun[, .N, by = gemeentecode][order(-N)]
## een inhaalslag geeft panden zonder recent besluit in de buurt, maar met een
## ouder besluit (17b_besluit_nabij.py); vergeleken met juni tot en met augustus
## 2025 en september 2025 tot en met mei 2026. Meierijstad (1948) en Bernheze
## (1721) apart: die registreerden in juni op een paar dagen ruim veertig keer
## hun gewone aantal (docs/BAG-dataschoning-inzichten.md, sectie 7)
BATCH <- c(1948L, 1721L)
f_pr <- file.path(PATHS$derived, "panden_recent.csv"); f_bn <- file.path(PATHS$derived, "besluit_nabij.csv")
if (!file.exists(f_bn)) {
  fwrite(p[bvv >= as.Date("2025-06-01") & bvv <= as.Date("2026-06-30"), .(pand_id, bvv, x, y)], f_pr, sep = ";")
  if (system2("py", c("-3.13", shQuote(normalizePath(file.path(.here, "17b_besluit_nabij.py"))), shQuote(normalizePath(f_pr)),
                      shQuote(normalizePath(f_bn, mustWork = FALSE)), shQuote(normalizePath(PATHS$verg)))) != 0)
    stop("17b_besluit_nabij.py faalde")
}
bn <- fread(f_bn, sep = ";", colClasses = list(character = "pand_id"))
r <- merge(p[bvv >= as.Date("2025-06-01") & bvv <= as.Date("2026-06-30")], bn, by = "pand_id")
r[, maand := fifelse(format(bvv, "%Y-%m") == "2026-06", fifelse(gm %in% BATCH, "juni 2026, Meierijstad en Bernheze", "juni 2026, overige gemeenten"),
             fifelse(bvv >= as.Date("2025-09-01"), "september 2025 tot en met mei 2026", "juni tot en met augustus 2025"))]
R$juni2026_besluiten <- r[, .(panden = .N, objecten = sum(objecten), gemeenten = uniqueN(gm), dagen = uniqueN(bvv),
                              maanden = uniqueN(format(bvv, "%Y-%m")),
                              gekoppeld = mean(zaak), aandeel_objecten_gekoppeld = sum(objecten[zaak]) / sum(objecten),
                              mediaan_publicatie = format(median(uitk, na.rm = TRUE)),
                              rond14 = mean(abs(na_publ[uitkomst %in% "verleend"]) <= 14),
                              ongekoppeld_alleen_oud = mean(oud[!zaak] == 1L & recent[!zaak] == 0L),
                              ongekoppeld_geen_besluit = mean(oud[!zaak] == 0L & recent[!zaak] == 0L)), by = maand]
R$juni2026_besluiten[, `:=`(panden_per_maand = panden / maanden, objecten_per_maand = objecten / maanden)]
## dezelfde twee gemeenten in de twaalf maanden ervoor
R$juni2026_batch_ervoor <- p[gm %in% BATCH & bvv >= as.Date("2025-06-01") & bvv < as.Date("2026-06-01"), .(panden_per_maand = .N / 12)]
print(R$juni2026_besluiten)
p[, mnd := format(bvv, "%Y-%m")]
vgl <- p[mnd >= "2025-09" & mnd <= "2026-05"]
pj <- p[mnd == "2026-06"]
R$juni2026 <- list(
  objecten = nrow(jun), objecten_gemiddeld_2025 = o[substr(mnd, 1L, 4L) == "2025", .N] / 12,
  objecten_jan_mei_2026 = o[mnd >= "2026-01" & mnd <= "2026-05", .N] / 5,
  gemeenten = nrow(g), aandeel_grootste_drie = sum(head(g$N, 3L)) / nrow(jun),
  aandeel_drukste_dag = max(jun[, .N, by = vergunning_datum]$N) / nrow(jun),
  panden = nrow(pj), gekoppeld = mean(!is.na(pj$uitk)), gekoppeld_sep_mei = mean(!is.na(vgl$uitk)),
  rond14 = pj[uitkomst %in% "verleend", mean(abs(na_publ) <= 14)], rond14_sep_mei = vgl[uitkomst %in% "verleend", mean(abs(na_publ) <= 14)],
  later30 = pj[uitkomst %in% "verleend", mean(na_publ > 30)], later30_sep_mei = vgl[uitkomst %in% "verleend", mean(na_publ > 30)],
  mediaan_publicatie = format(median(pj$uitk, na.rm = TRUE)))
str(R$juni2026)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("registratie_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("registratie_%s.json", EXTRACT)))
sink()
