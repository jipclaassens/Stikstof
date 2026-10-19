## =====================================================================
## 39_mediaclaims.R -- beweringen uit het mediadebat ("Nederland op slot")
##                     naast de data
##
## Input : _derived/vbo_analyse.rds (02_classify.R); analyse/zaken.csv in de
##         datamap van de bekendmakingen (VERGUNNINGEN_DATA);
##         _derived/aanvragen_titels.csv (09a_titels.py);
##         _derived/gemeentenamen.csv (19_ringen.R)
## Output: _output/media_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Een overzicht van waar het beeld "Nederland zit op slot" vandaan komt,
## noemt een paar beweringen die het rapport nog niet apart toetst:
##  1. de daling van de vergunningen in 2019 kwam door de PAS-uitspraak
##     (29 mei 2019); kwam ze voor of na de uitspraak, en nabij of overal?
##  2. na de uitspraken over intern salderen (december 2024, januari 2025)
##     stopten provincies met stikstofvergunningen, met Noord-Brabant en
##     Fryslan als voorbeeld; viel de woningbouw daar stil?
##  3. de woningbouw "blijft op slot" in 2026.
## Dezelfde steekproef en afstand als 10_landelijk.R: vergunde objecten op
## de eerste bouwvergunning, nabij = binnen 2.500 m van stikstofgevoelig
## habitat. De vergelijking binnen de gemeente volgt hoofdstuk 8 en 9:
## Poisson per gemeente en kwartaal, standaardfouten geclusterd op gemeente.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
sink(file.path(PATHS$output, sprintf("media_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()

## ---- parameters ---------------------------------------------------------
M <- list(
  nabij_m       = 2500,                       # drempel van hoofdstuk 8 en 9
  pas           = as.Date("2019-05-29"),      # PAS-uitspraak
  stop_begin    = as.Date("2025-01-01"),      # eerste kwartaal na intern salderen (18-12-2024) en Pasgeld-West (14-01-2025)
  herstart      = as.Date("2026-01-01"),      # eigen term vanaf 2026: Noord-Brabant verwachtte weer te vergunnen, Friesland stopte tot uiterlijk september 2026
  venster_begin = as.Date("2021-01-01"),      # provinciale vergelijking: vier jaar voor de stop
  laatste_verg  = as.Date("2026-03-31"),      # laatste volle kwartaal vergunningen in het extract (10_landelijk.R)
  provincies    = c("Noord-Brabant", "Friesland")     # zoals provincienaam in de BAG-export
)

d <- read_derived("vbo_analyse")
verg <- d[in_sample == TRUE & !is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01") &
          vergunning_datum <= M$laatste_verg & !is.na(dist2n2000_m) & !is.na(gemeentecode)]
verg[, `:=`(nabij = as.integer(dist2n2000_m <= M$nabij_m), maand = as.IDate(format(vergunning_datum, "%Y-%m-01")),
            jaar = year(vergunning_datum), kw = (month(vergunning_datum) - 1L) %/% 3L + 1L)]
verg[, q := (jaar - 2012L) * 4L + kw]
cat("vergunde objecten:", nfmt(nrow(verg)), "\n")
cat("provincienamen:", paste(sort(unique(verg$provincienaam)), collapse = " | "), "\n")
stopifnot(all(M$provincies %in% verg$provincienaam))

## ---------------------------------------------------------------------
## 1. De daling van 2019: voor of na de uitspraak, nabij of overal?
## ---------------------------------------------------------------------
kop("1. Vergunde objecten per maand, 2017 tot en met 2020, nabij en ver")
pm <- dcast(verg[jaar %in% 2017:2020, .N, by = .(maand, nabij)], maand ~ nabij, value.var = "N", fill = 0)
setnames(pm, c("0", "1"), c("ver", "nabij"))
pm[, aandeel_nabij := nabij / (ver + nabij)]
print(pm[, .(maand, ver, nabij, aandeel_nabij = round(aandeel_nabij, 3))])
R$per_maand <- pm

## Hetzelfde stuk van het jaar tegenover een jaar eerder: januari tot en met
## mei (voor de uitspraak) en juni tot en met december (erna)
deel <- function(m) fifelse(month(m) <= month(M$pas), "jan-mei", "jun-dec")
vj <- verg[jaar %in% 2017:2020, .N, by = .(jaar, deel = deel(maand), nabij)]
vj <- dcast(vj, deel + nabij ~ jaar, value.var = "N")
vj[, `:=`(pct_2019_op_2018 = 100 * (`2019` / `2018` - 1), pct_2020_op_2019 = 100 * (`2020` / `2019` - 1))]
kop("1b. Hetzelfde deel van het jaar tegenover een jaar eerder")
print(vj[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
R$deel_jaar <- vj

## Begon de daling nabij eerder of sterker? Per halfjaar binnen de gemeente,
## nabij tegenover ver, elk halfjaar tegenover hetzelfde halfjaar van 2018
## (dus met nabij x seizoen als vast effect, zoals hoofdstuk 8)
p1 <- verg[jaar %in% 2017:2020, .N, by = .(gemeentecode, nabij, hj = paste0(jaar, "H", fifelse(month(maand) <= 6L, 1L, 2L)))]
vol <- CJ(gemeentecode = unique(p1$gemeentecode), nabij = 0:1, hj = sort(unique(p1$hj)))
p1 <- p1[vol, on = .(gemeentecode, nabij, hj)][is.na(N), N := 0L]
p1[, `:=`(gn = paste(gemeentecode, nabij), gt = paste(gemeentecode, hj), sz = paste(nabij, substr(hj, 6, 6)))]
m1 <- fepois(N ~ i(hj, nabij, ref = c("2018H1", "2018H2")) | gn + gt + sz, data = p1, cluster = ~gemeentecode, notes = FALSE)
ct <- as.data.table(coeftable(m1), keep.rownames = "term")
setnames(ct, c("term", "b", "se", "z", "p"))
ct[, `:=`(halfjaar = sub("^hj::(.*):nabij$", "\\1", term), pct = 100 * (exp(b) - 1))]
kop("1c. Nabij tegenover ver binnen de gemeente, per halfjaar, tegenover hetzelfde halfjaar van 2018 (procent)")
print(ct[, .(halfjaar, pct = round(pct, 1), se = round(se, 3), p = round(p, 3))])
R$nabij_halfjaar <- ct[, .(halfjaar, b, se, p, pct)]

## ---------------------------------------------------------------------
## 2. De provinciale vergunningstop: Noord-Brabant en Fryslan
## ---------------------------------------------------------------------
kop("2. Vergunde objecten per provincie en jaar (2026: eerste kwartaal)")
verg[, groep := fifelse(provincienaam %chin% M$provincies, provincienaam, "overige tien")]
py <- verg[jaar >= 2018L, .N, by = .(groep, jaar, nabij)]
py <- dcast(py, groep + jaar ~ nabij, value.var = "N", fill = 0)
setnames(py, c("0", "1"), c("ver", "nabij"))
py[, totaal := ver + nabij]
py[, index_2021_2023 := 100 * totaal / mean(totaal[jaar %in% 2021:2023]), by = groep]
print(py[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
R$provincie_jaar <- py

## Eerste kwartaal tegenover het eerste kwartaal, want 2026 heeft er een
pq1 <- verg[kw == 1L & jaar >= 2022L, .N, by = .(groep, jaar, nabij)]
pq1 <- rbind(pq1[, .(N = sum(N), objecten = "alle"), by = .(groep, jaar)],
             pq1[nabij == 1L, .(N, objecten = "nabij"), by = .(groep, jaar)])
pq1 <- dcast(pq1, groep + objecten ~ jaar, value.var = "N")
pq1[, pct_2026_op_2025 := 100 * (`2026` / `2025` - 1)]
kop("2b. Eerste kwartaal per jaar")
print(pq1[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
R$provincie_q1 <- pq1

## Binnen de gemeente: de provincie na de stop tegenover de overige tien,
## gemeente en kwartaal als vaste effecten. Een term voor 2025 (de stop) en
## een voor 2026Q1 (Noord-Brabant verwachtte weer te vergunnen).
qi <- function(dt) (year(dt) - 2012L) * 4L + (month(dt) - 1L) %/% 3L + 1L
p2 <- verg[maand >= M$venster_begin, .N, by = .(gemeentecode, groep, q, nabij)]
vol <- unique(p2[, .(gemeentecode, groep)])[, .(q = qi(M$venster_begin):qi(M$laatste_verg)), by = .(gemeentecode, groep)]
vol <- vol[, .(nabij = 0:1), by = .(gemeentecode, groep, q)]
p2 <- p2[vol, on = .(gemeentecode, groep, q, nabij)][is.na(N), N := 0L]
p2[, `:=`(na_stop = as.integer(q >= qi(M$stop_begin) & q < qi(M$herstart)), na_herstart = as.integer(q >= qi(M$herstart)))]

did <- function(prov, data, nabij_alleen = NULL) {
  x <- data[groep %chin% c(prov, "overige tien")]
  if (!is.null(nabij_alleen)) x <- x[nabij == nabij_alleen]
  x <- x[, .(N = sum(N)), by = .(gemeentecode, groep, q, na_stop, na_herstart)]
  x[, `:=`(in_2025 = na_stop * (groep == prov), in_2026 = na_herstart * (groep == prov))]
  m <- fepois(N ~ in_2025 + in_2026 | gemeentecode + q, data = x, cluster = ~gemeentecode, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "b", "se", "z", "p"))
  ct[, `:=`(provincie = prov, objecten = if (is.null(nabij_alleen)) "alle" else if (nabij_alleen == 1L) "nabij" else "ver",
            pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1),
            gemeenten_prov = uniqueN(x[groep == prov, gemeentecode]), n_prov_na = x[groep == prov & na_stop == 1L, sum(N)])]
  ct[]
}
## Drievoudig: nabij tegenover ver in de provincie na de stop, tegenover
## hetzelfde verschil in de overige tien. Vaste effecten gemeente x nabij,
## gemeente x kwartaal en nabij x kwartaal.
did3 <- function(prov, data) {
  x <- data[groep %chin% c(prov, "overige tien")]
  x[, `:=`(in_2025 = nabij * na_stop * (groep == prov), in_2026 = nabij * na_herstart * (groep == prov),
           gn = paste(gemeentecode, nabij), gq = paste(gemeentecode, q), nq = paste(nabij, q))]
  m <- fepois(N ~ in_2025 + in_2026 | gn + gq + nq, data = x, cluster = ~gemeentecode, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "b", "se", "z", "p"))
  ct[, `:=`(provincie = prov, objecten = "nabij min ver", pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1),
            hi = 100 * (exp(b + 1.96 * se) - 1), gemeenten_prov = uniqueN(x[groep == prov, gemeentecode]),
            n_prov_na = x[groep == prov & na_stop == 1L & nabij == 1L, sum(N)])]
  ct[]
}
R$provincie_did <- rbindlist(c(lapply(M$provincies, function(p) did(p, p2)),
                               lapply(M$provincies, function(p) did(p, p2, 1L)),
                               lapply(M$provincies, did3, data = p2)))
kop("2c. Provincie na de stop tegenover de overige tien, binnen de gemeente (venster 2021Q1 tot en met 2026Q1)")
print(R$provincie_did[, .(provincie, objecten, term, pct = round(pct, 1), lo = round(lo, 1), hi = round(hi, 1),
                          p = round(p, 3), gemeenten_prov, n_prov_na)])

## ---------------------------------------------------------------------
## 3. Aanvragen voor nieuwbouw van woningen per provincie
## ---------------------------------------------------------------------
## De bekendmakingen zijn sinds juli 2021 landelijk. De gemeente staat er
## als naam in; de provincie komt uit de BAG (gemeentecode) via de
## gemeentenamen van de buurtkaart. Wat niet koppelt, blijft buiten.
kop("3. Aanvragen voor nieuwbouw van woningen per provincie (bekendmakingen)")
z <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
           select = c("zaak_id", "gemeente", "datum_aanvraag", "nabij", "verleend", "geweigerd"))
tt <- fread(file.path(PATHS$derived, "aanvragen_titels.csv"), select = c("zaak_id", "streng", "tijdelijk", "soort"))
z[tt, on = "zaak_id", nb := as.integer(i.soort == "nieuwbouw_woning" & i.streng %in% 1L & !(i.tijdelijk %in% 1L))]
z[is.na(nb), nb := 0L]
z[, datum := as.IDate(substr(as.character(datum_aanvraag), 1, 10))]
LAATSTE <- max(z$datum, na.rm = TRUE)
nm <- fread(file.path(PATHS$derived, "gemeentenamen.csv"), encoding = "UTF-8")
gp <- verg[, .N, by = .(gemeentecode, provincienaam)][order(-N)][, .SD[1], by = gemeentecode]
nm <- merge(nm, gp[, .(gemeentecode, provincienaam)], by = "gemeentecode")
## zonder toevoeging tussen haakjes, zoals "Hengelo (O.)" en "Rijswijk (ZH.)"
norm <- function(s) tolower(gsub("[^[:alnum:]]", "", iconv(sub("\\s*\\(.*\\)$", "", s), "UTF-8", "ASCII//TRANSLIT")))
nm[, sleutel := norm(gemeentenaam)]
z[, sleutel := norm(gemeente)]
z[nm, on = "sleutel", provincienaam := i.provincienaam]
gek <- z[datum >= as.IDate("2021-07-01"), .(zaken = .N, gekoppeld = sum(!is.na(provincienaam)))]
cat(sprintf("gekoppeld aan een provincie sinds juli 2021: %s van %s zaken (%.1f%%)\n", nfmt(gek$gekoppeld), nfmt(gek$zaken),
            100 * gek$gekoppeld / gek$zaken))
cat("niet gekoppelde gemeentenamen (meeste zaken):\n")
print(z[datum >= as.IDate("2021-07-01") & is.na(provincienaam), .N, by = gemeente][order(-N)][1:15])
za <- z[!is.na(provincienaam) & datum >= as.IDate("2022-01-01") & nb == 1L]
za[, groep := fifelse(provincienaam %chin% M$provincies, provincienaam, "overige tien")]
## per jaar tot en met de laatste aanvraagdag, en 2026 naast dezelfde
## dagen van 2025
dag <- as.integer(format(LAATSTE, "%j"))
aj <- za[, .(aanvragen = .N, tot_dag = sum(as.integer(format(datum, "%j")) <= dag)), by = .(groep, jaar = year(datum))]
aj <- aj[order(groep, jaar)]
aj[, index_2022_2024 := 100 * aanvragen / mean(aanvragen[jaar %in% 2022:2024]), by = groep]
print(aj[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat(sprintf("tot_dag = aanvragen tot en met dag %d van het jaar (laatste aanvraag %s)\n", dag, LAATSTE))
R$aanvragen_jaar <- aj
R$aanvragen_koppeling <- gek

## Dezelfde vergelijking binnen de gemeente, op de aanvragen
p3 <- za[datum <= as.IDate(format(LAATSTE - 90L)), .N, by = .(sleutel, groep, q = qi(datum))]
vol <- unique(p3[, .(sleutel, groep)])[, .(q = min(p3$q):max(p3$q)), by = .(sleutel, groep)]
p3 <- p3[vol, on = .(sleutel, groep, q)][is.na(N), N := 0L]
p3[, `:=`(na_stop = as.integer(q >= qi(M$stop_begin) & q < qi(M$herstart)), na_herstart = as.integer(q >= qi(M$herstart)))]
R$aanvragen_did <- rbindlist(lapply(M$provincies, function(prov) {
  x <- p3[groep %chin% c(prov, "overige tien")]
  x[, `:=`(in_2025 = na_stop * (groep == prov), in_2026 = na_herstart * (groep == prov))]
  m <- fepois(N ~ in_2025 + in_2026 | sleutel + q, data = x, cluster = ~sleutel, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "b", "se", "z", "p"))
  ct[, `:=`(provincie = prov, pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1),
            gemeenten_prov = uniqueN(x[groep == prov, sleutel]))]
}))
kop("3b. Nieuwbouwaanvragen, provincie na de stop tegenover de overige tien, binnen de gemeente (2022Q1 tot drie maanden voor de laatste aanvraag)")
print(R$aanvragen_did[, .(provincie, term, pct = round(pct, 1), lo = round(lo, 1), hi = round(hi, 1), p = round(p, 3), gemeenten_prov)])

## ---------------------------------------------------------------------
## 4. 2026 tegenover 2025: het eerste kwartaal, nabij en ver, per zone
## ---------------------------------------------------------------------
kop("4. Het eerste kwartaal van 2026 tegenover dat van 2025")
zone <- function(m) fifelse(m <= 1000, "0-1 km", fifelse(m <= 3000, "1-3 km", fifelse(m <= 5000, "3-5 km", "verder")))
z4 <- dcast(verg[kw == 1L & jaar %in% 2024:2026, .N, by = .(zone = zone(dist2n2000_m), jaar)], zone ~ jaar, value.var = "N")
z4 <- rbind(z4, z4[, c(list(zone = "totaal"), lapply(.SD, sum)), .SDcols = -"zone"])
z4[, pct_2026_op_2025 := 100 * (`2026` / `2025` - 1)]
print(z4[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
R$q1_zones <- z4

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), nabij_m = M$nabij_m,
               stop_begin = format(M$stop_begin), herstart = format(M$herstart), laatste_verg = format(M$laatste_verg),
               laatste_aanvraag = format(LAATSTE))
writeLines(to_json(R), file.path(PATHS$output, sprintf("media_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
