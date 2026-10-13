## =====================================================================
## 22_langereeks.R -- liep nabij natuur al voor 2012 achter?
##                    Nieuwe woningen naar bouwjaar, 1995 tot en met 2025
##
## Input : _derived/econlogic_vbo.rds (21a_econlogic.R: alle woningen in de
##         BAG sinds 2012, ook de bestaande voorraad, met bouwjaar),
##         R/22a_habitat.py (afstand tot habitat op het raster van 25 m),
##         _derived/vbo_analyse.rds (alleen voor de controle van de afstand)
## Output: _output/langereeks_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## De reeksen van dit rapport beginnen in 2012. De achterstand nabij natuur
## in de opleveringen heeft bij de PAS-uitspraak geen knik en zit in grote
## projecten; is ze een trend die al veel langer liep, dan is 2019 een
## willekeurig punt op die trend. Het bouwjaar van de woningvoorraad geeft
## een reeks vanaf 1995: elke woning die sinds 2012 in de BAG staat, met het
## bouwjaar van haar pand. Een woning die voor 2012 gesloopt is, ontbreekt;
## voor woningen van na 1995 is dat verwaarloosbaar. Woningen die na 2012
## door splitsing of transformatie in een bestaand pand ontstonden, tellen
## niet mee, want hun bouwjaar is dat van het pand.
## Schatting: Poisson op gemeente x nabij x bouwjaar, vaste effecten
## gemeente x nabij en gemeente x jaar, een coëfficiënt per jaar tegenover
## 2018, en een trendbreuk met een lineaire trend voor, een sprong en een
## trendverandering, bij elk breekjaar van 2000 tot en met 2022, zodat te
## zien is of 2019 eruit springt. Standaardfouten geclusterd op gemeente.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
stopifnot(requireNamespace("bit64", quietly = TRUE))
options(width = 200)

sink(file.path(PATHS$output, sprintf("langereeks_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
JAREN <- 1995:2025
NABIJ_M <- 2500

e <- readRDS(file.path(PATHS$derived, "econlogic_vbo.rds"))
e <- e[woon == TRUE & !is.na(x) & !is.na(bouwjaar) & !is.na(gemeente_code)]
setorder(e, vbo_id, redev_yearmonth)
e <- e[!duplicated(vbo_id)]

## afstand tot habitat: de unieke punten door 22a_habitat.py
pt <- unique(e[, .(x, y)])[, id := .I]
f_in <- file.path(PATHS$derived, "langereeks_xy.csv"); f_uit <- file.path(PATHS$derived, "langereeks_habitat.csv")
fwrite(pt[, .(id, x, y)], f_in, sep = ";")
qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
cmd <- tempfile(fileext = ".cmd")
writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO",
             sprintf('call "%s" "%s" punten "%s" "%s" "%s"', normalizePath(qgis), normalizePath(file.path(.here, "22a_habitat.py")),
                     normalizePath(f_in), normalizePath(f_uit, mustWork = FALSE), normalizePath(PATHS$verg)), "set RC=%ERRORLEVEL%", "endlocal & exit /b %RC%"), cmd)
if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("22a_habitat.py faalde")
h <- fread(f_uit, sep = ";")
pt[h, on = "id", `:=`(afstand = i.afstand, ndep = i.ndep_2018)]
e[pt, on = c("x", "y"), `:=`(afstand = i.afstand, ndep = i.ndep)]
stopifnot(e[is.na(afstand), .N] == 0L)
e <- e[afstand >= 0]

## ---------------------------------------------------------------------
## 0. Controle: de rasterafstand tegenover dist2n2000_m uit GeoDMS, voor de
##    nieuwbouw die in beide bestanden staat
## ---------------------------------------------------------------------
kop("0. Controle van de afstand: raster van 25 m tegenover dist2n2000_m")
d <- read_derived("vbo_analyse")[cbs_nieuwbouw == TRUE & !is.na(dist2n2000_m), .(vbo_bag_nr, dist2n2000_m)]
d[, vbo_id := as.character(vbo_bag_nr)]
d[, vbo_id := paste0(strrep("0", 16L - nchar(vbo_id)), vbo_id)]
v <- merge(d[, .(vbo_id, dist2n2000_m)], e[, .(vbo_id, afstand)], by = "vbo_id")
R$controle <- v[, .(n = .N, zelfde_klasse = mean((dist2n2000_m <= NABIJ_M) == (afstand <= NABIJ_M)),
                    mediaan_verschil = median(abs(afstand - dist2n2000_m)), p99_verschil = quantile(abs(afstand - dist2n2000_m), .99))]
print(R$controle)
rm(d, v); invisible(gc())

## ---------------------------------------------------------------------
## 1. Nieuwe woningen per bouwjaar, nabij en ver
## ---------------------------------------------------------------------
kop("1. Nieuwe woningen per bouwjaar, nabij (<= 2.500 m) en ver, landelijk")
e <- e[!redev_type %in% c("Toevoeging", "Transformatie_Plus") & bouwjaar %in% JAREN]
e[, G := as.integer(afstand <= NABIJ_M)]
R$landelijk <- dcast(e[, .N, by = .(jaar = bouwjaar, G)], jaar ~ G, value.var = "N")
setnames(R$landelijk, c("0", "1"), c("ver", "nabij"))
R$landelijk[, aandeel_nabij := nabij / (nabij + ver)]
print(R$landelijk[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. Binnen de gemeente: een coëfficiënt per jaar tegenover 2018
## ---------------------------------------------------------------------
paneel <- function(dt) {
  cel <- dt[, .(y = .N), by = .(E = gemeente_code, G, t = bouwjaar)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:1, t = JAREN)
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t))]
  p[]
}
WEG <- list("geen" = character(0), "vier grote steden" = c("GM0363", "GM0599", "GM0518", "GM0344"))
kop("2. Nabij tegenover ver per bouwjaar, binnen de gemeente, tegenover 2018")
R$jaren <- rbindlist(lapply(names(WEG), function(w) {
  p <- paneel(e[!gemeente_code %in% WEG[[w]]])
  m <- fepois(y ~ i(t, G, ref = 2018) | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  ct[, jaar := as.integer(sub("^t::(\\d+):G$", "\\1", term))]
  rbind(ct[, .(zonder = w, jaar, b = Estimate, se = `Std. Error`, p = `Pr(>|z|)`)],
        data.table(zonder = w, jaar = 2018L, b = 0, se = 0, p = NA_real_))[order(jaar)]
}))
R$jaren[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
print(dcast(R$jaren, jaar ~ zonder, value.var = "irr")[, lapply(.SD, function(x) round(x, 3))])

## ---------------------------------------------------------------------
## 3. Trendbreuk bij elk breekjaar: trend voor, sprong, trendverandering
## ---------------------------------------------------------------------
kop("3. Trendbreuk nabij tegenover ver, binnen de gemeente, per breekjaar")
p0 <- paneel(e)
R$breuk <- rbindlist(lapply(2000:2022, function(bj) {
  p <- copy(p0)[, `:=`(tt = t - bj, na = as.integer(t >= bj))]
  p[, `:=`(trend = G * tt, sprong = G * na, natrend = G * na * tt)]
  m <- fepois(y ~ trend + sprong + natrend | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
  ct <- coeftable(m)
  data.table(breekjaar = bj, voortrend = ct["trend", 1], se_voortrend = ct["trend", 2],
             sprong = ct["sprong", 1], se_sprong = ct["sprong", 2], p_sprong = ct["sprong", 4],
             natrend = ct["natrend", 1], se_natrend = ct["natrend", 2], p_natrend = ct["natrend", 4])
}))
print(R$breuk[, lapply(.SD, function(x) round(x, 3))])
## de gemiddelde jaarlijkse trend nabij tegenover ver over de hele periode
m_lin <- fepois(y ~ trend | E_G + E_t, data = copy(p0)[, trend := G * (t - 2018)], cluster = ~E, notes = FALSE)
R$lineair <- data.table(b = coef(m_lin)[["trend"]], se = se(m_lin)[["trend"]], p = pvalue(m_lin)[["trend"]])
print(R$lineair)
## ---------------------------------------------------------------------
## 4. Drie perioden: 1995-2010 en 2019-2025 tegenover 2011-2018, met een
##    Wald-toets op het verschil tussen de eerste en de laatste
## ---------------------------------------------------------------------
kop("4. Drie perioden, binnen de gemeente: 1995-2010 en 2019-2025 tegenover 2011-2018")
perioden <- function(p) {
  p <- copy(p)[, `:=`(vroeg = G * as.integer(t <= 2010L), laat = G * as.integer(t >= 2019L))]
  m <- fepois(y ~ vroeg + laat | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
  b <- coef(m); V <- vcov(m)
  dv <- b[["laat"]] - b[["vroeg"]]; sv <- sqrt(V["laat", "laat"] + V["vroeg", "vroeg"] - 2 * V["laat", "vroeg"])
  data.table(b_vroeg = b[["vroeg"]], se_vroeg = se(m)[["vroeg"]], p_vroeg = pvalue(m)[["vroeg"]],
             b_laat = b[["laat"]], se_laat = se(m)[["laat"]], p_laat = pvalue(m)[["laat"]],
             laat_min_vroeg = dv, se_verschil = sv, p_verschil = 2 * pnorm(-abs(dv / sv)))
}
TYPEN <- list("alle woningen" = NULL, "appartementen" = "appartement",
              "grondgebonden" = c("vrijstaand", "twee_onder_1_kap", "rijtjeswoning"))
R$perioden <- rbindlist(lapply(names(WEG), function(w) rbindlist(lapply(names(TYPEN), function(ty) {
  x <- e[!gemeente_code %in% WEG[[w]]]
  if (!is.null(TYPEN[[ty]])) x <- x[woningtype %in% TYPEN[[ty]]]
  cbind(zonder = w, woningen = ty, perioden(paneel(x)))
}))))
print(R$perioden[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 5. Per woningtype een coëfficiënt per jaar tegenover 2018
## ---------------------------------------------------------------------
kop("5. Nabij tegenover ver per bouwjaar en woningtype, binnen de gemeente, tegenover 2018")
R$jaren_type <- rbindlist(lapply(names(TYPEN)[-1], function(ty) {
  m <- fepois(y ~ i(t, G, ref = 2018) | E_G + E_t, data = paneel(e[woningtype %in% TYPEN[[ty]]]), cluster = ~E, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  ct[, jaar := as.integer(sub("^t::(\\d+):G$", "\\1", term))]
  rbind(ct[, .(woningen = ty, jaar, b = Estimate, se = `Std. Error`, p = `Pr(>|z|)`)],
        data.table(woningen = ty, jaar = 2018L, b = 0, se = 0, p = NA_real_))[order(jaar)]
}))
R$jaren_type[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
print(dcast(R$jaren_type, jaar ~ woningen, value.var = "irr")[, lapply(.SD, function(x) round(x, 3))])
R$per_type <- e[, .N, by = .(woningtype, jaar = bouwjaar)][order(jaar, woningtype)]

## ---------------------------------------------------------------------
## 6. Vier perioden: voor de Natuurbeschermingswet 1998 van kracht werd
##    (oktober 2005), daarna tot 2010, 2011-2018 als referentie, 2019-2025
## ---------------------------------------------------------------------
kop("6. Vier perioden, binnen de gemeente, tegenover 2011-2018")
vier <- function(p) {
  p <- copy(p)[, `:=`(a = G * as.integer(t <= 2004L), b = G * as.integer(t >= 2005L & t <= 2010L), c = G * as.integer(t >= 2019L))]
  m <- fepois(y ~ a + b + c | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
  k <- coef(m); V <- vcov(m)
  w <- function(i, j) { dv <- k[[i]] - k[[j]]; sv <- sqrt(V[i, i] + V[j, j] - 2 * V[i, j]); c(dv, sv, 2 * pnorm(-abs(dv / sv))) }
  ca <- w("c", "a"); cb <- w("c", "b")
  data.table(b_1995_2004 = k[["a"]], se_1995_2004 = se(m)[["a"]], b_2005_2010 = k[["b"]], se_2005_2010 = se(m)[["b"]],
             b_2019_2025 = k[["c"]], se_2019_2025 = se(m)[["c"]], p_2019_2025 = pvalue(m)[["c"]],
             na_min_1995 = ca[1], p_na_min_1995 = ca[3], na_min_2005 = cb[1], p_na_min_2005 = cb[3])
}
R$vier <- rbindlist(lapply(names(WEG), function(w) cbind(zonder = w, vier(paneel(e[!gemeente_code %in% WEG[[w]]])))))
print(R$vier[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 7. Tweede route voor 2012-2025: dezelfde jaarcoëfficiënten uit de
##    opleveringen (CBS-nieuwbouw, jaar van de mutatie), met de afstand en
##    de gemeente uit het objectenbestand
## ---------------------------------------------------------------------
kop("7. Bouwjaar tegenover jaar van oplevering, 2012-2025, binnen de gemeente, tegenover 2018")
d <- read_derived("vbo_analyse")[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(mutatiemaand) & !is.na(gemeentecode)]
d[, `:=`(t = as.integer(format(mutatiemaand, "%Y")), G = as.integer(dist2n2000_m <= NABIJ_M), E = as.character(gemeentecode))]
cel <- d[t >= 2012L & t <= 2025L, .(y = .N), by = .(E, G, t)]
p <- merge(CJ(E = sort(unique(cel$E)), G = 0:1, t = 2012:2025), cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
p[, `:=`(E_G = paste(E, G), E_t = paste(E, t))]
m <- fepois(y ~ i(t, G, ref = 2018) | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
ct <- as.data.table(coeftable(m), keep.rownames = "term")[, jaar := as.integer(sub("^t::(\\d+):G$", "\\1", term))]
R$oplevering <- rbind(ct[, .(jaar, b = Estimate, se = `Std. Error`)], data.table(jaar = 2018L, b = 0, se = 0))[order(jaar)][, irr := exp(b)]
v <- merge(R$oplevering[, .(jaar, irr_oplevering = irr)], R$jaren[zonder == "geen", .(jaar, irr_bouwjaar = irr)], by = "jaar")
print(v[, lapply(.SD, function(x) round(x, 3))])
R$samenhang <- v[, .(r = cor(irr_oplevering, irr_bouwjaar), gem_2012_2018_opl = mean(irr_oplevering[jaar <= 2018]),
                     gem_2012_2018_bj = mean(irr_bouwjaar[jaar <= 2018]), gem_2019_2025_opl = mean(irr_oplevering[jaar >= 2019]),
                     gem_2019_2025_bj = mean(irr_bouwjaar[jaar >= 2019]))]
print(R$samenhang)

## ---------------------------------------------------------------------
## 8. De ringen naast elkaar: oppervlakte, voorraad van 2012 en de bouw
##    per duizend bestaande woningen, voor en na 2019Q2 (elk 29 kwartalen)
## ---------------------------------------------------------------------
kop("8. Oppervlakte, voorraad en bouw per ring")
f_ring <- file.path(PATHS$derived, "ringen_oppervlakte.csv")
writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO",
             sprintf('call "%s" "%s" ringen - "%s" "%s"', normalizePath(qgis), normalizePath(file.path(.here, "22a_habitat.py")),
                     normalizePath(f_ring, mustWork = FALSE), normalizePath(PATHS$verg)), "set RC=%ERRORLEVEL%", "endlocal & exit /b %RC%"), cmd)
if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("22a_habitat.py ringen faalde")
RINGEN <- c("0 tot 1 km", "1 tot 2,5 km", "2,5 tot 5 km", "verder dan 5 km")
ring_van <- function(a) factor(fifelse(a <= 1000, RINGEN[1], fifelse(a <= 2500, RINGEN[2], fifelse(a <= 5000, RINGEN[3], RINGEN[4]))), levels = RINGEN)
opp <- fread(f_ring, sep = ";", encoding = "UTF-8")
## de voorraad van 2012: woningen die er toen al stonden (geen toevoegende mutatie sinds 2012)
vr <- readRDS(file.path(PATHS$derived, "econlogic_vbo.rds"))[woon == TRUE & !is.na(x) & !is.na(bouwjaar)]
## per object: een object met een toevoegende mutatie in een van zijn rijen telt niet mee
plus <- vr[redev_type %in% c("Nieuwbouw", "SN_Nieuwbouw", "Toevoeging", "Transformatie_Plus"), unique(vbo_id)]
vr <- vr[bouwjaar <= 2011L & !vbo_id %in% plus]
vr <- vr[!duplicated(vbo_id)]
vr[pt, on = c("x", "y"), afstand := i.afstand]
vr <- vr[afstand >= 0]
kw_i <- function(datum) (as.integer(format(datum, "%Y")) - 2012L) * 4L + (as.integer(format(datum, "%m")) - 1L) %/% 3L + 1L
d[, q := kw_i(mutatiemaand)]
d <- d[q >= 1L & q <= 58L]
R$ringen <- Reduce(function(a, b) merge(a, b, by = "ring"), list(
  opp,
  vr[, .(voorraad_2012 = .N), by = .(ring = ring_van(afstand))],
  d[q < 30L, .(opgeleverd_voor = .N), by = .(ring = ring_van(dist2n2000_m))],
  d[q >= 30L, .(opgeleverd_na = .N), by = .(ring = ring_van(dist2n2000_m))]))
R$ringen <- R$ringen[order(match(ring, RINGEN))]
R$ringen[, `:=`(voorraad_per_km2 = voorraad_2012 / km2_nederland,
                per_1000_per_jaar_voor = 1000 * opgeleverd_voor / voorraad_2012 / (29 / 4),
                per_1000_per_jaar_na = 1000 * opgeleverd_na / voorraad_2012 / (29 / 4))]
print(R$ringen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

## ---------------------------------------------------------------------
## 9. Per depositiekwartiel: liepen zwaar belaste plekken al voor 2019
##    achter, en sinds wanneer? De totale depositie van 2018 op de plek
##    (RIVM, 1 km), met de kwartielgrenzen van hoofdstuk 10 (11_ruimte.R:
##    de vergunde objecten van 2012 tot de uitspraak), per kwartiel
##    tegenover het lichtste, binnen de gemeente
## ---------------------------------------------------------------------
kop("9. Per depositiekwartiel tegenover het lichtste, binnen de gemeente")
gr <- unlist(jsonlite::fromJSON(file.path(PATHS$output, sprintf("ruimte_%s.json", EXTRACT)))$dosis_grenzen)
cat("grenzen:", gr, "
")
e[, kw := fifelse(ndep > 0, findInterval(ndep, gr) + 1L, NA_integer_)]
print(e[, .N, by = kw][order(kw)])
R$depositie <- rbindlist(lapply(2:4, function(k) cbind(kwartiel = k, vier(paneel(copy(e[kw %in% c(1L, k)])[, G := as.integer(kw == k)])))))
print(R$depositie[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
R$depositie_jaren <- rbindlist(lapply(2:4, function(k) {
  m <- fepois(y ~ i(t, G, ref = 2018) | E_G + E_t, data = paneel(copy(e[kw %in% c(1L, k)])[, G := as.integer(kw == k)]), cluster = ~E, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[, jaar := as.integer(sub("^t::(\\d+):G$", "\\1", term))]
  rbind(ct[, .(kwartiel = k, jaar, b = Estimate, se = `Std. Error`)], data.table(kwartiel = k, jaar = 2018L, b = 0, se = 0))[order(jaar)]
}))
R$depositie_jaren[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
print(dcast(R$depositie_jaren, jaar ~ kwartiel, value.var = "irr")[, lapply(.SD, function(x) round(x, 3))])
R$depositie_grenzen <- gr
## het zwaarste kwartiel tegenover het lichtste: zonder de vier grote steden,
## per woningtype, en een trendbreuk bij elk breekjaar
k4 <- function(x) paneel(copy(x[kw %in% c(1L, 4L)])[, G := as.integer(kw == 4L)])
R$depositie_varianten <- rbindlist(list(
  cbind(variant = "zonder de vier grote steden", vier(k4(e[!gemeente_code %in% WEG[["vier grote steden"]]]))),
  cbind(variant = "appartementen", vier(k4(e[woningtype %in% TYPEN[["appartementen"]]]))),
  cbind(variant = "grondgebonden", vier(k4(e[woningtype %in% TYPEN[["grondgebonden"]]])))))
print(R$depositie_varianten[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
p4 <- k4(e)
R$depositie_breuk <- rbindlist(lapply(2005:2022, function(bj) {
  p <- copy(p4)[, `:=`(trend = G * (t - bj), sprong = G * as.integer(t >= bj), natrend = G * as.integer(t >= bj) * (t - bj))]
  m <- fepois(y ~ trend + sprong + natrend | E_G + E_t, data = p, cluster = ~E, notes = FALSE)
  ct <- coeftable(m)
  data.table(breekjaar = bj, voortrend = ct["trend", 1], sprong = ct["sprong", 1], se_sprong = ct["sprong", 2], p_sprong = ct["sprong", 4],
             natrend = ct["natrend", 1], p_natrend = ct["natrend", 4], loglik = logLik(m))
}))
print(R$depositie_breuk[, lapply(.SD, function(x) round(x, 3))])
## tweede route: dezelfde jaarcoefficienten uit de opleveringen (CBS-nieuwbouw, jaar van de mutatie), met de
## depositie per object uit 11a_rasters.py (vbo_raster.csv, via vbo_xy.csv)
rs <- merge(fread(file.path(PATHS$derived, "vbo_xy.csv"), sep = ";"), fread(file.path(PATHS$derived, "vbo_raster.csv"), sep = ";")[, .(id, ndep_2018)], by = "id")
d[rs, on = c("x", "y"), ndep := i.ndep_2018]
d2 <- d[t >= 2012L & t <= 2025L & ndep > 0][, kw := findInterval(ndep, gr) + 1L][kw %in% c(1L, 4L)][, G := as.integer(kw == 4L)]
cel2 <- d2[, .(y = .N), by = .(E, G, t)]
p2 <- merge(CJ(E = sort(unique(cel2$E)), G = 0:1, t = 2012:2025), cel2, by = c("E", "G", "t"), all.x = TRUE); p2[is.na(y), y := 0]
p2[, `:=`(E_G = paste(E, G), E_t = paste(E, t))]
m2 <- fepois(y ~ i(t, G, ref = 2018) | E_G + E_t, data = p2, cluster = ~E, notes = FALSE)
c2 <- as.data.table(coeftable(m2), keep.rownames = "term")[, jaar := as.integer(sub("^t::(\\d+):G$", "\\1", term))]
R$depositie_oplevering <- rbind(c2[, .(jaar, b = Estimate, se = `Std. Error`)], data.table(jaar = 2018L, b = 0, se = 0))[order(jaar)][, irr := exp(b)]
v2 <- merge(R$depositie_oplevering[, .(jaar, irr_oplevering = irr)], R$depositie_jaren[kwartiel == 4L, .(jaar, irr_bouwjaar = irr)], by = "jaar")
print(v2[, lapply(.SD, function(x) round(x, 3))])
R$depositie_samenhang <- v2[, .(r = cor(irr_oplevering, irr_bouwjaar), gem_2012_2014_opl = mean(irr_oplevering[jaar <= 2014]),
                                gem_2012_2014_bj = mean(irr_bouwjaar[jaar <= 2014]), gem_2019_2025_opl = mean(irr_oplevering[jaar >= 2019]),
                                gem_2019_2025_bj = mean(irr_bouwjaar[jaar >= 2019]))]
print(R$depositie_samenhang)

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), woningen = nrow(e),
               gemeenten = uniqueN(e$gemeente_code), jaren = range(JAREN))

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("langereeks_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("langereeks_%s.json", EXTRACT)))
sink()
