## =====================================================================
## 26_samenstelling.R -- zijn de depositiedaling en de stap bij de
##                        appartementen een kwestie van stad en land?
##
## Input : _derived/vbo_analyse.rds, vbo_xy.csv, vbo_raster.csv,
##         _derived/econlogic_vbo.rds (21a_econlogic.R),
##         _output/ruimte_<extract>.json (de depositiegrenzen),
##         R/22a_habitat.py (afstand tot habitat en tot overbelaste
##         natuur, depositie 2018, vroege contour, per punt)
## Output: _output/samenstelling_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Twee uitkomsten van het rapport kunnen een verschil tussen stad en land
## zijn in plaats van iets van de natuur:
##   A. Op zwaar belaste plekken (totale depositie 2018) daalde de bouw
##      tegenover de lichtst belaste in dezelfde gemeente, in de
##      vergunningen al vanaf 2012 (hoofdstuk 10). Zwaar belaste plekken
##      liggen vooral in de landelijke delen van een gemeente.
##   B. In de binnenste kilometer daalden de vergunde appartementen sinds
##      de Omgevingswet tegenover verder dan 5 km (hoofdstuk 13), vooral
##      doordat ze verderop, in de stedelijke delen, verdubbelden.
## Toets: dezelfde vergelijking binnen gemeente en buurtdichtheid (OAD van
## de CBS-buurt van 2012 in drie klassen, zoals 21_verdichting.R), en
## voor A ook binnen en buiten de vroege contour (de Ladder voor duurzame
## verstedelijking raakt bouwen buiten bestaand stedelijk gebied) en naar
## de afstand tot overbelaste natuur (AERIUS, depositie boven de KDW,
## binnen dezelfde gemeente en hetzelfde depositiekwartiel: waar de
## stikstofruimte krap is).
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
stopifnot(requireNamespace("bit64", quietly = TRUE), requireNamespace("jsonlite", quietly = TRUE))
options(width = 200)

sink(file.path(PATHS$output, sprintf("samenstelling_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
KW_START <- 2012L
kw_index <- function(datum) (as.integer(format(datum, "%Y")) - KW_START) * 4L + (as.integer(format(datum, "%m")) - 1L) %/% 3L + 1L
Q_PAS <- kw_index(as.Date("2019-05-29")); Q_OW <- kw_index(as.Date("2024-01-01")); Q_SAL <- kw_index(as.Date("2024-12-18"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31")); Q_MAX_NWB <- kw_index(as.Date("2026-06-30"))
klasse <- function(oad) fifelse(is.na(oad), NA_character_, fifelse(oad >= 1500, "hoog", fifelse(oad >= 500, "midden", "laag")))
gr <- unlist(jsonlite::fromJSON(file.path(PATHS$output, sprintf("ruimte_%s.json", EXTRACT)))$dosis_grenzen)
cat("depositiegrenzen:", gr, "\n")
alle <- readRDS(file.path(PATHS$derived, "econlogic_vbo.rds"))

## =====================================================================
## A1. Woningen naar bouwjaar, 1995 tot en met 2025 (zoals 22_langereeks.R)
## =====================================================================
kop("A1. Woningen naar bouwjaar: zwaarst tegenover lichtst belast, binnen gemeente en dichtheid, contour en overbelasting")
e <- alle[woon == TRUE & !is.na(x) & !is.na(bouwjaar) & !is.na(gemeente_code)]
setorder(e, vbo_id, redev_yearmonth)
e <- e[!duplicated(vbo_id)]
pt <- unique(e[, .(x, y)])[, id := .I]
f_in <- file.path(PATHS$derived, "samenstelling_xy.csv"); f_uit <- file.path(PATHS$derived, "samenstelling_raster.csv")
fwrite(pt[, .(id, x, y)], f_in, sep = ";")
qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
cmd <- tempfile(fileext = ".cmd")
writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO",
             sprintf('call "%s" "%s" punten "%s" "%s" "%s"', normalizePath(qgis), normalizePath(file.path(.here, "22a_habitat.py")),
                     normalizePath(f_in), normalizePath(f_uit, mustWork = FALSE), normalizePath(PATHS$verg)),
             "set RC=%ERRORLEVEL%", "endlocal & exit /b %RC%"), cmd)
if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("22a_habitat.py faalde")
h <- fread(f_uit, sep = ";")
pt[h, on = "id", `:=`(afstand = i.afstand, ndep = i.ndep_2018, ovb = i.afst_overbelast, vroeg = i.stedelijk_vroeg)]
e[pt, on = c("x", "y"), `:=`(afstand = i.afstand, ndep = i.ndep, ovb = i.ovb, vroeg = i.vroeg)]
e <- e[afstand >= 0 & !redev_type %in% c("Toevoeging", "Transformatie_Plus") & bouwjaar %in% 1995:2025]
e[, `:=`(dichtheid = klasse(oad), kw = fifelse(ndep > 0, findInterval(ndep, gr) + 1L, NA_integer_),
         grond = woningtype %in% c("vrijstaand", "twee_onder_1_kap", "rijtjeswoning"), app = woningtype %in% "appartement")]
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), woningen = nrow(e),
               met_dichtheid = e[, mean(!is.na(dichtheid))], in_vijf_provincies = e[, mean(vroeg >= 0)],
               overbelast_binnen_2500 = e[, mean(ovb >= 0 & ovb <= 2500)])
print(R$meta)
## hoe dicht zijn de kwartielen: aandeel per dichtheidsklasse
R$kwartiel_dichtheid <- e[!is.na(kw) & !is.na(dichtheid), .N, by = .(kw, dichtheid)][, aandeel := N / sum(N), by = kw][order(kw, dichtheid)]
print(dcast(R$kwartiel_dichtheid, kw ~ dichtheid, value.var = "aandeel")[, lapply(.SD, function(x) round(x, 3))])

paneel_bj <- function(dt) {
  cel <- dt[, .(y = .N), by = .(E, G, t = bouwjaar)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:1, t = 1995:2025)
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t))]
  p[]
}
vier <- function(p) {
  p <- copy(p)[, `:=`(a = G * as.integer(t <= 2004L), b = G * as.integer(t >= 2005L & t <= 2010L), c = G * as.integer(t >= 2019L))]
  m <- tryCatch(fepois(y ~ a + b + c | E_G + E_t, data = p, cluster = ~gem, notes = FALSE), error = function(x) NULL)
  if (is.null(m)) return(data.table(b_1995_2004 = NA_real_))
  k <- coef(m); V <- vcov(m)
  w <- function(i, j) { dv <- k[[i]] - k[[j]]; sv <- sqrt(V[i, i] + V[j, j] - 2 * V[i, j]); c(dv, 2 * pnorm(-abs(dv / sv))) }
  ca <- w("c", "a"); cb <- w("c", "b")
  data.table(b_1995_2004 = k[["a"]], se_1995_2004 = se(m)[["a"]], b_2005_2010 = k[["b"]], se_2005_2010 = se(m)[["b"]],
             b_2019_2025 = k[["c"]], se_2019_2025 = se(m)[["c"]], p_2019_2025 = pvalue(m)[["c"]],
             na_min_1995 = ca[1], p_na_min_1995 = ca[2], na_min_2005 = cb[1], p_na_min_2005 = cb[2], n = nobs(m))
}
## een vergelijking: welke woningen, welke groepen (G = 1 tegenover G = 0), en de eenheid van de vaste effecten
toets <- function(naam, dt, g1, g0, eenheid) {
  x <- copy(dt[eval(g1) | eval(g0)])
  x[, G := as.integer(eval(g1))]
  x[, `:=`(E = eval(eenheid), gem = gemeente_code)]
  x <- x[!is.na(E)]
  p <- paneel_bj(x)
  p[x[, .(gem = gem[1L]), by = E], on = "E", gem := i.gem]
  cbind(toets = naam, vier(p))
}
Q4 <- quote(kw %in% 4L); Q1 <- quote(kw %in% 1L)
GM <- quote(gemeente_code); GMD <- quote(fifelse(is.na(dichtheid), NA_character_, paste(gemeente_code, dichtheid)))
R$bouwjaar <- rbindlist(list(
  toets("zwaarst tegenover lichtst, binnen de gemeente", e, Q4, Q1, GM),
  toets("idem, binnen gemeente en dichtheid", e, Q4, Q1, GMD),
  toets("kwartiel 2 tegenover 1, binnen gemeente en dichtheid", e, quote(kw %in% 2L), Q1, GMD),
  toets("kwartiel 3 tegenover 1, binnen gemeente en dichtheid", e, quote(kw %in% 3L), Q1, GMD),
  toets("grondgebonden, binnen de gemeente", e[grond == TRUE], Q4, Q1, GM),
  toets("grondgebonden, binnen gemeente en dichtheid", e[grond == TRUE], Q4, Q1, GMD),
  toets("appartementen, binnen gemeente en dichtheid", e[app == TRUE], Q4, Q1, GMD),
  toets("vijf provincies, binnen de gemeente", e[vroeg >= 0], Q4, Q1, GM),
  toets("vijf provincies, binnen de vroege contour", e[vroeg == 1], Q4, Q1, GM),
  toets("vijf provincies, buiten de vroege contour", e[vroeg == 0], Q4, Q1, GM),
  ## stikstofruimte: binnen 2,5 km van overbelaste natuur tegenover verder, binnen gemeente en depositiekwartiel
  toets("overbelaste natuur binnen 2,5 km tegenover verder, binnen gemeente en depositiekwartiel", e[!is.na(kw) & ovb >= 0],
        quote(ovb <= 2500), quote(ovb > 2500), quote(paste(gemeente_code, kw))),
  toets("idem, binnen gemeente, depositiekwartiel en dichtheid", e[!is.na(kw) & ovb >= 0],
        quote(ovb <= 2500), quote(ovb > 2500), quote(fifelse(is.na(dichtheid), NA_character_, paste(gemeente_code, kw, dichtheid))))
), fill = TRUE)
R$bouwjaar[, `:=`(pct_2019_2025 = 100 * (exp(b_2019_2025) - 1), pct_na_min_1995 = 100 * (exp(na_min_1995) - 1))]
print(R$bouwjaar[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## =====================================================================
## Dichtheid voor de objecten van het rapport (zoals 21_verdichting.R)
## =====================================================================
d <- read_derived("vbo_analyse")
d[, vbo_id := as.character(vbo_bag_nr)]
d[, vbo_id := paste0(strrep("0", 16L - nchar(vbo_id)), vbo_id)]
e1 <- alle[redev_type %in% c("SN_Nieuwbouw", "Nieuwbouw", "Toevoeging", "Transformatie_Plus")]
setorder(e1, vbo_id, -redev_yearmonth); e1 <- e1[!duplicated(vbo_id)]
d[e1, on = "vbo_id", oad_obj := i.oad]
sleutel <- function(x, y, m) (x %/% m) * 1e5 + (y %/% m)
for (m in c(100, 500, 2000)) {
  cel <- alle[!is.na(oad) & !is.na(x), .(oad_cel = as.numeric(median(oad))), by = .(k = sleutel(x, y, m))]
  d[, k := sleutel(x, y, m)]
  d[cel, on = "k", (paste0("oad_", m)) := i.oad_cel]
}
d[, k := NULL]
d[, dichtheid := klasse(fcoalesce(as.numeric(oad_obj), oad_100, oad_500, oad_2000))]
rs <- merge(fread(file.path(PATHS$derived, "vbo_xy.csv"), sep = ";"),
            fread(file.path(PATHS$derived, "vbo_raster.csv"), sep = ";")[, .(id, ndep_2018, stedelijk)], by = "id")
d <- merge(d, rs[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
rm(alle, e1); invisible(gc())
verg <- d[in_sample == TRUE & !is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]

## =====================================================================
## A2. Vergunningen en opleveringen per kwartaal, per depositiekwartiel
##     (zoals 11_ruimte.R, deel C), binnen de gemeente en binnen gemeente
##     en dichtheid: DiD na 2019Q2 en de trendbreuk tot en met 2024Q3
## =====================================================================
kop("A2. Depositiekwartielen per kwartaal: binnen de gemeente en binnen gemeente en dichtheid")
paneel_kw <- function(dt, eenheid) {
  dt <- dt[!is.na(gemeentecode) & ndep_2018 > 0]
  dt[, `:=`(G = findInterval(ndep_2018, gr), E = eval(eenheid))]
  dt <- dt[!is.na(E)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(G = G[1L], E = E[1L], gem = gemeentecode[1L], n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[pp[, .(gem = gem[1L]), by = E], on = "E", gem := i.gem]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L), post = as.integer(t >= Q_PAS))]
  p[]
}
EENH <- list("binnen de gemeente" = quote(as.character(gemeentecode)),
             "binnen gemeente en dichtheid" = quote(fifelse(is.na(dichtheid), NA_character_, paste(gemeentecode, dichtheid))))
R$kwartaal <- rbindlist(lapply(c("vergunningen", "nieuwbouw"), function(u) rbindlist(lapply(names(EENH), function(en) {
  p <- paneel_kw(copy(if (u == "vergunningen") verg else nwb), EENH[[en]])
  m <- fepois(y ~ i(G, post, ref = 0) | E_G + E_t + G_szn, data = p, cluster = ~gem, notes = FALSE, glm.iter = 200)
  did <- data.table(uitkomst = u, eenheid = en, kwartiel = paste0("Q", 2:4), deel = "na 2019Q2", b = coef(m), se = se(m), p = pvalue(m))
  q2 <- copy(p[t <= Q_SAL - 1L]); q2[, tj := (t - Q_PAS) / 4]
  for (g in 1:3) q2[, (paste0(c("voortrend_", "sprong_", "natrend_"), g)) :=
                      list(as.integer(G == g) * tj, as.integer(G == g) * post, as.integer(G == g) * post * tj)]
  v <- paste(c(paste0("voortrend_", 1:3), paste0("sprong_", 1:3), paste0("natrend_", 1:3)), collapse = " + ")
  mt <- fepois(as.formula(sprintf("y ~ %s | E_G + E_t + G_szn", v)), data = q2, cluster = ~gem, notes = FALSE, glm.iter = 200)
  tb <- data.table(term = names(coef(mt)), b = coef(mt), se = se(mt), p = pvalue(mt))
  tb[, `:=`(uitkomst = u, eenheid = en, kwartiel = paste0("Q", as.integer(sub(".*_", "", term)) + 1L), deel = sub("_.*", "", term))]
  rbind(did, tb[, .(uitkomst, eenheid, kwartiel, deel, b, se, p)])
}))))
print(R$kwartaal[kwartiel == "Q4", lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## =====================================================================
## B. De stap bij de Omgevingswet voor appartementen in de binnenste
##    kilometer (zoals 13_binnenring.R, sectie 4), binnen de gemeente,
##    binnen gemeente en dichtheid, en binnen gemeente en contour
## =====================================================================
kop("B. Appartementen in de binnenste kilometer: de stap bij de Omgevingswet, binnen gemeente en dichtheid")
per_pand <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
verg[per_pand, on = "pand_bag_nr", n_pand := i.n_pand]
verg[, type := fifelse(n_pand >= 3L, "meergezins", "eengezins")]
ring_van <- function(k) fifelse(k <= 4L, 1L, fifelse(k <= 10L, 2L, fifelse(k <= 20L, 3L, 0L)))
paneel_ring <- function(dt, eenheid) {
  dt <- dt[!is.na(gemeentecode)]
  dt[, E := eval(eenheid)]
  dt <- dt[!is.na(E)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(k = min(afstandsklasse), E = E[1L], gem = gemeentecode[1L], n = .N), by = .(projectid, q)]
  pp[, ring := ring_van(k)]
  cel <- pp[, .(y = sum(n)), by = .(E, ring, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), ring = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "ring", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[pp[, .(gem = gem[1L]), by = E], on = "E", gem := i.gem]
  p[, `:=`(E_r = paste(E, ring), E_t = paste(E, t), r_szn = paste(ring, (t - 1L) %% 4L))]
  p[]
}
EENH_B <- c(EENH, list("binnen gemeente en huidige contour" = quote(fifelse(is.na(stedelijk) | stedelijk < 0, NA_character_, paste(gemeentecode, stedelijk)))))
R$appartementen <- rbindlist(lapply(c("meergezins", "eengezins"), function(ty) rbindlist(lapply(names(EENH_B), function(en) {
  p <- paneel_ring(copy(verg[type == ty]), EENH_B[[en]])[t >= Q_PAS]
  p[, `:=`(ow = as.integer(t >= Q_OW), sal = as.integer(t >= Q_SAL))]
  m <- tryCatch(fepois(y ~ i(ring, ow, ref = 0) + i(ring, sal, ref = 0) | E_r + E_t + r_szn, data = p, cluster = ~gem,
                       notes = FALSE, glm.iter = 200), error = function(x) NULL)
  if (is.null(m)) return(NULL)
  ct <- coeftable(m)
  data.table(woningtype = ty, eenheid = en, stap = c("Omgevingswet", "intern salderen"),
             b = ct[c("ring::1:ow", "ring::1:sal"), 1], se = ct[c("ring::1:ow", "ring::1:sal"), 2], p = ct[c("ring::1:ow", "ring::1:sal"), 4], n = nobs(m))
}))))
R$appartementen[, pct := 100 * (exp(b) - 1)]
print(R$appartementen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
## de ruwe aantallen: vergunde appartementen per jaar en ring, en per dichtheidsklasse
R$app_jaar <- verg[type == "meergezins" & q >= kw_index(as.Date("2021-01-01")),
                   .N, by = .(jaar = as.integer(format(vergunning_datum, "%Y")),
                              ring = fifelse(dist2n2000_m <= 1000, "0 tot 1 km", fifelse(dist2n2000_m <= 5000, "1 tot 5 km", "verder dan 5 km")),
                              dichtheid)][order(jaar, ring, dichtheid)]
print(dcast(R$app_jaar, dichtheid + ring ~ jaar, value.var = "N", fill = 0))

writeLines(to_json(R), file.path(PATHS$output, sprintf("samenstelling_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("samenstelling_%s.json", EXTRACT)))
sink()
