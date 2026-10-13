## =====================================================================
## 18_nnn.R -- verklaart het Natuurnetwerk Nederland de achterstand nabij
##             natuur binnen kernen?
##
## Input : _derived/vbo_analyse.rds, _derived/vbo_xy.csv (11_ruimte.R),
##         _derived/vbo_kern.csv en kern_hoofd.csv (16a_kernen.py),
##         _derived/vbo_nnn.csv (18a_nnn.py)
## Output: _output/nnn_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 15 van het rapport vindt dat de achterstand nabij natuur na
## 2019 binnen dezelfde kern en gemeente blijft staan (16_kernen.R). Twee
## kandidaten bleven over: de ruimte die aan de kant van de natuur opraakt,
## en regels voor bouwen bij natuur die ouder zijn dan 2019, zoals die van
## het Natuurnetwerk Nederland (NNN). Beide voorspellen een achterstand die
## niet aan stikstofgevoelig habitat vastzit maar aan de rand van natuur in
## het algemeen. Dit script toetst dat met drie vergelijkingen, alle met
## dezelfde schatter als 16_kernen.R (Poisson op eenheid x groep x kwartaal,
## vaste effecten eenheid x groep, eenheid x kwartaal en groep x seizoen,
## een term per groep vanaf 2019Q2, eenheid = gemeente of kern x gemeente):
##   1. placebo: objecten verder dan 2,5 km van habitat maar dicht bij NNN
##      dat geen Natura 2000 is, waar geen stikstoftoets geldt;
##   2. nabij habitat gesplitst naar NNN binnen 500 meter of niet: zit de
##      achterstand aan de natuurrand?
##   3. placebo: Natura 2000 zonder stikstofgevoelig habitat in de buurt
##      (verder dan 5 km van habitat, binnen 2,5 km van een Natura 2000-
##      gebied), met de ring van 2,5 tot 5 km apart.
## NNN uit de provinciale lagen; als controle de rijkslaag (CDDA) en de
## drempels 250 meter en 1 kilometer.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("nnn_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
d <- read_derived("vbo_analyse")
R <- list()

KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))

f_xy <- file.path(PATHS$derived, "vbo_xy.csv"); f_n <- file.path(PATHS$derived, "vbo_nnn.csv")
if (!file.exists(f_n)) {
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  arg <- paste(sprintf('"%s"', c(normalizePath(file.path(.here, "18a_nnn.py")), normalizePath(f_xy),
                                 normalizePath(f_n, mustWork = FALSE), normalizePath(PATHS$verg))), collapse = " ")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO", sprintf('call "%s" %s', normalizePath(qgis), arg), "endlocal"), cmd)
  if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("18a_nnn.py faalde")
}
xy <- merge(merge(fread(f_xy, sep = ";"), fread(f_n, sep = ";"), by = "id"),
            fread(file.path(PATHS$derived, "vbo_kern.csv"), sep = ";"), by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
hoofd <- fread(file.path(PATHS$derived, "kern_hoofd.csv"), sep = ";")[, .(gemeentecode, kern, is_hoofd = TRUE)]
d <- merge(d, hoofd, by = c("gemeentecode", "kern"), all.x = TRUE, sort = FALSE)
d[, kerntype := fifelse(is.na(kern), NA_character_, fifelse(kern == 0L, "buiten de contour",
                fifelse(is_hoofd %in% TRUE, "hoofdkern", "andere kern")))]
d[, kern_id := fifelse(kern > 0L, paste0("k", kern), paste0("b", gemeentecode))]
d[, kern_gm := paste(kern_id, gemeentecode)]

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               met_nnn = mean(!is.na(s$d_nnn)),
               ## aandelen van de steekproef (vergunde en opgeleverde objecten samen, zoals in_sample)
               nnn_x_500 = s[!is.na(d_nnn), mean(d_nnn_x <= 500)],
               nnn_x_500_ver = s[!is.na(d_nnn) & dist2n2000_m > 2500, mean(d_nnn_x <= 500)],
               nabij = s[!is.na(d_nnn), mean(dist2n2000_m <= 2500)],
               nabij_nnn_500 = s[!is.na(d_nnn) & dist2n2000_m <= 2500, mean(d_nnn <= 500)])

## ---------------------------------------------------------------------
## Panelen en schatter (als in 16_kernen.R, met meer groepen)
## ---------------------------------------------------------------------
paneel <- function(dt, eenheid, groep_expr, ng) {
  dt <- dt[!is.na(kerntype) & !is.na(gemeentecode) & !is.na(d_nnn)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(E = get(eenheid)[1L], G = eval(groep_expr)[1L], n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:(ng - 1L), t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L))]
  for (g in 1:(ng - 1L)) p[, (paste0("tr", g)) := as.integer(G == g) * as.integer(t >= Q_PAS)]
  p[]
}
## geeft de termen, en als attribuut "verschil" elk paar termen met het
## verschil, de standaardfout uit de geclusterde vcov en een Wald-toets
did <- function(p, ng) {
  f <- as.formula(sprintf("y ~ %s | E_G + E_t + G_szn", paste0("tr", 1:(ng - 1L), collapse = " + ")))
  m <- fepois(f, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m); V <- vcov(m)
  r <- data.table(term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m))
  if (nrow(ct) >= 2L) {
    pr <- t(combn(rownames(ct), 2L))
    v <- data.table(term_a = pr[, 1], term_b = pr[, 2])
    v[, verschil := ct[term_a, 1] - ct[term_b, 1]]
    v[, se := sqrt(V[cbind(term_a, term_a)] + V[cbind(term_b, term_b)] - 2 * V[cbind(term_a, term_b)])]
    v[, p := 2 * pnorm(-abs(verschil / se))]
    attr(r, "verschil") <- v
  }
  r
}

## de groepen; groep 0 is steeds de referentie
MODELLEN <- list(
  basis = list(ng = 2L, g = quote(as.integer(dist2n2000_m <= 2500)),
               lab = c("nabij habitat")),
  nnn = list(ng = 3L, g = quote(fifelse(dist2n2000_m <= 2500, 1L, fifelse(d_nnn_x <= 500, 2L, 0L))),
             lab = c("nabij habitat", "NNN zonder Natura 2000 binnen 500 m")),
  nnn_rijk = list(ng = 3L, g = quote(fifelse(dist2n2000_m <= 2500, 1L, fifelse(d_cdda_x <= 500, 2L, 0L))),
                  lab = c("nabij habitat", "NNN zonder Natura 2000 binnen 500 m, rijkslaag")),
  nnn_250_1000 = list(ng = 4L, g = quote(fifelse(dist2n2000_m <= 2500, 1L, fifelse(d_nnn_x <= 250, 2L, fifelse(d_nnn_x <= 1000, 3L, 0L)))),
                      lab = c("nabij habitat", "NNN zonder Natura 2000 binnen 250 m", "NNN zonder Natura 2000 op 250 m tot 1 km")),
  rand = list(ng = 4L, g = quote(fifelse(dist2n2000_m <= 2500, fifelse(d_nnn <= 500, 1L, 2L), fifelse(d_nnn_x <= 500, 3L, 0L))),
              lab = c("nabij habitat, NNN binnen 500 m", "nabij habitat, geen NNN binnen 500 m", "NNN zonder Natura 2000 binnen 500 m")),
  rand_1km = list(ng = 4L, g = quote(fifelse(dist2n2000_m <= 2500, fifelse(d_nnn <= 1000, 1L, 2L), fifelse(d_nnn_x <= 500, 3L, 0L))),
                  lab = c("nabij habitat, NNN binnen 1 km", "nabij habitat, geen NNN binnen 1 km", "NNN zonder Natura 2000 binnen 500 m")),
  n2000 = list(ng = 5L, g = quote(fifelse(dist2n2000_m <= 2500, 1L,
                                  fifelse(dist2n2000_m <= 5000, fifelse(d_n2000 <= 2500, 2L, 3L),
                                  fifelse(d_n2000 <= 2500, 4L, 0L)))),
               lab = c("nabij habitat", "2,5 tot 5 km van habitat, Natura 2000 binnen 2,5 km",
                       "2,5 tot 5 km van habitat, geen Natura 2000 binnen 2,5 km", "verder dan 5 km van habitat, Natura 2000 binnen 2,5 km"))
)

kop("Groepsgrootte in de steekproef (objecten)")
R$groepen <- rbindlist(lapply(names(MODELLEN), function(mn) {
  M <- MODELLEN[[mn]]
  s[!is.na(d_nnn), .(objecten = .N), by = .(G = eval(M$g))][order(G)][, `:=`(model = mn, groep = c("referentie", M$lab)[G + 1L])]
}))
print(R$groepen)

kop("Na 2019Q2 per groep tegenover de referentie, binnen de gemeente en binnen kern x gemeente")
res <- list(); vers <- list()
for (u in names(UIT)) for (om in c("alle", "20 of meer")) {
  dt <- copy(UIT[[u]])
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  dt[, gm := as.character(gemeentecode)]
  for (e in c("gm", "kern_gm")) for (mn in names(MODELLEN)) {
    M <- MODELLEN[[mn]]
    r <- did(paneel(dt, e, M$g, M$ng), M$ng)
    v <- attr(r, "verschil")
    id <- data.table(uitkomst = u, projecten = om, vergelijking = fifelse(e == "gm", "binnen de gemeente", "binnen kern x gemeente"), model = mn)
    r[, groep := M$lab[as.integer(sub("tr", "", term))]]
    res[[length(res) + 1L]] <- cbind(id, r)
    if (!is.null(v)) vers[[length(vers) + 1L]] <- cbind(id, v)
  }
}
R$modellen <- rbindlist(res)
R$modellen[, pct := 100 * (exp(b) - 1)]
print(R$modellen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 500)
## verschil tussen twee groepen in hetzelfde model: nabij habitat tegen NNN
## zonder Natura 2000 (nnn, tr1 - tr2), en nabij habitat zonder NNN binnen
## 500 m tegen met (rand, tr2 - tr1)
kop("Wald-toetsen op het verschil tussen groepen")
R$verschillen <- rbindlist(vers)
print(R$verschillen[model %in% c("nnn", "rand", "n2000"), lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 500)

## ---------------------------------------------------------------------
## Natura 2000 zonder habitat: waar zit het, en zonder de grootste gemeenten
## ---------------------------------------------------------------------
kop("Vergunningen verder dan 5 km van habitat en binnen 2,5 km van Natura 2000: gemeenten en zonder de grootste")
M <- MODELLEN$n2000
v <- copy(verg); v[, `:=`(G = eval(M$g), gm = as.character(gemeentecode))]
g4 <- v[G == 4L, .(voor = sum(q < Q_PAS), na = sum(q >= Q_PAS)), by = .(gemeentecode = gm)][order(-(voor + na))]
g4[, aandeel := (voor + na) / sum(voor + na)]
R$n2000_gemeenten <- head(g4, 10L)
R$meta$n2000_gemeenten <- nrow(g4)
print(R$n2000_gemeenten)
nb <- copy(nwb); nb[, `:=`(G = eval(M$g), gm = as.character(gemeentecode))]
R$n2000_zonder <- rbindlist(lapply(list("geen" = character(0), "363" = "363", "34" = "34", "363 en 34" = c("363", "34")), function(weg) {
  rbindlist(lapply(c("vergunningen", "nieuwbouw"), function(u) rbindlist(lapply(c("alle", "20 of meer"), function(om) {
    if (u == "nieuwbouw" && !(length(weg) == 0L || identical(weg, "363"))) return(NULL)   # opleveringen: alleen met alle gemeenten en zonder Amsterdam
    dt <- (if (u == "vergunningen") v else nb)[!gm %in% weg]
    if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
    rbindlist(lapply(c("gm", "kern_gm"), function(e) {
      r <- did(paneel(dt, e, M$g, M$ng), M$ng)[term %in% c("tr1", "tr3", "tr4")]
      cbind(uitkomst = u, zonder = paste(weg, collapse = " en "), projecten = om,
            vergelijking = fifelse(e == "gm", "binnen de gemeente", "binnen kern x gemeente"), r)
    }))
  }))))
}))
R$n2000_zonder[, pct := 100 * (exp(b) - 1)]
print(R$n2000_zonder[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("nnn_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("nnn_%s.json", EXTRACT)))
sink()
