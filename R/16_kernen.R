## =====================================================================
## 16_kernen.R -- zit de achterstand nabij natuur in een verschuiving
##                van grote projecten tussen kernen?
##
## Input : _derived/vbo_analyse.rds, _derived/vbo_xy.csv (11_ruimte.R),
##         _derived/vbo_kern.csv (16a_kernen.py)
## Output: _output/kernen_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 8 vindt dat er na 2019 nabij natuur minder wordt opgeleverd
## dan verderop in dezelfde gemeente, zonder knik bij de uitspraak en vooral
## in projecten van twintig objecten en meer. Hoofdstuk 15 sloot uit dat
## het een verschuiving van uitleg naar binnenstedelijk is. De kandidaat die
## overbleef: grote projecten verschuiven binnen een gemeente van de kleinere
## kernen, vaak dichter bij natuur, naar de hoofdkern. Dit script toetst dat:
##   1. beschrijvend: waar liggen grote projecten, nabij en ver, per jaar;
##   2. binnen de gemeente: verliezen de andere kernen grote projecten aan de
##      hoofdkern na 2019 (groep = kerntype in plaats van nabij);
##   3. nabij tegenover ver binnen dezelfde kern (vaste effecten kern x nabij
##      en kern x kwartaal; buiten de contour telt per gemeente als een kern)
##      en binnen gemeente x kerntype. Verdwijnt de achterstand daar, dan zit
##      hij tussen kernen; blijft hij, dan zit hij in de kernen zelf.
## Kern = verbonden stuk verstedelijkingscontour (16a_kernen.py); hoofdkern =
## de kern met de meeste inwoners die bij de gemeente hoort.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("kernen_%s.txt", EXTRACT)), split = TRUE)
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

f_xy <- file.path(PATHS$derived, "vbo_xy.csv"); f_k <- file.path(PATHS$derived, "vbo_kern.csv")
if (!file.exists(f_k)) {
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  arg <- paste(sprintf('"%s"', c(normalizePath(file.path(.here, "16a_kernen.py")), normalizePath(f_xy),
                                 normalizePath(f_k, mustWork = FALSE), normalizePath(PATHS$verg))), collapse = " ")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO", sprintf('call "%s" %s', normalizePath(qgis), arg), "endlocal"), cmd)
  if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("16a_kernen.py faalde")
}
xy <- merge(fread(f_xy, sep = ";"), fread(f_k, sep = ";"), by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
## hoofdkern per gemeente: de kern met de meeste eigen inwoners (16a_kernen.py)
hoofd <- fread(file.path(PATHS$derived, "kern_hoofd.csv"), sep = ";")[, .(gemeentecode, kern, is_hoofd = TRUE)]
d <- merge(d, hoofd, by = c("gemeentecode", "kern"), all.x = TRUE, sort = FALSE)
d[, kerntype := fifelse(is.na(kern), NA_character_, fifelse(kern == 0L, "buiten de contour",
                fifelse(is_hoofd %in% TRUE, "hoofdkern", "andere kern")))]
## een kern als eenheid; buiten de contour telt per gemeente als een kern. Een
## kern kan over gemeentegrenzen lopen (een stad en haar buurgemeenten), dus
## ook kern x gemeente als eenheid
d[, kern_id := fifelse(kern > 0L, paste0("k", kern), paste0("b", gemeentecode))]
d[, kern_gm := paste(kern_id, gemeentecode)]

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               kernen_met_objecten = uniqueN(d[kern > 0, kern]), met_kern = lapply(UIT, function(x) mean(!is.na(x$kerntype))))
km <- readLines(file.path(PATHS$derived, "kernen_meta.json"), warn = FALSE)
for (veld in c("kernen", "inwoners_in_kernen", "gemeenten", "gemeenten_met_hoofdkern", "kernen_die_hoofdkern_zijn")) {
  R$meta[[veld]] <- as.numeric(sub(sprintf('.*"%s": ([0-9.]+).*', veld), "\\1", paste(km, collapse = "")))
}
## aandeel van de objecten in grote projecten met een kern die over meer gemeenten loopt
R$meta$groot_in_meergemeentekern <- UIT$nieuwbouw[projectomvang >= 20L & kern > 0L,
  mean(kern %in% d[kern > 0L, .(g = uniqueN(gemeentecode)), by = kern][g > 1L, kern])]

## ---------------------------------------------------------------------
## 1. Beschrijvend: waar liggen de objecten van grote projecten
## ---------------------------------------------------------------------
kop("1. Opgeleverde objecten van projecten van 20 of meer, naar kerntype, nabij (<= 2.500 m) en ver, per jaar")
R$aandelen <- rbindlist(lapply(names(UIT), function(u) {
  UIT[[u]][!is.na(kerntype), .(n = .N), by = .(jaar = KW_START + (q - 1L) %/% 4L, nabij = as.integer(dist2n2000_m <= 2500),
                                            groot = as.integer(projectomvang >= 20L), kerntype)][, uitkomst := u]
}))
R$aandelen[, aandeel := n / sum(n), by = .(uitkomst, jaar, nabij, groot)]
print(dcast(R$aandelen[uitkomst == "nieuwbouw" & groot == 1L & jaar <= 2025], jaar ~ nabij + kerntype, value.var = "aandeel")[, lapply(.SD, function(x) round(x, 3))])

## ---------------------------------------------------------------------
## Panelen en schatters
## ---------------------------------------------------------------------
## eenheid x groep x kwartaal; per project en kwartaal telt het object met de
## kleinste afstand (zoals D in 08_strikt.R)
paneel <- function(dt, eenheid, groep_expr) {
  dt <- dt[!is.na(kerntype) & !is.na(gemeentecode)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(E = get(eenheid)[1L], G = eval(groep_expr)[1L], n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:1, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L), tr = G * as.integer(t >= Q_PAS))]
  p[]
}
did <- function(p, cl = ~E) {
  m <- fepois(y ~ tr | E_G + E_t + G_szn, data = p, cluster = cl, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m)
  data.table(b = ct["tr", 1], se = ct["tr", 2], p = ct["tr", 4], n = nobs(m))
}
trend <- function(p) {
  p[, tj := (t - Q_PAS) / 4]
  p[, `:=`(voortrend = G * tj, sprong = tr, natrend = tr * tj)]
  m <- fepois(y ~ voortrend + sprong + natrend | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m)
  data.table(term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4])
}

## ---------------------------------------------------------------------
## 2. Verliezen de andere kernen grote projecten aan de hoofdkern?
## ---------------------------------------------------------------------
## groep 1 = andere kern, 0 = hoofdkern, binnen de gemeente; alleen gemeenten
## met een eigen hoofdkern; buiten de contour valt hier weg
kop("2. Andere kernen tegenover de hoofdkern, binnen de gemeente, na 2019Q2")
R$kerntype <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(c("alle", "20 of meer"), function(om) {
  dt <- UIT[[u]][kerntype %in% c("hoofdkern", "andere kern")]
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  dt[, gm := gemeentecode]
  p <- paneel(dt, "gm", quote(as.integer(kerntype == "andere kern")))
  cbind(uitkomst = u, projecten = om, did(p))
}))))
R$kerntype[, pct := 100 * (exp(b) - 1)]
print(R$kerntype[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 3. Nabij tegenover ver: binnen de gemeente, binnen gemeente x kerntype,
##    binnen de kern
## ---------------------------------------------------------------------
kop("3. Nabij (<= 2.500 m) tegenover ver na 2019Q2: binnen gemeente, gemeente x kerntype en kern")
R$nabij <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(c("alle", "20 of meer"), function(om) {
  dt <- copy(UIT[[u]])
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  dt[, `:=`(gm = as.character(gemeentecode), gm_type = paste(gemeentecode, kerntype))]
  g <- quote(as.integer(dist2n2000_m <= 2500))
  rbindlist(list(
    cbind(uitkomst = u, projecten = om, vergelijking = "binnen de gemeente", did(paneel(dt, "gm", g))),
    cbind(uitkomst = u, projecten = om, vergelijking = "binnen gemeente x kerntype", did(paneel(dt, "gm_type", g))),
    cbind(uitkomst = u, projecten = om, vergelijking = "binnen de kern", did(paneel(dt, "kern_id", g))),
    cbind(uitkomst = u, projecten = om, vergelijking = "binnen kern x gemeente", did(paneel(dt, "kern_gm", g)))
  ))
}))))
R$nabij[, pct := 100 * (exp(b) - 1)]
print(R$nabij[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

kop("3b. Trendbreuk, opleveringen van projecten van 20 of meer, binnen kern x gemeente")
dt <- nwb[projectomvang >= 20L]
R$trend_kern <- trend(paneel(dt, "kern_gm", quote(as.integer(dist2n2000_m <= 2500))))
print(R$trend_kern)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("kernen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("kernen_%s.json", EXTRACT)))
sink()
