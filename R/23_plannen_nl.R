## =====================================================================
## 23_plannen_nl.R -- raakt de ruimte aan de kant van de natuur op?
##                    Landelijke plancapaciteit per ring
##
## Input : _derived/vbo_analyse.rds
##         _derived/plannen_nl.csv (22a_habitat.py plannen: de projecten van
##         de nationale woningbouwkaart van VRO, levering 18 augustus 2026,
##         peilmoment najaar 2025 of voorjaar 2026, met hun kleinste afstand
##         tot habitat en die van hun zwaartepunt)
## Output: _output/plannen_nl_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hetzelfde als 20_plannen_nh.R, nu voor heel Nederland: ligt er nabij
## natuur minder plancapaciteit dan de bouw van voor 2019 zou doen
## verwachten? Per gemeente verdelen we de netto plancapaciteit naar de
## verdeling van de opleveringen van 2012 tot en met 2019Q1 over nabij en
## ver, en zetten dat verwachte aantal nabij naast het werkelijke; hetzelfde
## voor de opleveringen van 2019Q2 tot en met 2026Q2. Een plan telt met zijn
## dichtstbijzijnde rand, zoals een project in de BAG met zijn dichtstbij-
## zijnde object; daarom ook de afstand van het zwaartepunt en drempels van
## 1.000 tot 2.500 meter. De marge komt uit een bootstrap over gemeenten (1.999 trekkingen).
## De kaart is een momentopname die gemeenten zelf vullen: verkennend.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)

sink(file.path(PATHS$output, sprintf("plannen_nl_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
set.seed(20260818)
R <- list()

f_pl <- file.path(PATHS$derived, "plannen_nl.csv")
if (!file.exists(f_pl)) {
  ## de projectvlakken van de kaart (RuimteScanner-levering, Beleid/Plancapaciteit/20260818), lokaal gekopieerd
  vro <- Sys.getenv("VRO_PROJECTEN", "C:/LocalData/Stikstof/plancapaciteit_vro_20260818/projecten-shapes.geojson")
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO",
               sprintf('call "%s" "%s" plannen "%s" "%s" "%s"', normalizePath(qgis), normalizePath(file.path(.here, "22a_habitat.py")),
                       normalizePath(vro), normalizePath(f_pl, mustWork = FALSE), normalizePath(PATHS$verg)),
               "set RC=%ERRORLEVEL%", "endlocal & exit /b %RC%"), cmd)
  if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("22a_habitat.py plannen faalde")
}
pl <- fread(f_pl, sep = ";", encoding = "UTF-8")
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), projecten = nrow(pl),
               zonder_netto = pl[is.na(netto_plancapaciteit) | netto_plancapaciteit <= 0, .N],
               dubbel = pl[, sum(duplicated(project_id))])
pl <- pl[!is.na(netto_plancapaciteit) & netto_plancapaciteit > 0 & afstand_min >= 0]
pl[, `:=`(gemeentecode = as.integer(sub("^GM", "", gemeente_code)), cap = as.numeric(netto_plancapaciteit),
          hard = as.integer(planstatus == "Hard"))]
pl[, vroeg := rowSums(.SD, na.rm = TRUE), .SDcols = "periode_2026_tm_2030"]
R$meta <- c(R$meta, list(met_netto = nrow(pl), woningen = pl[, sum(cap)], hard = pl[hard == 1L, sum(cap)],
                         gemeenten = uniqueN(pl$gemeentecode), peilmoment = pl[, .(n = .N, woningen = sum(cap)), by = peilmoment]))
kop("De kaart")
print(R$meta)

## opleveringen, per project op het dichtstbijzijnde object
d <- read_derived("vbo_analyse")
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
Q_PAS <- kw_index(as.Date("2019-05-29")); Q_MAX_NWB <- kw_index(as.Date("2026-06-30"))
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(mutatiemaand) & !is.na(gemeentecode)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
nwb[, afst_project := min(dist2n2000_m), by = projectid]
nwb[, periode := fifelse(q < Q_PAS, "voor", "na")]
rm(d); invisible(gc())

## ---------------------------------------------------------------------
## 1. Per ring: opleveringen voor en na, plancapaciteit (alle, hard, zacht)
## ---------------------------------------------------------------------
kop("1. Aandelen per ring: opleveringen en plancapaciteit")
RINGEN <- c("0 tot 1 km", "1 tot 2,5 km", "2,5 tot 5 km", "verder dan 5 km")
ring_van <- function(a) fifelse(a <= 1000, RINGEN[1], fifelse(a <= 2500, RINGEN[2], fifelse(a <= 5000, RINGEN[3], RINGEN[4])))
GM <- intersect(unique(pl$gemeentecode), unique(nwb[periode == "voor", gemeentecode]))
aandelen <- rbind(
  nwb[gemeentecode %in% GM, .(n = .N), by = .(maat = fifelse(periode == "voor", "opleveringen 2012 tot 2019Q1", "opleveringen 2019Q2 tot 2026Q2"), ring = ring_van(afst_project))],
  pl[gemeentecode %in% GM, .(n = sum(cap)), by = .(ring = ring_van(afstand_min))][, maat := "plancapaciteit, alle plannen"],
  pl[gemeentecode %in% GM & hard == 1L, .(n = sum(cap)), by = .(ring = ring_van(afstand_min))][, maat := "plancapaciteit, harde plannen"],
  pl[gemeentecode %in% GM & hard == 0L, .(n = sum(cap)), by = .(ring = ring_van(afstand_min))][, maat := "plancapaciteit, zachte plannen"],
  pl[gemeentecode %in% GM, .(n = sum(cap)), by = .(ring = ring_van(afstand_midden))][, maat := "plancapaciteit, zwaartepunt"], use.names = TRUE)
aandelen[, aandeel := n / sum(n), by = maat]
R$aandelen <- dcast(aandelen, maat ~ factor(ring, levels = RINGEN), value.var = "aandeel", fill = 0)
print(R$aandelen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. Binnen de gemeente: werkelijk nabij tegenover verwacht, per drempel,
##    met een bootstrap over gemeenten
## ---------------------------------------------------------------------
kop("2. Werkelijk nabij tegenover verwacht uit de opleveringen van 2012 tot en met 2019Q1, binnen de gemeente")
## de opleveringen op dezelfde gemeenten als de plannen, en ter vergelijking op alle gemeenten
MATEN <- list(
  "opleveringen 2019Q2 tot en met 2026Q2, gemeenten met plannen" = function(dr) nwb[periode == "na" & gemeentecode %in% GM, .(totaal = .N, nabij = sum(afst_project <= dr)), by = gemeentecode],
  "opleveringen 2019Q2 tot en met 2026Q2, alle gemeenten" = function(dr) nwb[periode == "na", .(totaal = .N, nabij = sum(afst_project <= dr)), by = gemeentecode],
  "plancapaciteit, alle plannen" = function(dr) pl[, .(totaal = sum(cap), nabij = sum(cap * (afstand_min <= dr))), by = gemeentecode],
  "plancapaciteit, harde plannen" = function(dr) pl[hard == 1L, .(totaal = sum(cap), nabij = sum(cap * (afstand_min <= dr))), by = gemeentecode],
  "plancapaciteit, zachte plannen" = function(dr) pl[hard == 0L, .(totaal = sum(cap), nabij = sum(cap * (afstand_min <= dr))), by = gemeentecode],
  "plancapaciteit, tot en met 2030" = function(dr) pl[, .(totaal = sum(vroeg), nabij = sum(vroeg * (afstand_min <= dr))), by = gemeentecode],
  "plancapaciteit, zwaartepunt" = function(dr) pl[, .(totaal = sum(cap), nabij = sum(cap * (afstand_midden <= dr))), by = gemeentecode])
WEG <- list("geen" = integer(0), "Amsterdam" = 363L, "vier grote steden" = c(363L, 599L, 518L, 344L))
B <- 1999L
R$verhoudingen <- rbindlist(lapply(names(WEG), function(w) rbindlist(lapply(c(1000, 1500, 2000, 2500), function(dr) {
  basis <- nwb[periode == "voor" & !gemeentecode %in% WEG[[w]], .(aandeel_voor = mean(afst_project <= dr)), by = gemeentecode]
  rbindlist(lapply(names(MATEN), function(mn) {
    x <- merge(basis, MATEN[[mn]](dr), by = "gemeentecode")
    x <- x[totaal > 0]
    x[, verwacht := totaal * aandeel_voor]
    r <- x[, sum(nabij) / sum(verwacht)]
    set.seed(20260818)   # elke maat dezelfde reeks trekkingen, los van de volgorde van de maten
    bs <- replicate(B, { i <- sample.int(nrow(x), replace = TRUE); sum(x$nabij[i]) / sum(x$verwacht[i]) })
    data.table(zonder = w, drempel = dr, maat = mn, gemeenten = nrow(x), werkelijk = x[, sum(nabij)], verwacht = x[, sum(verwacht)],
               ratio = r, lo = quantile(bs, .025, names = FALSE), hi = quantile(bs, .975, names = FALSE))
  }))
}))))
print(R$verhoudingen[zonder == "geen", lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 100)
print(dcast(R$verhoudingen, zonder + maat ~ drempel, value.var = "ratio")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## de grootste plannen net binnen de drempel van 2,5 km
R$groot_in_rand <- pl[afstand_min > 2000 & afstand_min <= 2500][order(-cap)][1:8, .(gemeente_naam, cap, planstatus, ha, afstand_min, afstand_midden)]
print(R$groot_in_rand)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("plannen_nl_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("plannen_nl_%s.json", EXTRACT)))
sink()
