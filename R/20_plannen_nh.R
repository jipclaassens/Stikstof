## =====================================================================
## 20_plannen_nh.R -- raakt de ruimte aan de kant van de natuur op?
##                    Plancapaciteit per ring in Noord-Holland
##
## Input : _derived/vbo_analyse.rds
##         _derived/plannen_nh.csv (20a_plannen_nh.py: de Noord-Hollandse
##         monitor plancapaciteit van juli 2023 en november 2025, met
##         Almere en Lelystad, zonder vertrouwelijke plannen)
## Output: _output/plannen_nh_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Een kandidaat voor de lagere oplevering nabij natuur was dat de ruimte
## aan die kant van de kernen opraakt. Dan hoort er nabij natuur ook minder
## plancapaciteit te liggen dan de bouw van voor 2019 zou doen verwachten.
## Alleen Noord-Holland heeft een openbare monitor met plannen op de kaart in
## twee standen (2023 en 2025). Per gemeente verdelen we de plancapaciteit
## over de ringen naar de verdeling van de opleveringen van 2012 tot en met
## 2019Q1, en zetten dat verwachte aantal nabij natuur naast het werkelijke;
## hetzelfde voor de opleveringen van 2019Q2 tot en met 2026Q2. Ligt de
## capaciteit nabij natuur verhoudingsgewijs even laag als de opleveringen
## erna, dan past dat bij ruimte die opraakt; ligt ze hoger, dan is er
## ruimte, maar wordt die minder gebruikt. Verkennend: een provincie, twee
## standen na 2019, en een monitor die gemeenten zelf vullen.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)

sink(file.path(PATHS$output, sprintf("plannen_nh_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()

f_pl <- file.path(PATHS$derived, "plannen_nh.csv")
if (!file.exists(f_pl)) {
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  arg <- paste(sprintf('"%s"', c(normalizePath(file.path(.here, "20a_plannen_nh.py")), normalizePath(f_pl, mustWork = FALSE),
                                 normalizePath(PATHS$verg))), collapse = " ")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO", sprintf('call "%s" %s', normalizePath(qgis), arg), "endlocal"), cmd)
  if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("20a_plannen_nh.py faalde")
}
pl <- fread(f_pl, sep = ";")
RINGEN <- c("0 tot 1 km", "1 tot 2,5 km", "2,5 tot 5 km", "verder dan 5 km")
ring_van <- function(a) fifelse(a <= 1000, RINGEN[1], fifelse(a <= 2500, RINGEN[2], fifelse(a <= 5000, RINGEN[3], RINGEN[4])))
pl <- pl[afstand_min >= 0 & totaal > 0]
pl[, ring := ring_van(afstand_min)]
GM <- sort(unique(pl$cbs_code))

## opleveringen in dezelfde gemeenten, per project op het dichtstbijzijnde object
d <- read_derived("vbo_analyse")
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
Q_PAS <- kw_index(as.Date("2019-05-29")); Q_MAX_NWB <- kw_index(as.Date("2026-06-30"))
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(mutatiemaand) & gemeentecode %in% GM]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
nwb[, afst_project := min(dist2n2000_m), by = projectid]
nwb[, `:=`(ring = ring_van(afst_project), periode = fifelse(q < Q_PAS, "voor", "na"))]
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), gemeenten = length(GM),
               plannen_2023 = pl[stand == 2023, .N], plannen_2025 = pl[stand == 2025, .N],
               woningen_2023 = pl[stand == 2023, sum(totaal)], woningen_2025 = pl[stand == 2025, sum(totaal)],
               opgeleverd_voor = nwb[periode == "voor", .N], opgeleverd_na = nwb[periode == "na", .N])

## ---------------------------------------------------------------------
## 1. Per ring: opleveringen voor en na, plancapaciteit 2023 en 2025
## ---------------------------------------------------------------------
kop("1. Opleveringen en plancapaciteit per ring, Noord-Holland met Almere en Lelystad")
per_ring <- function(weg) {
  o <- dcast(nwb[!gemeentecode %in% weg, .N, by = .(ring, periode)], ring ~ periode, value.var = "N", fill = 0)
  p <- dcast(pl[!cbs_code %in% weg, .(alle = sum(totaal), hard = sum(totaal * hard)), by = .(ring, stand)],
             ring ~ stand, value.var = c("alle", "hard"), fill = 0)
  r <- merge(o, p, by = "ring", all = TRUE)
  for (v in setdiff(names(r), "ring")) r[is.na(get(v)), (v) := 0]
  r[, names(r)[-1] := lapply(.SD, function(x) x / sum(x)), .SDcols = names(r)[-1]]   # aandelen per kolom
  r[order(match(ring, RINGEN))]
}
R$aandelen <- rbind(cbind(zonder = "geen", per_ring(integer(0))), cbind(zonder = "Amsterdam", per_ring(363L)))
print(R$aandelen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. Binnen de gemeente: werkelijk nabij tegenover verwacht uit de
##    verdeling van de opleveringen van voor 2019Q2
## ---------------------------------------------------------------------
kop("2. Nabij natuur (<= 2.500 m): werkelijk tegenover verwacht uit de opleveringen van 2012 tot en met 2019Q1, binnen de gemeente")
## per gemeente het aandeel nabij in de opleveringen van voor; alleen gemeenten met opleveringen voor.
## Een plan telt met zijn dichtstbijzijnde rand, dus grote plannen net binnen de drempel wegen zwaar;
## daarom naast 2.500 m ook de drempels 1.000, 1.500 en 2.000 m
afst_pl <- function(x) x$afstand_min
DREMPELS <- c(1000, 1500, 2000, 2500)
basis_d <- function(dr) nwb[periode == "voor", .(aandeel_nabij_voor = mean(afst_project <= dr), n_voor = .N), by = gemeentecode]
basis <- basis_d(2500)
verhouding <- function(tot, nabij, weg) {
  x <- merge(basis[!gemeentecode %in% weg], tot, by = "gemeentecode")
  x <- merge(x, nabij, by = "gemeentecode", all.x = TRUE); x[is.na(nabij), nabij := 0]
  list(werkelijk = sum(x$nabij), verwacht = sum(x$totaal * x$aandeel_nabij_voor), gemeenten = nrow(x))
}
R$verhoudingen <- rbindlist(lapply(list("geen" = integer(0), "Amsterdam" = 363L), function(weg) {
  rbindlist(list(
    c(maat = "opleveringen 2019Q2 tot en met 2026Q2",
      verhouding(nwb[periode == "na", .(totaal = .N), by = gemeentecode],
                 nwb[periode == "na" & ring %in% RINGEN[1:2], .(nabij = .N), by = gemeentecode], weg)),
    c(maat = "plancapaciteit juli 2023, alle plannen",
      verhouding(pl[stand == 2023, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)],
                 pl[stand == 2023 & ring %in% RINGEN[1:2], .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)], weg)),
    c(maat = "plancapaciteit juli 2023, harde plannen",
      verhouding(pl[stand == 2023 & hard == 1, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)],
                 pl[stand == 2023 & hard == 1 & ring %in% RINGEN[1:2], .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)], weg)),
    c(maat = "plancapaciteit november 2025, alle plannen",
      verhouding(pl[stand == 2025, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)],
                 pl[stand == 2025 & ring %in% RINGEN[1:2], .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)], weg)),
    c(maat = "plancapaciteit november 2025, harde plannen",
      verhouding(pl[stand == 2025 & hard == 1, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)],
                 pl[stand == 2025 & hard == 1 & ring %in% RINGEN[1:2], .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)], weg))
  ))[, zonder := names(which(sapply(list("geen" = integer(0), "Amsterdam" = 363L), identical, weg)))]
}))
R$verhoudingen[, `:=`(werkelijk = as.numeric(werkelijk), verwacht = as.numeric(verwacht), gemeenten = as.integer(gemeenten))]
R$verhoudingen[, ratio := werkelijk / verwacht]
print(R$verhoudingen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

kop("2b. Hetzelfde bij drempels van 1.000, 1.500, 2.000 en 2.500 m, met alle gemeenten")
R$drempels <- rbindlist(lapply(DREMPELS, function(dr) {
  b <- basis_d(dr)
  vh <- function(tot, nab) {
    x <- merge(merge(b, tot, by = "gemeentecode"), nab, by = "gemeentecode", all.x = TRUE); x[is.na(nabij), nabij := 0]
    data.table(werkelijk = sum(x$nabij), verwacht = sum(x$totaal * x$aandeel_nabij_voor))
  }
  rbindlist(list(
    cbind(drempel = dr, maat = "opleveringen 2019Q2 tot en met 2026Q2",
          vh(nwb[periode == "na", .(totaal = .N), by = gemeentecode], nwb[periode == "na" & afst_project <= dr, .(nabij = .N), by = gemeentecode])),
    cbind(drempel = dr, maat = "plancapaciteit juli 2023, alle plannen",
          vh(pl[stand == 2023, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)], pl[stand == 2023 & afstand_min <= dr, .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)])),
    cbind(drempel = dr, maat = "plancapaciteit juli 2023, harde plannen",
          vh(pl[stand == 2023 & hard == 1, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)], pl[stand == 2023 & hard == 1 & afstand_min <= dr, .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)])),
    cbind(drempel = dr, maat = "plancapaciteit november 2025, alle plannen",
          vh(pl[stand == 2025, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)], pl[stand == 2025 & afstand_min <= dr, .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)])),
    cbind(drempel = dr, maat = "plancapaciteit november 2025, harde plannen",
          vh(pl[stand == 2025 & hard == 1, .(totaal = sum(totaal)), by = .(gemeentecode = cbs_code)], pl[stand == 2025 & hard == 1 & afstand_min <= dr, .(nabij = sum(totaal)), by = .(gemeentecode = cbs_code)]))))
}))
R$drempels[, ratio := werkelijk / verwacht]
print(dcast(R$drempels, maat ~ drempel, value.var = "ratio")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
## het grootste plan in de band van 2.000 tot 2.500 m, als voorbeeld van het gewicht van zo'n rand
R$groot_in_rand <- pl[afstand_min > 2000 & afstand_min <= 2500][order(-totaal)][1:5, .(stand, gemeente, totaal, hard, afstand_min)]
print(R$groot_in_rand)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("plannen_nh_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("plannen_nh_%s.json", EXTRACT)))
sink()
