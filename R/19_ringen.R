## =====================================================================
## 19_ringen.R -- waarom groeide de ring van 2,5 tot 5 kilometer, en hoe
##                hangt de achterstand nabij natuur daarvan af?
##
## Input : _derived/vbo_analyse.rds, _derived/vbo_xy.csv en vbo_raster.csv
##         (11_ruimte.R), _derived/vbo_kern.csv (16a_kernen.py),
##         _derived/gemeentenamen.csv (hier gemaakt uit de buurtkaart)
## Output: _output/ringen_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 8 vindt dat de ringen tot 1 en van 1 tot 2,5 kilometer bij
## opleveringen binnen de gemeente niet lager liggen dan het gebied verder
## dan 5 kilometer, en dat de achterstand op de drempel van 2.500 meter komt
## van de ring van 2,5 tot 5 kilometer, die na 2019 meer opleverde; 18_nnn.R
## vond hetzelfde binnen kern en gemeente. Dit script zoekt uit waar die
## ring groeide (projectomvang, woningtype, vroege contour, gemeenten) en of
## het beeld blijft staan zonder de grootste steden: zonder Amsterdam, zonder
## de vier grote steden en zonder de tien gemeenten met de meeste
## opleveringen. Zelfde schatter als 16_kernen.R: Poisson op eenheid x ring x
## kwartaal, vaste effecten eenheid x ring, eenheid x kwartaal en ring x
## seizoen, een term per ring vanaf 2019Q2 tegenover verder dan 5 km, en
## daarnaast nabij (<= 2.500 m) tegenover alles verderop.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("ringen_%s.txt", EXTRACT)), split = TRUE)
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

## gemeentenamen uit de buurtkaart van de Klimaateffectatlas (16a_kernen.py)
f_nm <- file.path(PATHS$derived, "gemeentenamen.csv")
if (!file.exists(f_nm)) {
  ogr <- Sys.getenv("QGIS_OGR2OGR", "C:/Program Files/QGIS 3.40.7/bin/ogr2ogr.exe")
  gpkg <- file.path(PATHS$verg, "bron", "waterbodem", "klimaateffectatlas", "kea_buurtdashboard_20260312.gpkg")
  sql <- "SELECT DISTINCT CAST(substr(gemeentecode, 3) AS integer) AS gemeentecode, gemeentenaam FROM buurtdashboard_20260312 ORDER BY 1"
  cmd <- tempfile(fileext = ".cmd")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO",
               sprintf('call "%s" -f CSV "%s" "%s" -sql "%s" -lco SEPARATOR=SEMICOLON', normalizePath(ogr),
                       normalizePath(f_nm, mustWork = FALSE), normalizePath(gpkg), sql), "endlocal"), cmd)
  if (system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd"))) != 0) stop("gemeentenamen: ogr2ogr faalde")
}
namen <- fread(f_nm, sep = ";")

xy <- fread(file.path(PATHS$derived, "vbo_xy.csv"), sep = ";")
xy <- merge(merge(xy, fread(file.path(PATHS$derived, "vbo_raster.csv"), sep = ";")[, .(id, stedelijk_vroeg)], by = "id"),
            fread(file.path(PATHS$derived, "vbo_kern.csv"), sep = ";"), by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
vpp <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
d <- merge(d, vpp, by = "pand_bag_nr", all.x = TRUE)
d[, `:=`(meergezins = n_pand >= 3L,
         binnen_vroeg = fifelse(stedelijk_vroeg %in% c(0, 1), as.integer(stedelijk_vroeg), NA_integer_),
         kern_gm = paste(fifelse(kern > 0L, paste0("k", kern), paste0("b", gemeentecode)), gemeentecode))]

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)

## ringen: 1 = tot 1 km, 2 = 1 tot 2,5 km, 3 = 2,5 tot 5 km, 0 = verder
RING  <- quote(fifelse(dist2n2000_m <= 1000, 1L, fifelse(dist2n2000_m <= 2500, 2L, fifelse(dist2n2000_m <= 5000, 3L, 0L))))
NABIJ <- quote(as.integer(dist2n2000_m <= 2500))
RINGLAB <- c(tr1 = "0 tot 1 km", tr2 = "1 tot 2,5 km", tr3 = "2,5 tot 5 km")

## ---------------------------------------------------------------------
## Panelen en schatter (als in 16_kernen.R en 18_nnn.R)
## ---------------------------------------------------------------------
paneel <- function(dt, eenheid, groep_expr, ng) {
  dt <- dt[!is.na(gemeentecode) & !is.na(kern)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(E = get(eenheid)[1L], G = eval(groep_expr)[1L], n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:(ng - 1L), t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L))]
  for (g in 1:(ng - 1L)) p[, (paste0("tr", g)) := as.integer(G == g) * as.integer(t >= Q_PAS)]
  p[]
}
did <- function(p, ng) {
  f <- as.formula(sprintf("y ~ %s | E_G + E_t + G_szn", paste0("tr", 1:(ng - 1L), collapse = " + ")))
  m <- fepois(f, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m)
  data.table(term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m), geconvergeerd = m$convStatus)
}

## de tien gemeenten met de meeste opleveringen in de hele reeks
groot <- merge(nwb[!is.na(gemeentecode), .(opgeleverd = .N), by = gemeentecode], namen, by = "gemeentecode", all.x = TRUE)[order(-opgeleverd)]
R$grootste <- head(groot, 10L)
WEG <- list("geen" = integer(0), "Amsterdam" = 363L, "de vier grote steden" = c(363L, 599L, 518L, 344L),
            "de tien grootste gemeenten" = head(groot$gemeentecode, 10L))
kop("De tien gemeenten met de meeste opleveringen 2012 tot en met 2026Q2")
print(R$grootste)

## ---------------------------------------------------------------------
## 1. Ringen en nabij, met en zonder de grootste steden
## ---------------------------------------------------------------------
kop("1. Ringen tegenover verder dan 5 km en nabij tegenover verderop, na 2019Q2, zonder de grootste steden")
R$zonder <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(c("alle", "20 of meer"), function(om) {
  rbindlist(lapply(names(WEG), function(w) rbindlist(lapply(c("gm", "kern_gm"), function(e) {
    dt <- UIT[[u]][!gemeentecode %in% WEG[[w]]]
    if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
    dt <- copy(dt)[, gm := as.character(gemeentecode)]
    r <- rbind(did(paneel(dt, e, RING, 4L), 4L)[, groep := RINGLAB[term]],
               did(paneel(dt, e, NABIJ, 2L), 2L)[, groep := "nabij tegenover verderop"])
    cbind(uitkomst = u, projecten = om, zonder = w, vergelijking = fifelse(e == "gm", "binnen de gemeente", "binnen kern x gemeente"), r)
  }))))
}))))
R$zonder[, pct := 100 * (exp(b) - 1)]
print(dcast(R$zonder, uitkomst + projecten + vergelijking + zonder ~ groep, value.var = "pct")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
print(dcast(R$zonder, uitkomst + projecten + vergelijking + zonder ~ groep, value.var = "p")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 1b. Tweede route: de ring per object in plaats van per project. De
##     rapportconventie legt een project in de ring van zijn dichtstbijzijnde
##     object; per object telt elk object in zijn eigen ring
## ---------------------------------------------------------------------
kop("1b. Tweede route: ring per object, opleveringen binnen de gemeente")
paneel_object <- function(dt) {
  dt <- dt[!is.na(gemeentecode) & !is.na(kern)]
  cel <- dt[, .(y = .N), by = .(E = as.character(gemeentecode), G = eval(RING), t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L))]
  for (g in 1:3) p[, (paste0("tr", g)) := as.integer(G == g) * as.integer(t >= Q_PAS)]
  p[]
}
R$objectniveau <- rbindlist(lapply(c("alle", "20 of meer"), function(om) rbindlist(lapply(names(WEG), function(w) {
  dt <- nwb[!gemeentecode %in% WEG[[w]]]
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  cbind(uitkomst = "nieuwbouw", projecten = om, zonder = w, did(paneel_object(dt), 4L)[, groep := RINGLAB[term]])
}))))
R$objectniveau[, pct := 100 * (exp(b) - 1)]
print(dcast(R$objectniveau, projecten + zonder ~ groep, value.var = c("pct", "p"))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 1c. Trendbreuk nabij (<= 2.500 m) tegenover alles verderop, zoals in
##     hoofdstuk 8 (08_strikt.R): voortrend, sprong bij de uitspraak en
##     verandering van de trend, tot en met 2024Q3
## ---------------------------------------------------------------------
kop("1c. Trendbreuk nabij tegenover verderop, opleveringen, met en zonder de grootste steden")
Q_TOT <- kw_index(as.Date("2024-09-30"))
R$trendbreuk <- rbindlist(lapply(c("alle", "20 of meer"), function(om) rbindlist(lapply(names(WEG), function(w) {
  dt <- copy(nwb[!gemeentecode %in% WEG[[w]]])[, gm := as.character(gemeentecode)]
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  p <- paneel(dt, "gm", NABIJ, 2L)[t <= Q_TOT]
  p[, tj := (t - Q_PAS) / 4]
  p[, `:=`(voortrend = G * tj, sprong = tr1, natrend = tr1 * tj)]
  m <- fepois(y ~ voortrend + sprong + natrend | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m)
  data.table(uitkomst = "nieuwbouw", projecten = om, zonder = w, term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4])
}))))
R$trendbreuk[, pct := 100 * (exp(b) - 1)]
print(R$trendbreuk[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 1d. Amsterdam: opleveringen per ring, en het ringenmodel met een eigen
##     term voor de ring van 2,5 tot 5 km in Amsterdam. Eén gedeelde term per
##     ring moet anders ook de verdrievoudiging daar dragen, en via gemeente x
##     kwartaal verschuiven dan ook de binnenste ringen
## ---------------------------------------------------------------------
kop("1d. Amsterdam: opleveringen per ring, en een eigen term voor de ring van 2,5 tot 5 km")
R$amsterdam <- nwb[gemeentecode == 363L, .N, by = .(ring = eval(RING), na = q >= Q_PAS)][order(ring, na)]
print(R$amsterdam)
R$eigen_term <- rbindlist(lapply(c("alle", "20 of meer"), function(om) {
  dt <- copy(nwb)[, gm := as.character(gemeentecode)]
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  p <- paneel(dt, "gm", RING, 4L)
  p[, tr3_ams := tr3 * as.integer(E == "363")]
  m <- fepois(y ~ tr1 + tr2 + tr3 + tr3_ams | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
  ct <- coeftable(m)
  data.table(uitkomst = "nieuwbouw", projecten = om, term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4])
}))
R$eigen_term[, pct := 100 * (exp(b) - 1)]
print(R$eigen_term[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. Waar groeide de ring van 2,5 tot 5 km: naar projectomvang, woningtype
##    en vroege contour, binnen de gemeente
## ---------------------------------------------------------------------
kop("2. Ringen binnen de gemeente per deelverzameling, opleveringen en vergunningen")
DELEN <- list("alle" = quote(rep(TRUE, .N)), "1 tot 19 objecten" = quote(projectomvang < 20L),
              "20 tot 199 objecten" = quote(projectomvang >= 20L & projectomvang < 200L),
              "200 objecten en meer" = quote(projectomvang >= 200L),
              "eengezins" = quote(meergezins == FALSE), "meergezins" = quote(meergezins == TRUE),
              "binnen de vroege contour" = quote(binnen_vroeg %in% 1L), "buiten de vroege contour" = quote(binnen_vroeg %in% 0L))
R$delen <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(names(DELEN), function(dn) rbindlist(lapply(c("geen", "Amsterdam"), function(w) {
  if (u == "vergunningen" && w == "Amsterdam") return(NULL)
  dt <- copy(UIT[[u]][!gemeentecode %in% WEG[[w]]][eval(DELEN[[dn]])])[, gm := as.character(gemeentecode)]
  cbind(uitkomst = u, deel = dn, zonder = w, did(paneel(dt, "gm", RING, 4L), 4L)[, groep := RINGLAB[term]])
}))))))
R$delen[, pct := 100 * (exp(b) - 1)]
print(dcast(R$delen, uitkomst + zonder + deel ~ groep, value.var = c("pct", "p"))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 3. Beschrijvend: opleveringen per ring voor en na 2019Q2 (elk 29
##    kwartalen), in projecten van 200 en meer, en per gemeente de groei
##    van de ring van 2,5 tot 5 km in die projecten
## ---------------------------------------------------------------------
kop("3. Opleveringen per ring voor en na 2019Q2, en de gemeenten achter de groei van de ring van 2,5 tot 5 km")
n <- nwb[!is.na(gemeentecode)][, `:=`(ring = eval(RING), na = q >= Q_PAS)]
R$aandelen <- n[, .(voor = sum(!na), na = sum(na), groot_voor = sum(!na & projectomvang >= 200L), groot_na = sum(na & projectomvang >= 200L)), by = ring][order(ring)]
R$aandelen[, `:=`(aandeel_voor = voor / sum(voor), aandeel_na = na / sum(na))]
print(R$aandelen)
b <- n[ring == 3L & projectomvang >= 200L, .(voor = sum(!na), na = sum(na)), by = gemeentecode]
b[, groei := na - voor]
b <- merge(b, namen, by = "gemeentecode", all.x = TRUE)[order(-groei)]
b[, aandeel_groei := groei / sum(groei)]
R$ring3_groot <- head(b, 10L)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               ring3_groot_voor = sum(b$voor), ring3_groot_na = sum(b$na), gemeenten_ring3_groot = nrow(b))
print(R$ring3_groot)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("ringen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("ringen_%s.json", EXTRACT)))
sink()
