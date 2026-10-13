## =====================================================================
## 21_verdichting.R -- is de achterstand nabij natuur een kant van de
##                     verdichting van de steden?
##
## Input : _derived/vbo_analyse.rds, _derived/vbo_xy.csv, vbo_kern.csv,
##         _derived/econlogic_vbo.rds (21a_econlogic.R)
## Output: _output/verdichting_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## De achterstand nabij natuur in de opleveringen (hoofdstuk 8 en 15) zit
## binnen gemeenten en kernen, ook binnen de oude contour, zonder knik bij
## de uitspraak en vooral in grote projecten. Dat past ook bij verdichting:
## vervanging en dichte appartementenbouw concentreren zich in dichte,
## centrale buurten, en die liggen gemiddeld verder van Natura 2000 dan de
## randen. Dit script toetst dat met de mutatietypering van het
## densification-paper (EconLogicPaper):
##   1. per ontwikkeltype: vervanging (nieuwbouw waar gesloopt is) en
##      nieuwbouw op een plek zonder eerdere bebouwing;
##   2. per dichtheidsklasse van de buurt: de omgevingsadressendichtheid
##      (OAD) van de CBS-buurt van 2012, hoog vanaf 1.500, midden 500 tot
##      1.500, laag onder 500 adressen per km2 (de klassen van het paper);
##   3. nabij tegenover ver binnen gemeente en dichtheidsklasse, en binnen
##      gemeente en ontwikkeltype: blijft de achterstand staan als alleen
##      plekken van dezelfde dichtheid of hetzelfde type vergeleken worden?
## Het ontwikkeltype is er alleen voor gerealiseerde objecten, dus alleen
## voor de opleveringen. De dichtheid is er voor elk object: via de
## koppeling, en anders de mediane OAD van de objecten in dezelfde cel van
## 100 meter (anders 500 meter, anders 2 kilometer).
## Schatter als 16_kernen.R: Poisson op eenheid x nabij x kwartaal, vaste
## effecten eenheid x nabij, eenheid x kwartaal en nabij x seizoen, een term
## vanaf 2019Q2, standaardfouten geclusterd op de eenheid.
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

sink(file.path(PATHS$output, sprintf("verdichting_%s.txt", EXTRACT)), split = TRUE)
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

## ---------------------------------------------------------------------
## De mutatietypering en de dichtheid aan de objecten
## ---------------------------------------------------------------------
alle <- readRDS(file.path(PATHS$derived, "econlogic_vbo.rds"))
klasse <- function(oad) fifelse(is.na(oad), NA_character_, fifelse(oad >= 1500, "hoog", fifelse(oad >= 500, "midden", "laag")))
PLUS <- c(SN_Nieuwbouw = "vervanging", Nieuwbouw = "nieuwbouw", Toevoeging = "toevoeging", Transformatie_Plus = "transformatie")
e <- alle[redev_type %in% names(PLUS)]
## een object met twee toevoegende mutaties: de laatste telt
setorder(e, vbo_id, -redev_yearmonth)
e <- e[!duplicated(vbo_id)]
e[, proces := PLUS[redev_type]]
d[, vbo_id := as.character(vbo_bag_nr)]
d[, vbo_id := paste0(strrep("0", 16L - nchar(vbo_id)), vbo_id)]
d <- merge(d, e[, .(vbo_id, proces, oad_obj = oad, x_el = x, y_el = y)], by = "vbo_id", all.x = TRUE, sort = FALSE)
kop("Koppeling: afstand tussen de coördinaten van beide bronnen bij gekoppelde objecten (m)")
print(d[!is.na(x_el), quantile(sqrt((x - x_el)^2 + (y - y_el)^2), c(.5, .9, .99, .999), na.rm = TRUE)])

## dichtheid via de cel voor objecten zonder koppeling (of zonder OAD)
sleutel <- function(x, y, m) (x %/% m) * 1e5 + (y %/% m)
for (m in c(100, 500, 2000)) {
  cel <- alle[!is.na(oad), .(oad_cel = as.numeric(median(oad))), by = .(k = sleutel(x, y, m))]
  d[, k := sleutel(x, y, m)]
  d[cel, on = "k", (paste0("oad_", m)) := i.oad_cel]
}
d[, k := NULL]
d[, `:=`(oad_bron = fcase(!is.na(oad_obj), "object", !is.na(oad_100), "cel 100 m", !is.na(oad_500), "cel 500 m",
                          !is.na(oad_2000), "cel 2 km", default = "geen"),
         oad = fcoalesce(as.numeric(oad_obj), oad_100, oad_500, oad_2000))]
d[, dichtheid := klasse(oad)]
rm(alle, e); invisible(gc())

## kernen, zoals in 16_kernen.R
xy <- merge(fread(file.path(PATHS$derived, "vbo_xy.csv"), sep = ";"), fread(file.path(PATHS$derived, "vbo_kern.csv"), sep = ";"), by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               n = lapply(UIT, nrow),
               gekoppeld = lapply(UIT, function(x) mean(!is.na(x$proces))),
               oad_bron = lapply(UIT, function(x) x[, .N, by = oad_bron][, aandeel := N / sum(N)][]))
kop("Koppeling aan de mutatietypering en herkomst van de dichtheid")
print(R$meta)

## ---------------------------------------------------------------------
## 1. Beschrijvend: ontwikkeltype en dichtheid, nabij en ver, voor en na
##    2019Q2 (29 en 29 kwartalen), en per jaar
## ---------------------------------------------------------------------
kop("1. Opleveringen naar ontwikkeltype en dichtheidsklasse, nabij en ver, voor en na 2019Q2")
n <- copy(nwb)[, `:=`(nabij = dist2n2000_m <= 2500, na = q >= Q_PAS)]
R$proces_aandelen <- n[!is.na(proces), .N, by = .(nabij, na, proces)][, aandeel := N / sum(N), by = .(nabij, na)][order(nabij, na, proces)]
R$dichtheid_aandelen <- n[!is.na(dichtheid), .N, by = .(nabij, na, dichtheid)][, aandeel := N / sum(N), by = .(nabij, na)][order(nabij, na, dichtheid)]
print(R$proces_aandelen)
print(R$dichtheid_aandelen)
## hoe ver van habitat ligt elke klasse: aandeel nabij per dichtheidsklasse
R$nabij_per_klasse <- n[!is.na(dichtheid), .(n = .N, aandeel_nabij = mean(nabij)), by = .(dichtheid, na)][order(dichtheid, na)]
print(R$nabij_per_klasse)
n[, jaar := 2012L + (q - 1L) %/% 4L]
R$jaarlijks <- n[!is.na(dichtheid) & jaar <= 2025L, .N, by = .(jaar, nabij, dichtheid)][order(jaar, nabij, dichtheid)]

## ---------------------------------------------------------------------
## Panelen en schatter
## ---------------------------------------------------------------------
paneel <- function(dt, eenheid) {
  dt <- dt[!is.na(gemeentecode)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(E = get(eenheid)[1L], G = as.integer(dist2n2000_m[1L] <= 2500), n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:1, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L), tr = G * as.integer(t >= Q_PAS))]
  p[]
}
did <- function(p) {
  m <- tryCatch(fepois(y ~ tr | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200), error = function(e) NULL)
  if (is.null(m) || !"tr" %in% names(coef(m))) return(data.table(b = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_, geconvergeerd = NA))
  ct <- coeftable(m)
  data.table(b = ct["tr", 1], se = ct["tr", 2], p = ct["tr", 4], n = nobs(m), geconvergeerd = m$convStatus)
}
maak <- function(dt, om) {
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  copy(dt)[, `:=`(gm = as.character(gemeentecode), gm_d = paste(gemeentecode, dichtheid), gm_p = paste(gemeentecode, proces),
                  gm_dp = paste(gemeentecode, dichtheid, proces))]
}
OMVANG <- c("alle", "20 of meer")

## ---------------------------------------------------------------------
## 2. Nabij tegenover ver binnen de gemeente, per deelverzameling
## ---------------------------------------------------------------------
kop("2. Nabij tegenover ver na 2019Q2 binnen de gemeente, per ontwikkeltype en per dichtheidsklasse")
DELEN <- list(nieuwbouw = list("alle" = quote(TRUE), "met ontwikkeltype" = quote(!is.na(proces)),
                               "vervanging" = quote(proces %in% "vervanging"), "nieuwbouw op een lege plek" = quote(proces %in% "nieuwbouw"),
                               "dichtheid hoog" = quote(dichtheid %in% "hoog"), "dichtheid midden" = quote(dichtheid %in% "midden"),
                               "dichtheid laag" = quote(dichtheid %in% "laag")),
              vergunningen = list("alle" = quote(TRUE), "dichtheid hoog" = quote(dichtheid %in% "hoog"),
                                  "dichtheid midden" = quote(dichtheid %in% "midden"), "dichtheid laag" = quote(dichtheid %in% "laag")))
R$delen <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(OMVANG, function(om) rbindlist(lapply(names(DELEN[[u]]), function(dn) {
  cbind(uitkomst = u, projecten = om, deel = dn, did(paneel(maak(UIT[[u]][eval(DELEN[[u]][[dn]])], om), "gm")))
}))))))
R$delen[, pct := 100 * (exp(b) - 1)]
print(R$delen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 200)

## ---------------------------------------------------------------------
## 3. Binnen gemeente x dichtheid, gemeente x type en gemeente x beide,
##    op dezelfde objecten als de vergelijking binnen de gemeente
## ---------------------------------------------------------------------
kop("3. Nabij tegenover ver na 2019Q2: binnen de gemeente en binnen gemeente x dichtheid of x type")
VERGELIJK <- list(nieuwbouw = c("binnen de gemeente" = "gm", "binnen gemeente x dichtheid" = "gm_d",
                                "binnen gemeente x type" = "gm_p", "binnen gemeente x dichtheid x type" = "gm_dp"),
                  vergunningen = c("binnen de gemeente" = "gm", "binnen gemeente x dichtheid" = "gm_d"))
R$binnen <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(OMVANG, function(om) {
  dt <- UIT[[u]][!is.na(dichtheid)]
  if (u == "nieuwbouw") dt <- dt[!is.na(proces)]
  dt <- maak(dt, om)
  v <- VERGELIJK[[u]]
  rbindlist(lapply(names(v), function(vn) cbind(uitkomst = u, projecten = om, vergelijking = vn, did(paneel(dt, v[[vn]])))))
}))))
R$binnen[, pct := 100 * (exp(b) - 1)]
print(R$binnen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("verdichting_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("verdichting_%s.json", EXTRACT)))
sink()
