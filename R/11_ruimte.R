## =====================================================================
## 11_ruimte.R -- drie ruimtelijke toetsen op de BAG-objecten:
##                verschuiving naar binnenstedelijk bouwen, netcongestie,
##                en stikstofdepositie als dosis
##
## Input : _derived/vbo_analyse.rds
##         _derived/vbo_raster.csv  (11a_rasters.py; wordt zo nodig hier
##                                   aangemaakt met de Python van QGIS)
## Output: _output/ruimte_<extract>.json, .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 8 liet zien dat de lagere oplevering nabij natuur binnen de
## gemeente al voor de PAS-uitspraak inzette en bij de uitspraak geen knik
## heeft. De voor de hand liggende verklaring is een verschuiving: minder
## uitleg aan de randen van de kernen, waar de natuur dichtbij is, en meer
## binnenstedelijk bouwen. Deel A toetst dat met de verstedelijkingscontour
## en met het woningtype. Deel B zet netcongestie op dezelfde manier naast
## de vergunningen. Deel C herhaalt de stikstoftoets met de depositie op de
## bouwlocatie als dosis in plaats van de afstand.
##
## Alle modellen: Poisson op cellen gemeente x groep x kwartaal, vaste
## effecten gemeente x groep, gemeente x kwartaal en groep x seizoen,
## standaardfouten geclusterd op gemeente; objecten geteld zoals in
## 08_strikt.R (per project en kwartaal de kleinste afstandsklasse).
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
sink(file.path(PATHS$output, sprintf("ruimte_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()

d <- read_derived("vbo_analyse")

## ---------------------------------------------------------------------
## Rasterwaarden per object
## ---------------------------------------------------------------------
f_xy <- sub("\\.rds$", ".csv", derived_path("vbo_xy"))
f_rs <- sub("vbo_xy", "vbo_raster", f_xy)
f_vroeg <- file.path(PATHS$derived, "stedelijk_vroeg_25m.tif")
## Python van QGIS (GDAL) in een eigen cmd-proces, tussen setlocal en
## endlocal: python-qgis zet PYTHONHOME en PYTHONPATH
qgis_py <- function(script, ...) {
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  arg <- paste(sprintf('"%s"', c(normalizePath(file.path(.here, script)), ...)), collapse = " ")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO", sprintf('call "%s" %s', normalizePath(qgis), arg), "endlocal"), cmd)
  status <- system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd")))
  if (status != 0) stop(script, " faalde; draai het met de Python van QGIS (zie de kop van dat script)")
}
if (!file.exists(f_vroeg)) qgis_py("11b_contour_vroeg.py", normalizePath(f_vroeg, mustWork = FALSE), normalizePath(PATHS$verg))
if (!file.exists(f_rs) || file.mtime(f_rs) < file.mtime(derived_path("vbo_analyse")) || file.mtime(f_rs) < file.mtime(f_vroeg)) {
  p <- unique(d[!is.na(x) & !is.na(y) & x > 1000 & y > 300000, .(x, y)])
  p[, id := .I]
  fwrite(p[, .(id, x, y)], f_xy, sep = ";")
  qgis_py("11a_rasters.py", normalizePath(f_xy), normalizePath(f_rs, mustWork = FALSE), normalizePath(PATHS$verg))
  if (!file.exists(f_rs)) stop("11a_rasters.py schreef geen uitvoer")
}
xy <- fread(f_xy, sep = ";"); rs <- fread(f_rs, sep = ";")
xy <- merge(xy, rs, by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), objecten = nrow(d),
               met_raster = sum(!is.na(d$stedelijk) & d$stedelijk >= 0))
cat(sprintf("objecten %s, met rasterwaarde %s\n", nfmt(nrow(d)), nfmt(R$meta$met_raster)))

## ---------------------------------------------------------------------
## Tijd, steekproeven, panelen: als 08_strikt.R
## ---------------------------------------------------------------------
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q_REF <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_SAL <- kw_index(as.Date("2024-12-18"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)

## woningtype uit het pand: meergezins als het pand drie of meer
## woonobjecten telt in het objectenbestand (alle objecten van een nieuw
## pand zijn nieuw, dus dit is het hele pand)
vbo_per_pand <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
for (u in names(UIT)) {
  UIT[[u]] <- merge(UIT[[u]], vbo_per_pand, by = "pand_bag_nr", all.x = TRUE)
  UIT[[u]][, `:=`(D = as.integer(afstandsklasse <= 2500 / PARAMS$klasse_breedte),
                  binnen = fifelse(stedelijk %in% c(0, 1), as.integer(stedelijk), NA_integer_),
                  binnen_vroeg = fifelse(stedelijk_vroeg %in% c(0, 1), as.integer(stedelijk_vroeg), NA_integer_),
                  meergezins = as.integer(n_pand >= 3L))]
}
R$meta$vroege_contour <- lapply(UIT, function(x) list(objecten = sum(!is.na(x$binnen_vroeg)), aandeel = mean(!is.na(x$binnen_vroeg))))

## Paneel gemeente x groep x kwartaal. `groep` is een expressie op de
## objecten; per project en kwartaal telt de groep van het object met de
## kleinste afstandsklasse, net als D in 08_strikt.R.
paneel <- function(dt, groep_expr, ngroep) {
  dt <- dt[!is.na(gemeentecode)]
  dt[, G := eval(groep_expr)]
  dt <- dt[!is.na(G)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(G = G[1L], n = .N), by = .(projectid, gemeentecode, q)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, G, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), G = 0:(ngroep - 1L), t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "G", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p[, `:=`(gem_G = paste(gemeentecode, G), gem_t = paste(gemeentecode, t), G_szn = paste(G, (t - 1L) %% 4L))]
  p[]
}
FE <- "gem_G + gem_t + G_szn"
coefrij <- function(m, term) {
  ct <- coeftable(m)
  data.table(term = term, b = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}
fit_post <- function(p, van, fe = FE) {
  ## groep 1 tegenover 0, na `van`
  p[, tr := as.integer(G == 1L) * as.integer(t >= van)]
  coefrij(fepois(as.formula(sprintf("y ~ tr | %s", fe)), data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200), "tr")
}
fit_trendbreuk <- function(p, van, fe = FE) {
  p[, tj := (t - van) / 4]
  p[, `:=`(voortrend = as.integer(G == 1L) * tj, sprong = as.integer(G == 1L) * as.integer(t >= van),
           natrend = as.integer(G == 1L) * as.integer(t >= van) * tj)]
  m <- fepois(as.formula(sprintf("y ~ voortrend + sprong + natrend | %s", fe)), data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  rbindlist(lapply(c("voortrend", "sprong", "natrend"), function(v) coefrij(m, v)))
}
fit_es <- function(p, ref = Q_REF, fe = FE) {
  p[, G1 := as.integer(G == 1L)]
  m <- fepois(as.formula(sprintf("y ~ i(t, G1, ref = c(%s)) | %s", paste(ref, collapse = ","), fe)),
              data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  cf <- coef(m); se <- se(m)
  tt <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  tab <- rbind(data.table(t = tt, b = cf, se = se), data.table(t = ref, b = 0, se = 0))[order(t)]
  tab[, `:=`(kw = kw_label(t), irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  b <- coef(m); V <- vcov(m)
  idx <- which(tt < min(ref))
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  jr <- tab[, .(b = mean(b)), by = .(jaar = substr(kw, 1, 4))]
  list(coef = tab[, .(t, kw, b, se, irr, lo, hi)], jaren = jr, pre_p = pchisq(W, length(idx), lower.tail = FALSE))
}
toon <- function(x) print(x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)])

## =====================================================================
## A. Verschuiving naar binnenstedelijk bouwen
## =====================================================================
kop("A. Verschuiving: binnen of buiten de verstedelijkingscontour, eengezins of meergezins")
## A1. Beschrijvend per jaar
jaarlijks <- function(dt, u) dt[, .(objecten = .N, buiten_contour = mean(binnen == 0L, na.rm = TRUE),
                                    objecten_vroeg = sum(!is.na(binnen_vroeg)), buiten_vroeg = mean(binnen_vroeg == 0L, na.rm = TRUE),
                                    meergezins = mean(meergezins == 1L, na.rm = TRUE), groot_project = mean(projectomvang >= 20L)),
                                by = .(jaar = KW_START + (q - 1L) %/% 4L, nabij = D)][order(jaar, nabij)][, uitkomst := u][]
R$aandelen <- rbind(jaarlijks(UIT$nieuwbouw, "nieuwbouw"), jaarlijks(UIT$vergunningen, "vergunningen"))
cat("aandeel buiten de huidige en de vroege contour, en meergezins, nieuwbouw, per jaar (nabij 0/1):\n")
print(dcast(R$aandelen[uitkomst == "nieuwbouw"], jaar ~ nabij, value.var = c("buiten_contour", "buiten_vroeg", "meergezins"))[, lapply(.SD, function(v) round(v, 3))])
cat("\nzelfde, vergunningen:\n")
print(dcast(R$aandelen[uitkomst == "vergunningen"], jaar ~ nabij, value.var = c("buiten_vroeg", "meergezins"))[, lapply(.SD, function(v) round(v, 3))])
R$kruis <- rbindlist(lapply(names(UIT), function(u) UIT[[u]][!is.na(binnen), .(objecten = .N), by = .(nabij = D, binnen, meergezins)][, uitkomst := u]))
cat("\nkruistabel nieuwbouw (objecten):\n")
print(dcast(R$kruis[uitkomst == "nieuwbouw"], nabij ~ binnen + meergezins, value.var = "objecten"))

## A2. Nabij tegenover ver, binnen de gemeente, per klasse
## Is de lagere oplevering nabij een verschil tussen de klassen (dan
## verdwijnt ze binnen een klasse) of zit ze in elke klasse?
R$binnen_klasse <- list()
for (u in names(UIT)) {
  rows <- list()
  alle <- paneel(copy(UIT[[u]]), quote(D), 2L)
  rows[[1]] <- cbind(klasse = "alle objecten", fit_post(alle, Q_PAS))
  for (k in list(list("binnen de contour", quote(binnen == 1L)), list("buiten de contour", quote(binnen == 0L)),
                 list("eengezins (pand met 1 of 2 woningen)", quote(meergezins == 0L)),
                 list("meergezins (pand met 3 of meer woningen)", quote(meergezins == 1L)),
                 list("vijf provincies met vroege contour", quote(!is.na(binnen_vroeg))),
                 list("binnen de vroege contour (2010-2014)", quote(binnen_vroeg == 1L)),
                 list("buiten de vroege contour (2010-2014)", quote(binnen_vroeg == 0L)))) {
    sub <- UIT[[u]][eval(k[[2]])]
    p <- paneel(copy(sub), quote(D), 2L)
    tb <- fit_trendbreuk(copy(p[t <= Q_SAL - 1L]), Q_PAS)
    rows[[length(rows) + 1L]] <- cbind(klasse = k[[1]], fit_post(p, Q_PAS),
                                       voortrend = tb$b[1], voortrend_se = tb$se[1], sprong = tb$b[2], sprong_se = tb$se[2], sprong_p = tb$p[2])
  }
  R$binnen_klasse[[u]] <- rbindlist(rows, fill = TRUE)
  cat("\n", u, ": nabij x na PAS, streng, per klasse\n", sep = ""); toon(R$binnen_klasse[[u]])
}

## A3. Samenstelling weggewerkt: nabij tegenover ver binnen gemeente EN
## klasse (contour x woningtype), een gemeenschappelijke nabijheidsterm
samenstelling <- function(u, contour, label) {
  dt <- UIT[[u]][!is.na(get(contour)) & !is.na(meergezins)]
  dt[, klasse := get(contour) * 2L + meergezins]
  ## groep = D, maar de vaste effecten per gemeente x klasse
  dt <- dt[!is.na(gemeentecode)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(D = D[1L], klasse = klasse[1L], n = .N), by = .(projectid, gemeentecode, q)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, klasse, D, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), klasse = 0:3, D = 0:1, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "klasse", "D", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(gkD = paste(gemeentecode, klasse, D), gkt = paste(gemeentecode, klasse, t), gt = paste(gemeentecode, t),
           gD = paste(gemeentecode, D), D_szn = paste(D, (t - 1L) %% 4L), tr = D * as.integer(t >= Q_PAS))]
  m1 <- fepois(y ~ tr | gD + gt + D_szn, data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  m2 <- fepois(y ~ tr | gkD + gkt + D_szn, data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  rbind(cbind(uitkomst = u, contour = label, model = "gemeente x nabij, gemeente x kwartaal (als hoofdstuk 8)", coefrij(m1, "tr")),
        cbind(uitkomst = u, contour = label, model = "plus klasse: gemeente x klasse x nabij, gemeente x klasse x kwartaal", coefrij(m2, "tr")))
}
R$samenstelling <- rbindlist(lapply(names(UIT), function(u)
  rbind(samenstelling(u, "binnen", "huidige contour, heel Nederland"),
        samenstelling(u, "binnen_vroeg", "vroege contour, vijf provincies"))))
cat("\nsamenstelling weggewerkt (klasse = contour x woningtype):\n"); toon(R$samenstelling)

## =====================================================================
## B. Netcongestie
## =====================================================================
## Groep 1: objecten op een plek die op 17 april 2024 al rood was voor
## afname; groep 0: pas later rood (juli 2025, september 2026) of nooit.
## Wanneer de vroeg rode gebieden rood werden, staat niet in de data (de
## eerste landelijke stand is januari 2024); een rem zou vanaf ongeveer
## 2022 zichtbaar moeten worden.
kop("B. Netcongestie: vroeg rood (april 2024) tegenover later of nooit")
Q_NET <- kw_index(as.Date("2022-01-01"))
R$netcongestie <- list()
for (u in names(UIT)) {
  dt <- UIT[[u]][cong_eerste %in% c(0, 1, 2, 3)]
  dt[, vroeg := as.integer(cong_eerste == 1)]
  besch <- dt[, .(objecten = .N, vroeg_rood = mean(vroeg)), by = .(jaar = KW_START + (q - 1L) %/% 4L)][order(jaar)]
  p <- paneel(copy(dt), quote(vroeg), 2L)
  es <- fit_es(copy(p))
  R$netcongestie[[u]] <- list(beschrijvend = besch, did_2022 = fit_post(copy(p), Q_NET),
                              did_2023 = fit_post(copy(p), kw_index(as.Date("2023-01-01"))),
                              es = es, aandeel_bekend = nrow(dt) / nrow(UIT[[u]]))
  cat(sprintf("%-12s aandeel vroeg rood %.3f; DiD vanaf 2022 %+.3f (%.3f) p=%.3f; pre p=%.3f; jaren: %s\n", u, mean(dt$vroeg),
              R$netcongestie[[u]]$did_2022$b, R$netcongestie[[u]]$did_2022$se, R$netcongestie[[u]]$did_2022$p, es$pre_p,
              paste(sprintf("%s %+.2f", es$jaren$jaar, es$jaren$b), collapse = "  ")))
}

## =====================================================================
## C. Stikstofdepositie op de bouwlocatie als dosis
## =====================================================================
## Kwartielen van de totale depositie 2018 over de vergunde objecten van
## 2012 tot en met 2018. Binnen dezelfde gemeente en hetzelfde kwartaal:
## Q2, Q3, Q4 tegenover Q1, na de uitspraak, en per kwartiel een trendbreuk.
kop("C. Depositie 2018 als dosis, kwartielen binnen de gemeente")
grenzen <- quantile(UIT$vergunningen[q < Q_PAS & ndep_2018 > 0, ndep_2018], c(.25, .5, .75))
R$dosis_grenzen <- as.list(setNames(round(grenzen), c("p25", "p50", "p75")))
cat("grenzen (mol N/ha/jr):", paste(round(grenzen), collapse = ", "), "\n")
R$dosis <- list()
for (u in names(UIT)) {
  dt <- UIT[[u]][ndep_2018 > 0]
  dt[, kwartiel := findInterval(ndep_2018, grenzen)]   # 0..3
  p <- paneel(copy(dt), quote(kwartiel), 4L)
  p[, post := as.integer(t >= Q_PAS)]
  m <- fepois(y ~ i(G, post, ref = 0) | gem_G + gem_t + G_szn, data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  did <- data.table(kwartiel = paste0("Q", 2:4), b = coef(m), se = se(m), p = pvalue(m), n = nobs(m))
  q2 <- copy(p[t <= Q_SAL - 1L]); q2[, tj := (t - Q_PAS) / 4]
  for (g in 1:3) q2[, (paste0(c("voortrend_", "sprong_", "natrend_"), g)) :=
                      list(as.integer(G == g) * tj, as.integer(G == g) * post, as.integer(G == g) * post * tj)]
  v <- paste(c(paste0("voortrend_", 1:3), paste0("sprong_", 1:3), paste0("natrend_", 1:3)), collapse = " + ")
  mt <- fepois(as.formula(sprintf("y ~ %s | gem_G + gem_t + G_szn", v)), data = q2, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  tb <- data.table(term = names(coef(mt)), b = coef(mt), se = se(mt), p = pvalue(mt))
  tb[, `:=`(kwartiel = paste0("Q", as.integer(sub(".*_", "", term)) + 1L), deel = sub("_.*", "", term))]
  ## ter vergelijking de oude specificatie: gemeente x kwartiel en landelijk kwartaal
  mb <- fepois(y ~ i(G, post, ref = 0) | gem_G + t + G_szn, data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  R$dosis[[u]] <- list(did = did, trendbreuk = tb,
                       did_landelijk = data.table(kwartiel = paste0("Q", 2:4), b = coef(mb), se = se(mb), p = pvalue(mb)))
  cat(u, ": na PAS per kwartiel (streng)\n"); toon(did)
  cat(u, ": trendbreuk per kwartiel\n"); toon(tb[deel != "natrend", .(kwartiel, deel, b, se, p)])
  cat(u, ": zelfde met alleen landelijk kwartaal\n"); toon(R$dosis[[u]]$did_landelijk)
}

writeLines(to_json(R), file.path(PATHS$output, sprintf("ruimte_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("ruimte_%s.json", EXTRACT)))
sink()
