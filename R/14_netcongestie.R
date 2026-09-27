## =====================================================================
## 14_netcongestie.R -- netcongestie in de tijd: vergunningen en
##                      opleveringen rond de datum waarop een gebied vol raakte
##
## Input : _derived/vbo_analyse.rds
##         _derived/vbo_xy.csv          (11_ruimte.R: id;x;y van de BAG-objecten)
##         _derived/vbo_pc6.csv         (14a_pc6.py: id;postcode)
##         _derived/pc6_congestie.csv   (14b_congestiedatum.py: eerste datum per PC6)
## Output: _output/netcongestie_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 16 (11_ruimte.R, deel B) vergeleek plekken die op 17 april 2024
## al rood waren met plekken die dat later of nooit werden. Twee zwaktes: de
## datum waarop een gebied vol raakte, ontbrak, en oranje (de melding bij de
## ACM, waarmee een wachtlijst voor nieuwe grote aansluitingen begint) telde
## als niet vol. Dit script doet twee dingen:
##   A. dezelfde vergelijking binnen gemeente en kwartaal, met als behandeld
##      'oranje of rood' op de complete landelijke stand van 3 maart 2024;
##   B. een gespreide invoering: per voedingsgebied de datum waarop afname
##      vol raakte (14b_congestiedatum.py), en de ontwikkeling van vergunde en
##      opgeleverde objecten rond die datum, geschat volgens Sun en Abraham
##      (fixest::sunab) op Poisson. De controle zijn gebieden die (nog) niet
##      vol waren. Vaste effecten: gebied en landelijk kwartaal, en strenger
##      gebied en provincie x kwartaal. Standaardfouten geclusterd op
##      voedingsgebied, en ter controle op TenneT-gebied, omdat de datum
##      meestal per TenneT-gebied valt.
## Netcongestie raakt aansluitingen boven 3 x 80 ampere. Kleinverbruikers,
## dus gewone woningen, werden tot 2026 in de regel aangesloten;
## appartementengebouwen met een collectieve installatie en nieuwe wijken
## die een eigen station nodig hebben, kunnen wel moeten wachten. Daarom
## ook per woningtype.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("netcongestie_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
d <- read_derived("vbo_analyse")
R <- list()

KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q_REF <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))   # 2026Q2 = batchregistratie
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))
Q_START_B  <- kw_index(as.Date("2016-01-01"))

## ---------------------------------------------------------------------
## Koppeling: object -> postcode -> datum
## ---------------------------------------------------------------------
f_xy  <- file.path(PATHS$derived, "vbo_xy.csv")
f_pc  <- file.path(PATHS$derived, "vbo_pc6.csv")
f_dat <- file.path(PATHS$derived, "pc6_congestie.csv")
if (!file.exists(f_xy)) stop("Ontbreekt: ", f_xy, "\nDraai eerst 11_ruimte.R")
qgis_py <- function(script, ...) {
  qgis <- Sys.getenv("QGIS_PY", "C:/Program Files/QGIS 3.40.7/bin/python-qgis-ltr.bat")
  cmd <- tempfile(fileext = ".cmd")
  arg <- paste(sprintf('"%s"', c(normalizePath(file.path(.here, script)), ...)), collapse = " ")
  writeLines(c("@echo off", "setlocal", "set GDAL_PAM_ENABLED=NO", sprintf('call "%s" %s', normalizePath(qgis), arg), "endlocal"), cmd)
  status <- system2("cmd.exe", c("/c", shQuote(normalizePath(cmd), type = "cmd")))
  if (status != 0) stop(script, " faalde; draai het met de Python van QGIS (zie de kop van dat script)")
}
if (!file.exists(f_pc) || file.mtime(f_pc) < file.mtime(f_xy))
  qgis_py("14a_pc6.py", normalizePath(f_xy), normalizePath(f_pc, mustWork = FALSE), normalizePath(PATHS$verg))
if (!file.exists(f_dat)) {
  py <- Sys.getenv("PYTHON", "py")
  status <- system2(py, c(if (py == "py") "-3.13", shQuote(file.path(.here, "14b_congestiedatum.py")),
                          shQuote(normalizePath(f_dat, mustWork = FALSE)), shQuote(PATHS$verg)))
  if (status != 0 || !file.exists(f_dat)) stop("Ontbreekt: ", f_dat, "\nDraai eerst: py -3.13 R/14b_congestiedatum.py")
}
xy  <- merge(fread(f_xy, sep = ";"), fread(f_pc, sep = ";", colClasses = c(postcode = "character")), by = "id")
dat <- fread(f_dat, sep = ";", colClasses = c(postcode = "character", voedingsgebied_id = "character"))
dat[, datum := as.Date(datum)]
xy  <- merge(xy[postcode != ""], dat[, .(postcode, voedingsgebied_id, congestiegebied_afname, datum, bron, gecensureerd, nooit, stand_20240303)],
             by = "postcode", all.x = TRUE)
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
vpp <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
d <- merge(d, vpp, by = "pand_bag_nr", all.x = TRUE)
d[, type := fifelse(n_pand >= 3L, "meergezins", "eengezins")]

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               objecten = lapply(UIT, nrow),
               met_postcode = lapply(UIT, function(x) mean(!is.na(x$voedingsgebied_id))),
               met_datum = lapply(UIT, function(x) mean(!is.na(x$datum))))
kop("Koppeling")
print(str(R$meta))

## ---------------------------------------------------------------------
## A. Hoofdstuk 16 opnieuw: oranje of rood op 3 maart 2024
## ---------------------------------------------------------------------
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
fit_post <- function(p, van) {
  p[, tr := as.integer(G == 1L) * as.integer(t >= van)]
  coefrij(fepois(as.formula(sprintf("y ~ tr | %s", FE)), data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200), "tr")
}
fit_es <- function(p) {
  p[, G1 := as.integer(G == 1L)]
  m <- fepois(as.formula(sprintf("y ~ i(t, G1, ref = c(%s)) | %s", paste(Q_REF, collapse = ","), FE)),
              data = p, cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  cf <- coef(m); tt <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  b <- cf; V <- vcov(m); idx <- which(tt < min(Q_REF))
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  tab <- rbind(data.table(t = tt, b = cf), data.table(t = Q_REF, b = 0))[order(t)]
  list(jaren = tab[, .(b = mean(b)), by = .(jaar = KW_START + (t - 1L) %/% 4L)], pre_p = pchisq(W, length(idx), lower.tail = FALSE))
}
kop("A. Vol (oranje of rood) op 3 maart 2024 tegenover niet, binnen gemeente en kwartaal")
R$vol_2024 <- list()
for (u in names(UIT)) {
  dt <- copy(UIT[[u]])[!is.na(stand_20240303) & stand_20240303 >= 0]
  dt[, vol := as.integer(stand_20240303 >= 2)]
  p <- paneel(copy(dt), quote(vol), 2L)
  es <- fit_es(copy(p))
  R$vol_2024[[u]] <- list(aandeel_vol = mean(dt$vol), objecten = nrow(dt),
                          did_2022 = fit_post(copy(p), kw_index(as.Date("2022-01-01"))),
                          did_2023 = fit_post(copy(p), kw_index(as.Date("2023-01-01"))),
                          pre_p = es$pre_p, jaren = es$jaren)
  x <- R$vol_2024[[u]]
  cat(sprintf("%-12s aandeel vol %.3f; DiD vanaf 2022 %+.3f (%.3f) p=%.3f; vanaf 2023 %+.3f (%.3f) p=%.3f; pre p=%.4f\n", u, x$aandeel_vol,
              x$did_2022$b, x$did_2022$se, x$did_2022$p, x$did_2023$b, x$did_2023$se, x$did_2023$p, x$pre_p))
  cat("   per jaar t.o.v. 2018:", paste(sprintf("%d %+.2f", es$jaren$jaar, es$jaren$b), collapse = "  "), "\n")
}
## dezelfde stand, maar alleen rood als vol: wat de indeling van versie 3.0 op deze stand zou geven
R$rood_2024 <- list()
for (u in names(UIT)) {
  dt <- copy(UIT[[u]])[!is.na(stand_20240303) & stand_20240303 >= 0]
  dt[, vol := as.integer(stand_20240303 == 3)]
  p <- paneel(copy(dt), quote(vol), 2L)
  R$rood_2024[[u]] <- list(aandeel_rood = mean(dt$vol), did_2022 = fit_post(copy(p), kw_index(as.Date("2022-01-01"))))
  cat(sprintf("%-12s alleen rood: aandeel %.3f; DiD vanaf 2022 %+.3f (%.3f) p=%.3f\n", u, R$rood_2024[[u]]$aandeel_rood,
              R$rood_2024[[u]]$did_2022$b, R$rood_2024[[u]]$did_2022$se, R$rood_2024[[u]]$did_2022$p))
}

## ---------------------------------------------------------------------
## B. Gespreide invoering per voedingsgebied
## ---------------------------------------------------------------------
kop("B. Gespreide invoering: vergunningen en opleveringen rond de datum waarop het gebied vol raakte")
## datum per voedingsgebied: de mediaan van de gedateerde PC6; nooit vol als
## de meerderheid van de PC6 nooit vol was; anders onbekend (valt af)
gebied <- dat[!is.na(voedingsgebied_id) & voedingsgebied_id != "", .(
  n_pc6 = .N, gedateerd = mean(!is.na(datum)), nooit = mean(nooit == 1L),
  dag = if (any(!is.na(datum))) as.numeric(median(as.numeric(datum), na.rm = TRUE)) else NA_real_,
  gecensureerd = mean(gecensureerd == 1L, na.rm = TRUE),
  tennet = names(which.max(table(congestiegebied_afname, useNA = "ifany")))), by = voedingsgebied_id]
gebied[, datum := as.Date(round(dag), origin = "1970-01-01")]
gebied[, status := fifelse(gedateerd >= 0.5, "vol", fifelse(nooit >= 0.5, "nooit", "onbekend"))]
gebied[, cohort := fifelse(status == "vol", kw_index(datum), fifelse(status == "nooit", 10000L, NA_integer_))]
gebied[is.na(tennet) | tennet == "" | tennet == "NA", tennet := paste0("vg_", voedingsgebied_id)]
R$gebieden <- gebied[, .(gebieden = .N, pc6 = sum(n_pc6)), by = .(status, cohort = fifelse(cohort == 10000L, NA_character_, kw_label(cohort)))][order(status, cohort)]
print(R$gebieden)
## hoe dun is de controle: nooit volle gebieden met objecten, en hun aandeel in de objecten
nooit_vg <- gebied[status == "nooit", voedingsgebied_id]
R$meta$nooit_vol <- list(gebieden = length(nooit_vg),
                         met_objecten = uniqueN(d[voedingsgebied_id %in% nooit_vg & (in_sample %in% TRUE | cbs_nieuwbouw %in% TRUE), voedingsgebied_id]),
                         aandeel_vergunningen = mean(verg$voedingsgebied_id %in% nooit_vg),
                         aandeel_opleveringen = mean(nwb$voedingsgebied_id %in% nooit_vg),
                         provincies = d[voedingsgebied_id %in% nooit_vg & in_sample %in% TRUE, .N, by = provincienaam][order(-N)])
print(R$meta$nooit_vol)
gecens <- dat[!is.na(voedingsgebied_id) & voedingsgebied_id != "" & !is.na(datum),
              .(aandeel_gecensureerd = mean(gecensureerd == 1L)), by = voedingsgebied_id]
R$meta$gebieden_meest_gecensureerd <- mean(gecens[voedingsgebied_id %in% gebied[status == "vol", voedingsgebied_id], aandeel_gecensureerd > 0.5])

prov_van <- d[!is.na(voedingsgebied_id), .N, by = .(voedingsgebied_id, provinciecode)][order(-N)][!duplicated(voedingsgebied_id)]
SOORTEN <- list(alle = quote(TRUE), eengezins = quote(type == "eengezins"), meergezins = quote(type == "meergezins"))
paneel_b <- function(dt, qmax) {
  dt <- dt[!is.na(voedingsgebied_id) & q >= Q_START_B]
  cel <- dt[, .(y = .N), by = .(voedingsgebied_id, t = q)]
  g <- gebied[!is.na(cohort), .(voedingsgebied_id, cohort, tennet)]
  vol <- CJ(voedingsgebied_id = g$voedingsgebied_id, t = Q_START_B:qmax)
  p <- merge(vol, cel, by = c("voedingsgebied_id", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p <- merge(p, g, by = "voedingsgebied_id")
  p <- merge(p, prov_van[, .(voedingsgebied_id, provinciecode)], by = "voedingsgebied_id", all.x = TRUE)
  p[, `:=`(prov_t = paste(provinciecode, t))]
  p[]
}
R$gespreid <- list(); R$gespreid_es <- list()
for (u in names(UIT)) for (sn in names(SOORTEN)) {
  p <- paneel_b(UIT[[u]][eval(SOORTEN[[sn]])], if (u == "vergunningen") Q_MAX_VERG else Q_MAX_NWB)
  for (fe in c("gebied + kwartaal", "gebied + provincie x kwartaal")) {
    ff <- if (fe == "gebied + kwartaal") "voedingsgebied_id + t" else "voedingsgebied_id + prov_t"
    m <- fepois(as.formula(sprintf("y ~ sunab(cohort, t) | %s", ff)), data = p, cluster = ~voedingsgebied_id, notes = FALSE, glm.iter = 200)
    att <- aggregate(m, c("ATT" = "t::[^-]"))
    att_t <- aggregate(summary(m, cluster = ~tennet), c("ATT" = "t::[^-]"))
    ## ATT over de eerste acht kwartalen na de datum (0 tot en met 7)
    ## de ruwe coefficienten van sunab heten t::<periode>:cohort::<cohort>
    att8 <- aggregate(m, c("ATT_0_7" = "^t::[0-7]:"))
    rij <- data.table(uitkomst = u, soort = sn, fe = fe, b = att[1, 1], se = att[1, 2], p = att[1, 4],
                      lo = exp(att[1, 1] - 1.96 * att[1, 2]) - 1, hi = exp(att[1, 1] + 1.96 * att[1, 2]) - 1,
                      se_tennet = att_t[1, 2], p_tennet = att_t[1, 4],
                      b_0_7 = att8[1, 1], se_0_7 = att8[1, 2], p_0_7 = att8[1, 4], n = nobs(m))
    R$gespreid[[length(R$gespreid) + 1L]] <- rij
    ## relatieve kwartalen -12 tot en met 12, uit de aggregatie per periode van sunab
    et <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("^t::", term)]
    setnames(et, c("term", "b", "se", "tw", "p"))
    et[, rel := as.integer(sub("^t::", "", term))]
    R$gespreid_es[[length(R$gespreid_es) + 1L]] <- et[rel >= -12 & rel <= 12, .(uitkomst = u, soort = sn, fe = fe, rel, b, se, p)]
    cat(sprintf("%-12s %-10s %-30s ATT %+.3f (%.3f) p=%.3f [TenneT-cluster p=%.3f]; kwartaal 0-7 %+.3f (%.3f) p=%.3f\n",
                u, sn, fe, rij$b, rij$se, rij$p, rij$p_tennet, rij$b_0_7, rij$se_0_7, rij$p_0_7))
  }
}
R$gespreid <- rbindlist(R$gespreid); R$gespreid_es <- rbindlist(R$gespreid_es)
R$gespreid[, pct := 100 * (exp(b) - 1)]
cat("\nrelatieve kwartalen, vergunningen, alle objecten, gebied + kwartaal:\n")
print(R$gespreid_es[uitkomst == "vergunningen" & soort == "alle" & fe == "gebied + kwartaal", .(rel, b = round(b, 3), se = round(se, 3))])

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("netcongestie_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("netcongestie_%s.json", EXTRACT)))
sink()
