## =====================================================================
## 13_binnenring.R -- de binnenste kilometer sinds 2024: meetbreuk of
##                    niet, en per woningtype
##
## Input : _derived/vbo_analyse.rds
## Output: _output/binnenring_<extract>.json   (voor het rapport)
##         _output/binnenring_<extract>.txt    (leesbare log)
##
## Waarom
## ---------------------------------------------------------------------
## 08_strikt.R vindt dat de binnenste ring (0 tot 1.000 m van stikstof-
## gevoelig habitat) binnen de gemeente vanaf 2024 minder vergunningen
## krijgt dan het gebied verder dan 5 km. Versie 3.0 van het rapport las
## de stap van 2024 deels als meetbreuk: onder de Wkb vervangt voor
## gevolgklasse 1 (vooral grondgebonden woningen) een bouwmelding de
## technische bouwvergunning, en als zulke panden nooit de BAG-status
## Bouwvergunning verleend krijgen, vallen ze uit de vergunningreeks.
## Die lezing heeft drie toetsbare gevolgen, en dit script toetst ze:
##   1. Wkb-woningen die inmiddels zijn opgeleverd, staan in de CBS-
##      nieuwbouwreeks zonder vergunningrecord: het aandeel opleveringen
##      zonder BAG-vergunning stijgt vanaf 2025.
##   2. Het aantal vergunde eengezinswoningen in de BAG zakt in 2024,
##      landelijk en tegenover meergezins.
##   3. Binnen de gemeente zit de dip in de binnenste ring bij eengezins.
## Daarna per woningtype en per ring een eigen stap bij de Omgevingswet
## (2024Q1) en bij de uitspraak over intern salderen (2024Q4), binnen
## gemeente en kwartaal, op het venster 2019Q2 tot en met 2026Q1.
##
## Woningtype zoals in 11_ruimte.R: meergezins als het pand drie of meer
## woonobjecten telt in het objectenbestand, anders eengezins.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("binnenring_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
d <- read_derived("vbo_analyse")
R <- list()

## Tijdindex en steekproeven: dezelfde als in 08_strikt.R
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_OW  <- kw_index(as.Date("2024-01-01"))
Q_SAL <- kw_index(as.Date("2024-12-18"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))   # 2026Q2 = batchregistratie
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))

vbo_per_pand <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
d <- merge(d, vbo_per_pand, by = "pand_bag_nr", all.x = TRUE)
d[, type := fifelse(n_pand >= 3L, "meergezins", "eengezins")]
d[, ringk := fifelse(is.na(dist2n2000_m), NA_character_,
             fifelse(dist2n2000_m <= 1000, "0 tot 1 km", fifelse(dist2n2000_m <= 2500, "1 tot 2,5 km",
             fifelse(dist2n2000_m <= 5000, "2,5 tot 5 km", "verder dan 5 km"))))]
halfjaar <- function(datum) paste0(format(datum, "%Y"), fifelse(as.integer(format(datum, "%m")) <= 6L, "H1", "H2"))

s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               q_ow = Q_OW, q_sal = Q_SAL, vergunningen = nrow(verg), opleveringen = nrow(nwb),
               laatste_oplevermaand = format(max(nwb$mutatiemaand)))

## ---------------------------------------------------------------------
## 1. Opleveringen zonder BAG-vergunning
## ---------------------------------------------------------------------
kop("1. Aandeel opleveringen zonder vergunningrecord in de BAG, per halfjaar")
nwb[, hj := halfjaar(mutatiemaand)]
R$zonder_bvv <- nwb[mutatiemaand >= as.Date("2019-01-01") & mutatiemaand < as.Date("2026-07-01"),
                    .(opgeleverd = .N, zonder_bvv = sum(!bron_vergunning)), by = .(hj, type)][order(hj, type)]
R$zonder_bvv[, aandeel := zonder_bvv / opgeleverd]
print(dcast(R$zonder_bvv, hj ~ type, value.var = c("opgeleverd", "aandeel"))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
R$zonder_bvv_ring <- nwb[mutatiemaand >= as.Date("2021-01-01") & mutatiemaand < as.Date("2026-07-01"),
                         .(opgeleverd = .N, zonder_bvv = sum(!bron_vergunning)), by = .(jaar = as.integer(format(mutatiemaand, "%Y")), ringk)][order(jaar, ringk)]
R$zonder_bvv_ring[, aandeel := zonder_bvv / opgeleverd]
cat("\nper jaar en ring (2026 = eerste halfjaar):\n")
print(dcast(R$zonder_bvv_ring, jaar ~ ringk, value.var = "aandeel")[, lapply(.SD, function(x) round(x, 3))])
## Zitten er al woningen van na de Omgevingswet tussen? Moment van de eerste
## vergunning op het pand voor opleveringen van juli 2025 tot en met juni 2026.
recent <- nwb[mutatiemaand >= as.Date("2025-07-01") & mutatiemaand < as.Date("2026-07-01")]
recent[, moment := fifelse(!bron_vergunning, "geen vergunningrecord",
                   fifelse(vergunning_datum < as.Date("2024-01-01"), "vergund voor 2024",
                   fifelse(vergunning_datum < as.Date("2024-07-01"), "vergund 2024H1", "vergund vanaf 2024H2")))]
R$recent_moment <- recent[, .(n = .N), by = .(type, moment)][, aandeel := n / sum(n), by = type][order(type, moment)]
cat("\nopleveringen juli 2025 tot en met juni 2026, naar moment van de vergunning:\n")
print(dcast(R$recent_moment, moment ~ type, value.var = "aandeel")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 2. Vergunde objecten per jaar, woningtype en ring
## ---------------------------------------------------------------------
kop("2. Vergunde objecten per jaar, woningtype en ring (2026 = eerste kwartaal)")
R$verg_jaar <- verg[vergunning_datum >= as.Date("2015-01-01"), .(n = .N, projecten = uniqueN(projectid)),
                    by = .(jaar = as.integer(format(vergunning_datum, "%Y")), type, ringk)][order(jaar, type, ringk)]
print(dcast(R$verg_jaar, jaar ~ type + ringk, value.var = "n"))
cat("\naandeel eengezins in alle vergunde objecten:\n")
print(R$verg_jaar[, .(eengezins = sum(n[type == "eengezins"]) / sum(n)), by = jaar][, eengezins := round(eengezins, 3)][])

## ---------------------------------------------------------------------
## 3. Binnen de gemeente: ring x jaar, per woningtype
## ---------------------------------------------------------------------
ring_van <- function(k) fifelse(k <= 4L, 1L, fifelse(k <= 10L, 2L, fifelse(k <= 20L, 3L, 0L)))
RINGEN <- c("0 tot 1.000 m", "1.000 tot 2.500 m", "2.500 tot 5.000 m")
paneel_ring <- function(dt) {
  pp  <- dt[!is.na(gemeentecode), .(k = min(afstandsklasse), n = .N), by = .(projectid, gemeentecode, q)]
  pp[, ring := ring_van(k)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, ring, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), ring = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "ring", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(gem_r = paste(gemeentecode, ring), gem_t = paste(gemeentecode, t), r_szn = paste(ring, (t - 1L) %% 4L),
           jr = factor(KW_START + (t - 1L) %/% 4L))]
  p[]
}
SOORTEN <- list(alle = quote(TRUE), eengezins = quote(type == "eengezins"), meergezins = quote(type == "meergezins"))
kop("3. Binnen gemeente en kwartaal: ring tegenover verder dan 5 km, per jaar (t.o.v. 2018)")
R$ringen_jaar <- rbindlist(lapply(names(SOORTEN), function(sn) {
  p <- paneel_ring(verg[eval(SOORTEN[[sn]])])
  m <- fepois(y ~ i(jr, i.ring, ref = "2018", ref2 = 0) | gem_r + gem_t + r_szn, data = p, cluster = ~gemeentecode,
              notes = FALSE, glm.iter = 200)
  cj <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(cj, c("term", "b", "se", "tw", "p"))
  cj[, .(soort = sn, ring = RINGEN[as.integer(sub(".*ring::", "", term))],
         jaar = as.integer(sub("^jr::([0-9]+):.*", "\\1", term)), b, se, p)]
}))
print(dcast(R$ringen_jaar[ring == RINGEN[1] & jaar >= 2015], jaar ~ soort, value.var = c("b", "p"))[, lapply(.SD, function(x) round(x, 3))])

## ---------------------------------------------------------------------
## 4. Een eigen stap per ring bij de Omgevingswet en bij intern salderen
## ---------------------------------------------------------------------
## Venster 2019Q2 tot en met 2026Q1: de stappen meten tegen 2019Q2-2023Q4.
kop("4. Stappen per ring bij de Omgevingswet (2024Q1) en intern salderen (2024Q4), binnen gemeente en kwartaal")
R$stappen <- rbindlist(lapply(names(SOORTEN), function(sn) {
  p <- paneel_ring(verg[eval(SOORTEN[[sn]])])[t >= Q_PAS]
  p[, `:=`(ow = as.integer(t >= Q_OW), sal = as.integer(t >= Q_SAL))]
  m <- fepois(y ~ i(ring, ow, ref = 0) + i(ring, sal, ref = 0) | gem_r + gem_t + r_szn, data = p,
              cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  ct <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(ct, c("term", "b", "se", "tw", "p"))
  ## de som ow + sal: het niveau na intern salderen tegenover 2019Q2-2023Q4
  V <- vcov(m); b <- coef(m)
  som <- rbindlist(lapply(1:3, function(r) {
    i1 <- sprintf("ring::%d:ow", r); i2 <- sprintf("ring::%d:sal", r)
    data.table(term = sprintf("ring::%d:som", r), b = b[i1] + b[i2],
               se = sqrt(V[i1, i1] + V[i2, i2] + 2 * V[i1, i2]))
  }))
  som[, `:=`(tw = b / se, p = 2 * pnorm(-abs(b / se)))]
  ct <- rbind(ct, som)
  ct[, .(soort = sn, ring = RINGEN[as.integer(sub("^ring::([0-9]):.*", "\\1", term))], stap = sub(".*:", "", term),
         b, se, p, n = nobs(m))]
}))
R$stappen[, pct := 100 * (exp(b) - 1)]
print(R$stappen[, .(soort, ring, stap, b = round(b, 3), se = round(se, 3), p = round(p, 3), pct = round(pct, 1))])

## ---------------------------------------------------------------------
## 5. In woningen: de stap bij intern salderen in de binnenste ring
## ---------------------------------------------------------------------
kop("5. In woningen, binnenste ring, 2025")
n25 <- verg[ringk == "0 tot 1 km" & format(vergunning_datum, "%Y") == "2025", .N, by = type]
R$woningen <- rbindlist(lapply(names(SOORTEN), function(sn) {
  st <- R$stappen[soort == sn & ring == RINGEN[1] & stap == "sal"]
  n <- if (sn == "alle") sum(n25$N) else n25[type == sn, N]
  data.table(soort = sn, vergund_2025 = n, b = st$b, se = st$se,
             minder = n * (exp(-st$b) - 1), minder_lo = n * (exp(-(st$b + 1.96 * st$se)) - 1),
             minder_hi = n * (exp(-(st$b - 1.96 * st$se)) - 1))
}))
print(R$woningen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 6. Gemeenten met woningbouw in de binnenste ring: absolute aantallen
## ---------------------------------------------------------------------
## Binnen de gemeente is de binnenste ring relatief tegenover verder dan
## 5 km. Waar komt de relatieve daling van 2024 vandaan: minder dichtbij
## of meer verderop? Per project de ring van zijn kleinste afstandsklasse,
## zoals in de modellen.
kop("6. Gemeenten met woningbouw in de binnenste ring 2019-2023: vergunde objecten per jaar, ring en type")
pr <- verg[!is.na(gemeentecode), .(k = min(afstandsklasse), n = .N, type = type[1]),
           by = .(projectid, gemeentecode, q, jaar = as.integer(format(vergunning_datum, "%Y")))]
pr[, ring := ring_van(k)]
gem1 <- pr[ring == 1L & jaar %between% c(2019L, 2023L), unique(gemeentecode)]
R$meta$gemeenten_binnenring <- length(gem1)
R$gemeenten_binnenring <- pr[gemeentecode %in% gem1 & jaar >= 2019L, .(n = sum(n), projecten = .N), by = .(jaar, type, ring)][order(jaar, type, ring)]
R$gemeenten_binnenring[, ring := c("verder dan 5 km", RINGEN)[ring + 1L]]
cat(length(gem1), "gemeenten\n")
print(dcast(R$gemeenten_binnenring, jaar ~ type + ring, value.var = "n"))

## ---------------------------------------------------------------------
## 7. Projecten tellen in plaats van objecten, en zonder de grootste
## ---------------------------------------------------------------------
paneel_proj <- function(pp, gewicht) {
  cel <- pp[, .(y = if (gewicht) sum(n) else .N), by = .(gemeentecode, ring, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), ring = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "ring", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(gem_r = paste(gemeentecode, ring), gem_t = paste(gemeentecode, t), r_szn = paste(ring, (t - 1L) %% 4L))]
  p <- p[t >= Q_PAS]; p[, `:=`(ow = as.integer(t >= Q_OW), sal = as.integer(t >= Q_SAL))]
  p[]
}
stap_tabel <- function(p, lab) {
  m <- fepois(y ~ i(ring, ow, ref = 0) + i(ring, sal, ref = 0) | gem_r + gem_t + r_szn, data = p,
              cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  ct <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(ct, c("term", "b", "se", "tw", "p"))
  ## de som ow + sal: het niveau na intern salderen tegenover 2019Q2-2023Q4
  V <- vcov(m); bb <- coef(m)
  som <- rbindlist(lapply(1:3, function(r) {
    i1 <- sprintf("ring::%d:ow", r); i2 <- sprintf("ring::%d:sal", r)
    data.table(term = sprintf("ring::%d:som", r), b = bb[i1] + bb[i2], se = sqrt(V[i1, i1] + V[i2, i2] + 2 * V[i1, i2]))
  }))
  som[, `:=`(tw = b / se, p = 2 * pnorm(-abs(b / se)))]
  ct <- rbind(ct, som)
  ct[, .(variant = lab, ring = RINGEN[as.integer(sub("^ring::([0-9]):.*", "\\1", term))], stap = sub(".*:", "", term), b, se, p, n = nobs(m))]
}
kop("7. Stappen per ring: projecten in plaats van objecten, en appartementen zonder projecten van 200 of meer")
R$stappen_varianten <- rbindlist(list(
  stap_tabel(paneel_proj(pr[type == "eengezins"], FALSE), "eengezins, projecten"),
  stap_tabel(paneel_proj(pr[type == "meergezins"], FALSE), "meergezins, projecten"),
  stap_tabel(paneel_proj(pr[type == "meergezins" & projectid %in% verg[projectomvang < 200L, unique(projectid)]], TRUE),
             "meergezins, objecten, zonder projecten van 200 of meer")
))
R$stappen_varianten[, pct := 100 * (exp(b) - 1)]
R$meta$aandeel_meergezins_in_projecten_200 <- verg[type == "meergezins", mean(projectomvang >= 200L)]
print(R$stappen_varianten[, .(variant, ring, stap, b = round(b, 3), se = round(se, 3), p = round(p, 3), pct = round(pct, 1))])

## ---------------------------------------------------------------------
## 7b. De stap voor intern salderen vanaf 2025Q1 in plaats van 2024Q4
## ---------------------------------------------------------------------
## De uitspraak viel op 18 december 2024; met de stap vanaf 2024Q4 telt bijna
## een heel kwartaal van voor de uitspraak mee als na.
kop("7b. Stappen per ring met intern salderen vanaf 2025Q1")
Q_SAL_B <- kw_index(as.Date("2025-01-01"))
R$stappen_2025 <- rbindlist(lapply(names(SOORTEN), function(sn) {
  p <- paneel_ring(verg[eval(SOORTEN[[sn]])])[t >= Q_PAS]
  p[, `:=`(ow = as.integer(t >= Q_OW), sal = as.integer(t >= Q_SAL_B))]
  cbind(soort = sn, stap_tabel(p, "")[, !"variant"])
}))
R$stappen_2025[, pct := 100 * (exp(b) - 1)]
print(R$stappen_2025[ring == RINGEN[1], .(soort, stap, b = round(b, 3), se = round(se, 3), p = round(p, 3), pct = round(pct, 1))])
n25 <- verg[ringk == "0 tot 1 km" & format(vergunning_datum, "%Y") == "2025", .N]
st <- R$stappen_2025[soort == "alle" & ring == RINGEN[1] & stap == "sal"]
R$woningen_2025q1 <- data.table(vergund_2025 = n25, b = st$b, se = st$se, minder = n25 * (exp(-st$b) - 1),
                                minder_lo = n25 * (exp(-(st$b + 1.96 * st$se)) - 1), minder_hi = n25 * (exp(-(st$b - 1.96 * st$se)) - 1))
print(R$woningen_2025q1)

## ---------------------------------------------------------------------
## 7c. Vergunde objecten per maand, 2024 tot en met juni 2026
## ---------------------------------------------------------------------
## Het moment van registratie veranderde: onder de Wkb telt voor gevolgklasse 1
## het akkoord op de bouwmelding, voor gevolgklasse 2 en 3 de technische
## bouwvergunning (VNG en Waarderingskamer, factsheet oktober 2023), en sinds
## september 2025 de vergunning voor de omgevingsplanactiviteit (BWTinfo,
## september 2025). Een inhaalslag zou als piek zichtbaar zijn.
kop("7c. Vergunde objecten (in_sample) per maand")
alle_verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2024-01-01") & vergunning_datum <= as.Date("2026-06-30")]
R$per_maand <- alle_verg[, .(n = .N), by = .(maand = format(vergunning_datum, "%Y-%m"), type)][order(maand, type)]
print(dcast(R$per_maand, maand ~ type, value.var = "n"))

## ---------------------------------------------------------------------
## 8. De golf aanvragen rond de Omgevingswet
## ---------------------------------------------------------------------
## Nieuwbouwaanvragen (strenge typering uit 09a_titels.py) per kwartaal en
## ring, alle gemeenten, uit de zaken van het vergunningenspoor.
kop("8. Nieuwbouwaanvragen per kwartaal en ring, 2022 tot en met 2025")
f_zaken <- file.path(PATHS$verg, "analyse", "zaken.csv")
f_tit   <- file.path(PATHS$derived, "aanvragen_titels.csv")
if (file.exists(f_zaken) && file.exists(f_tit)) {
  z <- fread(f_zaken, sep = ";", encoding = "UTF-8", select = c("zaak_id", "kwartaal", "dist_m", "woningen", "soort"))
  z <- merge(z, fread(f_tit), by = "zaak_id", all.x = TRUE)
  z <- z[soort == "nieuwbouw_woning" & streng %in% 1L & !(tijdelijk %in% 1L) & !is.na(dist_m) &
         kwartaal >= "2022K1" & kwartaal <= "2025K4"]
  z[, ring := fifelse(dist_m <= 1000, RINGEN[1], fifelse(dist_m <= 2500, RINGEN[2], fifelse(dist_m <= 5000, RINGEN[3], "verder dan 5 km")))]
  R$golf <- z[, .(aanvragen = .N, twintig_plus = sum(woningen >= 20L, na.rm = TRUE)), by = .(kwartaal, ring)][order(kwartaal, ring)]
  print(dcast(R$golf, kwartaal ~ ring, value.var = "aanvragen"))
} else {
  message("zaken.csv of aanvragen_titels.csv ontbreekt; sectie 8 overgeslagen (draai eerst 09_aanvragen.R)")
}

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("binnenring_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("binnenring_%s.json", EXTRACT)))
sink()
