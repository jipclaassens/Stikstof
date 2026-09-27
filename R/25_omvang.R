## =====================================================================
## 25_omvang.R: het tweede kanaal. Werden woningbouwprojecten nabij
##              stikstofgevoelige natuur na de PAS-uitspraak kleiner, in
##              plaats van minder?
##
## Input : _derived/vbo_analyse.rds
## Output: _output/omvang_<extract>.json   (voor het rapport)
##         _output/omvang_<extract>.txt    (leesbare log)
##
## Waarom
## ---------------------------------------------------------------------
## 08_strikt.R telt objecten per gemeente, nabij of ver, en kwartaal. Een
## daling van het aantal objecten nabij natuur kan twee dingen zijn: er
## komen minder projecten (het eerste kanaal), of de projecten worden
## kleiner (het tweede kanaal). 08_strikt.R merkt op dat stikstof vooral
## grote projecten zou moeten raken, omdat die meer depositie geven, maar
## dat een verschuiving van grote uitleglocaties aan de randen naar
## binnenstedelijk bouwen dat ook doet. Dit script maakt de omvang van het
## project zelf tot uitkomst, met dezelfde steekproeven, projectsleutel en
## vergelijking binnen gemeente en kwartaal als 08_strikt.R.
##
## Eenheid en definities
## ---------------------------------------------------------------------
## Project   : gemeente plus documentnummer van de vergunning (projectid).
## Vergund   : objecten met in_sample en een eerste bouwvergunning van
##             2012Q1 tot en met 2026Q1 (hoofdzaak). Een project telt een
##             keer, in het kwartaal van zijn eerste vergunning.
## Opgeleverd: objecten met cbs_nieuwbouw, zonder admin_fout, met een
##             mutatiemaand van 2012Q1 tot en met 2026Q2. Een project telt
##             een keer, in het kwartaal van zijn eerste oplevering; ook
##             08_strikt.R telt opleveringen in het kwartaal van de mutatie.
## Nabij     : de kleinste afstand van de objecten van het project in de
##             steekproef tot habitat is hoogstens de drempel.
## Omvang    : projectomvang, het aantal objecten met dezelfde
##             projectsleutel in het hele bestand (01_build_vbo.R).
## Na        : kwartaal 30 (2019Q2) en later.
##
## Onderdelen
## ---------------------------------------------------------------------
## 1. Beschrijvend: projecten, gemiddelde en mediane omvang, nabij en ver,
##    voor en na 2019Q2, op 500, 1.000 en 2.500 m.
## 2. De omvang als uitkomst: log(omvang) met OLS en de omvang met Poisson,
##    vaste effecten gemeente x nabij en gemeente x kwartaal,
##    standaardfouten geclusterd op gemeente.
## 3. Event study per jaar: nabij maal jaar, 2018 als referentie, 2.500 m.
##    Daarna een trendbreuk zoals fit_trendbreuk() in 08_strikt.R, per
##    drempel: voortrend nabij, sprong bij de uitspraak en verandering van
##    de trend erna, tot en met 2024Q3. Een verschil tussen voor en na zegt
##    op zichzelf niet of de omvang bij de uitspraak veranderde of al langer
##    daalde.
## 4. De verdeling: projecten per omvangsklasse, en een Poisson-DiD per
##    klasse op gemeente x nabij x kwartaal met specificatie D van
##    08_strikt.R. Daarnaast alle projecten en alle woningen, zodat de
##    verandering in woningen zich laat splitsen in minder en kleiner.
## 5. De toets van het conceptartikel: een afname van 7 tot 13 procent op
##    500 en 1.000 m hoort hier zichtbaar te zijn.
##
## Tweede routes bij onderdeel 2: de omvang in woningen in de steekproef in
## plaats van alle objecten met de sleutel; alleen projecten waarvan alle
## objecten nieuwbouw zijn; nabij op de mediane in plaats van de kleinste
## afstand (een groot project haalt met zijn kleinste afstand eerder de
## drempel, zodat de indeling zelf van de omvang afhangt); het venster tot
## en met 2023Q4, voor de Omgevingswet; de omvang afgetopt op 200 objecten,
## omdat de Poisson de grootste projecten zwaar laat wegen; en zonder
## projecten waarvan het eerste en laatste kwartaal in de steekproef vier
## jaar of meer uiteen liggen. Zo'n sleutel kan een gefaseerd project zijn,
## maar ook een nummer voor meer projecten: het grootste geval, 518
## FV20101220-04 in Den Haag, telt 2.430 objecten in 968 panden met
## vergunningdata van 1971 tot en met 2011 en opleveringen van 2012 tot
## en met 2017, op 118 m van habitat.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("omvang_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()

d <- read_derived("vbo_analyse")
NODIG <- c("projectid", "projectomvang", "projecttype", "gemeentecode", "provinciecode", "dist2n2000_m",
           "vergunning_datum", "in_sample", "cbs_nieuwbouw", "mutatiemaand")
if (length(setdiff(NODIG, names(d)))) stop("Ontbreekt in vbo_analyse: ", paste(setdiff(NODIG, names(d)), collapse = ", "))
R <- list()

## ---------------------------------------------------------------------
## Tijdindex en parameters: dezelfde als in 08_strikt.R
## ---------------------------------------------------------------------
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_OW  <- kw_index(as.Date("2024-01-01"))   # Omgevingswet
Q_SAL <- kw_index(as.Date("2024-12-18"))   # uitspraak over intern salderen
Q_MAX <- c(vergund = kw_index(as.Date("2026-03-31")), opgeleverd = kw_index(as.Date("2026-06-30")))
stopifnot(Q_PAS == 30L)
DREMPELS <- c(500, 1000, 2500)
KLASSE_GRENZEN <- c(0, 4, 19, 49, 199, Inf)
KLASSEN <- c("1 tot 4", "5 tot 19", "20 tot 49", "50 tot 199", "200 en meer")
CLAIM <- c(-13, -7)   # de afname volgens het conceptartikel op 500 en 1.000 m, in procenten
SCHATTERS <- c("log-OLS", "Poisson")

## ---------------------------------------------------------------------
## Steekproeven: precies die van 08_strikt.R
## ---------------------------------------------------------------------
s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX[["vergund"]]]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX[["opgeleverd"]]]
UIT <- list(vergund = verg, opgeleverd = nwb)

## ---------------------------------------------------------------------
## Projecten
## ---------------------------------------------------------------------
## Zuiver nieuwbouw: alle objecten met deze sleutel in het hele bestand
## hebben projecttype nieuwbouw, ook die buiten de steekproef. Voor de
## vergunde projecten is dat de enige manier waarop de toets iets doet,
## want in_sample eist per object al projecttype nieuwbouw.
zuiver_sleutel <- d[, .(zuiver = all(projecttype %in% "nieuwbouw")), by = projectid]
## voor sectie 6: sleutels van 100 objecten of meer met een vergunning van voor 2005, en het pseudoproject in Den Haag
oud_groot <- d[!is.na(vergunning_datum), .(eerste = min(vergunning_datum), n = .N), by = projectid][n >= 100L & eerste < as.Date("2005-01-01"), projectid]
pseudo_tel <- list(objecten_totaal = d[projectid == "518 FV20101220-04", .N], panden = d[projectid == "518 FV20101220-04", uniqueN(pand_bag_nr)],
                   eerste_vergunning = format(d[projectid == "518 FV20101220-04", min(vergunning_datum, na.rm = TRUE)]),
                   laatste_vergunning = format(d[projectid == "518 FV20101220-04", max(vergunning_datum, na.rm = TRUE)]),
                   kleinste_afstand = d[projectid == "518 FV20101220-04", min(dist2n2000_m, na.rm = TRUE)], mediane_afstand = d[projectid == "518 FV20101220-04", median(dist2n2000_m, na.rm = TRUE)],
                   sleutels_oud_groot = length(oud_groot), objecten_oud_groot = d[projectid %in% oud_groot, .N])
rm(s, d); invisible(gc())

projecten <- function(dt) {
  pp <- dt[!is.na(gemeentecode), .(gm = gemeentecode[1L], gemeenten = uniqueN(gemeentecode), t = min(q), t_laatst = max(q),
                                     dmin = min(dist2n2000_m), dmed = as.numeric(median(dist2n2000_m)),
                                     omvang = projectomvang[1L], n = .N), by = projectid]
  stopifnot(all(pp$gemeenten == 1L))   # de sleutel bevat de gemeente
  pp[zuiver_sleutel, on = "projectid", zuiver := i.zuiver]
  pp[, `:=`(gemeenten = NULL, jaar = KW_START + (t - 1L) %/% 4L, na = as.integer(t >= Q_PAS),
            omvang_200 = pmin(omvang, 200L),
            klasse = cut(omvang, KLASSE_GRENZEN, labels = KLASSEN),
            klasse_n = cut(n, KLASSE_GRENZEN, labels = KLASSEN))]
  pp[]
}
P <- lapply(UIT, projecten)

## nabij en nabij na de uitspraak bij een drempel; wijzigt x zelf, dus geef
## een kopie mee (een deelselectie of copy())
zet_nabij <- function(x, dr, afstand = "dmin") {
  x[, D := as.integer(get(afstand) <= dr)]
  x[, nabij_na := D * na]
  x[]
}

R$meta <- list(
  extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
  q_pas = Q_PAS, kw_pas = kw_label(Q_PAS), q_omgevingswet = Q_OW, q_salderen = Q_SAL,
  laatste_kwartaal = lapply(Q_MAX, kw_label), venster_trendbreuk = sprintf("2012Q1 tot en met %s", kw_label(Q_SAL - 1L)),
  drempels = DREMPELS, klassen = KLASSEN, claim_conceptartikel_pct = CLAIM,
  definities = list(
    project = "gemeente plus documentnummer van de vergunning (projectid)",
    vergund = "in_sample, eerste bouwvergunning 2012Q1 tot en met 2026Q1; project telt in het kwartaal van zijn eerste vergunning",
    opgeleverd = "cbs_nieuwbouw zonder admin_fout, mutatiemaand 2012Q1 tot en met 2026Q2; project telt in het kwartaal van zijn eerste oplevering",
    nabij = "kleinste afstand van de objecten van het project in de steekproef tot habitat is hoogstens de drempel",
    omvang = "projectomvang: objecten met dezelfde projectsleutel in het hele bestand",
    woningen = "objecten van het project in de steekproef (vergund: woonobjecten met in_sample; opgeleverd: CBS-nieuwbouw)",
    spec_omvang = "feols(log(omvang) ~ nabij_na | gemeente^nabij + gemeente^kwartaal, cluster = ~gemeente) en fepois(omvang ~ ...)",
    spec_klassen = "fepois(projecten ~ tr | gemeente x nabij + gemeente x kwartaal + nabij x seizoen, cluster = ~gemeente), specificatie D van 08_strikt.R",
    interval = "95 procent; t met G-1 vrijheidsgraden bij OLS, normaal bij Poisson, zoals de p-waarde van fixest"))

## ---------------------------------------------------------------------
## 0. Data: projecten, spreiding in de tijd en de grootste projecten
## ---------------------------------------------------------------------
kop("0. Steekproeven en projecten")
R$data <- lapply(names(UIT), function(u) {
  pp <- P[[u]]; dt <- UIT[[u]]
  lang <- pp[t_laatst - t >= 16L]
  list(objecten = nrow(dt), objecten_zonder_gemeente = sum(is.na(dt$gemeentecode)), projecten = nrow(pp),
       projecten_over_meer_kwartalen = sum(pp$t_laatst > pp$t), woningen_daarin = sum(pp$n[pp$t_laatst > pp$t]),
       projecten_over_4_jaar_of_meer = nrow(lang), woningen_daarin_4_jaar = sum(lang$n),
       aandeel_omvang_gelijk_aan_woningen = mean(pp$omvang == pp$n), aandeel_zuiver_nieuwbouw = mean(pp$zuiver),
       omvang_kwantielen = as.list(quantile(pp$omvang, c(.25, .5, .75, .9, .99, .999))),
       grootste = pp[order(-omvang)][1:10, .(projectid, omvang, woningen = n, eerste = kw_label(t), laatste = kw_label(t_laatst), dmin)])
})
names(R$data) <- names(UIT)
for (u in names(UIT)) {
  x <- R$data[[u]]
  cat(sprintf("%-10s objecten %s (zonder gemeente %d), projecten %s; over meer kwartalen %s projecten met %s woningen, over 4 jaar of meer %s met %s\n",
              u, nfmt(x$objecten), x$objecten_zonder_gemeente, nfmt(x$projecten), nfmt(x$projecten_over_meer_kwartalen),
              nfmt(x$woningen_daarin), nfmt(x$projecten_over_4_jaar_of_meer), nfmt(x$woningen_daarin_4_jaar)))
  cat(sprintf("%-10s omvang gelijk aan woningen in de steekproef: %.1f%%; zuiver nieuwbouw: %.1f%%; omvang p25/p50/p75/p90/p99/p99,9: %s\n",
              u, 100 * x$aandeel_omvang_gelijk_aan_woningen, 100 * x$aandeel_zuiver_nieuwbouw,
              paste(round(unlist(x$omvang_kwantielen), 1), collapse = " / ")))
  print(x$grootste)
}

## ---------------------------------------------------------------------
## 1. Beschrijvend
## ---------------------------------------------------------------------
kop("1. Projecten en omvang, nabij en ver, voor en na 2019Q2")
R$beschrijvend <- rbindlist(lapply(names(P), function(u) rbindlist(lapply(DREMPELS, function(dr) {
  x <- P[[u]][, .(projecten = .N, objecten = sum(omvang), gem_omvang = mean(omvang), med_omvang = as.numeric(median(omvang)),
                  gem_log_omvang = mean(log(omvang)), aandeel_een_object = mean(omvang == 1L),
                  gem_woningen = mean(n), med_woningen = as.numeric(median(n))),
              keyby = .(nabij = as.integer(dmin <= dr), na)]
  cbind(uitkomst = u, drempel = dr, x)
}))))
print(R$beschrijvend[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 100)

## Ruwe verschil-in-verschil zonder vaste effecten: het log van de
## gemiddelde omvang en het gemiddelde van de log-omvang
rw <- dcast(R$beschrijvend, uitkomst + drempel ~ nabij + na, value.var = c("gem_omvang", "gem_log_omvang"))
R$ruwe_did <- rw[, .(uitkomst, drempel,
                     log_gemiddelde = log(gem_omvang_1_1 / gem_omvang_1_0) - log(gem_omvang_0_1 / gem_omvang_0_0),
                     gemiddelde_log = (gem_log_omvang_1_1 - gem_log_omvang_1_0) - (gem_log_omvang_0_1 - gem_log_omvang_0_0))]
cat("\nruw verschil-in-verschil, zonder vaste effecten (log-punten):\n")
print(R$ruwe_did[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## Schatters
## ---------------------------------------------------------------------
## Een rij met coefficient, standaardfout, p en 95%-interval; confint()
## gebruikt dezelfde verdeling als de p-waarde van fixest.
rij <- function(m, term) {
  if (is.null(m) || !term %in% names(coef(m)))
    return(data.table(b = NA_real_, se = NA_real_, p = NA_real_, lo = NA_real_, hi = NA_real_, n = NA_integer_))
  ct <- coeftable(m); ci <- confint(m, level = 0.95)
  data.table(b = ct[term, 1], se = ct[term, 2], p = ct[term, 4], lo = ci[term, 1], hi = ci[term, 2], n = nobs(m))
}
## Projectniveau: vaste effecten gemeente x nabij en gemeente x kwartaal
schat <- function(x, y, schatter, rhs) {
  lhs <- if (schatter == "log-OLS") sprintf("log(%s)", y) else y
  f <- as.formula(sprintf("%s ~ %s | gm^D + gm^t", lhs, rhs))
  tryCatch(if (schatter == "log-OLS") feols(f, data = x, cluster = ~gm, notes = FALSE)
           else fepois(f, data = x, cluster = ~gm, notes = FALSE, glm.iter = 200),
           error = function(e) { cat("  mislukt:", conditionMessage(e), "\n"); NULL })
}
procent <- function(dt) dt[, `:=`(pct = 100 * (exp(b) - 1), pct_lo = 100 * (exp(lo) - 1), pct_hi = 100 * (exp(hi) - 1))][]
toon <- function(dt) dt[, .(uitkomst, drempel, b = round(b, 3), se = round(se, 3), p = round(p, 3),
                            interval = sprintf("[%+.3f; %+.3f]", lo, hi),
                            procent = sprintf("%+.1f%% [%+.1f; %+.1f]", pct, pct_lo, pct_hi), n)]

## ---------------------------------------------------------------------
## 2. De omvang als uitkomst
## ---------------------------------------------------------------------
kop("2. De omvang als uitkomst: nabij na 2019Q2, binnen gemeente x nabij en gemeente x kwartaal")
VARIANTEN <- list(
  "hoofd"                     = list(y = "omvang",     afstand = "dmin", filter = quote(TRUE)),
  "woningen in de steekproef" = list(y = "n",          afstand = "dmin", filter = quote(TRUE)),
  "alleen zuivere nieuwbouw"  = list(y = "omvang",     afstand = "dmin", filter = quote(zuiver == TRUE)),
  "nabij op mediane afstand"  = list(y = "omvang",     afstand = "dmed", filter = quote(TRUE)),
  "tot en met 2023Q4"         = list(y = "omvang",     afstand = "dmin", filter = quote(t < Q_OW)),
  "omvang afgetopt op 200"    = list(y = "omvang_200", afstand = "dmin", filter = quote(TRUE)),
  "zonder sleutels over vier jaar of meer" = list(y = "omvang", afstand = "dmin", filter = quote(t_laatst - t < 16L)))
R$omvang <- rbindlist(lapply(names(P), function(u) rbindlist(lapply(names(VARIANTEN), function(vn) {
  v <- VARIANTEN[[vn]]
  rbindlist(lapply(DREMPELS, function(dr) {
    x <- zet_nabij(P[[u]][eval(v$filter)], dr, v$afstand)
    ## hoeveel nabij-projecten na de uitspraak liggen in een gemeente-kwartaal
    ## met ook een project ver weg: alleen die dragen de vergelijking
    info <- data.table(projecten = nrow(x), projecten_nabij_na = sum(x$nabij_na),
                       gemeenten_nabij_en_ver = x[, uniqueN(D), by = gm][V1 == 2L, .N],
                       nabij_na_met_ver_in_kwartaal = x[, .(k = if (any(D == 0L)) sum(nabij_na) else 0L), by = .(gm, t)][, sum(k)])
    rbindlist(lapply(SCHATTERS, function(sc) cbind(uitkomst = u, variant = vn, drempel = dr, schatter = sc,
                                                   rij(schat(x, v$y, sc, "nabij_na"), "nabij_na"), info)))
  }))
}))))
R$omvang <- procent(R$omvang)
for (sc in SCHATTERS) {
  cat(sprintf("\n%s (coefficient in log-punten; procent = exp(b) - 1)\n", sc))
  print(R$omvang[schatter == sc, cbind(variant = variant, toon(.SD), nabij_na = projecten_nabij_na, met_ver = nabij_na_met_ver_in_kwartaal)], nrows = 200)
}

## ---------------------------------------------------------------------
## 3. Event study per jaar op 2.500 m
## ---------------------------------------------------------------------
kop("3. Event study per jaar op 2.500 m: nabij maal jaar, 2018 = 0, binnen de gemeente")
es_tabel <- function(m) {
  ct <- coeftable(m); ci <- confint(m, level = 0.95)
  tab <- data.table(jaar = as.integer(sub("^jaar::([0-9]+):D$", "\\1", rownames(ct))),
                    b = ct[, 1], se = ct[, 2], p = ct[, 4], lo = ci[, 1], hi = ci[, 2])
  rbind(tab, data.table(jaar = PARAMS$referentie_jaar, b = 0, se = 0, p = NA_real_, lo = 0, hi = 0))[order(jaar)]
}
## gezamenlijke toets dat alle jaren voor 2018 nul zijn (chi-kwadraat)
wald_voor <- function(m) {
  b <- coef(m); V <- vcov(m)
  idx <- which(as.integer(sub("^jaar::([0-9]+):D$", "\\1", names(b))) < PARAMS$referentie_jaar)
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  data.table(stat = W, df = length(idx), p = pchisq(W, length(idx), lower.tail = FALSE))
}
## de hoofdspecificatie, en ter controle zonder de sleutels die over vier
## jaar of meer lopen (het pseudo-project in Den Haag ligt in 2012)
ES_VARIANTEN <- c("hoofd", "zonder sleutels over vier jaar of meer")
R$eventstudy <- list(); R$eventstudy_voortoets <- list()
for (vn in ES_VARIANTEN) for (u in names(P)) for (sc in SCHATTERS) {
  x <- zet_nabij(P[[u]][eval(VARIANTEN[[vn]]$filter)], 2500)
  m <- schat(x, "omvang", sc, sprintf("i(jaar, D, ref = %d)", PARAMS$referentie_jaar))
  if (is.null(m)) next
  R$eventstudy[[paste(vn, u, sc)]] <- cbind(variant = vn, uitkomst = u, schatter = sc, es_tabel(m), n = nobs(m))
  R$eventstudy_voortoets[[paste(vn, u, sc)]] <- cbind(variant = vn, uitkomst = u, schatter = sc, wald_voor(m))
}
R$eventstudy <- rbindlist(R$eventstudy)
R$eventstudy[, `:=`(deeljaar = jaar == 2026L, pct = 100 * (exp(b) - 1))]
R$eventstudy_voortoets <- rbindlist(R$eventstudy_voortoets)
for (vn in ES_VARIANTEN) {
  cat(sprintf("\n%s\n", vn))
  print(dcast(R$eventstudy[variant == vn, .(jaar, kolom = paste(uitkomst, schatter),
                                            w = fifelse(jaar == PARAMS$referentie_jaar, "referentie",
                                                        sprintf("%+.3f (%.3f) p=%.2f", b, se, p)))], jaar ~ kolom, value.var = "w"))
}
cat("2026 is een deeljaar: vergund tot en met Q1, opgeleverd tot en met Q2; intervallen staan in de JSON\n")
cat("\ngezamenlijke toets 2012 tot en met 2017 = 0:\n")
print(R$eventstudy_voortoets[, .(variant, uitkomst, schatter, stat = round(stat, 2), df, p = round(p, 3))])

## ---------------------------------------------------------------------
## 3b. Trendbreuk per drempel, tot en met 2024Q3
## ---------------------------------------------------------------------
## Zoals fit_trendbreuk() in 08_strikt.R: tj in jaren ten opzichte van de
## uitspraak; voortrend = trend nabij tegenover ver over het hele venster,
## sprong = niveauverschuiving bij de uitspraak, natrend = verandering van
## de trend erna. Het venster eindigt voor de uitspraak over intern
## salderen, zoals E en F in 08_strikt.R. De DiD in hetzelfde venster staat
## erbij, zodat duidelijk is wat de trendtermen eraan veranderen.
kop("3b. Trendbreuk per drempel, 2012Q1 tot en met 2024Q3: voortrend, sprong bij de uitspraak, natrend")
R$trendbreuk <- rbindlist(lapply(names(P), function(u) rbindlist(lapply(DREMPELS, function(dr) {
  x <- zet_nabij(P[[u]][t < Q_SAL], dr)
  x[, tj := (t - Q_PAS) / 4]
  x[, `:=`(voortrend = D * tj, sprong = nabij_na, natrend = nabij_na * tj)]
  rbindlist(lapply(SCHATTERS, function(sc) {
    m_did <- schat(x, "omvang", sc, "nabij_na")
    m_tb  <- schat(x, "omvang", sc, "voortrend + sprong + natrend")
    rbind(cbind(uitkomst = u, drempel = dr, schatter = sc, term = "DiD", rij(m_did, "nabij_na")),
          rbindlist(lapply(c("voortrend", "sprong", "natrend"), function(v)
            cbind(uitkomst = u, drempel = dr, schatter = sc, term = v, rij(m_tb, v)))))
  }))
}))))
cat("voortrend en natrend in log-punten per jaar; DiD en sprong in log-punten\n")
print(dcast(R$trendbreuk[, .(uitkomst, schatter, drempel, term = factor(term, levels = c("DiD", "voortrend", "sprong", "natrend")),
                             w = sprintf("%+.3f (%.3f) p=%.3f", b, se, p))],
            uitkomst + schatter + drempel ~ term, value.var = "w"))

## ---------------------------------------------------------------------
## 4. De verdeling over omvangsklassen
## ---------------------------------------------------------------------
kop("4a. Projecten per omvangsklasse, nabij en ver, voor en na 2019Q2")
R$klassen <- rbindlist(lapply(names(P), function(u) rbindlist(lapply(DREMPELS, function(dr) {
  a <- P[[u]][, .(projecten = .N, objecten = sum(omvang)), by = .(nabij = as.integer(dmin <= dr), na, klasse)]
  g <- CJ(nabij = 0:1, na = 0:1, klasse = factor(KLASSEN, levels = KLASSEN))
  a <- merge(g, a, by = c("nabij", "na", "klasse"), all.x = TRUE)
  a[is.na(projecten), `:=`(projecten = 0L, objecten = 0L)]
  a[, `:=`(aandeel_projecten = projecten / sum(projecten), aandeel_objecten = objecten / sum(objecten)), by = .(nabij, na)]
  cbind(uitkomst = u, drempel = dr, a)
}))))
## verschuiving in het aandeel projecten, in procentpunten: (nabij na min
## nabij voor) min (ver na min ver voor)
vs <- dcast(R$klassen, uitkomst + drempel + klasse ~ nabij + na, value.var = "aandeel_projecten")
R$klassen_verschuiving <- vs[, .(uitkomst, drempel, klasse, ver_voor = 100 * `0_0`, ver_na = 100 * `0_1`,
                                 nabij_voor = 100 * `1_0`, nabij_na = 100 * `1_1`,
                                 verschuiving_pp = 100 * ((`1_1` - `1_0`) - (`0_1` - `0_0`)))]
print(R$klassen_verschuiving[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], nrows = 100)
cat("\naantal projecten per klasse (nabij_na: 1_1 = nabij en na):\n")
print(dcast(R$klassen, uitkomst + drempel + klasse ~ nabij + na, value.var = "projecten"), nrows = 100)

kop("4b. Poisson-DiD per omvangsklasse, gemeente x nabij x kwartaal (specificatie D van 08_strikt.R)")
## y = aantal projecten (of woningen) met hun eerste kwartaal t in de cel;
## het volle paneel met nullen, zoals paneel_gemeente() in 08_strikt.R.
## In de klasse van 200 en meer liggen zo weinig projecten nabij dat de
## Poisson soms scheidt: een coefficient boven 10 in absolute waarde of een
## standaardfout van nul. Die cellen krijgen geen schatting maar een
## status; gemeenten_met_nabij zegt op hoeveel gemeenten een schatting rust.
cel_did <- function(x, u, w) {
  cel <- data.table(gm = x$gm, D = x$D, t = x$t, w = w)[, .(y = sum(w)), by = .(gm, D, t)]
  vol <- CJ(gm = sort(unique(P[[u]]$gm)), D = 0:1, t = 1:Q_MAX[[u]])
  p <- merge(vol, cel, by = c("gm", "D", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p[, `:=`(gem_D = paste(gm, D), gem_t = paste(gm, t), D_szn = paste(D, (t - 1L) %% 4L), tr = D * as.integer(t >= Q_PAS))]
  m <- tryCatch(fepois(y ~ tr | gem_D + gem_t + D_szn, data = p, cluster = ~gm, notes = FALSE, glm.iter = 200),
                error = function(e) NULL)
  r <- rij(m, "tr")
  status <- if (is.null(m)) "mislukt" else if (is.na(r$b)) "geen term" else
    if (abs(r$b) > 10 || r$se < 1e-6) "niet schatbaar (scheiding)" else "geschat"
  if (status != "geschat") r[, `:=`(b = NA_real_, se = NA_real_, p = NA_real_, lo = NA_real_, hi = NA_real_)]
  cbind(r, status = status, eenheden = sum(w), eenheden_nabij_na = sum(w[x$nabij_na == 1L]),
        gemeenten_met_nabij = uniqueN(x$gm[w > 0 & x$D == 1L]))
}
R$klassen_did <- rbindlist(lapply(names(P), function(u) rbindlist(lapply(DREMPELS, function(dr) {
  x <- zet_nabij(copy(P[[u]]), dr)
  rijen <- list(cbind(maat = "alle projecten", klasse = "alle", cel_did(x, u, rep(1L, nrow(x)))),
                cbind(maat = "woningen in de steekproef", klasse = "alle", cel_did(x, u, x$n)))
  for (kv in c("klasse", "klasse_n")) for (k in KLASSEN) {
    mt <- if (kv == "klasse") "projecten, klasse naar projectomvang" else "projecten, klasse naar woningen"
    rijen[[length(rijen) + 1L]] <- cbind(maat = mt, klasse = k, cel_did(x, u, as.integer(x[[kv]] == k)))
  }
  cbind(uitkomst = u, drempel = dr, rbindlist(rijen))
}))))
R$klassen_did <- procent(R$klassen_did)
for (mt in unique(R$klassen_did$maat)) {
  cat(sprintf("\n%s\n", mt))
  print(R$klassen_did[maat == mt, cbind(klasse = klasse, toon(.SD), eenheden = eenheden, nabij_na = eenheden_nabij_na,
                                        gem_nabij = gemeenten_met_nabij, status = status)], nrows = 100)
}

kop("4c. Splitsing: woningen = projecten x gemiddelde omvang")
## rekenkundige splitsing van de puntschattingen, geen eigen standaardfout;
## de toets op het tweede kanaal zelf is de Poisson op de woningen per
## project in onderdeel 2
sp <- dcast(R$klassen_did[klasse == "alle"], uitkomst + drempel ~ maat, value.var = "b")
setnames(sp, c("alle projecten", "woningen in de steekproef"), c("b_projecten", "b_woningen"))
om <- R$omvang[variant == "woningen in de steekproef" & schatter == "Poisson",
               .(uitkomst, drempel, b_omvang_project = b, se_omvang_project = se, p_omvang_project = p)]
R$splitsing <- merge(sp, om, by = c("uitkomst", "drempel"))
R$splitsing[, b_woningen_min_projecten := b_woningen - b_projecten]
print(R$splitsing[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## 5. Toets van het conceptartikel: 7 tot 13 procent minder op 500 en 1.000 m
## ---------------------------------------------------------------------
kop("5. Is een afname van 7 tot 13 procent op 500 en 1.000 m zichtbaar?")
MDE_Z <- qnorm(0.975) + qnorm(0.80)   # kleinste afname die met 80% kans significant wordt
t5 <- rbind(
  R$omvang[variant %in% c("hoofd", "woningen in de steekproef") & drempel %in% c(500, 1000),
           .(uitkomst, drempel, wat = paste("omvang per project,", variant), schatter, b, se, p, lo, hi, pct, pct_lo, pct_hi)],
  R$klassen_did[klasse == "alle" & drempel %in% c(500, 1000),
                .(uitkomst, drempel, wat = paste("aantal,", maat), schatter = "Poisson op cellen", b, se, p, lo, hi, pct, pct_lo, pct_hi)])
t5[, `:=`(kleinste_aantoonbare_afname_pct = 100 * (1 - exp(-MDE_Z * se)),
          min7_in_interval = pct_lo <= -7 & pct_hi >= -7,
          min13_in_interval = pct_lo <= -13 & pct_hi >= -13,
          nul_in_interval = pct_lo <= 0 & pct_hi >= 0)]
t5[, oordeel := fcase(
  is.na(b), "geen schatting",
  pct_lo > -7, "een afname van 7 procent of meer valt buiten het interval",
  pct_hi < -13, "afname groter dan 13 procent",
  pct_lo > -13 & pct_hi < -7, "afname tussen 7 en 13 procent",
  pct_lo > -13, "7 procent past binnen het interval, 13 procent niet",
  pct_hi < -7, "13 procent past binnen het interval, 7 procent niet",
  pct_hi < 0, "significante afname; 7 tot 13 procent past binnen het interval",
  default = "niet significant; 7 tot 13 procent past binnen het interval, geen verandering ook")]
R$toets_7_13 <- t5
print(t5[, .(uitkomst, drempel, wat, schatter, b = round(b, 3), se = round(se, 3), p = round(p, 3),
             procent = sprintf("%+.1f%% [%+.1f; %+.1f]", pct, pct_lo, pct_hi),
             mde80 = round(kleinste_aantoonbare_afname_pct, 1), oordeel)], nrows = 100)

## ---------------------------------------------------------------------
## 6. Het pseudoproject in Den Haag en de telling van hoofdstuk 8
## ---------------------------------------------------------------------
## Een sleutel in Den Haag (518 FV20101220-04) bundelt 2.430 objecten in 968
## panden met vergunningen van 1971 tot en met 2011, opgeleverd in 2012 tot en
## met 2017, op 118 m van habitat (mediane afstand 3.123 m). Omdat een project
## in hoofdstuk 8 op zijn dichtstbijzijnde object telt, staan die objecten daar
## als nabij in de voorperiode. Hier de DiD van 08_strikt.R (specificatie D,
## opleveringen, 2.500 m) met en zonder dat project, zonder alle sleutels van
## 100 objecten of meer met een vergunning van voor 2005, en met elk object op
## zijn eigen afstand in plaats van de kleinste van het project.
kop("6. Het pseudoproject in Den Haag en de DiD van hoofdstuk 8 (opleveringen, binnen gemeente en kwartaal)")
nwb6 <- copy(nwb)
paneel6 <- function(dt) {
  dt <- dt[!is.na(gemeentecode)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(G = as.integer(dist2n2000_m[1L] <= 2500), n = .N), by = .(projectid, gemeentecode, q)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, G, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), G = 0:1, t = 1:Q_MAX[["opgeleverd"]])
  p <- merge(vol, cel, by = c("gemeentecode", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(gemeentecode, G), E_t = paste(gemeentecode, t), G_szn = paste(G, (t - 1L) %% 4L), tr = G * as.integer(t >= Q_PAS))]
  p[]
}
did6 <- function(dt, naam) {
  m <- fepois(y ~ tr | E_G + E_t + G_szn, data = paneel6(dt), cluster = ~gemeentecode, notes = FALSE, glm.iter = 200)
  data.table(variant = naam, b = coef(m)[["tr"]], se = se(m)[["tr"]], p = pvalue(m)[["tr"]], objecten = nrow(dt))
}
R$pseudo <- rbindlist(list(
  did6(nwb6, "alle (08_strikt.R, D)"),
  did6(nwb6[projectid != "518 FV20101220-04"], "zonder het pseudoproject in Den Haag"),
  did6(nwb6[!projectid %in% oud_groot], "zonder alle sleutels van 100 of meer met een vergunning voor 2005"),
  did6(copy(nwb6)[, projectid := paste(projectid, vbo_bag_nr)], "elk object op zijn eigen afstand")))
R$pseudo[, pct := 100 * (exp(b) - 1)]
R$pseudo_project <- c(pseudo_tel, list(objecten_opgeleverd = nwb6[projectid == "518 FV20101220-04", .N],
                                       kleinste_afstand_opgeleverd = nwb6[projectid == "518 FV20101220-04", min(dist2n2000_m)],
                                       mediane_afstand_opgeleverd = nwb6[projectid == "518 FV20101220-04", median(dist2n2000_m)]))
print(R$pseudo[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)]); str(R$pseudo_project)

## ---------------------------------------------------------------------
## Samenvatting
## ---------------------------------------------------------------------
kop("Samenvatting: omvang per project, hoofdspecificatie")
R$samenvatting <- R$omvang[variant == "hoofd", .(uitkomst, drempel, schatter, b, se, p, lo, hi, pct, pct_lo, pct_hi, n)]
print(R$samenvatting[, cbind(schatter = schatter, toon(.SD))])

## ---------------------------------------------------------------------
## Wegschrijven
## ---------------------------------------------------------------------
json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("omvang_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec: %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("omvang_%s.json", EXTRACT)))
sink()
