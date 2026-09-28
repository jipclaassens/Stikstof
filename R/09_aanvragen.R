## =====================================================================
## 09_aanvragen.R -- de aanvraagkant: gemeentelijke bekendmakingen van
##                   omgevingsvergunningen, met de strenge vergelijking
##
## Input : <PATHS$verg>/analyse/zaken.csv
##           een rij per zaak van een gemeente met een aanvraagpublicatie,
##           uit vergunningen/pipeline/analyse_export.py
##         <PATHS$verg>/analyse/panel.csv          (alleen voor de brug)
##         _derived/aanvragen_nieuwbouw_streng.csv (09a_titels.py)
## Output: _output/aanvragen_<extract>.json   (voor het rapport)
##         _output/aanvragen_<extract>.txt    (leesbare log)
##
## Waarom
## ---------------------------------------------------------------------
## De BAG ziet alleen vergunningen die verleend zijn en een nieuw pand
## opleveren. Een stikstofrem die zich uit in niet aanvragen, intrekken,
## weigeren of langer wachten, is daar onzichtbaar. De bekendmakingen zien
## elke aanvraag, landelijk vanaf juli 2021 en daarvoor alleen bij de
## gemeenten die hun kennisgevingen al in het elektronische Gemeenteblad
## zetten. Dit script schat op die zaken met de strenge vergelijking van
## 08_strikt.R (nabij en ver binnen dezelfde gemeente en hetzelfde
## kwartaal), en met vier correcties op vergunningen/pipeline/analyse.R en
## analyse_vastpanel.R:
##   1. het paneel is volledig: een cel zonder aanvragen telt als nul
##      (panel.csv bevat alleen cellen met minstens een aanvraag);
##   2. zaken zonder locatie tellen niet als ver (analyse_export.py zet
##      nabij op 0 als de afstand ontbreekt);
##   3. effecten als IRR, exp(b), en niet als 100 x b;
##   4. nieuwbouw van woningen ook streng getypeerd (09a_titels.py).
## De PAS-uitspraak wordt getoetst op 2016Q1 tot en met 2021Q2, voordat de
## Wet elektronische publicaties (1 juli 2021) de publicatievorm veranderde
## en de bouwvrijstelling inging; de latere wisselingen op alle gemeenten
## vanaf 2021Q3. Kwartaalconventie als in 06_rapport.R en 08_strikt.R: het
## kwartaal van de wisseling telt als na; de brug laat zien wat het eerste
## volle kwartaal zou geven.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("aanvragen_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()

## ---------------------------------------------------------------------
## Tijdindex en wisselingen: dezelfde als in 08_strikt.R
## ---------------------------------------------------------------------
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_van_label <- function(k) (as.integer(substr(k, 1, 4)) - KW_START) * 4L + as.integer(substr(k, 6, 6))   # "2016K2"
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q_REF   <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_REF_L <- kw_index(as.Date(c("2021-10-01", "2022-01-01", "2022-04-01", "2022-07-01")))   # het jaar voor Porthos
Q_PAS  <- kw_index(as.Date("2019-05-29"))
Q_BVR  <- kw_index(as.Date("2021-07-01"))   # bouwvrijstelling en nieuwe publicatievorm
Q_POR  <- kw_index(as.Date("2022-11-02"))
Q_OW   <- kw_index(as.Date("2024-01-01"))
Q_SAL  <- kw_index(as.Date("2024-12-18"))
Q_A0   <- kw_index(as.Date("2016-01-01"))
Q_EIND <- kw_index(as.Date("2026-06-30"))   # 2026Q3 loopt nog
WISSELS   <- c("na_pas", "na_bvr", "na_por", "na_ow", "na_sal")
WISSELS_L <- c("na_por", "na_ow", "na_sal")

## ---------------------------------------------------------------------
## Zaken
## ---------------------------------------------------------------------
kop("Zaken inlezen")
f_zaken <- file.path(PATHS$verg, "analyse", "zaken.csv")
if (!file.exists(f_zaken)) stop("Ontbreekt: ", f_zaken, "\nZet VERGUNNINGEN_DATA op de datamap van het vergunningenspoor.")
z <- fread(f_zaken, sep = ";", encoding = "UTF-8",
           select = c("zaak_id", "gemeente", "datum_aanvraag", "kwartaal", "dist_m", "nabij", "verleend", "geweigerd",
                      "ingetrokken", "vergunningvrij", "besluit_onbekend", "geen", "doorlooptijd", "verlengingen",
                      "bouw", "uitgebreid", "woningen"))
z[, `:=`(t = kw_van_label(kwartaal), jaar = as.integer(substr(kwartaal, 1, 4)))]
z_ruw <- copy(z)   # voor de brug: precies wat analyse_vastpanel.R ziet (alle aanvragen, zonder soort)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               zaken = nrow(z), zonder_locatie = sum(is.na(z$dist_m)),
               eerste_aanvraag = min(z$datum_aanvraag), laatste_aanvraag = max(z$datum_aanvraag),
               q_ref = Q_REF, q_ref_l = Q_REF_L, q_pas = Q_PAS, q_bvr = Q_BVR, q_por = Q_POR, q_ow = Q_OW,
               q_sal = Q_SAL, q_a0 = Q_A0, q_eind = Q_EIND)
z <- z[!is.na(dist_m) & t <= Q_EIND]
z[, D := as.integer(dist_m <= 2500)]

## Een standaardpunt: veel zaken van een gemeente op precies dezelfde
## afstand (Hardenberg 688 zaken op 7.496 m). Onder de 100 m is een
## herhaalde afstand de rasterstap van 20 m bij adressen tegen habitat aan,
## vanaf 9.999 m de afkapping van het afstandsraster; die tellen niet.
z[, n_zelfde := .N, by = .(gemeente, dist_m)]
z[, stdpunt := dist_m > 100 & dist_m < 9999 & n_zelfde >= 25 & n_zelfde >= 0.005 * .N, by = gemeente]

## de soort komt uit 09a_titels.py (analyse_export.projectsoort, met de woordgrens van 28 september 2026), niet uit
## zaken.csv, dat tot de volgende herbouw van de database de oude soort heeft (vergunningen/inzichten.md, hoofdstuk 36)
f_tit <- file.path(PATHS$derived, "aanvragen_titels.csv")
if (!file.exists(f_tit) || file.mtime(f_tit) < file.mtime(f_zaken) || !"soort" %in% names(fread(f_tit, nrows = 0L))) {
  ## de typering uit de titels (09a_titels.py) leest vergunningen.sqlite; standaard-Python volstaat
  py <- Sys.getenv("PYTHON", "py")
  args <- c(if (py == "py") "-3.13", shQuote(file.path(.here, "09a_titels.py")), shQuote(PATHS$verg), shQuote(f_tit))
  status <- system2(py, args)
  if (status != 0 || !file.exists(f_tit)) stop("Ontbreekt: ", f_tit, "\nDraai eerst: py -3.13 R/09a_titels.py")
}
z <- merge(z, fread(f_tit), by = "zaak_id", all.x = TRUE)
z[, nb_streng := as.integer(soort == "nieuwbouw_woning" & streng %in% 1L & !(tijdelijk %in% 1L))]
z[, log_dl := fifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
z[, `:=`(D_szn = paste(D, (t - 1L) %% 4L), gem_D = paste(gemeente, D), gem_t = paste(gemeente, t))]
R$meta$standaardpunt <- sum(z$stdpunt)
R$meta$nieuwbouw_titel <- sum(z$soort == "nieuwbouw_woning")
R$meta$nieuwbouw_streng <- sum(z$nb_streng)
R$meta$veestal <- sum(z$veestal)
cat(sprintf("zaken %s, zonder locatie %s, standaardpunt %s, nieuwbouw titel %s, streng %s\n", nfmt(R$meta$zaken),
            nfmt(R$meta$zonder_locatie), nfmt(R$meta$standaardpunt), nfmt(R$meta$nieuwbouw_titel), nfmt(R$meta$nieuwbouw_streng)))

SOORT <- list(
  alle            = list(naam = "alle aanvragen",                        sel = quote(TRUE)),
  nieuwbouw       = list(naam = "nieuwbouw van woningen, streng",        sel = quote(nb_streng == 1L)),
  nieuwbouw_titel = list(naam = "nieuwbouw van woningen, titel (ruim)",  sel = quote(soort == "nieuwbouw_woning")),
  uitgebreid      = list(naam = "uitgebreide procedure",                 sel = quote(uitgebreid == 1L)),
  veestal         = list(naam = "stallen en mest (positieve controle)",  sel = quote(veestal == 1L)),
  klein           = list(naam = "klein werk en verbouw (placebo)",       sel = quote(soort == "klein_verbouw")))

## ---------------------------------------------------------------------
## Panelen van gemeenten
## ---------------------------------------------------------------------
## Per gemeente is de reeks alles of niets: een gemeente publiceert haar
## kennisgevingen in het Gemeenteblad of niet. A en C zijn de vaste panelen
## van analyse_vastpanel.R; L is het landelijk dekkende venster.
per_jaar <- z[, .N, by = .(gemeente, jaar)]
vast <- function(jaren, minimum) sort(per_jaar[jaar %in% jaren & N >= minimum, .N, by = gemeente][N == length(jaren), gemeente])
PANEL <- list(
  A = list(naam = "vast paneel A: minstens 40 aanvragen in elk jaar 2016 tot en met 2020", gem = vast(2016:2020, 40), van = Q_A0),
  C = list(naam = "vast paneel C: minstens 20 aanvragen in elk jaar 2018 tot en met 2020", gem = vast(2018:2020, 20),
           van = kw_index(as.Date("2018-01-01"))),
  L = list(naam = "alle gemeenten vanaf 2021Q3", gem = sort(unique(z[t >= Q_BVR, gemeente])), van = Q_BVR))
## Elke gemeente alleen in de kwartalen waarin ze publiceerde: een gemeente
## die opging in een fusie telt daarna niet als nul.
actief <- z[, .(t_eerst = min(t), t_laatst = max(t)), by = gemeente]

tel_paneel <- function(pn, s, tot = Q_EIND, extra = quote(TRUE), klasse = NULL) {
  g <- PANEL[[pn]]
  dt <- z[gemeente %in% g$gem & t >= g$van & t <= tot][eval(extra)][eval(SOORT[[s]]$sel)]
  if (!is.null(klasse)) dt[, D := klasse(dist_m)]
  cel <- dt[, .(y = .N), by = .(gemeente, D, t)]
  vol <- CJ(gemeente = g$gem, D = if (is.null(klasse)) 0:1 else 0:3, t = g$van:tot)
  vol <- merge(vol, actief, by = "gemeente")[t >= t_eerst & t <= t_laatst][, c("t_eerst", "t_laatst") := NULL]
  p <- merge(vol, cel, by = c("gemeente", "D", "t"), all.x = TRUE)
  p[is.na(y), y := 0L]
  p[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L), Dt = D * t)]
  p[]
}

## ---------------------------------------------------------------------
## Schatters
## ---------------------------------------------------------------------
FE <- list(B = "gem_D + t + D_szn", D = "gem_D + gem_t + D_szn", E = "gem_D + gem_t + D_szn + gemeente[Dt]")
coefrij <- function(m, term) {
  ct <- coeftable(m)
  data.table(term = term, b = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}
zet_wissels <- function(p) invisible(p[, `:=`(na_pas = D * as.integer(t >= Q_PAS), na_bvr = D * as.integer(t >= Q_BVR),
                                     na_por = D * as.integer(t >= Q_POR), na_ow = D * as.integer(t >= Q_OW),
                                     na_sal = D * as.integer(t >= Q_SAL))])
fit_termen <- function(p, termen, fe, y = "y", fam = c("pois", "ols")) {
  fam <- match.arg(fam)
  f <- as.formula(sprintf("%s ~ %s | %s", y, paste(termen, collapse = " + "), fe))
  m <- if (fam == "pois") fepois(f, data = p, cluster = ~gemeente, notes = FALSE, glm.iter = 200)
       else feols(f, data = p, cluster = ~gemeente, notes = FALSE)
  rbindlist(lapply(termen, function(v) coefrij(m, v)))
}
fit_trendbreuk <- function(p, fe) {
  p[, tj := (t - Q_PAS) / 4]
  p[, `:=`(voortrend = D * tj, sprong = D * as.integer(t >= Q_PAS), natrend = D * as.integer(t >= Q_PAS) * tj)]
  fit_termen(p, c("voortrend", "sprong", "natrend"), fe)
}
fit_es <- function(p, fe, ref) {
  m <- fepois(as.formula(sprintf("y ~ i(t, D, ref = c(%s)) | %s", paste(ref, collapse = ","), fe)),
              data = p, cluster = ~gemeente, notes = FALSE, glm.iter = 200)
  cf <- coef(m); se <- se(m)
  tt <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  tab <- rbind(data.table(t = tt, b = cf, se = se), data.table(t = ref, b = 0, se = 0))[order(t)]
  tab[, `:=`(kw = kw_label(t), irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  list(model = m, tab = tab)
}
wald_pre <- function(m, ref) {
  b <- coef(m); V <- vcov(m)
  tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(tt < min(ref))
  if (!length(idx)) return(list(stat = NA_real_, df = 0L, p = NA_real_))
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  list(stat = W, df = length(idx), p = pchisq(W, length(idx), lower.tail = FALSE))
}
## Jaargemiddelden van de kwartaalcoefficienten, met de covariantie; de
## referentiekwartalen tellen mee met nul.
jaar_tabel <- function(es) {
  b <- coef(es$model); V <- vcov(es$model); tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  rbindlist(lapply(sort(unique(substr(es$tab$kw, 1, 4))), function(j) {
    qs <- es$tab[substr(kw, 1, 4) == j, t]
    idx <- which(tt %in% qs)
    if (!length(idx)) return(data.table(jaar = j, b = 0, se = NA_real_, kwartalen = length(qs)))
    w <- rep(1 / length(qs), length(idx))
    data.table(jaar = j, b = sum(w * b[idx]), se = sqrt(as.numeric(t(w) %*% V[idx, idx, drop = FALSE] %*% w)), kwartalen = length(qs))
  }))[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se), p = 2 * pnorm(-abs(b / se)))][]
}
## Kleinste aantoonbare effect bij 80% onderscheidend vermogen en 5%
## tweezijdig: 2,8 standaardfouten; voor aantallen als daling in procenten.
mde_log <- function(se) 1 - exp(-2.8 * se)
toon <- function(x, label) {
  x <- copy(x)
  cat(label, "\n")
  print(x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)])
}

## ---------------------------------------------------------------------
## 1. Dekking
## ---------------------------------------------------------------------
kop("1. Dekking en panelen")
R$dekking <- z_ruw[, .(zaken = .N, gemeenten = uniqueN(gemeente)), by = jaar][order(jaar)]
R$panelen <- lapply(PANEL, function(g) {
  zz <- z[gemeente %in% g$gem & t >= g$van]
  list(naam = g$naam, gemeenten = length(g$gem), van = kw_label(g$van), tot = kw_label(Q_EIND),
       zaken = nrow(zz), nabij = sum(zz$D), nieuwbouw = sum(zz$nb_streng), nieuwbouw_nabij = sum(zz$nb_streng * zz$D),
       klein = sum(zz$soort == "klein_verbouw"), uitgebreid = sum(zz$uitgebreid))
})
str(R$panelen)
R$A_jaar <- z[gemeente %in% PANEL$A$gem & t >= Q_A0,
              .(aanvragen = .N, nieuwbouw = sum(nb_streng), nieuwbouw_titel = sum(soort == "nieuwbouw_woning"),
                klein = sum(soort == "klein_verbouw"), uitgebreid = sum(uitgebreid), veestal = sum(veestal)), by = .(jaar, D)][order(jaar, D)]
print(dcast(R$A_jaar, jaar ~ D, value.var = c("aanvragen", "nieuwbouw")))

## ---------------------------------------------------------------------
## 2. Brug: van analyse_vastpanel.R naar deze specificatie
## ---------------------------------------------------------------------
## Stap voor stap, op paneel A en alle aanvragen, met de PAS-term uit het
## model met vijf gestapelde wisselingen. Stap 1 is analyse_vastpanel.R
## zelf (hun panel.csv, hun kwartaalindeling); elke volgende stap
## verandert een ding.
kop("2. Brug van analyse_vastpanel.R naar deze specificatie (paneel A, alle aanvragen)")
kw_orde <- function(k) as.integer(substr(k, 1, 4)) * 4L + as.integer(substr(k, 6, 6)) - 1L
ev_hun <- c(pas = kw_orde("2019K3"), bvr = kw_orde("2021K3"), por = kw_orde("2023K1"), ow = kw_orde("2024K1"), sal = kw_orde("2025K1"))
per_jaar_ruw <- z_ruw[, .N, by = .(gemeente, jaar)]
vast_ruw <- per_jaar_ruw[jaar %in% 2016:2020 & N >= 40, .N, by = gemeente][N == 5, gemeente]
hun_wissels <- function(p, k) invisible(p[, `:=`(na_pas = D * as.integer(k >= ev_hun["pas"]), na_bvr = D * as.integer(k >= ev_hun["bvr"]),
                                        na_por = D * as.integer(k >= ev_hun["por"]), na_ow = D * as.integer(k >= ev_hun["ow"]),
                                        na_sal = D * as.integer(k >= ev_hun["sal"]))])
brug <- list()
## stap 1: hun panel.csv, zonder nulcellen
pc <- fread(file.path(PATHS$verg, "analyse", "panel.csv"), sep = ";", encoding = "UTF-8",
            select = c("gemeente", "kwartaal", "nabij", "aanvragen"))
pc[, `:=`(k = kw_orde(kwartaal), D = nabij, y = aanvragen)]
pc <- pc[gemeente %in% vast_ruw & k >= kw_orde("2016K1") & k <= kw_orde("2026K2")]
hun_wissels(pc, pc$k)
brug[[1]] <- cbind(stap = "1. analyse_vastpanel.R: hun paneel zonder nulcellen", zaken = sum(pc$y),
                   fit_termen(pc, WISSELS, "gemeente^D + gemeente^kwartaal")[term == "na_pas"])
## stap 2: hetzelfde, maar met nulcellen binnen de publicatieperiode van elke gemeente
vol <- function(zz, gem, van, tot) {
  cel <- zz[gemeente %in% gem & t >= van & t <= tot, .(y = .N), by = .(gemeente, D, t)]
  act <- zz[, .(t_eerst = min(t), t_laatst = max(t)), by = gemeente]
  v <- merge(CJ(gemeente = gem, D = 0:1, t = van:tot), act, by = "gemeente")[t >= t_eerst & t <= t_laatst]
  p <- merge(v[, .(gemeente, D, t)], cel, by = c("gemeente", "D", "t"), all.x = TRUE)
  p[is.na(y), y := 0L]
  p[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L))][]
}
zr <- z_ruw[t <= Q_EIND]; zr[, D := nabij]
p2 <- vol(zr, vast_ruw, Q_A0, Q_EIND); hun_wissels(p2, p2$t + (KW_START * 4L - 1L))
brug[[2]] <- cbind(stap = "2. plus nulcellen", zaken = sum(p2$y), fit_termen(p2, WISSELS, "gem_D + gem_t")[term == "na_pas"])
## stap 3: zaken zonder locatie eruit (ze telden als ver)
zl <- z_ruw[!is.na(dist_m) & t <= Q_EIND]; zl[, D := as.integer(dist_m <= 2500)]
p3 <- vol(zl, vast_ruw, Q_A0, Q_EIND); hun_wissels(p3, p3$t + (KW_START * 4L - 1L))
brug[[3]] <- cbind(stap = "3. plus zaken zonder locatie eruit", zaken = sum(p3$y), fit_termen(p3, WISSELS, "gem_D + gem_t")[term == "na_pas"])
## stap 4: het kwartaal van de wisseling telt als na (zoals hoofdstuk 5 tot 8), en nabij x seizoen
p4 <- tel_paneel("A", "alle"); zet_wissels(p4)
brug[[4]] <- cbind(stap = "4. plus kwartaal van de wisseling als na, en nabij x seizoen", zaken = sum(p4$y),
                   fit_termen(p4, WISSELS, FE$D)[term == "na_pas"])
## stap 5: alleen 2016Q1 tot en met 2021Q2, voor de nieuwe publicatievorm
p5 <- p4[t < Q_BVR]
brug[[5]] <- cbind(stap = "5. alleen 2016Q1 tot en met 2021Q2", zaken = sum(p5$y), fit_termen(p5, "na_pas", FE$D))
## ter vergelijking: dezelfde stap 5 met alleen een landelijk kwartaaleffect
brug[[6]] <- cbind(stap = "5b. als 5, met alleen een landelijk kwartaaleffect", zaken = sum(p5$y), fit_termen(copy(p5), "na_pas", FE$B))
R$brug <- rbindlist(brug)
R$brug[, `:=`(irr = exp(b), pct = 100 * (exp(b) - 1))]
R$meta$gemeenten_A_brug <- length(vast_ruw)
toon(R$brug, "PAS-term per stap:")

## ---------------------------------------------------------------------
## 3. Aanvragen: event study, DiD, trendbreuk, eigen trend
## ---------------------------------------------------------------------
kop("3. Aanvragen nabij tegenover ver")
R$es <- list(); R$jaren <- list(); did <- list(); trend <- list(); pre <- list()
for (s in c("alle", "nieuwbouw", "nieuwbouw_titel", "uitgebreid", "veestal", "klein")) {
  ## paneel A, heel venster, strenge vergelijking
  p <- tel_paneel("A", s); zet_wissels(p)
  es <- fit_es(p, FE$D, Q_REF)
  w <- wald_pre(es$model, Q_REF)
  jr <- jaar_tabel(es)
  R$jaren[[paste("A", s)]] <- cbind(paneel = "A", soort = s, jr)
  pre[[s]] <- data.table(soort = s, pre_p = w$p, pre_df = w$df,
                         zaken_A = z[gemeente %in% PANEL$A$gem & t >= Q_A0][eval(SOORT[[s]]$sel), .N],
                         nabij_A = z[gemeente %in% PANEL$A$gem & t >= Q_A0 & D == 1L][eval(SOORT[[s]]$sel), .N])
  if (s %in% c("alle", "nieuwbouw")) R$es[[paste("A", s)]] <- list(coef = es$tab[, .(t, kw, b, se, irr, lo, hi)], pre_wald = w)
  pas <- p[t < Q_BVR]
  for (sp in c("B", "D")) {
    did[[length(did) + 1L]] <- cbind(paneel = "A", soort = s, spec = sp, venster = "2016Q1-2021Q2", fit_termen(copy(pas), "na_pas", FE[[sp]]))
  }
  did[[length(did) + 1L]] <- cbind(paneel = "A", soort = s, spec = "E", venster = "2016Q1-2021Q2", fit_termen(copy(pas), "na_pas", FE$E))
  did[[length(did) + 1L]] <- cbind(paneel = "A", soort = s, spec = "D", venster = "2016Q1-2026Q2", fit_termen(p, WISSELS, FE$D))
  trend[[length(trend) + 1L]] <- cbind(paneel = "A", soort = s, venster = "2016Q1-2021Q2", fit_trendbreuk(copy(pas), FE$D))
  ## paneel C als controle op de keuze van het paneel (C bevat A)
  pcx <- tel_paneel("C", s); zet_wissels(pcx)
  did[[length(did) + 1L]] <- cbind(paneel = "C", soort = s, spec = "D", venster = "2018Q1-2021Q2", fit_termen(pcx[t < Q_BVR], "na_pas", FE$D))
  ## alle gemeenten vanaf 2021Q3
  pl <- tel_paneel("L", s); zet_wissels(pl)
  esl <- fit_es(pl, FE$D, Q_REF_L)
  R$jaren[[paste("L", s)]] <- cbind(paneel = "L", soort = s, jaar_tabel(esl))
  if (s %in% c("alle", "nieuwbouw")) R$es[[paste("L", s)]] <- list(coef = esl$tab[, .(t, kw, b, se, irr, lo, hi)], pre_wald = wald_pre(esl$model, Q_REF_L))
  for (sp in c("B", "D")) {
    did[[length(did) + 1L]] <- cbind(paneel = "L", soort = s, spec = sp, venster = "2021Q3-2026Q2", fit_termen(copy(pl), WISSELS_L, FE[[sp]]))
  }
  cat(sprintf("%-16s A: pre-Wald p=%.3f (df %d)  jaren: %s\n", s, w$p, w$df,
              paste(sprintf("%s %+.3f", jr$jaar, jr$b), collapse = "  ")))
}
R$pre <- rbindlist(pre)
R$did <- rbindlist(did)
R$did[, `:=`(irr = exp(b), pct = 100 * (exp(b) - 1), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se), mde = mde_log(se))]
R$trend <- rbindlist(trend)
R$jaren <- rbindlist(R$jaren)
toon(R$did[, .(paneel, soort, spec, venster, term, b, se, p, pct, mde, n)], "DiD-termen (Poisson):")
toon(R$trend, "Trendbreuk bij de PAS-uitspraak, paneel A, streng (voortrend en natrend per jaar):")

## Ringen, zoals hoofdstuk 8: 0-1.000, 1.000-2.500, 2.500-5.000 m tegenover
## verder dan 5.000 m, binnen dezelfde gemeente en hetzelfde kwartaal.
kop("Ringen, paneel A, 2016Q1-2021Q2")
RINGEN <- c("0 tot 1.000 m", "1.000 tot 2.500 m", "2.500 tot 5.000 m")
ring_van_m <- function(m) fifelse(m <= 1000, 1L, fifelse(m <= 2500, 2L, fifelse(m <= 5000, 3L, 0L)))
R$ringen <- rbindlist(lapply(c("alle", "nieuwbouw"), function(s) {
  p <- tel_paneel("A", s, tot = Q_BVR - 1L, klasse = ring_van_m)
  p[, post := as.integer(t >= Q_PAS)]
  m <- fepois(y ~ i(D, post, ref = 0) | gem_D + gem_t + D_szn, data = p, cluster = ~gemeente, notes = FALSE, glm.iter = 200)
  data.table(soort = s, ring = RINGEN, b = coef(m), se = se(m), p = pvalue(m), n = nobs(m))
}))
R$ringen[, pct := 100 * (exp(b) - 1)]
toon(R$ringen, "PAS-term per ring:")

## ---------------------------------------------------------------------
## 4. Uitkomsten per zaak: verleend, geweigerd, ingetrokken, geen uitkomst
##    gevonden, besluit zonder leesbare uitkomst, en de doorlooptijd
## ---------------------------------------------------------------------
## Lineair op zaakniveau met dezelfde vaste effecten (gemeente x nabij,
## gemeente x kwartaal, nabij x seizoen). Alleen aanvragen met minstens
## negen maanden vervolg in de data, zodat een uitkomst er kan zijn.
kop("4. Uitkomsten per zaak")
einde <- max(z$datum_aanvraag) - 270L
R$meta$uitkomst_tot <- einde
invisible(zet_wissels(z))
UITK <- c("verleend", "geweigerd", "ingetrokken", "geen", "besluit_onbekend", "log_dl")
uk <- list()
for (s in c("alle", "nieuwbouw", "uitgebreid", "klein")) {
  za <- z[gemeente %in% PANEL$A$gem & t >= Q_A0 & t < Q_BVR][eval(SOORT[[s]]$sel)]
  zl <- z[t >= Q_BVR & datum_aanvraag <= einde][eval(SOORT[[s]]$sel)]
  for (u in UITK) {
    da <- if (u == "log_dl") za[verleend == 1L & !is.na(log_dl)] else za
    dl <- if (u == "log_dl") zl[verleend == 1L & !is.na(log_dl)] else zl
    niv_a <- da[D == 1L & t < Q_PAS, mean(get(u))]
    niv_l <- dl[D == 1L & t < Q_POR, mean(get(u))]
    uk[[length(uk) + 1L]] <- cbind(paneel = "A", soort = s, uitkomst = u, niveau_nabij_voor = niv_a,
                                   fit_termen(da, "na_pas", FE$D, y = u, fam = "ols"))
    uk[[length(uk) + 1L]] <- cbind(paneel = "L", soort = s, uitkomst = u, niveau_nabij_voor = niv_l,
                                   fit_termen(dl, WISSELS_L, FE$D, y = u, fam = "ols"))
  }
}
R$uitkomst <- rbindlist(uk)
R$uitkomst[, mde := 2.8 * se]

## Ringen vanaf 2021Q3, alle gemeenten: de latere wisselingen per ring. De
## BAG-vergunningen in de binnenste kilometer dalen vanaf 2024 (08_strikt.R,
## ringen_jaar); hier dezelfde vraag voor de aanvragen, die geen last hebben
## van de meetbreuk van de bouwmelding in de BAG. Verleend alleen voor
## aanvragen met minstens negen maanden vervolg.
kop("Ringen vanaf 2021Q3, alle gemeenten: Porthos, Omgevingswet en intern salderen per ring")
RL <- list(list(soort = "alle aanvragen", s = "alle", extra = quote(TRUE)),
           list(soort = "nieuwbouw, streng", s = "nieuwbouw", extra = quote(TRUE)),
           list(soort = "nieuwbouw, streng, verleend", s = "nieuwbouw", extra = quote(verleend == 1L & datum_aanvraag <= einde)))
R$ringen_L <- rbindlist(lapply(RL, function(x) {
  p <- tel_paneel("L", x$s, extra = x$extra, klasse = ring_van_m)
  p[, `:=`(por = as.integer(t >= Q_POR), ow = as.integer(t >= Q_OW), sal = as.integer(t >= Q_SAL))]
  m <- fepois(y ~ i(D, por, ref = 0) + i(D, ow, ref = 0) + i(D, sal, ref = 0) | gem_D + gem_t + D_szn, data = p,
              cluster = ~gemeente, notes = FALSE, glm.iter = 200)
  ct <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(ct, c("term", "b", "se", "tw", "p"))
  ct[, `:=`(soort = x$soort, ring = RINGEN[as.integer(sub("^D::([0-9]):.*", "\\1", term))], wissel = sub(".*:", "", term), n = nobs(m))]
  ct[, .(soort, ring, wissel, b, se, p, n)]
}))
R$ringen_L[, pct := 100 * (exp(b) - 1)]
toon(R$ringen_L, "Ringen vanaf 2021Q3:")
toon(R$uitkomst[, .(paneel, soort, uitkomst, term, niveau = niveau_nabij_voor, b, se, p, mde, n)], "Uitkomsten (lineair; log_dl in log-punten):")

## ---------------------------------------------------------------------
## 5. Beschrijvend: doorlooptijd en uitkomsten per jaar, nabij en ver
## ---------------------------------------------------------------------
kop("5. Doorlooptijd en uitkomsten per jaar")
besch <- function(zz, paneel) zz[, .(aanvragen = .N, verleend = mean(verleend), geweigerd = mean(geweigerd),
                                     ingetrokken = mean(ingetrokken), geen = mean(geen), besluit_onbekend = mean(besluit_onbekend),
                                     dagen_mediaan = as.numeric(median(doorlooptijd[verleend == 1L & doorlooptijd > 0], na.rm = TRUE)),
                                     dagen_p75 = as.numeric(quantile(doorlooptijd[verleend == 1L & doorlooptijd > 0], .75, na.rm = TRUE))),
                                 by = .(jaar, D)][order(jaar, D)][, paneel := paneel][]
R$per_jaar <- rbind(besch(z[gemeente %in% PANEL$A$gem & t >= Q_A0 & jaar <= 2025], "A"),
                    besch(z[jaar >= 2022 & jaar <= 2025], "L"),
                    besch(z[gemeente %in% PANEL$A$gem & t >= Q_A0 & jaar <= 2025 & nb_streng == 1L], "A nieuwbouw"),
                    besch(z[jaar >= 2022 & jaar <= 2025 & nb_streng == 1L], "L nieuwbouw"))
toon(R$per_jaar[paneel %in% c("A", "L")], "Per jaar:")
R$dagen_kwartaal <- z[t >= Q_A0 & verleend == 1L & doorlooptijd > 0 & datum_aanvraag <= einde &
                      (gemeente %in% PANEL$A$gem | t >= Q_BVR),
                      .(dagen = as.numeric(median(doorlooptijd)), n = .N),
                      by = .(paneel = fifelse(gemeente %in% PANEL$A$gem, "A", "overig"), t, D)][order(paneel, t, D)]
R$dagen_kwartaal[, kw := kw_label(t)]

## ---------------------------------------------------------------------
## 6. Gevoeligheid van de PAS-term (paneel A, alle aanvragen, streng)
## ---------------------------------------------------------------------
kop("6. Gevoeligheid van de PAS-term")
gv <- list()
basis <- tel_paneel("A", "alle", tot = Q_BVR - 1L); zet_wissels(basis)
gv[[1]] <- cbind(variant = "basis: paneel A, streng, 2016Q1-2021Q2", fit_termen(copy(basis), "na_pas", FE$D))
p <- tel_paneel("A", "alle", tot = Q_BVR - 1L, extra = quote(stdpunt == FALSE)); zet_wissels(p)
gv[[2]] <- cbind(variant = "zonder zaken op een standaardpunt", fit_termen(p, "na_pas", FE$D))
p <- copy(basis); p[, na_pas := D * as.integer(t >= Q_PAS + 1L)]
gv[[3]] <- cbind(variant = "eerste volle kwartaal na de uitspraak (2019Q3) als na", fit_termen(p, "na_pas", FE$D))
p <- copy(basis)[t != Q_PAS]
gv[[4]] <- cbind(variant = "kwartaal van de uitspraak (2019Q2) weggelaten", fit_termen(p, "na_pas", FE$D))
gv[[5]] <- cbind(variant = "zonder nabij x seizoen", fit_termen(copy(basis), "na_pas", "gem_D + gem_t"))
p <- tel_paneel("A", "alle", tot = Q_BVR - 1L, extra = quote(bouw == 1L)); zet_wissels(p)
gv[[6]] <- cbind(variant = "alleen bouwzaken (label bouwen of bouwwoord)", fit_termen(p, "na_pas", FE$D))
p <- tel_paneel("A", "alle", tot = Q_BVR - 1L, extra = quote(dist_m <= 1000 | dist_m > 2500)); zet_wissels(p)
p[, D := D]   # nabij is nu binnen 1.000 m, ver blijft verder dan 2.500 m
gv[[7]] <- cbind(variant = "nabij binnen 1.000 m, ver verder dan 2.500 m", fit_termen(p, "na_pas", FE$D))
R$gevoeligheid <- rbindlist(gv)
R$gevoeligheid[, pct := 100 * (exp(b) - 1)]
toon(R$gevoeligheid, "Gevoeligheid:")

## ---------------------------------------------------------------------
## 7. Hoeveel toetsen, hoeveel significant
## ---------------------------------------------------------------------
alle_p <- c(R$did$p, R$trend$p, R$ringen$p, R$ringen_L$p, R$uitkomst$p, R$gevoeligheid$p)
R$toetsen <- list(termen = length(alle_p), p_onder_005 = sum(alle_p < 0.05, na.rm = TRUE),
                  verwacht_bij_toeval = round(0.05 * length(alle_p), 1))
kop("7. Toetsen: %d termen, %d met p < 0,05 (bij toeval verwacht %.1f)", R$toetsen$termen, R$toetsen$p_onder_005,
    R$toetsen$verwacht_bij_toeval)
print(rbind(R$did[p < 0.05, .(tabel = "did", paneel, soort, spec, term, b = round(b, 4), p = round(p, 4))],
            R$uitkomst[p < 0.05, .(tabel = "uitkomst", paneel, soort, spec = uitkomst, term, b = round(b, 4), p = round(p, 4))],
            R$trend[p < 0.05, .(tabel = "trend", paneel, soort, spec = venster, term, b = round(b, 4), p = round(p, 4))], fill = TRUE))

## ---------------------------------------------------------------------
## Wegschrijven
## ---------------------------------------------------------------------
json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("aanvragen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("aanvragen_%s.json", EXTRACT)))
sink()
