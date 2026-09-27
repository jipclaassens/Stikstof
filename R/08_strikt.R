## =====================================================================
## 08_strikt.R -- de strenge vergelijking: nabij en ver binnen dezelfde
##                gemeente en hetzelfde kwartaal
##
## Input : _derived/vbo_analyse.rds
## Output: _output/strikt_<extract>.json   (voor het rapport)
##         _output/strikt_<extract>.txt    (leesbare log)
##
## Waarom
## ---------------------------------------------------------------------
## De hoofdspecificatie (06_rapport.R) vergelijkt afstandsklassen binnen
## een provincie en een kwartaal. Een schok die per gemeente verschilt en
## samenvalt met hoeveel natuur er in de gemeente ligt, zoals een eigen
## woningbouwgolf of de verschuiving van uitleg aan de randen naar
## binnenstedelijk bouwen, verschijnt dan als effect van nabijheid. Het
## vergunningenspoor (vergunningen/inzichten.md, hoofdstuk 24 en 25) vond
## dat de nabijheidseffecten in de bekendmakingen verdwijnen zodra nabij
## en ver binnen dezelfde gemeente en hetzelfde kwartaal worden vergeleken.
## Dit script legt dezelfde toets op de objectdata van het rapport, met
## precies de steekproeven, projectsleutel en referentieperiode van
## 06_rapport.R. Alleen de vergelijking verandert.
##
## Specificaties (Poisson; cel = gemeente x nabij x kwartaal, behalve A)
##   A  provincie x afstandsklasse; FE provincie x kwartaal, provincie x
##      klasse, klasse x seizoen; cluster provincie x klasse (= 06_rapport.R)
##   B  FE gemeente x nabij + kwartaal + nabij x seizoen
##   C  FE gemeente x nabij + provincie x kwartaal + nabij x seizoen
##   D  FE gemeente x nabij + gemeente x kwartaal + nabij x seizoen (streng)
##   E  als D, plus een eigen lineaire trend nabij per gemeente
##   F  als C en D, met een trendbreuk: trend nabij voor de uitspraak,
##      sprong bij de uitspraak en verandering van de trend erna
## B tot en met F: standaardfouten geclusterd op gemeente. E en F lopen
## tot en met 2024Q3, het laatste kwartaal voor de uitspraak over intern
## salderen, zodat er maar een gebeurtenis in het venster zit.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("strikt_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()

d <- read_derived("vbo_analyse")
R <- list()

## ---------------------------------------------------------------------
## Tijdindex en gebeurtenissen: dezelfde als in 06_rapport.R
## ---------------------------------------------------------------------
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q_REF <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_POR <- kw_index(as.Date("2022-11-02"))
Q_SAL <- kw_index(as.Date("2024-12-18"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))   # 2026Q2: piek van juni 2026 (17_registratie.R)
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))
N_KL <- as.integer(PARAMS$klasse_max_m / PARAMS$klasse_breedte) + 1L

## ---------------------------------------------------------------------
## Steekproeven: precies die van 06_rapport.R
## ---------------------------------------------------------------------
s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               q_ref = Q_REF, q_pas = Q_PAS, q_por = Q_POR, q_sal = Q_SAL,
               objecten = lapply(UIT, nrow),
               zonder_gemeente = lapply(UIT, function(x) sum(is.na(x$gemeentecode))))

## ---------------------------------------------------------------------
## Panelen
## ---------------------------------------------------------------------
## Per project en kwartaal de kleinste afstandsklasse, zoals in 06_rapport.R;
## een project ligt altijd in een gemeente, want de sleutel is gemeente plus
## documentnummer.
paneel_provincie <- function(dt) {
  pp  <- dt[, .(k = min(afstandsklasse), n = .N), by = .(projectid, provinciecode, q)]
  cel <- pp[, .(y = sum(n)), by = .(provinciecode, k, t = q)]
  vol <- CJ(provinciecode = sort(unique(cel$provinciecode)), k = 1:N_KL, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("provinciecode", "k", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p[, `:=`(prov_t = paste(provinciecode, t), prov_k = paste(provinciecode, k), k_szn = paste(k, (t - 1L) %% 4L))]
  p[]
}
paneel_gemeente <- function(dt, drempel) {
  pp  <- dt[!is.na(gemeentecode), .(k = min(afstandsklasse), n = .N), by = .(projectid, gemeentecode, q)]
  pp[, D := as.integer(k <= drempel / PARAMS$klasse_breedte)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, D, t = q)]
  ## de provincie van een gemeente: de meest voorkomende (huidige indeling)
  prov <- dt[!is.na(gemeentecode), .N, by = .(gemeentecode, provinciecode)][order(-N)][!duplicated(gemeentecode)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), D = 0:1, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "D", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p <- merge(p, prov[, .(gemeentecode, provinciecode)], by = "gemeentecode", all.x = TRUE)
  p[, `:=`(gem_D = paste(gemeentecode, D), gem_t = paste(gemeentecode, t), prov_t = paste(provinciecode, t),
           D_szn = paste(D, (t - 1L) %% 4L), Dt = D * t)]
  p[]
}

## ---------------------------------------------------------------------
## Schatters
## ---------------------------------------------------------------------
fit_es <- function(p, fe, cl) {
  m <- fepois(as.formula(sprintf("y ~ i(t, D, ref = c(%s)) | %s", paste(Q_REF, collapse = ","), fe)),
              data = p, cluster = cl, notes = FALSE)
  cf <- coef(m); se <- se(m)
  tt <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  tab <- rbind(data.table(t = tt, b = cf, se = se), data.table(t = Q_REF, b = 0, se = 0))[order(t)]
  tab[, `:=`(kw = kw_label(t), irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  list(model = m, tab = tab)
}
wald_pre <- function(m) {
  b <- coef(m); V <- vcov(m)
  tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(tt < min(Q_REF))
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  list(stat = W, df = length(idx), p = pchisq(W, length(idx), lower.tail = FALSE))
}
gem_regime <- function(m, t_van, t_tot) {
  b <- coef(m); V <- vcov(m)
  tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(tt >= t_van & tt <= t_tot); w <- rep(1 / length(idx), length(idx))
  est <- sum(w * b[idx]); se <- sqrt(as.numeric(t(w) %*% V[idx, idx] %*% w))
  list(b = est, se = se, irr = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se),
       p = 2 * pnorm(-abs(est / se)), n = length(idx))
}
coefrij <- function(m, term) {
  ct <- coeftable(m)
  data.table(term = term, b = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}
fit_did <- function(p, fe, cl, post_van = Q_PAS) {
  p[, tr := D * as.integer(t >= post_van)]
  coefrij(fepois(as.formula(sprintf("y ~ tr | %s", fe)), data = p, cluster = cl, notes = FALSE), "tr")
}
fit_regime <- function(p, fe, cl) {
  p[, `:=`(na_pas = D * as.integer(t >= Q_PAS), na_por = D * as.integer(t >= Q_POR), na_sal = D * as.integer(t >= Q_SAL))]
  m <- fepois(as.formula(sprintf("y ~ na_pas + na_por + na_sal | %s", fe)), data = p, cluster = cl, notes = FALSE)
  rbindlist(lapply(c("na_pas", "na_por", "na_sal"), function(v) coefrij(m, v)))
}
## Trendbreuk: tj in jaren ten opzichte van de uitspraak; voortrend = trend
## nabij tegenover ver, sprong = niveauverschuiving bij de uitspraak,
## natrend = verandering van de trend erna.
fit_trendbreuk <- function(p, fe, cl) {
  p[, tj := (t - Q_PAS) / 4]
  p[, `:=`(voortrend = D * tj, sprong = D * as.integer(t >= Q_PAS), natrend = D * as.integer(t >= Q_PAS) * tj)]
  m <- fepois(as.formula(sprintf("y ~ voortrend + sprong + natrend | %s", fe)), data = p, cluster = cl, notes = FALSE)
  rbindlist(lapply(c("voortrend", "sprong", "natrend"), function(v) coefrij(m, v)))
}

SPECS <- list(
  B = list(naam = "gemeente x nabij, landelijk kwartaal",           fe = "gem_D + t + D_szn"),
  C = list(naam = "gemeente x nabij, provincie x kwartaal",         fe = "gem_D + prov_t + D_szn"),
  D = list(naam = "gemeente x nabij, gemeente x kwartaal (streng)", fe = "gem_D + gem_t + D_szn"))
FE_E <- "gem_D + gem_t + D_szn + gemeentecode[Dt]"   # D plus een eigen trend nabij per gemeente

samenvatting <- list()
vat_samen <- function(u, spec, naam, dr, es, w, g1, did) {
  samenvatting[[length(samenvatting) + 1L]] <<- data.table(
    uitkomst = u, spec = spec, naam = naam, drempel = dr, pre_p = w$p,
    irr_pas = g1$irr, irr_pas_lo = g1$lo, irr_pas_hi = g1$hi, irr_pas_p = g1$p,
    did_b = did$b, did_se = did$se, did_p = did$p, n = did$n)
}

## ---------------------------------------------------------------------
## 1. A: de hoofdspecificatie van het rapport, ter controle
## ---------------------------------------------------------------------
kop("A. Hoofdspecificatie van het rapport (provincie x afstandsklasse)")
R$A <- list()
for (u in names(UIT)) {
  p <- paneel_provincie(UIT[[u]])
  R$A[[u]] <- list()
  for (dr in c(1000, 2500, 5000)) {
    p[, D := as.integer(k <= dr / PARAMS$klasse_breedte)]
    es <- fit_es(p, "prov_t + prov_k + k_szn", ~prov_k)
    w <- wald_pre(es$model); g1 <- gem_regime(es$model, Q_PAS, Q_SAL - 1L)
    did <- fit_did(p, "prov_t + prov_k + k_szn", ~prov_k)
    R$A[[u]][[as.character(dr)]] <- list(coef = es$tab[, .(t, kw, b, se, irr, lo, hi)], pre_wald = w, gem_pas = g1, did = did)
    vat_samen(u, "A", "provincie x afstandsklasse (rapport)", dr, es, w, g1, did)
    cat(sprintf("%-12s %5dm  pre p=%.3f  pas IRR %.3f [%.3f-%.3f]  DiD %+.3f (%.3f)\n", u, dr, w$p, g1$irr, g1$lo, g1$hi, did$b, did$se))
  }
}

## ---------------------------------------------------------------------
## 2. B, C en D: gemeente x nabij, van landelijk tot streng
## ---------------------------------------------------------------------
kop("B, C en D. Gemeente x nabij x kwartaal")
R$es <- list(); R$regime <- list(); R$gemeenten <- list()
for (u in names(UIT)) {
  R$es[[u]] <- list(); R$regime[[u]] <- list(); R$gemeenten[[u]] <- list()
  for (dr in c(1000, 2500, 5000)) {
    p <- paneel_gemeente(UIT[[u]], dr)
    ## gemeenten die aan de strenge vergelijking bijdragen: objecten nabij en ver
    bijdrage <- p[, .(nabij = sum(y[D == 1]), ver = sum(y[D == 0])), by = gemeentecode]
    R$gemeenten[[u]][[as.character(dr)]] <- list(gemeenten = nrow(bijdrage),
                                                 met_nabij_en_ver = sum(bijdrage$nabij > 0 & bijdrage$ver > 0),
                                                 objecten_nabij = sum(bijdrage$nabij), objecten_ver = sum(bijdrage$ver))
    for (sp in names(SPECS)) {
      if (dr != 2500 && sp != "D") next
      es <- fit_es(p, SPECS[[sp]]$fe, ~gemeentecode)
      w <- wald_pre(es$model); g1 <- gem_regime(es$model, Q_PAS, Q_SAL - 1L); g2 <- gem_regime(es$model, Q_SAL, max(p$t))
      did <- fit_did(p, SPECS[[sp]]$fe, ~gemeentecode)
      R$es[[u]][[paste(sp, dr)]] <- list(spec = sp, naam = SPECS[[sp]]$naam, drempel = dr,
                                         coef = es$tab[, .(t, kw, b, se, irr, lo, hi)],
                                         pre_wald = w, gem_pas = g1, gem_sal = g2, did = did)
      R$regime[[u]][[paste(sp, dr)]] <- cbind(spec = sp, drempel = dr, fit_regime(p, SPECS[[sp]]$fe, ~gemeentecode))
      vat_samen(u, sp, SPECS[[sp]]$naam, dr, es, w, g1, did)
      cat(sprintf("%-12s %s %5dm  pre p=%.3f  pas IRR %.3f [%.3f-%.3f]  DiD %+.3f (%.3f)  n=%d\n", u, sp, dr, w$p,
                  g1$irr, g1$lo, g1$hi, did$b, did$se, did$n))
    }
  }
  R$regime[[u]] <- rbindlist(R$regime[[u]])
}

## ---------------------------------------------------------------------
## 3. E en F: eigen trend per gemeente, trendbreuk; venster t/m 2024Q3
## ---------------------------------------------------------------------
kop("E en F. Eigen trend per gemeente en trendbreuk, 2012Q1 t/m 2024Q3")
R$trend <- list()
for (u in names(UIT)) {
  p <- paneel_gemeente(UIT[[u]], 2500)[t <= Q_SAL - 1L]
  e <- fit_did(p, FE_E, ~gemeentecode)
  vat_samen(u, "E", "streng, plus eigen trend nabij per gemeente", 2500,
            NULL, list(p = NA_real_), list(irr = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_), e)
  fc <- fit_trendbreuk(copy(p), SPECS$C$fe, ~gemeentecode)
  fd <- fit_trendbreuk(copy(p), SPECS$D$fe, ~gemeentecode)
  R$trend[[u]] <- list(E_did = e, F_provincie = fc, F_streng = fd)
  cat(sprintf("%-12s E  DiD %+.3f (%.3f) p=%.3f\n", u, e$b, e$se, e$p))
  for (x in list(list("F provincie x kwartaal", fc), list("F streng", fd))) {
    cat(sprintf("%-12s %-24s voortrend %+.4f/jr (%.4f)  sprong %+.3f (%.3f) p=%.3f  natrend %+.4f/jr (%.4f)\n", u, x[[1]],
                x[[2]]$b[1], x[[2]]$se[1], x[[2]]$b[2], x[[2]]$se[2], x[[2]]$p[2], x[[2]]$b[3], x[[2]]$se[3]))
  }
}

## ---------------------------------------------------------------------
## 4. Placebo: nepgebeurtenis 2016Q1 op 2012-2018, streng
## ---------------------------------------------------------------------
kop("Placebo 2016Q1, streng")
R$placebo <- rbindlist(lapply(names(UIT), function(u) {
  p <- paneel_gemeente(UIT[[u]], 2500)[t <= max(Q_REF)]
  cbind(uitkomst = u, fit_did(p, SPECS$D$fe, ~gemeentecode, post_van = kw_index(as.Date("2016-01-01"))))
}))
print(R$placebo)

## ---------------------------------------------------------------------
## 5. Woningen: de vertaling van 06_rapport.R, nu met de strenge IRR
## ---------------------------------------------------------------------
kop("Woningen, 2019Q2 t/m 2024Q3, binnen 2.500 m")
p <- paneel_gemeente(UIT$nieuwbouw, 2500)
Ypost <- sum(p$y[p$D == 1 & p$t >= Q_PAS & p$t < Q_SAL]); Ytot <- sum(p$y[p$t >= Q_PAS & p$t < Q_SAL])
minder <- function(irr) Ypost / irr - Ypost
R$homes <- rbindlist(lapply(c("A", "C", "D"), function(sp) {
  g <- if (sp == "A") R$A$nieuwbouw[["2500"]]$gem_pas else R$es$nieuwbouw[[paste(sp, 2500)]]$gem_pas
  data.table(spec = sp, behandeld_post = Ypost, totaal_post = Ytot, irr = g$irr, irr_lo = g$lo, irr_hi = g$hi,
             minder_punt = round(minder(g$irr)), minder_lo = round(minder(g$hi)), minder_hi = round(minder(g$lo)))
}))
print(R$homes)

## ---------------------------------------------------------------------
## 6. Ringen in plaats van geneste drempels
## ---------------------------------------------------------------------
## De drempels van het rapport zijn genest: 2.500 m bevat 1.000 m. Een
## stikstofmechanisme voorspelt een effect dat sterker is naarmate het
## project dichter bij habitat ligt. Losse ringen tegenover verder dan
## 5.000 m laten zien waar het verschil zit.
kop("Ringen: 0-1.000, 1.000-2.500, 2.500-5.000 m tegenover > 5.000 m")
ring_van <- function(k) fifelse(k <= 4L, 1L, fifelse(k <= 10L, 2L, fifelse(k <= 20L, 3L, 0L)))
RINGEN <- c("0 tot 1.000 m", "1.000 tot 2.500 m", "2.500 tot 5.000 m")
paneel_gemeente_ring <- function(dt) {
  pp  <- dt[!is.na(gemeentecode), .(k = min(afstandsklasse), n = .N), by = .(projectid, gemeentecode, q)]
  pp[, ring := ring_van(k)]
  cel <- pp[, .(y = sum(n)), by = .(gemeentecode, ring, t = q)]
  vol <- CJ(gemeentecode = sort(unique(cel$gemeentecode)), ring = 0:3, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("gemeentecode", "ring", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p[, `:=`(gem_r = paste(gemeentecode, ring), gem_t = paste(gemeentecode, t), r_szn = paste(ring, (t - 1L) %% 4L))]
  p[]
}
ring_modellen <- function(p, fe, cl) {
  p[, post := as.integer(t >= Q_PAS)]
  m <- fepois(as.formula(sprintf("y ~ i(ring, post, ref = 0) | %s", fe)), data = p, cluster = cl, notes = FALSE)
  did <- data.table(ring = RINGEN, b = coef(m), se = se(m), p = pvalue(m), n = nobs(m))
  ## trendbreuk per ring, venster t/m 2024Q3
  q <- p[t <= Q_SAL - 1L]
  q[, tj := (t - Q_PAS) / 4]
  for (r in 1:3) q[, (paste0(c("voortrend_", "sprong_", "natrend_"), r)) :=
                     list(as.integer(ring == r) * tj, as.integer(ring == r) * post, as.integer(ring == r) * post * tj)]
  v <- paste(c(paste0("voortrend_", 1:3), paste0("sprong_", 1:3), paste0("natrend_", 1:3)), collapse = " + ")
  mt <- fepois(as.formula(sprintf("y ~ %s | %s", v, fe)), data = q, cluster = cl, notes = FALSE)
  tb <- data.table(term = names(coef(mt)), b = coef(mt), se = se(mt), p = pvalue(mt))
  tb[, `:=`(ring = RINGEN[as.integer(sub(".*_", "", term))], deel = sub("_.*", "", term))]
  list(did = did, trendbreuk = tb)
}
R$ringen <- list(); R$ringen_jaar <- list()
for (u in names(UIT)) {
  pa <- paneel_provincie(UIT[[u]]); pa[, ring := ring_van(k)]
  ra <- ring_modellen(pa, "prov_t + prov_k + k_szn", ~prov_k)
  pg <- paneel_gemeente_ring(UIT[[u]])
  rd <- ring_modellen(pg, "gem_r + gem_t + r_szn", ~gemeentecode)
  R$ringen[[u]] <- list(A = ra, D = rd)
  ## per jaar: wanneer valt het verschil per ring, binnen gemeente en kwartaal
  pg[, jr := factor(KW_START + (t - 1L) %/% 4L)]
  mj <- fepois(y ~ i(jr, i.ring, ref = "2018", ref2 = 0) | gem_r + gem_t + r_szn, data = pg, cluster = ~gemeentecode,
               notes = FALSE, glm.iter = 200)
  cj <- as.data.table(coeftable(mj), keep.rownames = "term"); setnames(cj, c("term", "b", "se", "tw", "p"))
  cj[, `:=`(uitkomst = u, ring = RINGEN[as.integer(sub(".*ring::", "", term))], jaar = as.integer(sub("^jr::([0-9]+):.*", "\\1", term)))]
  R$ringen_jaar[[u]] <- cj[, .(uitkomst, ring, jaar, b, se, p)]
  cat(sprintf("%-12s D binnenste ring per jaar: %s\n", u,
              paste(cj[ring == RINGEN[1], sprintf("%d %+.2f", jaar, b)], collapse = "  ")))
  for (x in list(list("A", ra), list("D", rd))) {
    cat(sprintf("%-12s %s DiD per ring:", u, x[[1]]))
    with(x[[2]]$did, cat(sprintf("  %s %+.3f (%.3f)", ring, b, se)))
    cat("\n")
    sp <- x[[2]]$trendbreuk[deel == "sprong"]; vt <- x[[2]]$trendbreuk[deel == "voortrend"]
    cat(sprintf("%-12s %s   voortrend/jr:", u, x[[1]])); with(vt, cat(sprintf("  %s %+.4f (%.4f)", ring, b, se))); cat("\n")
    cat(sprintf("%-12s %s   sprong:      ", u, x[[1]])); with(sp, cat(sprintf("  %s %+.3f (%.3f)", ring, b, se))); cat("\n")
  }
}
R$ringen_jaar <- rbindlist(R$ringen_jaar)

## ---------------------------------------------------------------------
## 7. Zelfde steekproef, en projecten tellen in plaats van objecten
## ---------------------------------------------------------------------
## D gebruikt alleen gemeenten met objecten nabij en ver in hetzelfde
## kwartaal; C schat op alle gemeenten. C op de cellen van D scheidt het
## effect van de steekproef van dat van de vaste effecten. Projecten tellen
## (een project telt een keer per kwartaal van oplevering) haalt het
## gewicht van de grootste projecten weg.
kop("Zelfde steekproef en projecten tellen, 2.500 m")
R$gevoeligheid <- rbindlist(lapply(names(UIT), function(u) {
  p <- paneel_gemeente(UIT[[u]], 2500)
  p[, tr := D * as.integer(t >= Q_PAS)]
  mD <- fepois(as.formula(sprintf("y ~ tr | %s", SPECS$D$fe)), data = p, cluster = ~gemeentecode, notes = FALSE)
  pd <- p[obs(mD)]
  mC <- fepois(as.formula(sprintf("y ~ tr | %s", SPECS$C$fe)), data = pd, cluster = ~gemeentecode, notes = FALSE)
  pp <- UIT[[u]][!is.na(gemeentecode), .(k = min(afstandsklasse)), by = .(projectid, gemeentecode, q)]
  pp[, D := as.integer(k <= 2500 / PARAMS$klasse_breedte)]
  pr <- pp[, .(y = .N), by = .(gemeentecode, D, t = q)]
  vol <- CJ(gemeentecode = sort(unique(pr$gemeentecode)), D = 0:1, t = min(pr$t):max(pr$t))
  pr <- merge(vol, pr, by = c("gemeentecode", "D", "t"), all.x = TRUE); pr[is.na(y), y := 0]
  pr <- merge(pr, unique(p[, .(gemeentecode, provinciecode)]), by = "gemeentecode", all.x = TRUE)
  pr[, `:=`(gem_D = paste(gemeentecode, D), gem_t = paste(gemeentecode, t), prov_t = paste(provinciecode, t),
            D_szn = paste(D, (t - 1L) %% 4L), tr = D * as.integer(t >= Q_PAS))]
  rbind(cbind(uitkomst = u, variant = "D, objecten", coefrij(mD, "tr")),
        cbind(uitkomst = u, variant = "C op de cellen van D", coefrij(mC, "tr")),
        cbind(uitkomst = u, variant = "C, projecten tellen",
              coefrij(fepois(as.formula(sprintf("y ~ tr | %s", SPECS$C$fe)), data = pr, cluster = ~gemeentecode, notes = FALSE), "tr")),
        cbind(uitkomst = u, variant = "D, projecten tellen",
              coefrij(fepois(as.formula(sprintf("y ~ tr | %s", SPECS$D$fe)), data = pr, cluster = ~gemeentecode, notes = FALSE), "tr")))
}))
print(R$gevoeligheid[, .(uitkomst, variant, b = round(b, 3), se = round(se, 3), p = round(p, 3), n)])

## Grote en kleine projecten apart: stikstof zou vooral grote projecten
## moeten raken (meer depositie), maar een verschuiving van grote
## uitleglocaties aan de randen naar binnenstedelijk bouwen ook.
kop("Naar projectomvang, 2.500 m (objecten tellen)")
R$omvang <- rbindlist(lapply(names(UIT), function(u) {
  rbindlist(lapply(list(c(1, 19), c(20, Inf)), function(gr) {
    dt <- UIT[[u]][projectomvang >= gr[1] & projectomvang <= gr[2]]
    p <- paneel_gemeente(dt, 2500)
    rbindlist(lapply(c("C", "D"), function(sp) cbind(uitkomst = u, omvang = if (is.finite(gr[2])) "1 tot 19" else "20 of meer",
                                                     spec = sp, objecten = nrow(dt), fit_did(p, SPECS[[sp]]$fe, ~gemeentecode))))
  }))
}))
print(R$omvang[, .(uitkomst, omvang, spec, objecten, b = round(b, 3), se = round(se, 3), p = round(p, 3), n)])

## Trendbreuk voor grote projecten: een sprong bij de uitspraak past bij
## stikstof, een trend die al liep bij een verschuiving van de bouw.
kop("Trendbreuk en jaarverloop, projecten van 20 of meer objecten, 2.500 m")
R$omvang_trend <- list()
for (u in names(UIT)) {
  p <- paneel_gemeente(UIT[[u]][projectomvang >= 20], 2500)
  es <- fit_es(p, SPECS$C$fe, ~gemeentecode)
  jr <- es$tab[, .(b = mean(b)), by = .(jaar = substr(kw, 1, 4))]
  tc <- fit_trendbreuk(copy(p[t <= Q_SAL - 1L]), SPECS$C$fe, ~gemeentecode)
  td <- fit_trendbreuk(copy(p[t <= Q_SAL - 1L]), SPECS$D$fe, ~gemeentecode)
  R$omvang_trend[[u]] <- list(jaar_C = jr, F_provincie = tc, F_streng = td, pre_wald_C = wald_pre(es$model))
  cat(sprintf("%-12s jaargemiddelde log-IRR t.o.v. 2018 (C): %s\n", u, paste(sprintf("%s %+.2f", jr$jaar, jr$b), collapse = "  ")))
  cat(sprintf("%-12s pre-Wald C p=%.3f\n", u, wald_pre(es$model)$p))
  for (x in list(list("F provincie x kwartaal", tc), list("F streng", td))) {
    cat(sprintf("%-12s %-24s voortrend %+.4f/jr (%.4f)  sprong %+.3f (%.3f) p=%.3f  natrend %+.4f/jr (%.4f)\n", u, x[[1]],
                x[[2]]$b[1], x[[2]]$se[1], x[[2]]$b[2], x[[2]]$se[2], x[[2]]$p[2], x[[2]]$b[3], x[[2]]$se[3]))
  }
}

## ---------------------------------------------------------------------
## 8. Vergunde woonpanden met alle wisselingen: dip en herstel?
## ---------------------------------------------------------------------
## Het vergunningenspoor (eigen BAG-afleiding op pandniveau) zag na de
## PAS-uitspraak een dip in vergunde woonpanden nabij en herstel precies
## vanaf de bouwvrijstelling (2021Q3), maar schatte geen term voor die
## bouwvrijstelling. Hier dezelfde vraag op de GeoDMS-objectdata: een pand
## telt een keer, in het kwartaal van zijn eerste vergunning, met de
## kleinste afstandsklasse van zijn objecten. Elke wisseling krijgt een
## eigen sprong; de coefficienten zijn incrementeel.
kop("Vergunde woonpanden en objecten, alle wisselingen gestapeld, 2.500 m")
Q_BVR <- kw_index(as.Date("2021-07-01"))
Q_OW  <- kw_index(as.Date("2024-01-01"))
zet_wissels <- function(p) p[, `:=`(na_pas = D * as.integer(t >= Q_PAS), na_bvr = D * as.integer(t >= Q_BVR),
                                     na_por = D * as.integer(t >= Q_POR), na_ow = D * as.integer(t >= Q_OW),
                                     na_sal = D * as.integer(t >= Q_SAL))]
WISSELS <- "na_pas + na_bvr + na_por + na_ow + na_sal"
panden <- verg[!is.na(gemeentecode), .(afstandsklasse = min(afstandsklasse), q = min(q), gemeentecode = gemeentecode[1L],
                                       provinciecode = provinciecode[1L]), by = pand_bag_nr]
panden[, projectid := paste0("__pand_", .I)]   # een pand is hier zijn eigen eenheid
R$panden <- list(meta = list(panden = nrow(panden), objecten = nrow(verg)))
R$wissels <- list()
for (eenheid in c("woonpanden", "objecten")) {
  dt <- if (eenheid == "woonpanden") panden else verg
  p <- paneel_gemeente(dt, 2500)
  zet_wissels(p)
  es_rij <- list()
  for (sp in c("B", "C", "D")) {
    m <- fepois(as.formula(sprintf("y ~ %s | %s", WISSELS, SPECS[[sp]]$fe)), data = p, cluster = ~gemeentecode, notes = FALSE)
    R$wissels[[paste(eenheid, sp)]] <- cbind(eenheid = eenheid, spec = sp,
                                             rbindlist(lapply(c("na_pas", "na_bvr", "na_por", "na_ow", "na_sal"), function(v) coefrij(m, v))))
    es <- fit_es(p, SPECS[[sp]]$fe, ~gemeentecode)
    dip <- gem_regime(es$model, kw_index(as.Date("2019-07-01")), Q_BVR - 1L)
    herstel <- gem_regime(es$model, Q_BVR, Q_POR - 1L)
    es_rij[[sp]] <- data.table(eenheid = eenheid, spec = sp, pre_p = wald_pre(es$model)$p,
                               dip_b = dip$b, dip_se = dip$se, herstel_b = herstel$b, herstel_se = herstel$se)
    R$es_wissels[[paste(eenheid, sp)]] <- es$tab[, .(t, kw, b, se, irr, lo, hi)]
  }
  R$dip_herstel[[eenheid]] <- rbindlist(es_rij)
}
R$wissels <- rbindlist(R$wissels)
print(R$wissels[, .(eenheid, spec, term, b = round(b, 3), se = round(se, 3), p = round(p, 3), n)])

## Dezelfde sprongen, nu tegenover een doorgetrokken lineaire voortrend
## (trend nabij tegenover ver over de hele periode, sprongen erbovenop):
## een PAS-sprong die de voortrend overleeft en een herstel bij de
## bouwvrijstelling samen zouden op een echt effect wijzen.
R$wissels_trend <- rbindlist(lapply(c("woonpanden", "objecten"), function(eenheid) {
  dt <- if (eenheid == "woonpanden") panden else verg
  p <- paneel_gemeente(dt, 2500)
  zet_wissels(p)
  p[, voortrend := D * (t - Q_PAS) / 4]
  rbindlist(lapply(c("C", "D"), function(sp) {
    m <- fepois(as.formula(sprintf("y ~ voortrend + %s | %s", WISSELS, SPECS[[sp]]$fe)), data = p, cluster = ~gemeentecode, notes = FALSE)
    cbind(eenheid = eenheid, spec = sp, rbindlist(lapply(c("voortrend", "na_pas", "na_bvr", "na_por", "na_ow", "na_sal"), function(v) coefrij(m, v))))
  }))
}))
cat("\nmet lineaire voortrend (per jaar) en sprongen:\n")
print(R$wissels_trend[, .(eenheid, spec, term, b = round(b, 3), se = round(se, 3), p = round(p, 3))])
cat("\ngemiddelde event-study-coefficient in de dip (2019Q3-2021Q2) en het herstel (2021Q3-2022Q3), t.o.v. 2018:\n")
print(rbindlist(R$dip_herstel)[, .(eenheid, spec, pre_p = round(pre_p, 3), dip = sprintf("%+.3f (%.3f)", dip_b, dip_se),
                                   herstel = sprintf("%+.3f (%.3f)", herstel_b, herstel_se))])

R$samenvatting <- rbindlist(samenvatting, fill = TRUE)
kop("Samenvatting, 2.500 m")
print(R$samenvatting[drempel == 2500, .(uitkomst, spec, pre_p = round(pre_p, 3), irr_pas = round(irr_pas, 3),
                                        lo = round(irr_pas_lo, 3), hi = round(irr_pas_hi, 3),
                                        did = sprintf("%+.3f (%.3f)", did_b, did_se), did_p = round(did_p, 3), n)])

## ---------------------------------------------------------------------
## Wegschrijven
## ---------------------------------------------------------------------
json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("strikt_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("strikt_%s.json", EXTRACT)))
sink()
