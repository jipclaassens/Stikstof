## =====================================================================
## 05_eventstudy_kwartaal.R -- event study op kwartaalbasis, twee uitspraken
##
## Input : _derived/vbo_analyse.rds
## Output: _output/eventstudy_kw_<extract>.txt
##         _output/eventstudy_kw_<extract>_<uitkomst>.csv   (coefficienten)
##         _output/eventstudy_kw_<extract>_<uitkomst>.png   (figuur)
##
## Waarom kwartalen
## ---------------------------------------------------------------------
## De RERQ-tekst (sec. 4 en 5.1) zegt "per kwartaal" en "vaste effecten
## voor elk kwartaal"; de Stata-code aggregeert naar jaren. De bron is
## dag (vergunning) en maand (oplevering), dus kwartalen kosten niets.
## En het maakt uit: de PAS-uitspraak is 29 mei 2019, dus in jaardata is
## 2019 half voor en half na. Op kwartaalbasis is 2019Q1 nog schoon pre.
##
## De gebeurtenissen
## ---------------------------------------------------------------------
##   29-05-2019  PAS-uitspraak (ABRvS)          -> 2019Q2
##   01-07-2021  bouwvrijstelling in werking    -> 2021Q3   (versoepeling)
##   02-11-2022  Porthos: vrijstelling van tafel -> 2022Q4
##   01-01-2024  Omgevingswet + Wkb             -> 2024Q1   (meetbreuk BVV)
##   18-12-2024  intern salderen (Rendac/Amer)  -> 2024Q4
##   14-01-2025  Pasgeld-West: ook voor plannen -> 2025Q1
##
## De twee die ertoe doen zijn 2019Q2 en 2024Q4. Beide raken het hele
## land tegelijk; de identificatie komt uit de verschillende blootstelling
## (afstand tot stikstofgevoelig habitat), niet uit timing.
##
## Specificatie (identiek aan het paper, alleen kwartaal i.p.v. jaar):
##   Poisson, y = aantal per provincie x afstandsklasse x kwartaal
##   FE: provincie x kwartaal, provincie x afstandsklasse
##   cluster: provincie x afstandsklasse
##   referentie: 2019Q1, het laatste volle kwartaal voor de PAS-uitspraak
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))

sink(file.path(PATHS$output, sprintf("eventstudy_kw_%s.txt", EXTRACT)), split = TRUE)

d <- read_derived("vbo_analyse")

## ---------------------------------------------------------------------
## Kwartaalindex en gebeurtenissen
## ---------------------------------------------------------------------

KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)

EVENTS <- data.table(
  naam  = c("PAS", "bouwvrijstelling", "Porthos", "Omgevingswet", "salderen"),
  datum = as.Date(c("2019-05-29", "2021-07-01", "2022-11-02", "2024-01-01", "2024-12-18")),
  hoofd = c(TRUE, FALSE, FALSE, FALSE, TRUE))
EVENTS[, q := kw_index(datum)]
EVENTS[, kw := kw_label(q)]
Q_PAS <- EVENTS[naam == "PAS", q]
Q_SAL <- EVENTS[naam == "salderen", q]
Q_POR <- EVENTS[naam == "Porthos", q]

## Referentieperiode: de vier kwartalen van 2018, net als het jaar 2018 in
## het paper. Een enkel referentiekwartaal is te ruis -- 2019Q1 bleek een
## dipkwartaal voor behandeld en controle allebei, en dan worden alle
## andere coefficienten daar tegen opgeblazen.
Q_REF <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_REF_MAX <- max(Q_REF)

kop("Gebeurtenissen en kwartaalindex (1 = %dQ1)", KW_START)
print(EVENTS[, .(naam, datum, kwartaal = kw, q, hoofd)])
cat(sprintf("\nreferentieperiode: %s t/m %s (q = %d-%d)\n",
            kw_label(min(Q_REF)), kw_label(max(Q_REF)), min(Q_REF), max(Q_REF)))

## Laatste VOLLEDIGE kwartaal per uitkomst. Vergunningen lopen tot
## 2026-07-08 (dus 2026Q3 is 8 dagen), opleveringen tot 2026-07.
## Juni 2026 bevat een batchregistratie van BVV-panden (Meierijstad en
## Bernheze, 21% van de maand; docs/BAG-dataschoning-inzichten.md sec. 7).
## Dat kwartaal is voor vergunningen daarom niet bruikbaar: index 2,39
## tegen ~1,0 in de kwartalen ervoor. Opleveringen zijn er niet door
## geraakt (index 0,99) en lopen door t/m 2026Q2.
q_max_verg <- kw_index(as.Date("2026-03-31"))
q_max_nwb  <- kw_index(as.Date("2026-06-30"))
cat(sprintf("laatste kwartaal vergunningen: %s (2026Q2 uitgesloten: batchregistratie)\n",
            kw_label(q_max_verg)))
cat(sprintf("laatste kwartaal nieuwbouw   : %s\n", kw_label(q_max_nwb)))

## ---------------------------------------------------------------------
## Steekproeven
## ---------------------------------------------------------------------

verg <- d[in_sample == TRUE & !is.na(vergunning_datum) &
            vergunning_datum >= as.Date(sprintf("%d-01-01", KW_START))]
verg[, q := kw_index(vergunning_datum)]
verg <- verg[q >= 1L & q <= q_max_verg]

nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" &
           !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]
nwb <- nwb[q >= 1L & q <= q_max_nwb]

cat(sprintf("\nvergunningen : %s objecten, %s t/m %s\n", nfmt(nrow(verg)),
            kw_label(min(verg$q)), kw_label(max(verg$q))))
cat(sprintf("nieuwbouw    : %s objecten, %s t/m %s\n", nfmt(nrow(nwb)),
            kw_label(min(nwb$q)), kw_label(max(nwb$q))))

## ---------------------------------------------------------------------
## Paneel: provincie x afstandsklasse x kwartaal
## ---------------------------------------------------------------------

bouw_paneel_kw <- function(dt) {
  stopifnot(!any(is.na(dt$projectid)))
  per_project <- dt[, .(kl = min(afstandsklasse), n = .N), by = .(projectid, provinciecode, q)]
  cel <- per_project[, .(y = sum(n)), by = .(provinciecode, kl, q)]
  k_max <- as.integer(PARAMS$klasse_max_m / PARAMS$klasse_breedte) + 1L
  vol <- CJ(provinciecode = sort(unique(cel$provinciecode)), kl = 1:k_max,
            q = min(cel$q):max(cel$q))
  p <- merge(vol, cel, by = c("provinciecode", "kl", "q"), all.x = TRUE)
  p[is.na(y), y := 0]
  for (dr in PARAMS$drempels) {
    p[, paste0("kl", dr) := as.integer(kl <= dr / PARAMS$klasse_breedte)]
  }
  ## Drie vaste effecten:
  ##   prov_kw  provincie x kwartaal        -- autonome ontwikkeling
  ##   prov_kl  provincie x afstandsklasse  -- niveauverschillen
  ##   kl_szn   afstandsklasse x seizoen    -- NIEUW t.o.v. het paper.
  ## Nabij N2000 is landelijker en grondgebondener, dus seizoensgevoeliger
  ## dan ver ervan. prov_kw absorbeert het gemiddelde seizoenspatroon,
  ## niet het verschil tussen de klassen. Op jaarbasis speelt dat niet;
  ## op kwartaalbasis zit het anders in de pre-trend.
  p[, `:=`(prov_kw = paste(provinciecode, q),
           prov_kl = paste(provinciecode, kl),
           kl_szn  = paste(kl, (q - 1L) %% 4L))]
  p[]
}

pv <- bouw_paneel_kw(verg)
pn <- bouw_paneel_kw(nwb)

## ---------------------------------------------------------------------
## Schatters
## ---------------------------------------------------------------------

## Event study: een dummy per kwartaal x behandeld, referentie 2019Q1
event_study <- function(p, drempel) {
  p[, D := get(paste0("kl", drempel))]
  m <- fepois(y ~ i(q, D, ref = Q_REF) | prov_kw + prov_kl + kl_szn, data = p,
              cluster = ~prov_kl, notes = FALSE)
  cf <- coef(m); s <- se(m)
  qq <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  out <- data.table(q = qq, kw = kw_label(qq), b = cf, se = s,
                    irr = exp(cf), lo = exp(cf - 1.96 * s), hi = exp(cf + 1.96 * s))
  out <- rbind(out, data.table(q = Q_REF, kw = kw_label(Q_REF), b = 0, se = 0,
                               irr = 1, lo = 1, hi = 1))
  list(model = m, tab = out[order(q)])
}

## Joint Wald op de pre-periode (alles voor de referentieperiode)
pre_wald <- function(m) {
  b <- coef(m); V <- vcov(m)
  qq <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(qq < min(Q_REF))
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  c(stat = W, df = length(idx), p = pchisq(W, length(idx), lower.tail = FALSE))
}

## Gemiddeld effect per regime uit de event study (zoals lincom in Stata)
regime_gemiddelde <- function(m, q_van, q_tot) {
  b <- coef(m); V <- vcov(m)
  qq <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(qq >= q_van & qq <= q_tot)
  w <- rep(1 / length(idx), length(idx))
  est <- sum(w * b[idx]); se <- sqrt(as.numeric(t(w) %*% V[idx, idx] %*% w))
  c(irr = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se), n_kw = length(idx))
}

## Regime-DiD: drie periodes, twee stappen
regime_did <- function(p, drempel) {
  p[, D := get(paste0("kl", drempel))]
  p[, `:=`(na_pas = as.integer(q >= Q_PAS) * D,
           na_por = as.integer(q >= Q_POR) * D,
           na_sal = as.integer(q >= Q_SAL) * D)]
  m2 <- fepois(y ~ na_pas + na_sal | prov_kw + prov_kl + kl_szn, data = p, cluster = ~prov_kl, notes = FALSE)
  m3 <- fepois(y ~ na_pas + na_por + na_sal | prov_kw + prov_kl + kl_szn, data = p, cluster = ~prov_kl, notes = FALSE)
  list(twee = m2, drie = m3)
}

## ---------------------------------------------------------------------
## Draaien
## ---------------------------------------------------------------------

for (uitkomst in c("vergunningen", "nieuwbouw")) {
  p <- if (uitkomst == "vergunningen") pv else pn
  q_max <- max(p$q)
  kop("%s -- event study per kwartaal", toupper(uitkomst))

  alle <- list()
  for (dr in c(500, 1000, 2500, 5000)) {
    es <- event_study(p, dr)
    w  <- pre_wald(es$model)
    g1 <- regime_gemiddelde(es$model, Q_PAS, Q_SAL - 1L)
    g2 <- regime_gemiddelde(es$model, Q_SAL, q_max)

    cat(sprintf("\n--- drempel %d m ---\n", dr))
    cat(sprintf("pre-trend (joint Wald, %d kwartalen 2012Q1-2017Q4): stat = %.1f, p = %.3f\n",
                w["df"], w["stat"], w["p"]))
    cat(sprintf("gemiddeld %s t/m %s : IRR %.3f [%.3f - %.3f]  (%d kwartalen)\n",
                kw_label(Q_PAS), kw_label(Q_SAL - 1L), g1["irr"], g1["lo"], g1["hi"], g1["n_kw"]))
    cat(sprintf("gemiddeld %s t/m %s : IRR %.3f [%.3f - %.3f]  (%d kwartalen)\n",
                kw_label(Q_SAL), kw_label(q_max), g2["irr"], g2["lo"], g2["hi"], g2["n_kw"]))

    es$tab[, drempel := dr]
    alle[[as.character(dr)]] <- es$tab
  }
  es_all <- rbindlist(alle)
  fwrite(es_all, file.path(PATHS$output, sprintf("eventstudy_kw_%s_%s.csv", EXTRACT, uitkomst)))

  kop("%s -- regime-DiD (coefficient, se)", toupper(uitkomst))
  cat("na_pas = behandeld x 1{q >= 2019Q2}; na_sal = behandeld x 1{q >= 2024Q4};\n")
  cat("na_por = behandeld x 1{q >= 2022Q4}. Coefficienten zijn INCREMENTEEL:\n")
  cat("het effect na 2024Q4 is na_pas + na_sal.\n\n")
  rd <- rbindlist(lapply(c(500, 1000, 2500, 5000), function(dr) {
    r <- regime_did(p, dr)
    data.table(drempel = paste0(dr, " m"),
               pas    = sprintf("%+.3f (%.3f)", coef(r$twee)["na_pas"], se(r$twee)["na_pas"]),
               sal    = sprintf("%+.3f (%.3f)", coef(r$twee)["na_sal"], se(r$twee)["na_sal"]),
               pas_3  = sprintf("%+.3f (%.3f)", coef(r$drie)["na_pas"], se(r$drie)["na_pas"]),
               por_3  = sprintf("%+.3f (%.3f)", coef(r$drie)["na_por"], se(r$drie)["na_por"]),
               sal_3  = sprintf("%+.3f (%.3f)", coef(r$drie)["na_sal"], se(r$drie)["na_sal"]))
  }))
  print(rd)

  ## ---- figuur: vier panelen, IRR met 95%-BI, verticale lijnen bij de uitspraken
  png(file.path(PATHS$output, sprintf("eventstudy_kw_%s_%s.png", EXTRACT, uitkomst)),
      width = 2000, height = 1400, res = 150)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  for (dr in c(500, 1000, 2500, 5000)) {
    t <- es_all[drempel == dr][order(q)]
    ylim <- c(min(0.4, min(t$lo)), max(2.0, max(t$hi)))
    ylim <- c(max(0.2, ylim[1]), min(3, ylim[2]))
    plot(t$q, t$irr, type = "n", ylim = ylim, xaxt = "n", xlab = "", ylab = "IRR t.o.v. 2018",
         main = sprintf("%s, behandeld = tot %d m", uitkomst, dr))
    abline(h = 1, col = "grey50")
    for (i in seq_len(nrow(EVENTS))) {
      abline(v = EVENTS$q[i] - 0.5, col = if (EVENTS$hoofd[i]) "firebrick" else "grey70",
             lty = if (EVENTS$hoofd[i]) 1 else 3, lwd = if (EVENTS$hoofd[i]) 2 else 1)
    }
    polygon(c(t$q, rev(t$q)), c(t$lo, rev(t$hi)), col = adjustcolor("steelblue", 0.2), border = NA)
    lines(t$q, t$irr, col = "steelblue4", lwd = 1.5)
    points(t$q, t$irr, pch = 16, cex = 0.6, col = "steelblue4")
    jaren <- seq(1, max(t$q), by = 4)
    axis(1, at = jaren, labels = KW_START + (jaren - 1) %/% 4, las = 2, cex.axis = 0.8)
  }
  dev.off()
  cat(sprintf("\nfiguur: eventstudy_kw_%s_%s.png\n", EXTRACT, uitkomst))
}

## ---------------------------------------------------------------------
## Descriptief: geindexeerde reeksen (zoals figuur 1 in het paper)
## ---------------------------------------------------------------------

kop("Descriptief: kwartaalreeksen, index 2018 = 1, behandeld = tot 2500 m")
for (uitkomst in c("vergunningen", "nieuwbouw")) {
  p <- if (uitkomst == "vergunningen") pv else pn
  s <- p[, .(y = sum(y)), by = .(q, D = kl2500)]
  basis <- s[q >= kw_index(as.Date("2018-01-01")) & q <= kw_index(as.Date("2018-12-31")),
             .(basis = mean(y)), by = D]
  s <- merge(s, basis, by = "D")
  s[, index := round(y / basis, 3)]
  w <- dcast(s[q >= kw_index(as.Date("2018-01-01"))], q ~ D, value.var = "index")
  setnames(w, c("0", "1"), c("controle", "behandeld"))
  w[, kw := kw_label(q)]
  w[, verschil := round(behandeld - controle, 3)]
  cat(sprintf("\n%s:\n", uitkomst))
  print(w[, .(kw, controle, behandeld, verschil)], nrows = 40)
}

sink()
