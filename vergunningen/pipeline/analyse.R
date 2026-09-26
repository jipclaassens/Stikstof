# Statistische analyse van de vergunningenzaken: aanvragen, uitkomsten en doorlooptijd nabij stikstofgevoelig habitat.
#
# Gebruik: Rscript analyse.R <analysemap>        (de map met zaken.csv, panel.csv en uitspraken.csv uit analyse_export.py)
# Schrijft in dezelfde map: resultaten.json (alle tabellen en schattingen) en de tabellen als csv.
#
# Dezelfde modelfamilie als de BAG-analyse in R/: Poisson met vaste effecten (fepois) voor aantallen, lineair (feols) voor
# aandelen en log-doorlooptijd, vaste effecten voor gemeente maal nabij en voor kwartaal, standaardfouten geclusterd op
# gemeente. Nabij is binnen 2.500 meter van de AERIUS relevante habitatkartering, de specificatie die in de BAG-analyse
# het verhaal droeg. Gebeurtenissen binnen het waarnemingsvenster (juli 2021 tot september 2026): Porthos (2 november
# 2022, einde bouwvrijstelling), Omgevingswet (1 januari 2024), uitspraak intern salderen (18 december 2024).
# De PAS-uitspraak (29 mei 2019) valt voor het venster; daarvoor is de oogst van het Gemeenteblad 2014 tot 2021 nodig.

suppressMessages({ library(data.table); library(fixest); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
map <- if (length(args)) args[1] else "C:/LocalData/vergunningen/analyse"
zaken <- fread(file.path(map, "zaken.csv"), sep = ";", encoding = "UTF-8")
panel <- fread(file.path(map, "panel.csv"), sep = ";", encoding = "UTF-8")
uitspraken <- fread(file.path(map, "uitspraken.csv"), sep = ";", encoding = "UTF-8")
res <- list(meta = list(datum = format(Sys.time(), "%Y-%m-%d %H:%M"), zaken = nrow(zaken), panelcellen = nrow(panel),
                        uitspraken = nrow(uitspraken)))

kw_orde <- function(k) { j <- as.integer(substr(k, 1, 4)); q <- as.integer(substr(k, 6, 6)); j * 4 + q - 1 }
zaken[, kw_idx := kw_orde(kwartaal)]
panel[, kw_idx := kw_orde(kwartaal)]
# De hele reeks (sinds 2013, per gemeente alles of niets) blijft apart voor het vaste panel in blok 7; de hoofdmodellen
# gebruiken alleen het landelijk dekkende venster vanaf 2021K3 (Wet elektronische publicaties, 5 juli 2021), anders
# schuift de samenstelling van het panel met de tijd mee en vervuilt dat de kwartaaleffecten.
zaken_alles <- copy(zaken)
panel_alles <- copy(panel)
venster <- kw_orde("2021K3")
zaken <- zaken[kw_idx >= venster]
panel <- panel[kw_idx >= venster]
res$meta$zaken <- nrow(zaken)
res$meta$zaken_alle_jaren <- nrow(zaken_alles)
res$meta$panelcellen <- nrow(panel)
laatste_kw <- max(panel$kw_idx)
# gebeurtenissen als kwartaalindex van het eerste volledig behandelde kwartaal
ev <- list(porthos = kw_orde("2023K1"), omgevingswet = kw_orde("2024K1"), intern_salderen = kw_orde("2025K1"))
panel[, `:=`(post_porthos = as.integer(kw_idx >= ev$porthos), post_ow = as.integer(kw_idx >= ev$omgevingswet),
             post_is = as.integer(kw_idx >= ev$intern_salderen))]
zaken[, `:=`(post_porthos = as.integer(kw_idx >= ev$porthos), post_ow = as.integer(kw_idx >= ev$omgevingswet),
             post_is = as.integer(kw_idx >= ev$intern_salderen))]

coefs <- function(m, naam) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "schatting", "se", "t", "p"))
  ct[, `:=`(model = naam, n = nobs(m))]
  ct[]
}
veilig <- function(expr, naam) tryCatch(expr, error = function(e) { message(naam, ": ", conditionMessage(e)); NULL })

# 1. beschrijvend: per kwartaal en nabij
besch <- panel[, .(aanvragen = sum(aanvragen), verleend = sum(verleend), geweigerd = sum(geweigerd), ingetrokken = sum(ingetrokken),
                   geen = sum(geen), verlengingen = sum(verlengingen), bouw_aanvragen = sum(bouw_aanvragen)), by = .(kwartaal, nabij)][order(kwartaal, nabij)]
res$beschrijvend_kwartaal_nabij <- besch
res$beschrijvend_klasse <- zaken[, .(aanvragen = .N, verleend = mean(verleend), geweigerd = mean(geweigerd), ingetrokken = mean(ingetrokken),
                                      geen = mean(geen), mediaan_dagen = as.numeric(median(doorlooptijd[verleend == 1], na.rm = TRUE)),
                                      verlengingen_per_100 = 100 * mean(verlengingen)), by = klasse][order(klasse)]

# 2. event study op aanvragen: nabij x kwartaal, referentie het laatste kwartaal voor Porthos (2022K4 is deels behandeld)
ref_kw <- "2022K3"
if (ref_kw %in% panel$kwartaal && uniqueN(panel$kwartaal) > 3) {
  panel[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
  m_es <- veilig(fepois(aanvragen ~ i(kwartaal_f, nabij, ref = ref_kw) | gemeente^nabij + kwartaal_f, data = panel, cluster = ~gemeente), "event study aanvragen")
  if (!is.null(m_es)) res$event_study_aanvragen <- coefs(m_es, "fepois aanvragen ~ nabij x kwartaal | gemeente^nabij + kwartaal")
  m_esb <- veilig(fepois(bouw_aanvragen ~ i(kwartaal_f, nabij, ref = ref_kw) | gemeente^nabij + kwartaal_f, data = panel, cluster = ~gemeente), "event study bouwaanvragen")
  if (!is.null(m_esb)) res$event_study_bouwaanvragen <- coefs(m_esb, "fepois bouw_aanvragen ~ nabij x kwartaal | gemeente^nabij + kwartaal")
}

# 3. difference in differences met de drie regimewisselingen tegelijk (gestapeld: elke term is de extra verandering nabij vanaf die datum)
m_did <- veilig(fepois(aanvragen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = panel, cluster = ~gemeente), "did aanvragen")
if (!is.null(m_did)) res$did_aanvragen <- coefs(m_did, "fepois aanvragen ~ nabij x post | gemeente^nabij + kwartaal")
m_didb <- veilig(fepois(bouw_aanvragen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = panel, cluster = ~gemeente), "did bouwaanvragen")
if (!is.null(m_didb)) res$did_bouwaanvragen <- coefs(m_didb, "fepois bouw_aanvragen ~ nabij x post | gemeente^nabij + kwartaal")

# 4. uitkomsten op zaakniveau; alleen aanvragen met minstens negen maanden vervolg zodat de uitkomst bekend kan zijn
einde <- as.IDate(max(zaken$datum_aanvraag))
z9 <- zaken[as.IDate(datum_aanvraag) <= einde - 270]
z9[, log_dl := ifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
uitk <- list()
for (y in c("geweigerd", "ingetrokken", "geen", "verleend")) {
  m <- veilig(feols(as.formula(paste(y, "~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")), data = z9, cluster = ~gemeente), y)
  if (!is.null(m)) uitk[[y]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal"))
}
m <- veilig(feols(log_dl ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = z9[verleend == 1], cluster = ~gemeente), "doorlooptijd")
if (!is.null(m)) uitk[["log_doorlooptijd_verleend"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + kwartaal, verleend")
m <- veilig(fepois(verlengingen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = z9, cluster = ~gemeente), "verlengingen")
if (!is.null(m)) uitk[["verlengingen"]] <- coefs(m, "fepois verlengingen ~ nabij x post | gemeente^nabij + kwartaal")
res$did_uitkomsten <- rbindlist(uitk)

# 5. afstandsgradient over de hele periode, met gemeente- en kwartaaleffecten (beschrijvend, geen causale lezing)
z9[, klasse_f := factor(klasse, levels = c("5000_", "2500_5000", "1000_2500", "500_1000", "250_500", "0_250", "onbekend"))]
grad <- list()
for (y in c("geweigerd", "ingetrokken", "geen", "verleend")) {
  m <- veilig(feols(as.formula(paste(y, "~ i(klasse_f, ref = '5000_') | gemeente + kwartaal")), data = z9, cluster = ~gemeente), paste("gradient", y))
  if (!is.null(m)) grad[[y]] <- coefs(m, paste("feols", y, "~ klasse | gemeente + kwartaal"))
}
m <- veilig(feols(log_dl ~ i(klasse_f, ref = "5000_") | gemeente + kwartaal, data = z9[verleend == 1], cluster = ~gemeente), "gradient doorlooptijd")
if (!is.null(m)) grad[["log_doorlooptijd_verleend"]] <- coefs(m, "feols log(doorlooptijd) ~ klasse | gemeente + kwartaal, verleend")
m <- veilig(fepois(verlengingen ~ i(klasse_f, ref = "5000_") | gemeente + kwartaal, data = z9, cluster = ~gemeente), "gradient verlengingen")
if (!is.null(m)) grad[["verlengingen"]] <- coefs(m, "fepois verlengingen ~ klasse | gemeente + kwartaal")
res$gradient_klasse <- rbindlist(grad)

# 5b. per projecttype: nieuwbouw van woningen als doelgroep, klein werk en verbouw als placebo (stikstof kan een dakkapel niet raken)
if ("soort" %in% names(zaken)) {
  res$soort_verdeling <- zaken[, .(aanvragen = .N, nabij = sum(nabij), verleend = mean(verleend), geweigerd = mean(geweigerd),
                                   ingetrokken = mean(ingetrokken), geen = mean(geen),
                                   mediaan_dagen = as.numeric(median(doorlooptijd[verleend == 1], na.rm = TRUE))), by = soort][order(-aanvragen)]
  ps <- zaken[, .(aanvragen = .N), by = .(gemeente, kwartaal, nabij, soort)]
  ps[, kw_idx := kw_orde(kwartaal)]
  ps[, `:=`(post_porthos = as.integer(kw_idx >= ev$porthos), post_ow = as.integer(kw_idx >= ev$omgevingswet), post_is = as.integer(kw_idx >= ev$intern_salderen))]
  ps_full <- CJ(gemeente = unique(ps$gemeente), kwartaal = unique(ps$kwartaal), nabij = 0:1, soort = unique(ps$soort))
  ps <- merge(ps_full, ps, by = c("gemeente", "kwartaal", "nabij", "soort"), all.x = TRUE)
  ps[is.na(aanvragen), aanvragen := 0]
  ps[, kw_idx := kw_orde(kwartaal)]
  ps[, `:=`(post_porthos = as.integer(kw_idx >= ev$porthos), post_ow = as.integer(kw_idx >= ev$omgevingswet), post_is = as.integer(kw_idx >= ev$intern_salderen))]
  soort_did <- list(); soort_uitk <- list(); soort_grad <- list()
  for (s in c("nieuwbouw_woning", "nieuwbouw_overig", "klein_verbouw", "sloop_kap")) {
    m <- veilig(fepois(aanvragen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = ps[soort == s], cluster = ~gemeente), paste("did", s))
    if (!is.null(m)) soort_did[[s]] <- coefs(m, paste("fepois aanvragen ~ nabij x post | gemeente^nabij + kwartaal,", s))
    zs <- z9[soort == s]
    for (y in c("geweigerd", "ingetrokken", "geen", "verleend")) {
      m <- veilig(feols(as.formula(paste(y, "~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")), data = zs, cluster = ~gemeente), paste(s, y))
      if (!is.null(m)) soort_uitk[[paste(s, y)]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal,", s))
    }
    m <- veilig(feols(log_dl ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = zs[verleend == 1], cluster = ~gemeente), paste(s, "doorlooptijd"))
    if (!is.null(m)) soort_uitk[[paste(s, "log_doorlooptijd")]] <- coefs(m, paste("feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + kwartaal, verleend,", s))
    for (y in c("verleend", "geen")) {
      m <- veilig(feols(as.formula(paste(y, "~ i(klasse_f, ref = '5000_') | gemeente + kwartaal")), data = zs, cluster = ~gemeente), paste("gradient", s, y))
      if (!is.null(m)) soort_grad[[paste(s, y)]] <- coefs(m, paste("feols", y, "~ klasse | gemeente + kwartaal,", s))
    }
    m <- veilig(feols(log_dl ~ i(klasse_f, ref = "5000_") | gemeente + kwartaal, data = zs[verleend == 1], cluster = ~gemeente), paste("gradient", s, "doorlooptijd"))
    if (!is.null(m)) soort_grad[[paste(s, "log_doorlooptijd")]] <- coefs(m, paste("feols log(doorlooptijd) ~ klasse | gemeente + kwartaal, verleend,", s))
  }
  res$soort_did_aanvragen <- rbindlist(soort_did)
  res$soort_did_uitkomsten <- rbindlist(soort_uitk)
  res$soort_gradient <- rbindlist(soort_grad)
}

# 5c. alternatieve blootstelling uit de AERIUS-hexagonen: nabij een overbelast hexagon (binnen 2.500 m) in plaats van nabij habitat,
#     en de mate van overbelasting binnen 5 km in tertielen (ovb_max_5km); dezelfde modellen als 3 en 4
if ("nabij_overbelast" %in% names(zaken) && any(!is.na(zaken$nabij_overbelast))) {
  zo <- zaken[!is.na(nabij_overbelast)]
  zo[, nabij := as.integer(nabij_overbelast)]
  po <- zo[, .(aanvragen = .N, bouw_aanvragen = sum(bouw)), by = .(gemeente, kwartaal, nabij)]
  po_full <- CJ(gemeente = unique(po$gemeente), kwartaal = unique(po$kwartaal), nabij = 0:1)
  po <- merge(po_full, po, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
  po[is.na(aanvragen), `:=`(aanvragen = 0, bouw_aanvragen = 0)]
  po[, kw_idx := kw_orde(kwartaal)]
  po[, `:=`(post_porthos = as.integer(kw_idx >= ev$porthos), post_ow = as.integer(kw_idx >= ev$omgevingswet), post_is = as.integer(kw_idx >= ev$intern_salderen))]
  alt <- list()
  m <- veilig(fepois(aanvragen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = po, cluster = ~gemeente), "alt did aanvragen")
  if (!is.null(m)) alt[["aanvragen"]] <- coefs(m, "fepois aanvragen ~ nabij_overbelast x post | gemeente^nabij + kwartaal")
  zo9 <- zo[as.IDate(datum_aanvraag) <= einde - 270]
  zo9[, log_dl := ifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
  for (y in c("verleend", "geweigerd", "ingetrokken", "geen")) {
    m <- veilig(feols(as.formula(paste(y, "~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")), data = zo9, cluster = ~gemeente), paste("alt", y))
    if (!is.null(m)) alt[[y]] <- coefs(m, paste("feols", y, "~ nabij_overbelast x post | gemeente^nabij + kwartaal"))
  }
  m <- veilig(feols(log_dl ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = zo9[verleend == 1], cluster = ~gemeente), "alt doorlooptijd")
  if (!is.null(m)) alt[["log_doorlooptijd"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij_overbelast x post | gemeente^nabij + kwartaal, verleend")
  if ("soort" %in% names(zo9)) {
    m <- veilig(feols(log_dl ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = zo9[verleend == 1 & soort == "nieuwbouw_woning"], cluster = ~gemeente), "alt doorlooptijd nieuwbouw")
    if (!is.null(m)) alt[["log_doorlooptijd_nieuwbouw"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij_overbelast x post | gemeente^nabij + kwartaal, verleend, nieuwbouw_woning")
  }
  # mate van overbelasting binnen 5 km, tertielen over zaken met een overbelast hexagon binnen 5 km
  zo9[, ovb5 := as.numeric(ovb_max_5km)]
  zo9[, ovb_tertiel := fifelse(is.na(ovb5) | ovb5 <= 0, "geen", fifelse(ovb5 <= quantile(ovb5[ovb5 > 0], 1/3, na.rm = TRUE), "laag",
                           fifelse(ovb5 <= quantile(ovb5[ovb5 > 0], 2/3, na.rm = TRUE), "midden", "hoog")))]
  zo9[, ovb_tertiel := factor(ovb_tertiel, levels = c("geen", "laag", "midden", "hoog"))]
  for (y in c("verleend", "geen")) {
    m <- veilig(feols(as.formula(paste(y, "~ i(ovb_tertiel, post_porthos, ref = 'geen') | gemeente^ovb_tertiel + kwartaal")), data = zo9, cluster = ~gemeente), paste("tertiel", y))
    if (!is.null(m)) alt[[paste("tertiel", y)]] <- coefs(m, paste("feols", y, "~ overbelasting 5 km (tertiel) x post_porthos | gemeente^tertiel + kwartaal"))
  }
  m <- veilig(feols(log_dl ~ i(ovb_tertiel, post_porthos, ref = "geen") | gemeente^ovb_tertiel + kwartaal, data = zo9[verleend == 1], cluster = ~gemeente), "tertiel doorlooptijd")
  if (!is.null(m)) alt[["tertiel log_doorlooptijd"]] <- coefs(m, "feols log(doorlooptijd) ~ overbelasting 5 km (tertiel) x post_porthos | gemeente^tertiel + kwartaal, verleend")
  res$alt_overbelast <- rbindlist(alt)
  res$alt_overbelast_verdeling <- zo[, .(aanvragen = .N, aandeel_nabij_habitat = mean(nabij_overbelast == 1 & get("nabij") == 1)), by = .(overbelast_klasse)][order(overbelast_klasse)]
}

# 6. beroep uit de rechtspraak
if (nrow(uitspraken)) {
  res$beroep_per_jaar_object <- uitspraken[jaar != "", .N, by = .(jaar, object)][order(jaar, object)]
  res$beroep_uitkomst <- uitspraken[object %in% c("omgevingsvergunning", "bestemmingsplan", "omgevingsplan"), .N, by = .(object, uitkomst)][order(object, -N)]
  res$beroep_stikstof_per_jaar <- uitspraken[jaar != "", .(uitspraken = .N, stikstof = sum(stikstof == 1), natura2000 = sum(natura2000 == 1),
                                                          aandeel_stikstof = round(mean(stikstof == 1), 3)), by = jaar][order(jaar)]
  res$beroep_duur <- uitspraken[!is.na(dagen_besluit_uitspraak) & dagen_besluit_uitspraak > 0 & dagen_besluit_uitspraak < 3000,
                                .(n = .N, mediaan_dagen = as.numeric(median(dagen_besluit_uitspraak)), p75 = as.numeric(quantile(dagen_besluit_uitspraak, .75))),
                                by = .(jaar, object)][object %in% c("omgevingsvergunning", "bestemmingsplan", "omgevingsplan")][order(object, jaar)]
  res$beroep_per_gemeente <- uitspraken[object == "omgevingsvergunning" & gemeente != "", .N, by = gemeente][order(-N)][1:30]
}

# 7. het vaste panel van gemeenten die al voor 2021 in het Gemeenteblad publiceerden (stroom oud): de PAS-uitspraak in het
#    venster van de bekendmakingen, 2016K1 tot 2022K2 (voor Porthos), referentie 2019K1; alleen gemeenten met minstens
#    veertig aanvragen in elk van de jaren 2016 tot 2020, zodat de samenstelling van het panel niet met de tijd verschuift
if (min(panel_alles$kw_idx) <= kw_orde("2016K1")) {
  zaken <- zaken_alles
  panel <- panel_alles
  per_jaar <- zaken[, .(n = .N), by = .(gemeente, jaar = substr(kwartaal, 1, 4))]
  vast <- per_jaar[jaar %in% as.character(2016:2020) & n >= 40, .N, by = gemeente][N == 5, gemeente]
  po <- panel[gemeente %in% vast & kw_idx >= kw_orde("2016K1") & kw_idx <= kw_orde("2022K2")]
  po[, post_pas := as.integer(kw_idx >= kw_orde("2019K3"))]
  po[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
  res$oud_meta <- list(gemeenten = length(vast), cellen = nrow(po), aanvragen = sum(po$aanvragen), nabij = sum(po[nabij == 1, aanvragen]))
  m <- veilig(fepois(aanvragen ~ i(kwartaal_f, nabij, ref = "2019K1") | gemeente^nabij + kwartaal_f, data = po, cluster = ~gemeente), "oud event study")
  if (!is.null(m)) res$oud_event_study_aanvragen <- coefs(m, "fepois aanvragen ~ nabij x kwartaal | gemeente^nabij + kwartaal, vast panel 2016K1 tot 2022K2")
  m <- veilig(fepois(bouw_aanvragen ~ i(kwartaal_f, nabij, ref = "2019K1") | gemeente^nabij + kwartaal_f, data = po, cluster = ~gemeente), "oud event study bouw")
  if (!is.null(m)) res$oud_event_study_bouwaanvragen <- coefs(m, "fepois bouw_aanvragen ~ nabij x kwartaal | gemeente^nabij + kwartaal, vast panel 2016K1 tot 2022K2")
  od <- list()
  m <- veilig(fepois(aanvragen ~ nabij:post_pas | gemeente^nabij + kwartaal, data = po, cluster = ~gemeente), "oud did")
  if (!is.null(m)) od[["aanvragen"]] <- coefs(m, "fepois aanvragen ~ nabij x post_pas | gemeente^nabij + kwartaal, vast panel 2016K1 tot 2022K2")
  m <- veilig(fepois(bouw_aanvragen ~ nabij:post_pas | gemeente^nabij + kwartaal, data = po, cluster = ~gemeente), "oud did bouw")
  if (!is.null(m)) od[["bouw_aanvragen"]] <- coefs(m, "fepois bouw_aanvragen ~ nabij x post_pas | gemeente^nabij + kwartaal, vast panel 2016K1 tot 2022K2")
  # dezelfde trendbreuktoets als in analyse_blootstelling.R: lineaire voortrend nabij, sprong bij de uitspraak, natrend
  po[, t := (kw_idx - kw_orde("2019K3")) / 4]
  po[, `:=`(t_voor = nabij * t * as.integer(post_pas == 0), t_na = nabij * t * post_pas, sprong = nabij * post_pas)]
  m <- veilig(fepois(aanvragen ~ t_voor + sprong + t_na | gemeente^nabij + kwartaal, data = po, cluster = ~gemeente), "oud trendbreuk")
  if (!is.null(m)) od[["trendbreuk"]] <- coefs(m, "fepois aanvragen ~ nabij x (trend voor, sprong, trend na) | gemeente^nabij + kwartaal, vast panel 2016K1 tot 2022K2")
  res$oud_did_aanvragen <- rbindlist(od)
  zo <- zaken[gemeente %in% vast & kw_idx >= kw_orde("2016K1") & kw_idx <= kw_orde("2022K2")]
  zo[, post_pas := as.integer(kw_idx >= kw_orde("2019K3"))]
  zo[, log_dl := ifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
  uo <- list()
  for (y in c("verleend", "geweigerd", "ingetrokken", "geen", "besluit_onbekend")) {
    m <- veilig(feols(as.formula(paste(y, "~ nabij:post_pas | gemeente^nabij + kwartaal")), data = zo, cluster = ~gemeente), paste("oud", y))
    if (!is.null(m)) uo[[y]] <- coefs(m, paste("feols", y, "~ nabij x post_pas | gemeente^nabij + kwartaal, vast panel"))
  }
  m <- veilig(feols(log_dl ~ nabij:post_pas | gemeente^nabij + kwartaal, data = zo[verleend == 1], cluster = ~gemeente), "oud doorlooptijd")
  if (!is.null(m)) uo[["log_doorlooptijd_verleend"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post_pas | gemeente^nabij + kwartaal, verleend, vast panel")
  if ("soort" %in% names(zo)) {
    zn <- zo[soort == "nieuwbouw_woning"]
    pn <- zn[, .(aanvragen = .N), by = .(gemeente, kwartaal, nabij)]
    pn <- merge(CJ(gemeente = vast, kwartaal = sort(unique(po$kwartaal)), nabij = 0:1), pn, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
    pn[is.na(aanvragen), aanvragen := 0]
    pn[, post_pas := as.integer(kw_orde(kwartaal) >= kw_orde("2019K3"))]
    m <- veilig(fepois(aanvragen ~ nabij:post_pas | gemeente^nabij + kwartaal, data = pn, cluster = ~gemeente), "oud did nieuwbouw")
    if (!is.null(m)) uo[["aanvragen_nieuwbouw_woning"]] <- coefs(m, "fepois aanvragen ~ nabij x post_pas | gemeente^nabij + kwartaal, vast panel, nieuwbouw_woning")
    m <- veilig(feols(log_dl ~ nabij:post_pas | gemeente^nabij + kwartaal, data = zn[verleend == 1], cluster = ~gemeente), "oud doorlooptijd nieuwbouw")
    if (!is.null(m)) uo[["log_doorlooptijd_nieuwbouw_woning"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post_pas | gemeente^nabij + kwartaal, verleend, vast panel, nieuwbouw_woning")
  }
  res$oud_did_uitkomsten <- rbindlist(uo)
  res$oud_beschrijvend_jaar_nabij <- zo[, .(aanvragen = .N, verleend = mean(verleend), geweigerd = mean(geweigerd), ingetrokken = mean(ingetrokken),
                                            besluit_onbekend = mean(besluit_onbekend), geen = mean(geen),
                                            mediaan_dagen = as.numeric(median(doorlooptijd[verleend == 1], na.rm = TRUE))),
                                        by = .(jaar = substr(kwartaal, 1, 4), nabij)][order(jaar, nabij)]
  res$oud_dekking_jaar <- zaken[, .(gemeenten = uniqueN(gemeente), aanvragen = .N), by = .(jaar = substr(kwartaal, 1, 4))][order(jaar)]
}

for (n in names(res)) if (is.data.frame(res[[n]])) fwrite(res[[n]], file.path(map, paste0(n, ".csv")), sep = ";")
write_json(res, file.path(map, "resultaten.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE, na = "null")
cat("klaar:", paste(names(res), collapse = ", "), "\n")
