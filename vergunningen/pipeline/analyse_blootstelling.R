# Tweede ronde modellen: blootstelling in plaats van alleen afstand, en de andere remmen naast stikstof.
#
# Gebruik: Rscript analyse_blootstelling.R <analysemap>
# Leest bag_panden.csv en pand_kenmerken.csv (BAG-spoor), zaken.csv en zaak_kenmerken.csv (bekendmakingen) en zaak_bag.csv
# (koppeling zaak aan BAG-pand); schrijft blootstelling_resultaten.json en een csv per tabel (voorvoegsel bl_).
#
# Vier vragen, telkens in dezelfde modelfamilie als analyse.R en analyse_bag.R (Poisson op aantallen per gemeente, kwartaal
# en groep met gemeente maal groep en kwartaal als vaste effecten; lineair op uitkomsten per zaak of pand; standaardfouten
# geclusterd op gemeente):
# 1. Dosis: hangt de uitval na de PAS-uitspraak af van de stikstofdepositie ter plaatse in 2018 (RIVM GDN, 1 km)? Zowel
#    over heel Nederland in kwartielen als binnen de groep nabij habitat, laag tegenover hoog.
# 2. Concurrentie om stikstofruimte: is de uitval groter waar veel vee staat of waar PAS-melders liggen?
# 3. Binnen of buiten de verstedelijkingscontour van de provincie, en netcongestie op het afnamenet als tweede rem (alleen
#    als de rasters in bron\afgeleid er zijn en verrijk.py ze heeft afgelezen).
# 4. Echte woningbouwprojecten: de tijd van de aanvraagpublicatie tot de BAG-vergunning voor zaken met een nieuw woonpand,
#    nabij tegenover ver, voor en na de regimewisselingen, ook gewogen naar het aantal woningen.

suppressMessages({ library(data.table); library(fixest); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
map <- if (length(args)) args[1] else "C:/LocalData/vergunningen/analyse"
res <- list(meta = list(datum = format(Sys.time(), "%Y-%m-%d %H:%M")))

kw_orde <- function(k) { j <- as.integer(substr(k, 1, 4)); q <- as.integer(substr(k, 6, 6)); j * 4 + q - 1 }
coefs <- function(m, naam) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "schatting", "se", "t", "p"))
  ct[, `:=`(model = naam, n = nobs(m))]
  ct[]
}
veilig <- function(expr, naam) tryCatch(expr, error = function(e) { message(naam, ": ", conditionMessage(e)); NULL })
ev <- list(pas = kw_orde("2019K3"), porthos = kw_orde("2023K1"), omgevingswet = kw_orde("2024K1"), intern_salderen = kw_orde("2025K1"))
zet_post <- function(d) d[, `:=`(post_pas = as.integer(kw_idx >= ev$pas), post_porthos = as.integer(kw_idx >= ev$porthos),
                                 post_ow = as.integer(kw_idx >= ev$omgevingswet), post_is = as.integer(kw_idx >= ev$intern_salderen))]
# panel van aantallen per gemeente, kwartaal en groep, met nullen aangevuld
maak_panel <- function(d, groep_kol, waarde = NULL) {
  a <- if (is.null(waarde)) d[, .(n = .N), by = c("gemeente", "kwartaal", groep_kol)] else d[, .(n = sum(get(waarde))), by = c("gemeente", "kwartaal", groep_kol)]
  setnames(a, groep_kol, "groep")
  vol <- CJ(gemeente = unique(a$gemeente), kwartaal = sort(unique(d$kwartaal)), groep = unique(a$groep))
  a <- merge(vol, a, by = c("gemeente", "kwartaal", "groep"), all.x = TRUE)
  a[is.na(n), n := 0]
  a[, kw_idx := kw_orde(kwartaal)]
  zet_post(a)
  a[]
}
did_groep <- function(pan, ref, termen, naam) {
  f <- as.formula(paste("n ~", paste(sprintf("i(groep, %s, ref = '%s')", termen, ref), collapse = " + "), "| gemeente^groep + kwartaal"))
  m <- veilig(fepois(f, data = pan, cluster = ~gemeente), naam)
  if (is.null(m)) NULL else coefs(m, naam)
}
drie <- function(v, naam) {
  q <- quantile(v, c(1/3, 2/3), na.rm = TRUE)
  factor(fifelse(is.na(v), NA_character_, fifelse(v <= q[1], "laag", fifelse(v <= q[2], "midden", "hoog"))), levels = c("laag", "midden", "hoog"))
}

# ---------- BAG-spoor ----------
stand <- as.IDate("2026-09-08")
p <- fread(file.path(map, "bag_panden.csv"), encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "gemeentecode", "bouwjaar", "huidige_status", "n2000_klasse")))
setnames(p, c("x", "y"), c("x_rd", "y_rd"))
k <- fread(file.path(map, "pand_kenmerken.csv"), sep = ";", encoding = "UTF-8", colClasses = list(character = "sleutel"))
p <- merge(p, k, by.x = "identificatie", by.y = "sleutel", all.x = TRUE)
p[, verg := as.IDate(verg_begin)]
p <- p[!is.na(verg) & verg >= as.IDate("2012-01-01") & verg <= stand & gemeentecode != "" & !is.na(dist2n2000_m)]
p[, `:=`(gemeente = gemeentecode, kwartaal = sprintf("%dK%d", year(verg), quarter(verg)))]
p <- p[kwartaal <= "2026K2"]
p[, kw_idx := kw_orde(kwartaal)]
zet_post(p)
p[, nabij := as.integer(dist2n2000_m <= 2500)]
w <- p[n_woon >= 1]
res$meta$woonpanden <- nrow(w)

# 1. dosis: kwartielen van de depositie 2018 over de woonpanden van voor de uitspraak
qd <- quantile(w[kw_idx < ev$pas, ndep_2018], c(.25, .5, .75), na.rm = TRUE)
res$meta$depositie_kwartielgrenzen_2018 <- as.list(round(qd))
w[, dep_kw := factor(fifelse(is.na(ndep_2018), NA_character_, fifelse(ndep_2018 <= qd[1], "Q1", fifelse(ndep_2018 <= qd[2], "Q2", fifelse(ndep_2018 <= qd[3], "Q3", "Q4")))), levels = c("Q1", "Q2", "Q3", "Q4"))]
res$bl_dosis_beschrijvend <- w[!is.na(dep_kw), .(woonpanden = .N, nabij = mean(nabij), ndep_2018_mediaan = as.numeric(median(ndep_2018))), by = dep_kw][order(dep_kw)]
pan <- maak_panel(w[!is.na(dep_kw)], "dep_kw")
out <- list()
out[["dosis kwartiel"]] <- did_groep(pan, "Q1", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woonpanden ~ depositiekwartiel 2018 x post | gemeente^kwartiel + kwartaal")
# voorperiode: event study per kwartiel tegenover Q1, referentie 2019K1; en dezelfde DiD met een eigen lineaire trend per
# kwartiel die alleen op de jaren voor de uitspraak is geschat (trendbreuk in plaats van niveauverschil)
es_groep <- function(pan, ref_groep, ref_kw, naam) {
  pan <- copy(pan)
  pan[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
  uit <- list()
  for (g in setdiff(unique(pan$groep), ref_groep)) {
    d <- pan[groep %in% c(ref_groep, g)]
    d[, behandeld := as.integer(groep == g)]
    m <- veilig(fepois(n ~ i(kwartaal_f, behandeld, ref = ref_kw) | gemeente^groep + kwartaal_f, data = d, cluster = ~gemeente), paste(naam, g))
    if (!is.null(m)) uit[[g]] <- coefs(m, paste0(naam, ", ", g, " tegenover ", ref_groep))
  }
  rbindlist(uit)
}
res$bl_dosis_event_study <- es_groep(pan, "Q1", "2019K1", "fepois woonpanden ~ kwartaal x depositiekwartiel | gemeente^kwartiel + kwartaal")
pan[, t := (kw_idx - ev$pas) / 4]
pan[, `:=`(t_voor = t * as.integer(kw_idx < ev$pas), t_na = t * post_pas)]
m <- veilig(fepois(n ~ i(groep, t_voor, ref = "Q1") + i(groep, post_pas, ref = "Q1") + i(groep, t_na, ref = "Q1") | gemeente^groep + kwartaal, data = pan, cluster = ~gemeente), "dosis trendbreuk")
if (!is.null(m)) out[["dosis trendbreuk"]] <- coefs(m, "fepois woonpanden ~ kwartiel x (trend voor, sprong na, trend na) | gemeente^kwartiel + kwartaal")
# binnen nabij: laag tegenover hoog (mediaan van nabij), ver als referentie
mn <- median(w[nabij == 1 & kw_idx < ev$pas, ndep_2018], na.rm = TRUE)
w[, groep_dn := fifelse(nabij == 0, "ver", fifelse(is.na(ndep_2018), NA_character_, fifelse(ndep_2018 <= mn, "nabij_laag", "nabij_hoog")))]
res$meta$depositie_mediaan_nabij_2018 <- round(mn)
pan <- maak_panel(w[!is.na(groep_dn)], "groep_dn")
out[["dosis nabij"]] <- did_groep(pan, "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woonpanden ~ nabij laag of hoog depositie 2018 x post | gemeente^groep + kwartaal")
res$bl_dosis_nabij_event_study <- es_groep(pan, "ver", "2019K1", "fepois woonpanden ~ kwartaal x groep | gemeente^groep + kwartaal")
trendbreuk <- function(pan, ref, naam) {
  pan <- copy(pan)
  pan[, t := (kw_idx - ev$pas) / 4]
  pan[, `:=`(t_voor = t * as.integer(kw_idx < ev$pas), t_na = t * post_pas)]
  f <- as.formula(sprintf("n ~ i(groep, t_voor, ref = '%s') + i(groep, post_pas, ref = '%s') + i(groep, t_na, ref = '%s') | gemeente^groep + kwartaal", ref, ref, ref))
  m <- veilig(fepois(f, data = pan, cluster = ~gemeente), naam)
  if (is.null(m)) NULL else coefs(m, naam)
}
out[["dosis nabij trendbreuk"]] <- trendbreuk(pan, "ver", "fepois woonpanden ~ nabij laag of hoog x (trend voor, sprong na, trend na) | gemeente^groep + kwartaal")
# de hoofdspecificatie van analyse_bag.R (nabij tegenover ver) met dezelfde trendbreuk, en met een voorperiode vanaf 2015
w[, groep_n := fifelse(nabij == 1, "nabij", "ver")]
pan_n <- maak_panel(w, "groep_n")
out[["nabij trendbreuk"]] <- trendbreuk(pan_n, "ver", "fepois woonpanden ~ nabij x (trend voor, sprong na, trend na) | gemeente^groep + kwartaal")
out[["nabij vanaf 2015"]] <- did_groep(pan_n[kw_idx >= kw_orde("2015K1")], "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woonpanden ~ nabij x post | gemeente^groep + kwartaal, vanaf 2015K1")
# eigen lineaire trend per gemeente en groep (varying slopes): de sprong bij de uitspraak als afwijking van de trend van
# dezelfde gemeente, zodat een verschuiving binnen gemeenten van rand naar centrum in de trend opgaat
pan_n[, t := (kw_idx - ev$pas) / 4]
m <- veilig(fepois(n ~ i(groep, post_pas, ref = "ver") | gemeente^groep[t] + kwartaal, data = pan_n[kw_idx <= kw_orde("2022K2")], cluster = ~gemeente), "nabij gemeentetrend")
if (!is.null(m)) out[["nabij gemeentetrend"]] <- coefs(m, "fepois woonpanden ~ nabij x post | gemeente^groep met eigen trend + kwartaal, 2012K1 tot 2022K2")
pan_d <- maak_panel(w[!is.na(dep_kw)], "dep_kw")
pan_d[, t := (kw_idx - ev$pas) / 4]
m <- veilig(fepois(n ~ i(groep, post_pas, ref = "Q1") | gemeente^groep[t] + kwartaal, data = pan_d[kw_idx <= kw_orde("2022K2")], cluster = ~gemeente), "dosis gemeentetrend")
if (!is.null(m)) out[["dosis gemeentetrend"]] <- coefs(m, "fepois woonpanden ~ depositiekwartiel x post | gemeente^kwartiel met eigen trend + kwartaal, 2012K1 tot 2022K2")
pan_w <- maak_panel(w[!is.na(groep_dn)], "groep_dn", "n_woon")
out[["dosis nabij woningen"]] <- did_groep(pan_w, "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woningen ~ nabij laag of hoog depositie 2018 x post | gemeente^groep + kwartaal")

# 2. concurrentie: veel of weinig vee binnen 1 km, en PAS-melders binnen het blok van 5 km
if ("nh3_1km_kg" %in% names(w)) {
  mv <- median(w[nabij == 1 & kw_idx < ev$pas & !is.na(nh3_1km_kg), nh3_1km_kg])
  w[, groep_vee := fifelse(nabij == 0, "ver", fifelse(is.na(nh3_1km_kg), NA_character_, fifelse(nh3_1km_kg <= mv, "nabij_weinig_vee", "nabij_veel_vee")))]
  res$meta$nh3_1km_mediaan_nabij <- round(mv)
  pan <- maak_panel(w[!is.na(groep_vee)], "groep_vee")
  out[["vee nabij"]] <- did_groep(pan, "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woonpanden ~ nabij met weinig of veel vee binnen 1 km x post | gemeente^groep + kwartaal")
}
if ("pas_melders_5km" %in% names(w)) {
  w[, groep_pas := fifelse(nabij == 0, "ver", fifelse(is.na(pas_melders_5km), NA_character_, fifelse(pas_melders_5km == 0, "nabij_geen_melders", "nabij_met_melders")))]
  pan <- maak_panel(w[!is.na(groep_pas)], "groep_pas")
  out[["pas-melders nabij"]] <- did_groep(pan, "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), "fepois woonpanden ~ nabij met of zonder PAS-melders in 5 km-blok x post | gemeente^groep + kwartaal")
}

# 3a. binnen of buiten de verstedelijkingscontour
if ("stedelijk_gebied" %in% names(w) && any(!is.na(w$stedelijk_gebied))) {
  w[, groep_sg := fifelse(is.na(stedelijk_gebied), NA_character_, paste0(fifelse(nabij == 1, "nabij_", "ver_"), fifelse(stedelijk_gebied == 1, "binnen", "buiten")))]
  res$bl_contour_beschrijvend <- w[!is.na(groep_sg), .(woonpanden = .N, woningen = sum(n_woon)), by = .(groep_sg, periode = fifelse(kw_idx < ev$pas, "voor PAS", "na PAS"))][order(groep_sg, periode)]
  for (s in c("binnen", "buiten")) {
    ws <- w[groep_sg %in% paste0(c("ver_", "nabij_"), s)]
    ws[, groep := fifelse(nabij == 1, "nabij", "ver")]
    pan <- maak_panel(ws, "groep")
    out[[paste("contour", s)]] <- did_groep(pan, "ver", c("post_pas", "post_porthos", "post_ow", "post_is"), paste("fepois woonpanden ~ nabij x post | gemeente^groep + kwartaal,", s, "de contour"))
  }
}
res$bl_bag_did <- rbindlist(out, fill = TRUE)

# 3b. netcongestie op het afnamenet, BAG-woonpanden 2022K1 tot 2026K2
nc <- list()
if ("congestie_afname_eerste" %in% names(w) && any(!is.na(w$congestie_afname_eerste))) {
  wn <- w[kw_idx >= kw_orde("2022K1") & !is.na(congestie_afname_eerste) & congestie_afname_eerste != 255]
  wn[, groep := fifelse(congestie_afname_eerste == 1, "rood_2024", fifelse(congestie_afname_eerste == 2, "rood_2025", fifelse(congestie_afname_eerste == 3, "rood_2026", "nooit")))]
  res$bl_congestie_beschrijvend <- wn[, .(woonpanden = .N, woningen = sum(n_woon), nabij = mean(nabij)), by = groep][order(groep)]
  pan <- maak_panel(wn, "groep")
  pan[, `:=`(post_2024 = as.integer(kw_idx >= kw_orde("2024K2")), post_2025 = as.integer(kw_idx >= kw_orde("2025K3")))]
  m <- veilig(fepois(n ~ i(groep, post_2024, ref = "nooit") + i(groep, post_2025, ref = "nooit") | gemeente^groep + kwartaal, data = pan[groep %in% c("nooit", "rood_2024", "rood_2025")], cluster = ~gemeente), "congestie bag")
  if (!is.null(m)) nc[["bag"]] <- coefs(m, "fepois woonpanden ~ rood voor afname (april 2024 of juli 2025) x post | gemeente^groep + kwartaal, 2022K1 tot 2026K2")
  pan[, kwartaal_f := factor(kwartaal)]
  m <- veilig(fepois(n ~ i(kwartaal_f, groep == "rood_2024", ref = "2024K1") | gemeente^groep + kwartaal_f, data = pan[groep %in% c("nooit", "rood_2024")], cluster = ~gemeente), "congestie bag es")
  if (!is.null(m)) res$bl_congestie_event_study_bag <- coefs(m, "fepois woonpanden ~ rood april 2024 x kwartaal | gemeente^groep + kwartaal")
}

# ---------- bekendmakingen ----------
z <- fread(file.path(map, "zaken.csv"), sep = ";", encoding = "UTF-8")
zk <- fread(file.path(map, "zaak_kenmerken.csv"), sep = ";", encoding = "UTF-8")
z <- merge(z, zk, by.x = "zaak_id", by.y = "sleutel", all.x = TRUE)
z[, kw_idx := kw_orde(kwartaal)]
z <- z[kw_idx >= kw_orde("2021K3")]
zet_post(z)
res$meta$zaken <- nrow(z)

# 1b. dosis in de aanvragen: kwartielen van de depositie 2018, na Porthos, OW en intern salderen
qz <- quantile(z[kw_idx < ev$porthos, ndep_2018], c(.25, .5, .75), na.rm = TRUE)
z[, dep_kw := factor(fifelse(is.na(ndep_2018), NA_character_, fifelse(ndep_2018 <= qz[1], "Q1", fifelse(ndep_2018 <= qz[2], "Q2", fifelse(ndep_2018 <= qz[3], "Q3", "Q4")))), levels = c("Q1", "Q2", "Q3", "Q4"))]
oz <- list()
pan <- maak_panel(z[!is.na(dep_kw)], "dep_kw")
oz[["alle aanvragen"]] <- did_groep(pan, "Q1", c("post_porthos", "post_ow", "post_is"), "fepois aanvragen ~ depositiekwartiel 2018 x post | gemeente^kwartiel + kwartaal")
pan <- maak_panel(z[!is.na(dep_kw) & soort == "nieuwbouw_woning"], "dep_kw")
oz[["nieuwbouw woningen"]] <- did_groep(pan, "Q1", c("post_porthos", "post_ow", "post_is"), "fepois aanvragen ~ depositiekwartiel 2018 x post | gemeente^kwartiel + kwartaal, nieuwbouw_woning")
if ("congestie_afname_eerste" %in% names(z) && any(!is.na(z$congestie_afname_eerste))) {
  zn <- z[!is.na(congestie_afname_eerste) & congestie_afname_eerste != 255]
  zn[, groep := fifelse(congestie_afname_eerste == 1, "rood_2024", fifelse(congestie_afname_eerste == 2, "rood_2025", fifelse(congestie_afname_eerste == 3, "rood_2026", "nooit")))]
  for (s in list(c("alle aanvragen", NA), c("bouwaanvragen", "bouw"), c("nieuwbouw woningen", "nieuwbouw_woning"))) {
    d <- if (is.na(s[2])) zn else if (s[2] == "bouw") zn[bouw == 1] else zn[soort == s[2]]
    pan <- maak_panel(d, "groep")
    pan[, `:=`(post_2024 = as.integer(kw_idx >= kw_orde("2024K2")), post_2025 = as.integer(kw_idx >= kw_orde("2025K3")))]
    m <- veilig(fepois(n ~ i(groep, post_2024, ref = "nooit") + i(groep, post_2025, ref = "nooit") | gemeente^groep + kwartaal, data = pan[groep %in% c("nooit", "rood_2024", "rood_2025")], cluster = ~gemeente), paste("congestie", s[1]))
    if (!is.null(m)) nc[[s[1]]] <- coefs(m, paste("fepois aanvragen ~ rood voor afname (april 2024 of juli 2025) x post | gemeente^groep + kwartaal,", s[1]))
  }
}
if ("stedelijk_gebied" %in% names(z) && any(!is.na(z$stedelijk_gebied))) {
  for (s in c(1, 0)) {
    d <- z[stedelijk_gebied == s & soort == "nieuwbouw_woning"]
    d[, groep := fifelse(nabij == 1, "nabij", "ver")]
    pan <- maak_panel(d, "groep")
    oz[[paste("nieuwbouw contour", s)]] <- did_groep(pan, "ver", c("post_porthos", "post_ow", "post_is"), paste("fepois aanvragen nieuwbouw_woning ~ nabij x post | gemeente^groep + kwartaal,", fifelse(s == 1, "binnen", "buiten"), "de contour"))
  }
}
res$bl_aanvragen_did <- rbindlist(oz, fill = TRUE)
res$bl_congestie_did <- rbindlist(nc, fill = TRUE)

# 4. echte woningbouwprojecten: van aanvraag tot BAG-vergunning
zb <- fread(file.path(map, "zaak_bag.csv"), sep = ";", encoding = "UTF-8")
e <- merge(z, zb, by = "zaak_id")
e <- e[bag_woningen >= 1 & !is.na(datum_aanvraag)]
e[, dagen := as.integer(as.IDate(bag_vergunning) - as.IDate(datum_aanvraag))]
e <- e[!is.na(dagen) & dagen >= 0 & dagen <= 1500]
e[, grootte := fifelse(bag_woningen == 1, "1 woning", fifelse(bag_woningen <= 9, "2 tot 9 woningen", "10 of meer woningen"))]
e[, log_dagen := log(pmax(dagen, 1))]
einde <- max(as.IDate(e$datum_aanvraag))
e9 <- e[as.IDate(datum_aanvraag) <= einde - 365]
res$bl_projecten_beschrijvend <- e9[, .(projecten = .N, woningen = sum(bag_woningen), mediaan_dagen = as.numeric(median(dagen)), p75_dagen = as.numeric(quantile(dagen, .75))),
                                    by = .(jaar = substr(kwartaal, 1, 4), nabij)][order(jaar, nabij)]
res$bl_projecten_grootte <- e9[, .(projecten = .N, woningen = sum(bag_woningen), mediaan_dagen = as.numeric(median(dagen))), by = .(grootte, nabij)][order(grootte, nabij)]
op <- list()
m <- veilig(feols(log_dagen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = e9, cluster = ~gemeente), "projecten")
if (!is.null(m)) op[["alle"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ nabij x post | gemeente^nabij + kwartaal, projecten met woningen")
m <- veilig(feols(log_dagen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = e9, weights = ~bag_woningen, cluster = ~gemeente), "projecten gewogen")
if (!is.null(m)) op[["gewogen"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ nabij x post | gemeente^nabij + kwartaal, gewogen naar woningen")
for (g in c("1 woning", "2 tot 9 woningen", "10 of meer woningen")) {
  m <- veilig(feols(log_dagen ~ nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = e9[grootte == g], cluster = ~gemeente), paste("projecten", g))
  if (!is.null(m)) op[[g]] <- coefs(m, paste("feols log(dagen aanvraag tot BAG-vergunning) ~ nabij x post | gemeente^nabij + kwartaal,", g))
}
if (!is.null(qz)) {
  e9[, dep_hoog := as.integer(!is.na(ndep_2018) & ndep_2018 > qz[3])]
  m <- veilig(feols(log_dagen ~ dep_hoog:post_porthos + dep_hoog:post_ow + dep_hoog:post_is | gemeente^dep_hoog + kwartaal, data = e9[!is.na(ndep_2018)], cluster = ~gemeente), "projecten depositie")
  if (!is.null(m)) op[["depositie hoog"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ depositie 2018 in het hoogste kwartiel x post | gemeente^groep + kwartaal")
}
res$bl_projecten_did <- rbindlist(op, fill = TRUE)
res$meta$projecten <- nrow(e9)

for (n in names(res)) if (is.data.frame(res[[n]])) fwrite(res[[n]], file.path(map, paste0(n, ".csv")), sep = ";")
write_json(res, file.path(map, "blootstelling_resultaten.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE, na = "null")
cat("klaar:", paste(names(res), collapse = ", "), "\n")
str(res$meta)
for (n in c("bl_dosis_event_study", "bl_dosis_nabij_event_study")) if (!is.null(res[[n]]) && nrow(res[[n]])) {
  cat("==", n, "(procent, per jaar gemiddeld over de kwartalen)\n")
  e2 <- res[[n]][grepl("::", term)]
  e2[, `:=`(groep = sub(".*, ", "", model), jaar = substr(sub("kwartaal_f::", "", term), 1, 4))]
  print(dcast(e2[, .(gem = round(100 * mean(schatting), 1)), by = .(groep, jaar)], jaar ~ groep, value.var = "gem"))
}
for (n in c("bl_bag_did", "bl_aanvragen_did", "bl_congestie_did", "bl_projecten_did")) if (!is.null(res[[n]]) && nrow(res[[n]])) {
  cat("==", n, "\n")
  for (i in seq_len(nrow(res[[n]]))) with(res[[n]][i], cat(sprintf("  %-100s %-38s %8.2f [%8.2f, %8.2f] p=%.3f\n", substr(model, 1, 100), term,
                                                                   100 * schatting, 100 * (schatting - 1.96 * se), 100 * (schatting + 1.96 * se), p)))
}
