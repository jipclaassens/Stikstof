# Grote woningbouwprojecten op projectniveau.
#
# Gebruik: Rscript analyse_projecten.R <analysemap>        (leest projecten.csv uit projecten.py)
# Schrijft projecten_resultaten.json en een csv per tabel (voorvoegsel pr_).
#
# Een groot project is een zaak met een aanvraagpublicatie en minstens twintig woningen volgens de titel of de gekoppelde
# BAG-panden (als controle ook de drempel van twaalf, die van de voorrangsregel bij de Raad van State). Vier vragen:
# 1. Hoe vaak heeft een groot project een eigen stikstofprocedure bij de provincie (een natuurbesluit binnen 500 meter en
#    drie jaar, waarvan titel of tekst stikstof, AERIUS, Natura 2000, passende beoordeling of salderen noemt), nabij en ver,
#    per jaar?
# 2. Verschuiven de uitkomsten van grote projecten nabij habitat na Porthos, de Omgevingswet en intern salderen (gemeente maal
#    nabij en kwartaal als vaste effecten; het aantal projecten per gemeente is te klein voor gemeente maal kwartaal)?
# 3. Hoeveel langer duurt een groot project met een stikstofprocedure (verband binnen gemeente en kwartaal, geen causaal
#    effect: projecten met een procedure zijn anders)?
# 4. De PAS-uitspraak: aanvragen voor grote projecten in de gemeenten van het vaste panel (minstens veertig aanvragen per
#    jaar 2016 tot 2020), per jaar, nabij tegenover ver.

suppressMessages({ library(data.table); library(fixest); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
map <- if (length(args)) args[1] else "C:/LocalData/vergunningen/analyse"
p <- fread(file.path(map, "projecten.csv"), sep = ";", encoding = "UTF-8")
# alleen zaken die als project tellen (geen sloop, BAG-aantal alleen bij nieuwbouw of titel zonder soort) en geen tweede publicatie
res_telling <- list(zaken_10 = nrow(p), telt = sum(p$telt == 1), dubbel = sum(p$telt == 1 & !is.na(p$dubbel_van)))
p <- p[telt == 1 & is.na(dubbel_van)]
zaken <- fread(file.path(map, "zaken.csv"), sep = ";", encoding = "UTF-8", select = c("gemeente", "kwartaal"))
kw_orde <- function(k) { j <- as.integer(substr(k, 1, 4)); q <- as.integer(substr(k, 6, 6)); j * 4 + q - 1 }
coefs <- function(m, naam) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "schatting", "se", "t", "p"))
  ct[, `:=`(model = naam, n = nobs(m))]
  ct[]
}
veilig <- function(expr, naam) tryCatch(expr, error = function(e) { message(naam, ": ", conditionMessage(e)); NULL })
res <- list(meta = c(list(datum = format(Sys.time(), "%Y-%m-%d %H:%M")), res_telling))

p[, kw_idx := kw_orde(kwartaal)]
p[, jaar := as.integer(substr(kwartaal, 1, 4))]
p[, `:=`(post_porthos = as.integer(kw_idx >= kw_orde("2023K1")), post_ow = as.integer(kw_idx >= kw_orde("2024K1")), post_is = as.integer(kw_idx >= kw_orde("2025K1")))]
p[, stikstofprocedure := as.integer(n_natuur_gebied_500m > 0)]
p[, soortenprocedure := as.integer(n_natuur_soorten_250m > 0)]
p[, beroep := as.integer(n_uitspraken > 0)]
p[, dagen_bag := suppressWarnings(as.numeric(dagen_aanvraag_bagvergunning))]
p[, log_dagen_bag := fifelse(!is.na(dagen_bag) & dagen_bag > 0 & dagen_bag <= 1500, log(pmax(dagen_bag, 1)), NA_real_)]
p[, log_dl := fifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(pmax(doorlooptijd, 1)), NA_real_)]
# fread leest de datumkolommen als IDate; een vergelijking met "" geeft dan NA, dus alleen op NA toetsen
p[, bag_vergund := as.integer(!is.na(bag_vergunning) & nzchar(as.character(bag_vergunning)))]
p[, grootte := fifelse(woningen >= 100, "100 of meer", fifelse(woningen >= 50, "50 tot 99", fifelse(woningen >= 20, "20 tot 49", "10 tot 19")))]
einde <- max(as.IDate(p$datum_aanvraag), na.rm = TRUE)
g <- p[woningen >= 20 & kw_idx >= kw_orde("2021K3")]
g9 <- g[as.IDate(datum_aanvraag) <= einde - 365]
res$meta$projecten_20 <- nrow(g)
res$meta$projecten_20_met_vervolg <- nrow(g9)
res$meta$woningen_20 <- sum(g$woningen)

# 1. beschrijvend
res$pr_per_jaar <- g[, .(projecten = .N, woningen = sum(woningen), met_stikstofprocedure = sum(stikstofprocedure),
                         aandeel_stikstofprocedure = mean(stikstofprocedure), met_soortenprocedure = sum(soortenprocedure),
                         met_beroep = sum(beroep), bag_vergund = mean(bag_vergund)), by = .(jaar, nabij)][order(jaar, nabij)]
res$pr_per_grootte <- g[, .(projecten = .N, woningen = sum(woningen), aandeel_stikstofprocedure = mean(stikstofprocedure),
                            aandeel_soortenprocedure = mean(soortenprocedure), aandeel_beroep = mean(beroep)), by = .(grootte, nabij)][order(grootte, nabij)]
res$pr_uitkomsten <- g9[, .(projecten = .N, verleend = mean(verleend), geweigerd = mean(geweigerd), ingetrokken = mean(ingetrokken), geen = mean(geen),
                            bag_vergund = mean(bag_vergund), mediaan_dagen_publicatie = as.numeric(median(doorlooptijd[verleend == 1], na.rm = TRUE)),
                            mediaan_dagen_bag = as.numeric(median(dagen_bag[dagen_bag > 0 & dagen_bag <= 1500], na.rm = TRUE))),
                        by = .(nabij, stikstofprocedure)][order(nabij, stikstofprocedure)]

# 2. uitkomsten van grote projecten nabij na de wisselingen
uk <- list()
termen <- "nabij:post_porthos + nabij:post_ow + nabij:post_is"
for (y in c("verleend", "geweigerd", "ingetrokken", "geen", "bag_vergund", "stikstofprocedure")) {
  m <- veilig(feols(as.formula(paste(y, "~", termen, "| gemeente^nabij + kwartaal")), data = g9, cluster = ~gemeente), y)
  if (!is.null(m)) uk[[y]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal, projecten van 20 of meer woningen"))
}
m <- veilig(feols(as.formula(paste("log_dagen_bag ~", termen, "| gemeente^nabij + kwartaal")), data = g9, cluster = ~gemeente), "dagen bag")
if (!is.null(m)) uk[["log_dagen_bag"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ nabij x post | gemeente^nabij + kwartaal, projecten van 20 of meer woningen")
m <- veilig(feols(as.formula(paste("log_dl ~", termen, "| gemeente^nabij + kwartaal")), data = g9[verleend == 1], cluster = ~gemeente), "dl")
if (!is.null(m)) uk[["log_dl"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + kwartaal, verleend, projecten van 20 of meer woningen")
g12 <- p[woningen >= 12 & kw_idx >= kw_orde("2021K3") & as.IDate(datum_aanvraag) <= einde - 365]
for (y in c("verleend", "bag_vergund")) {
  m <- veilig(feols(as.formula(paste(y, "~", termen, "| gemeente^nabij + kwartaal")), data = g12, cluster = ~gemeente), paste(y, 12))
  if (!is.null(m)) uk[[paste(y, "12")]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal, projecten van 12 of meer woningen"))
}
res$pr_did <- rbindlist(uk)

# 3. hoeveel langer met een stikstofprocedure (verband)
vb <- list()
m <- veilig(feols(log_dagen_bag ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal, data = g9, cluster = ~gemeente), "verband bag")
if (!is.null(m)) vb[["bag"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal")
m <- veilig(feols(log_dl ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal, data = g9[verleend == 1], cluster = ~gemeente), "verband dl")
if (!is.null(m)) vb[["dl"]] <- coefs(m, "feols log(doorlooptijd) ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal, verleend")
m <- veilig(feols(bag_vergund ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal, data = g9, cluster = ~gemeente), "verband vergund")
if (!is.null(m)) vb[["vergund"]] <- coefs(m, "feols BAG-vergunning gevonden ~ stikstofprocedure + soortenprocedure + log(woningen) | gemeente + kwartaal")
m <- veilig(feols(log_dagen_bag ~ stikstofprocedure + stikstofprocedure:nabij + soortenprocedure + log(woningen) | gemeente^nabij + kwartaal, data = g9, cluster = ~gemeente), "verband bag nabij")
if (!is.null(m)) vb[["bag nabij"]] <- coefs(m, "feols log(dagen aanvraag tot BAG-vergunning) ~ stikstofprocedure x nabij + soortenprocedure + log(woningen) | gemeente^nabij + kwartaal")
res$pr_verband <- rbindlist(vb)
res$pr_beroep <- g[, .(projecten = .N, met_uitspraak = sum(beroep), stikstof_grond = sum(stikstof_grond), stikstof_gegrond = sum(stikstof_gegrond)), by = nabij][order(nabij)]
# wanneer valt het eerste stikstofbesluit ten opzichte van de aanvraag
g[, dagen_natuur := as.numeric(as.IDate(eerste_natuur_gebied) - as.IDate(datum_aanvraag))]
res$pr_timing <- g[stikstofprocedure == 1, .(projecten = .N, aandeel_voor_aanvraag = mean(dagen_natuur < 0), mediaan_dagen = as.numeric(median(dagen_natuur)),
                                             p25 = as.numeric(quantile(dagen_natuur, 0.25)), p75 = as.numeric(quantile(dagen_natuur, 0.75)),
                                             met_woningbouwwoord_1km = sum(n_natuur_woningbouw_1km > 0)), by = nabij][order(nabij)]

# 4. de PAS-uitspraak in de grote projecten van het vaste panel, per jaar
per_jaar <- zaken[, .(n = .N), by = .(gemeente, jaar = substr(kwartaal, 1, 4))]
vast <- per_jaar[jaar %in% as.character(2016:2020) & n >= 40, .N, by = gemeente][N == 5, gemeente]
# alleen het woningaantal uit de titel, zodat de definitie over de jaren gelijk blijft (de BAG-koppeling begint in juni 2021);
# 2019 is een overgangsjaar (uitspraak 29 mei) en krijgt een eigen term, zodat het niet in de voorperiode meetelt
pv <- p[woningen_titel >= 20 & soort != "sloop_kap" & gemeente %in% vast & jaar >= 2016 & jaar <= 2025,
        .(n = .N, woningen = sum(woningen_titel)), by = .(gemeente, jaar, nabij)]
pv <- merge(CJ(gemeente = vast, jaar = 2016:2025, nabij = 0:1), pv, by = c("gemeente", "jaar", "nabij"), all.x = TRUE)
pv[is.na(n), `:=`(n = 0, woningen = 0)]
pv[, `:=`(overgang_2019 = as.integer(jaar == 2019), post_pas = as.integer(jaar >= 2020), post_porthos = as.integer(jaar >= 2023), jaar_f = factor(jaar))]
res$meta$vastpanel_gemeenten <- length(vast)
res$pr_vastpanel_beschrijvend <- pv[, .(projecten = sum(n), woningen = sum(woningen)), by = .(jaar, nabij)][order(jaar, nabij)]
vp <- list()
m <- veilig(fepois(n ~ i(jaar_f, nabij, ref = "2018") | gemeente^nabij + jaar_f, data = pv, cluster = ~gemeente), "vp es")
if (!is.null(m)) res$pr_vastpanel_event_study <- coefs(m, "fepois grote projecten ~ nabij x jaar | gemeente^nabij + jaar, vast panel")
vt <- "nabij:overgang_2019 + nabij:post_pas + nabij:post_porthos"
m <- veilig(fepois(as.formula(paste("n ~", vt, "| gemeente^nabij + jaar_f")), data = pv, cluster = ~gemeente), "vp did")
if (!is.null(m)) vp[["n"]] <- coefs(m, "fepois grote projecten ~ nabij x post | gemeente^nabij + jaar, vast panel 2016 tot 2025")
m <- veilig(fepois(as.formula(paste("n ~", vt, "| gemeente^nabij + gemeente^jaar_f")), data = pv, cluster = ~gemeente), "vp did gj")
if (!is.null(m)) vp[["n gj"]] <- coefs(m, "fepois grote projecten ~ nabij x post | gemeente^nabij + gemeente^jaar, vast panel 2016 tot 2025")
m <- veilig(fepois(as.formula(paste("woningen ~", vt, "| gemeente^nabij + jaar_f")), data = pv, cluster = ~gemeente), "vp did w")
if (!is.null(m)) vp[["woningen"]] <- coefs(m, "fepois woningen in grote projecten ~ nabij x post | gemeente^nabij + jaar, vast panel 2016 tot 2025")
res$pr_vastpanel_did <- rbindlist(vp)

# casustabel: grote projecten nabij habitat met een stikstofprocedure, de grootste eerst (alleen gemeente, jaar en omvang)
res$pr_casus <- g[nabij == 1 & stikstofprocedure == 1][order(-woningen)][1:40, .(gemeente, jaar, woningen, uitkomst,
                  dagen_publicatie = doorlooptijd, dagen_tot_bagvergunning = dagen_bag, bag_bouwstart, n_natuur_gebied_500m,
                  eerste_natuur_gebied, beroep = n_uitspraken, stikstof_grond)]

for (n in names(res)) if (is.data.frame(res[[n]])) fwrite(res[[n]], file.path(map, paste0(n, ".csv")), sep = ";")
write_json(res, file.path(map, "projecten_resultaten.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE, na = "null")
cat("klaar:", paste(names(res), collapse = ", "), "\n")
str(res$meta)
print(res$pr_per_jaar)
print(res$pr_uitkomsten)
print(res$pr_timing)
print(res$pr_vastpanel_beschrijvend)
for (n in c("pr_did", "pr_verband", "pr_vastpanel_did")) if (nrow(res[[n]])) {
  cat("==", n, "\n")
  for (i in seq_len(nrow(res[[n]]))) with(res[[n]][i], cat(sprintf("  %-100s %-22s %8.2f [%8.2f, %8.2f] p=%.3f n=%d\n", substr(model, 1, 100), term,
                                                                   100 * schatting, 100 * (schatting - 1.96 * se), 100 * (schatting + 1.96 * se), p, as.integer(n))))
}
e <- res$pr_vastpanel_event_study
if (!is.null(e)) print(e[, .(term, schatting = round(100 * schatting, 1), p = round(p, 3))])
