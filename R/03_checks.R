## =====================================================================
## 03_checks.R -- datakwaliteitscontroles
##
## Input : _derived/vbo_analyse.rds
## Output: _output/checks_<extract>.txt  (en alles naar de console)
##
## Elke check hier bestaat omdat hij een keer iets heeft opgeleverd.
## Bij een nieuw extract eerst dit script draaien, dan pas analyseren.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))

logbestand <- file.path(PATHS$output, sprintf("checks_%s.txt", EXTRACT))
sink(logbestand, split = TRUE)

d <- read_derived("vbo_analyse")

kop("03_checks.R -- extract %s -- %s", EXTRACT, format(Sys.time(), "%Y-%m-%d %H:%M"))

## ---------------------------------------------------------------------
## CHECK 1 -- Project-id: is de nieuwe sleutel beter dan documentnummer_rel?
## ---------------------------------------------------------------------
## Aanleiding: in de Stata-pipeline is projectid_doc = vbo_documentnummer_rel,
## en die is NA voor alles wat niet aan een vergunning kon worden gekoppeld.
## `collapse (min) afstandsklasse, by(projectid_doc ...)` pooit dan alle
## niet-gekoppelde VBO's binnen een gemeente-jaar tot een pseudo-project en
## geeft ze allemaal de kleinste afstandsklasse van die hoop.

kop("CHECK 1 -- projectsleutel")
b <- d[bron_vergunning == TRUE]
cat(sprintf("projectid-groepen die meerdere documentnummer_rel bevatten: %s\n",
            nfmt(sum(b[, uniqueN(documentnummer_rel), by = projectid]$V1 > 1))))
cat(sprintf("documentnummer_rel-waarden die over meerdere gemeenten lopen: %s\n",
            nfmt(sum(b[, uniqueN(gemeentecode), by = documentnummer_rel]$V1 > 1))))
cat("  -> de nieuwe sleutel splitst geen bestaand project, maar herstelt wel\n")
cat("     documentnummers die in meerdere gemeenten zijn hergebruikt.\n\n")
cat(sprintf("VBO's zonder documentnummer (krijgen een eigen project-id): %s\n",
            nfmt(sum(grepl("^__los_", d$projectid)))))
cat("\nProjectomvang binnen de ex-ante steekproef:\n")
print(summary(d[in_sample == TRUE, .N, by = projectid]$N))

## ---------------------------------------------------------------------
## CHECK 2 -- Koppelgraad per jaar
## ---------------------------------------------------------------------
## De koppeling nieuwbouw <-> vergunning is in de eerste jaren slecht,
## omdat de BAG-vergunninghistorie niet ver genoeg terugloopt. Dat is
## geen probleem voor tellingen van nieuwbouw, wel voor alles waarin
## projectkenmerken of vergunningdatums meedoen.

kop("CHECK 2 -- koppelgraad nieuwbouw -> vergunning, per mutatiejaar")
print(d[cbs_nieuwbouw == TRUE & !is.na(mutatiejaar),
        .(nieuwbouw = .N,
          zonder_vergunning = sum(!bron_vergunning),
          pct = round(100 * mean(!bron_vergunning), 1)),
        by = mutatiejaar][order(mutatiejaar)])
cat("\n  -> vanaf 2015 is dit ~1,5-3%. 2012-2013 apart bekijken.\n")

## ---------------------------------------------------------------------
## CHECK 3 -- Censurering per vergunningjaar
## ---------------------------------------------------------------------
## Dit is de belangrijkste bedreiging voor de identificatie: het aandeel
## nog-niet-opgeleverde vergunningen loopt naar het einde van de reeks
## naar 90%, EN verschilt tussen dichtbij en ver van N2000. Elke analyse
## die conditioneert op gerealiseerd zijn, selecteert dus differentieel.

kop("CHECK 3 -- censurering per vergunningjaar x nabijheid (<= 1000 m)")
s <- d[in_sample_verg == TRUE]
s[, nabij := fifelse(kl1000 == 1, "<=1000m", ">1000m")]
print(dcast(s[, .(pct_pipeline = round(100 * mean(uitkomst == "pipeline"), 1)),
              by = .(vergunning_jaar, nabij)],
            vergunning_jaar ~ nabij, value.var = "pct_pipeline"))
cat("\n  -> gebruik `gerealiseerd_binnen_Njr` met cohorten waarvoor het\n")
cat("     venster vol is, niet een kale vergelijking over vergunningjaren.\n")

## ---------------------------------------------------------------------
## CHECK 4 -- Pand/VBO-inconsistentie
## ---------------------------------------------------------------------
## De aanleiding voor deze hele exercitie: panden met status
## "Niet_gerealiseerd_pand" die wel VBO's in de voorraad hebben.

kop("CHECK 4 -- pandstatus vs. VBO-status")
print(dcast(d[last_pand_status == "Niet_gerealiseerd_pand", .N, by = vbo_status],
            vbo_status ~ ., value.var = "N")[order(-.)])
cat("\nIs dit een aggregatie-artefact (pand met deels wel, deels niet)?\n")
mix <- d[last_pand_status == "Niet_gerealiseerd_pand",
         .(k = uniqueN(vbo_in_voorraad)), by = pand_bag_nr]
cat(sprintf("  panden met gemengde VBO-statussen: %s van %s\n",
            nfmt(sum(mix$k > 1)), nfmt(nrow(mix))))
cat("  -> nee. Het is een echte inconsistentie in de BAG, geen artefact.\n")
cat("     Daarom wordt realisatie op vbo_status bepaald (CBS-definitie).\n")

## ---------------------------------------------------------------------
## CHECK 5 -- Is de ruis gecorreleerd met de behandeling?
## ---------------------------------------------------------------------
## Als misclassificatie even vaak voorkomt dichtbij als ver van N2000,
## en niet verandert rond 2019, dan is hij hinderlijk maar onschuldig.

kop("CHECK 5 -- uitkomstverdeling naar afstandsklasse")
s[, band := cut(dist2n2000_m, c(-1, 250, 500, 1000, 2500, 5000, 10000, Inf),
                labels = c("0-250", "250-500", "500-1000", "1-2,5k",
                           "2,5-5k", "5-10k", ">10k"))]
tb <- dcast(s[, .N, by = .(band, uitkomst)], band ~ uitkomst,
            value.var = "N", fill = 0)
m <- as.matrix(tb[, -1]); rownames(m) <- as.character(tb$band)
cat("rijpercentages:\n"); print(round(100 * m / rowSums(m), 2))

cat("\nAandeel uitval per vergunningjaar x nabijheid:\n")
print(dcast(s[, .(pct = round(100 * mean(uitkomst == "uitval"), 2)),
              by = .(vergunning_jaar, nabij)],
            vergunning_jaar ~ nabij, value.var = "pct"))

cat("\nAandeel inconsistente records per vergunningjaar x nabijheid:\n")
print(dcast(s[, .(pct = round(100 * mean(pand_vbo_inconsistent), 2)),
              by = .(vergunning_jaar, nabij)],
            vergunning_jaar ~ nabij, value.var = "pct"))

## ---------------------------------------------------------------------
## CHECK 6 -- Structurele breuken in de reeks
## ---------------------------------------------------------------------

kop("CHECK 6 -- reeksbreuken")
cat("Mutatiemaanden per jaar (12 = volledig jaar):\n")
print(d[cbs_nieuwbouw == TRUE, .(maanden = uniqueN(mutatiemaand),
                                 vbo = .N), by = mutatiejaar][order(mutatiejaar)])
cat("\n  -> een jaar met minder dan 12 maanden hoort niet in de analyse.\n")
cat("\nVergunningen per jaar (Wkb/'de knip' per 1-1-2024, zie docs sec. 3):\n")
print(d[in_sample == TRUE & !is.na(vergunning_jaar) & vergunning_jaar >= 2010,
        .N, by = vergunning_jaar][order(vergunning_jaar)])

## ---------------------------------------------------------------------
## CHECK 7 -- Ontbrekende waarden op de sleutelvariabelen
## ---------------------------------------------------------------------

kop("CHECK 7 -- ontbrekende waarden")
vars <- c("dist2n2000_m", "provinciecode", "gemeentecode", "vergunning_jaar",
          "pand_bouwjaar", "vergunningjaar", "vbo_status", "last_pand_status")
for (v in vars) {
  cat(sprintf("  %-20s ontbreekt bij %8s VBO's (%.2f%%)\n", v,
              nfmt(sum(is.na(d[[v]]))), 100 * mean(is.na(d[[v]]))))
}

## ---------------------------------------------------------------------
## CHECK 9 -- VBO-velden uit Object_Nieuwbouw  (issue #27)
## ---------------------------------------------------------------------
## Sinds #27 leveren beide objectbestanden last_vbo_status en
## last_vbo_gebruiksdoel. Waar ze elkaar overlappen moeten ze gelijk zijn:
## het is dezelfde eindstand van hetzelfde VBO uit dezelfde uq_vbo.

kop("CHECK 9 -- consistentie last_vbo_* tussen beide exports (#27)")
if (!"nwb_vbo_status" %in% names(d) || all(is.na(d$nwb_vbo_status))) {
  cat("  Object_Nieuwbouw levert deze velden nog niet (extract van voor #27).\n")
  cat(sprintf("  gebruiksdoel afgeleid als 'woon' zonder bron: %s VBO's\n",
              nfmt(sum(d$gebruiksdoel_afgeleid))))
} else {
  ## NA's tellen niet als verschil, anders levert != een NA op die de som wist
  wijkt_af <- function(a, b) sum(!is.na(a) & !is.na(b) & a != b)
  ov <- d[bron_vergunning == TRUE & !is.na(nwb_vbo_status)]
  cat(sprintf("VBO's in beide bestanden: %s\n", nfmt(nrow(ov))))
  cat(sprintf("  status wijkt af       : %s\n",
              nfmt(wijkt_af(ov$vbo_status, ov$nwb_vbo_status))))
  cat(sprintf("  gebruiksdoel wijkt af : %s\n",
              nfmt(wijkt_af(ov$vbo_gebruiksdoel, ov$nwb_vbo_gebruiksdoel))))
  cat("  -> beide horen 0 te zijn; anders komen de exports niet uit dezelfde run.\n")
  cat(sprintf("\nnog afgeleid ('woon' zonder bron): %s VBO's (was ~43.000)\n",
              nfmt(sum(d$gebruiksdoel_afgeleid))))
  cat("\ngebruiksdoel van de VBO's zonder vergunningrecord:\n")
  print(d[bron_vergunning == FALSE, .N, by = vbo_gebruiksdoel][order(-N)])
}

kop("Klaar -- log staat in %s", logbestand)

sink()
