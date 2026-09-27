## =====================================================================
## run_all.R -- draait de hele pipeline
##
## Vanaf de commandline:
##   "C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe" C:\ProjDir\Stikstof\R\run_all.R
##
## Of in RStudio: setwd naar deze map en source dit bestand.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})

scripts <- c("01_build_vbo.R",
             "02_classify.R",
             "03_checks.R",
             "04_analyse_aanbod.R",
             "05_eventstudy_kwartaal.R",
             "06_rapport.R",
             "07_plannen.R",
             "08_strikt.R",
             "09_aanvragen.R",   # roept zo nodig 09a_titels.py aan
             "10_landelijk.R",
             "11_ruimte.R",      # roept zo nodig 11a_rasters.py en 11b_contour_vroeg.py aan (Python van QGIS)
             "12_beroep.R",
             "13_binnenring.R",
             "14_netcongestie.R",   # roept zo nodig 14a_pc6.py (Python van QGIS) en 14b_congestiedatum.py aan; na 11_ruimte.R
             "15_natuurbesluiten.R", # roept 15a_natuurbesluiten.py aan
             "16_kernen.R",         # roept zo nodig 16a_kernen.py aan (Python van QGIS); na 11_ruimte.R
             "17_registratie.R",    # roept zo nodig 17a_pand_zaak.py en 17b_besluit_nabij.py aan (leest vergunningen.sqlite)
             "18_nnn.R",            # roept zo nodig 18a_nnn.py aan (Python van QGIS); na 11_ruimte.R en 16_kernen.R
             "19_ringen.R",         # maakt zo nodig gemeentenamen.csv met ogr2ogr (QGIS); na 11_ruimte.R en 16_kernen.R
             "20_plannen_nh.R",     # roept zo nodig 20a_plannen_nh.py aan (Python van QGIS)
             "21a_econlogic.R",     # leest de objectexport van het densification-paper (OVSRV08, ECONLOGIC_PEROBJECT)
             "21_verdichting.R",    # na 21a
             "22_langereeks.R",     # roept 22a_habitat.py aan (Python van QGIS); na 21a en 11_ruimte.R (depositiegrenzen)
             "23_plannen_nl.R",     # roept zo nodig 22a_habitat.py plannen aan (Python van QGIS; VRO_PROJECTEN)
             "25_omvang.R",         # het tweede kanaal: de projectomvang als uitkomst
             "26_samenstelling.R")  # roept 22a_habitat.py aan; na 11_ruimte.R en 21a_econlogic.R
## 09 tot en met 16 lezen de datamap van het vergunningenspoor (VERGUNNINGEN_DATA, zie 00_config.R).
## Het rapport zelf: node R/rapport/bouw_rapport.js 20260730

for (s in scripts) {
  message("\n>>> ", s, " ...")
  t0 <- Sys.time()
  ## in een aparte omgeving, zodat scripts niet op elkaars objecten leunen
  sys.source(file.path(.here, s), envir = new.env(parent = globalenv()))
  message(sprintf("<<< %s klaar in %.0f sec", s,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

message("\nAlles klaar. Output staat in R/_output/.")
