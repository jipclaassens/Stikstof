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
             "06_rapport.R")

for (s in scripts) {
  message("\n>>> ", s, " ...")
  t0 <- Sys.time()
  ## in een aparte omgeving, zodat scripts niet op elkaars objecten leunen
  sys.source(file.path(.here, s), envir = new.env(parent = globalenv()))
  message(sprintf("<<< %s klaar in %.0f sec", s,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

message("\nAlles klaar. Output staat in R/_output/.")
