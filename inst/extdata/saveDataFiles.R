# Converting raw data into package data
library(usethis)
library(magrittr)
library(dplyr)
library(countrycode)

#=========================================================
#=========================================================
# POllutants to be included in the model
panel_pollutants <- c("BC", "OC", "PrimPM25", "NOx", "VOC", "NMVOC")
usethis::use_data(panel_pollutants, overwrite = T)

# Panel data
panel_data <- create_panel()
usethis::use_data(panel_data, overwrite = T)








