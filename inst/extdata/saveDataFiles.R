# Converting raw data into package data
library(usethis)
library(magrittr)
library(dplyr)
library(countrycode)

#=========================================================
#=========================================================
# Pollutants to be included in the model
panel_pollutants <- c("BC", "OC", "PrimPM25", "NOx", "VOC", "NMVOC")
usethis::use_data(panel_pollutants, overwrite = T)

# Panel data
panel_data <- create_panel()
usethis::use_data(panel_data, overwrite = T)

#Raw ssp data
raw.ssp.data = read.csv("./inst/extdata/socioeconomic/SSP_database_2024.csv", skip = 8)
usethis::use_data(raw.ssp.data, overwrite = T)

# Per capita GDP by SSP scenario
gdp_pc.SSP1 = get_gdppc(ssp = 'SSP1')
usethis::use_data(gdp_pc.SSP1, overwrite = T)
gdp_pc.SSP2 = get_gdppc(ssp = 'SSP2')
usethis::use_data(gdp_pc.SSP2, overwrite = T)
gdp_pc.SSP3 = get_gdppc(ssp = 'SSP3')
usethis::use_data(gdp_pc.SSP3, overwrite = T)
gdp_pc.SSP4 = get_gdppc(ssp = 'SSP4')
usethis::use_data(gdp_pc.SSP4, overwrite = T)
gdp_pc.SSP5 = get_gdppc(ssp = 'SSP5')
usethis::use_data(gdp_pc.SSP5, overwrite = T)








