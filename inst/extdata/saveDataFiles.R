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

# Country-level GDP by SSP scenario
gdp_ctry.SSP1 = get_gdp_ctry(ssp = 'SSP1')
usethis::use_data(gdp_ctry.SSP1, overwrite = T)
gdp_ctry.SSP2 = get_gdp_ctry(ssp = 'SSP2')
usethis::use_data(gdp_ctry.SSP2, overwrite = T)
gdp_ctry.SSP3 = get_gdp_ctry(ssp = 'SSP3')
usethis::use_data(gdp_ctry.SSP3, overwrite = T)
gdp_ctry.SSP4 = get_gdp_ctry(ssp = 'SSP4')
usethis::use_data(gdp_ctry.SSP4, overwrite = T)
gdp_ctry.SSP5 = get_gdp_ctry(ssp = 'SSP5')
usethis::use_data(gdp_ctry.SSP5, overwrite = T)

# Country-level population by SSP scenario
pop_ctry.SSP1 = get_pop_ctry(ssp = 'SSP1')
usethis::use_data(pop_ctry.SSP1, overwrite = T)
pop_ctry.SSP2 = get_pop_ctry(ssp = 'SSP2')
usethis::use_data(pop_ctry.SSP2, overwrite = T)
pop_ctry.SSP3 = get_pop_ctry(ssp = 'SSP3')
usethis::use_data(pop_ctry.SSP3, overwrite = T)
pop_ctry.SSP4 = get_pop_ctry(ssp = 'SSP4')
usethis::use_data(pop_ctry.SSP4, overwrite = T)
pop_ctry.SSP5 = get_pop_ctry(ssp = 'SSP5')
usethis::use_data(pop_ctry.SSP5, overwrite = T)

# Adjust country names due to differences in datasets:
adj_ctry = read.csv("./inst/extdata/adj_ctry.csv")
usethis::use_data(adj_ctry, overwrite = T)





