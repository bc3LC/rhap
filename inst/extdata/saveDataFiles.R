# Converting raw data into package data
library(usethis)
library(magrittr)

# AUX function to deal with ASCII characters
decode_ascii <- function(text) {
  escaped_text <- htmltools::htmlEscape(text)
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}

#=========================================================
#=========================================================
# Pollutants to be included in the model
panel_pollutants <- c("BC", "OC", "PrimPM25", "NOx", "VOC", "NMVOC")
usethis::use_data(panel_pollutants, overwrite = TRUE)

# Panel data
panel_data <- create_panel()
usethis::use_data(panel_data, overwrite = TRUE)

#Raw ssp data
raw.ssp.data <- read.csv("./inst/extdata/socioeconomic/SSP_database_2024.csv",
                        skip = 8, fileEncoding = "UTF-8-BOM")
raw.ssp.data$Region <- sapply(raw.ssp.data$Region, decode_ascii)
usethis::use_data(raw.ssp.data, overwrite = TRUE)

# Country-level GDP by SSP scenario
gdp_ctry.SSP1 <- get_gdp_ctry(ssp = 'SSP1')
usethis::use_data(gdp_ctry.SSP1, overwrite = TRUE)
gdp_ctry.SSP2 <- get_gdp_ctry(ssp = 'SSP2')
usethis::use_data(gdp_ctry.SSP2, overwrite = TRUE)
gdp_ctry.SSP3 <- get_gdp_ctry(ssp = 'SSP3')
usethis::use_data(gdp_ctry.SSP3, overwrite = TRUE)
gdp_ctry.SSP4 <- get_gdp_ctry(ssp = 'SSP4')
usethis::use_data(gdp_ctry.SSP4, overwrite = TRUE)
gdp_ctry.SSP5 <- get_gdp_ctry(ssp = 'SSP5')
usethis::use_data(gdp_ctry.SSP5, overwrite = TRUE)

# Country-level population by SSP scenario
pop_ctry.SSP1 <- get_pop_ctry(ssp = 'SSP1')
usethis::use_data(pop_ctry.SSP1, overwrite = TRUE)
pop_ctry.SSP2 <- get_pop_ctry(ssp = 'SSP2')
usethis::use_data(pop_ctry.SSP2, overwrite = TRUE)
pop_ctry.SSP3 <- get_pop_ctry(ssp = 'SSP3')
usethis::use_data(pop_ctry.SSP3, overwrite = TRUE)
pop_ctry.SSP4 <- get_pop_ctry(ssp = 'SSP4')
usethis::use_data(pop_ctry.SSP4, overwrite = TRUE)
pop_ctry.SSP5 <- get_pop_ctry(ssp = 'SSP5')
usethis::use_data(pop_ctry.SSP5, overwrite = TRUE)

# Adjust country names due to differences in datasets:
adj_ctry <- read.csv("./inst/extdata/adj_ctry.csv", fileEncoding = "UTF-8-BOM")
# Applying the function to decode HTML entities in col1
adj_ctry$country <- sapply(adj_ctry$country, decode_ascii)
adj_ctry$adj_country <- sapply(adj_ctry$adj_country, decode_ascii)
usethis::use_data(adj_ctry, overwrite = TRUE)

# Adjust country names due to differences in the output and the panel data
adj_ctry_output <- read.csv("./inst/extdata/adj_ctry_output.csv", fileEncoding = "UTF-8-BOM")
adj_ctry_output$country_name <- sapply(adj_ctry_output$country_name, decode_ascii)
adj_ctry_output$data_name <- sapply(adj_ctry_output$data_name, decode_ascii)
usethis::use_data(adj_ctry_output, overwrite = TRUE)

# Adjust country names due to differences in the output and map raster
adj_ctry_map <- read.csv("./inst/extdata/adj_ctry_map.csv", fileEncoding = "UTF-8-BOM")
adj_ctry_map$country_name = sapply(adj_ctry_map$country_name, decode_ascii)
adj_ctry_map$country_map = sapply(adj_ctry_map$country_map, decode_ascii)
adj_ctry_map <- unique(adj_ctry_map)
usethis::use_data(adj_ctry_map, overwrite = TRUE)

# Load GDP data for some missing countries from a previous dataset (SSP-v9)
ssp_gdp_adj <- read.csv("./inst/extdata/socioeconomic/SSP_adj.csv",
                       skip = 8, fileEncoding = "UTF-8-BOM")
ssp_gdp_adj$Region = sapply(ssp_gdp_adj$Region, decode_ascii)
usethis::use_data(ssp_gdp_adj, overwrite = TRUE)

# Bias adder by IA and country
# hia_adder <- read.csv("./inst/extdata/mort/hia_adder.csv", fileEncoding = "UTF-8-BOM")
hia_adder <- rhap::calc_hia_adder()
hia_adder$country = sapply(hia_adder$country, decode_ascii)
usethis::use_data(hia_adder, overwrite = TRUE)

# List of pollutants from the residential sector
all_pollutants = c("BC", "CH4", "CO", "HFC125", "HFC134a", "HFC143a", "HFC23", "HFC32",
                   "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2")
usethis::use_data(all_pollutants, overwrite = TRUE)

# List of GCAM-32 + EU-27 + Global regions
gcam_regions = c(
  "Africa_Eastern","Africa_Northern","Africa_Southern","Africa_Western","Argentina",
  "Australia_NZ","Brazil","Canada","Central America and Caribbean","Central Asia",
  "China","Colombia","EU-12","EU-15","Europe_Eastern","Europe_Non_EU",
  "European Free Trade Association","India","Indonesia","Japan","Mexico",
  "Middle East","Pakistan","Russia","South Africa","South America_Northern",
  "South America_Southern","South Asia","South Korea","Southeast Asia","Taiwan",
  "USA","EU-27","Global"
)
usethis::use_data(gcam_regions, overwrite = TRUE)

# Load test dat file:
test_rhap <- rgcam::loadProject("./test_rhap")
usethis::use_data(test_rhap, overwrite = TRUE)
