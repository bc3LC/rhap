#' Selected pollutants
#'
#' @description Pollutants selected to be included in the ecomoetric regression model
#' @source -
#' @format vector
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::panel_pollutants
#' }
"panel_pollutants"


#' Panel data
#'
#' @description
#' Create a panel data to fit the econometric model for the analysis. The panel includes information of the different nations of the world and covers the period 1990-2019.
#'
#' In terms of variables, the panel includes:
#'
#' - Premature deaths, YLLs, and DALYs, attributable to indoor air pollution (IHME)
#'
#' - Emissions from the residential sector of different pollutants, namely BC, OC, CH4, N2O, CO, NOx, NMVOC, NH3 and NMVOC. They are collected from the Community Emissions Database System (CEDS)
#'
#' - Regionally-averaged ambient air pollution levels (World Bank, WB)
#'
#' - Gross Domestic Product (GDP) in PPP (Our World in Data)
#'
#' - Urban/Rural shares by region (WB)
#'
#' - Share of population with access to electricity and clean cooking fuels (World Health Organization, WHO)
#'
#' - Residential floorspace, from multiple sources: India NSSO, Chinese Statistical Yearbook (CSY), Residential Energy Consumption Survey (RECS), EU statistics on income and living conditions (EU-SILC), Odysee
#'
#' - Temperature and precipitation time series  (Climate Change Knowledge Portal (CCKP))
#'
#' - Heating and cooling degree days (Community Climate System Model (CCSM))
#'
#' @source  Multiple sources (described above). These include IHME, CEDS, WB, Our World in Data, WHO, NSSO, CSY, RECS, EU-SILC, Odysee, CCK, CCSM,
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::panel_data
#' }
"panel_data"



#' #' Percen
#'
#' @description  Percentages to downscale GCAM emissions to country-level. Based on CEDS database.
#' @source For GHGs: European Commission, Joint Research Centre (EC-JRC)/Netherlands Environmental Assessment Agency (PBL) (EDGARv7.0_GHG website (https://edgar.jrc.ec.europa.eu/dataset_ghg70)).
#' Emissions Database for Global Atmospheric Research (EDGAR), release EDGAR v7.0_GHG (1970 - 2021) of September 2022. For the energy related sectors the activity data are mainly based on IEA data from IEA (2021) World Energy Balances, www.iea.org/statistics,
#' Crippa, M., Guizzardi, D., Banja, M., Solazzo, E., Muntean, M., Schaaf, E., Pagani, F., Monforti-Ferrario, F., Olivier, J., Quadrelli, R., Risquez Martin, A., Taghavi-Moharamli, P., Grassi, G., Rossi, S., Jacome Felix Oom, D., Branco, A., San-Miguel-Ayanz, J. and Vignati, E., CO2 emissions of all world countries - 2022 Report, EUR 31182 EN, Publications Office of the European Union, Luxembourg, 2022, doi:10.2760/730164, JRC130363
#' Crippa, M., Guizzardi, D., Solazzo, E., Muntean, M., Schaaf, E., Monforti-Ferrario, F., Banja, M., Olivier, J.G.J., Grassi, G., Rossi, S., Vignati, E.,GHG emissions of all world countries - 2021 Report, EUR 30831 EN, Publications Office of the European Union, Luxembourg, 2021, ISBN 978-92-76-41547-3, doi:10.2760/173513, JRC126363
#' For APs: https://zenodo.org/record/3754964#.Y3O_lXbMKUk (CEDS-GBD)
#'
#'
#' \dontrun{
#'  library(rhap);
#'  rhap::Percen
#' }
#'
#'
"Percen"



#' Raw SSP data
#'
#' @description Country level population and GDP data per SSP (SSP_database_2024.csv).
#' @source  IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::raw.ssp.data
#' }
"raw.ssp.data"


#' GDP-SSP database
#'
#' @description Filtered GDP data per SSP1 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gdp_ctry.SSP1
#' }
"gdp_ctry.SSP1"


#' GDP-SSP database
#'
#' @description Filtered GDP data per SSP2 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gdp_ctry.SSP2
#' }
"gdp_ctry.SSP2"


#' GDP-SSP database
#'
#' @description Filtered GDP data per SSP3 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gdp_ctry.SSP3
#' }
"gdp_ctry.SSP3"


#' GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gdp_ctry.SSP4
#' }
"gdp_ctry.SSP4"


#' GDP-SSP database
#'
#' @description Filtered GDP data per SSP5 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gdp_ctry.SSP5
#' }
"gdp_ctry.SSP5"


#' Pop-SSP database
#'
#' @description Filtered Population data per SSP1 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::pop_ctry.SSP1
#' }
"pop_ctry.SSP1"


#' Pop-SSP database
#'
#' @description Filtered Population data per SSP2 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::pop_ctry.SSP2
#' }
"pop_ctry.SSP2"


#' Pop-SSP database
#'
#' @description Filtered Population data per SSP3 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::pop_ctry.SSP3
#' }
"pop_ctry.SSP3"


#' Pop-SSP database
#'
#' @description Filtered Population data per SSP4 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::pop_ctry.SSP4
#' }
"pop_ctry.SSP4"


#' Pop-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_2024.csv).
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::pop_ctry.SSP5
#' }
"pop_ctry.SSP5"


#' adj_ctry
#'
#' @description File to adjust country names and homogenize differences across datasets
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::adj_ctry
#' }
"adj_ctry"


#' ssp_gdp_adj
#'
#' @description File with GDP data for some countries that are not in the updated dataset, but i a previous version
#' @source IIASA SSP database - SSPv9
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::ssp_gdp_adj
#' }
"ssp_gdp_adj"


#' adj_ctry_output
#'
#' @description File to adjust country names and homogenize differences between the output and the panel data
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::adj_ctry_output
#' }
"adj_ctry_output"


#' adj_ctry_map
#'
#' @description File to adjust country names and homogenize differences between the output and the map raster
#' @source IIASA SSP database
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::adj_ctry_map
#' }
"adj_ctry_map"


#' hia_adder
#'
#' @description Bias adder, calculated as the difference between the observed and "predicted" values in final observed year
#' @source Own calculations based on IHME data
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::hia_adder
#' }
"hia_adder"


#' all_pollutants
#'
#' @description List of pollutants from the residential sector
#' @source GCAM results
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::all_pollutants
#' }
"all_pollutants"


#' gcam_regions
#'
#' @description List of GCAM-32 + EU-27 + Global regions
#' @source GCAM results
#' @format .csv
#' @examples
#' \dontrun{
#' library(rhap)
#' rhap::gcam_regions
#' }
"gcam_regions"
