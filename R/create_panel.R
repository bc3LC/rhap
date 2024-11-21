#' create_panel
#'
#'@description
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
#' @keywords Panel data
#' @importFrom magrittr %>%
#' @export
#' @return A multinational and multi-year data panel to complete the econometric analysis
create_panel <- function() {

  rlang::inform('Generating panel ...')

  datadir <- paste0(getwd(),"/inst/extdata")


  reg_iso<-read.csv(file.path(datadir,"/iso_region.csv"))

  resid_em_kt<-dplyr::bind_rows(
    read.csv(file.path(datadir,"/emissions/BC_total_CEDS_emissions.csv"), skip = 6) %>% dplyr::mutate(ghg = "BC"),
    read.csv(file.path(datadir,"/emissions/CO_total_CEDS_emissions.csv"), skip = 7) %>% dplyr::mutate(ghg = "CO"),
    read.csv(file.path(datadir,"/emissions/CH4_total_CEDS_emissions.csv"), skip = 7) %>% dplyr::mutate(ghg = "CH4"),
    read.csv(file.path(datadir,"/emissions/N2O_total_CEDS_emissions.csv"), skip = 6) %>% dplyr::mutate(ghg = "N2O"),
    read.csv(file.path(datadir,"/emissions/NH3_total_CEDS_emissions.csv"), skip = 6) %>% dplyr::mutate(ghg = "NH3"),
    read.csv(file.path(datadir,"/emissions/NMVOC_total_CEDS_emissions.csv"), skip = 6) %>% dplyr::mutate(ghg = "NMVOC"),
    read.csv(file.path(datadir,"/emissions/NOx_total_CEDS_emissions.csv"), skip = 6) %>% dplyr::mutate(ghg = "NOx"),
    read.csv(file.path(datadir,"/emissions/OC_total_CEDS_emissions.csv"), skip = 7) %>% dplyr::mutate(ghg = "OC"),
    read.csv(file.path(datadir,"/emissions/SO2_total_CEDS_emissions.csv"), skip = 7) %>% dplyr::mutate(ghg = "SO2")) %>%
    tidyr::gather(year, value, -iso, -sector, -ghg ,-fuel, -units) %>%
    dplyr::mutate(year = gsub("X","",year)) %>%
    dplyr::filter(grepl("Residential",sector)) %>%
    dplyr::group_by(iso, sector, ghg, year , units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(ghg, value) %>%
    dplyr::mutate(year = as.numeric(year))


  mort<-tibble::as_tibble(dplyr::bind_rows(read.csv(file.path(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-1.csv")),
                                           read.csv(file.path(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-2.csv")))) %>%
    dplyr::select(country_name = location_name, rei_name, measure_name, cause_name, year, sex_name, val) %>%
    dplyr::filter(sex_name %in% c("Both")) %>%
    tidyr::spread(measure_name, val) %>%
    gcamdata::left_join_error_no_match(reg_iso, by = "country_name")

  # Aggregate mort (Disaggregations to be tested in the future)
  mort <- mort %>%
    tidyr::replace_na(list(Deaths = 0)) %>%
    tidyr::replace_na(list(`YLLs (Years of Life Lost)` = 0)) %>%
    dplyr::group_by(country_name, rei_name, year, sex_name, iso) %>%
    dplyr::summarise(Deaths = sum(Deaths), `DALYs (Disability-Adjusted Life Years)` = sum(`DALYs (Disability-Adjusted Life Years)`), `YLLs (Years of Life Lost)` = sum(`YLLs (Years of Life Lost)`)) %>%
    dplyr::ungroup()


  #-------------
  # Check the country names that need to be manually adjusted
  names_ok<-mort %>%  dplyr::filter(complete.cases(.)) %>% dplyr::select(country_name) %>% dplyr::distinct()
  names_all<-mort %>% dplyr::select(country_name) %>% dplyr::distinct()
  names_missing<- dplyr::anti_join(names_all,names_ok, by = "country_name")
  if (nrow(names_missing > 0)) stop(paste0('The Country ', names_missing, ' needs to be adjusted. '))
  #-------------


  data<-mort %>%
    dplyr::left_join(resid_em_kt %>%
                       dplyr::select(-sector, -units),
                     by = c("iso","year"))

  #-------------
  # Check missing data
  # TODO -  "Andorra", "Monaco", "Nauru", "Northern Mariana Islands", "San Marino", "Tuvalu" needs to be checked
  # data_complete<- data %>% filter(!grepl("Sense", cause_name)) %>% filter(complete.cases(.))
  # data_missing<- dplyr::anti_join(data %>% filter(!grepl("Sense", cause_name)), data_complete)
  # missing_contries <- unique(data_missing$country_name)
  # if (length(missing_contries > 0)) stop(paste0('The Country ', missing_contries, ' needs to be adjusted. '))
  #-------------

  # Load all the socioeconomic data to be included
  pop<- read.csv(file.path(datadir, "/socioeconomic/population.csv")) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  gdp<- read.csv(file.path(datadir, "/socioeconomic/gdp.csv")) %>%
    dplyr::select(country_name, iso, year, gdp_ppp_dol2011) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  # Adjust 2019 GDP
  gdp_2019 <- gdp %>%
    filter(year == 2018) %>%
    mutate(year = 2019)

  gdp <- bind_rows(gdp, gdp_2019)

  urbrur<-read.csv(file.path(datadir, "/socioeconomic/urb_rur_shares.csv"))%>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  elec_acces<-read.csv(file.path(datadir, "/socioeconomic/elec_access.csv")) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name)  %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  cc_acces<-read.csv(file.path(datadir, "/socioeconomic/clean_cook_access.csv")) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")


  # Create the dataset with all the socioeconomic data
  socio<-tibble::as_tibble(pop) %>%
    #gcamdata::dplyr::left_join_error_no_match(gdp) %>%
    dplyr::left_join(gdp, by = c("iso", "year")) %>%
    dplyr::left_join(urbrur, by = c("iso", "year")) %>%
    dplyr::left_join(elec_acces, by = c("iso", "year")) %>%
    dplyr::left_join(cc_acces, by = c("iso", "year"))


  # Combine all the final data to be analyzed:
  data_fin<-data %>%
    dplyr::left_join(socio, by = c("iso","year")) %>%
    dplyr::mutate(gdppc_ppp_dol2011 = gdp_ppp_dol2011 / pop) %>%
    # Make some adjustments for econometric analysis
    dplyr::mutate(PrimPM25 = BC + OC)

  # Function for climate data
  preprocess_clima <- function(file_path, value_name) {
    data <- read.csv(file_path)
    data_long <- data %>%
      tidyr::gather(key = "year_month", value = "value", -code, -name) %>%
      dplyr::mutate(year = as.numeric(sub("X", "", sub("\\..*", "", year_month))),
             iso = tolower(code),
             !!value_name := value) %>%
      dplyr::select(iso, year, !!rlang::sym(value_name))
    return(data_long)
  }


  # Climate variables
  tas1 <- preprocess_clima(file.path(datadir, "/climate/tas_timeseries.csv"), "tas")
  pr1 <- preprocess_clima(file.path(datadir, "/climate/pr_timeseries.csv"), "pr")
  tasmax1 <- preprocess_clima(file.path(datadir, "/climate/tasmax_timeseries.csv"), "tasmax")
  tasmin1 <- preprocess_clima(file.path(datadir, "/climate/tasmin_timeseries.csv"), "tasmin")

  # Merge new data with `data_fin`
  data_fin <- data_fin %>%
    dplyr::left_join(tas1, by = c("iso", "year")) %>%
    dplyr::left_join(pr1, by = c("iso", "year")) %>%
    dplyr::left_join(tasmax1, by = c("iso", "year")) %>%
    dplyr::left_join(tasmin1, by = c("iso", "year"))

  #HDD and CDD variables
   HDD <- read.csv(paste0(datadir, "/climate/HDD.csv")) %>%
    dplyr::select(iso, year, HDD_value = value) %>%
    dplyr::mutate(iso = tolower(iso))

  CDD <- read.csv(paste0(datadir, "/climate/CDD.csv")) %>%
    dplyr::select(iso, year, CDD_value = value) %>%
    dplyr::mutate(iso = tolower(iso))

  data_fin <- data_fin %>%
    dplyr::left_join(HDD, by = c("iso", "year")) %>%
    dplyr::left_join(CDD, by = c("iso", "year"))


  # AAP
  AAP <- read.csv(file.path(datadir, "/concentrations/pm2.5.csv"))

  colnames(AAP) <- c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", as.character(1960:2019))

  AAP_long <- AAP %>%
    tidyr::gather(key = "year", value = "AAP", -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code) %>%
    dplyr::mutate(year = as.numeric(year),
           iso = tolower(Country.Code)) %>%
    dplyr::select(iso, year, AAP)

  data_fin <- data_fin %>%
    dplyr::left_join(AAP_long, by = c("iso", "year"))

  # Floorspace
  flsp <- read.csv(file.path(datadir, "/socioeconomic/floorspace.csv"))

  flsp_long <- flsp %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 values_to = "flsp") %>%
    dplyr::mutate(year = as.integer(stringr::str_remove(year, "X")),
           iso = tolower(iso)) %>%
    dplyr::select(iso, year, flsp)  %>%
    mutate(year = if_else(year == 2020, 2019, year))

  data_fin <- data_fin %>%
    dplyr::left_join(flsp_long, by = c("iso", "year"))

  rm(flsp, AAP)

  interpolate_columns <- function(data, columns) {
    data[columns] <- lapply(data[columns], function(x) {
      if (is.numeric(x)) {
        return(zoo::na.approx(x, na.rm = FALSE))
      }
      return(x)
    })
    return(data)
  }

  # Identify columns with missing values
  columns_with_na <- colnames(data_fin)[colSums(is.na(data_fin)) > 0]

  # Apply interpolation to the specified columns grouping by 'iso'
  data_fin <- data_fin %>%
    dplyr::group_by(iso) %>%
    dplyr::do(interpolate_columns(., columns_with_na)) %>%
    dplyr::ungroup()


  # Add GCAM_region
  iso_GCAM_regID <- read.csv(paste0(datadir, "/iso_GCAM_regID.csv")) %>%
    select(iso, GCAM_region)

  data_fin_ret <- data_fin %>%
    left_join(iso_GCAM_regID, by = "iso") %>%
    dplyr::filter(!is.na(GCAM_region)) %>%
    # adjust Taiwan Flsp
    mutate(flsp = if_else(year == 2019 & iso == "twn", 5.639237, flsp)) %>%
    # normalize emissions and deaths
    mutate(BC_per_100k = (BC / pop) * 100000,
           OC_per_100k = (OC / pop) * 100000,
           PrimPM25_per_100k = (PrimPM25 / pop) * 100000,
           NOx_per_100k = (NOx / pop) * 100000,
           CO_per_100k = (CO / pop) * 100000,
           VOC_per_100k = (NMVOC / pop) * 100000,
           Deaths_per_100k = (Deaths / pop) * 100000,
           YLL_per_100k = (`YLLs (Years of Life Lost)` / pop) * 100000,
           DALY_per_100k = (`DALYs (Disability-Adjusted Life Years)` / pop) * 100000) %>%
    # normalize emissions per floorspace
    mutate(flsp_mm2 = flsp * pop * 1E-6) %>%
    mutate(BC_per_flsp = BC / flsp_mm2,
           OC_per_flsp = OC / flsp_mm2,
           PrimPM25_per_flsp = PrimPM25 / flsp_mm2,
           NOx_per_flsp = BC / flsp_mm2,
           CO_per_flsp = CO / flsp_mm2,
           VOC_per_flsp = NMVOC / flsp_mm2,) %>%
    # add small numbers to 0 emisisons for log
    mutate(BC_per_100k = if_else(BC_per_100k == 0, 1E-9, BC_per_100k),
           OC_per_100k = if_else(OC_per_100k == 0, 1E-9, OC_per_100k),
           PrimPM25_per_100k = if_else(PrimPM25_per_100k == 0, 1E-9, PrimPM25_per_100k),
           NOx_per_100k = if_else(NOx_per_100k == 0, 1E-9, NOx_per_100k),
           CO_per_100k = if_else(CO_per_100k == 0, 1E-9, CO_per_100k),
           VOC_per_100k = if_else(VOC_per_100k == 0, 1E-9, VOC_per_100k),
           HDD_value = if_else(HDD_value == 0, 1E-9, HDD_value),
           CDD_value = if_else(CDD_value == 0, 1E-9, CDD_value),
           BC_per_flsp = if_else(BC_per_flsp == 0, 1E-9, BC_per_flsp),
           OC_per_flsp = if_else(OC_per_flsp == 0, 1E-9, OC_per_flsp),
           PrimPM25_per_flsp = if_else(PrimPM25_per_flsp == 0, 1E-9, PrimPM25_per_flsp),
           NOx_per_flsp = if_else(NOx_per_flsp == 0, 1E-9, NOx_per_flsp),
           CO_per_flsp = if_else(CO_per_flsp == 0, 1E-9, CO_per_flsp)) %>%
    # add logs for numeric variables
    mutate(log_BC_per_100k = log(BC_per_100k),
           log_OC_per_100k = log(OC_per_100k),
           log_PrimPM25_per_100k = log(PrimPM25_per_100k),
           log_NOx_per_100k = log(NOx_per_100k),
           log_CO_per_100k = log(CO_per_100k),
           log_VOC_per_100k = log(VOC_per_100k),
           log_Deaths_per_100k = log(Deaths_per_100k),
           log_YLL_per_100k = log(YLL_per_100k),
           log_DALY_per_100k = log(DALY_per_100k),
           log_gdppc_ppp_dol2011 = log(gdppc_ppp_dol2011),
           log_AAP = log(AAP),
           log_flsp = log(flsp),
           log_HDD_value = log(HDD_value),
           log_CDD_value = log(CDD_value),
           log_BC_per_flsp = log(BC_per_flsp),
           log_OC_per_flsp = log(OC_per_flsp),
           log_PrimPM25_per_flsp = log(PrimPM25_per_flsp),
           log_NOx_per_flsp = log(NOx_per_flsp),
           log_CO_per_flsp = log(CO_per_flsp)) %>%
    # Add continent and development degree
    mutate(continent = countrycode(sourcevar = iso,
                                   origin = "iso3c",
                                   destination = "continent")) %>%
    mutate(continent = if_else(grepl("Asia", continent) | grepl("Oceania", continent), "Asia & Oceania", continent)) %>%
    mutate(dev = if_else(grepl("Europe", continent) |
                           grepl("Japan", country_name) |
                           grepl("usa", iso) |
                           grepl("can", country_name), "Developed", "Developing"))


  return(data_fin_ret)

}
