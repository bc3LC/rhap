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

  year <- value <- iso <- sector <- ghg <- fuel <- location_name <- rei_name <-
    measure_name <- cause_name <- sex_name <- val <- country_name <- Deaths <-
    `DALYs (Disability-Adjusted Life Years)` <- `YLLs (Years of Life Lost)` <-
    country <- gdp_ppp_dol2011 <- BC <- OC <- code <- name <- year_month <-
    value <- iso <- year <- Country.Name <- Country.Code <- Indicator.Name <-
    Indicator.Code <- GCAM_region <- PrimPM25 <- NOx <- CO <- NMVOC <- flsp_mm2 <-
    BC_per_100k <- OC_per_100k <- PrimPM25_per_100k <- NOx_per_100k <- CO_per_100k <-
    VOC_per_100k <- HDD_value <- CDD_value <- BC_per_flsp <- OC_per_flsp <-
    PrimPM25_per_flsp <- NOx_per_flsp <- CO_per_flsp <- VOC_per_flsp <-
    Deaths_per_100k <- YLL_per_100k <- DALY_per_100k <- continent <-
    gdppc_ppp_dol2011 <- . <- NULL




  # AUX functions to deal with ASCII characters
  decode_ascii <- function(text) {

    escaped_text <- htmltools::htmlEscape(text)
    xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))

  }

  read_ascii <- function(file_path, skip = 0, ascii_cols = NULL) {

    data <- utils::read.csv(file.path(file_path), skip = skip, fileEncoding = "UTF-8-BOM")

    for (col in ascii_cols) {
      data[[col]] <- sapply(data[[col]], decode_ascii)
    }

    return(invisible(data))
  }






  rlang::inform('Generating panel ...')

  datadir <- paste0(getwd(),"/inst/extdata")


  reg_iso<-utils::read.csv(file.path(datadir,"/iso_region.csv"), fileEncoding = "UTF-8-BOM")
  reg_iso$country_name <- sapply(reg_iso$country_name, decode_ascii)

  resid_em_kt<-dplyr::bind_rows(
    utils::read.csv(file.path(datadir,"/emissions/BC_total_CEDS_emissions.csv"), skip = 6, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "BC"),
    utils::read.csv(file.path(datadir,"/emissions/CO_total_CEDS_emissions.csv"), skip = 7, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "CO"),
    utils::read.csv(file.path(datadir,"/emissions/CH4_total_CEDS_emissions.csv"), skip = 7, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "CH4"),
    utils::read.csv(file.path(datadir,"/emissions/N2O_total_CEDS_emissions.csv"), skip = 6, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "N2O"),
    utils::read.csv(file.path(datadir,"/emissions/NH3_total_CEDS_emissions.csv"), skip = 6, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "NH3"),
    utils::read.csv(file.path(datadir,"/emissions/NMVOC_total_CEDS_emissions.csv"), skip = 6, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "NMVOC"),
    utils::read.csv(file.path(datadir,"/emissions/NOx_total_CEDS_emissions.csv"), skip = 6, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "NOx"),
    utils::read.csv(file.path(datadir,"/emissions/OC_total_CEDS_emissions.csv"), skip = 7, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "OC"),
    utils::read.csv(file.path(datadir,"/emissions/SO2_total_CEDS_emissions.csv"), skip = 7, fileEncoding = "UTF-8-BOM") %>% dplyr::mutate(ghg = "SO2")) %>%
    tidyr::gather(year, value, -iso, -sector, -ghg ,-fuel, -units) %>%
    dplyr::mutate(year = gsub("X","",year)) %>%
    dplyr::filter(grepl("Residential",sector)) %>%
    dplyr::group_by(iso, sector, ghg, year , units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(ghg, value) %>%
    dplyr::mutate(year = as.numeric(year))


  mort<-tibble::as_tibble(dplyr::bind_rows(read_ascii(file_path = file.path(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-1.csv"), ascii_cols = 'location_name'),
                                           read_ascii(file_path = file.path(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-2.csv"), ascii_cols = 'location_name'))) %>%
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


  # Join emissions and mortality data
  data <- mort %>%
    dplyr::left_join(resid_em_kt %>%
                       dplyr::select(-sector, -units),
                     by = c("iso","year"))

  # # Check NA data
  # data_missing <- data %>% dplyr::filter(is.na(BC) | is.na(Deaths))
  # unique(data_missing$country_name)

  # Andorra, San Marino, Monaco, Nauru, Tuvalu and Mariana Islands do not have complete data, and they are dplyr::filtered out
  data <- data %>%
    dplyr::filter(stats::complete.cases(.))

  #-------------

  # Load all the socioeconomic data to be included
  pop <- read_ascii(file_path = file.path(datadir, "/socioeconomic/population.csv"), ascii_cols = 'country') %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::mutate(iso = dplyr::if_else(country == "Sudan", "ssd", iso)) %>%
    dplyr::select(-country) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  gdp <- read_ascii(file_path = file.path(datadir, "/socioeconomic/gdp.csv"), ascii_cols = 'country_name') %>%
    dplyr::select(country_name, iso, year, gdp_ppp_dol2011) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::mutate(iso = dplyr::if_else(country_name == "Romania", "rom", iso),
                  iso = dplyr::if_else(country_name == "Former Sudan", "ssd", iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  # Adjust 2019 GDP
  gdp_2019 <- gdp %>%
    dplyr::filter(year == 2018) %>%
    dplyr::mutate(year = 2019)

  gdp <- dplyr::bind_rows(gdp, gdp_2019)

  urbrur<-read_ascii(file_path = file.path(datadir, "/socioeconomic/urb_rur_shares.csv"), ascii_cols = 'country_name') %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  elec_acces<-read_ascii(file_path = file.path(datadir, "/socioeconomic/elec_access.csv"), ascii_cols = 'country_name') %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name)  %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  cc_acces<-read_ascii(file_path = file.path(datadir, "/socioeconomic/clean_cook_access.csv"), ascii_cols = 'country_name') %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")


  # Create the dataset with all the socioeconomic data
  socio<-tibble::as_tibble(pop) %>%
    #gcamdata::left_join_error_no_match(gdp) %>%
    dplyr::left_join(gdp, by = c("iso", "year")) %>%
    dplyr::left_join(urbrur, by = c("iso", "year")) %>%
    dplyr::left_join(elec_acces, by = c("iso", "year")) %>%
    dplyr::left_join(cc_acces, by = c("iso", "year")) %>%
    dplyr::mutate(iso = dplyr::if_else(iso == "rom", "rou", iso))
  # NOTE: There are some countries with no harmonized data available. These are listed in ./inst/extdata/socioeconomic/missing_socio.csv


  # Combine all the final data to be analyzed:
  data_fin<-data %>%
    dplyr::left_join(socio, by = c("iso","year")) %>%
    dplyr::mutate(gdppc_ppp_dol2011 = gdp_ppp_dol2011 / pop) %>%
    # Make some adjustments for econometric analysis
    dplyr::mutate(PrimPM25 = BC + OC)

  # Function for climate data
  preprocess_clima <- function(file_path, value_name) {
    data <- read_ascii(file_path = file_path, ascii_cols = 'name')
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
   HDD <- read_ascii(file_path = file.path(datadir, "/climate/HDD.csv"), ascii_cols = 'country') %>%
    dplyr::select(iso, year, HDD_value = value) %>%
    dplyr::mutate(iso = tolower(iso))

  CDD <- read_ascii(file_path = file.path(datadir, "/climate/CDD.csv"), ascii_cols = 'country') %>%
    dplyr::select(iso, year, CDD_value = value) %>%
    dplyr::mutate(iso = tolower(iso))

  data_fin <- data_fin %>%
    dplyr::left_join(HDD, by = c("iso", "year")) %>%
    dplyr::left_join(CDD, by = c("iso", "year"))


  # AAP
  AAP <- read_ascii(file_path = file.path(datadir, "/concentrations/pm2.5.csv"), ascii_cols = 'Country.Name')

  colnames(AAP) <- c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", as.character(1960:2019))

  AAP_long <- AAP %>%
    tidyr::gather(key = "year", value = "AAP", -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code) %>%
    dplyr::mutate(year = as.numeric(year),
           iso = tolower(Country.Code)) %>%
    dplyr::select(iso, year, AAP)

  data_fin <- data_fin %>%
    dplyr::left_join(AAP_long, by = c("iso", "year"))

  # Floorspace
  flsp <- read_ascii(file_path = file.path(datadir, "/socioeconomic/floorspace.csv"), ascii_cols = 'country_name')

  flsp_long <- flsp %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                 names_to = "year",
                 values_to = "flsp") %>%
    dplyr::mutate(year = as.integer(stringr::str_remove(year, "X")),
           iso = tolower(iso)) %>%
    dplyr::select(iso, year, flsp)  %>%
    dplyr::mutate(year = dplyr::if_else(year == 2020, 2019, year))

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
  iso_GCAM_regID <- utils::read.csv(paste0(datadir, "/iso_GCAM_regID.csv"), fileEncoding = "UTF-8-BOM") %>%
    dplyr::select(iso, GCAM_region)

  data_fin_ret <- data_fin %>%
    dplyr::left_join(iso_GCAM_regID, by = "iso") %>%
    dplyr::filter(!is.na(GCAM_region)) %>%
    # adjust Taiwan Flsp
    dplyr::mutate(flsp = dplyr::if_else(year == 2019 & iso == "twn", 5.639237, flsp)) %>%
    # normalize emissions and deaths
    dplyr::mutate(BC_per_100k = (BC / pop) * 100000,
                 OC_per_100k = (OC / pop) * 100000,
                 PrimPM25_per_100k = (PrimPM25 / pop) * 100000,
                 NOx_per_100k = (NOx / pop) * 100000,
                 CO_per_100k = (CO / pop) * 100000,
                 VOC_per_100k = (NMVOC / pop) * 100000,
                 Deaths_per_100k = (Deaths / pop) * 100000,
                 YLL_per_100k = (`YLLs (Years of Life Lost)` / pop) * 100000,
                 DALY_per_100k = (`DALYs (Disability-Adjusted Life Years)` / pop) * 100000) %>%
    # normalize emissions per floorspace
    dplyr::mutate(flsp_mm2 = flsp * pop * 1E-6) %>%
    dplyr::mutate(BC_per_flsp = BC / flsp_mm2,
                 OC_per_flsp = OC / flsp_mm2,
                 PrimPM25_per_flsp = PrimPM25 / flsp_mm2,
                 NOx_per_flsp = BC / flsp_mm2,
                 CO_per_flsp = CO / flsp_mm2,
                 VOC_per_flsp = NMVOC / flsp_mm2,) %>%
    # add small numbers to 0 emisisons for log
    dplyr::mutate(BC_per_100k = dplyr::if_else(BC_per_100k == 0, 1E-9, BC_per_100k),
                 OC_per_100k = dplyr::if_else(OC_per_100k == 0, 1E-9, OC_per_100k),
                 PrimPM25_per_100k = dplyr::if_else(PrimPM25_per_100k == 0, 1E-9, PrimPM25_per_100k),
                 NOx_per_100k = dplyr::if_else(NOx_per_100k == 0, 1E-9, NOx_per_100k),
                 CO_per_100k = dplyr::if_else(CO_per_100k == 0, 1E-9, CO_per_100k),
                 VOC_per_100k = dplyr::if_else(VOC_per_100k == 0, 1E-9, VOC_per_100k),
                 HDD_value = dplyr::if_else(HDD_value == 0, 1E-9, HDD_value),
                 CDD_value = dplyr::if_else(CDD_value == 0, 1E-9, CDD_value),
                 BC_per_flsp = dplyr::if_else(BC_per_flsp == 0, 1E-9, BC_per_flsp),
                 OC_per_flsp = dplyr::if_else(OC_per_flsp == 0, 1E-9, OC_per_flsp),
                 PrimPM25_per_flsp = dplyr::if_else(PrimPM25_per_flsp == 0, 1E-9, PrimPM25_per_flsp),
                 NOx_per_flsp = dplyr::if_else(NOx_per_flsp == 0, 1E-9, NOx_per_flsp),
                 CO_per_flsp = dplyr::if_else(CO_per_flsp == 0, 1E-9, CO_per_flsp)) %>%
    # add logs for numeric variables
    dplyr::mutate(log_BC_per_100k = log(BC_per_100k),
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
    dplyr::mutate(continent = countrycode::countrycode(sourcevar = iso,
                                                       origin = "iso3c",
                                                       destination = "continent")) %>%
    dplyr::mutate(continent = dplyr::if_else(grepl("Asia", continent) | grepl("Oceania", continent), "Asia & Oceania", continent)) %>%
    dplyr::mutate(dev = dplyr::if_else(grepl("Europe", continent) |
                                       grepl("Japan", country_name) |
                                       grepl("usa", iso) |
                                       grepl("can", country_name), "Developed", "Developing"))


  return(data_fin_ret)

}
