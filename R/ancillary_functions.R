
#' create_panel
#'
#' @return data panel to do the econometric analysis
create_panel <- function() {
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
    dplyr::filter(sex_name %in% c("Male", "Female")) %>%
    tidyr::spread(measure_name, val) %>%
    gcamdata::left_join_error_no_match(reg_iso, by = "country_name")

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
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

  gdp<- read.csv(file.path(datadir, "/socioeconomic/gdp.csv")) %>%
    dplyr::select(country_name, iso, year, gdp_ppp_dol2011) %>%
    dplyr::filter(year %in% unique(data$year)) %>%
    dplyr::mutate(iso = tolower(iso)) %>%
    dplyr::select(-country_name) %>%
    # erase empty iso codes (e.g., the ones that belong to "Africa" or "North America")
    dplyr::filter(!is.na(iso), iso != "")

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
    dplyr::select(iso, year, flsp)

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


  return(data_fin)

}
