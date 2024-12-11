#' get_gdp_ctry
#'
#'@description
#' Ancillary function to get country-level GDPpc from SSP database
#'
#' SSP database version 3.0.1 updated in 2024
#'
#' @source  IIASA SSP database
#' @keywords GDPpc, SSP
#' @param ssp SSP scenario c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
#' @importFrom magrittr %>%
#' @export
#' @return Per capita GDP by country

get_gdp_ctry <- function(ssp) {

  iso <- country_name <- year <- pop <- continent <- dev <- variable <- value <-
    Model <- Scenario <- Region <- Variable <- Unit <- . <- NULL

  # Check user input
  if (!ssp %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
    stop(sprintf(
      "Error: The specified SSP '%s' is invalid. Accepted SSPs are: %s. Please rerun the `get_pop_ctry` function with a valid SSP.",
      ssp, paste(c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"), collapse = ", ")
    ))
  }

  # Set some constants
  conv_bil_dol <- 1E9

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  max_base_year <- raw.ssp.data %>%
    tidyr::gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
    dplyr::mutate(year=gsub("X", "", year)) %>%
    dplyr::filter(Scenario == "Historical Reference") %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::select(year) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  ssp.data <- raw.ssp.data %>%
    tidyr::gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
    dplyr::mutate(year=gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, Scenario)) %>%
    dplyr::rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit) %>%
    dplyr::filter(year >= max_base_year)

  # Extract GDP
  gdp <- tibble::as_tibble(ssp.data) %>%
    dplyr::filter(variable == "GDP|PPP") %>%
  # Transfrom units to dol2011PPP
    dplyr::mutate(gdp = value * conv_bil_dol * gcamdata::gdp_deflator(2011, 2017)) %>%
    dplyr::select(region, scenario, year, gdp_dol2011_ppp = gdp)


  invisible(gdp)

}

