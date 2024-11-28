#' get_pop_ctry
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

get_pop_ctry <- function(ssp) {

  # Set some constants
  conv_M_people <- 1E6

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  max_base_year <- raw.ssp.data %>%
    tidyr::gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
    dplyr::mutate(year=gsub("X", "", year)) %>%
    dplyr::filter(Scenario == "Historical Reference") %>%
    dplyr::filter(complete.cases(.)) %>%
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

  # Substract country-level population data:
  pop <- tibble::as_tibble(ssp.data) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::mutate(pop = value * conv_M_people) %>%
    dplyr::select(scenario, region, year, pop)

  invisible(pop)

}

