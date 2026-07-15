#' fit_model
#'
#' @description
#' Fit the model using fixed effects model and plm function.
#'
#' For the model dplyr::selection, testing and validation see "./rhap/inst/extdata/model_testing.R"
#'
#' @source  Details on plm estimation can be found here: https://cran.r-project.org/web/packages/plm/plm.pdf#'
#' @keywords Econometric model; fixed effects
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys")
#' @param countries Target countries over which the model will be fit (defaults to \code{"All"}).
#' For a complete list of valid country names, check \code{unique(rhap::panel_data$country_name)}.
#' @importFrom magrittr %>%
#' @export
#' @return Regression model and coefficients for prediction

fit_model <- function(HIA_var, countries = 'All') {
  iso <- country_name <- year <- pop <- continent <- dev <- log_AAP <- value <-
    Model <- Scenario <- Region <- Variable <- Unit <- . <- NULL

  # Check user input
  if (!HIA_var %in% c("deaths", "yll", "dalys")) {
    stop(sprintf(
      "Error: The specified HIA_var '%s' is invalid. Accepted HIA_var are: %s. Please rerun the `fit_model` function with a valid HIA_var.",
      HIA_var, paste(c("deaths", "yll", "dalys"), collapse = ", ")
    ))
  }
  if ((length(countries) == 1 && countries != "All") || (length(countries) != 1)) {
    wrong_countries <- as.character(setdiff(countries, unique(rhap::panel_data$country_name)))
    if (length(wrong_countries) == 1) {
      stop(sprintf(
        "Error: The specified country '%s' is invalid. Run `unique(rhap::panel_data$country_name)` to see the accepted country names are. Please rerun the `fit_model` function with valid `coutries`.",
        wrong_countries))
    } else if (length(wrong_countries) > 1) {
      stop(sprintf(
        "Error: The specified countries '%s' are invalid. Run `unique(rhap::panel_data$country_name)` to see the accepted country names are. Please rerun the `fit_model` function with valid `coutries`.",
        paste(wrong_countries, collapse = ", ")))
    }
  }

  # Adjust the data
  data <- rhap::panel_data %>%
    dplyr::select(iso, country_name, year, pop, dplyr::starts_with("log"), continent, dev) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::select(-log_AAP, -log_HDD_value, -log_CDD_value) %>%
    dplyr::filter(stats::complete.cases(.))

  if (countries == 'All') {
    # List all the available countries if All are considered
    countries = unique(data$country_name)
  }

  # dplyr::select the dependent variable (deaths, YLLs, or DALYs)

  # Create a named list to map HIA_var values to corresponding dep_var values
  HIA_var_map <- list(
    deaths = "log_Deaths_per_100k",
    ylls = "log_YLL_per_100k",
    dalys = "log_DALY_per_100k"
  )

  # Assign the value from the named list
  dep_var <- HIA_var_map[[HIA_var]]


  # Fit the fixed effect model

  # write formula which depends on the dplyr::selected variable
  model_formula <- stats::as.formula(paste(
    dep_var, "~ log_PrimPM25_per_100k + log_NOx_per_100k + log_VOC_per_100k +",
    "log_gdppc_ppp_dol2011 + log_flsp"
  ))

  data_ctry_full <- data %>% dplyr::filter(country_name %in% countries)
  # Safe check for non-complete country datasets
  if (nrow(data_ctry_full) == 0) return(NULL)

  model.fixed <- plm::plm(
    model_formula,
    data = data_ctry_full,
    index = c("country_name", "year"),
    model = "within")

  predictable_regions <- unique(data_ctry_full$country_name)
  data_train <- data_ctry_full

  return(list(model.fixed, predictable_regions, data_train))
}
