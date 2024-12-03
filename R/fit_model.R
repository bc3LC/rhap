#' fit_model
#'
#'@description
#' Fit the model using fixed effects model and plm function.
#'
#' For the model selection, testing and validation see "./rhap/inst/extdata/model_testing.R"
#'
#' @source  Details on plm estimation can be found here: https://cran.r-project.org/web/packages/plm/plm.pdf#'
#' @keywords Econometric model; fixed effects
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys")
#' @importFrom magrittr %>%
#' @export
#' @return Regression model and coefficients for prediction

fit_model <- function(HIA_var) {

  # Adjust the data
  data <- panel_data %>%
    select(iso, country_name, year, pop, starts_with("log"), continent, dev) %>%
    select(-log_AAP) %>%
    filter(complete.cases(.))

  # select the dependent variable (deaths, YLLs, or DALYs)

  # Create a named list to map HIA_var values to corresponding dep_var values
  HIA_var_map <- list(
    deaths = "log_Deaths_per_100k",
    ylls = "log_YLL_per_100k",
    dalys = "log_DALY_per_100k"
  )

  # Assign the value from the named list
  dep_var <- HIA_var_map[[HIA_var]]


  # Fit the fixed effect model

  # write formula which depends on the selected variable
  model_formula <- as.formula(paste(
    dep_var, "~ log_PrimPM25_per_100k + log_NOx_per_100k + log_VOC_per_100k +",
    "log_gdppc_ppp_dol2011 + log_flsp"
  ))

  model.fixed <- plm::plm(
    model_formula,
    data = data,
    index = c("country_name", "year"),
    model = "within"
  )

  return(model.fixed)

}

