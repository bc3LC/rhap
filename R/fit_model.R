#' fit_model
#'
#'@description
#' Fit the model using fixed effects model and plm function.
#'
#' For the model selection, testing and validation see "./rhap/inst/extdata/model_testing.R"
#'
#' @source  Details on plm estimation can be found here: https://cran.r-project.org/web/packages/plm/plm.pdf#'
#' @keywords Econometric model; fixed effects
#' @importFrom magrittr %>%
#' @export
#' @return Regression model and coefficients for prediction

fit_model <- function() {

  # Adjust the data
  data <- create_panel() %>%
    select(iso, country_name, year, pop, starts_with("log"), continent, dev) %>%
    select(-log_AAP) %>%
    filter(complete.cases(.))

  # Fit the fixed effect model
  model.fixed <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k +
        log_NOx_per_100k +
        log_VOC_per_100k +
        log_gdppc_ppp_dol2011 +
        log_flsp,
      data = data,
      index = c("country_name", "year"), model = "within")

  return(model.fixed)

}

