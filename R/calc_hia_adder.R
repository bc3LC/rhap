#' calc_hia_adder
#'
#'@description
#' Calculate the adder for HAP impact analysis
#'
#' The adder is calculated as the difference between the observed and estimated values in final data year
#'
#' @source  IHME
#' @keywords Bias adder
#' @importFrom magrittr %>%
#' @export
#' @return Adder

calc_hia_adder <- function(){



datadir <- paste0(getwd(),"/inst/extdata")
# ----------------------------------------------
# Adjust the data
data <- rhap::panel_data %>%
  dplyr::select(iso, country_name, year, pop, dplyr::starts_with("log"), continent, dev) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::select(-log_AAP, -log_HDD_value, -log_CDD_value) %>%
  dplyr::filter(stats::complete.cases(.))

# ----------------------------------------------
# ----------------------------------------------
# FIT THE MODEL
# ----------------------------------------------
fixed_fin <- plm::plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k +
                   log_NOx_per_100k +
                   log_VOC_per_100k +
                   log_gdppc_ppp_dol2011 +
                   log_flsp,
                 data = data,
                 index=c("country_name", "year"), model="within")
summary(fixed_fin)

# ----------------------------------------------
# ----------------------------------------------
# PREDICTIONS
# ----------------------------------------------
data.panel <- plm::pdata.frame(data, index = c("country_name", "year"))

data$pred_log_Deaths_per_100k <- predict(fixed_fin, data.panel)

# Calculate adders
data_adder <- data %>%
  dplyr::select(iso, country_name, year, pop, log_Deaths_per_100k, pred_log_Deaths_per_100k) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::mutate(Deaths_per_100k = exp(log_Deaths_per_100k),
         pred_Deaths_per_100k = exp(pred_log_Deaths_per_100k)) %>%
  dplyr::mutate(bias.adder = Deaths_per_100k - pred_Deaths_per_100k) %>%
  dplyr::select(iso, country = country_name, bias.adder) %>%
  dplyr::mutate(HIA = "deaths")

# Repeat the process for YLL and DALYs

# 1- YLLs
fixed_fin_yll <- plm::plm(log_YLL_per_100k ~ log_PrimPM25_per_100k +
                        log_NOx_per_100k +
                        log_VOC_per_100k +
                        log_gdppc_ppp_dol2011 +
                        log_flsp,
                      data = data,
                      index=c("country_name", "year"), model="within")
summary(fixed_fin_yll)

data.panel_yll <- plm::pdata.frame(data, index = c("country_name", "year"))

data$pred_log_YLL_per_100k <- predict(fixed_fin_yll, data.panel_yll)

data_adder_yll <- data %>%
  dplyr::select(iso, country_name, year, pop, log_YLL_per_100k, pred_log_YLL_per_100k) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::mutate(YLL_per_100k = exp(log_YLL_per_100k),
                pred_YLL_per_100k = exp(pred_log_YLL_per_100k)) %>%
  dplyr::mutate(bias.adder = YLL_per_100k - pred_YLL_per_100k) %>%
  dplyr::select(iso, country = country_name, bias.adder) %>%
  dplyr::mutate(HIA = "yll")

# 2- DALYs
fixed_fin_daly <- plm::plm(log_DALY_per_100k ~ log_PrimPM25_per_100k +
                            log_NOx_per_100k +
                            log_VOC_per_100k +
                            log_gdppc_ppp_dol2011 +
                            log_flsp,
                          data = data,
                          index=c("country_name", "year"), model="within")
summary(fixed_fin_daly)

data.panel_daly <- plm::pdata.frame(data, index = c("country_name", "year"))

data$pred_log_DALY_per_100k <- predict(fixed_fin_daly, data.panel_daly)


data_adder_daly <- data %>%
  dplyr::select(iso, country_name, year, pop, log_DALY_per_100k, pred_log_DALY_per_100k) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::mutate(DALY_per_100k = exp(log_DALY_per_100k),
                pred_DALY_per_100k = exp(pred_log_DALY_per_100k)) %>%
  dplyr::mutate(bias.adder = DALY_per_100k - pred_DALY_per_100k) %>%
  dplyr::select(iso, country = country_name, bias.adder) %>%
  dplyr::mutate(HIA = "dalys")

#---

# add all metrics
data_adder_fin <- dplyr::bind_rows(
  data_adder,
  data_adder_yll,
  data_adder_daly
)

invisible(data_adder_fin)

}
