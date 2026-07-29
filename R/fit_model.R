#' fit_model
#'
#' @description
#' Fit the model using a fixed effects model (plm, entity within-estimator)
#' with a linear year trend, and return Driscoll-Kraay standard errors
#' alongside the fitted model.
#'
#' Model selection notes (see "./rhap/inst/extdata/model_testing.R" for the
#' full diagnostic history):
#' - Full year fixed effects (factor(year)) over-absorb common trend that
#'   overlaps with the pollutant decline across countries, washing out
#'   pollutant coefficients almost entirely. A linear year trend gives
#'   coefficients consistent with a spline(df=4) and full year-FE
#'   specification, so it was preferred for parsimony and because it
#'   extrapolates sensibly to future scenario years (unlike year dummies).
#' - log_VOC_per_100k was dropped: coefficient is ~0 and non-significant
#'   under every specification and SE estimator tested (naive, Arellano
#'   clustered, Driscoll-Kraay), and does not survive a joint Wald test
#'   either alone or jointly with PM2.5/floorspace.
#' - Under Driscoll-Kraay SEs (robust to both serial correlation and
#'   cross-sectional dependence — appropriate for a country-year panel
#'   with common shocks like oil prices, recessions, and multilateral
#'   pollution treaties), only log_NOx_per_100k and log_gdppc_ppp_dol2011
#'   are individually significant. log_PrimPM25_per_100k and log_flsp are
#'   correctly signed (positive / negative respectively, as expected) but
#'   not individually or jointly significant (joint Wald test p = 0.35).
#'   They are retained on theoretical grounds (omitted-variable-bias risk
#'   from dropping known-relevant regressors) but their point estimates
#'   should be treated as directionally informative only, not precise,
#'   for any scenario-comparison use of this model's coefficients.
#'
#' - The country-level bias adder previously stored in the external
#'   rhap::hia_adder table is now computed internally from the fitted
#'   model itself (see below), so it can never drift out of sync with
#'   the coefficients used to produce it. It's calculated the same way
#'   predict() will later be used on scenario data — coefficients only,
#'   no fixed effect — so the gap it captures is exactly the gap that
#'   needs to be added back at prediction time (country fixed effect +
#'   any residual model bias), calibrated to recent observed years
#'   rather than a single year to avoid baking in a one-off shock.
#'
#' - The bias adder is ADDITIVE (level scale, per_100k units), not
#'   multiplicative, despite log(Y) = X*beta + alpha_i implying alpha_i is
#'   technically a multiplicative scalar exp(alpha_i) in levels. A
#'   multiplicative geometric-mean correction was tried and is more
#'   "correct" in that narrow sense, but performed far worse in practice:
#'   validated against real GBD out-of-sample data, it collapsed
#'   cross-country correlation from ~0.86 (additive) to ~0.18. The reason
#'   is that ~22% of countries (e.g. Uganda) have a common linear year
#'   trend that doesn't match their own trajectory, so the gap between
#'   the trend-implied (fixed-effect-free) prediction and reality keeps
#'   growing even within the training years -- sometimes to a fixed
#'   effect worth 1000x+ in levels. A multiplicative correction amplifies
#'   that growing gap catastrophically when applied to a slightly
#'   different (scenario) year; an additive correction, bounded by the
#'   actual scale of the outcome variable, does not. See
#'   inst/extdata/model_testing.R for the comparison.
#'
#' - IMPORTANT: the linear year trend is passed as an explicit numeric
#'   column (year_num), not as.numeric(year) inline in the formula.
#'   plm::pdata.frame() (used downstream at prediction time) converts
#'   the "year" index column into a factor; as.numeric() on a factor
#'   returns the factor's level codes, not the original calendar year,
#'   and those codes are reassigned per-dataset — so a scenario dataset
#'   spanning different years than the training data would get a
#'   trend encoding disconnected from the one the coefficient was
#'   estimated against. year_num is a plain (non-index) numeric column,
#'   so it isn't touched by pdata.frame()'s factor coercion and stays
#'   on a consistent calendar-year scale in both training and
#'   prediction.
#'

#' @source Details on plm estimation: https://cran.r-project.org/web/packages/plm/plm.pdf
#' @source Driscoll-Kraay SEs: plm::vcovSCC documentation
#' @keywords Econometric model; fixed effects
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys")
#' @param n_years Number of most recent historical years (per country) to
#'   average over when computing the bias adder. Default 5 — long enough
#'   to smooth out a single-year shock, short enough to reflect the
#'   country's current level rather than its full historical average.
#' @importFrom magrittr %>%
#' @export
#' @return A list: model.fixed (plm object), predictable_regions (character
#'   vector of countries in the estimation sample), vcov_dk (Driscoll-Kraay
#'   variance-covariance matrix for inference — use this instead of the
#'   default plm SEs for confidence intervals / significance testing on
#'   this model's coefficients), bias_adder (tibble of country_name,
#'   bias.adder, reliability_ratio — bias.adder is the per-country additive
#'   level-calibration term, on the per_100k scale, to add to future
#'   predictions [do not multiply]; reliability_ratio is
#'   abs(bias.adder)/naive_prediction over the calibration window, where a
#'   high value flags a country whose absolute-level prediction rests
#'   almost entirely on the bias correction rather than the model's
#'   covariates, and should be trusted less; replaces rhap::hia_adder),
#'   HIA_var (the HIA_var this model was fit for, echoed back so a cached
#'   fit_model() result can be validated against a different call site's
#'   HIA_var before reuse).
fit_model <- function(HIA_var, n_years = 5) {
  iso <- country_name <- year <- pop <- continent <- dev <- log_AAP <- value <-
    Model <- Scenario <- Region <- Variable <- Unit <- resid_per_100k <- . <- NULL
  # Check user input
  if (!HIA_var %in% c("deaths", "yll", "dalys")) {
    stop(sprintf(
      "Error: The specified HIA_var '%s' is invalid. Accepted HIA_var are: %s. Please rerun the `fit_model` function with a valid HIA_var.",
      HIA_var, paste(c("deaths", "yll", "dalys"), collapse = ", ")
    ))
  }
  # Adjust the data
  data <- rhap::panel_data %>%
    dplyr::select(iso, country_name, year, pop, dplyr::starts_with("log"), continent, dev) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::select(-log_AAP, -log_HDD_value, -log_CDD_value) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    # Plain numeric copy of year, kept alongside the character "year" used
    # for the panel index. plm::pdata.frame() (used at prediction time)
    # coerces the index "year" column into a factor, and as.numeric() on
    # a factor returns level codes, not the calendar year — those codes
    # are reassigned per-dataset, so a scenario dataset with different
    # years would get a trend encoding disconnected from training. This
    # plain column isn't touched by that coercion, so it's what the
    # trend term should reference.
    dplyr::mutate(year_num = as.numeric(year))
  predictable_regions <- unique(data$country_name)
  # dplyr::select the dependent variable (deaths, YLLs, or DALYs)
  # Create a named list to map HIA_var values to corresponding dep_var values
  HIA_var_map <- list(
    deaths = "log_Deaths_per_100k",
    yll = "log_YLL_per_100k",
    dalys = "log_DALY_per_100k"
  )
  # Assign the value from the named list
  dep_var <- HIA_var_map[[HIA_var]]
  # Fit the fixed effect model
  # Final specification: entity FE + linear year trend, VOC excluded.
  # See roxygen notes above for the diagnostic history behind this choice.
  model_formula <- stats::as.formula(paste(
    dep_var, "~ log_PrimPM25_per_100k + log_NOx_per_100k +",
    "log_gdppc_ppp_dol2011 + log_flsp + year_num"
  ))
  model.fixed <- plm::plm(
    model_formula,
    data = data,
    index = c("country_name", "year"),
    model = "within",
    effect = "individual"
  )
  # Driscoll-Kraay SEs: robust to serial correlation and cross-sectional
  # dependence, appropriate for this country-year panel. Use this vcov
  # for any inference (CIs, significance tests) on model.fixed's
  # coefficients rather than the default plm summary() SEs.
  vcov_dk <- plm::vcovSCC(model.fixed, type = "HC1", maxlag = 4)

  # ------------------------------------------------------------------
  # Bias adder: level-calibration term for out-of-sample prediction.
  # predict() on a plm "within" model returns fitted values from the
  # coefficients alone, without the entity fixed effect (this is also
  # how scenario/future predictions are generated downstream, since
  # scenario data has no fixed effect to draw on). That means a
  # naive prediction will be systematically off by each country's
  # fixed effect plus any remaining model bias. We recover that gap
  # by predicting on the model's own training data and comparing to
  # what was actually observed, averaged over the most recent
  # n_years to avoid anchoring to a single noisy year.
  #
  # This is deliberately ADDITIVE (level scale, per_100k units), not a
  # multiplicative ratio, even though log(Y) = X*beta + alpha_i implies
  # alpha_i is technically a multiplicative scalar exp(alpha_i) in
  # levels. A multiplicative geometric-mean version was tried and tested
  # against real GBD data: it collapsed cross-country correlation from
  # ~0.86 (additive) to ~0.18. Cause: the model's common linear year
  # trend doesn't match every country's own trajectory (e.g. Uganda's
  # mortality declines far more slowly than the trend assumes), so the
  # gap between the trend-implied, fixed-effect-free prediction and
  # reality keeps growing even within the training years -- for ~22% of
  # countries the implied fixed effect exceeds 10x, and for some (Uganda)
  # exceeds 1000x. A multiplicative correction amplifies that growing gap
  # catastrophically the moment it's applied to a slightly different
  # (scenario) year; a bounded additive correction does not. See
  # inst/extdata/model_testing.R for the full comparison.
  #
  # The one real problem with the additive version -- it can produce a
  # negative "corrected" rate for countries with a large negative bias
  # adder -- is handled downstream in calc_hap_impacts() by flooring the
  # corrected value at a small fraction of the naive (uncorrected)
  # prediction, not by silently clamping to exactly 0.
  # ------------------------------------------------------------------
  train_panel <- plm::pdata.frame(data, index = c("country_name", "year"))
  data$pred_log <- stats::predict(model.fixed, train_panel)
  data$observed_per_100k <- exp(data[[dep_var]])
  data$pred_per_100k <- exp(data$pred_log)
  data$resid_per_100k <- data$observed_per_100k - data$pred_per_100k

  # reliability_ratio: |bias.adder| relative to the naive (uncorrected)
  # prediction it's being added to, both averaged over the same n_years
  # calibration window. A ratio near 0 means the model's own covariates
  # already explain most of the country's level, so the correction (and
  # by extension the country fixed effect it stands in for) is small
  # relative to the prediction -- high confidence. A ratio >> 1 means the
  # correction dwarfs the naive prediction (e.g. Uganda, where it's
  # ~100x), meaning the model's covariates explain almost none of that
  # country's absolute level and the fixed effect is doing all the work
  # -- low confidence in the absolute-level prediction, even though the
  # correction keeps it numerically well-behaved (see additive-vs-
  # multiplicative note above).
  bias_adder <- data %>%
    dplyr::group_by(country_name) %>%
    dplyr::filter(year_num %in% utils::tail(sort(unique(year_num)), n_years)) %>%
    dplyr::summarise(
      bias.adder = mean(resid_per_100k, na.rm = TRUE),
      naive_pred_per_100k = mean(pred_per_100k, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(reliability_ratio = round(abs(bias.adder) / naive_pred_per_100k, 3)) %>%
    dplyr::select(country_name, bias.adder, reliability_ratio)

  return(list(
    model.fixed = model.fixed,
    predictable_regions = predictable_regions,
    vcov_dk = vcov_dk,
    bias_adder = bias_adder,
    HIA_var = HIA_var
  ))
}
# KEEPING PRIMARY PM25 AND FLOORSPACE:

# Omitted-variable-bias risk cuts one way. Dropping a theoretically-grounded, correctly-signed regressor because it's
# imprecisely estimated risks biasing the coefficients you do care about (NOx, GDP) if PM2.5 or floorspace has any real,
# non-zero relationship with mortality that's correlated with the retained regressors — which is plausible here,
# since PM2.5 in particular is one of the best-established health-relevant pollutants in the epidemiological literature.
# Keeping them costs you two degrees of freedom; wrongly dropping them could cost you unbiasedness elsewhere.

# "Not significant" isn't "zero" — it's "we can't pin down the size precisely with this data," which is a power problem
# stemming from PM2.5's decline being unusually correlated with the common trend across countries, not evidence the effect
# doesn't exist. The point estimates stayed correctly signed and stable across every specification
# tried (linear, spline, decade, full FE), which is a different and stronger form of evidence than a single t-test.
