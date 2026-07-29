# Diagnostic checks and model-selection history for fit_model().
# Rebuild `data` and `model_formula` the same way fit_model() does before
# running any of this.
#
# ============================================================
# SUMMARY OF FINDINGS (see step-by-step diagnostics below)
# ============================================================
# 1. plm(..., effect="twoways") throws "empty model" — caused by
#    singleton entities/years in an unbalanced panel. Fixed via LSDV
#    (entity FE via within + year FE via factor(year) dummies) rather
#    than plm's native twoways transform.
# 2. Cross-sectional VIFs on the panel (not single-country) are all
#    < 3 — the multicollinearity initially suspected was actually an
#    artifact of restricting to a single country (Spain), which
#    collapses all cross-sectional variation and leaves GDP,
#    floorspace, and pollutants riding one shared trajectory (VIFs of
#    15-27 in that restricted sample). The full panel does not have
#    this problem.
# 3. Full year dummies (factor(year)) over-absorb common trend that
#    overlaps with the pollutant decline across countries (year
#    coefficients are almost perfectly linear, ~-0.045/year), washing
#    out PM2.5 and VOC coefficients almost entirely and leaving only
#    NOx significant.
# 4. Comparing no trend control / linear trend / spline(df=4) / decade
#    dummies / full year dummies: linear, spline, and full-year-FE all
#    converge to similar pollutant coefficients; only the "no trend
#    control" specification disagrees — meaning the "correct-looking"
#    signs from that specification were actually the biased ones
#    (omitted common-trend bias from declining pollution + declining
#    mortality for unrelated reasons: medicine, smoking, diet, etc.).
#    Linear trend was chosen: parsimonious, agrees with more complex
#    alternatives, and extrapolates sensibly to future GCAM scenario
#    years (year dummies do not extrapolate at all).
# 5. Under naive plm SEs, all 5 regressors are significant. Under
#    Arellano country-clustered SEs, only GDP is unambiguously
#    significant (NOx marginal, p=0.085). Under Driscoll-Kraay SEs
#    (robust to both serial correlation AND cross-sectional
#    dependence — more appropriate here, since global shocks like
#    oil prices/recessions/pollution treaties hit many countries at
#    once), NOx and GDP are both clearly significant; PM2.5, VOC, and
#    floorspace are not.
# 6. VOC's coefficient is ~0 and non-significant under every
#    specification and every SE estimator tested, and fails a joint
#    Wald test (p ~ 0.99 individually / p=0.26 jointly with
#    PM2.5+VOC). Dropped from the final model.
# 7. PM2.5 and floorspace remain correctly signed (positive / negative
#    respectively) in the final model but are not individually or
#    jointly significant under Driscoll-Kraay SEs (joint test
#    p=0.35). Retained on theoretical/omitted-variable-bias grounds,
#    but their point estimates should be read as directionally
#    informative only, not precise, for scenario-comparison work.
#
# Final specification:
#   log_HIA_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
#     log_gdppc_ppp_dol2011 + log_flsp + as.numeric(year)
#   plm(model = "within", effect = "individual")
#   inference via plm::vcovSCC(model, type = "HC1", maxlag = 4)

library(plm)
library(car)
library(dplyr)
library(lmtest)

# ------------------------------------------------------------
# 0. Rebuild inputs (mirror fit_model()'s data prep)
# ------------------------------------------------------------
data <- rhap::panel_data %>%
  dplyr::select(iso, country_name, year, pop, dplyr::starts_with("log"),
                continent, dev) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::select(-log_AAP, -log_HDD_value, -log_CDD_value) %>%
  dplyr::filter(stats::complete.cases(.))

model_formula <- log_Deaths_per_100k ~ log_PrimPM25_per_100k +
  log_NOx_per_100k + log_VOC_per_100k + log_gdppc_ppp_dol2011 + log_flsp

# ------------------------------------------------------------
# 1. Panel structure / singleton check
# ------------------------------------------------------------
# Run this BEFORE attempting effect="twoways" — singleton entities or
# years are the most common cause of plm's "empty model" error on an
# unbalanced panel.
check_panel_structure <- function(data) {
  cat("\n---- pdim ----\n")
  print(plm::pdim(data, index = c("country_name", "year")))

  cat("\n---- Countries with only 1 year observed ----\n")
  print(data %>% dplyr::count(country_name) %>% dplyr::filter(n == 1))

  cat("\n---- Years with only 1 country observed ----\n")
  print(data %>% dplyr::count(year) %>% dplyr::filter(n == 1))
}

# If singletons are found:
# data <- data %>%
#   dplyr::add_count(country_name, name = "n_years") %>%
#   dplyr::add_count(year, name = "n_countries") %>%
#   dplyr::filter(n_years > 1, n_countries > 1) %>%
#   dplyr::select(-n_years, -n_countries)

# ------------------------------------------------------------
# 2. LSDV as a robust alternative to effect="twoways"
# ------------------------------------------------------------
fit_lsdv <- function(model_formula, data) {
  formula_lsdv <- update(model_formula, . ~ . + factor(year))
  plm::plm(formula_lsdv, data = data,
           index = c("country_name", "year"),
           model = "within", effect = "individual")
}

# ------------------------------------------------------------
# 3. Multicollinearity check (run on the FULL panel, not a
#    single-country subset — restricting to one country removes
#    cross-sectional variation and inflates VIFs artificially)
# ------------------------------------------------------------
check_multicollinearity <- function(model_formula, data) {
  lm_check <- lm(model_formula, data = data)
  cat("\n---- VIF ----\n")
  print(car::vif(lm_check))

  regressors <- all.vars(model_formula)[-1]
  cat("\n---- Correlation matrix ----\n")
  print(round(cor(data[, regressors], use = "complete.obs"), 2))
}

# ------------------------------------------------------------
# 4. Trend-specification comparison
#    (this is the key diagnostic: compare pollutant coefficients
#    across no-trend / linear / spline / decade / full-year-FE to
#    see whether "theoretically correct" signs are robust or are an
#    artifact of omitted common trend)
# ------------------------------------------------------------
compare_trend_specs <- function(model_formula, data) {
  m_none <- plm::plm(model_formula, data = data,
                     index = c("country_name", "year"),
                     model = "within", effect = "individual")

  formula_linear <- update(model_formula, . ~ . + as.numeric(year))
  m_linear <- plm::plm(formula_linear, data = data,
                       index = c("country_name", "year"),
                       model = "within", effect = "individual")

  formula_spline <- update(model_formula, . ~ . + splines::ns(as.numeric(year), df = 4))
  m_spline <- plm::plm(formula_spline, data = data,
                       index = c("country_name", "year"),
                       model = "within", effect = "individual")

  data_decade <- data %>% dplyr::mutate(decade = paste0(substr(year, 1, 3), "0s"))
  formula_decade <- update(model_formula, . ~ . + factor(decade))
  m_decade <- plm::plm(formula_decade, data = data_decade,
                       index = c("country_name", "year"),
                       model = "within", effect = "individual")

  m_full_year <- fit_lsdv(model_formula, data)

  regressors <- all.vars(model_formula)[-1]
  comparison <- sapply(
    list(none = m_none, linear = m_linear, spline = m_spline,
         decade = m_decade, full_year = m_full_year),
    function(m) coef(m)[regressors]
  )
  print(comparison)

  invisible(list(none = m_none, linear = m_linear, spline = m_spline,
                 decade = m_decade, full_year = m_full_year))
}

# ------------------------------------------------------------
# 5. Inference: naive vs. Arellano-clustered vs. Driscoll-Kraay
# ------------------------------------------------------------
compare_se_estimators <- function(model) {
  cat("\n---- Naive (default plm) SEs ----\n")
  print(summary(model)$coefficients)

  cat("\n---- Arellano country-clustered SEs ----\n")
  print(lmtest::coeftest(model, vcov = plm::vcovHC(model, method = "arellano", cluster = "group")))

  cat("\n---- Driscoll-Kraay SEs (robust to serial correlation AND ----\n")
  cat("---- cross-sectional dependence -- preferred for this panel) ----\n")
  print(lmtest::coeftest(model, vcov = plm::vcovSCC(model, type = "HC1", maxlag = 4)))
}

# ------------------------------------------------------------
# 6. Joint significance test for weak-but-theoretically-motivated
#    regressors (use this instead of relying on individual t-tests
#    when deciding whether to drop a correctly-signed variable)
# ------------------------------------------------------------
joint_test <- function(model, vars) {
  hyp <- paste0(vars, " = 0")
  car::linearHypothesis(model, hyp,
                        vcov = plm::vcovSCC(model, type = "HC1", maxlag = 4))
}

# ------------------------------------------------------------
# 7. Final model
# ------------------------------------------------------------
fit_final_model <- function(data, dep_var = "log_Deaths_per_100k") {
  formula_final <- stats::as.formula(paste(
    dep_var, "~ log_PrimPM25_per_100k + log_NOx_per_100k +",
    "log_gdppc_ppp_dol2011 + log_flsp + as.numeric(year)"
  ))
  model.final <- plm::plm(formula_final, data = data,
                          index = c("country_name", "year"),
                          model = "within", effect = "individual")
  vcov_dk <- plm::vcovSCC(model.final, type = "HC1", maxlag = 4)
  list(model = model.final, vcov_dk = vcov_dk)
}

# ------------------------------------------------------------
# Example run-through
# ------------------------------------------------------------
# check_panel_structure(data)
# check_multicollinearity(model_formula, data)
# trend_models <- compare_trend_specs(model_formula, data)
# compare_se_estimators(trend_models$linear)
#
# model_formula_novoc <- update(model_formula, . ~ . - log_VOC_per_100k + as.numeric(year))
# model.novoc <- plm::plm(model_formula_novoc, data = data,
#                          index = c("country_name", "year"),
#                          model = "within", effect = "individual")
# compare_se_estimators(model.novoc)
# joint_test(model.novoc, c("log_PrimPM25_per_100k", "log_flsp"))
#
# final <- fit_final_model(data)
# lmtest::coeftest(final$model, vcov = final$vcov_dk)
