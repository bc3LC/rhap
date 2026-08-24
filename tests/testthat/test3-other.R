library(rhap)
library(testthat)
library(magrittr)

test_that("get_gdp_ctry", {
  testOutput <- get_gdp_ctry(ssp = "SSP3")
  testResult <- load_snapshot(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/get_gdp_ctry.RData"))
  testthat::expect_equal(testOutput, testResult)

  # error messages
  expect_error(
    get_gdp_ctry(ssp = "SSP15"),
    "Error: The specified SSP 'SSP15' is invalid. Accepted SSPs are: SSP1, SSP2, SSP3, SSP4, SSP5. Please rerun the `get_pop_ctry` function with a valid SSP."
  )
})



test_that("get_pop_ctry", {
  testOutput <- get_pop_ctry(ssp = "SSP5")
  testResult <- load_snapshot(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/get_pop_ctry.RData"))
  testthat::expect_equal(testOutput, testResult)

  # error messages
  expect_error(
    get_pop_ctry(ssp = "ssp3"),
    "Error: The specified SSP 'ssp3' is invalid. Accepted SSPs are: SSP1, SSP2, SSP3, SSP4, SSP5. Please rerun the `get_pop_ctry` function with a valid SSP."
  )
})


test_that("create_panel", {
  testOutput <- create_panel()
  testResult <- load_snapshot(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/create_panel.RData"))
  testthat::expect_equal(testOutput, testResult)
})


test_that("fit_model", {
  # A full-object snapshot comparison (the previous version of this test)
  # is the wrong tool here: fit_model() returns a plm model object plus a
  # Driscoll-Kraay vcov matrix, both of which are brittle to compare
  # exactly across R/plm versions or minor upstream data refreshes. It's
  # also how this test silently stopped checking anything at all: the old
  # snapshot file stored its object under the name "testOutput", which
  # collided with the local variable holding the freshly-computed result
  # -- load()'s side effect overwrote it before expect_equal() ever ran,
  # so the test was comparing the stale snapshot to itself regardless of
  # what fit_model() actually returned. Structural and sign checks below
  # are more robust and substantively more informative.
  result <- fit_model(HIA_var = "deaths")

  expect_named(result, c("model.fixed", "predictable_regions", "vcov_dk", "bias_adder", "HIA_var"))
  expect_s3_class(result$model.fixed, "plm")
  expect_true(is.character(result$predictable_regions))
  expect_true(length(result$predictable_regions) > 0)
  expect_true(is.matrix(result$vcov_dk))
  expect_equal(result$HIA_var, "deaths")
  expect_named(result$bias_adder, c("country_name", "bias.adder", "reliability_ratio"))
  expect_true(all(result$bias_adder$reliability_ratio >= 0))

  # Coefficient signs must match the epidemiological/economic prior the
  # model was built to satisfy: more emissions -> more health impacts,
  # more GDP/floorspace -> fewer health impacts (see fit_model.R's roxygen
  # notes and the model-validation vignette for why this specification
  # was chosen over alternatives, e.g. the rejected GDP x PM2.5
  # interaction, which violated this exact check).
  coefs <- stats::coef(result$model.fixed)
  expect_gt(coefs[["log_PrimPM25_per_100k"]], 0)
  expect_gt(coefs[["log_NOx_per_100k"]], 0)
  expect_lt(coefs[["log_gdppc_ppp_dol2011"]], 0)
  expect_lt(coefs[["log_flsp"]], 0)

  # n_years controls the bias_adder calibration window; a shorter window
  # should still return one bias.adder per predictable country, just
  # calibrated over fewer years.
  result_n3 <- fit_model(HIA_var = "deaths", n_years = 3)
  expect_equal(nrow(result_n3$bias_adder), nrow(result$bias_adder))

  # error messages
  expect_error(
    fit_model(HIA_var = "Death"),
    "Error: The specified HIA_var 'Death' is invalid. Accepted HIA_var are: deaths, yll, dalys. Please rerun the `fit_model` function with a valid HIA_var."
  )
})
