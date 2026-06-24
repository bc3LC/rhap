library(rhap)
library(testthat)
library(magrittr)

test_that("get_gdp_ctry", {
  testOutput <- get_gdp_ctry(ssp = "SSP3")
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/get_gdp_ctry.RData")))
  testthat::expect_equal(testOutput, testResult)

  # error messages
  expect_error(
    get_gdp_ctry(ssp = "SSP15"),
    "Error: The specified SSP 'SSP15' is invalid. Accepted SSPs are: SSP1, SSP2, SSP3, SSP4, SSP5. Please rerun the `get_pop_ctry` function with a valid SSP."
  )
})



test_that("get_pop_ctry", {
  testOutput <- get_pop_ctry(ssp = "SSP5")
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/get_pop_ctry.RData")))
  testthat::expect_equal(testOutput, testResult)

  # error messages
  expect_error(
    get_pop_ctry(ssp = "ssp3"),
    "Error: The specified SSP 'ssp3' is invalid. Accepted SSPs are: SSP1, SSP2, SSP3, SSP4, SSP5. Please rerun the `get_pop_ctry` function with a valid SSP."
  )
})


test_that("create_panel", {
  testOutput <- create_panel()
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/create_panel.RData")))
  testthat::expect_equal(testOutput, testResult)
})


test_that("fit_model", {
  testOutput <- fit_model(HIA_var = "deaths")
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/fit_model.RData")))
  testthat::expect_equal(testOutput, testResult)


  testOutput <- fit_model(HIA_var = "deaths", countries = "Spain")
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/fit_model_esp.RData")))
  testthat::expect_equal(testOutput, testResult)

  # error messages
  expect_error(
    fit_model(HIA_var = "Death"),
    "Error: The specified HIA_var 'Death' is invalid. Accepted HIA_var are: deaths, yll, dalys. Please rerun the `fit_model` function with a valid HIA_var."
  )

  expect_error(
    fit_model(HIA_var = "deaths", countries = "dummy"),
    "Error: The specified country 'dummy' is invalid. Run `unique(rhap::panel_data$country_name)` to see the accepted country names are. Please rerun the `fit_model` function with valid `coutries`.",
    fixed = TRUE
  )

  expect_error(
    fit_model(HIA_var = "deaths", countries = c("dummy","Zambia","Uzbekistan2")),
    "Error: The specified countries 'dummy, Uzbekistan2' are invalid. Run `unique(rhap::panel_data$country_name)` to see the accepted country names are. Please rerun the `fit_model` function with valid `coutries`.",
    fixed = TRUE
  )

})


test_that("listYears", {
  prj_name <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "test_prj_v7p1.dat")
  prj <- rgcam::loadProject(prj_name)

  prj_years <- listYears(prj)
  testthat::expect_equal(prj_years, c(1975, 1990, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055))
})

