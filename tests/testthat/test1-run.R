library(rhap)
library(testthat)
library(magrittr)

test_that("Download db, create project, and run", {
  # load a reference GCAM 7.1 project
  prj_name <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "test_prj_v7p1.dat")
  scen_name <- "Reference"

  testOutput <- calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                                 final_db_year = 2050, HIA_var = "deaths",
                                 saveOutput = T, map = T, anim = F,
                                 normalized = F, by_gr = F)
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/calc_hap_impacts_output1.RData")))
  testthat::expect_equal(testOutput, testResult)

  # check saved files

  # AUX functions to deal with ASCII characters
  decode_ascii <- function(text) {

    escaped_text <- htmltools::htmlEscape(text)
    xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))

  }

  testOutput <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/Reference_HAP_deaths.csv"),
                         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/Reference_HAP_deaths.csv"),
                         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  testOutput$country <- sapply(testOutput$country, decode_ascii)
  testResult$country <- sapply(testResult$country, decode_ascii)
  testthat::expect(!is.null(testOutput), 'Empty file. Check that the results were correctly produced.')
  testthat::expect_equal(as.data.frame.table(testOutput), as.data.frame.table(testResult))

  # # check figures
  # to be run locally, avoid running it in github actions
  # testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_Reference_byYear/map_param_2030_PRETTY.png"))
  # testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_param_2030_PRETTY.png"))
  # testthat::expect_equal(testOutput, testResult)
  #
  # testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_Reference_allYears.png"))
  # testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_Reference_allYears.png"))
  # testthat::expect_equal(testOutput, testResult)
  #
  # testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_allScen_byYear/map_allScen_2050.png"))
  # testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_allScen_2050.png"))
  # testthat::expect_equal(testOutput, testResult)


  # error messages
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "death",
                     saveOutput = T, map = T, anim = F,
                     normalized = F, by_gr = F),
    "Error: The specified HIA_var 'death' is invalid. Accepted HIA_var are: deaths, yll, dalys. Please rerun the calc_hap_impacts function with a valid HIA_var value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2063, HIA_var = "deaths",
                     saveOutput = T, map = T, anim = F,
                     normalized = F, by_gr = F),
    "Error: The specified final_db_year '2063' is invalid. Accepted final_db_year are: 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100. Please rerun the calc_hap_impacts function with a valid final_db_year value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "deaths",
                     saveOutput = T, map = T, anim = 'true',
                     normalized = F, by_gr = F),
    "Error: The specified anim 'true' is invalid. Accepted anim values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid anim value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "deaths",
                     saveOutput = T, map = 'false', anim = F,
                     normalized = F, by_gr = F),
    "Error: The specified map 'false' is invalid. Accepted map values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid map value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "deaths",
                     saveOutput = 'T', map = T, anim = F,
                     normalized = F, by_gr = F),
    "Error: The specified saveOutput 'T' is invalid. Accepted saveOutput values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid saveOutput value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "deaths",
                     saveOutput = T, map = F, anim = F,
                     normalized = 'no', by_gr = F),
    "Error: The specified normalized 'no' is invalid. Accepted normalized values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid normalized value."
  )
  expect_error(
    calc_hap_impacts(prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050, HIA_var = "deaths",
                     saveOutput = T, map = F, anim = F,
                     normalized = T, by_gr = 0),
    "Error: The specified by_gr '0' is invalid. Accepted by_gr values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid by_gr value."
  )

})
