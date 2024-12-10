library(rhap)
library(testthat)
library(magrittr)

test_that("Download db, create project, and run", {
  # load a reference GCAM db form a Zenodo repository
  db_path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs")
  rpackageutils::download_unpack_zip(
    data_directory = db_path,
    url = "https://zenodo.org/record/13361605/files/database_basexdb_ref.zip?download=1"
  )
  testthat::expect_equal(1, 1)

  db_name <- "database_basexdb_ref"
  prj_name <- "test_prj_v7p1.dat"
  scen_name <- "Reference"

  testOutput <- calc_hap_impacts(db_path = db_path, db_name = db_name,
                                 prj_name = prj_name, scen_name = scen_name,
                                 final_db_year = 2050, HIA_var = "deaths",
                                 saveOutput = T, map = T, anim = F,
                                 normalized = F, by_gr = F)
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/calc_hap_impacts_output1.RData")))
  testthat::expect_equal(testOutput, testResult)

  # check saved files
  testOutput <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/Reference_HAP_deaths.csv"))
  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/Reference_HAP_deaths.csv"))
  testthat::expect(!is.null(testOutput), 'Empty file. Check that the results were correctly produced.')
  testthat::expect_equal(as.data.frame.table(testOutput),
                         as.data.frame.table(testResult))

  # check figures
  testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_Reference_byYear/map_param_2030_PRETTY.png"))
  testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_param_2030_PRETTY.png"))
  testthat::expect_equal(testOutput, testResult)

  testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_Reference_allYears.png"))
  testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_Reference_allYears.png"))
  testthat::expect_equal(testOutput, testResult)

  testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/maps/map_allScen_byYear/map_allScen_2050.png"))
  testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/map_allScen_2050.png"))
  testthat::expect_equal(testOutput, testResult)



})
