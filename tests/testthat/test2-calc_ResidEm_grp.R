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

  testOutput <- calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                                 prj_name = prj_name, scen_name = scen_name,
                                 final_db_year = 2050,
                                 year = 2030,
                                 pollutant = 'CH4',
                                 region = 'EU-15',
                                 pie = T)

  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/calc_ResidEm_grp_output_eu15.RData")))
  testthat::expect_equal(testOutput, testResult)

  testOutput <- calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                                 prj_name = prj_name, scen_name = scen_name,
                                 final_db_year = 2050,
                                 year = 2030,
                                 pollutant = 'CH4',
                                 region = 'EU-27',
                                 pie = T)

  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/calc_ResidEm_grp_output_eu27.RData")))
  testthat::expect_equal(testOutput, testResult)


  # check figures
  testOutput <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/ResidEm_grp/pie_charts/ResidEm_grp_EU-15_2030_CH4.png"))
  testResult <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/ResidEm_grp_EU-15_2030_CH4.png"))
  testthat::expect_equal(testOutput, testResult)

  testOutput <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "output/ResidEm_grp/ResidEm_grp_EU-27_2030_CH4.csv"))
  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/ResidEm_grp_EU-27_2030_CH4.csv"))
  testthat::expect(!is.null(testOutput), 'Empty file. Check that the results were correctly produced.')
  testthat::expect_equal(as.data.frame.table(testOutput),
                         as.data.frame.table(testResult))


  # error messages
  expect_error(
    calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                     prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050,
                     year = 2030,
                     pollutant = 'ch4',
                     region = 'EU-15',
                     pie = T),
    "Error: The specified pollutant 'ch4' is invalid. Accepted pollutants are: BC, CH4, CO, HFC125, HFC134a, HFC143a, HFC23, HFC32, N2O, NH3, NMVOC, NOx, OC, SO2. Please rerun the `calc_ResidEm_grp` function with a valid pollutant."
  )

  expect_error(
    calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                     prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050,
                     year = 2030,
                     pollutant = 'CH4',
                     region = 'EU-13',
                     pie = T),
    "Error: The specified region 'EU-13' is invalid. Accepted regions are: Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Argentina, Australia_NZ, Brazil, Canada, Central America and Caribbean, Central Asia, China, Colombia, EU-12, EU-15, Europe_Eastern, Europe_Non_EU, European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia, South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia, Taiwan, USA, EU-27, Global. Please rerun the `calc_ResidEm_grp` function with a valid region."
  )

  expect_error(
    calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                     prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050,
                     year = 2032,
                     pollutant = 'CH4',
                     region = 'EU-15',
                     pie = T),
    "Error: The specified year '2032' is invalid. Accepted years are: 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100. Please rerun the `calc_ResidEm_grp` function with a valid year."
  )


  expect_error(
    calc_ResidEm_grp(db_path = db_path, db_name = db_name,
                     prj_name = prj_name, scen_name = scen_name,
                     final_db_year = 2050,
                     year = 2055,
                     pollutant = 'CH4',
                     region = 'EU-15',
                     pie = T),
    "Error: The specified year '2055' is invalid. The database only contains data up to 2050. Accepted years are: 2020, 2025, 2030, 2035, 2040, 2045, 2050. Please rerun the `calc_ResidEm_grp` function with a valid year."
  )

})
