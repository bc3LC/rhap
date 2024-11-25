##' run
#'
#'
#' Run the model to estimate the health impacts attributable to household air pollution for GCAM scenarios
#' @keywords health impacts, HAP
#' @return Health impacts attributable to HAP for all the selected years
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param ssp SSP scenario c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param saveOutput Writes the emission files. By default=T
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys"). By default = deaths
#' @importFrom magrittr %>%
#' @export

run <- function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, ssp = "SSP2",
                               scen_name, queries = "queries_rhap.xml", final_db_year = 2100,
                               saveOutput = T, map = F, anim = T, HIA_var = "deaths") {

  rlang::inform('Running rhap ...')

  # First: Get model to substract coefficients
  model.fixed <- fit_model(HIA_var = HIA_var)

  # Then, load the rgcam project if prj not passed as a parameter:
    if (!is.null(db_path) & !is.null(db_name)) {
      rlang::inform('Creating project ...')
      conn <- rgcam::localDBConn(db_path,
                                 db_name,migabble = FALSE)
      prj <- rgcam::addScenario(conn,
                                prj_name,
                                scen_name,
                                paste0(query_path,"/",queries),
                                saveProj = T)


  } else {

    rlang::inform('Loading project ...')
    prj <- rgcam::loadProject(prj_name)

  }

  #-----
  # Extract data from gcam secenario outpus

  #  1- Emissions
  em <- rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    # filter direct emissions from the residential sector
    dplyr::filter(grepl("resid", sector)) %>%
    # separate groups and sectors
    tidyr::separate(sector, c("sector", "group"), sep = "_") %>%
    # rename and aggregate sectors
    dplyr::group_by(scenario, region, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # adjust pollutants
    tidyr::separate(ghg, c("ghg", "adj"), sep = "_", fill = "right") %>%
    dplyr::select(-adj) %>%
    # add-up BC and OC into primary PM2.5 (PrimPM25)
    #dplyr::mutate(ghg = dplyr::if_else(ghg == "BC" | ghg == "OC", "PrimPM25", ghg)) %>%
    # sum pollutants after adjustments
    dplyr::group_by(scenario, region, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # filters the pollutants that will be used in the econometric analysis
    dplyr::filter(ghg %in% panel_pollutants,
                  year <= final_db_year)

  # The estimation is at country level, so need downscale emissions from GCAM_region to country level
  # First, calculate the share of the gas-specific emissions in each GCAM_region
  em_shares_gr <- em %>%
    dplyr::group_by(scenario, region, year, ghg, Units) %>%
    dplyr::mutate(value_reg = sum(value)) %>%
    dplyr::ungroup() %>%
    mutate(dec_share = value / value_reg) %>%
    select(scenario, region, year, group, ghg, dec_share)

  # Aggregate region-level emissions to downscale
  em_reg <- em %>%
    dplyr::group_by(scenario, region, year, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  # Load data for downscaling (percentages)
  em_ctry <- Percen %>%
    dplyr::mutate(Pollutant = dplyr::if_else(Pollutant == "POM", "OC", Pollutant),
                  year = as.numeric(as.character(year))) %>%
    dplyr::rename(region = `GCAM Region`,
                  ghg = Pollutant,
                  country = Country,
                  iso3 = `ISO 3`) %>%
    dplyr::filter(ghg %in% panel_pollutants,
                  year <= final_db_year) %>%
    #gcamdata::left_join_error_no_match(em_reg, by = c("region", "ghg", "year"))
    left_join(em_reg, by = c("region", "ghg", "year"))




  #  2- Floorspace
  # flsp

  #  3- Per capita GDP (gdppc)
  # Get SSP-specific data
  gdp_pc<-get(paste0('gdp_pc.',ssp))

  #-----

}
