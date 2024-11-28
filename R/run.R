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
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param saveOutput Writes the emission files. By default=T
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys"). By default = deaths
#' @importFrom magrittr %>%
#' @export

run <- function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                               scen_name, queries = "queries_rhap.xml", final_db_year = 2100,
                               saveOutput = T, map = F, anim = T, HIA_var = "deaths") {

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

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
  # EXTRACT DATA FROM GCAM SCENARIO OUTPUTS

  # First, create a database to transform from GCAM_region to country-level:
  reg_to_ctry <- rhap::Percen %>%
    dplyr::select(region = `GCAM Region`, country = Country) %>%
    dplyr::distinct() %>%
    dplyr::mutate(country = stringr::str_to_title(country))

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
    dplyr::mutate(dec_share = value / value_reg) %>%
    dplyr::select(scenario, region, year, group, ghg, dec_share) %>%
    dplyr::left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
    dplyr::select(scenario, country, year, group, ghg, dec_share)

  # Aggregate region-level emissions to downscale
  em_reg <- em %>%
    dplyr::group_by(scenario, region, year, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()


  # Load data for downscaling (percentages)
  em_ctry_gr <- Percen %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Pollutant = dplyr::if_else(Pollutant == "POM", "OC", Pollutant),
                  year = as.numeric(as.character(year))) %>%
    dplyr::rename(region = `GCAM Region`,
                  ghg = Pollutant,
                  country = Country,
                  iso3 = `ISO 3`) %>%
    dplyr::filter(ghg %in% panel_pollutants,
                  year <= final_db_year) %>%
    # TODO add for more scenarios!!!
    gcamdata::left_join_error_no_match(em_reg, by = c("region", "ghg", "year")) %>%
    dplyr::mutate(em_ctry = Percentage * value) %>%
    dplyr::mutate(iso = tolower(iso3)) %>%
    dplyr::mutate(country = stringr::str_to_title(country)) %>%
    dplyr::select(scenario, country, iso, ghg, year, unit = Units, em_ctry) %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(ssp = "SSP2",
                  ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
                  ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
                  ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
                  ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)) %>%
    # Use shares to calculate emissions by household group (e.g., decile)
    gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
    gcamdata::left_join_error_no_match(em_shares_gr, by = c("scenario", "country", "ghg", "year", "group")) %>%
    dplyr::mutate(em_ctry_gr = em_ctry * dec_share) %>%
    dplyr::select(scenario, ssp, country, ghg, group, year, em_ctry_gr)




  #  2- Floorspace
  # First, extract population data
  pop_gr <- rgcam::getQuery(prj, "subregional population") %>%
    dplyr::filter(grepl("resid", `gcam-consumer`)) %>%
    dplyr::mutate(group = gsub("resid_", "", `gcam-consumer`),
                  pop = value * 1E3) %>%
    dplyr::select(scenario, region, year, group, pop)

  # Extract floorspace data and combine it with subregional population to compute per capita floorspace
  flsp_pc <- rgcam::getQuery(prj, "building floorspace") %>%
    dplyr::filter(grepl("resid", building)) %>%
    dplyr::mutate(group = gsub("resid_", "", building),
                  flsp_m2 = value * 1E9) %>%
    dplyr::select(scenario, region, year, group, flsp_m2) %>%
    gcamdata::left_join_error_no_match(pop_gr, by = c("scenario", "region", "year", "group")) %>%
    dplyr::mutate(flsp_pc_gr = flsp_m2 / pop,
                  unit = "m2/pers") %>%
    dplyr::select(scenario, region, year, group, unit, flsp_pc_gr)


  # Assume that al countries within the region have similar flps_pc
  flsp_pc_ctry_gr <- flsp_pc %>%
    left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(ssp = "SSP2",
                  ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
                  ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
                  ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
                  ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)) %>%
    dplyr::select(scenario, ssp, country, group, year, flsp_pc_gr)


  #  3- Per capita GDP (gdppc) by group

  # Get country-level SSP-specific data
  gdp_ctry <- dplyr::bind_rows(
    get(paste0('gdp_ctry.',"SSP1")),
    get(paste0('gdp_ctry.',"SSP2")),
    get(paste0('gdp_ctry.',"SSP3")),
    get(paste0('gdp_ctry.',"SSP4")),
    get(paste0('gdp_ctry.',"SSP5")),
  ) %>%
    rename(ssp = scenario,
           country = region)

  pop_ctry <- dplyr::bind_rows(
    get(paste0('pop_ctry.',"SSP1")),
    get(paste0('pop_ctry.',"SSP2")),
    get(paste0('pop_ctry.',"SSP3")),
    get(paste0('pop_ctry.',"SSP4")),
    get(paste0('pop_ctry.',"SSP5")),
  ) %>%
    rename(ssp = scenario,
           country = region)

  # Process Population: Population is evenly distributed across groups, but could be updated
  pop_share <- rgcam::getQuery(prj, "subregional population") %>%
    dplyr::filter(grepl("resid", `gcam-consumer`)) %>%
    tidyr::separate(`gcam-consumer`, c("sector", "group"), sep = "_") %>%
    # rename and aggregate sectors
    dplyr::group_by(scenario, region, year) %>%
    dplyr::mutate(value_agg = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share_pop = round(value / value_agg, 1)) %>%
    dplyr::select(scenario, region, year, group, share_pop) %>%
    # Temp fix for Taiwan
    dplyr::mutate(share_pop = dplyr::if_else(region == "Taiwan", 0.1, share_pop)) %>%
    # expand shares to countries
    left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(ssp = "SSP2",
                  ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
                  ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
                  ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
                  ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)) %>%
    dplyr::select(scenario, ssp, country, year, group, share_pop) %>%
    dplyr::mutate(year = as.character(year))

  pop_ctry_gr <- pop_ctry %>%
    # filter only ssps used
    dplyr::filter(ssp %in% unique(pop_share$ssp)) %>%
    # add groups
    gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
    # adjust country names to match
    dplyr::left_join(adj_ctry, by = "country") %>%
    dplyr::mutate(country = dplyr::if_else(is.na(adj_country), country, adj_country)) %>%
    dplyr::select(-adj_country) %>%
    # filter out Mayotte and Curasao
    dplyr::filter(country %!in% c("Mayotte", "Curasao")) %>%
    gcamdata::left_join_error_no_match(pop_share, by = c("ssp", "country", "year", "group")) %>%
    # compute group-level population
    dplyr::mutate(pop_gr = pop * share_pop) %>%
    dplyr::select(scenario, ssp, country, year, group, pop_gr)

  # Process GDP: First need to calculate the income shares by GCAM_region and period (to be applied to all countries within each region)
  gdp_share <- rgcam::getQuery(prj, "subregional income") %>%
    dplyr::filter(grepl("resid", `gcam-consumer`)) %>%
    tidyr::separate(`gcam-consumer`, c("sector", "group"), sep = "_") %>%
    dplyr::mutate(gdp_pc = value * 1E3) %>%
    dplyr::select(-value, -Units) %>%
    gcamdata::left_join_error_no_match(
      rgcam::getQuery(prj, "subregional population") %>%
        dplyr::filter(grepl("resid", `gcam-consumer`)) %>%
        tidyr::separate(`gcam-consumer`, c("sector", "group"), sep = "_"),
          by = c("scenario", "sector", "group", "region", "year")) %>%
    dplyr::mutate(gdp = gdp_pc * (value * 1E3)) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::mutate(gdp_agg = sum(gdp)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share_gdp = round(gdp / gdp_agg, 5)) %>%
    dplyr::select(scenario, region, year, group, share_gdp) %>%
  # expand shares to countries
    dplyr::left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(ssp = "SSP2",
                  ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
                  ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
                  ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
                  ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)) %>%
    dplyr::select(scenario, ssp, country, year, group, share_gdp) %>%
    dplyr::mutate(year = as.character(year))

  # Adjust Taiwan using China's shares
  gdp_share_twn <- gdp_share %>%
    dplyr::filter(country == "China") %>%
    dplyr::mutate(country = "Taiwan")

  gdp_share <- gdp_share %>%
    dplyr::filter(country != "Taiwan") %>%
    dplyr::bind_rows(gdp_share_twn)


  gdp_ctry_gr <- gdp_ctry %>%
    # filter only ssps used
    dplyr::filter(ssp %in% unique(gdp_share$ssp)) %>%
    # add groups
    gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
    # adjust country names to match
    dplyr::left_join(adj_ctry, by = "country") %>%
    dplyr::mutate(country = dplyr::if_else(is.na(adj_country), country, adj_country)) %>%
    dplyr::select(-adj_country) %>%
    # filter out Mayotte and Curasao
    dplyr::filter(country %!in% c("Mayotte", "Curasao")) %>%
    gcamdata::left_join_error_no_match(gdp_share, by = c("ssp", "country", "year", "group")) %>%
    # compute group-level population
    dplyr::mutate(gdp_dol2011_ppp_gr = gdp_dol2011_ppp * share_gdp) %>%
    dplyr::select(scenario, ssp, country, year, group, gdp_dol2011_ppp_gr)


  # Combine processed datasets to get per capita GDP
  gdp_pc_ctry_gr <- gdp_ctry_gr %>%
    gcamdata::left_join_error_no_match(pop_ctry_gr, by = c("scenario", "ssp", "country", "year", "group")) %>%
    dplyr::mutate(gdp_pc_dol2011_ppp_gr = gdp_dol2011_ppp_gr / pop_gr) %>%
    dplyr::select(scenario, ssp, country, year, group, gdp_pc_dol2011_ppp_gr)

  #-----
  # COMBINE THE DATA AND TRANSFORM IT TO MODEL VARIABLES

   output <- em_ctry_gr %>%
    gcamdata::left_join_error_no_match(flsp_pc_ctry, by = c("scenario", "ssp", "country", "group", "year")) %>%
    dplyr::filter(year >= max_base_year) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::left_join(gdp_pc_ctry_gr)
    #gcamdata::left_join_error_no_match(gdp_pc_ctry_gr)









}
