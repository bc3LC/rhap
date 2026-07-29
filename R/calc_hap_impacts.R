##' calc_hap_impacts
#'
#'
#' Run the model to estimate the health impacts attributable to household air pollution for GCAM scenarios
#' @keywords health impacts, HAP
#' @return Health impacts attributable to HAP for all the selected years, as
#'   a tibble of scenario, country, year, pred_var, pred_value,
#'   pred_value_normalized, reliability_ratio, and reliability. The last two
#'   flag how much of a country's predicted level rests on fit_model()'s
#'   bias-adder correction rather than on the regression's own covariates
#'   (reliability_ratio = abs(bias.adder)/naive prediction; reliability is a
#'   "high"/"medium"/"low" label over that ratio, thresholds 0.5 and 2) --
#'   "low" flags a country whose absolute-level prediction the model has
#'   little independent basis for, not necessarily a wrong number.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param saveOutput Writes the emission files. By default=TRUE
#' @param map Produce the maps. By default = FALSE
#' @param anim If set to TRUE, produces multi-year animations. By default=TRUE
#' @param HIA_var Health metric to be predicted. c("deaths", "yll", "dalys"). By default = deaths
#' @param normalized Transform the output to "normalized" values. By default = FALSE
#' @param by_gr Report within-country income-decile group shares of
#'   population, PM2.5+NOx exposure, and GDP as a CSV table plus a pie-chart
#'   snapshot at final_db_year (output/by_gr/). Descriptive only -- it does
#'   not estimate group-level health impacts, since the regression model is
#'   fit on between-country variation and was never validated against
#'   within-country/sub-national variation. By default = FALSE
#' @param fit_result Optional pre-computed result from fit_model(HIA_var),
#'   e.g. from a previous call, to reuse instead of refitting the national
#'   regression from scratch. fit_model() doesn't depend on GCAM scenario
#'   data at all, so refitting it (including its Driscoll-Kraay vcov, which
#'   isn't cheap) on every calc_hap_impacts() call is pure waste when
#'   comparing many scenarios for the same HIA_var. Must have been fit with
#'   the same HIA_var as this call; a mismatch raises an error rather than
#'   silently using the wrong model. By default = NULL (fits internally).
#' @importFrom magrittr %>%
#' @export

calc_hap_impacts <- function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                             scen_name, queries = "queries_rhap.xml", final_db_year = 2100,
                             saveOutput = TRUE, map = FALSE, anim = TRUE, HIA_var = "deaths",
                             normalized = FALSE, by_gr = FALSE, fit_result = NULL) {
  Country <- country <- sector <- scenario <- region <- year <- group <- ghg <-
    Units <- value <- adj <- value_reg <- dec_share <- Pollutant <- `ISO 3` <-
    Percentatge <- iso3 <- iso <- ssp <- `gcam-consumer` <- value_adj <- building <-
    unit <- flsp_m2 <- Model <- Scenario <- Region <- Variable <- Unit <- gdp <-
    value_agg <- share_pop <- gdp_pc <- gdp_agg <- share_gdp <- gdp_dol2011_ppp <-
    gdp_dol2011_ppp_gr <- data_name <- country_name <- bias.adder <- pred_value <-
    pred_var <- pred_value_per_100K <- pred_value_per_100K_adj <- HIA <-
    PrimPM25 <- NOx <- NMVOC <- OC <- BC <- PrimPM25_per_100k <- NOx_per_100k <-
    VOC_per_100k <- `GCAM Region` <- Percentage <- adj_country <- country_map <-
    gdp_pc_dol2011_ppp <- mapCountries <- pop <-
    metric <- share <- reliability_ratio <- reliability <- . <- NULL

  # Check user input
  if (!HIA_var %in% c("deaths", "yll", "dalys")) {
    stop(sprintf(
      "Error: The specified HIA_var '%s' is invalid. Accepted HIA_var are: %s. Please rerun the calc_hap_impacts function with a valid HIA_var value.",
      HIA_var, paste(c("deaths", "yll", "dalys"), collapse = ", ")
    ))
  }
  final_db_year <- as.numeric(as.character(final_db_year))
  if (!final_db_year %in% seq(2015, 2100, 5)) {
    stop(sprintf(
      "Error: The specified final_db_year '%s' is invalid. Accepted final_db_year are: %s. Please rerun the calc_hap_impacts function with a valid final_db_year value.",
      final_db_year, paste(seq(2015, 2100, 5), collapse = ", ")
    ))
  }
  if (!is.logical(saveOutput)) {
    stop(sprintf(
      "Error: The specified saveOutput '%s' is invalid. Accepted saveOutput values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid saveOutput value.",
      saveOutput
    ))
  }
  if (!is.logical(map)) {
    stop(sprintf(
      "Error: The specified map '%s' is invalid. Accepted map values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid map value.",
      map
    ))
  }
  if (!is.logical(anim)) {
    stop(sprintf(
      "Error: The specified anim '%s' is invalid. Accepted anim values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid anim value.",
      anim
    ))
  }
  if (!is.logical(normalized)) {
    stop(sprintf(
      "Error: The specified normalized '%s' is invalid. Accepted normalized values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid normalized value.",
      normalized
    ))
  }
  if (!is.logical(by_gr)) {
    stop(sprintf(
      "Error: The specified by_gr '%s' is invalid. Accepted by_gr values are: TRUE, FALSE. Please rerun the calc_hap_impacts function with a valid by_gr value.",
      by_gr
    ))
  }
  if (!is.null(fit_result) && !identical(fit_result$HIA_var, HIA_var)) {
    stop(sprintf(
      "Error: the supplied fit_result was fit for HIA_var = '%s', but this call uses HIA_var = '%s'. Pass a fit_result from fit_model(HIA_var = '%s'), or leave fit_result = NULL to fit it internally.",
      fit_result$HIA_var, HIA_var, HIA_var
    ))
  }



  # Ancillary Functions
  `%!in%` <- Negate(`%in%`)

  # Add converter: Teragram to kt
  CONV_Tg_kt <- 1E3

  # Create the directory if they do not exist:
  if (!dir.exists("output")) dir.create("output")

  # Then, load the rgcam project if prj not passed as a parameter:
  if (!is.null(db_path) & !is.null(db_name)) {
    rlang::inform("Creating project ...")
    conn <- rgcam::localDBConn(db_path,
      db_name,
      migabble = FALSE
    )
    prj <- suppressWarnings(
      rgcam::addScenario(conn,
        prj_name,
        scen_name,
        paste0(query_path, "/", queries),
        saveProj = TRUE
      )
    )
  } else {
    rlang::inform("Loading project ...")
    prj <- rgcam::loadProject(prj_name)
  }
  # Consider the final_db_year as the user indicated year or the closes year available in the project file
  final_db_year <- min(
    final_db_year,
    max(rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)")$year)
  )


  #-----
  # EXTRACT DATA FROM GCAM SCENARIO OUTPUTS

  rlang::inform("Running rhap ...")

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
    # dplyr::mutate(ghg = dplyr::if_else(ghg == "BC" | ghg == "OC", "PrimPM25", ghg)) %>%
    # sum pollutants after adjustments
    dplyr::group_by(scenario, region, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # filters the pollutants that will be used in the econometric analysis
    dplyr::filter(
      ghg %in% rhap::panel_pollutants,
      year <= final_db_year
    ) %>%
    # transform unit to kt (unit of the emissions in the panel)
    dplyr::mutate(
      value = value * CONV_Tg_kt,
      Units = "kt"
    )

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
  em_ctry_gr <- rhap::Percen %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Pollutant = dplyr::if_else(Pollutant == "POM", "OC", Pollutant),
      year = as.numeric(as.character(year))
    ) %>%
    dplyr::rename(
      region = `GCAM Region`,
      ghg = Pollutant,
      country = Country,
      iso3 = `ISO 3`
    ) %>%
    dplyr::filter(
      ghg %in% rhap::panel_pollutants,
      year <= final_db_year
    ) %>%
    # TODO add for more scenarios!!!
    gcamdata::left_join_error_no_match(em_reg, by = c("region", "ghg", "year")) %>%
    dplyr::mutate(em_ctry = Percentage * value) %>%
    dplyr::mutate(iso = tolower(iso3)) %>%
    dplyr::mutate(country = stringr::str_to_title(country)) %>%
    dplyr::select(scenario, country, iso, ghg, year, unit = Units, em_ctry) %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(
      ssp = "SSP2",
      ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
      ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
      ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
      ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)
    ) %>%
    # Use shares to calculate emissions by household group (e.g., decile)
    gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
    gcamdata::left_join_error_no_match(em_shares_gr, by = c("scenario", "country", "ghg", "year", "group")) %>%
    dplyr::mutate(em_ctry_gr = em_ctry * dec_share) %>%
    dplyr::select(scenario, ssp, country, ghg, group, year, em_ctry_gr)

  # get emissions at country level
  em_ctry <- em_ctry_gr %>%
    dplyr::group_by(scenario, ssp, country, ghg, year) %>%
    dplyr::summarise(em_ctry = sum(em_ctry_gr)) %>%
    dplyr::ungroup()


  #  2- Floorspace
  # First, extract population data
  pop_gr <- rgcam::getQuery(prj, "subregional population") %>%
    dplyr::filter(
      grepl("resid", `gcam-consumer`),
      year <= final_db_year
    ) %>%
    dplyr::mutate(
      group = gsub("resid_", "", `gcam-consumer`),
      pop = value * 1E3
    ) %>%
    dplyr::select(scenario, region, year, group, pop)

  # If the `subregional population` query misses some values, compute them
  # using the `population by region` query
  n_groups <- length(unique(pop_gr$group))

  # fix (Taiwan): if the `subregional population` query misses some values,
  # compute them using the `population by region` query
  pop_gr <- pop_gr %>%
    dplyr::full_join(
      rgcam::getQuery(prj, "population by region") %>%
        dplyr::filter(year <= final_db_year) %>%
        dplyr::group_by(scenario, region, year, value) %>%
        tidyr::expand(group = unique(pop_gr$group)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value_adj = (value / n_groups) * 1E3) %>%
        dplyr::select(-value),
      by = c("scenario", "region", "group", "year")
    ) %>%
    dplyr::mutate(pop = dplyr::if_else(is.na(pop), value_adj, pop)) %>%
    dplyr::select(scenario, region, year, group, pop)

  pop <- pop_gr %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(pop = sum(pop)) %>%
    dplyr::ungroup()

  # Extract floorspace data and combine it with subregional population to compute per capita floorspace
  flsp_pc <- rgcam::getQuery(prj, "building floorspace") %>%
    dplyr::filter(grepl("resid", building)) %>%
    dplyr::mutate(
      group = gsub("resid_", "", building),
      flsp_m2 = value * 1E9
    ) %>%
    dplyr::select(scenario, region, year, group, flsp_m2) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(flsp_m2 = sum(flsp_m2)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year <= final_db_year) %>%
    gcamdata::left_join_error_no_match(pop, by = c("scenario", "region", "year")) %>%
    dplyr::mutate(
      flsp_pc = flsp_m2 / pop,
      unit = "m2/pers"
    ) %>%
    dplyr::select(scenario, region, year, unit, flsp_pc)


  # Assume that al countries within the region have similar flps_pc
  flsp_pc_ctry <- flsp_pc %>%
    dplyr::left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
    # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
    dplyr::mutate(
      ssp = "SSP2",
      ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
      ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
      ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
      ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)
    ) %>%
    dplyr::select(scenario, ssp, country, year, flsp_pc)


  #  3- Per capita GDP (gdppc) by group

  # Get country-level SSP-specific data
  gdp_ctry <- dplyr::bind_rows(
    get(paste0("gdp_ctry.", "SSP1")),
    get(paste0("gdp_ctry.", "SSP2")),
    get(paste0("gdp_ctry.", "SSP3")),
    get(paste0("gdp_ctry.", "SSP4")),
    get(paste0("gdp_ctry.", "SSP5")),
  ) %>%
    dplyr::rename(
      ssp = scenario,
      country = region
    )

  # Add missing countries
  gdp_adj <- rhap::ssp_gdp_adj %>%
    tidyr::gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(
      year %in% unique(gdp_ctry$year),
      Scenario != "Historical Reference"
    ) %>%
    dplyr::rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit) %>%
    dplyr::mutate(gdp = value * 1E9 * gcamdata::gdp_deflator(2011, 2017)) %>%
    dplyr::select(country = region, ssp = scenario, year, gdp_dol2011_ppp = gdp)

  gdp_ctry <- dplyr::bind_rows(gdp_ctry, gdp_adj)


  pop_ctry <- dplyr::bind_rows(
    get(paste0("pop_ctry.", "SSP1")),
    get(paste0("pop_ctry.", "SSP2")),
    get(paste0("pop_ctry.", "SSP3")),
    get(paste0("pop_ctry.", "SSP4")),
    get(paste0("pop_ctry.", "SSP5")),
  ) %>%
    dplyr::rename(
      ssp = scenario,
      country = region
    )

  # pop_share/pop_ctry_gr and gdp_share/gdp_ctry_gr are only consumed by the
  # by_gr shares block further down (gated on the same by_gr && saveOutput
  # condition). Skip these GCAM queries and their processing entirely when
  # by_gr = FALSE (the default) -- there's no point hitting
  # "subregional population"/"subregional income" and downscaling them to
  # country x group level if nothing will ever read the result.
  if (by_gr && saveOutput) {
    # Process Population: Population is evenly distributed across groups, but could be updated
    pop_share <- rgcam::getQuery(prj, "subregional population") %>%
      dplyr::filter(
        year <= final_db_year,
        grepl("resid", `gcam-consumer`)
      ) %>%
      tidyr::separate(`gcam-consumer`, c("sector", "group"), sep = "_") %>%
      # fix (Taiwan): if the `subregional population` query misses some values,
      # compute them using the `population by region` query
      dplyr::full_join(
        rgcam::getQuery(prj, "population by region") %>%
          dplyr::filter(year <= final_db_year) %>%
          dplyr::group_by(scenario, region, year, value) %>%
          tidyr::expand(group = unique(pop_gr$group)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            value_adj = value / n_groups,
            sector = "resid"
          ) %>%
          dplyr::select(-value),
        by = c("scenario", "region", "sector", "group", "year")
      ) %>%
      dplyr::mutate(value = dplyr::if_else(is.na(value), value_adj, value)) %>%
      # rename and aggregate sectors
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(value_agg = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(share_pop = round(value / value_agg, 1)) %>%
      dplyr::select(scenario, region, year, group, share_pop) %>%
      # Temp fix for Taiwan
      dplyr::mutate(share_pop = dplyr::if_else(region == "Taiwan", 0.1, share_pop)) %>%
      # expand shares to countries
      dplyr::left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
      # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
      dplyr::mutate(
        ssp = "SSP2",
        ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
        ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
        ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
        ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)
      ) %>%
      dplyr::select(scenario, ssp, country, year, group, share_pop) %>%
      dplyr::mutate(year = as.character(year))

    pop_ctry_gr <- pop_ctry %>%
      dplyr::filter(year <= final_db_year) %>%
      # filter only ssps used
      dplyr::filter(ssp %in% unique(pop_share$ssp)) %>%
      # add groups
      gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
      # adjust country names to match
      dplyr::left_join(rhap::adj_ctry, by = "country") %>%
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
      dplyr::filter(
        grepl("resid", `gcam-consumer`),
        year <= final_db_year
      ) %>%
      tidyr::separate(`gcam-consumer`, c("sector", "group"), sep = "_") %>%
      dplyr::mutate(gdp_pc = value * 1E3) %>%
      dplyr::select(-value, -Units) %>%
      gcamdata::left_join_error_no_match(
        pop_gr %>%
          dplyr::rename(value = pop),
        by = c("scenario", "group", "region", "year")
      ) %>%
      dplyr::mutate(gdp = gdp_pc * (value * 1E3)) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(gdp_agg = sum(gdp)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(share_gdp = round(gdp / gdp_agg, 5)) %>%
      dplyr::select(scenario, region, year, group, share_gdp) %>%
      # expand shares to countries
      dplyr::left_join(reg_to_ctry, by = "region", relationship = "many-to-many") %>%
      # Add SSP narrative associated with the scenario. Use SSP2 by default if no other SSP is specified in the scenario name
      dplyr::mutate(
        ssp = "SSP2",
        ssp = dplyr::if_else(grepl("SSP1", scenario), "SSP1", ssp),
        ssp = dplyr::if_else(grepl("SSP3", scenario), "SSP3", ssp),
        ssp = dplyr::if_else(grepl("SSP4", scenario), "SSP4", ssp),
        ssp = dplyr::if_else(grepl("SSP5", scenario), "SSP5", ssp)
      ) %>%
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
      dplyr::filter(
        ssp %in% unique(gdp_share$ssp),
        year <= final_db_year
      ) %>%
      # add groups
      gcamdata::repeat_add_columns(tibble::tibble(group = unique(em_shares_gr$group))) %>%
      # adjust country names to match
      dplyr::left_join(rhap::adj_ctry, by = "country") %>%
      dplyr::mutate(country = dplyr::if_else(is.na(adj_country), country, adj_country)) %>%
      dplyr::select(-adj_country) %>%
      # filter out Mayotte and Curasao
      dplyr::filter(country %!in% c("Mayotte", "Curasao")) %>%
      gcamdata::left_join_error_no_match(gdp_share, by = c("ssp", "country", "year", "group")) %>%
      # compute group-level population
      dplyr::mutate(gdp_dol2011_ppp_gr = gdp_dol2011_ppp * share_gdp) %>%
      dplyr::select(scenario, ssp, country, year, group, gdp_dol2011_ppp_gr)
  }


  # Combine processed datasets to get per capita GDP
  gdp_pc_ctry <- gdp_ctry %>%
    tibble::as_tibble() %>%
    gcamdata::left_join_error_no_match(
      pop_ctry %>%
        dplyr::mutate(
          country = dplyr::if_else(country == "Palestine", "Palestinian Territory, Occupied", country),
          country = dplyr::if_else(country == "Syria", "Syrian Arab Republic", country)
        ),
      by = c("country", "ssp", "year")
    ) %>%
    dplyr::mutate(gdp_pc_dol2011_ppp = gdp_dol2011_ppp / pop) %>%
    # adjust country names to match
    dplyr::left_join(rhap::adj_ctry, by = "country") %>%
    dplyr::mutate(country = dplyr::if_else(is.na(adj_country), country, adj_country)) %>%
    dplyr::select(-adj_country) %>%
    gcamdata::repeat_add_columns(tibble::tibble(scenario = unique(em_ctry$scenario))) %>%
    dplyr::select(scenario, ssp, country, year, gdp_pc_dol2011_ppp)

  #-----
  # COMBINE THE DATA AND TRANSFORM IT TO MODEL VARIABLES

  # Fit the model once and reuse everywhere below. Previously fit_model()
  # was called twice (once for predictable_regions via old positional
  # indexing [[2]], again later for model.fixed/bias_adder) — wasteful
  # (refits the whole plm model twice) and the [[2]] access breaks now
  # that fit_model() returns a named list. A caller running many scenarios
  # for the same HIA_var can skip this refit entirely by passing a
  # previously-computed fit_model(HIA_var) result via the fit_result
  # argument (validated against HIA_var above).
  if (is.null(fit_result)) {
    fit_result <- fit_model(HIA_var = HIA_var)
  }
  model.fixed <- fit_result$model.fixed
  bias_adder <- fit_result$bias_adder
  predictable_regions <- fit_result$predictable_regions

  # 1- Calculate the outputs at country level
  output <- em_ctry %>%
    gcamdata::left_join_error_no_match(flsp_pc_ctry, by = c("scenario", "ssp", "country", "year")) %>%
    dplyr::filter(year >= min(unique(gdp_pc_ctry$year))) %>%
    dplyr::mutate(year = as.character(year)) %>%
    # Filter out some small countries not in the SSP database
    dplyr::filter(country %!in% c(
      "Bermuda", "Cook Islands", "Cook Islands", "Dominica", "Falkland Islands (Malvinas)",
      "Faroe Islands", "Gibraltar", "Guadeloupe", "Greenland", "Saint Kitts And Nevis",
      "Liechtenstein", "Marshall Islands", "Montserrat", "Martinique", "Niue", "Palau",
      "Reunion", "Saint Pierre And Miquelon", "Isle Of Man", "Turks And Caicos", "Tokelau",
      "Wallis And Futuna", "Virgin Islands, British", "Kosovo", "Cayman Islands",
      "American Samoa", "Samoa"
    )) %>%
    gcamdata::left_join_error_no_match(gdp_pc_ctry, by = c("scenario", "ssp", "country", "year")) %>%
    # filter pollutants in the regression model: BC/OC, NOx, and VOC
    dplyr::filter(ghg %in% rhap::panel_pollutants) %>%
    tidyr::pivot_wider(
      names_from = "ghg",
      values_from = "em_ctry"
    ) %>%
    # Combine BC and OC
    dplyr::mutate(PrimPM25 = BC + OC) %>%
    dplyr::select(-BC, -OC) %>%
    # Add population to compute pollutants per 100K
    gcamdata::left_join_error_no_match(
      pop_ctry %>%
        dplyr::left_join(rhap::adj_ctry, by = "country") %>%
        dplyr::mutate(country = dplyr::if_else(is.na(adj_country), country, adj_country)) %>%
        dplyr::select(-adj_country),
      by = c("ssp", "country", "year")
    ) %>%
    dplyr::mutate(
      PrimPM25_per_100k = (PrimPM25 / pop) * 100000,
      NOx_per_100k = (NOx / pop) * 100000,
      VOC_per_100k = (NMVOC / pop) * 100000
    ) %>%
    # Add logarithms
    dplyr::mutate(
      log_PrimPM25_per_100k = log(PrimPM25_per_100k),
      log_NOx_per_100k = log(NOx_per_100k),
      log_VOC_per_100k = log(VOC_per_100k),
      log_gdppc_ppp_dol2011 = log(gdp_pc_dol2011_ppp),
      log_flsp = log(flsp_pc)
    ) %>%
    dplyr::select(scenario, country_name = country, year, dplyr::starts_with("log"), pop) %>%
    # adjust country names to match to panel data
    gcamdata::left_join_error_no_match(rhap::adj_ctry_output, by = "country_name") %>%
    dplyr::mutate(country_name = dplyr::if_else(data_name == "", country_name, data_name)) %>%
    dplyr::select(-data_name) %>%
    # adjust Rou
    dplyr::mutate(country_name = dplyr::if_else(country_name == "Roumania", "Romania", country_name)) %>%
    # remove not predictable regions
    dplyr::filter(country_name %in% predictable_regions) %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.finite(.), ., NA_real_))) %>%
    tidyr::drop_na() %>%
    # Plain numeric year column referenced by the model's trend term
    # (year_num). Must exist as a real column, not derived via
    # as.numeric(year) at predict time — see fit_model() notes on why
    # as.numeric() on a pdata.frame's index column is unsafe.
    dplyr::mutate(year_num = as.numeric(year))


  #-----

  # PREDICTION
  # model.fixed / bias_adder were already computed once above, alongside
  # predictable_regions, and are reused here rather than refit.
  #
  # NOTE: predict() is called directly on the plain `output` data frame,
  # NOT on a plm::pdata.frame(). output contains multiple scenarios (and
  # output_gr additionally multiple groups) sharing the same
  # country_name+year combination, which is exactly what
  # plm::pdata.frame()'s index requires to be unique — building a
  # pdata.frame here would error ("duplicate couples (id-time)") as soon
  # as more than one scenario is processed at once. Out-of-sample
  # prediction here only needs coefficients %*% regressors (no lag(),
  # diff(), or other panel-specific transform), so a plain data.frame is
  # sufficient and sidesteps the uniqueness requirement entirely.

  # Predict
  # suppressWarnings: plm::predict.plm() warns that newdata isn't a
  # pdata.frame and falls back to coefficients-only prediction (no fixed
  # effect). That's expected and intentional here (see NOTE above) -- the
  # bias_adder computed in fit_model() is exactly what corrects for the
  # missing fixed effect, so this isn't a problem to surface as a warning.
  output$pred_value <- suppressWarnings(stats::predict(model.fixed, newdata = output))

  output_fin <- output %>%
    dplyr::mutate(pred_var = paste0("pred_log_", HIA_var, "_per_100K")) %>%
    gcamdata::left_join_error_no_match(
      bias_adder %>%
        gcamdata::repeat_add_columns(tibble::tibble(year = unique(output$year))),
      by = c("country_name", "year")
    ) %>%
    # dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(
      pred_value_per_100K = exp(pred_value),
      pred_var = gsub("log_", "", pred_var)
    ) %>%
    dplyr::mutate(
      # bias.adder is additive (see fit_model() for why an additive, not
      # multiplicative, correction is used). It can be negative enough to
      # push the corrected value below 0 for some countries; floor at a
      # small fraction of the naive (uncorrected) prediction rather than
      # clamping to exactly 0, so a real (if highly uncertain) low value
      # is reported instead of a hard, discontinuous "zero risk".
      pred_value_per_100K_adj = pred_value_per_100K + bias.adder,
      pred_value_per_100K_adj = pmax(pred_value_per_100K_adj, pred_value_per_100K * 0.01),
      pred_value_per_100K_adj = round(pred_value_per_100K_adj, 2),
      pred_value = round(pred_value_per_100K_adj * pop / 100000, 0),
      pred_var = gsub("pred_", "", pred_var),
      pred_var = gsub("_per_100K", "", pred_var)
    ) %>%
    # reliability: a convenience label over reliability_ratio (from
    # fit_model()'s bias_adder, joined in above) -- how much of this
    # country's predicted level rests on the bias correction rather than
    # the model's own covariates. "low" doesn't mean the number is wrong;
    # it means the model has little independent basis for that country's
    # absolute level, and the prediction is closer to "last observed rate,
    # lightly adjusted for scenario changes" than to a model-driven estimate.
    dplyr::mutate(
      reliability = dplyr::case_when(
        reliability_ratio <= 0.5 ~ "high",
        reliability_ratio <= 2 ~ "medium",
        TRUE ~ "low"
      )
    ) %>%
    dplyr::select(
      scenario, country = country_name, year, pred_var, pred_value,
      pred_value_normalized = pred_value_per_100K_adj, reliability_ratio, reliability
    )


  # Create a function to write the data (by scenario)
  output.write <- function(df) {
    df <- as.data.frame(df)
    utils::write.csv(df, paste0("output/", unique(df$scenario), "_HAP_", unique(HIA_var), ".csv"),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
  }

  if (saveOutput == TRUE) {
    invisible(lapply(split(output_fin, output_fin$scenario), output.write))
  }


  # If by group = TRUE, report descriptive group-level shares
  #
  # NOTE: by_gr reports within-country income-decile group SHARES of
  # population, PM2.5+NOx exposure, and GDP -- it does not estimate
  # group-level health impacts. An earlier version applied the
  # country-level regression model (fit on between-country variation) to
  # within-country decile-level inputs to produce a fabricated
  # "group-level death rate". That is an extrapolation the model was never
  # fit to support (there is no sub-national variation in the training
  # data to validate it against), so it has been replaced with the
  # directly-observable shares GCAM already computes per group -- these
  # carry no such extrapolation risk. For detailed per-pollutant group
  # breakdowns (not just combined PM2.5+NOx), see calc_ResidEm_grp().
  if (saveOutput == TRUE & by_gr == TRUE) {
    # Create the directory if they do not exist:
    if (!dir.exists("output/by_gr")) dir.create("output/by_gr")

    # Each share's denominator is the sum across groups WITHIN the same
    # frame (not a join back to the separate country-level frames, e.g.
    # pop_ctry/gdp_ctry) -- those go through a different country-name
    # normalization step (rhap::adj_ctry) than the *_ctry_gr frames do, so
    # a name-based join between them silently drops non-matching
    # countries. Summing within the group frame itself sidesteps that
    # entirely and is exact by construction (group shares of a country's
    # own group breakdown must sum to that country's total).
    pop_share_gr <- pop_ctry_gr %>%
      dplyr::group_by(scenario, ssp, country, year) %>%
      dplyr::mutate(pop_ctry_total = sum(pop_gr)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(metric = "population", share = pop_gr / pop_ctry_total) %>%
      dplyr::select(scenario, country, year, group, metric, share)

    exposure_share_gr <- em_ctry_gr %>%
      dplyr::filter(ghg %in% c("BC", "OC", "NOx")) %>%
      dplyr::group_by(scenario, ssp, country, group, year) %>%
      dplyr::summarise(em_ctry_gr = sum(em_ctry_gr), .groups = "drop") %>%
      dplyr::group_by(scenario, ssp, country, year) %>%
      dplyr::mutate(em_ctry_total = sum(em_ctry_gr)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(metric = "exposure", share = em_ctry_gr / em_ctry_total, year = as.character(year)) %>%
      dplyr::select(scenario, country, year, group, metric, share)

    gdp_share_gr <- gdp_ctry_gr %>%
      dplyr::group_by(scenario, ssp, country, year) %>%
      dplyr::mutate(gdp_ctry_total = sum(gdp_dol2011_ppp_gr)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(metric = "gdp", share = gdp_dol2011_ppp_gr / gdp_ctry_total) %>%
      dplyr::select(scenario, country, year, group, metric, share)

    shares_gr <- dplyr::bind_rows(pop_share_gr, exposure_share_gr, gdp_share_gr) %>%
      dplyr::mutate(share = round(share, 4))

    utils::write.csv(shares_gr, "output/by_gr/shares_byGR.csv",
      row.names = FALSE, fileEncoding = "UTF-8"
    )

    # Pie chart: a single snapshot at final_db_year (one panel per
    # scenario x metric). For other years, filter shares_gr directly and
    # build a custom plot -- the CSV above has the full scenario/year range.
    # A handful of small countries can have a 0/0 (NA) share for a given
    # metric where GCAM reports zero for both the group and country total;
    # drop those from the plot rather than let ggplot silently warn.
    pie_data <- shares_gr %>%
      dplyr::filter(year == as.character(final_db_year), !is.na(share))

    if (nrow(pie_data) > 0) {
      pl <- ggplot2::ggplot(
        pie_data,
        ggplot2::aes(
          x = "", y = share,
          fill = factor(group, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10"))
        )
      ) +
        ggplot2::geom_bar(width = 1, stat = "identity") +
        ggplot2::theme_void() +
        ggplot2::facet_grid(scenario ~ metric) +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          strip.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::scale_fill_manual(values = c(
          "gray20", "gray50", "#ad440c", "#ef8e27", "#d01c2a",
          "darkorchid3", "#507fab", "deepskyblue1", "#11d081", "#00931d"
        ))
      ggplot2::ggsave(pl,
        file = file.path("output/by_gr", paste0("shares_byGR_", final_db_year, ".png")),
        width = 250, height = 120, units = "mm"
      )
    }
  }

  # Add map
  if (map == TRUE) {
    # Create the directory if they do not exist:
    if (!dir.exists("output/maps")) dir.create("output/maps")

    # The variable to be plotted depends on if the user selects or no to use normalized values
    if (normalized == TRUE) {
      var_to_plot <- "pred_value_normalized"
    } else {
      var_to_plot <- "pred_value"
    }


    output_fin_map <- output_fin %>%
      dplyr::rename(country_name = country) %>%
      # adjust country names to match raster
      gcamdata::left_join_error_no_match(rhap::adj_ctry_map, by = "country_name") %>%
      dplyr::mutate(country_name = dplyr::if_else(country_map != "", country_map, country_name)) %>%
      dplyr::select(scenario, country_name, year, pred_var, dplyr::all_of(var_to_plot)) %>%
      dplyr::rename(
        subRegion = country_name,
        value = var_to_plot
      ) %>%
      tidyr::complete(tidyr::nesting(scenario, year, pred_var),
        subRegion = unique(rmap::mapCountries$region)
      )

    # NOTE: must be assigned to the global environment (<<-), not local.
    # rmap::map() -> rmap::map_plot() -> rmap::map_find() looks up
    # "mapCountries" via get(), which only finds it in .GlobalEnv, not in
    # calc_hap_impacts()'s local frame. Using <- here breaks map = TRUE
    # with "object 'mapCountries' not found".
    mapCountries <<- rmap::mapCountries


    # Figures:
    #   a) figure for every scenario & year combination
    #   b) single figure for every scenario containing all years
    for (sc in unique(output_fin_map$scenario)) {
      # 1. Plot
      rmap::map(
        data = output_fin_map %>% dplyr::filter(scenario == sc),
        folder = paste("output/maps/map", sc, "allYears", sep = "_"),
        legendType = "pretty",
        background = TRUE,
        animate = anim,
        underLayer = mapCountries,
        colorNA = "grey92",
        showNA = TRUE
      )

      # 2. Reorder folders and rename figures
      # 2.1. move the allYears figure
      file.rename(
        from = file.path(paste("output/maps/map", sc, "allYears", sep = "_"), "map_param_PRETTY.png"),
        to = paste0("output/maps/map_", sc, "_allYears", ".png")
      )

      # 2.2. move all annual figures
      files_to_move <- list.files(file.path(paste("output/maps/map", sc, "allYears", sep = "_"), "byYear"), full.names = TRUE)
      success <- sapply(files_to_move, function(file) {
        file.rename(file, file.path(paste("output/maps/map", sc, "allYears", sep = "_"), basename(file)))
      })

      # 2.3. remove unnecessary directories and files
      if (all(success)) {
        unlink(file.path(paste("output/maps/map", sc, "allYears", sep = "_"), "byYear"), recursive = TRUE)
      } else {
        message("Some files could not be moved. The source folder was not deleted.")
      }
      unlink(file.path(paste("output/maps/map", sc, "allYears", sep = "_"), "map_param_MEAN_PRETTY.png"), recursive = TRUE)
      unlink(file.path(paste("output/maps/map", sc, "allYears", sep = "_"), "map_param.csv"), recursive = TRUE)

      # 2.4. rename folder
      if (dir.exists(file.path(paste("output/maps/map", sc, "byYear", sep = "_")))) {
        unlink(file.path(paste("output/maps/map", sc, "byYear", sep = "_")), recursive = TRUE)
      }
      file.rename(
        file.path(paste("output/maps/map", sc, "allYears", sep = "_")),
        file.path(paste("output/maps/map", sc, "byYear", sep = "_"))
      )
    }


    # Figures: single figure for every year containing all scenarios
    for (y in unique(output_fin_map$year)) {
      # 1. Plot
      rmap::map(
        data = output_fin_map %>% dplyr::filter(year == y) %>%
          dplyr::rename(class = scenario),
        folder = paste("output/maps/map", "allScen", y, sep = "_"),
        legendType = "pretty",
        background = TRUE,
        animate = anim,
        underLayer = mapCountries,
        colorNA = "grey92",
        showNA = TRUE
      )

      # 2. Reorder folders and rename figures
      # 2.1. remove an intermediate folder
      file.rename(
        from = file.path(paste("output/maps/map", "allScen", y, sep = "_"), "map_param_PRETTY.png"),
        to = paste0("output/maps/map_", "allScen_", y, ".png")
      )
      unlink(paste("output/maps/map", "allScen", y, sep = "_"), recursive = TRUE)
    }
    # 2.2. gather all figures in "map_allScen_byYear" new folder
    # NOTE: pattern must match only the .png files, not "^map_allScen_" alone.
    # On a rerun against a not-freshly-cleaned output/ dir, the destination
    # folder itself ("map_allScen_byYear") already exists and would match a
    # bare "^map_allScen_" prefix, causing file.rename() to try moving that
    # folder into itself.
    files_to_move <- list.files(path = file.path("output/maps"), pattern = "^map_allScen_.*\\.png$", full.names = TRUE)
    if (!dir.exists("output/maps/map_allScen_byYear")) dir.create("output/maps/map_allScen_byYear")
    success <- sapply(files_to_move, function(file) {
      file.rename(file, file.path("output/maps/map_allScen_byYear", basename(file)))
    })
  }


  invisible(output_fin)
}
