##' calc_ResidEm_grp
#'
#'
#' Generate pie-charts with the contribution of different within-region population groups to different pollutant emissions by scenario and year
#' @keywords population groups, pollutants
#' @return Contribution of different groups to pollutant emissions
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param year Select the year. 2020:2100
#' @param pollutant Select the pollutant. List of available gases in "rhap::all_pollutants"
#' @param region Select the region. It includes the GCAM 32 geopolitical regions + EU-27 + Global: https://github.com/JGCRI/gcam-doc/blob/gh-pages/overview.md
#' @param saveOutput Writes the emission files. By default=T
#' @param pie Produce the maps. By default=F
#' @importFrom magrittr %>%
#' @export

calc_ResidEm_grp <- function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                             scen_name, queries = "queries_rhap.xml", final_db_year = 2100,
                             year, pollutant, region,
                             saveOutput = T, pie = T) {


  # Create the directory if they do not exist:
  if (!dir.exists("output")) dir.create("output")

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

  rlang::inform('Running ...')

  # Get emissions by scenario, region, period and group
  em_reg_gr <- rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
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
    # sum pollutants after adjustments
    dplyr::group_by(scenario, region, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # filters the pollutants that will be used in the econometric analysis
    dplyr::filter(year <= final_db_year)

  # Add EU-27
  em_reg_gr_eu27 <- em_reg_gr %>%
    dplyr::filter(region %in% c("EU-12", "EU-15")) %>%
    dplyr::group_by(scenario, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "EU-27")

  # Add Global
  em_reg_gr_glob <- em_reg_gr %>%
    dplyr::group_by(scenario, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "Global")

  em_reg_gr <- dplyr::bind_rows(
    em_reg_gr,
    em_reg_gr_eu27,
    em_reg_gr_glob
  ) %>%
    dplyr::rename(Year = `year`,
           Pollutant = `ghg`,
           Region = `region`)

  # ----------
  # Filter dataset with user-defined parameters
  em_reg_gr_fin <- em_reg_gr %>%
    dplyr::filter(Year == year,
                  Pollutant == pollutant,
                  Region == region) %>%
    dplyr::group_by(scenario, Region, Year, Pollutant, Units) %>%
    dplyr::mutate(value_agg = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc_value = round(value / value_agg , 3)) %>%
    dplyr::select(-value_agg)

  if(saveOutput == T){
    # Create the directory if they do not exist:
    if (!dir.exists("output/ResidEm_grp")) dir.create("output")

    write.csv(em_reg_gr_fin, paste0("ResidEm_grp_", unique(em_reg_gr_fin$Region), "_", unique(em_reg_gr_fin$Year), "_",
                                    unique(em_reg_gr_fin$Pollutant), ".csv"),
              row.names = F)

  }


  if(pie == T){

    ggplot(em_reg_gr_fin, aes(x="",
                              y = value,
                              fill = factor(group,levels = c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10"))))+
      geom_bar(width = 1, stat = "identity") +
      theme_void() +
      facet_wrap(~ scenario) +
      labs(x = "", y = "") +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            strip.text = element_text(size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("gray20","gray50","#ad440c","#ef8e27","#d01c2a",
                                   "darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")) +
      geom_text(aes(label = percent(perc_value)),
                color = "white",
                size = 4,
                position = position_stack(vjust = 0.5))

  }


  invisible(em_reg_gr_fin)



}
