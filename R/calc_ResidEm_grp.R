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

  # Checks
  if (length(year) != 1) stop("Error: Please provide a single year as input to the `calc_ResidEm_grp` function.")
  if (length(pollutant) != 1) stop("Error: Please provide a single pollutant as input to the `calc_ResidEm_grp` function.")
  if (length(region) != 1) stop("Error: Please provide a single region as input to the `calc_ResidEm_grp` function.")

  year <- as.numeric(as.character(year))
  if (!year %in% seq(2020, 2100, 5)) {
    stop(sprintf(
      "Error: The specified year '%s' is invalid. Accepted years are: %s. Please rerun the `calc_ResidEm_grp` function with a valid year.",
      year, paste(seq(2020, 2100, 5), collapse = ", ")
    ))
  }
  if (!pollutant %in% rhap::all_pollutants) {
    stop(sprintf(
      "Error: The specified pollutant '%s' is invalid. Accepted pollutants are: %s. Please rerun the `calc_ResidEm_grp` function with a valid pollutant.",
      pollutant, paste(rhap::all_pollutants, collapse = ", ")
    ))
  }
  if (!region %in% rhap::gcam_regions) {
    stop(sprintf(
      "Error: The specified region '%s' is invalid. Accepted regions are: %s. Please rerun the `calc_ResidEm_grp` function with a valid region.",
      region, paste(rhap::gcam_regions, collapse = ", ")
    ))
  }


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
  # Consider the final_db_year as the user indicated year or the closes year available in the project file
  final_db_year<-min(final_db_year,
                     max(rgcam::getQuery(prj,'nonCO2 emissions by sector (excluding resource production)')$year))

  if (year > final_db_year) {
    stop(sprintf(
      "Error: The specified year '%s' is invalid. The database only contains data up to %s. Accepted years are: %s. Please rerun the `calc_ResidEm_grp` function with a valid year.",
      year, final_db_year, paste(seq(2020, final_db_year, 5), collapse = ", ")
    ))
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
    if (!dir.exists("output/ResidEm_grp")) dir.create("output/ResidEm_grp")

    write.csv(em_reg_gr_fin,
              file.path('output/ResidEm_grp',
                        paste0("ResidEm_grp_", unique(em_reg_gr_fin$Region), "_",
                               unique(em_reg_gr_fin$Year), "_", unique(em_reg_gr_fin$Pollutant), ".csv")),
              row.names = F)
  }


  if(pie == T){

    # Create the directory if they do not exist:
    if (!dir.exists("output/ResidEm_grp")) dir.create("output/ResidEm_grp")
    if (!dir.exists("output/ResidEm_grp/pie_charts")) dir.create("output/ResidEm_grp/pie_charts")

    pl <- ggplot2::ggplot(em_reg_gr_fin,
                          ggplot2::aes(x="",
                                       y = value,
                                       fill = factor(group,
                                                     levels = c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10"))))+
      ggplot2::geom_bar(width = 1, stat = "identity") +
      ggplot2::theme_void() +
      ggplot2::facet_wrap(~ scenario) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = "bottom",
                     strip.text = ggplot2::element_text(size = 14)) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::scale_fill_manual(values = c("gray20","gray50","#ad440c","#ef8e27","#d01c2a",
                                   "darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")) +
                                     ggplot2::geom_text(ggplot2::aes(label = scales::percent(perc_value)),
                                                        color = "white",
                                                        size = 4,
                                                        position = ggplot2::position_stack(vjust = 0.5))
    ggplot2::ggsave(pl, file = file.path('output/ResidEm_grp/pie_charts',
                                         paste0("ResidEm_grp_", unique(em_reg_gr_fin$Region), "_",
                                                unique(em_reg_gr_fin$Year), "_", unique(em_reg_gr_fin$Pollutant), ".png")),
                    width = 150, height = 200, units = 'mm')


  }


  invisible(em_reg_gr_fin)



}
