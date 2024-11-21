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

  # Extarct data from gcam secenario outpus

  #  Emissions
  # getQuery(prj, )

}
