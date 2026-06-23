#' listYears
#'
#' Return the years of the queries available for a scenario in a project data set.
#' This function requires the data set to have been previously loaded, so it cannot take a file name.
#' Source: gcamreport
#'
#' @param projData The data set to report on.
#' @param scenarios The name(s) of the scenario(s) to report on. If NULL, report on all of them.
#' @param queries The name(s) of the queries(s) to report on. If NULL, report on all of them.
#' @param anyscen If TRUE, then list queries that are in any scenario. If FALSE, list queries that are in all scenarios.
#' @return list of years reported in the project/scenario/queries.
#' @export
listYears <- function (projData, scenarios = NULL, queries = NULL, anyscen = TRUE) {
  if (is.character(projData)) {
    stop("listYears() function requires the data set to have been already loaded.")
  }
  if (is.null(scenarios)) {
    scenarios <- rgcam::listScenarios(projData)
  }
  if (is.null(queries)) {
    queries <- rgcam::listQueries(projData)
  }
  sqlist <- lapply(scenarios, function(scen) {
    lapply(queries, function(quer) {
      if ("year" %in% names(projData[[scen]][[quer]])) {
        yy = unique(projData[[scen]][[quer]][['year']])
        if (length(yy) > 100) {
          NULL
        } else {
          yy
        }
      } else {
        NULL
      }
    })
  })

  combine <- if (anyscen) union else intersect

  if (identical(combine, union)) {
    # Union case: count appearances and keep values appearing >10 times
    # (avoid problems with 2020 and 2021)
    all_years <- unlist(sqlist)
    all_years <- all_years[!is.na(all_years)]
    year_counts <- table(all_years)
    if (length(queries) == 1) {
      result <- sort(as.numeric(names(year_counts)))
    } else {
      result <- sort(as.numeric(names(year_counts[year_counts > (length(queries)/2 + 1)])))
    }
  } else {
    # Intersect case: just intersect all elements
    result <- Reduce(intersect, Reduce(intersect, sqlist))
  }

  return(result)
}

