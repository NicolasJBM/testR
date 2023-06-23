#' @name set_question_parameters
#' @title Set question parameters
#' @author Nicolas Mangin
#' @description Function retrieving the parameters defined in "test_parameters" for the version of a question identified by "versionid".
#' @param versionid Character. Version identifier.
#' @param test_parameters Tibble. Parameters of all the versions of all the questions included in a test.
#' @param docformat Character Whether the output is "latex", "html", or "pandoc".
#' @param record_solution Logical. Whether the propositions associated to the question should be recorded in the folder "d_feedback".
#' @return Set all the parameters for the question.
#' @importFrom dplyr filter
#' @importFrom stringr str_remove
#' @export


set_question_parameters <- function(versionid, test_parameters = NA, docformat = "pandoc", record_solution = FALSE){
  
  if (base::length(test_parameters) == 1){
    test_parameters <- tibble::tibble(
      test = "default",
      show_version = FALSE,
      show_points = FALSE,
      question = versionid,
      points = 0,
      version = versionid,
      altnbr = 4,
      seed = 123456789
    )
  }
  
  version_parameters <- test_parameters |>
    dplyr::mutate(version = stringr::str_replace_all(version, "^..", substr(versionid, 1,2))) |>
    dplyr::filter(version == versionid)
  
  test <- test_parameters$test[1]
  questionid <- test_parameters$question[1]
  seed <- version_parameters$seed[1]
  altnbr <- version_parameters$altnbr
  
  if (version_parameters$show_version[1]) {
    show_version <- base::paste0(
      stringr::str_remove(version_parameters$version[1], ".Rmd$"),
      " - "
    )
  } else show_version <- ""
  
  if (version_parameters$show_points[1]) {
    points <- version_parameters$points[1]
    if (points > 1) {
      show_points <- base::paste0("(", points, " points)")
    } else show_points <- base::paste0("(", points, " point)")
  } else show_points <- ""
  
  if (docformat == "latex") {
    
    euros <- "\\texteuro"
    dollars <- "\\textdollar"
    pounds <- "\\pounds"
    percents <- "\\%"
    
  } else if (docformat == "html") {
    
    euros <- "&euro;"
    dollars <- "$"
    pounds <- "&pound;"
    percents <- "%"
    
  } else {
    
    euros <- "\u20ac"
    dollars <- "\u0024"
    pounds <- "\u00A3"
    percents <- "u\0025"
    
  }
  
  parameters <- base::list(
    test = test,
    seed = seed,
    altnbr = altnbr,
    show_version = show_version,
    show_points = show_points,
    record_solution = record_solution,
    euros = euros,
    dollars = dollars,
    pounds = pounds,
    percents = percents
  )
  
  return(parameters)
}
