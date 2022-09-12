#' @name set_question_parameters
#' @title Set question parameters
#' @author Nicolas Mangin
#' @description Function retrieving the parameters defined in "test_parameters" for the version of a question identified by "versionid".
#' @param versionid Character. Version identifier.
#' @param test_parameters Tibble. Parameters of all the versions of all the questions included in a test.
#' @param as_latex Logical. Whether the output is a PDF and therefore should be formatted first as Latex.
#' @param record_version Logical. Whether the propositions associated to the question should be recorded in the folder "d_feedback".
#' @return Set all the parameters for the question.
#' @importFrom dplyr filter
#' @importFrom stringr str_remove
#' @export


set_question_parameters <- function(versionid, test_parameters = NA, as_latex = FALSE, record_version = FALSE){
  
  version_parameters <- test_parameters |>
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
  
  if (as_latex) {
    
    euros <- "\\texteuro"
    dollars <- "\\textdollar"
    pounds <- "\\pounds"
    yens <- "\\textyen"
    percents <- "\\%"
    
  } else {
    
    euros <- "&euro;"
    dollars <- "$"
    pounds <- "&pound;"
    yens <- "&yen;"
    percents <- "%"
    
  }
  
  parameters <- base::list(
    test = test,
    seed = seed,
    altnbr = altnbr,
    show_version = show_version,
    show_points = show_points,
    record_version = record_version,
    euros = euros,
    dollars = dollars,
    pounds = pounds,
    yens = yens,
    percents = percents
  )
  
  return(parameters)
}
