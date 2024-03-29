#' @name record_version
#' @title Save feedback
#' @author Nicolas Mangin
#' @description If requested, save the explanations associated with each proposition of each possible version so that it can be used later to produce individual reports for students.
#' @param record_solution Logical. Whether an individual report will be sent later to each student.
#' @param test Character. Name of the test.
#' @param version Character. Name of the version of the question.
#' @param exercise Tibble. Output of one of the "questions_create_..." functions setting interrogations, propositions, and explanations among other pieces of information.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export


record_version <- function(record_solution, test, version, exercise){
  
  number <- NULL
  letter <- NULL
  item <- NULL
  type <- NULL
  document <- NULL
  language <- NULL
  modifications <- NULL
  interrogation <- NULL
  proposition <- NULL
  value <- NULL
  explanation <- NULL
  keywords <- NULL
  correct <- NULL
  weight <- NULL
  
  if (record_solution){
    feedback_path <- base::paste0(
      "../4_solutions/", version, ".csv"
    )
    exercise <- exercise |>
      dplyr::mutate(
        test = test,
        version = version,
        weight = NA
      ) |>
      dplyr::select(
        test, version, number, letter,
        item, type, document, language, modifications,
        interrogation, proposition, value, scale,
        explanation, keywords, correct, weight
      )
    utils::write.csv(exercise, feedback_path, row.names = FALSE)
  }
}
