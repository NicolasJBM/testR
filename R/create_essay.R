#' @name create_essay
#' @title Create an essay
#' @author Nicolas Mangin
#' @description Function creating an essay question by retrieving grading criteria from the feedback table.
#' @param propositions Tibble. Table from which items (alternatives) and associated feedback are selected.
#' @param codes Character string. Code of the question to which the propositions are linked.
#' @param langiso Character. ISO code of the language of the question.
#' @param interrogation Character. Question asked to the student.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @export


create_essay <- function(
  propositions, codes, langiso, interrogation
){
  
  type <- NULL
  language <- NULL
  proposition <- NULL
  value <- NULL
  
  
  exercise <- propositions |>
    dplyr::filter(
      type == "Essay",
      code %in% codes,
      language == langiso
    )
  
  if (nrow(exercise) == 0){
    exercise <- tibble::tibble(
      item = "", code = "", type = "", document = "",
      language = "", modifications = 0,
      proposition = "", value = 1, scale = "",
      keywords = ""
    )
  }
  
  exercise <- exercise |>
    dplyr::arrange(proposition) |>
    dplyr::mutate(
      number = base::seq_len(base::nrow(exercise)),
      letter = base::letters[base::seq_len(base::nrow(exercise))],
      correct = value,
      interrogation = interrogation
    )
  
  return(exercise)
}
