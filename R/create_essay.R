#' @name create_essay
#' @title Create an essay
#' @author Nicolas Mangin
#' @description Function creating an essay question and retrieving grading criteria from the propositions table.
#' @param propositions Tibble. Table from which items (alternatives) and associated explanations are selected.
#' @param codes Character string. Code of the question to which the propositions are linked.
#' @param interrogation Character. Question asked to the student.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export


create_essay <- function(propositions, codes, interrogation){
  
  type <- NULL
  language <- NULL
  proposition <- NULL
  value <- NULL
  code <- NULL
  retire <- NULL
  category <- NULL
  correct <- NULL
  document <- NULL
  explanation <- NULL
  item <- NULL
  keywords <- NULL
  letter <- NULL
  modifications <- NULL
  number <- NULL
  weight <- NULL
  
  
  exercise <- propositions |>
    dplyr::filter(
      type == "Essay",
      code %in% codes,
      retire == FALSE
    )
  
  if (nrow(exercise) == 0){
    exercise <- tibble::tibble(
      item = "", code = "", type = "", document = "",
      language = "", modifications = 0,
      proposition = "", value = 1, scale = "",
      explanation = "", keywords = "", category = ""
    )
  }
  
  extendedleters <- c(letters, base::c(base::t(base::outer(letters, letters, paste, sep = ""))))
  
  exercise <- exercise |>
    dplyr::arrange(proposition) |>
    dplyr::mutate(
      number = base::seq_len(base::nrow(exercise)),
      letter = extendedleters[base::seq_len(base::nrow(exercise))],
      correct = value,
      interrogation = interrogation,
      category = "",
    ) |>
    dplyr::select(
      number, letter, item,
      type, document, language, modifications,
      interrogation, proposition, value, scale,
      explanation, keywords, correct,
      weight, category
    )
  
  return(exercise)
}
