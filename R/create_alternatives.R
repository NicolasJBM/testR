#' @name create_alternatives
#' @title Create a single or multiple true or false alternative selection question
#' @author Nicolas Mangin
#' @description Function creating the interrogation and different propositions as well as the explanations associated with a given version of a question based on alternatives.
#' @param propositions Tibble. Table from which items (alternatives) and associated explanations are selected.
#' @param codes Character string. Code of the question to which the propositions are linked.
#' @param situation List. The entry selected as the situation which determines which items are true.
#' @param altnbr Integer. Number of propositions (i.e. choices) to offer to the student.
#' @param correctnbr Integer. Number of correct propositions (i.e. choices) to offer to the student.
#' @param exclude Character vector. Items which should be excluded.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr slice_sample
#' @importFrom tibble tibble
#' @export

create_alternatives <- function(
    propositions, codes, situation, altnbr, correctnbr, exclude = NA
){
  
  type <- NULL
  language <- NULL
  value <- NULL
  code <- NULL
  retire <- NULL
  keywords <- NULL
  
  interrogation <- situation[[1]]$interrogation
  correct_answer <- situation[[1]]$correct
  imperfect_answer <- situation[[1]]$imperfect
  
  selected <- propositions |>
    dplyr::filter(
      type == "Alternatives",
      code %in% codes,
      retire == FALSE,
      !(item %in% exclude)
    ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        item %in% correct_answer ~ 1,
        item %in% imperfect_answer ~ 0.5,
        TRUE ~ 0
      )
    )
  
  maxcorrect <- base::nrow(dplyr::filter(selected, value == 1))
  correctnbr <- base::min(maxcorrect, correctnbr)
  correctnbr <- base::min(altnbr, correctnbr)
  incorrectnbr <- altnbr - correctnbr
  
  if (base::nrow(selected) >= altnbr){
    
    maxtrue <- base::min(altnbr, base::sum(selected$value))
    nbrtrue <- base::sample(1:maxtrue, 1)
    
    selected_correct <- selected |>
      dplyr::filter(value == 1) |>
      dplyr::slice_sample(n = correctnbr)
    
    selected_incorrect <- selected |>
      dplyr::filter(value < 1) |>
      dplyr::slice_sample(n = incorrectnbr)
    
    exercise <- selected_correct |>
      dplyr::bind_rows(selected_incorrect)
    
  } else {
    
    exercise <- tibble::tibble(
      item = 1:altnbr, code = "", type = "", document = "",
      language = "", modifications = 0,
      proposition = "", value = c(1, base::rep(0, (altnbr-1))), scale = "",
      explanation = "", keywords = ""
    )
    
  }
  
  exercise <- exercise |>
    dplyr::slice_sample(n = altnbr) |>
    dplyr::mutate(
      number = base::seq_len(altnbr),
      letter = base::letters[base::seq_len(altnbr)],
      correct = value,
      interrogation = interrogation
    )
  
  return(exercise)
}

