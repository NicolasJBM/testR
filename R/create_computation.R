#' @name create_computation
#' @title Create a single or multiple true or false alternative selection question
#' @author Nicolas Mangin
#' @description Function creating a computation question by retrieving in feedback
#' @param feedbacks Tibble. Table from which items (alternatives) and associated feedback are selected.
#' @param codes Character string. Code of the question to which the propositions are linked.
#' @param langiso Character. ISO code of the language of the question.
#' @param altnbr Integer. Number of propositions (i.e. choices) to offer to the student.
#' @param interrogation Character. Question asked to the student.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr filter
#' @importFrom dplyr slice_sample
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @export


create_computation <- function(
  feedbacks, codes, langiso, altnbr, interrogation
){
  
  type <- NULL
  language <- NULL
  value <- NULL
  
  selected <- feedbacks |>
    dplyr::filter(
      type == "Computation",
      code %in% codes,
      language == langiso
    )
  
  if (base::nrow(selected) >= altnbr &
      base::sum(selected$value) > 0 &
      altnbr > 1){
    
    selected_true <- selected |>
      dplyr::filter(value == 1) |>
      dplyr::slice_sample(n = 1)
    
    selected_false <- selected |>
      dplyr::filter(value == 0) |>
      dplyr::slice_sample(n = (altnbr-1))
    
    exercise <- selected_true |>
      dplyr::bind_rows(selected_false)
    
  } else if (base::nrow(selected) >= altnbr &
             base::sum(selected$value) > 0 &
             altnbr <= 1) {
    
    exercise <- selected
    
  } else {
    
    exercise <- tibble::tibble(
      item = c(1:altnbr), code = "", type = "", document = "",
      language = "", modifications = 0,
      proposition = "right1", value = c(1, base::rep(0, (altnbr-1))),
      scale = "", explanation = "", keywords = ""
    )
  }
  
  slctaltnbr <- base::nrow(exercise)
  exercise <- exercise |>
    dplyr::slice_sample(n = slctaltnbr) |>
    dplyr::mutate(
      number = base::seq_len(slctaltnbr),
      letter = base::letters[base::seq_len(slctaltnbr)],
      correct = value,
      interrogation = interrogation
    )
  
  return(exercise)
}
