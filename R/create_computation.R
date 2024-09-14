#' @name create_computation
#' @title Create a closed or open computation question
#' @author Nicolas Mangin
#' @description Function creating a computation question which can be either open (altnbr = 0) or closed. Explanations associated with different answers are also retrieved.
#' @param propositions Tibble. Table from which items (alternatives) and associated explanations are selected.
#' @param codes Character string. Code of the question to which the propositions are linked.
#' @param altnbr Integer. Number of propositions (i.e. choices) to offer to the student.
#' @param interrogation Character. Question asked to the student.
#' @param exclude Character vector. Propositions which should be excluded.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr slice_sample
#' @importFrom tibble tibble
#' @export


create_computation <- function(propositions, codes, altnbr, interrogation, exclude = NA){
  
  type <- NULL
  language <- NULL
  value <- NULL
  code <- NULL
  retire <- NULL
  proposition <- NULL
  
  selected <- propositions |>
    dplyr::filter(
      type == "Computation",
      code %in% codes,
      retire == FALSE
    )
  
  if (!base::is.na(exclude)){
    selected <- dplyr::filter(selected, !(proposition %in% exclude))
  }
  
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
      item = 1:altnbr, code = "", type = "", document = "",
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
