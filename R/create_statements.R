#' @name create_statements
#' @title Create a single or multiple true or false statement selection question
#' @author Nicolas Mangin
#' @description Function creating the interrogation and different propositions as well as the feedback associated with a given version of a question based on statements.
#' @param propositions Tibble. Table from which items (statements) and associated feedback are selected.
#' @param documents Character vector. List of document ids from that the selected statements should address.
#' @param langiso Character. ISO code of the language of the question.
#' @param altnbr Integer. Number of propositions (i.e. choices) to offer to the student.
#' @param truenbr Integer. Number of true propositions (i.e. choices) to offer to the student.
#' @param indication Logical. Whether the number of alternatives to select should be specified in the question.
#' @return Tibble. Table containing all the information about the propositions made to the student.
#' @importFrom dplyr filter
#' @importFrom dplyr slice_sample
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @export



create_statements <- function(
  propositions, documents, langiso, altnbr, truenbr, indication = TRUE
){
  
  type <- NULL
  document <- NULL
  language <- NULL
  value <- NULL
  
  selected <- propositions |>
    dplyr::filter(
      type == "Statements",
      document %in% documents,
      language == langiso
    )
  
  truenbr <- base::min(altnbr, truenbr)
  falsenbr <- altnbr - truenbr
  
  if (base::nrow(selected) >= altnbr & base::sum(selected$value) >= truenbr){
    
    selected_true <- selected |>
      dplyr::filter(value == 1) |>
      dplyr::slice_sample(n = truenbr)
    
    selected_false <- selected |>
      dplyr::filter(value == 0) |>
      dplyr::slice_sample(n = falsenbr)
    
    exercise <- selected_true |>
      dplyr::bind_rows(selected_false)
    
  } else {
    
    exercise <- tibble::tibble(
      item = c(1:altnbr), code = "", type = "", document = "",
      language = "", modifications = 0,
      proposition = "", value = c(1, base::rep(0, (altnbr-1))), scale = "",
      explanation = "", keywords = ""
    )
    
  }
  
  interrogation <- dplyr::case_when(
    indication == FALSE ~ "Which of the following statement(s) is (are) **true**? (multiple answers are possible)",
    indication == TRUE & truenbr == 1 ~ "Which of the following statements is **true**? (1 answer)",
    indication == TRUE & falsenbr == 1  ~ "Which of the following statements is **false**? (1 answer)",
    TRUE ~ base::paste0("Among the following propositions, which are the **", truenbr," true** statements?")
  )
  
  exercise <- exercise |>
    dplyr::slice_sample(n = altnbr) |>
    tibble::rowid_to_column("number") |>
    dplyr::mutate(
      number = base::seq_len(altnbr),
      letter = base::letters[base::seq_len(altnbr)],
      correct = dplyr::case_when(
        falsenbr == 1 ~ 1 - value,
        TRUE ~ value
      ),
      interrogation = interrogation
    )
  
  return(exercise)
}

