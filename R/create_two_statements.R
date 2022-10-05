#' @name create_two_statements
#' @title Create a two-statements question
#' @author Nicolas Mangin
#' @description Function creating the different propositions as well as the feedback associated with a given version of a question in which two statements can be either true or false.
#' @param propositions Tibble. Table from which statements and associated feedback are selected.
#' @param documents Character vector. List of document ids from that the selected statements should address.
#' @return Tibble. Table containing all the information about the propositions (i.e. choices) made to the student.
#' @importFrom dplyr filter
#' @importFrom dplyr slice_sample
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @export


create_two_statements <- function(
  propositions, documents
){
  
  type <- NULL
  document <- NULL
  language <- NULL
  tmp <- NULL
  value <- NULL
  
  selected <- propositions |>
    dplyr::filter(
      type == "Statements",
      document %in% documents
    )
  
  if (base::nrow(selected) > 1){
    
    selected <- selected |>
      dplyr::slice_sample(n = 2)
    exercise <- dplyr::bind_rows(base::rep(base::list(selected),4)) |>
      dplyr::mutate(
        number = c(1,1,2,2,3,3,4,4),
        letter = c("a","a","b","b","c","c","d","d"),
        tmp = base::rep(c(1,2), 4)
      ) |>
      dplyr::mutate(
        correct = dplyr::case_when(
          tmp == 1 & value == 0 & letter %in% c("a","c") ~ 1,
          tmp == 2 & value == 0 & letter %in% c("a","b") ~ 1,
          tmp == 1 & value == 1 & letter %in% c("b","d") ~ 1,
          tmp == 2 & value == 1 & letter %in% c("c","d") ~ 1,
          TRUE ~ 0
        ),
        interrogation = "Are both statement false? Is statement one true while statement two is false? The opposite? Or are they both true?"
      ) |>
      dplyr::select(-tmp)
    
  } else {
    
    exercise <- tibble::tibble(
      number = c(1,1,2,2,3,3,4,4),
      letter = c("a","a","b","b","c","c","d","d"),
      item = c(1:8), code = "", type = "", document = "",
      language = "", modifications = 0,
      interrogation = "Are both statement false? Is statement one true while statement two is false? The opposite? Or are they both true?",
      proposition = "", value = c(1, 1, base::rep(0, 6)),
      scale = "", explanation = "", keywords = "", correct = 0
    )
    
  }
  return(exercise)
}
