#' @name prepare_evaluation
#' @title Prepare evaluation of an expression
#' @author Nicolas Mangin
#' @description Function taking a string of characters and transforming it to allow its interpretation as an expression or formula to be computed.
#' @param x Character. A string of character which can be interpreted as an expression.
#' @return Expression of formula which can then be computed to produce the numeric part of a proposition.
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @export


prepare_evaluation <- function(x){
  objects <- stringr::str_extract_all(x, "\\w+", simplify = TRUE)
  for (object in objects) x <- stringr::str_replace_all(
    x, object, base::paste0('get("', object, '")')
  )
  return(x)
}
