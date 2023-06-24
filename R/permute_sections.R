#' @name permute_sections
#' @title Permute Sections
#' @author Nicolas Mangin
#' @description Function creating permutations of the sections of a test.
#' @param sections Character vector. Names of the sections of the test.
#' @return Permutations.
#' @export


permute_sections <- function(sections) {
  n <- length(sections)
  if (n == 1) sections
  else {
    permutations <- NULL
    for (i in 1:n) {
      permutations <- base::rbind(
        permutations,
        base::cbind(sections[i], permute_sections(sections[-i]))
      )
    }
    permutations
  }
}

