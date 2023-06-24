#' @name generate_md_files
#' @title Transform questions into MD files
#' @author Nicolas Mangin
#' @description Function creating tests to be printed out.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated feedback.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param test_folder Character. Path to the exam folder.
#' @return Path to the zip files containing the test(s)
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



generate_md_files <- function(
    test_parameters, propositions, translations, test_folder
){
  versions_folder <- base::paste0(test_folder, "/2_versions")
  exam_folder <- base::paste0(test_folder, "/5_examination")
  md_folder <- base::paste0(exam_folder, "/mdfiles")
  base::unlink(
    base::paste0(md_folder, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  shiny::req(base::dir.exists(test_folder))
  shiny::req(base::dir.exists(exam_folder))
  shiny::req(base::dir.exists(versions_folder))
  if (!base::dir.exists(md_folder)) base::dir.create(md_folder)
  record_solution <- FALSE
  docformat <- "pandoc"
  rmdfiles <- base::list.files(versions_folder, full.names = TRUE)
  for (file in rmdfiles) {
    newfile <- stringr::str_replace(file, "Rmd$", "md")
    newfile <- stringr::str_replace(newfile, versions_folder, md_folder)
    knitr::knit(file, newfile, envir = base::new.env(), quiet = TRUE)
  }
}
