#' @name generate_docx_files
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
#' @importFrom rmarkdown render
#' @export


generate_docx_files <- function(
    test_parameters, propositions, translations, test_folder
){
  versions_folder <- base::paste0(test_folder, "/2_versions")
  exam_folder <- base::paste0(test_folder, "/5_examination")
  docx_folder <- base::paste0(exam_folder, "/docxfiles")
  base::unlink(
    base::paste0(docx_folder, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  shiny::req(base::dir.exists(test_folder))
  shiny::req(base::dir.exists(versions_folder))
  shiny::req(base::dir.exists(exam_folder))
  if (!base::dir.exists(docx_folder)) base::dir.create(docx_folder)
  record_solution <- FALSE
  docformat <- "pandoc"
  rmdfiles <- base::list.files(versions_folder, full.names = TRUE)
  for (file in rmdfiles) {
    newfile <- stringr::str_replace(file, "Rmd$", "docx")
    newfile <- stringr::str_replace(newfile, versions_folder, docx_folder)
    rmarkdown::render(
      input = file,
      output_format = "word_document",
      output_file = newfile,
      quiet = TRUE
    )
  }
}

