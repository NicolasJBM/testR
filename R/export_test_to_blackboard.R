#' @name export_test_to_blackboard
#' @title Export test to blackboard
#' @author Nicolas Mangin
#' @description Function creating tests to be uploaded on Blackboard.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated explanations.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param exam_folder Character. Path to the exam folder.
#' @return Path to the zip files containing the test(s)
#' @importFrom shinybusy show_modal_spinner
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



export_test_to_blackboard <- function(
    test_parameters, propositions, translations, exam_folder
){
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the test is exported for Blackboard..."
  )
  
  section <- NULL
  bloc <- NULL
  points <- NULL
  
  testname <- test_parameters$test[1]
  docdir <- base::paste0(exam_folder, "/2_versions")
  tmpdir <- base::paste0(exam_folder, "/3_temporary")
  outdir <- base::paste0(exam_folder, "/4_solutions")
  
  base::unlink(tmpdir, recursive = TRUE, force = TRUE, expand = TRUE)
  base::dir.create(tmpdir)
  
  question_list <- test_parameters |>
    dplyr::arrange(section, bloc) |>
    dplyr::select(section, bloc, points, version) |>
    dplyr::group_by(section, bloc) |>
    dplyr::summarise(
      points = base::max(points), version = c(version), .groups = "drop"
    )
  
  record_version <- TRUE
  as_latex <- FALSE
  record_version <<- record_version
  as_latex <<- as_latex
  test_parameters <<- test_parameters
  propositions <<- propositions
  translations <<- translations
  
  exams::exams2blackboard(
    file = question_list$version,
    n = 1,
    name = "blackboard",
    dir = outdir,
    edir = docdir,
    tdir = tmpdir,
    sdir = tmpdir,
    points = base::unlist(question_list$points),
    quiet = TRUE,
    verbose = FALSE,
    base64 = c("png", "jpg"),
    zip = TRUE
  )
  
  record_version <<- NULL
  as_latex <<- NULL
  test_parameters <<- NULL
  propositions <<- NULL
  
  shinybusy::remove_modal_spinner()
}
