#' @name export_test_to_blackboard
#' @title Export test to blackboard
#' @author Nicolas Mangin
#' @description Function creating tests to be uploaded on Blackboard.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param feedbacks Tibble. List of propositions, criteria, and associated feedback.
#' @return Path to the zip files containing the test(s)
#' @importFrom shinybusy show_modal_spinner
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



export_test_to_blackboard <- function(test_parameters, feedbacks){
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the test is exported for Blackboard..."
  )
  
  section <- NULL
  bloc <- NULL
  points <- NULL
  
  initwd <- base::getwd()
  testname <- test_parameters$test[1]
  testwd <- base::paste0(initwd, "/5_tests/", testname)
  docdir <- base::paste0(testwd, "/b_versions")
  tmpdir <- base::paste0(testwd, "/c_tmp")
  outdir <- base::paste0(testwd, "/e_output")
  
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
  feedbacks <<- feedbacks
  
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
  feedbacks <<- NULL
  
  shinybusy::remove_modal_spinner()
}
