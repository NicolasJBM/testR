#' @name export_test_to_pdf
#' @title Export test to PDF files
#' @author Nicolas Mangin
#' @description Function creating tests to be printed out.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param feedbacks Tibble. List of propositions, criteria, and associated feedback.
#' @param template Character. Name of the template to be used to format the test.
#' @return Path to the zip files containing the test(s)
#' @importFrom shinybusy show_modal_spinner
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @importFrom gtools permutations
#' @importFrom fs dir_delete
#' @export



export_test_to_pdf <- function(test_parameters, feedbacks, template){
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the test is exported to PDF..."
  )
  
  section <- NULL
  bloc <- NULL
  altnbr <- NULL
  
  initwd <- base::getwd()
  testname <- test_parameters$test[1]
  testwd <- base::paste0(initwd, "/5_tests/", testname)
  docdir <- base::paste0(testwd, "/b_versions")
  tmpdir <- base::paste0(testwd, "/c_tmp")
  outdir <- base::paste0(testwd, "/e_output")
  direxam <- base::paste0(tmpdir,"/exam1")
  
  questions_template <- base::paste0(
    initwd, "/1_preparation/templates/test/", template, "_questions.tex"
  )
  
  solutions_template <- base::paste0(
    initwd, "/1_preparation/templates/test/", template, "_solutions.tex"
  )
  
  mcq <- test_parameters |> dplyr::filter(altnbr > 0) |>
    dplyr::select(section, bloc, version) |>
    dplyr::group_by(section, bloc) |>
    tidyr::nest()
  
  open <- test_parameters |> dplyr::filter(altnbr == 0) |>
    dplyr::select(section, bloc, version) |>
    dplyr::group_by(section, bloc) |>
    tidyr::nest()
  
  mcq_sections <- base::unique(mcq$section)
  mcq_sectionnbr <- base::length(mcq_sections)
  test_permutations <- gtools::permutations(mcq_sectionnbr, mcq_sectionnbr)
  
  utils::write.csv(
    base::t(test_permutations),
    file = base::paste0(outdir, "/permutations.csv")
  )
  
  tests <- base::list()
  mcq <- base::split(mcq, mcq$section)
  for (i in base::seq_len(base::nrow(test_permutations))){
    question_list <- mcq[test_permutations[i,]]
    question_list <- dplyr::bind_rows(question_list)$data
    question_list <- c(question_list, open$data)
    tests[[base::paste0("version_", i)]] <- question_list
  }
  
  test_date <- test_parameters$test_date[[1]]
  record_version <- TRUE
  as_latex <- TRUE
  record_version <<- record_version
  as_latex <<- as_latex
  test_parameters <<- test_parameters
  feedbacks <<- feedbacks
  
  for (test in base::names(tests)){
    
    test_version <- stringr::str_replace_all(test, "_", " ")
    
    base::Sys.sleep(5)
    if (base::dir.exists(direxam)) fs::dir_delete(direxam)
    
    exams::exams2pdf(
      file = tests[[test]],
      n = 1,
      name = base::paste0("questions_", test),
      template = questions_template,
      header = base::list(ID = test_version, Date = test_date),
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg")
    )
    
    base::Sys.sleep(5)
    if (base::dir.exists(direxam)) fs::dir_delete(direxam)
    
    exams::exams2pdf(
      file = tests[[test]],
      n = 1,
      name = base::paste0("solutions_", test),
      template = solutions_template,
      header = base::list(ID = test_version, Date = test_date),
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg")
    )
  }
  
  record_version <<- NULL
  as_latex <<- NULL
  test_parameters <<- NULL
  feedbacks <<- NULL
  
  shinybusy::remove_modal_spinner()
}
