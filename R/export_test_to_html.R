#' @name export_test_to_html
#' @title Export test to HTML files
#' @author Nicolas Mangin
#' @description Function creating tests to be printed out.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated feedback.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param exam_folder Character. Path to the exam folder.
#' @param template_folder Character. Path to the html folder.
#' @param template Character. Name of the template to be used to format the test.
#' @param language Character. ISO2 code of the printed language.
#' @return Path to the zip files containing the test(s)
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @importFrom gtools permutations
#' @importFrom fs dir_delete
#' @export



export_test_to_html <- function(
  test_parameters, propositions, translations,
  exam_folder, template_folder, template, language
){
  
  section <- NULL
  bloc <- NULL
  altnbr <- NULL
  
  testname <- test_parameters$test[1]
  docdir <- base::paste0(exam_folder, "/2_versions")
  tmpdir <- base::paste0(exam_folder, "/3_temporary")
  outdir <- base::paste0(exam_folder, "/5_examination")
  direxam <- base::paste0(tmpdir,"/exam1")
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  
  questions_template <- base::paste0(
    template_folder, "/", template, "_questions_", language, ".html"
  )
  
  solutions_template <- base::paste0(
    template_folder, "/", template, "_solutions_", language, ".html"
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
    file = base::paste0(outdir, "/permutations.csv"),
    row.names = FALSE
  )
  
  tests <- base::list()
  mcq <- base::split(mcq, mcq$section)
  for (i in base::seq_len(base::nrow(test_permutations))){
    question_list <- mcq[test_permutations[i,]]
    question_list <- dplyr::bind_rows(question_list)$data
    question_list <- c(question_list, open$data)
    tests[[base::paste0(language, "_", i)]] <- question_list
  }
  
  test_date <- test_parameters$test_date[[1]]
  record_solution <- TRUE
  docformat <- "html"
  record_solution <<- record_solution
  docformat <<- docformat
  test_parameters <<- test_parameters
  propositions <<- propositions
  translations <<- translations
  
  for (test in base::names(tests)){
    
    test_version <- stringr::str_replace_all(test, "_", " ")
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam)) fs::dir_delete(direxam)
    base::Sys.sleep(3)
    
    exams::exams2html(
      file = tests[[test]],
      n = 1,
      question = TRUE,
      solution = FALSE,
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
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam)) fs::dir_delete(direxam)
    base::Sys.sleep(3)
    
    exams::exams2html(
      file = tests[[test]],
      n = 1,
      question = TRUE,
      solution = TRUE,
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
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam)) fs::dir_delete(direxam)
    base::Sys.sleep(3)
  }
  
  record_solution <<- NULL
  docformat <<- NULL
  test_parameters <<- NULL
  propositions <<- NULL
}
