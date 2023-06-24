#' @name export_test_to_html
#' @title Export test to HTML files
#' @author Nicolas Mangin
#' @description Function creating tests to be printed out.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated feedback.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param exam_folder Character. Path to the exam folder.
#' @param language Character. ISO2 code of the printed language.
#' @return Path to the zip files containing the test(s)
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



export_test_to_html <- function(
  test_parameters, propositions, translations,
  exam_folder, language
){
  
  section <- NULL
  bloc <- NULL
  altnbr <- NULL
  
  testname <- test_parameters$test[1]
  docdir <- base::paste0(exam_folder, "/2_versions")
  tmpdir <- base::paste0(exam_folder, "/3_temporary")
  outdir <- base::paste0(exam_folder, "/5_examination/htmlfiles")
  direxam <- base::paste0(tmpdir,"/exam1")
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  base::unlink(
    base::paste0(outdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
  if (!base::dir.exists(outdir)) base::dir.create(outdir)
  
  mcq <- test_parameters |> dplyr::filter(altnbr > 0) |>
    dplyr::select(section, bloc, version) |>
    dplyr::group_by(section, bloc) |>
    tidyr::nest()
  
  open <- test_parameters |> dplyr::filter(altnbr == 0) |>
    dplyr::select(section, bloc, version) |>
    dplyr::group_by(section, bloc) |>
    tidyr::nest()
  
  nbrsections <- base::length(base::unique(mcq$section))
  
  tests <- base::list()
  if (nbrsections > 1){
    test_permutations <- testR::permute_sections(c(1:nbrsections))
    utils::write.csv(
      base::t(test_permutations),
      file = base::paste0(outdir, "/permutations.csv"),
      row.names = FALSE
    )
    mcq <- base::split(mcq, mcq$section)
    for (i in base::seq_len(base::nrow(test_permutations))){
      question_list <- mcq[test_permutations[i,]]
      question_list <- dplyr::bind_rows(question_list)$data
      question_list <- c(question_list, open$data)
      tests[[base::paste0(language, "_", i)]] <- question_list
    }
  } else {
    tests[[base::paste0(language, "_0")]] <- c(mcq$data, open$data)
  }
  
  test_date <- test_parameters$test_date[[1]]
  record_solution <- TRUE
  docformat <- "html"
  
  for (test in base::names(tests)){
    
    test_version <- stringr::str_replace_all(test, "_", " ")
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam))
      base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
    base::Sys.sleep(3)
    
    exams::exams2html(
      file = tests[[test]],
      n = 1,
      question = TRUE,
      solution = FALSE,
      name = base::paste0("questions_", test),
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg"),
      envir = base::new.env()
    )
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam))
      base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
    base::Sys.sleep(3)
    
    exams::exams2html(
      file = tests[[test]],
      n = 1,
      question = FALSE,
      solution = TRUE,
      name = base::paste0("solutions_", test),
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg"),
      envir = base::new.env()
    )
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam))
      base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
    base::Sys.sleep(3)
    
    exams::exams2html(
      file = tests[[test]],
      n = 1,
      question = TRUE,
      solution = TRUE,
      name = base::paste0("all_", test),
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg"),
      envir = base::new.env()
    )
    
    base::Sys.sleep(3)
    if (base::dir.exists(direxam))
      base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
    base::Sys.sleep(3)
  }
  
}
