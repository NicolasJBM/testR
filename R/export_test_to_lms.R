#' @name export_test_to_lms
#' @title Export test to blackboard
#' @author Nicolas Mangin
#' @description Function creating tests to be uploaded on Blackboard.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated explanations.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param exam_folder Character. Path to the exam folder.
#' @param lms Character. Name of the learning management system for which the export should be generated.
#' @return Path to the zip files containing the test(s)
#' @importFrom shinybusy show_modal_spinner
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom exams exams2blackboard
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



export_test_to_lms <- function(
    test_parameters, propositions, translations, exam_folder, lms = 'Moodle'
){
  
  section <- NULL
  bloc <- NULL
  points <- NULL
  
  testname <- test_parameters$test[1]
  docdir <- base::paste0(exam_folder, "/2_versions")
  tmpdir <- base::paste0(exam_folder, "/3_temporary")
  outdir <- base::paste0(exam_folder, "/5_examination")
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  
  question_list <- test_parameters |>
    dplyr::arrange(section, bloc) |>
    dplyr::select(section, bloc, points, version) |>
    dplyr::group_by(section, bloc) |>
    dplyr::summarise(
      points = base::max(points), version = c(version), .groups = "drop"
    )
  
  record_solution <- TRUE
  as_latex <- FALSE
  record_solution <<- record_solution
  as_latex <<- as_latex
  test_parameters <<- test_parameters
  propositions <<- propositions
  translations <<- translations
  
  filename <- base::paste0(
    lms, "_", stringr::str_extract(test_parameters$version[1], "^..")
  )
  
  base::print(base::paste0("Export for ", lms, "."))
  
  if (lms == "Moodle"){
    
    exams::exams2moodle(
      file = question_list$version,
      n = 1,
      name = filename,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE
    )
    
  } else if (lms == "Canvas"){
    
    exams::exams2canvas(
      file = question_list$version,
      n = 1,
      name = filename,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE
    )
    
  } else if (lms == "Blackboard"){
    
    exams::exams2blackboard(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  } else if (lms == "OpenOlat"){
    
    exams::exams2openolat(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  } else if (lms == "ARSnova"){
    
    exams::exams2arsnova(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  } else if (lms == "Partificy"){
    
    exams::exams2particify(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  } else if (lms == "Ilias"){
    
    exams::exams2ilias(
      file = question_list$version,
      n = 1,
      name = filename,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE
    )
    
  } else if (lms == "TCexam"){
    
    exams::exams2tcexam(
      file = question_list$version,
      n = 1,
      name = filename,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE
    )
    
  } else if (lms == "Testvision"){
    
    exams::exams2testvision(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  } else if (lms == "QUI12"){
    
    exams::exams2qti12(
      file = question_list$version,
      n = 1,
      name = filename,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE
    )
    
  } else if (lms == "QTI21"){
    
    exams::exams2qti21(
      file = question_list$version,
      n = 1,
      name = filename,
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
    
  }
  
  record_solution <<- NULL
  as_latex <<- NULL
  test_parameters <<- NULL
  propositions <<- NULL
  translations <<- NULL
  
  base::Sys.sleep(3)
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  
  base::Sys.sleep(3)
}
