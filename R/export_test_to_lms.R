#' @name export_test_to_lms
#' @title Export test to LMS.
#' @author Nicolas Mangin
#' @description Function creating tests to be uploaded on various Learning Management Systems.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated explanations.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param exam_folder Character. Path to the exam folder.
#' @param lms Character. Name of the learning management system for which the export should be generated.
#' @param language Character. ISO2 code of the printed language.
#' @param section Character. Name of the section.
#' @param bloc Character. Name of the bloc.
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom exams exams2arsnova
#' @importFrom exams exams2blackboard
#' @importFrom exams exams2canvas
#' @importFrom exams exams2ilias
#' @importFrom exams exams2moodle
#' @importFrom exams exams2openolat
#' @importFrom exams exams2particify
#' @importFrom exams exams2qti12
#' @importFrom exams exams2qti21
#' @importFrom exams exams2tcexam
#' @importFrom exams exams2testvision
#' @export



export_test_to_lms <- function(
    test_parameters, propositions, translations,
    exam_folder, lms = 'Moodle', language,
    section = "A", bloc = "A"
){
  
  points <- NULL
  
  testname <- test_parameters$test[1]
  docdir <- base::paste0(exam_folder, "/2_versions")
  tmpdir <- base::paste0(exam_folder, "/3_temporary")
  outdir <- base::paste0(exam_folder, "/5_examination")
  outdirlms <- base::paste0(outdir, "/", lms)
  if (!base::dir.exists(outdirlms)) base::dir.create(outdirlms)
  outdirlmslg <- base::paste0(outdirlms, "/", language)
  if (!base::dir.exists(outdirlmslg)) base::dir.create(outdirlmslg)
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  
  question_list <- test_parameters |>
    dplyr::arrange(section, bloc) |>
    dplyr::select(section, bloc, points, version)
  
  record_solution <- TRUE
  docformat <- "html"
  
  export_name <- base::paste0(section, "-", bloc)
  category_names <- stringr::str_remove_all(question_list$version, "-[0-9]+.Rmd")
  
  
  base::print(base::paste0(
    "Generating section ", section,
    " bloc ", bloc,
    " in ", language,
    " for ", lms,"..."
  ))
  
  if (lms == "Moodle"){
    
    exams::exams2moodle(
      file = question_list$version,
      n = 1,
      name = export_name,
      dir = outdirlmslg,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      stitle = category_names,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = FALSE,
      envir = base::new.env()
    )
    
  } else if (lms == "Canvas"){
    
    exams::exams2canvas(
      file = question_list$version,
      n = 1,
      name = export_name,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE,
      envir = base::new.env()
    )
    
  } else if (lms == "Blackboard"){
    
    test_parameters <<- test_parameters
    propositions <<- propositions
    translations <<- translations
    record_solution <<- record_solution
    docformat <<- docformat
    
    exams::exams2blackboard(
      file = question_list$version,
      n = 1,
      name = export_name,
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
      name = export_name,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg"),
      zip = TRUE,
      envir = base::new.env()
    )
    
  } else if (lms == "ARSnova"){
    
    test_parameters <<- test_parameters
    propositions <<- propositions
    translations <<- translations
    record_solution <<- record_solution
    docformat <<- docformat
    
    exams::exams2arsnova(
      file = question_list$version,
      n = 1,
      name = export_name,
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
    
    test_parameters <<- test_parameters
    propositions <<- propositions
    translations <<- translations
    record_solution <<- record_solution
    docformat <<- docformat
    
    exams::exams2particify(
      file = question_list$version,
      n = 1,
      name = export_name,
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
      name = export_name,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE,
      envir = base::new.env()
    )
    
  } else if (lms == "TCexam"){
    
    test_parameters <<- test_parameters
    propositions <<- propositions
    translations <<- translations
    record_solution <<- record_solution
    docformat <<- docformat
    
    exams::exams2tcexam(
      file = question_list$version,
      n = 1,
      name = export_name,
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
    
    test_parameters <<- test_parameters
    propositions <<- propositions
    translations <<- translations
    record_solution <<- record_solution
    docformat <<- docformat
    
    exams::exams2testvision(
      file = question_list$version,
      n = 1,
      name = export_name,
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
      name = export_name,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      zip = TRUE,
      envir = base::new.env()
    )
    
  } else if (lms == "QTI21"){
    
    exams::exams2qti21(
      file = question_list$version,
      n = 1,
      name = export_name,
      dir = outdir,
      edir = docdir,
      tdir = tmpdir,
      sdir = tmpdir,
      points = base::unlist(question_list$points),
      quiet = TRUE,
      verbose = FALSE,
      base64 = c("png", "jpg"),
      zip = TRUE,
      envir = base::new.env()
    )
    
  }
  
  base::Sys.sleep(3)
  
  base::unlink(
    base::paste0(tmpdir, "/*"),
    recursive = TRUE, force = TRUE, expand = TRUE
  )
  
  base::Sys.sleep(3)
}
