#' @name export_test_to_files
#' @title Transform questions into MD, DOCX, HTML, or PDF files
#' @author Nicolas Mangin
#' @description Function creating tests to be printed out.
#' @param test_parameters Tibble. List of questions with associated parameters.
#' @param propositions Tibble. List of propositions, criteria, and associated feedback.
#' @param translations Tibble. Table containing translations of items and explanations.
#' @param test_folder Character. Path to the exam folder.
#' @param format Character. Whether the files should be MD, DOCX, HTML, or PDF
#' @param language Character. ISO2 code of the printed language.
#' @param template_folder Character. Path to the tex folder.
#' @param template Character. Name of the template to be used to format the test.
#' @return Path to the zip files containing the test(s)
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom utils globalVariables
#' @export



export_test_to_files <- function(
    test_parameters, propositions, translations,
    test_folder, format, language,
    template_folder = NA, template = NA
){
  
  altnbr <- NULL
  bloc <- NULL
  section <- NULL
  
  shiny::req(base::dir.exists(test_folder))
  docdir <- base::paste0(test_folder, "/2_versions")
  shiny::req(base::dir.exists(docdir))
  exam_folder <- base::paste0(test_folder, "/5_examination")
  shiny::req(base::dir.exists(exam_folder))
  
  if (format %in% c("MD","DOCX")){
    if (format == "MD"){
      outdir <- base::paste0(exam_folder, "/mdfiles")
      if (!base::dir.exists(outdir)) base::dir.create(outdir)
      record_solution <- TRUE
      docformat <- "html"
      rmdfiles <- base::paste0(docdir, "/", test_parameters$version)
      for (file in rmdfiles) {
        newfile <- stringr::str_replace(file, "Rmd$", "md")
        newfile <- stringr::str_replace(newfile, docdir, outdir)
        knitr::knit(file, newfile, envir = base::new.env(), quiet = TRUE)
      }
    } else {
      outdir <- base::paste0(exam_folder, "/docxfiles")
      if (!base::dir.exists(outdir)) base::dir.create(outdir)
      record_solution <- FALSE
      docformat <- "pandoc"
      rmdfiles <- base::paste0(docdir, "/", test_parameters$version)
      for (file in rmdfiles) {
        newfile <- stringr::str_replace(file, "Rmd$", "docx")
        newfile <- stringr::str_replace(newfile, docdir, outdir)
        rmarkdown::render(
          input = file,
          output_format = "word_document",
          output_file = newfile,
          quiet = TRUE
        )
      }
    }
    
  } else {
    
    tmpdir <- base::paste0(test_folder, "/3_temporary")
    base::unlink(
      base::paste0(tmpdir, "/*"),
      recursive = TRUE, force = TRUE, expand = TRUE
    )
    direxam <- base::paste0(tmpdir,"/exam1")
    if (base::dir.exists(direxam))
      base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
    
    mcq <- test_parameters |> dplyr::filter(altnbr > 0) |>
      dplyr::select(section, bloc, version) |>
      dplyr::group_by(section, bloc) |>
      dplyr::sample_n(1) |>
      tidyr::nest()
    
    open <- test_parameters |> dplyr::filter(altnbr == 0) |>
      dplyr::select(section, bloc, version) |>
      dplyr::group_by(section, bloc) |>
      tidyr::nest()
    
    nbrsections <- base::length(base::unique(mcq$section))
    tests <- base::list()
    
    if (nbrsections > 1){
      test_permutations <- testR::permute_sections(c(1:nbrsections))
      mcq <- base::split(mcq, mcq$section)
      for (i in base::seq_len(base::nrow(test_permutations))){
        question_list <- mcq[test_permutations[i,]]
        question_list <- dplyr::bind_rows(question_list)$data
        question_list <- c(question_list, open$data)
        tests[[base::paste0(language, "_", i)]] <- question_list
      }
    } else {
      tests[[base::paste0(language, "_0")]] <- c(mcq$data, open$data)
      test_permutations <- NA
    }
    
    record_solution <- FALSE
    
    if (format == "HTML"){
      outdir <- base::paste0(exam_folder, "/htmlfiles")
      if (!base::dir.exists(outdir)) base::dir.create(outdir)
      
      if (base::length(test_permutations) < 2){
        utils::write.csv(
          base::t(test_permutations),
          file = base::paste0(outdir, "/permutations.csv"),
          row.names = FALSE
        )
      }
      
      docformat <- "html"
      
      for (test in base::names(tests)){
        
        test_version <- stringr::str_replace_all(test, "_", " ")
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
        
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
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
        
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
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
        
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
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
      }
      
    } else {
      
      testname <- test_parameters$test[1]
      test_date <- test_parameters$test_date[[1]]
      
      outdir <- base::paste0(exam_folder, "/pdffiles")
      if (!base::dir.exists(outdir)) base::dir.create(outdir)
      
      if (base::length(test_permutations) < 2){
        utils::write.csv(
          base::t(test_permutations),
          file = base::paste0(outdir, "/permutations.csv"),
          row.names = FALSE
        )
      }
      
      questions_template <- base::paste0(
        template_folder, "/", template, "_questions_", language, ".tex"
      )
      solutions_template <- base::paste0(
        template_folder, "/", template, "_solutions_", language, ".tex"
      )
      
      docformat <- "latex"
      
      for (test in base::names(tests)){
        
        test_version <- stringr::str_replace_all(test, "_", " ")
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
        
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
          base64 = c("png", "jpg"),
          envir = base::new.env()
        )
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
        
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
          base64 = c("png", "jpg"),
          envir = base::new.env()
        )
        
        base::Sys.sleep(5)
        if (base::dir.exists(direxam))
          base::unlink(direxam, recursive = TRUE, force = FALSE, expand = TRUE)
        base::Sys.sleep(5)
      }
      
    }
    
  }
}
