#' @name update_test_parameters
#' @title Update test parameters
#' @author Nicolas Mangin
#' @description Function adding missing questions as unclassified, removing non-existing questions, and updating question information in test parameters from a document list.
#' @param documents Tibble. List of documents
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map2_chr
#' @export


update_test_parameters <- function(documents){
  
  group <- NULL
  institution <- NULL
  language <- NULL
  other_languages <- NULL
  path <- NULL
  program <- NULL
  program_level <- NULL
  question <- NULL
  question_path <- NULL
  show_points <- NULL
  show_version <- NULL
  test <- NULL
  test_assessment <- NULL
  test_date <- NULL
  test_documentation <- NULL
  test_duration <- NULL
  test_format <- NULL
  test_languages <- NULL
  test_points <- NULL
  test_unit <- NULL
  tree <- NULL
  type <- NULL
  
  test_parameters <- documents |>
    dplyr::filter(type %in% c(
      "Statements","Alternatives","Computation","Essay","Problem")
    ) |>
    dplyr::mutate(
      tree = "unclassified",
      institution = "",
      program = "",
      program_level = "",
      group = "",
      test = "default",
      test_format = "",
      test_assessment = "",
      test_unit = "",
      test_documentation = "",
      test_languages = purrr::map2_chr(
        language, other_languages, function(x,y){
          base::paste(stats::na.omit(c(x,y)), collapse = " ")
        }
      ),
      test_date = base::Sys.Date(),
      test_duration = 0,
      test_points = 0,
      show_version = 0,
      show_points = 0
    ) |>
    dplyr::select(
      tree,
      institution,
      program,
      program_level,
      group,
      test,
      test_format,
      test_unit,
      test_assessment,
      test_documentation,
      test_languages,
      test_date,
      test_duration,
      test_points,
      show_version,
      show_points,
      question = file,
      question_path = path
    ) |>
    dplyr::mutate(
      section = base::character(1),
      bloc = base::character(1),
      altnbr = 4,
      points = 0,
      partial_credits = 0,
      penalty = 0,
      version = question,
      version_path = question_path,
      seed = 123456789
    )
  
  base::save(
    test_parameters,
    file = "5_tests/default/test_parameters.RData"
  )
  
  return(test_parameters)
}
