#' @name edit_test_server
#' @title Create, edit, and publish tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to create, edit and publish tests from a questions bank.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param intake Reactive. Selected intake.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the test parameters in the relevant test sub-folder in the appropriate test subfolder.
#' @importFrom chartR draw_composition_barchart
#' @importFrom chartR draw_composition_heatmap
#' @importFrom chartR draw_composition_scatterplot
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr rename
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom editR display_question
#' @importFrom editR make_infobox
#' @importFrom editR selection_server
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom readr locale
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny dateInput
#' @importFrom shiny fluidRow
#' @importFrom shiny h3
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny tagList
#' @importFrom shiny updateSelectInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets multiInput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyWidgets switchInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom shinydashboard valueBox
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @export



edit_test_server <- function(
  id, filtered, course_data, intake, course_paths
){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      type <- NULL
      question <- NULL
      section <- NULL
      bloc <- NULL
      altnbr <- NULL
      points <- NULL
      partial_credits <- NULL
      penalty <- NULL
      seed <- NULL
      intake <- NULL
      institution <- NULL
      program <- NULL
      program_level <- NULL
      group <- NULL
      test_format <- NULL
      test_unit <- NULL
      test_assessment <- NULL
      test_documentation <- NULL
      test_languages <- NULL
      test_date <- NULL
      test_duration <- NULL
      test_points <- NULL
      show_version <- NULL
      show_points <- NULL
      data <- NULL
      version_nbr <- NULL
      nbrinbloc <- NULL
      version_id <- NULL
      language <- NULL
      versions <- NULL
      expected <- NULL
      expected_points <- NULL
      success <- NULL
      document_types <- NULL
      question_path <- NULL
      version_path <- NULL
      count <- NULL
      propositions <- NULL
      translations <- NULL
      folder <- NULL
      test_name <- NULL
      title <- NULL
      category <- NULL
      category1 <- NULL
      category2 <- NULL
      catvalue <- NULL
      discrimination <- NULL
      document_parameters <- NULL
      filtlevel <- NULL
      guess <- NULL
      position <- NULL
      text <- NULL
      value <- NULL
      numeric_position <- NULL
      flag <- NULL
      langiso <- NULL
      difficulty <- NULL
      code <- NULL
      tag <- NULL
      depth <- NULL
      test <- NULL
      
      modrval <- shiny::reactiveValues()
      
      output$testpattern <- shiny::renderUI({
        shinyWidgets::searchInput(
          inputId = ns("deftestpattern"),
          label = "Preselect based on pattern:", 
          btnSearch = shiny::icon("search"), 
          btnReset = shiny::icon("remove"),
          width = "100%"
        )
      })
      
      preselectedtests <- shiny::reactive({
        shiny::req(!base::is.na(course_data()$tests))
        shiny::req(!base::is.null(input$deftestpattern))
        preslcttests <- base::unique(stats::na.omit(course_data()$tests$test))
        if (base::nchar(input$deftestpattern) > 0) {
          preslcttests <- preslcttests[stringr::str_detect(preslcttests, input$deftestpattern)]
        }
        preslcttests
      })
      
      selecttest <- editR::selection_server("pslt", preselectedtests)
      
      
      # Preselected questions ##################################################
      
      questions <- shiny::reactive({
        shiny::req(!base::is.null(filtered()))
        filtered() |>
          dplyr::filter(type %in% c(
            "Free","Statements","Alternatives","Computation","Essay","Problem")
          )
      })
      
      
      
      # Select or create a test to edit ########################################
      
      shiny::observe({
        shiny::req(!base::is.null(selecttest()))
        modrval$test_folder <- base::paste0(
          course_paths()$subfolders$tests, "/", selecttest()
        )
        modrval$parameters_path <- base::paste0(
          modrval$test_folder, "/test_parameters.RData"
        )
        shiny::req(base::file.exists(modrval$parameters_path))
        base::load(modrval$parameters_path)
        modrval$test_parameters <- test_parameters
        modrval$questions_included <- stats::na.omit(
          base::unique(modrval$test_parameters$question)
        )
        tree_tests <- base::list.files(
          course_paths()$subfolders$tests, full.names = FALSE, recursive = FALSE
        )
        tree_tests <- tree_tests[!stringr::str_detect(tree_tests, "^archives$|^default$")]
        modrval$tree_tests <- tree_tests
        base::load(course_paths()$databases$propositions)
        modrval$propositions <- propositions
        base::load(course_paths()$databases$translations)
        modrval$translations <- translations
        answers <- base::list.files(base::paste0(
          modrval$test_folder, "/7_answers"
        ))
        check_answers <- base::any(stringr::str_detect(answers, ".csv$"))
        modrval$test_administered <- check_answers > 1
      })
      
      
      
      # Edit main test parameters ##############################################
      
      output$test_definition <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$test_parameters))
        base::list(
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::h3(modrval$test_parameters$test[1])
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                ns("def_test_format"), "Format",
                choices = c("quiz","mid-term","final","assignment","resit"),
                selected = modrval$test_parameters$test_format[1],
                width = "100%"
              ),
              shiny::selectInput(
                ns("def_test_unit"), "Unit",
                choices = c("student","team"),
                selected = modrval$test_parameters$test_unit[1],
                width = "100%"
              ),
              shiny::selectInput(
                ns("def_test_assessment"), "Assessment",
                choices = c("formative","summative"),
                selected = modrval$test_parameters$test_assessment[1],
                width = "100%"
              ),
              shiny::selectInput(
                ns("def_test_documentation"), "Documentation",
                choices = c("none","limited","open-book"),
                selected = modrval$test_parameters$test_documentation[1],
                width = "100%"
              ),
              shiny::selectInput(
                ns("def_test_languages"), "Languages",
                choices = course_data()$languages$langiso,
                selected = stringr::str_split(
                  modrval$test_parameters$test_languages[1], ";", simplify = TRUE
                ),
                multiple = TRUE,
                width = "100%"
              )
            ),
            shiny::column(
              6,
              shiny::dateInput(
                ns("def_test_date"), "Date",
                value = NULL,
                width = "100%"
              ),
              shiny::numericInput(
                ns("def_test_duration"), "Duration (minutes)",
                value = modrval$test_parameters$test_duration[1],
                width = "100%"
              ),
              shiny::numericInput(
                ns("def_test_points"), "Total points",
                value = modrval$test_parameters$test_points[1],
                width = "100%"
              ),
              shiny::fluidRow(
                shiny::column(
                  5,
                  shinyWidgets::switchInput(
                    ns("def_show_version"), "Show version name",
                    value = modrval$test_parameters$show_version[1],
                    onStatus = "success", offStatus = "danger",
                    width = "100%"
                  )
                ),
                shiny::column(
                  7,
                  shinyWidgets::switchInput(
                    ns("def_show_points"), "Show question points",
                    value = modrval$test_parameters$show_points[1],
                    onStatus = "success", offStatus = "danger",
                    width = "100%"
                  )
                )
              ),
              shiny::actionButton(
                ns("save_test_def"), "Save", icon = shiny::icon("floppy-disk"),
                style = "background-color:#006600;color:#FFF;
                width:100%;margin-top:10px;"
              )
            )
          )
        )
      })
      
      shiny::observeEvent(input$save_test_def, {
        test_parameters <- modrval$test_parameters |>
          dplyr::mutate(
            tree = modrval$test_parameters$tree[1],
            test = modrval$test_parameters$test[1],
            test_format = input$def_test_format,
            test_unit = input$def_test_unit,
            test_assessment = input$def_test_assessment,
            test_documentation = input$def_test_documentation,
            test_languages = base::paste(
              input$def_test_languages, collapse = ";"
            ),
            test_date = input$def_test_date,
            test_duration = input$def_test_duration,
            test_points = input$def_test_points,
            show_version = input$def_show_version,
            show_points = input$def_show_points
          )
        
        modrval$test_parameters <- test_parameters
        base::save(test_parameters, file = modrval$parameters_path)
        shinyalert::shinyalert(
          "Test information updated!",
          base::paste0('The test ', input$slcttest, ' has been updated.'),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      })
      
      
      
      # Select questions #######################################################
      
      output$selectoldtests <- shiny::renderUI({
        shiny::req(base::length(modrval$tree_tests) > 1)
        existing_tests <- base::setdiff(modrval$tree_tests, selecttest())
        shinyWidgets::multiInput(
          inputId = ns("slctpriortests"),
          label = "Select prior tests to identify untested questions",
          choices = existing_tests,
          width = "100%"
        )
      })
      
      shiny::observe({
        modrval$questions_preselected <- base::union(
          questions()$file,
          modrval$questions_included
        )
        if (base::is.null(input$slctpriortests)){
          modrval$questions_pretested <- base::character(0)
        } else {
          modrval$questions_pretested <- course_data()$tests |>
            dplyr::filter(test %in% input$slctpriortests) |>
            dplyr::select(question) |>
            base::unlist() |>
            base::as.character() |>
            base::sort()
        }
        
        modrval$preselected_untested <- base::setdiff(
          modrval$questions_preselected,
          modrval$questions_pretested
        ) |>
          base::sort()
        modrval$untested_included <- base::setdiff(
          modrval$questions_included,
          modrval$questions_pretested
        ) |>
          base::sort()
        
        modrval$preselected_pretested <- base::intersect(
          modrval$questions_preselected,
          modrval$questions_pretested
        ) |>
          base::sort()
        modrval$pretested_included <- base::intersect(
          modrval$questions_included,
          modrval$questions_pretested
        ) |>
          base::sort()
      })
      
      output$selectuntested <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$preselected_untested))
        shinyWidgets::multiInput(
          inputId = ns("slctuntested"),
          label = "Questions not tested in prior tests",
          choices = modrval$preselected_untested,
          selected = modrval$untested_included,
          width = "100%"
        )
      })
      
      output$selecttested <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$questions_pretested))
        shinyWidgets::multiInput(
          inputId = ns("slcttested"),
          label = "Questions already tested in prior tests",
          choices = modrval$preselected_pretested,
          selected = modrval$pretested_included,
          width = "100%"
        )
      })
      
      display_list <- shiny::reactive({
        shiny::req(!base::is.null(course_data()$documents))
        shiny::req(base::length(questions()) > 1)
        questorder <- modrval$untested_included |>
          base::union(modrval$pretested_included)
        add_selection <- base::sort(base::setdiff(
          modrval$questions_preselected,
          questorder
        ))
        questorder <- base::union(questorder, add_selection)
        sampled_questions <- course_data()$documents |>
          dplyr::filter(file %in% questorder) |>
          dplyr::arrange(base::match(file, questorder))
        if (base::nrow(sampled_questions) > 0){
          to_display <- sampled_questions$file
          base::names(to_display) <- base::paste0(
            sampled_questions$code, " - ",
            sampled_questions$title
          )
        }  else to_display <- base::character(0)
        to_display
      })
      
      question_to_display <- editR::selection_server(
        "slctquest2disp", display_list
      )
      
      output$viewquestionstats <- shiny::renderUI({
        shiny::req(!base::is.null(question_to_display()))
        shiny::req(question_to_display() != "")
        editR::make_infobox(course_data, question_to_display(), "results")
      })
      
      output$viewquestion <- shiny::renderUI({
        shiny::req(!base::is.null(questions()))
        shiny::req(question_to_display())
        shiny::req(question_to_display() %in% questions()$file)
        to_view <- questions() |>
          dplyr::filter(file == question_to_display())
        to_view$filepath <- base::paste0(
          course_paths()$subfolders$original, "/", to_view$file
        )
        editR::display_question(to_view, course_paths)
      })
      
      shiny::observeEvent(input$applyselection, {
        if (modrval$test_administered){
          shinyalert::shinyalert(
            title = "Changes not allowed!",
            text = "You cannot change the selection of questions when the test has been administered.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = TRUE, showConfirmButton = FALSE
          )
        } else {
          if (!base::is.null(input$slctuntested)){
            selection <- input$slctuntested
          } else selection <- base::character(0)
          if (!base::is.null(input$slcttested)){
            selection <- c(selection, input$slcttested)
          }
          
          test_parameters <- modrval$test_parameters
          test_information <- test_parameters[1,1:12]
          languages <- stringr::str_split(
            test_information$test_languages[1], ";", simplify = TRUE
          )
          
          remove_questions <- base::setdiff(
            base::unique(test_parameters$question),
            selection
          )
          keep_questions <- base::intersect(
            base::unique(test_parameters$question),
            selection
          )
          add_questions <- base::setdiff(
            selection,
            base::unique(test_parameters$question)
          )
          
          test_parameters <- test_parameters |>
            dplyr::filter(question %in% keep_questions)
          
          available_blocs <- base::setdiff(
            base::toupper(letters), base::unique(test_parameters$bloc)
          )
          
          add <- tibble::tibble(
            question = add_questions,
            section = "Z",
            bloc = available_blocs[base::seq_len(base::length(add_questions))],
            altnbr = 5,
            points = 0,
            partial_credits = 0,
            penalty = 0,
            version = stringr::str_extract(add_questions, "(?<=_)(.*)(?=.Rmd)")
          ) |>
            dplyr::mutate(
              version = base::paste0(version, section, bloc, 1, 1, ".Rmd"),
              seed = base::ceiling(stats::runif(base::length(add_questions)) * 10000)
            )
          
          add <- dplyr::bind_cols(test_information, add)
          
          test_parameters <- dplyr::bind_rows(test_parameters, add) |>
            stats::na.omit()
          
          # Manage files
          questinfold <- base::list.files(base::paste0(
            modrval$test_folder, "/1_questions/"
          ))
          slctedquest <- base::unique(test_parameters$question)
          rmvquest <- base::setdiff(questinfold, slctedquest)
          
          for (q in rmvquest){
            for (l in languages){
              file <- base::paste0(
                modrval$test_folder, "/1_questions/",
                stringr::str_replace(q, "_...Rmd$", base::paste0("_", l, ".Rmd"))
              )
              if (base::file.exists(file)) base::file.remove(file)
            }
          }
          
          for (q in slctedquest){
            for (l in languages){
              file <- stringr::str_replace(q, "_...Rmd$", base::paste0("_", l, ".Rmd"))
              origin <- base::paste0(
                course_paths()$subfolders$original, "/", file
              )
              if (!base::file.exists(origin)){
                origin <- base::paste0(
                  course_paths()$subfolders$translated, "/", file
                )
              }
              destination <- base::paste0(
                modrval$test_folder, "/1_questions/", file
              )
              if (base::file.exists(origin) & !base::file.exists(destination)){
                base::file.copy(
                  from = origin,
                  to = destination
                )
              }
            }
          }
          
          if (base::nrow(test_parameters) == 0){
            test_parameters <- test_information |>
              dplyr::mutate(
                question = base::as.character(NA),
                section = base::as.character(NA),
                bloc = base::as.character(NA),
                altnbr = base::as.integer(NA),
                points = base::as.integer(NA),
                partial_credits = base::as.logical(NA),
                penalty = base::as.logical(NA),
                version = base::as.character(NA),
                seed = base::as.integer(NA)
              )
          }
          
          modrval$test_parameters <- test_parameters
          base::save(test_parameters, file = modrval$parameters_path)
          
          shinyalert::shinyalert(
            title = "Selection of questions updated!",
            text = "Unselected questions have been removed and additional questions added to your test parameters.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = FALSE, showConfirmButton = TRUE
          )
        }
      })
      
      
      
      # Organize questions #####################################################
      
      output$question_organization <- rhandsontable::renderRHandsontable({
        input$update_question_organization
        shiny::req(!base::is.na(intake()$intake$intake[1]))
        shiny::req(base::nchar(modrval$parameters_path) > 3)
        shiny::req(base::file.exists(modrval$parameters_path))
        base::load(modrval$parameters_path)
        test_parameters |>
          dplyr::left_join(dplyr::select(
            course_data()$documents, question = file, language, title, type
          ), by = "question") |>
          dplyr::group_by(
            question, title, type, language, section, bloc, altnbr,
            points, partial_credits, penalty
          ) |>
          dplyr::summarise(
            version_nbr = dplyr::n(), .groups = "drop"
          ) |>
          dplyr::mutate(
            partial_credits = base::as.logical(partial_credits),
            penalty = base::as.logical(penalty)
          ) |>
          dplyr::arrange(section, bloc) |>
          rhandsontable::rhandsontable(
            width = "80%", rowHeaders = NULL, stretchH = "all"
          ) |>
          rhandsontable::hot_col(1:4, readOnly = TRUE) |>
          rhandsontable::hot_cols(
            colWidths = c("10%","40%","10%","5%","5%","5%","5%","5%","5%","5%","5%")
          ) |>
          rhandsontable::hot_context_menu(
            allowRowEdit = TRUE, allowColEdit = FALSE
          )
      })
      
      shiny::observeEvent(input$save_question_organization, {
        shiny::req(!base::is.null(input$question_organization))
        if (modrval$test_administered){
          
          shinyalert::shinyalert(
            title = "Changes not allowed",
            text = "You cannot change the organization of questions when the test has been administered.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = TRUE, showConfirmButton = FALSE
          )
          
        } else {
          
          test_parameters <- modrval$test_parameters
          test_information <- test_parameters[1,1:12]
          languages <- stringr::str_split(
            test_information$test_languages[1], ";", simplify = TRUE
          )
          
          initial_versions <- test_parameters |>
            dplyr::select(question, version, seed) |>
            base::unique()
          
          updatequest <- rhandsontable::hot_to_r(input$question_organization) |>
            dplyr::select(-title, -type) |>
            dplyr::group_by(section, bloc) |>
            tidyr::nest() |>
            dplyr::mutate(data = purrr::map(
              data,
              function(x) tibble::rowid_to_column(as.data.frame(x), "nbrinbloc")
            )) |>
            tidyr::unnest(data) |>
            dplyr::ungroup() |>
            dplyr::mutate(version_nbr = purrr::map(
              version_nbr,
              function(x) {
                y <- base::seq_len(base::ceiling(base::as.numeric(x)))
                nc <- base::nchar(base::max(y))
                z <- purrr::map_chr(y, function(y, nc) {
                  base::paste(c(base::rep(0, (nc - base::nchar(y))), y), collapse = "")
                }, nc)
                return(z)
              }
            )) |>
            tidyr::unnest(version_nbr) |>
            base::unique()
          
          shiny::req(base::nrow(updatequest) > 0)
          
          updatequest <- updatequest |>
            dplyr::mutate_if(base::is.factor, base::as.character) |>
            dplyr::mutate(version = base::paste0(
              language, "-", section, "-", bloc, nbrinbloc, "-",version_nbr, ".Rmd"
            )) |>
            dplyr::select(-version_nbr, -version_id, -nbrinbloc) |>
            dplyr::left_join(initial_versions, by = c("question", "version")) |>
            dplyr::mutate(
              seed = purrr::map_dbl(seed, function(x){
                if (base::is.na(x)) base::ceiling(stats::runif(1)*10000) else x
              })
            ) |>
            dplyr::mutate(seed = base::as.integer(seed))
          
          updatequest <- dplyr::bind_cols(test_information, updatequest) |>
            dplyr::select(base::names(test_parameters))
          
          test_parameters <- updatequest
          
          base::save(test_parameters, file = modrval$parameters_path)
          
          for (i in base::seq_len(base::nrow(test_parameters))){
            for (l in languages){
              q <- test_parameters$question[i]
              q <- stringr::str_replace(q, "_...Rmd$", base::paste0("_", l, ".Rmd"))
              q <- base::paste0(modrval$test_folder, "/1_questions/", q)
              v <- test_parameters$version[i]
              v <- stringr::str_replace(v, "^..", l)
              v <- base::paste0(modrval$test_folder, "/2_versions/", v)
              lines <- base::readLines(q)
              if (!base::any(stringr::str_detect(
                  lines,
                  "exextra\\[type\\]:[ ]{0,3}Free"
                ))){
                vid <- stringr::str_replace(test_parameters$version[i], "^..", l)
                lines[2] <- base::paste0('versionid <- "', vid,'"')
              }
              base::writeLines(lines, v, useBytes = TRUE)
            }
          }
          
          modrval$test_parameters <- test_parameters
          
          all_versions <- base::list.files(base::paste0(
            modrval$test_folder, "/2_versions"
          ), full.names = FALSE)
          
          all_expected <- base::character(0)
          for (l in languages){
            all_expected <- c(
              all_expected,
              stringr::str_replace_all(
                base::unique(test_parameters$version), "^..", l
              )
            )
          }
          
          remove <- base::setdiff(all_versions, all_expected)
          if (base::length(remove) > 0){
            base::file.remove(base::paste(
              base::paste0(modrval$test_folder, "/2_versions/"),
              remove, sep = ""
            ))
          }
          
          shinyalert::shinyalert(
            title = "Questions and versions updated!",
            text = "Check all the versions of all the question before producing the test.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        }
      })
      
      output$test_statistics <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(base::nrow(modrval$test_parameters) > 0)
        base_test_statistics <- modrval$test_parameters |>
          dplyr::select(
            file = question, section, bloc, points, test_points, test_duration
          ) |>
          dplyr::left_join(
            dplyr::select(course_data()$documents, file, code, language),
            by = "file"
          ) |>
          dplyr::left_join(course_data()$document_parameters, by = "file") |>
          dplyr::select(
            file, section, bloc, points, test_points, test_duration, success
          )
        
        base_test_statistics <- base_test_statistics |>
          tidyr::replace_na(base::list(
            success = base::mean(base_test_statistics$success, na.rm = TRUE)
          )) |>
          dplyr::mutate(expected = points * success / 100)
        
        distinct_questions <- base_test_statistics |>
          dplyr::group_by(section, bloc) |>
          dplyr::summarise(
            count = 1,
            points = base::mean(points, na.rm = TRUE),
            test_points = base::max(test_points, na.rm = TRUE),
            test_duration = base::max(test_duration, na.rm = TRUE),
            expected = base::mean(expected, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::summarise(
            count = base::sum(count),
            points = base::sum(points, na.rm = TRUE),
            test_points = base::max(test_points, na.rm = TRUE),
            test_duration = base::max(test_duration, na.rm = TRUE),
            expected = base::sum(expected, na.rm = TRUE)
          ) |>
          dplyr::mutate(
            minute_per_question = base::round(test_duration / count, 2),
            percent_success = base::round(expected / points, 2),
            expected = base::round(expected, 2)
          ) |>
          dplyr::mutate(
            points_color = dplyr::case_when(
              points == test_points ~ "green",
              TRUE ~ "red"
            ),
            minpquest_color = dplyr::case_when(
              minute_per_question <= 2 ~ "red",
              minute_per_question <= 3 ~ "orange",
              minute_per_question <= 5 ~ "olive",
              TRUE ~ "green"
            ),
            expect_color = dplyr::case_when(
              percent_success <= 0.4 ~ "red",
              percent_success <= 0.6 ~ "orange",
              percent_success <= 0.8 ~ "olive",
              TRUE ~ "green"
            )
          )
        
        shiny::fluidRow(
          shiny::column(
            3,
            shinydashboard::valueBox(
              distinct_questions$count,
              "Questions",
              icon = shiny::icon("circle-question"),
              color = "blue",
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              distinct_questions$minute_per_question,
              "Minutes per question",
              icon = shiny::icon("stopwatch"),
              color = distinct_questions$minpquest_color,
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              distinct_questions$points,
              "Maximum points",
              icon = shiny::icon("award"),
              color = distinct_questions$points_color,
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              distinct_questions$expected,
              "Expected average",
              icon = shiny::icon("arrow-up-9-1"),
              color = distinct_questions$expect_color,
              width = 12
            )
          )
        )
      })
      
      
      
      # Test composition #######################################################
      
      selected_positions <- shiny::reactive({
        
        levnbr <- stringr::str_count(intake()$tbltree$position[1], "-")+1
        depthlevel <- base::min(input$slctdepth, levnbr)
        depthlevel <- base::paste0("LEV", depthlevel)
        
        selected_positions <- filtered() |>
          dplyr::select(file) |>
          dplyr::left_join(dplyr::select(intake()$tbltree, file, position, title), by = "file") |>
          tidyr::separate(position, into = base::paste0("LEV", base::seq_len(levnbr)), sep = "-", fill = "right", remove = FALSE) |>
          dplyr::rename(depth = dplyr::all_of(depthlevel), section = title)
        
        selected_positions |>
          dplyr::arrange(position) |>
          dplyr::group_by(depth) |>
          dplyr::slice_head(n = 1) |>
          dplyr::ungroup() |>
          dplyr::select(depth, section) |>
          dplyr::left_join(dplyr::select(selected_positions, depth, position, title), by = "depth") |>
          dplyr::select(section, position)
      })
      
      composition <- shiny::reactive({
        shiny::req(!base::is.na(intake()$intake$intake[1]))
        base::load(course_paths()$databases$document_parameters)
        modrval$test_parameters |>
          dplyr::group_by(section, bloc) |>
          dplyr::sample_n(1) |>
          dplyr::ungroup() |>
          dplyr::select(question, points) |>
          base::unique() |>
          dplyr::mutate(count = 1) |>
          dplyr::left_join(
            dplyr::select(intake()$tbltree, question = file, position, type, dplyr::starts_with("tag_")), 
            by = "question"
          ) |>
          dplyr::left_join(
            dplyr::select(document_parameters, question = file, difficulty, discrimination, guess), 
            by = "question"
          ) |>
          dplyr::left_join(selected_positions(), by = "position") |>
          dplyr::mutate(
            category1 = base::get(input$slctdim1),
            category2 = base::get(input$slctdim2),
            value = base::get(input$slctval)
          )
      })
      
      categorical_values <- shiny::reactive({
        shiny::req(!base::is.na(intake()$course[1]))
        base::load(course_paths()$databases$doctypes)
        base::load(course_paths()$databases$tags)
        positions <- selected_positions() |>
          dplyr::arrange(position) |>
          dplyr::mutate(
            category = "section",
            catvalue = section
          )
        tags <- tags |>
          dplyr::arrange(order)
        dplyr::bind_rows(
          dplyr::select(positions, category, catvalue),
          tibble::tibble(
            category = "type",
            catvalue = c("Free","Statements","Alternatives","Computation","Essay","Problem"),
            order = 1:6
          ),
          dplyr::select(tags, category = tag, catvalue = value)
        )
      })
      
      shiny::observe({
        shiny::updateSelectInput(
          session,
          "slctdim1",
          choices = base::unique(c("section", categorical_values()$category))
        )
        shiny::updateSelectInput(
          session,
          "slctdim2",
          choices = base::unique(c("type", categorical_values()$category))
        )
      })
      
      categorical_values1 <- shiny::reactive({
        dplyr::rename(categorical_values(), category1 = category) |>
          dplyr::filter(category1 == input$slctdim1) |>
          dplyr::select(category1 = catvalue) |>
          base::unique()
      })
      
      categorical_values2 <- shiny::reactive({
        dplyr::rename(categorical_values(), category2 = category) |>
          dplyr::filter(category2 == input$slctdim2) |>
          dplyr::select(category2 = catvalue) |>
          base::unique()
      })
      
      output$scatterplot <- shiny::renderPlot({
        chartR::draw_composition_scatterplot(composition())
      })
      
      output$barchart <- shiny::renderPlot({
        chartR::draw_composition_barchart(
          composition(), 
          input$slctdim1,
          categorical_values1()
        )
      })
      
      output$heatmap <- shiny::renderPlot({
        chartR::draw_composition_heatmap(
          composition(),
          input$slctdim1, input$slctdim2,
          categorical_values1(), categorical_values2()
        )
      })
      
      
      
      # Edit questions #########################################################
      
      output$slcteditquestlang <- shiny::renderUI({
        languages <- stringr::str_split(
          modrval$test_parameters$test_languages[1], ";", simplify = TRUE
        )
        shinyWidgets::radioGroupButtons(
          inputId = ns("slcteditlanguage"),
          label = "Language",
          choices = languages,
          selected = languages[1],
          status = "primary", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      edit_list <- shiny::reactive({
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(!base::is.null(input$slcteditlanguage))
        shiny::req(base::dir.exists(modrval$test_folder))
        questions <- base::list.files(base::paste0(
          modrval$test_folder, "/1_questions"
        ))
        questions[stringr::str_detect(questions, input$slcteditlanguage)]
      })
      
      question_to_edit <- editR::selection_server("slctquestion2edit", edit_list)
      
      output$edittestquestion <- shiny::renderUI({
        shiny::req(!base::is.null(question_to_edit()))
        filepath <- base::paste0(
          modrval$test_folder, "/1_questions/", question_to_edit()
        )
        if (base::file.exists(filepath)){
          lines <- base::readLines(filepath)
          shiny::fluidRow(
            shiny::column(12, shinyAce::aceEditor(
              outputId = ns("editedtestquest"), value = lines,
              mode = "markdown", wordWrap = TRUE, debounce = 10,
              autoComplete = "live", height = "750px"
            ))
          )
        }
      })
      
      shiny::observeEvent(input$savetestquestion, {
        if (modrval$test_administered){
          shinyalert::shinyalert(
            title = "Changes not allowed",
            text = "You cannot change the questions when the test has been administered.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = TRUE, showConfirmButton = FALSE
          )
        } else {
          selected_question <- shiny::isolate({ question_to_edit() })
          edited_question <- shiny::isolate({ input$editedtestquest })
          selected_language <- shiny::isolate({ input$slcteditlanguage })
          shiny::req(!base::is.null(selected_question))
          shiny::req(!base::is.null(edited_question))
          shiny::req(!base::is.null(selected_language))
          base::writeLines(
            edited_question,
            base::paste0(modrval$test_folder, "/1_questions/", selected_question),
            useBytes = TRUE
          )
          versions_to_change <- modrval$test_parameters |>
            dplyr::select(question, version) |>
            dplyr::mutate(
              question = stringr::str_replace_all(
                question, "_...Rmd$", base::paste0("_", selected_language, ".Rmd")
              ),
              version = stringr::str_replace_all(version, "^..", selected_language)
            ) |>
            dplyr::filter(question == question_to_edit()) |>
            dplyr::mutate(
              question_path = base::paste0(modrval$test_folder, "/1_questions/", question),
              version_path = base::paste0(modrval$test_folder, "/2_versions/", version)
            )
            
          for (i in base::seq_len(base::nrow(versions_to_change))){
            lines <- base::readLines(versions_to_change$question_path[i])
            if (!base::any(stringr::str_detect(
              lines,
              "exextra\\[type\\]:[ ]{0,3}Free"
            ))){
              lines[2] <- base::paste0(
                'versionid <- "', versions_to_change$version[i],'"'
              )
            }
            base::writeLines(
              lines, versions_to_change$version_path[i], useBytes = TRUE
            )
          }
        }
      })
      
      output$viewtestquestion <- shiny::renderUI({
        shiny::req(question_to_edit())
        shiny::req(!base::is.null(input$slcteditlanguage))
        input$savetestquestion
        document_to_edit <- course_data()$documents |>
          dplyr::mutate(file = stringr::str_replace_all(
            file, "_...Rmd$", base::paste0("_", input$slcteditlanguage, ".Rmd")
          )) |>
          dplyr::filter(file == question_to_edit()) |>
          dplyr::mutate(filepath = base::paste0(
            modrval$test_folder, "/1_questions/", question_to_edit()
          ))
        shiny::req(base::nrow(document_to_edit) > 0)
        editR::display_question(document_to_edit, course_paths)
      })
      
      
      
      # Check ##################################################################
      
      output$selectlanguage <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        languages <- test_parameters$test_languages[1] |>
          stringr::str_split(";", simplify = TRUE) |>
          base::as.character()
        exam_languages <- course_data()$languages |>
          dplyr::select(langiso, language, flag) |>
          dplyr::filter(langiso %in% languages)
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctlanguage"), label = NULL,
          choiceNames = base::lapply(
            base::seq_along(exam_languages$langiso), 
            function(i) shiny::tagList(
              shiny::tags$img(src = exam_languages$flag[i], width = 20, height = 15),
              exam_languages$language[i]
            )
          ),
          choiceValues = exam_languages$langiso,
          status = "primary", justified = FALSE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$selectsection <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(test_parameters))
        shiny::req(base::nrow(test_parameters) > 0)
        existing_sections <- base::unique(test_parameters$section)
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctsection"),
          label = "Section",
          choices = existing_sections,
          selected = existing_sections[1],
          status = "info", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$selectbloc <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(input$slctsection))
        existing_blocs <- test_parameters |>
          dplyr::filter(section == input$slctsection)
        existing_blocs <- base::unique(existing_blocs$bloc)
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctbloc"),
          label = "Bloc",
          choices = existing_blocs,
          selected = existing_blocs[1],
          status = "success", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$selectquestion <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(input$slctlanguage))
        shiny::req(!base::is.null(input$slctsection))
        shiny::req(!base::is.null(input$slctbloc))
        test_parameters <- test_parameters |>
          dplyr::mutate(
            question = stringr::str_replace_all(
              question, "_...Rmd$",
              base::paste0("_", input$slctlanguage, ".Rmd")
            ),
            version = stringr::str_replace_all(version, "^..", input$slctlanguage)
          )
        existing_questions <- test_parameters |>
          dplyr::filter(section == input$slctsection, bloc == input$slctbloc)
        existing_questions <- base::unique(existing_questions$question)
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctquestion"),
          label = "Question",
          choices = existing_questions,
          selected = existing_questions[1],
          status = "warning", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$selectversion <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(input$slctlanguage))
        shiny::req(!base::is.null(input$slctquestion))
        test_parameters <- test_parameters |>
          dplyr::mutate(
            question = stringr::str_replace_all(
              question, "_...Rmd$",
              base::paste0("_", input$slctlanguage, ".Rmd")
            ),
            version = stringr::str_replace_all(version, "^..", input$slctlanguage)
          )
        existing_versions <- test_parameters |>
          dplyr::filter(question == input$slctquestion)
        existing_versions <- base::unique(existing_versions$version)
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctversion"),
          label = "Version",
          choices = existing_versions,
          selected = existing_versions[1],
          status = "danger", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$editseed <- shiny::renderUI({
        selecttest()
        input$refresh_organization
        shiny::req(!base::is.null(input$slctversion))
        shiny::req(stringr::str_detect(modrval$test_parameters$version[1], input$slctlanguage))
        seed <- modrval$test_parameters |>
          dplyr::filter(version == input$slctversion)
        seed <- seed$seed
        shiny::numericInput(
          ns("newseed"), "Seed", value = seed
        )
      })
      
      shiny::observeEvent(input$savenewseed, {
        shiny::req(!base::is.null(input$newseed))
        if (modrval$test_administered){
          shinyalert::shinyalert(
            title = "Changes not allowed",
            text = "You cannot change a seed when the test has been administered.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = TRUE, showConfirmButton = FALSE
          )
        } else {
          test_parameters <- modrval$test_parameters |>
            dplyr::mutate(
              seed = dplyr::case_when(
                version == input$slctversion ~ input$newseed,
                TRUE ~ seed
              )
            )
          base::save(test_parameters, file = modrval$parameters_path)
          modrval$test_parameters <- test_parameters
        }
      })
      
      # Display
      
      output$displayversion <- shiny::renderUI({
        selecttest()
        shiny::req(!base::is.null(input$slctversion))
        version_to_view <- course_data()$documents |>
          dplyr::left_join(
            dplyr::select(modrval$test_parameters, file = question, version),
            by = "file"
          ) |>
          dplyr::mutate(
            version = stringr::str_replace_all(version, "^..", input$slctlanguage)
          ) |>
          dplyr::filter(version == input$slctversion) |>
          dplyr::mutate(filepath = base::paste0(
            modrval$test_folder, "/2_versions/", input$slctversion
          ))
        new_test_parameters <- modrval$test_parameters |>
          dplyr::mutate(
            version = stringr::str_replace_all(version, "^..", input$slctlanguage)
          )
        shiny::req(base::nrow(version_to_view) == 1)
        editR::display_question(version_to_view, course_paths, new_test_parameters)
      })
      
      
      
      # Publish ################################################################
      
      output$slctpdftemplate <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctfileformat))
        if ("PDF" %in% input$slctfileformat){
          templates <- base::list.files(course_paths()$subfolders$exams)
          templates <- templates[stringr::str_detect(templates, "tex$")]
          templates <- base::unique(stringr::str_remove_all(
            templates, "_questions_...tex$|_solutions_...tex$"
          ))
          shinyWidgets::radioGroupButtons(
            inputId = ns("slctexamtemplate"),label = "Template:", 
            choices = templates,
            status = "info", justified = FALSE, size = "sm",
            checkIcon = base::list(yes = shiny::icon("check"))
          )
        }
      })
      
      shiny::observeEvent(input$export_to_file, {
        test_parameters <- modrval$test_parameters
        languages <- stringr::str_split(
          test_parameters$test_languages[1], ";", simplify = TRUE
        )
        shiny::req(base::length(languages)>0)
        formats <- base::unique(c("MD",input$slctfileformat))
        shiny::req(base::length(formats)>0)
        pubnbr <- base::length(languages)*base::length(formats)
        pgr <- 0
        shinybusy::show_modal_progress_line(
          value = pgr, text = "Starting the publication process"
        )
        for (l in languages){
          test_parameters <- test_parameters |>
            dplyr::mutate(version = stringr::str_replace_all(version, "^..", l))
          for (f in formats){
            pgr <- pgr+1/pubnbr
            shinybusy::update_modal_progress(
              value = pgr,
              text = base::paste0("Generating ", l, " files for ", f,"...")
            )
            if (f == "PDF"){
              templatedir <- course_paths()$subfolders$exams
              template <- input$slctexamtemplate
            } else {
              templatedir <- NA
              template <- NA
            }
            testR::export_test_to_files(
              test_parameters,
              modrval$propositions,
              modrval$translations,
              modrval$test_folder,
              f,
              l,
              templatedir,
              template
            )
          }
        }
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = "Exports generated!",
          text = "You can now retrieve the files for the selected formats in the examination folder.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      })
      
      shiny::observeEvent(input$export_to_lms, {
        test_parameters <- modrval$test_parameters
        languages <- stringr::str_split(
          test_parameters$test_languages[1], ";", simplify = TRUE
        )
        shiny::req(base::length(languages)>0)
        shiny::req(base::length(input$slctlms)>0)
        pubnbr <- base::length(languages)*base::length(input$slctlms)
        pgr <- 0
        shinybusy::show_modal_progress_line(
          value = pgr, text = "Starting the publication progress"
        )
        for (l in languages){
          test_parameters <- test_parameters |>
            dplyr::mutate(version = stringr::str_replace_all(version, "^..", l))
          for (lms in input$slctlms){
            shinybusy::update_modal_progress(
              value = pgr,
              text = base::paste0("Generating ", l, " files for ", lms,"...")
            )
            pgr <- pgr+1/pubnbr
            testR::export_test_to_lms(
              test_parameters,
              modrval$propositions,
              modrval$translations,
              modrval$test_folder,
              lms, l
            )
          }
        }
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = "Exports generated!",
          text = "You can now retrieve the .zip files for the selected LMS in the examination folder.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      })
      
      # Open folder
      
      shiny::observeEvent(input$openexamfolder, {
        exam_folder <- base::paste0(modrval$test_folder, "/5_examination")
        if (base::dir.exists(exam_folder)){
          if (.Platform['OS.type'] == "windows"){
            shell.exec(exam_folder)
          } else {
            system2("open", exam_folder)
          }
        }
      })
      
      
    }
  )
}
