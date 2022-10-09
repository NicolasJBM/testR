#' @name edit_test_server
#' @title Create tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to create tests from a selection of questions.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Selected tree.
#' @param test Reactive.. Selected test.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the test parameters in the relevant test sub-folder in the folder "5_tests".
#' @import shiny
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom tibble tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr slice_sample
#' @importFrom dplyr case_when
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom shinyalert shinyalert
#' @importFrom fs dir_copy
#' @importFrom tidyr replace_na
#' @importFrom rhandsontable hot_to_r
#' @importFrom stats na.omit
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_if
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_cols
#' @importFrom shinybusy show_modal_spinner
#' @importFrom utils zip
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom exams exams2blackboard
#' @export



edit_test_server <- function(
  id, filtered, course_data, tree, test, course_paths
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
      tree <- NULL
      institution <- NULL
      program <- NULL
      program_level <- NULL
      group <- NULL
      test <- NULL
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
      
      modrval <- shiny::reactiveValues()
      
      
      
      # Preselected questions ##################################################
      
      questions <- shiny::reactive({
        shiny::req(!base::is.null(filtered()))
        filtered() |>
          dplyr::filter(type %in% c(
            "Statements","Alternatives","Computation","Essay","Problem")
          )
      })
      
      
      
      # Select or create a test to edit ########################################
      
      shiny::observe({
        shiny::req(!base::is.na(tree()$course[1]))
        shiny::req(base::nrow(test()) > 1)
        
        modrval$tree <- tree()$course[1]
        modrval$selected_test <- test()$test[1]
        modrval$tests <- base::unique(course_data()$tests$test)
        
        shiny::req(modrval$selected_test %in% modrval$tests)
        
        modrval$test_folder <- base::paste0(
          course_paths()$subfolders$tests, "/", modrval$selected_test
        )
        
        test_parameters <- test() |>
          dplyr::mutate(
            question_path = purrr::map_chr(
              question,
              function(x, y){base::paste0(y, "/", x)},
              modrval$test_folder
            )
          ) |>
          dplyr::mutate(
            version_path = purrr::map_chr(
              version,
              function(x, y){base::paste0(y, "/", x)},
              modrval$test_folder
            )
          )
        modrval$test_parameters <- test_parameters
        
        modrval$questions_included <- stats::na.omit(
          base::unique(test_parameters$question)
        ) 
        
        base::load(course_paths()$databases$propositions)
        modrval$propositions <- propositions
        
        base::load(course_paths()$databases$translations)
        modrval$translations <- translations
        
        answers <- base::list.files(base::paste0(
          modrval$test_folder, "/7_answers"
        ))
        check_answers <- base::length(base::unlist(answers))
        modrval$test_administered <- check_answers > 0
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
                choices = "US",
                selected = modrval$test_parameters$test_languages[1],
                multiple = TRUE,
                width = "100%"
              )
            ),
            shiny::column(
              6,
              shiny::dateInput(
                ns("def_test_date"), "Date",
                value = modrval$test_parameters$test_date[1],
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
              shinyWidgets::switchInput(
                ns("def_show_version"), "Show version name",
                value = modrval$test_parameters$show_version[1],
                onStatus = "success", offStatus = "danger",
                labelWidth = "150px", handleWidth = "50px"
              ),
              shinyWidgets::switchInput(
                ns("def_show_points"), "Show question points",
                value = modrval$test_parameters$show_points[1],
                onStatus = "success", offStatus = "danger",
                labelWidth = "150px", handleWidth = "50px"
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
              input$def_test_languages, collapse = "; "
            ),
            test_date = input$def_test_date,
            test_duration = input$def_test_duration,
            test_points = input$def_test_points,
            show_version = input$def_show_version,
            show_points = input$def_show_points
          )
        
        modrval$test_parameters <- test_parameters
        path_param <- base::paste0(
          modrval$test_folder, "/test_parameters.RData"
        )
        base::save(test_parameters, file = path_param)
        shinyalert::shinyalert(
          "Test information updated!",
          base::paste0('The test ', input$slcttest, ' has been updated.'),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      })
      
      
      
      # Select questions #######################################################
      
      output$selectoldtests <- shiny::renderUI({
        shiny::req(base::length(test()$test) > 0)
        existing_tests <- base::setdiff(modrval$tests, test()$test[1])
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
      
      output$question2display <- shiny::renderUI({
        questorder <- modrval$untested_included |>
          base::union(modrval$pretested_included)
        add_selection <- base::sort(base::setdiff(
          modrval$questions_preselected,
          questorder
        ))
        questorder <- questorder |>
          base::union(add_selection)
        sampled_questions <- course_data()$documents |>
          dplyr::filter(file %in% base::union(
            modrval$questions_included, questions()$file
          )) |>
          dplyr::arrange(base::match(file, questorder))
        sampled <- sampled_questions$file
        base::names(sampled) <- base::paste(
          sampled_questions$file,
          sampled_questions$title,
          sep = " - "
        )
        shiny::selectInput(
          ns("slctquest2display"), "Select a question",
          choices = sampled, selected = sampled[1],
          width = "100%"
        )
      })
      
      output$viewquestionstats <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctquest2display))
        shiny::req(input$slctquest2display != "")
        editR::make_infobox(course_data, input$slctquest2display, "results")
      })
      
      output$viewquestion <- shiny::renderUI({
        shiny::req(!base::is.null(questions()))
        shiny::req(input$slctquest2display)
        shiny::req(input$slctquest2display %in% questions()$file)
        to_view <- questions() |>
          dplyr::filter(file == input$slctquest2display)
        to_view$filepath <- base::paste0(
          course_paths()$subfolders$original, "/", to_view$file
        )
        editR::view_document(to_view, TRUE, course_data, course_paths)
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
          test_information <- test_parameters[1,1:16]
          
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
          
          add <- tibble::tibble(
            question = add_questions,
            question_path = base::paste0(
              modrval$test_folder, "/1_questions/", add_questions
            ),
            section = "Z",
            bloc = base::toupper(
              letters[base::seq_len(base::length(add_questions))]
            ),
            altnbr = 5,
            points = 0,
            partial_credits = 0,
            penalty = 0
          ) |>
            dplyr::mutate(version = base::paste0(section,bloc,1,1,".Rmd")) |>
            dplyr::mutate(
              version_path = base::paste0(
                modrval$test_folder, "/2_versions/", version
              ),
              seed = base::ceiling(
                stats::runif(base::length(add_questions))* 10000
              )
            )
          
          add <- dplyr::bind_cols(test_information, add)
          
          test_parameters <- dplyr::bind_rows(test_parameters, add)
          
          base::save(test_parameters, file = base::paste0(
            modrval$test_folder, "/test_parameters.RData"
          ))
          
          for (q in remove_questions){
            file <- base::paste0(
              modrval$test_folder, "/1_questions/", q
            )
            if (base::file.exists(file)) base::file.remove(file)
          }
          
          for (q in add_questions){
            base::file.copy(
              from = base::paste0(course_paths()$subfolders$original, "/", q),
              to = base::paste0(
                modrval$test_folder, "/1_questions/", q
              ),
              overwrite = FALSE
            )
          }
          
          modrval$test_parameters <- test_parameters
          
          shinyalert::shinyalert(
            title = "Selection of questions updated!",
            text = "Unselected questions have been removed and additional questions added to your test parameters.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
            showCancelButton = TRUE, showConfirmButton = FALSE
          )
        }
      })
      
      # Organize questions #####################################################
      
      output$question_organization <- rhandsontable::renderRHandsontable({
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(base::nrow(modrval$test_parameters) > 0)
        modrval$test_parameters |>
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
          rhandsontable::hot_col(c(1:4), readOnly = TRUE) |>
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
          
          test_info <- test_parameters[1,1:16]
          
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
            dplyr::mutate(version_nbr = purrr::map_int(
              version_nbr,
              function(x) base::as.integer(base::ceiling(base::min(9,x)))
            )) |>
            dplyr::mutate(version_nbr = purrr::map(
              version_nbr,
              function(x) base::seq_len(x)
            )) |>
            tidyr::unnest(version_nbr) |>
            base::unique()
          
          shiny::req(base::nrow(updatequest) > 0)
          
          updatequest <- updatequest |>
            dplyr::mutate_if(base::is.factor, base::as.character) |>
            dplyr::mutate(version = base::paste0(
              language, section, bloc, nbrinbloc, version_nbr, ".Rmd"
            )) |>
            dplyr::select(-version_nbr, -version_id, -nbrinbloc) |>
            dplyr::left_join(initial_versions, by = c("question", "version")) |>
            dplyr::mutate(
              seed = purrr::map_dbl(seed, function(x){
                if (base::is.na(x)) base::ceiling(stats::runif(1)*10000) else x
              })
            ) |>
            dplyr::mutate(seed = base::as.integer(seed)) |>
            dplyr::mutate(
              question_path = base::paste0(
                modrval$test_folder, "/1_questions/", question
              ),
              version_path = base::paste0(
                modrval$test_folder, "/2_versions/", version
              )
            )
          
          updatequest <- dplyr::bind_cols(test_info, updatequest) |>
            dplyr::select(base::names(test_parameters))
          
          test_parameters <- updatequest
          
          base::save(test_parameters, file = base::paste0(
            modrval$test_folder,
            "/test_parameters.RData"
          ))
          
          
          
          
          
          ######################################################################
          ####   NEED TO ADD THE TRANSFER OF QUESTIONS AND CREATION OF     #####
          ####   VERSIONS IN OTHER LANGUAGES HERE                          #####
          ####   THESE VERSIONS SHOULD BE APPENDED TO PARAMETERS           #####
          ####   TO ALLOW FOR DEFFERENT SEEDS                              #####
          ######################################################################
          
          
          
          
          
          for (i in base::seq_len(base::nrow(test_parameters))){
            lines <- base::readLines(test_parameters$question_path[i])
            lines[2] <- base::paste0(
              'versionid <- "', test_parameters$version[i],'"'
            )
            base::writeLines(
              lines, test_parameters$version_path[i], useBytes = TRUE
            )
          }
          
          modrval$test_parameters <- test_parameters
          
          remove <- base::setdiff(
            base::list.files(base::paste0(
              modrval$test_folder, "/2_versions"
            ), full.names = FALSE),
            base::unique(test_parameters$version)
          )
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
        
        base_test_statistics <- modrval$test_parameters |>
          dplyr::select(
            file = question, section, bloc, points, test_points, test_duration
          ) |>
          dplyr::left_join(
            dplyr::select(course_data()$documents, file, code, language),
            by = "file"
          ) |>
          dplyr::left_join(course_data()$document_parameters, by = c("file")) |>
          dplyr::select(
            file, section, bloc, points, test_points, test_duration, success
          )
        
        base_test_statistics <- base_test_statistics |>
          tidyr::replace_na(base::list(
            success = base::mean(base_test_statistics$success, na.rm = TRUE)
          )) |>
          dplyr::mutate(expected = points * success)
        
        dictinct_questions <- base_test_statistics |>
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
              dictinct_questions$count,
              "Questions",
              icon = shiny::icon("circle-question"),
              color = "blue",
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              dictinct_questions$minute_per_question,
              "Minutes per question",
              icon = shiny::icon("stopwatch"),
              color = dictinct_questions$minpquest_color,
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              dictinct_questions$points,
              "Maximum points",
              icon = shiny::icon("award"),
              color = dictinct_questions$points_color,
              width = 12
            )
          ),
          shiny::column(
            3,
            shinydashboard::valueBox(
              dictinct_questions$expected,
              "Expected average",
              icon = shiny::icon("arrow-up-9-1"),
              color = dictinct_questions$expect_color,
              width = 12
            )
          )
        )
      })
      
      # Edit questions #########################################################
      
      output$testquestion2edit <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$test_parameters))
        test_questions <- course_data()$documents |>
          dplyr::filter(
            file %in% base::unique(modrval$test_parameters$question)
          )
        testquest <- test_questions$file
        base::names(testquest) <- base::paste(
          test_questions$file,
          test_questions$title,
          sep = " - "
        )
        
        
        ########################################################################
        ####   NEED TO ADD THE SELECTION OF OTHER LANGUAGES QUESTIONS      #####
        ########################################################################
        
        
        shiny::selectInput(
          ns("selectedtestquestion"),
          "Question to edit",
          choices = testquest,
          selected = testquest[1],
          width = "100%"
        )
      })
      
      output$edittestquestion <- shiny::renderUI({
        shiny::req(!base::is.null(input$selectedtestquestion))
        filepath <- base::paste0(
          modrval$test_folder,
          "/1_questions/", input$selectedtestquestion
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
          selected_question <- shiny::isolate({ input$selectedtestquestion })
          edited_question <- shiny::isolate({ input$editedtestquest })
          shiny::req(!base::is.null(selected_question))
          shiny::req(!base::is.null(edited_question))
          base::writeLines(
            edited_question,
            base::paste0(
              modrval$test_folder,
              "/1_questions/",
              selected_question
            ),
            useBytes = TRUE
          )
          versions_to_change <- modrval$test_parameters |>
            dplyr::filter(question == input$selectedtestquestion) |>
            dplyr::select(question_path, version, version_path)
          for (i in base::seq_len(base::nrow(versions_to_change))){
            lines <- base::readLines(versions_to_change$question_path[i])
            lines[2] <- base::paste0(
              'versionid <- "', versions_to_change$version[i],'"'
            )
            base::writeLines(
              lines, versions_to_change$version_path[i], useBytes = TRUE
            )
          }
        }
      })
      
      output$viewtestquestion <- shiny::renderUI({
        shiny::req(input$selectedtestquestion)
        input$savetestquestion
        document_to_edit <- course_data()$documents |>
          dplyr::filter(file == input$selectedtestquestion) |>
          dplyr::mutate(filepath = base::paste0(
            modrval$test_folder,
            "/1_questions/",
            input$selectedtestquestion
          ))
        editR::view_document(document_to_edit, TRUE, course_data, course_paths)
      })
      
      
      
      # Check and publish ######################################################
      
      output$textemplate_selection <- shiny::renderUI({
        templates <- base::list.files(course_paths()$subfolders$tex)
        templates <- c("", base::unique(stringr::str_remove_all(
          templates, "_questions.tex$|_solutions.tex$"
        )))
        shiny::selectInput(
          ns("slcttextemplate"), "Select a PDF template:",
          choices = templates, selected = "", multiple = FALSE
        )
      })
      
      
      
      ##########################################################################
      ####   NEED TO ADD THE SELECTION OF OTHER LANGUAGES here             #####
      ##########################################################################
      
      
      
      output$selectsection <- shiny::renderUI({
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
          status = "primary", size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$selectbloc <- shiny::renderUI({
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
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(input$slctsection))
        shiny::req(!base::is.null(input$slctbloc))
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
        shiny::isolate({
          test_parameters <- modrval$test_parameters
        })
        shiny::req(!base::is.null(input$slctquestion))
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
        shiny::req(!base::is.null(input$slctversion))
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
          
          base::save(test_parameters, file = base::paste0(
            modrval$test_folder,
            "/test_parameters.RData"
          ))
          
          modrval$test_parameters <- test_parameters
        }
        
      })
      
      output$displayversion <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctversion))
        version_to_view <- course_data()$documents |>
          dplyr::left_join(
            dplyr::select(modrval$test_parameters, file = question, version),
            by = "file"
          ) |>
          dplyr::filter(version == input$slctversion) |>
          dplyr::mutate(filepath = base::paste0(
            modrval$test_folder,
            "/2_versions/",
            input$slctversion
          ))
        editR::view_document(
          version_to_view, TRUE,
          course_data, course_paths,
          modrval$test_parameters
        )
      })
      
      
      
      ##########################################################################
      ####   MAKE SURE ONLY ONE LANGUAGE IS CREATED AT A TIME              #####
      ##########################################################################
      
      
      shiny::observeEvent(input$export_to_pdf, {
        if (input$slcttextemplate == ""){
          shinyalert::shinyalert(
            title = "Please select a template.",
            text = "You need to select a template to export a test as a PDF.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        } else {
          testR::export_test_to_pdf(
            modrval$test_parameters,
            modrval$propositions,
            modrval$translations,
            input$slcttextemplate
          )
          shinyalert::shinyalert(
            title = "PDF generated!",
            text = "You can now retrieve the file in the e_output directory of the test folder.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        }
      })
      
      
      shiny::observeEvent(input$export_to_blackboard, {
        testR::export_test_to_blackboard(
          modrval$test_parameters,
          modrval$propositions,
          modrval$translations
        )
        shinyalert::shinyalert(
          title = "Export generated for Blackboard!",
          text = "You can now retrieve the file in the e_output directory of the test folder.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      })
      
    }
  )
}
