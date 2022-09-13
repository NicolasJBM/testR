#' @name edit_question_server
#' @title Edit questions, solutions, and feedback
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of questions, solutions, and feedback
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing a list of all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom editR display_curve
#' @importFrom editR make_infobox
#' @importFrom editR make_new_name
#' @importFrom editR make_title_display
#' @importFrom knitr knit2html
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny wellPanel
#' @importFrom shiny withMathJax
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


edit_question_server <- function(
    id, filtered, course_data, course_paths
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    type <- NULL
    data <- NULL
    discrimination <- NULL
    document <- NULL
    explanation <- NULL
    item <- NULL
    keywords <- NULL
    language <- NULL
    modifications <- NULL
    proposition <- NULL
    success <- NULL
    value <- NULL
    

    # Load data ################################################################

    selection <- shiny::reactive({
      shiny::req(!base::is.null(filtered()))
      filtered() |>
        dplyr::filter(type %in% c("Statements","Alternatives","Computation","Essay","Problem"))
    })

    template_files <- shiny::reactive({
      base::list.files(course_paths()$subfolders$templates_question)
    })

    # Select document ##########################################################

    output$selectquest <- shiny::renderUI({
      shiny::req(!base::is.null(selection()))
      if (base::length(selection()$title) > 0){
        quest_list <- c("", selection()$file)
        base::names(quest_list) <- c(
          "", base::paste(selection()$code, " - ", selection()$title)
        )
      }  else quest_list <- ""
      if (base::length(input$selected) == 0){
        if (base::length(quest_list) > 1){
          selection <- quest_list[2]
        } else selection <- ""
      } else {
        selection <- input$selectedquest
      }
      shiny::selectInput(
        ns("selectedquest"), "Select a question:", choices = quest_list,
        selected = selection, width = "100%"
      )
    })
    
    doctype <- shiny::reactive({
      shiny::req(!base::is.null(input$selectedquest))
      shiny::req(!base::is.null(selection()))
      question <- selection() |>
        dplyr::filter(file == input$selectedquest)
      question$type[1]
    })

    # Display statistics #######################################################
    
    output$resultsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(input$selectedquest))
      shiny::req(input$selectedquest != "")
      editR::make_infobox(course_data, input$selectedquest, "results")
    })



    # Display question #########################################################

    output$viewquest <- shiny::renderUI({

      shiny::req(input$selectedquest)
      shiny::req(!base::is.null(selection()))
      shiny::req(input$selectedquest %in% selection()$file)

      input$savequest
      input$questrefresh

      selected <- selection() |>
        dplyr::filter(file == input$selectedquest)

      filepath <- base::paste0(
        course_paths()$subfolders$original, "/", input$selectedquest
      )
      
      base::load(course_paths()$databases$tags)
      title <- selected |>
        editR::make_title_display(course_data)
      
      base::load(course_paths()$databases$propositions)
      test_parameters <- NA
      as_latex <- FALSE
      record_version <- FALSE
      
      shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE,
        status = "primary", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            12,
            base::suppressWarnings(
              shiny::withMathJax(shiny::HTML(knitr::knit2html(
                text = base::readLines(filepath),
                fragment.only = TRUE, quiet = TRUE
              )))
            )
          )
        )
      )
    })
    
    output$displaycurve <- shiny::renderPlot({
      shiny::req(!base::is.null(input$selectedquest))
      shiny::req(input$selectedquest != "")
      selected_model <- course_data()$document_models |>
        dplyr::filter(file == input$selectedquest) |>
        dplyr::select(data) |>
        tidyr::unnest(data)
      if (base::nrow(selected_model) > 0){
        editR::display_curve(selected_model)
      } else shiny::tags$br()
    }, height = 300)
    


    # Edit question ############################################################

    output$editquest <- shiny::renderUI({
      shiny::req(input$selectedquest)
      shiny::req(!base::is.null(selection()))
      input$questrefresh
      if (input$selectedquest %in% selection()$file){
        lines <- base::readLines(base::paste0(
          course_paths()$subfolders$original, "/", input$selectedquest
        ))
        shinydashboardPlus::box(
          width = 12, title = "Edition", solidHeader = TRUE,
          status = "navy", collapsible = FALSE, collapsed = FALSE,
          height = "750px",
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                ns("savequest"), "Save", icon = shiny::icon("floppy-disk"),
                style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("questinrstudio"), "RStudio",
                icon = shiny::icon("r-project"),
                style = "background-color:#222222;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("questrefresh"), "Refresh",
                icon = shiny::icon("rotate"),
                style = "background-color:#003399;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12, shinyAce::aceEditor(
              outputId = ns("editedquest"), value = lines, mode = "markdown",
              wordWrap = TRUE, debounce = 10, autoComplete = "live",
              height = "750px"
            ))
          )
        )
      } else NULL
    })

    shiny::observeEvent(input$savequest, {
      selected_quest <- shiny::isolate({ input$selectedquest })
      edited_quest <- shiny::isolate({ input$editedquest })
      shiny::req(selected_quest != "")
      shiny::req(!base::is.null(edited_quest))
      base::writeLines(
        input$editedquest,
        base::paste0(course_paths()$subfolders$original, "/", selected_quest),
        useBytes = TRUE
      )
    })

    shiny::observeEvent(input$questinrstudio, {
      shiny::req(input$selectedquest != "")
      rstudioapi::navigateToFile(
        base::paste0(course_paths()$subfolders$original, "/", input$selectedquest)
      )
    })

    
    
    # Edit propositions ########################################################

    propositions <- shiny::reactive({
      shiny::req(!base::is.null(selection()))
      input$refreshprop
      base::load(course_paths()$databases$propositions)
      propositions
    })
    
    targeted_documents <- shiny::reactive({
      shiny::req(input$selectedquest)
      shiny::req(!base::is.null(selection()))
      selected_question <- selection() |>
        dplyr::filter(file == input$selectedquest)
      targeted_documents <- selected_question$document[1] |>
        stringr::str_split(pattern = " ", simplify = TRUE) |>
        base::unique() |>
        base::sort()
      targeted_documents
    })
    
    propositions_for_question <- shiny::reactive({
      shiny::req(!base::is.null(targeted_documents()))
      shiny::req(base::length(targeted_documents()) > 0)
      selected_question <- selection() |>
        dplyr::filter(file == input$selectedquest)
      slcttype <- selected_question$type[1]
      slctlang <- selected_question$language[1]
      if (slcttype == "Statements"){
        propositions() |>
          dplyr::filter(
            document %in% targeted_documents(),
            type == slcttype,
            language == slctlang
          )
      } else {
        propositions() |>
          dplyr::filter(
            code == selected_question$code[1],
            type == slcttype,
            language == slctlang
          )
      }
    })
    
    output$selectpropositions <- shiny::renderUI({
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions_for_question()))
      shiny::wellPanel(
        shiny::actionButton(
          ns("saveprop"), "Save propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#009933;color:#FFF;width:100%"
        ),
        tags$hr(),
        shiny::actionButton(
          ns("refreshprop"), "Refresh propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#003399;color:#FFF;width:100%"
        ),
        tags$hr(),
        shiny::selectInput(
          ns("slctpropdoc"), "Select a document:",
          choices = base::unique(propositions_for_question()$document),
          selected = base::unique(propositions_for_question()$document),
          multiple = TRUE
        ),
        tags$hr(),
        shiny::sliderInput(
          ns("slctpropval"), "Select a value range:",
          min = 0, max = 1, value = c(0,1)
        )
      )
    })
    
    propositions_to_edit <- shiny::reactive({
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions_for_question()))
      shiny::req(!base::is.null(input$slctpropdoc))
      shiny::req(!base::is.null(input$slctpropval))
      propositions_for_question() |>
        dplyr::filter(
          document %in% input$slctpropdoc,
          value >= input$slctpropval[1],
          value <= input$slctpropval[2]
        )
    })
    
    output$editprop <- rhandsontable::renderRHandsontable({
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(propositions_to_edit()))
      
      existing_names <- propositions() |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      newitemid <- testR::name_new_item(existing_names)
      levellanguage <- propositions_to_edit()$language[1]
      leveltype <- propositions_to_edit()$type[1]
      levelcode <- dplyr::case_when(
        leveltype == "Statements" ~ base::as.character(NA),
        TRUE ~ propositions_to_edit()$code[1]
      )
      levelscale <- c("logical","qualitative","percentage")
      
      tmprow <- tibble::tibble(
        item = newitemid,
        language = levellanguage,
        code = levelcode,
        type = leveltype,
        document = base::factor(targeted_documents()[1], levels = targeted_documents()),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor("logical", levels = levelscale),
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        success = base::as.numeric(NA),
        discrimination = base::as.numeric(NA)
      )
      
      if (base::nrow(propositions_to_edit()) > 0){
        itemsublist <- propositions_to_edit() |>
          dplyr::mutate(
            code = base::factor(code, levels = levelcode),
            type = base::factor(type, levels = leveltype),
            document = base::factor(document, levels = targeted_documents()),
            scale = base::factor(scale, levels = levelscale)
          ) |>
          dplyr::arrange(code, document, dplyr::desc(value), proposition) |>
          dplyr::left_join(
            course_data()$item_parameters,
            by = c("item","language")
          ) |>
          dplyr::select(
            item, language, code, type, document, modifications, proposition,
            value, scale, explanation, keywords, success, discrimination
          ) |>
          dplyr::bind_rows(tmprow)
      } else {
        itemsublist <- tmprow
      }
      
      itemsublist |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,12,13), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "7%","2%","7%","7%","7%","3%","18%","3%",
            "5%","25%","10%","3%","3%"
          )
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })

    shiny::observeEvent(input$saveprop, {
      shiny::req(!base::is.null(input$editprop))
      modified <- rhandsontable::hot_to_r(input$editprop) |>
        dplyr::mutate_if(base::is.factor, base::as.character)
      
      if (base::is.na(modified[base::nrow(modified), "proposition"])){
        modified <- modified[-base::nrow(modified),]
      }
      
      propositions <- shiny::isolate({ propositions() })
      
      modified <- modified |>
        dplyr::select(base::names(propositions))
      
      not_modified <- propositions |>
        dplyr::anti_join(modified, by = c("item","language"))
      
      propositions <- not_modified |>
        dplyr::bind_rows(modified) |>
        dplyr::filter(!base::is.na(type), !base::is.na(value)) |>
        dplyr::arrange(item)
      
      base::save(propositions, file = course_paths()$databases$propositions)
      
      shinyalert::shinyalert(
        "Propositions saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesaveprop"
      )
    })
    
    
    
    # Create question ##########################################################

    shiny::observeEvent(input$newquest, {
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::selectInput(
            ns("slcttemplatebasis"), "Based on the following template:",
            choices = template_files(), selected = "", width = "100%"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("createquest"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })

    shiny::observeEvent(input$createquest, {
      shiny::removeModal()
      newname <- editR::make_new_name("Q", course_paths)
      newfile <- base::paste0(newname, ".Rmd")
      if (input$slcttemplatebasis == "") {
        lines <- c(
          "",
          "",
          "Meta-information",
          "================",
          "exextra[title]:New question.  ",
          "exextra[type]: Problem ",
          base::paste0(
            "exextra[document]:",
            stringr::str_remove(newname, "_...Rmd$"),
            "  "
          ),
          "exextra[tag_custom]:  "
        )
      } else {
        lines = base::readLines(
          base::paste0(
            course_paths()$subfolders$templates_question, "/",
            input$slcttemplatebasis
          )
        )
        lines <- stringr::str_replace_all(
          lines, base::paste0("QXXXXXXXXX"),
          stringr::str_remove(newname, "_...Rmd$")
        )
      }
      base::writeLines(
        lines,
        base::paste0(
          course_paths()$subfolders$original, "/", newname
        ), useBytes = TRUE
      )
      shinyalert::shinyalert(
        "Document created!", "Update documents and reload the course to see it.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
  })
}

