#' @name edit_question_server
#' @title Edit questions, solutions, and feedback
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of questions, solutions, and feedback
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rmarkdown render
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @export


edit_question_server <- function(
    id, filtered, course_data, tree, course_paths
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    type <- NULL
    data <- NULL
    

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
      
      title <- selected |>
        editR::make_title_display(tags)
      
      base::load(course_paths()$databases$propositions)
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
      models <- 
      selected_model <- course_data()$document_models |>
        dplyr::filter(file == input$selectedquest) |>
        dplyr::select(data) |>
        tidyr::unnest(data)
      if (base::nrow(selected_model) > 0){
        editR::display_curve(selected_model)
      } else shiny::tags$br()
    }, height = 600)
    


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

