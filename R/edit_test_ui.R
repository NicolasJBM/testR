#' @name edit_test_ui
#' @title Create tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to create tests from a selection of questions.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the test parameters in the relevant test sub-folder in the folder "5_materials/blog".
#' @import shiny
#' @importFrom shinydashboardPlus box
#' @importFrom rhandsontable rHandsontableOutput
#' @export


edit_test_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("chalkboard"), "Resources",
          title = "Resources to help you test students.",
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("id-card"),"Definition"),
        shiny::uiOutput(ns("test_definition"))
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("filter"),"Selection"),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::uiOutput(ns("selectoldtests")),
            shiny::actionButton(
              ns("applyselection"),
              "Save selection", icon = shiny::icon("check"),
              style = "color:#FFF;background-color:#006600;
            width:100%;margin-top:10px;margin-bottom:25px;"
            ),
            shiny::uiOutput(ns("selectuntested")),
            shiny::uiOutput(ns("selecttested"))
          ),
          shiny::column(
            8,
            shiny::column(
              12,
              editR::selection_ui(ns("slctquest2disp"), "Question:")
            ),
            shiny::uiOutput(ns("viewquestionstats")),
            shiny::uiOutput(ns("viewquestion"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("shuffle"),"Organization"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("update_question_organization"), "Update questions",
              icon = shiny::icon("rotate"),
              style = "background-color:#003366;color:#FFF;width:100%;
                      margin-top:10px;margin-bottom:10px;"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("save_question_organization"), "Save questions",
              icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;width:100%;
                      margin-top:10px;margin-bottom:10px;"
            )
          )
        ),
        rhandsontable::rHandsontableOutput(ns("question_organization"))
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("scale-balanced"),"Composition"),
        shiny::uiOutput(ns("test_statistics")),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns("slctval"), "Value:",
              choices = c("count","points"), selected = "points",
              width = "100%"
            ),
            shiny::plotOutput(ns("scatterplot"), height = "700px")
          ),
          shiny::column(
            8,
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::selectInput(
                  ns("slctdim1"), "Fist dimension:",
                  choices = "section", selected = "section",
                  width = "100%"
                )
              ),
              shiny::column(
                4,
                shiny::numericInput(
                  ns("slctdepth"), "Section depth:",
                  min = 2, max = 9, value = 2,
                  width = "100%"
                )
              ),
              shiny::column(
                4,
                shiny::selectInput(
                  ns("slctdim2"), "Second dimension:",
                  choices = "type", selected = "type",
                  width = "100%"
                )
              )
            ),
            shiny::plotOutput(ns("barchart"), height = "700px")
          )
        ),
        shiny::plotOutput(ns("heatmap"), height = "700px")
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("pen-to-square"),"Edition"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns("slcteditquestlang")),
            shiny::column(12, editR::selection_ui(ns("slctquestion2edit"))),
            shiny::actionButton(
              ns("savetestquestion"), "Save", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::uiOutput(ns("edittestquestion"))
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("viewtestquestion"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("print"),"Publication"),
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::uiOutput(ns("selectlanguage")),
            shiny::uiOutput(ns("selectsection")),
            shiny::uiOutput(ns("selectbloc")),
            shiny::uiOutput(ns("selectquestion")),
            shiny::uiOutput(ns("selectversion")),
            shiny::tags$hr(),
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::uiOutput(ns("editseed"))
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("savenewseed"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;
                margin-top:25px;"
                )
              )
            )
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("displayversion"))
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("genmdfiles"), "Save MD files",
              icon = shiny::icon("floppy-disk"),
              style = "background-color:#006600;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::tags$hr(),
            shinyWidgets::radioGroupButtons(
              inputId = ns("slctfileformat"),label = "File format:", 
              choices = c("PDF","HTML","DOCX"),
              status = "danger", justified = TRUE, size = "sm",
              checkIcon = base::list(yes = shiny::icon("check"))
            ),
            shiny::uiOutput(ns("filetemplate_selection")),
            shiny::actionButton(
              ns("export_to_file"), "FILE", icon = shiny::icon("print"),
              style = "background-color:#660000;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;"
            ),
            shiny::tags$hr(),
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("slctlms"),
              label = "Select Learning Management Systems:", 
              choices = c("Moodle", "Canvas", "Blackboard","OpenOlat","ARSnova","Partificy","Ilias","TCexam","Testvision","QUI12","QTI21"),
              icon = icon("check-to-slot"), 
              status = "success",
              outline = TRUE, 
              animation = "tada"
            ),
            shiny::actionButton(
              ns("export_to_lms"), "LMS",
              icon = shiny::icon("graduation-cap"),
              style = "background-color:#000066;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::tags$hr(),
            shiny::actionButton(
              ns("openexamfolder"), "Open folder", icon = shiny::icon("folder-open"),
              style = "background-color:#222222;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            )
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("users"),"Students"),
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::actionButton(
              ns("savestudentlist"),
              "Save list", icon = shiny::icon("floppy-disk"),
              style = "color:#FFF;background-color:#006600;width:100%;margin-top:0px;margin-bottom:0px;"
            )
          ),
          shiny::column(
            10,
            rhandsontable::rHandsontableOutput(ns("studentlist"))
          )
        )
      )
    )
  )
}
