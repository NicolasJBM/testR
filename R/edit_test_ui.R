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
              "Apply selection", icon = shiny::icon("check"),
              style = "color:#FFF;background-color:#660033;
            width:100%;margin-top:10px;margin-bottom:25px;"
            ),
            shiny::uiOutput(ns("selectuntested")),
            shiny::uiOutput(ns("selecttested"))
          ),
          shiny::column(
            8,
            shiny::column(12, editR::selection_ui(ns("slctquest2disp"))),
            shiny::uiOutput(ns("viewquestionstats")),
            shiny::uiOutput(ns("viewquestion"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("shuffle"),"Organization"),
        shiny::uiOutput(ns("test_statistics")),
        shiny::actionButton(
          ns("save_question_organization"), "Save questions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#006633;color:#FFF;width:100%;
        margin-top:10px;margin-bottom:10px;"
        ),
        rhandsontable::rHandsontableOutput(ns("question_organization"))
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
            4,
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
            2,
            shiny::uiOutput(ns("textemplate_selection")),
            shiny::actionButton(
              ns("export_to_pdf"), "PDF", icon = shiny::icon("file-pdf"),
              style = "background-color:#660000;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_html"), "HTML", icon = shiny::icon("globe"),
              style = "background-color:#660033;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_moodle"), "Moodle", icon = shiny::icon("download"),
              style = "background-color:#330066;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_canvas"), "Canvas", icon = shiny::icon("download"),
              style = "background-color:#000099;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_blackboard"), "Blackboard",
              icon = shiny::icon("download"),
              style = "background-color:#003366;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_arsnova"), "ARS Nova", icon = shiny::icon("download"),
              style = "background-color:#006633;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_qti12"), "QTI 1.2", icon = shiny::icon("download"),
              style = "background-color:#006600;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_to_qti21"), "QTI 2.1", icon = shiny::icon("download"),
              style = "background-color:#336600;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::tags$hr(),
            shiny::actionButton(
              ns("openexamfolder"), "Open folder", icon = shiny::icon("folder"),
              style = "background-color:#222222;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            )
          )
        )
      )
    )
  )
}
