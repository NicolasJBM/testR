#' @name edit_question_ui
#' @title Edit questions, solutions, and feedback
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of questions, solutions, and feedback.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @export


edit_question_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("selectquest"))),
      shiny::column(
        6,
        shiny::actionButton(
          ns("newquest"), "New", icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366;color:#FFF;
          width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::uiOutput(ns("resultsstatistics")),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("viewquest"))),
      shiny::column(6, shiny::uiOutput(ns("editquest")))
    )
  )
}

