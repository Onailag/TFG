library(shiny)

passUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(
        width = 4, offset = 3,
        tags$div(
          wellPanel(
            class = "panel-body",
            tags$br(),
            tags$div(
              style = "text-align: center;",
              
              tags$h3("Por favor cambia tu contraseña")
            ),
            tags$br(),
            passwordInput(
              inputId = ns("pwd_one"),
              label = "Nueva contraseña:",
              width = "100%"
            ),
            passwordInput(
              inputId = ns("pwd_two"),
              label = "Confirma la  contraseña:",
              width = "100%"
            ),
            tags$span(
              class = "help-block",
              icon("circle-info"),
              "Requisitos de la contraseña. Aun por definir."
            ),
            tags$br(),
            tags$div(
              id = ns("container-btn-update"),
              actionButton(
                inputId = ns("update_pwd"),
                label = "Actualizar contraseña",
                width = "100%",
                class = "btn_passnew"
              ),
              tags$br(), tags$br(),
              textOutput("pass")
            )
          )
      )
    )
    )
  )
}

passServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pass <- renderText(input$pwd_one)
    }
  )
}