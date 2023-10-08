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
              "La contraseña debe incluir una mayúscula, una minúscula, un número y tener entre
              6 y 10 caracteres."
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
              shinyjs::hidden(
                tags$div(
                  id = ns("error"),
                  tags$p(
                    "Contraseña no válida.",
                    style = "color: red; font-weight: bold; padding-top: 5px;",
                    class = "text-center"
                  )
                )
              )
            )
          )
        )
      ),
    )
  )
}

passServer <- function(id, UserID) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$update_pwd,{
        
        if(!validPassword(input$pwd_one, input$pwd_two)){
          # Based on ShinyAuthr login error msg  
          shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
        }else{
          updatePassword(UserID, sodium::password_store(input$pwd_one))
        }
      })
      
      
    }
  )
}