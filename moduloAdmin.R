library(shiny)
library(bslib)

adminUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Modo administrador", 
      theme = bslib::theme_bootswatch("celurian"),
      tabPanel("Usuarios",
               icon = icon("user"),
               p(style="text-align: center;",
                 "Usuarios")
               ,
               
               
              actionButton(
                 inputId = ns("add_user"),
                 label = "Add a user",
                 icon = icon("plus"),
                 width = "24%",
                 class = "btn-primary"
               ),
              
              
              actionButton(
                 inputId = ns("remove_user"),
                 label = "Remove a user",
                 icon = icon("xmark"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
              
              actionButton(
                inputId = ns("add_multiple_user_csv"),
                label = "Add multiple users",
                icon = icon("file"),
                width = "24%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              
              actionButton(
                inputId = ns("remove_multiple_user_csv"),
                label = "Remove multiple users",
                icon = icon("file"),
                width = "24%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              br(), 
              
              DT::dataTableOutput(ns("userTable")),
              
              br(),
              
              actionButton(
                inputId = ns("reload_userTable"),
                label = "Reload UserTable",
                icon = icon("refresh"),
                width = "100%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              
              ),
      
      
      
      tabPanel("Asignaturas",
               icon = icon("book"),
               p(style="text-align: center;",
                 "Asignaturas -- Mientras usuarios"),
               
               
               
               DT::dataTableOutput(ns("subjectTable"))
      )
              
    )
  )
}

adminServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #callModule(modalModule, session$ns)
      
      
      
      addUsersCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          
          fileInput(ns("usersCSVAdd"), label = "Input csv"),
          checkboxInput("headerCSVAdd", label = "Header ?", value = FALSE),
          verbatimTextOutput(ns("previewAdd")),
          actionButton(ns("addUsers"), label = "Add users by CSV")
        )
      }
      
      ############### Observers usersCSVmodal
      observeEvent(input$add_multiple_user_csv,
                   showModal(addUsersCsvModal())
      )
      
      observeEvent(input$usersCSVAdd,{
        csv <- readr::read_csv(input$usersCSVAdd$datapath)
        output$previewAdd <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$addUsers, { 
        req(input$usersCSVAdd)
        csv <-  readr::read_csv(input$usersCSVAdd$datapath, show_col_types = FALSE)
        addUsersByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      ###############
      
      ############### 
      addUserModal <- function() {
        ns <- session$ns
        modalDialog(
          shinyjs::useShinyjs(),
          title = "Add a user",
          
          div(
            selectInput(ns("userRol"), label = "Select Rol", 
                        choices = list("Alumno" = "alumno", "Profesor" = "profesor", 
                                       "Admin" = "admin"), 
                        selected = "profesor"),
            textInput(inputId = ns("userID"), label = "userID"),
            textInput(inputId = ns("userPass"), label = "password", value = generatePassword()),
            textInput(inputId = ns("userName"), label = "name"),
            textInput(inputId = ns("userEmail"), label = "email"),
            checkboxInput(ns("userChangePass"), label = "Ask to change password at first login", value = TRUE),
            align = "center",
            
            tags$style(type="text/css", "#userID{text-align:center};"),
            tags$style(type="text/css", "#userPass{text-align:center};"),
            tags$style(type="text/css", "#userName{text-align:center};"),
            tags$style(type="text/css", "#userEmail{text-align:center};"),
          ),
          
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
            
            div(
              actionButton(inputId = ns("modalAddUserButton"), label = "Add User",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              
              align = "center"),
            
            shinyjs::hidden(
              tags$div(
                id = ns("errorIDAdd"),
                tags$p(
                  "Ese ID ya está registrado.",
                  style = "color: red; font-weight: bold; padding-top: 5px;",
                  class = "text-center"
                )
              )
            )
          ))
      }
      
      ############### Observers addUserModal
      observeEvent(input$add_user,
                   showModal(addUserModal())
      )
      
      observeEvent(input$modalAddUserButton,{
        req(input$userID)
        if(!existsUserID(input$userID)){
          addUser(input$userID, input$userName, input$userRol, input$userEmail, 
                  sodium::password_store(input$userPass), 0)
          removeModal()
        }else{
          shinyjs::toggle(id = "errorIDAdd", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
        }
        
      })
      
      ###############
      
      
      removeUserModal <- function(){
        ns <- session$ns
        modalDialog(
          shinyjs::useShinyjs(),
          div(
            textInput(inputId = ns("userIDRemove"), label = "userID"),
            tags$style(type="text/css", "#userID{text-align:center};"),
            align="center"
          ),
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
            
          div(
            actionButton(inputId = ns("modalRemoveUserButton"), label = "Remove User",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              
            align = "center"),
          shinyjs::hidden(
            tags$div(
              id = ns("errorIDRemove"),
              tags$p(
                "No existe ese usuario",
                style = "color: red; font-weight: bold; padding-top: 5px;",
                class = "text-center")
              )
            )
          )
        )
      }
      
      
      
      ############### Observers removeUserModal
      
      observeEvent(input$remove_user,
                   showModal(removeUserModal()))
      
      observeEvent(input$modalRemoveUserButton,{
        req(input$userIDRemove)
        if(!existsUserID(input$userIDRemove)){
          shinyjs::toggle(id = "errorIDRemove", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "errorIDRemove", anim = TRUE, time = 1, animType = "fade"))
          
        }else{
          removeUser(input$userIDRemove)
          removeModal()
          showNotification("Accion completada")
        }
        
      })
      
      ###############
      
      removeUsersCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          
          fileInput(ns("usersCSVRemove"), label = "Input csv"),
          checkboxInput("headerCSVRemove", label = "Header ?", value = FALSE),
          verbatimTextOutput(ns("previewRemove")),
          actionButton(ns("removeUsers"), label = "Remove users by CSV")
        )
      }
      
      ############### Observers removeUsersCSVmodal
      observeEvent(input$remove_multiple_user_csv,
                   showModal(removeUsersCsvModal())
      )
      
      observeEvent(input$usersCSVRemove,{
        csv <- readr::read_csv(input$usersCSVRemove$datapath)
        output$previewRemove <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$removeUsers, { 
        req(input$usersCSVRemove)
        csv <-  readr::read_csv(input$usersCSVRemove$datapath, show_col_types = FALSE)
        removeUsersByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      output$userTable <- DT::renderDataTable({
        input$reload_userTable
        isolate(getUsers())
      })
      
      
      
      
      
      output$subjectTable = DT::renderDataTable({
        conn <- dbConnect(MySQL(), user = "root", password = "root", 
                          host = "localhost")
        dbSendQuery(conn, "USE tablas")
        user_table <- dbGetQuery(conn, 
                                  "SELECT ID, Nombre, Rol, Email FROM Usuarios")
        dbDisconnect(conn)
        user_table
      })
    }
  )
}