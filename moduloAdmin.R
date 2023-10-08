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
                 width = "33%",
                 class = "btn-primary"
               ),
              
              
              actionButton(
                 inputId = ns("remove_user"),
                 label = "Remove a user",
                 icon = icon("xmark"),
                 width = "33%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
              
              actionButton(
                inputId = ns("add_multiple_user_csv"),
                label = "Add multiple users",
                icon = icon("file"),
                width = "33%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              
              
              DT::dataTableOutput(ns("userTable")),
              
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

adminServer <- function(id, dbConn = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$add_user, {
        showModal(modalDialog(
          title = "Add a user",
          
          div(
            selectInput("selectRol", label = "Select Rol", 
                        choices = list("Alumno" = "alumno", "Profesor" = "profesor", 
                                       "Admin" = "admin"), 
                        selected = "profesor"),
            textInput(inputId = "userID", label = "userID"),
            textInput(inputId = "userPass", label = "password", value = generatePassword()),
            textInput(inputId = "userName", label = "name"),
            textInput(inputId = "userEmail", label = "email"),
            checkboxInput("userChangePass", label = "Ask to change password at first login", value = TRUE),
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
              actionButton(inputId = "modalAddUserButton", label = "Add User",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             
            align = "center")
          ))
          
        )
      })
      
      observeEvent(input$add_multiple_user_csv,{
        showModal(modalDialog(
          
          div(
            fileInput("usersCSV", label = "Input csv")
          )
          
        ))
        
        
        
      })
      
      
      observeEvent(input$modalAddUserButton,{
        removeModal()
      })
      
      output$userTable = DT::renderDataTable({
        getUsers()
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