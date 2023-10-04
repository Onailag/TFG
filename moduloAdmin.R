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
                 width = "25%",
                 class = "btn-primary"
               ),
              actionButton(
                 inputId = ns("add_user_csv"),
                 label = "Add a user by csv",
                 icon = icon("file"),
                 width = "25%",
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
      
      output$userTable = DT::renderDataTable({
        conn <- dbConnect(MySQL(), user = "root", password = "root", 
                          host = "localhost")
        dbSendQuery(conn, "USE tablas")
        users_table <- dbGetQuery(conn, 
                                  "SELECT ID, Nombre, Rol, Email FROM Usuarios")
        dbDisconnect(conn)
        users_table
      })
      
      output$subjectTable = DT::renderDataTable({
        conn <- dbConnect(MySQL(), user = "root", password = "root", 
                          host = "localhost")
        dbSendQuery(conn, "USE tablas")
        subject_table <- dbGetQuery(conn, 
                                  "SELECT ID, Nombre, Rol, Email FROM Usuarios")
        dbDisconnect(conn)
        subject_table
      })
      
    }
  )
}