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
               h2(style="text-align: center;",
                 "Usuarios")
               ,
               
              actionButton(
                 inputId = ns("add_user"),
                 label = "Añadir usuario",
                 icon = icon("plus"),
                 width = "24%",
                 class = "btn-primary"
               ),
              
              
              actionButton(
                 inputId = ns("remove_user"),
                 label = "Eliminar usuario",
                 icon = icon("xmark"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
              
              actionButton(
                inputId = ns("add_multiple_user_csv"),
                label = "Añadir múltiples usuarios",
                icon = icon("file"),
                width = "24%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              
              actionButton(
                inputId = ns("remove_multiple_user_csv"),
                label = "Eliminar múltiples usuarios",
                icon = icon("file"),
                width = "24%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              br(),
              br(),
              
              DT::dataTableOutput(ns("userTable")),
              
              br(),
              
              actionButton(
                inputId = ns("reload_userTable"),
                label = "Recargar tabla",
                icon = icon("refresh"),
                width = "100%",
                class = "btn-primary", 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              
              ),
      
      
      
      tabPanel("Asignaturas",
               icon = icon("book"),
               h2(style="text-align: center;",
                 "Asignaturas"),
               
               actionButton(
                 inputId = ns("add_subject"),
                 label = "Añadir asignatura",
                 icon = icon("plus"),
                 width = "24%",
                 class = "btn-primary"
               ),
               
               
               actionButton(
                 inputId = ns("remove_subject"),
                 label = "Eliminar asignatura",
                 icon = icon("xmark"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               actionButton(
                 inputId = ns("add_multiple_subject_csv"),
                 label = "Añadir múltiples asignaturas",
                 icon = icon("file"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               actionButton(
                 inputId = ns("remove_multiple_subject_csv"),
                 label = "Eliminar múltiples asignaturas",
                 icon = icon("file"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               br(),
               br(),
               
               DT::dataTableOutput(ns("subjectTable")),
               
               br(),
               
               actionButton(
                 inputId = ns("reload_subjectTable"),
                 label = "Recargar tabla",
                 icon = icon("refresh"),
                 width = "100%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               
               
               
      ),
      
      tabPanel("Inscripciones",
               icon = icon("book"),
               h2(style="text-align: center;",
                 "Usuarios y asignaturas"),
               
               actionButton(
                 inputId = ns("add_userSubject"),
                 label = "Añadir inscripción",
                 icon = icon("plus"),
                 width = "24%",
                 class = "btn-primary"
               ),
               
               
               actionButton(
                 inputId = ns("remove_userSubject"),
                 label = "Eliminar inscripción",
                 icon = icon("xmark"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               actionButton(
                 inputId = ns("add_multiple_userSubject_csv"),
                 label = "Añadir inscripciones",
                 icon = icon("file"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               actionButton(
                 inputId = ns("remove_multiple_userSubject_csv"),
                 label = "Eliminar inscripciones",
                 icon = icon("file"),
                 width = "24%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               br(),
               br(),
               
               DT::dataTableOutput(ns("userSubjectTable")),
               
               br(),
               
               actionButton(
                 inputId = ns("reload_userSubjectTable"),
                 label = "Reload SubjectTable",
                 icon = icon("refresh"),
                 width = "100%",
                 class = "btn-primary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
               
               
               
               
      )
              
    )
  )
}

adminServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #callModule(modalModule, session$ns)
      ns <- session$ns
      
      
      addUsersCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          fileInput(ns("usersCSVAdd"), label = "Input csv"),
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
            textInput(inputId = ns("user_id"), label = "user_id"),
            textInput(inputId = ns("userPass"), label = "password", value = generatePassword()),
            textInput(inputId = ns("userName"), label = "name"),
            textInput(inputId = ns("userEmail"), label = "email"),
            checkboxInput(ns("userChangePass"), label = "Ask to change password at first login", value = TRUE),
            align = "center",
            
            tags$style(type="text/css", "#user_id{text-align:center};"),
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
            
          ))
      }
      
      ############### Observers addUserModal
      observeEvent(input$add_user,
                   showModal(addUserModal())
      )
      
      observeEvent(input$modalAddUserButton,{
        req(input$user_id)
        addUser(input$user_id, input$userName, input$userRol, input$userEmail, 
                sodium::password_store(input$userPass), input$userChangePass)
        removeModal()

        
      })
      
      ###############
      
      
      removeUserModal <- function(){
        ns <- session$ns
        modalDialog(
          shinyjs::useShinyjs(),
          div(
            textInput(inputId = ns("user_idRemove"), label = "user_id"),
            tags$style(type="text/css", "#user_id{text-align:center};"),
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
        req(input$user_idRemove)
          removeUser(input$user_idRemove)
          removeModal()
          showNotification("Accion completada")
        
      })
      
      ###############
      
      removeUsersCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          
          fileInput(ns("usersCSVRemove"), label = "Input csv"),
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
        users <- isolate(get_users())
        users$name <- iconv(users$name,  from = "UTF-8", to = "ISO-8859-1")
        users
      })
      
      
      
####################################################################################      
########  SUBJECT   ################################################################    
####################################################################################      
      
      output$subjectTable <- DT::renderDataTable({
        input$reload_subjectTable
        table <- get_subjects_table()
        table$description <- iconv(table$description,  from = "UTF-8", to = "ISO-8859-1")
        table$name <- iconv(table$name,  from = "UTF-8", to = "ISO-8859-1")
        table
      })
      
      observeEvent(input$add_subject, {
        ns <- session$ns
        showModal(modalDialog(
          div(
            textInput(inputId = ns("subject_code"), label = "subject_code"),
            textInput(inputId = ns("name"), label = "name"),
            textInput(inputId = ns("description"), label = "description"),
            textInput(inputId = ns("course"), label = "course"),
            align = "center",
            
            tags$style(type="text/css", "#subject_code{text-align:center};"),
            tags$style(type="text/css", "#name{text-align:center};"),
            tags$style(type="text/css", "#description{text-align:center};"),
            tags$style(type="text/css", "#course{text-align:center};"),
          ),
          
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
          div(
              actionButton(inputId = ns("modalAddSubjectButton"), label = "Añadir asignatura",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              align = "center"))
          ))
      })
      
      observeEvent(input$modalAddSubjectButton,{
        req(input$subject_code)
        addSubject(input$subject_code, input$name, input$description, 
                input$course)
        removeModal()
        
        
      })
      
      removeSubjectModal <- function(){
        ns <- session$ns
        modalDialog(
          shinyjs::useShinyjs(),
          div(
            textInput(inputId = ns("subject_codeRemove"), label = "subject_code"),
            tags$style(type="text/css", "#subject_code{text-align:center};"),
            align="center"
          ),
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
            
            div(
              actionButton(inputId = ns("modalRemoveSubjectButton"), label = "Eliminar asignatura",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              
              align = "center")
          )
        )
      }
      
      observeEvent(input$remove_subject, {
        showModal(removeSubjectModal())
      })
      
      observeEvent(input$modalRemoveSubjectButton,{
        req(input$subject_codeRemove)
          removeSubject(input$subject_codeRemove)
          removeModal()
          showNotification("Accion completada")
        
        
      })
      
      addSubjectsCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          fileInput(ns("subjectsCSVAdd"), label = "Input csv"),
          verbatimTextOutput(ns("previewSubjectAdd")),
          actionButton(ns("addSubjects"), label = "Añadir asignaturas")
        )
      }
      
      ############### Observers usersCSVmodal
      observeEvent(input$add_multiple_subject_csv,
                   showModal(addSubjectsCsvModal())
      )
      
      observeEvent(input$subjectsCSVAdd,{
        csv <- readr::read_csv(input$subjectsCSVAdd$datapath)
        output$previewSubjectAdd <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$addSubjects, { 
        req(input$subjectsCSVAdd)
        csv <-  readr::read_csv(input$subjectsCSVAdd$datapath, show_col_types = FALSE)
        addSubjectsByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      
      observeEvent(input$subjectsCSVRemove,{
        csv <- readr::read_csv(input$usersCSVRemove$datapath)
        output$previewSubjectRemove <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$removeSubjects, { 
        req(input$subjectsCSVRemove)
        csv <-  readr::read_csv(input$subjectsCSVRemove$datapath, show_col_types = FALSE)
        removeSubjectsByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      removeSubjectsCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          
          fileInput(ns("subjectsCSVRemove"), label = "Cargar csv asignaturas"),
          verbatimTextOutput(ns("previewSubjectRemove")),
          actionButton(ns("removeSubjects"), label = "Eliminar asignaturas")
        )
      }
      
      ############### Observers removeUsersCSVmodal
      observeEvent(input$remove_multiple_subject_csv,
                   showModal(removeSubjectsCsvModal())
      )
      
      
      
########################################################################################################     
############# User subject   ###########################################################################     
########################################################################################################        
      
      
      
      output$userSubjectTable <- DT::renderDataTable({
        input$reload_userSubjectTable
        table <- get_user_subjects_table()
        table
      })
      
      
      observeEvent(input$add_userSubject, {
        ns <- session$ns
        showModal(modalDialog(
          div(

            textInput(inputId = ns("user_id_usersubject"), label = "user_id"),
            textInput(inputId = ns("subject_code_usersubject"), label = "subject_code"),
            align = "center",
            
            tags$style(type="text/css", "#user_id{text-align:center};"),
            tags$style(type="text/css", "#subject_code{text-align:center};"),
          ),
          
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
            div(
              actionButton(inputId = ns("modalAddUserSubjectButton"), label = "Añadir inscripción",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              align = "center"))
        ))
      })
      
      observeEvent(input$modalAddUserSubjectButton,{
        req(input$user_id_usersubject)
        addUserSubject(input$user_id_usersubject, input$subject_code_usersubject)
        removeModal()
        
        
      })
      
      removeUserSubjectModal <- function(){
        ns <- session$ns
        modalDialog(
          shinyjs::useShinyjs(),
          div(
            
            textInput(inputId = ns("user_id_usersubjectRemove"), label = "user_id"),
            textInput(inputId = ns("subject_code_user_subjectRemove"), label = "subject_code"),
            tags$style(type="text/css", "#user_id_usersubjectRemove{text-align:center};"),
            tags$style(type="text/css", "#subject_code_user_subjectRemove{text-align:center};"),
            
            align="center"
          ),
          footer = tagList(
            div(
              modalButton("Cancelar")  
            ),
            
            
            div(
              actionButton(inputId = ns("modalRemoveUserSubjectButton"), label = "Eliminar inscripción",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              
              align = "center")
          )
        )
      }
      
      
      observeEvent(input$remove_userSubject, {
          showModal(removeUserSubjectModal())
      })
      
      
      observeEvent(input$modalRemoveUserSubjectButton,{
        req(input$user_id_usersubjectRemove)
        req(input$subject_code_user_subjectRemove)
        removeUserSubject(input$user_id_usersubjectRemove, input$subject_code_user_subjectRemove)
        removeModal()
        showNotification("Accion completada")
      })
      
      
      addUserSubjectsCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          fileInput(ns("user_subjectsCSVAdd"), label = "Input csv"),
          verbatimTextOutput(ns("previewUserSubjectAdd")),
          actionButton(ns("addUserSubjects"), label = "Añadir asignaturas")
        )
      }
      
      ############### Observers usersCSVmodal
      observeEvent(input$add_multiple_userSubject_csv,
                   showModal(addUserSubjectsCsvModal())
      )
      
      observeEvent(input$user_subjectsCSVAdd,{
        csv <- readr::read_csv(input$user_subjectsCSVAdd$datapath)
        output$previewUserSubjectAdd <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$addUserSubjects, { 
        req(input$user_subjectsCSVAdd)
        csv <-  readr::read_csv(input$user_subjectsCSVAdd$datapath, show_col_types = FALSE)
        addUserSubjectsByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      
      observeEvent(input$user_subjectsCSVRemove,{
        csv <- readr::read_csv(input$user_subjectsCSVRemove$datapath)
        output$previewUserSubjectRemove <- renderPrint({
          head(csv)
        })
      })
      
      
      observeEvent(input$removeUserSubjects, { 
        req(input$user_subjectsCSVRemove)
        csv <-  readr::read_csv(input$user_subjectsCSVRemove$datapath, show_col_types = FALSE)
        removeUserSubjectsByCSV(csv)
        removeModal()
        showNotification("Acción completada")
      })
      
      removeUserSubjectsCsvModal <- function() {
        ns <- session$ns
        modalDialog(
          
          fileInput(ns("user_subjectsCSVRemove"), label = "Cargar csv inscripciones"),
          verbatimTextOutput(ns("previewUserSubjectRemove")),
          actionButton(ns("removeUserSubjects"), label = "Eliminar inscripciones")
        )
      }
      
      ############### Observers removeUsersCSVmodal
      observeEvent(input$remove_multiple_userSubject_csv,
                   showModal(removeUserSubjectsCsvModal())
      )
      
      
    }
  )
}