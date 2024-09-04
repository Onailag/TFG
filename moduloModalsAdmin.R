modalModule <- function(input, output, session) {
  
  usersCsvModal <- function() {
    ns <- session$ns
    modalDialog(
      
      fileInput(ns("usersCSV"), label = "Input csv"),
      checkboxInput("headerCSV", label = "Header ?", value = FALSE),
      verbatimTextOutput(ns("preview")),
      actionButton(ns("addUsers"), label = "Add users by CSV")
    )
  }
  
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
            id = ns("errorID"),
            tags$p(
              "Ese ID ya está registrado.",
              style = "color: red; font-weight: bold; padding-top: 5px;",
              class = "text-center"
            )
          )
        )
      ))
  }
  
  # open modal on button click

  
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
      shinyjs::toggle(id = "errorID", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
    }
    
  })
  
  ###############  
  
  ###############  Observer removeUserModal
  
  
  ############### Observers usersCSVmodal
  observeEvent(input$add_multiple_user_csv,
               showModal(addUserModal())
  )
  
  observeEvent(input$usersCSV,{
    csv <- readr::read_csv(input$usersCSV$datapath)
    output$preview <- renderPrint({
      head(csv)
    })
  })
  
  
  observeEvent(input$addUsers, { 
    req(input$usersCSV)
    csv <-  readr::read_csv(input$usersCSV$datapath, show_col_types = FALSE)
    addUsersByCSV(csv)
    removeModal()
  })
  
  ###############
}