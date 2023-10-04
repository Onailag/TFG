library(shiny)
library(RMySQL)
library(dplyr)

source("moduloAdmin.R")
source("moduloAlumno.R")
source("moduloProfesor.R")
source("moduloPassw.R")

options(encoding = 'UTF-8')

conn <- dbConnect(MySQL(), user = "root", password = "root", 
                  host = "localhost")
dbSendQuery(conn, "USE tablas")
user_bd <- dbGetQuery(conn, "SELECT * FROM Usuarios")
dbDisconnect(conn)

ui <- fluidPage(
  
  # Boton logout
  div(class = "pull-left", shinyauthr::logoutUI(id = "logout")),
  
  # Modulo de inicio de sesion
  shinyauthr::loginUI(id = "login", title = "Cuestionarios", user_title = "Usuario", 
                      pass_title =  "Contraseña", login_title =  "Iniciar sesión",
                      error_message = "Usuario o contraseña no válida"),
  
  uiOutput("ui")
)

server <- function(input, output, session) {

  credenciales <- shinyauthr::loginServer(
    id = "login",
    data = user_bd,
    user_col = ID,
    pwd_col = Password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credenciales()$user_auth)
  )

  
  user_data <- reactive({
    credenciales()$info
  })
  
  output$ui <- renderUI({
    req(credenciales()$user_auth)
    if(credenciales()$info[["ChangePass"]] == 1){
      passUI("prueba")
    }else{
      if(credenciales()$info[["Rol"]] == "alumno"){
        alumnoUI("alumnoUI")
      }else if(credenciales()$info[["Rol"]] == "profesor"){
        profesorUI("profesorUI")
      }else if(credenciales()$info[["Rol"]] == "administrador"){
        adminUI("adminUI")
      }
    }
    
  })
  passServer("prueba")
  alumnoServer("alumnoUI")
  profesorServer("profesorUI")
  adminServer("adminUI")




}



  


shinyApp(ui = ui, server = server)

