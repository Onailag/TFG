library(shiny)
library(RMySQL)
library(dplyr)
library(bslib)

source("moduloAdmin.R")
source("moduloAlumno.R")
source("moduloProfesor.R")
source("moduloPassw.R")
source("moduloFunciones.R")

loginUI <- fluidPage(

  # Boton logout
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout", label = "Cerrar sesión",
                                                 icon = icon("right-from-bracket")), 
      style = "position: absolute; bottom: 0;right:0;"),
  
  # Modulo de inicio de sesion
  shinyauthr::loginUI(id = "login", title = "Cuestionarios", user_title = "Usuario", 
                      pass_title =  "Contraseña", login_title =  "Iniciar sesión",
                      error_message = "Usuario o contraseña no válida"),
  
  uiOutput("ui")
)

loginServer <- function(input, output, session) {
  options(encoding = 'UTF-8')
  user_bd <- getUsers()
  
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

  
  ## Si se produce un logout
  observe({
    req(logout_init())
    session$reload()
  })
  
  output$ui <- renderUI({
    req(credenciales()$user_auth)
    if(credenciales()$info[["ChangePass"]] == 1){
      passUI("passUI")
      
     
    }else{
      if(credenciales()$info[["Rol"]] == "alumno"){
        print(credenciales()$user_auth)
        alumnoUI("alumnoUI")
      }else if(credenciales()$info[["Rol"]] == "profesor"){
        nextUI <- "profesor"
      }else if(credenciales()$info[["Rol"]] == "administrador"){
        adminUI("adminUI")
      }
    }
    
  })
  passServer("passUI", credenciales()$info[["ID"]])
  alumnoServer("alumnoUI")
  profesorServer("profesorUI")
  adminServer("adminUI")




}



  


shinyApp(ui = loginUI, server = loginServer)

