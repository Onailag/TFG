library(shiny)
library(RMySQL)
library(dplyr)
library(bslib)
library(lubridate)
library(shinyjs)

source("moduloAdmin.R")
source("moduloAlumno.R")
source("moduloProfesor.R")
#source("moduloProfesorNoModals.R")

source("moduloPassw.R")
source("moduloFunciones.R")
#source("moduloModalsAdmin.R")

loginUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    # Boton logout
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout", label = "Cerrar sesión",
                                                   icon = icon("right-from-bracket")),
        
        style = "position: fixed; bottom: 20px; right: 20px; z-index: 1000;"),
    
    # Modulo de inicio de sesion
    shinyauthr::loginUI(id = "login", title = "Cuestionarios", user_title = "Usuario", 
                        pass_title =  "Contraseña", login_title =  "Iniciar sesión",
                        error_message = "Usuario o contraseña no válida"),
    
    
    
    uiOutput("ui")
  )
}

loginServer <- function(input, output, session) {
  options(encoding = 'UTF-8')
  user_bd <- get_users()
  
  credenciales <- shinyauthr::loginServer(
    id = "login",
    data = user_bd,
    user_col = user_id,
    pwd_col = password,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credenciales()$user_auth)
  )

  observe({
    req(logout_init())
    session$reload()
  })
  
  user_info <- reactive(
    credenciales()$info 
    
  )
  
  
  output$ui <- renderUI({
    req(credenciales()$user_auth)
    if(credenciales()$info[["change_pass"]] == 1){
      passUI("passUI")
    }else{
      if(credenciales()$info[["role"]] == "alumno"){
        alumnoUI("alumnoUI", user_info())
      }else if(credenciales()$info[["role"]] == "profesor"){
        profesorUI("profesorUI", user_info())
      }else if(credenciales()$info[["role"]] == "administrador"){
        adminUI("adminUI")
      }
    }
    
  })
  
  observe({
    req(credenciales()$user_auth)
    print(credenciales()$info)
    if(credenciales()$info[["change_pass"]] == 1){
      passServer("passUI", credenciales()$info[["user_id"]])
    }else{
      if(credenciales()$info[["role"]] == "alumno"){
        alumnoServer("alumnoUI", user_info())
      }else if(credenciales()$info[["role"]] == "profesor"){
        profesorServer("profesorUI", user_info())
      }else if(credenciales()$info[["role"]] == "administrador"){
        adminServer("adminUI")
      }
    }
  })
  
  onStop(function() {
    lapply(dbListConnections(MySQL()), dbDisconnect)
  })
  
  
}


shinyApp(ui = loginUI("login"), server = loginServer) #options = list(host = "192.168.18.4", port = 80)

