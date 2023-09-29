library(shiny)
library(RMySQL)
library(dplyr)

source("moduloAdmin.R")

################################################################################
########################    Prueba credenciales   ##############################
################################################################################

# Iniciar conexión con la base de datos
bd <- dbConnect(MySQL(), user = "root", password = "root", 
                host = "localhost")

dbSendQuery(bd, "CREATE DATABASE tablas;")

# Uso la base de datos
dbSendQuery(bd, "USE tablas")

# Creo tabla Usuarios
dbSendQuery(bd, "CREATE TABLE Usuarios (
  ID VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50) NOT NULL,
  Rol ENUM ('alumno', 'profesor', 'administrador') NOT NULL,
  Email VARCHAR(50) UNIQUE NOT NULL,
  Password VARCHAR(150) NOT NULL
)")


p1 <- sodium::password_store("pass1")
p2 <- sodium::password_store("pass2")

# Insert datos de prueba para el login

sql <- "INSERT INTO Usuarios (ID, Nombre, Rol, Email, Password)
          values
          ('a001', 'alumno1', 'alumno', 'alumno@app.com', ?pass1),
          ('p002', 'profesor1', 'profesor', 'profesor@app.com', ?pass2)"

querySql <- sqlInterpolate(bd, sql, pass1= p1, pass2 = p2)

dbSendQuery(bd, querySql)

user_bd <- dbGetQuery(bd, "SELECT * FROM Usuarios")

dbSendQuery(bd, "DROP DATABASE tablas;")

dbDisconnect(bd)
################################################################################

ui <- fluidPage(
  
  # Boton logout
  div(class = "pull-left", shinyauthr::logoutUI(id = "logout")),
  
  # Modulo de inicio de sesion
  shinyauthr::loginUI(id = "login", title = "Cuestionarios", user_title = "Usuario", 
                      pass_title =  "Contraseña", login_title =  "Iniciar sesión",
                      error_message = "Usuario o contraseña no válida"),
  
  uiOutput("ui")
  


  #tableOutput("user_table"),
  

  

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
  
  
  
  # Llama de forma reactiva para mostrar o no el boton
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credenciales()$user_auth)
  )

  
  user_data <- reactive({
    credenciales()$info
  })
  
  #output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    #req(credenciales()$user_auth)
    #user_data() %>%
     # mutate(across(starts_with("login_time"), as.character))
  #})
  
  user_rol <- reactive({
    credenciales()$info
  })
  
  output$ui <- renderUI({
    req(credenciales()$user_auth)
    if(credenciales()$info[["Rol"]] == "alumno"){
      adminUI("alumno1")
      
    }
  })
  adminServer("alumno1")
    
  

  #credenciales()$info[["Rol"]]



}



  


shinyApp(ui = ui, server = server)

