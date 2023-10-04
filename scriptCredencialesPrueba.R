library(shiny)
library(RMySQL)

################################################################################
########################    Prueba credenciales   ##############################
################################################################################

# Iniciar conexión con la base de datos
conn <- dbConnect(MySQL(), user = "root", password = "root", 
                  host = "localhost")

dbSendQuery(conn, "CREATE DATABASE tablas;")

# Uso la base de datos
dbSendQuery(conn, "USE tablas")

# Creo tabla Usuarios
dbSendQuery(conn, "CREATE TABLE Usuarios (
  ID VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50) NOT NULL,
  Rol ENUM ('alumno', 'profesor', 'administrador') NOT NULL,
  Email VARCHAR(50) UNIQUE NOT NULL,
  Password VARCHAR(150) NOT NULL,
  ChangePass Boolean NOT NULL DEFAULT 1
)")


p1 <- sodium::password_store("pass1")
p2 <- sodium::password_store("pass2")
p3 <- sodium::password_store("pass3")

# Insert datos de prueba para el login

sql <- "INSERT INTO Usuarios (ID, Nombre, Rol, Email, Password, ChangePass)
          values
          ('a001', 'alumno1', 'alumno', 'alumno@app.com', ?pass1, 1),
          ('p002', 'profesor1', 'profesor', 'profesor@app.com', ?pass2,0),
          ('a003', 'admin1', 'administrador', 'admin@app.com', ?pass3,0)"

querySql <- sqlInterpolate(conn, sql, pass1= p1, pass2 = p2, pass3 = p3)

dbSendQuery(conn, querySql)

user_bd <- dbGetQuery(conn, "SELECT * FROM Usuarios")

#dbSendQuery(conn, "DROP DATABASE tablas;")

dbDisconnect(conn)
################################################################################
