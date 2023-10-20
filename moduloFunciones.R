library(RMySQL)
library(stringr)


updatePassword <- function(user, password, change = 0){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "tablas", host = "localhost")
  sql <- "UPDATE Usuarios SET Password = ?pass, ChangePass = ?change WHERE ID = ?id;"
  
  querySql <- sqlInterpolate(conn, sql, pass = password, id = user, change = change)
  
  dbSendQuery(conn, querySql)
  print("Password Updated")
  
  dbDisconnect(conn)
}

# Pruebas
resetAlumno <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "tablas", host = "localhost")
  
  p1 <- sodium::password_store("pass1")
  
  sql <- "UPDATE Usuarios SET Password = ?pass, ChangePass=1 WHERE ID = 'a001';"
  
  querySql <- sqlInterpolate(conn, sql, pass = p1)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}


askToUpdatePassword <- function(user){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "tablas", host = "localhost")
  sql <- "UPDATE Usuarios SET ChangePass = 1 WHERE ID = ?id;"
  
  querySql <- sqlInterpolate(conn, sql, id = user)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}

getUsers <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "USE tablas;")
  user_bd <- dbGetQuery(conn, "SELECT * FROM Usuarios;")
  dbDisconnect(conn)
  print("User table provided")
  return(user_bd)
}

existsUserID <- function(userID){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "USE tablas;")
  
  sql <- "SELECT * FROM Usuarios where ID = ?id;"
  
  querySql <- sqlInterpolate(conn, sql, id = userID)
  
  users <- dbGetQuery(conn, querySql)
  
  dbDisconnect(conn)
  
  return(nrow(users) >= 1)
}

validPassword <- function(pass1, pass2){
  return((pass1==pass2)&& stringr::str_detect(pass1, 
                                              "((?=.*[[:lower:]])(?=.*[[:upper:]])(?=.*[[:digit:]]).{6,10})"))
}

generatePassword <- function(){
  pass <- stringi::stri_rand_strings(n = 1, length = 8, 
                             pattern = "[0-9a-zA-Z]")
  return(pass)
}

# password is passed already hashed
# Añadir trycatch y comprobar que el userID esta disponible y que role es valido
addUser <- function(userID, name = NA, role, email= NA, password, changePass = 1){

  
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "USE tablas;")
  
  if((is.na(name)||name=="")&(is.na(email)||email=="")){
    sql <- "INSERT INTO Usuarios (ID, Rol, Password, ChangePass)
          values
          (?Id, ?Role, ?Pass, ?ChangePass);"
    querySql <- sqlInterpolate(conn, sql, Id = userID, Role = role,
                              Pass = password, ChangePass = changePass)
  }else if (is.na(name)||name==""){
    sql <- "INSERT INTO Usuarios (ID, Rol, Email, Password, ChangePass)
          values
          (?Id, ?Role, ?Email, ?Pass, ?ChangePass);"
    
    querySql <- sqlInterpolate(conn, sql, Id = userID, Role = role,
                               Email = email, Pass = password, ChangePass = changePass)
  }else if(is.na(email)||email==""){
    sql <- "INSERT INTO Usuarios (ID, Nombre, Rol, Password, ChangePass)
          values
          (?Id, ?Name, ?Role, ?Pass, ?ChangePass);"
    
    querySql <- sqlInterpolate(conn, sql, Id = userID, Name = name, Role = role,
                               Pass = password, ChangePass = changePass)
  }else{
    sql <- "INSERT INTO Usuarios (ID, Nombre, Rol, Email, Password, ChangePass)
          values
          (?Id, ?Name, ?Role, ?Email, ?Pass, ?ChangePass);"
    
    querySql <- sqlInterpolate(conn, sql, Id = userID, Name = name, Role = role,
                               Email = email, Pass = password, ChangePass = changePass)
  }
  
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}

removeUser <- function(userID){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "USE tablas;")
  
  sql <- "DELETE FROM Usuarios WHERE ID = ?Id;"
  
  querySql <- sqlInterpolate(conn, sql, Id = userID)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}

resetDBpruebas <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "DROP DATABASE tablas;")
  dbSendQuery(conn, "CREATE DATABASE tablas;")
  
  # Uso la base de datos
  dbSendQuery(conn, "USE tablas")
  
  # Creo tabla Usuarios
  dbSendQuery(conn, "CREATE TABLE Usuarios (
  ID VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50),
  Rol ENUM ('alumno', 'profesor', 'administrador') NOT NULL,
  Email VARCHAR(50),
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
          ('p002', 'profesor1', 'profesor',NULL, ?pass2,0),
          ('a003', 'admin1', 'administrador', 'admin@app.com', ?pass3,0)"
  
  querySql <- sqlInterpolate(conn, sql, pass1= p1, pass2 = p2, pass3 = p3)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
  ################################################################################
}

addUsersByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    id <- df[i, ][[1]]
    name <- df[i, ][[2]]
    role <- df[i, ][[3]]
    email <- df[i, ][[4]]
    password <- sodium::password_store(df[i,][[5]]) 
    changePass <- df[i, ][[6]]
    if(!existsUserID(id))
    addUser(id, name, role, email, password, changePass)
  }
  
}

removeUsersByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    id <- df[i, ][[1]]
    if(existsUserID(id))
      removeUser(id)
  }
  
}


