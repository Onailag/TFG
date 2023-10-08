library(RMySQL)
library(stringr)


updatePassword <- function(user, password, change = 0){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "tablas", host = "localhost")
  sql <- "UPDATE Usuarios SET Password = ?pass, ChangePass = ?change WHERE ID = ?id"
  
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
  
  sql <- "UPDATE Usuarios SET Password = ?pass, ChangePass=1 WHERE ID = 'a001'"
  
  querySql <- sqlInterpolate(conn, sql, pass = p1)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
  
  
}


askToUpdatePassword <- function(user){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "tablas", host = "localhost")
  sql <- "UPDATE Usuarios SET ChangePass = 1 WHERE ID = ?id"
  
  querySql <- sqlInterpolate(conn, sql, id = user)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}

getUsers <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    host = "localhost")
  dbSendQuery(conn, "USE tablas")
  user_bd <- dbGetQuery(conn, "SELECT * FROM Usuarios")
  dbDisconnect(conn)
  print("User table provided")
  return(user_bd)
}

validUserID <- function(userID){
  
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


addUser <- function(){
  
}

addUsersByCSV <- function(){
  
  
}


