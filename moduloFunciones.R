library(RMySQL)
library(stringr)
library(xml2)
library(XML)
library(dplyr)
library(jsonlite)
library(sodium)

################################################################################
################################################################################
# Funciones Login
################################################################################
################################################################################


updatePassword <- function(user, password, change = 0){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost")
  sql <- "UPDATE users SET password = ?pass, change_pass = ?change WHERE user_id = ?id;"
  
  querySql <- sqlInterpolate(conn, sql, pass = password, id = user, change = change)
  
  dbSendQuery(conn, querySql)

  
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
  sql <- "UPDATE users SET change_pass = 1 WHERE user_id = ?id;"
  
  querySql <- sqlInterpolate(conn, sql, id = user)
  
  dbSendQuery(conn, querySql)
  
  dbDisconnect(conn)
}

get_users <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost")
  user_bd <- dbGetQuery(conn, "SELECT * FROM users;")
  dbDisconnect(conn)

  
  return(user_bd)
}




add_sessionid_to_db <- function(user, sessionid) {
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <-  "INSERT INTO sessionids (user_id, sessionid, login_time)
          values
          (?user, ?sessionid, ?time)"
    querySql <- sqlInterpolate(conn, sql, user= user, sessionid = sessionid,
                               time = as.character(now()))
    
    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
  })
  
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth
get_sessionids_from_db <- function() {
  sessions <- tryCatch({
    expiry <- 30
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sessions <- dbGetQuery(conn, "select * from sessionids") %>%
      mutate(login_time = ymd_hms(login_time)) %>%
      as_tibble() %>%
      filter(login_time > now() - seconds(expiry))
    dbDisconnect(conn)
    return(sessions)
  })
  return(sessions)
}


################################################################################
################################################################################
# Funciones vista Admin
################################################################################
################################################################################



validPassword <- function(pass1, pass2){
  return((pass1==pass2)&& stringr::str_detect(pass1, 
                                              "((?=.*[[:lower:]])(?=.*[[:upper:]])(?=.*[[:digit:]]).{6,10})"))
}

generatePassword <- function(){
  pass <- stringi::stri_rand_strings(n = 1, length = 8, 
                             pattern = "[0-9a-zA-Z]")
  return(pass)
}


addUser <- function(user_id, name = NA, role, email= NA, password, changePass = 1){

  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    
    if((is.na(name)||name=="")&(is.na(email)||email=="")){
      sql <- "INSERT INTO Users (user_id, role, password, change_pass)
          values
          (?Id, ?Role, ?Pass, ?ChangePass)
    ON DUPLICATE KEY UPDATE
    role = values(role),
    password = values(password),
    change_pass = values(change_pass);"
      querySql <- sqlInterpolate(conn, sql, Id = user_id, Role = role,
                                 Pass = password, ChangePass = changePass)
    }else if (is.na(name)||name==""){
      sql <- "INSERT INTO Users (user_id, role, mail, password, change_pass)
          values
          (?Id, ?Role, ?Email, ?Pass, ?ChangePass)ON DUPLICATE KEY UPDATE
    role = values(role),
    mail = values(mail),
    password = values(password),
    change_pass = values(change_pass);"
      
      querySql <- sqlInterpolate(conn, sql, Id = user_id, Role = role,
                                 Email = email, Pass = password, ChangePass = changePass)
    }else if(is.na(email)||email==""){
      sql <- "INSERT INTO Users (user_id, name, role, password, change_pass)
          values
          (?Id, ?Name, ?Role, ?Pass, ?ChangePass)
        ON DUPLICATE KEY UPDATE
    name = values(name),
    role = values(role),
    password = values(password),
    change_pass = values(change_pass);"
      
      querySql <- sqlInterpolate(conn, sql, Id = user_id, Name = name, Role = role,
                                 Pass = password, ChangePass = changePass)
    }else{
      sql <- "INSERT INTO users (user_id, name, role, mail, password, change_pass)
          values
          (?Id, ?Name, ?Role, ?Email, ?Pass, ?ChangePass)
        ON DUPLICATE KEY UPDATE
    name = values(name),
    role = values(role),
    mail = values(mail),
    password = values(password),
    change_pass = values(change_pass);"
      
      querySql <- sqlInterpolate(conn, sql, Id = user_id, Name = name, Role = role,
                                 Email = email, Pass = password, ChangePass = changePass)
    }
    
    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })
  
}

removeUser <- function(user_id){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    
    sql <- "DELETE FROM Users WHERE user_id = ?Id;"
    
    querySql <- sqlInterpolate(conn, sql, Id = user_id)
    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })

}

addSubject <- function(subject_code, name, description, course){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
      sql <- "INSERT INTO Subjects (subject_code, name, description, course, questions_per_test)
          values
          (?subject_code, ?name, ?description, ?course,10)
    ON DUPLICATE KEY UPDATE
    name = values(name),
    description = values(description),
    course = values(course);"
      querySql <- sqlInterpolate(conn, sql,subject_code = subject_code, name = name, description = description,
                                 course = course)

    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })
}

removeSubject <- function(subject_code){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    
    sql <- "DELETE FROM Subjects WHERE subject_code = ?;"
    
    querySql <- sqlInterpolate(conn, sql, subject_code)
    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })
  
}

get_user_subjects_table <- function(){
  table <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    
    sql <- "SELECT * FROM user_subjects;"
    
    table <- dbGetQuery(conn, sql)
    
    dbDisconnect(conn)
    return(table)
  })
  return(table)
}

addUserSubject <- function(user_id, subject_code){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    sql <- "INSERT INTO user_subjects (user_id, subject_code, current_node)
          values
          (?, ? ,1);"
    querySql <- sqlInterpolate(conn, sql, user_id, subject_code)
    
    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })
}

removeUserSubject <- function(user_id, subject_code){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    sql <- "DELETE FROM user_subjects WHERE user_id = ? AND subject_code = ?;"
    querySql <- sqlInterpolate(conn, sql, user_id, subject_code)
    
    
    dbSendQuery(conn, querySql)
    
    dbDisconnect(conn)
  })
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

addSubjectsByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    subject_code <- df[i, ][[1]]
    name <- df[i, ][[2]]
    description <- df[i, ][[3]]
    course <- df[i, ][[4]]

    addSubject(subject_code, name, description, course)
  }
}

addUserSubjectsByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    user_id <- df[i, ][[1]]
    subject_code <- df[i, ][[2]]

    
    addUserSubject(user_id, subject_code)
  }
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
    addUser(id, name, role, email, password, changePass)
  }
  
}

removeUsersByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    id <- df[i, ][[1]]
      removeUser(id)
  }
}

removeSubjectsByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    id <- df[i, ][[1]]
    removeSubject(id)
  }
}

removeUsersSubjectsByCSV <- function(inputCSV){
  df <- data.frame(inputCSV)
  for(i in 1:nrow(df)){
    user_id <- df[i, ][[1]]
    subject_code <- df[i, ][[2]]
    removeUserSubject(user_id,subject_code)
  }
}

get_subjects_table <- function(){
  table <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT * FROM subjects;"
    table <- dbGetQuery(conn, sql)
    dbDisconnect(conn)
    return(table)
  })
  return(table)
}



################################################################################
################################################################################
#
################################################################################
################################################################################

generate_id <- function() {
  chars <- c(letters, LETTERS, 0:9) 
  id <- paste0(sample(chars, 8, replace = TRUE), collapse = "")
  return(id)
}


################################################################################
################################################################################
# Funciones vista Profesor
################################################################################
################################################################################



get_subjects <- function(user_id){
  result <-tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    sql <- "SELECT subject_code FROM user_subjects WHERE user_id = ?;"
    
    querySql <- sqlInterpolate(conn, sql, user_id)
    
    subjects <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    return(subjects$subject_code)
  })
  return(result)
}

get_attb <- function(subject_code){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    sql <- "SELECT DISTINCT(attribute) FROM attributes WHERE subject_code = ?;"
    querySql <- sqlInterpolate(conn, sql, subject_code)
    
    attb <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    return(iconv(attb$attribute, from = "UTF-8", to = "latin1"))
  })
  return(result)
}

getEnroledStudentsTable <- function(subject){

  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    querySql <- "SELECT u.user_id, 
      u.name, 
      AVG(ta.score) AS average_score,
      MAX(ta.content) AS last_content_examined,
      MAX(ta.date) AS last_test_date,
      COUNT(ta.id) AS total_tests
      FROM appcuestionarios.users u
      JOIN appcuestionarios.user_subjects us ON u.user_id = us.user_id
      JOIN appcuestionarios.subjects s ON us.subject_code = s.subject_code
      LEFT JOIN appcuestionarios.test_attempts ta ON u.user_id = ta.user AND ta.subject = s.subject_code
      WHERE u.role = 'alumno'AND s.subject_code = ?
      GROUP BY u.user_id, u.name
      ORDER BY name;"
    querySql <- sqlInterpolate(conn, querySql, subject)
    
    enroled_students <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(enroled_students)
  }, error = function(e) {
    message("OcurriÃ³ un error: ", e$message)
    return(NULL)
  })


  return(as.data.frame(result))
}

get_subject_questions_table <- function(subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    querySql <- "SELECT * FROM questions WHERE subject_code = ?;"
    querySql <- sqlInterpolate(conn, querySql, subject)
    
    questions <- dbGetQuery(conn, querySql)
    questions$text <- iconv(questions$text, from = "UTF-8", to = "latin1")
    questions$name <- iconv(questions$name, from = "UTF-8", to = "latin1")
    questions$subject_code <- iconv(questions$subject_code, from = "UTF-8", to = "latin1")
    questions$content <- iconv(questions$content, from = "UTF-8", to = "latin1")
    questions$text <- lapply(questions$text, function(txt) {
      HTML(paste0("<div style='font-size: 12px;'>", txt, "</div>"))
    })

    dbDisconnect(conn)
    return(questions)
  }, error = function(e) {
    message("Ocurrió un error: ", e$message)
    return(NULL)
  })
  return(result)
}

get_question_answers_table <- function(subject, question){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    querySql <- "SELECT id, question_id, text, fraction, subject_code FROM answers WHERE subject_code = ? AND question_id = ?;"
    querySql <- sqlInterpolate(conn, querySql, subject, question)
    
    answers <- dbGetQuery(conn, querySql)
    answers$text <- iconv(answers$text, from = "UTF-8", to = "latin1")
    
    
    dbDisconnect(conn)
    return(answers)
  }, error = function(e) {
    message("OcurriÃ³ un error: ", e$message)
    
    return(NULL)
  })
  return(result)
}

update_test_question_number <- function(subject, nquestions){

  tryCatch({
    
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      host = "localhost", db="APPCUESTIONARIOS")
    querySql <- "UPDATE subjects SET questions_per_test = ? WHERE (subject_code = ?);"
    querySql <- sqlInterpolate(conn, querySql, nquestions, subject)
    
    dbSendQuery(conn, querySql)

    dbDisconnect(conn)

  }, error = function(e) {
    message("Ocurrió un error: ", e$message)
  })
}



add_questions <- function(file, subject, content){
  
  tryCatch({
    

  extract_question_id <- function(comment_text) {
    trimws(gsub(".*: (\\d+)", "\\1", comment_text))
  }
  
  extract_answer_info <- function(question_node){
    answers <- xml_find_all(question_node, ".//answer")
    question_type <- xml_attr(question_node, "type")
    question_comment <- xml_text(xml_find_first(question_node, ".//preceding-sibling::comment()[1]"))
    question_comment <- extract_question_id(question_comment)
    question_default_grade <-  as.numeric(xml_text(xml_find_first(question_node, ".//defaultgrade")))
    question_penalty <- as.numeric(xml_text(xml_find_first(question_node, ".//penalty")))
    
    correct_answers <- length(xml_find_all(question_node, ".//answer[@fraction > 0]"))
    incorrect_answers <- length(xml_find_all(question_node, ".//answer[@fraction = 0]"))
  
    
    answer_info <- function(answer_node, question_id, i){
      answer_fraction <- as.numeric(xml_attr(answer_node, "fraction"))
      answer_format <- xml_attr(answer_node, "format")
      answer_text <- xml_text(answer_node)
      answer_text <- stringr::str_replace_all(answer_text, pattern = stringr::fixed("\\"),
                                              replacement = "\\\\")
      answer_score_fraction <- 0
      answers_penalty_fraction <- 0
        if(correct_answers+incorrect_answers > 0){
          if(answer_fraction > 0 & correct_answers > 0){
            answer_score_fraction <- question_default_grade/correct_answers
            answers_penalty_fraction <- 0
          }else if(answer_fraction == 0 & incorrect_answers > 0){
            answer_score_fraction <- 0
            answers_penalty_fraction <- question_penalty/incorrect_answers
          }
        }



      # Concatenar question_id con el nÃºmero de respuesta (i)
      id <- paste0(trimws(question_id), "_", i)    
      answer_df <- data.frame(
        id = id,
        question_id = question_id,
        text = answer_text,
        fraction = answer_fraction,
        score_fraction = answer_score_fraction,
        penalty_fraction = answers_penalty_fraction,
        stringsAsFactors = FALSE
      )
      return(answer_df)
    }
    
    if(question_type=="category"){
      answers_df <- data.frame()
    }else{
      # Utilizamos mapply con la secuencia numÃ©rica para el nÃºmero de respuesta
      answers_df <- mapply(answer_info, answers, question_comment, 1:length(answers), SIMPLIFY = FALSE)
      # Luego combinamos las listas de dataframes en uno solo
      answers_df <- do.call(rbind, answers_df)
    }
    return(answers_df)
  }
  
  extract_question_info <- function(question_node){
    question_type <- xml_attr(question_node, "type")
    question_name <- xml_text(xml_find_first(question_node, ".//name/text"))
    question_comment <- xml_text(xml_find_first(question_node, ".//preceding-sibling::comment()[1]"))
    question_comment <- extract_question_id(question_comment)
    question_default_grade <-  xml_text(xml_find_first(question_node, ".//defaultgrade"))
    question_penalty <- xml_text(xml_find_first(question_node, ".//penalty"))
    question_single <- xml_text(xml_find_first(question_node, ".//single"))
    question_shuffle <- xml_text(xml_find_first(question_node, ".//shuffleanswers"))
    
    if(question_type=="category"){
      question_text <- NA
    }else{
      question_text <- xml_text(xml_find_first(question_node, ".//questiontext/text"))
      question_text <- stringr::str_replace_all(question_text, pattern = stringr::fixed("\\"),
                                                replacement = "\\\\")
      
    }
    
    question_df <- data.frame(
      id = question_comment,
      type = question_type,
      name = question_name,
      text = question_text,
      default_grade = question_default_grade,
      penalty = question_penalty,
      single = question_single,
      shuffle = question_shuffle,
      stringsAsFactors = FALSE
    )
    
    return(question_df)
  }
  
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                  dbname = "appcuestionarios",
                  host = "localhost")
  #quiz_xml <- read_xml("Calculo_Limites.xml")
  quiz_xml <- read_xml(file)
  questions <- xml_find_all(quiz_xml, "//question") 
  question_df <- do.call(rbind,Map(extract_question_info, questions))
  answer_df <- do.call(rbind, lapply(questions, extract_answer_info))
  answer_df$subject_code <- subject
  answer_df$content <- content
  answer_df
  question_df$subject_code <- subject
  question_df$content <- content
  
  question_df
  
  answer_col_ordered <- c("id","question_id", "text", "fraction", "subject_code", "content", "score_fraction", "penalty_fraction")
  build_insert_query <- function(row, tabla) {
    #row <- ifelse(is.na(row), "NULL", sprintf("'%s'", row))
    values <- paste0("'", row, "'", collapse = ", ")
    query <- paste("INSERT IGNORE INTO ", tabla, " VALUES (", values, ")", sep = "")
    return(query)
  }
  question_df <- subset(question_df, type != "category")
  question_df
  rownames(question_df) <- NULL 
  question_df$single <- ifelse(tolower(question_df$single) == "true", 1, 0)
  question_df$shuffle <- ifelse(tolower(question_df$shuffle) == "true", 1, 0)
  question_queries <- apply(question_df, 1, function(row) {
    build_insert_query(row, "Questions")
  })

  lapply(question_queries, dbSendQuery, conn = conn)
  answer_df <- answer_df[,answer_col_ordered]
  #answer_df$content <- iconv(answer_df$content, from = "UTF-8", to = "latin1")
  answer_queries <- apply(answer_df, 1, function(row) {
    build_insert_query(row, "Answers")
  })
  lapply(answer_queries, dbSendQuery, conn = conn)
  
  dbDisconnect(conn)
  })
}

delete_questions_and_answers <- function(question, subject, content){
  
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "  DELETE FROM questions
    WHERE id = ? AND subject_code = ? AND content = ? ;"
    querySql <- sqlInterpolate(conn, sql, question, subject, content)

    dbSendQuery(conn, querySql)
    
    sql <- "DELETE FROM answers
    WHERE question_id = ? AND subject_code = ? AND content = ? ;"
    querySql <- sqlInterpolate(conn, sql, question, subject, content)

    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
  }, error = function(e) {
    message("OcurriÃ³ un error: ", e$message)
  })

}



parseLattice <- function(json_data, subject_code){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    for (i in 1:nrow(json_data)) {
      nodeID <- json_data$id[[i]]
      insertNode(nodeID, subject_code, conn)
      children <- json_data$children[[i]]  # Acceder a la lista de children para la fila actual
      attributes <- json_data$attributes[[i]]  # Acceder a la lista de children para la fila actual
      if (length(children) > 0){
        lapply(children, function(childID) {
          insertChildren(nodeID, subject_code, childID,conn)
        })
      }
      if (length(attributes) > 0){
        lapply(attributes, function(attb) {
          if(attb != ""){
            insertAttb(nodeID, subject_code, attb,conn)
          }
        })
      }
    }

    dbDisconnect(conn)
  })

}



insertChildren <- function(nodeID,subject_code, childID, conn){
  sql <- "INSERT IGNORE INTO children (subject_code,nodeID, childID)
          values
          ( ?subject_code,?nodeID, ?childID);"
  
  querySql <- sqlInterpolate(conn, sql, nodeID = nodeID, subject_code = subject_code, childID = childID)
  dbSendQuery(conn,querySql)
}

insertNode <- function(nodeID,subject_code, conn){
  sql <- "INSERT IGNORE INTO nodes (subject_code,nodeID)
          values
          ( ?subject_code,?nodeID);"
  
  querySql <- sqlInterpolate(conn, sql, nodeID = nodeID, subject_code = subject_code)
  dbSendQuery(conn,querySql)
}

insertAttb <- function(nodeID,subject_code, attribute, conn){
  sql <- "INSERT IGNORE INTO attributes (subject_code,nodeID,attribute)
          values
          ( ?subject_code,?nodeID,?attribute);"
  
  querySql <- sqlInterpolate(conn, sql, nodeID = nodeID, subject_code = subject_code, attribute=attribute)
  dbSendQuery(conn,querySql)
}


get_subject_attempts_table <- function(user, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT * FROM test_attempts WHERE user = ? AND subject = ?"
    querySql <- sqlInterpolate(conn, sql, user, subject)
    result <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(result)
  })
  return(result)
}

get_average_score <- function(user, subject){

  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT u.user_id, 
      AVG(ta.score) AS average_score
      FROM appcuestionarios.users u
      JOIN appcuestionarios.test_attempts ta ON u.user_id = ta.user
      WHERE u.user_id = ? AND ta.subject = ?
      GROUP BY u.user_id;"
    querySql <- sqlInterpolate(conn, sql, user, subject)
    res <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(res$average_score)
  }, error = function(e) {

    return(NULL)
  })
  return(result)
}

get_barplot_subject <- function(subject, content){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "ISO-8859-1")
    query <- " SELECT 
        SUM(CASE WHEN score < 5 THEN 1 ELSE 0 END) AS suspensos,
        SUM(CASE WHEN score BETWEEN 5 AND 9 THEN 1 ELSE 0 END) AS aprobados,
        SUM(CASE WHEN score > 9 THEN 1 ELSE 0 END) AS sobresalientes
    FROM test_attempts WHERE subject = ? AND content = ?;"
    querySql <- sqlInterpolate(conn, query, subject, content)

    result <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    result[is.na(result)] <- 0
    result <- unlist(result[1,])
    return(result)
  })
  return(result)
}

update_test_number_question <- function(subject, nquestions){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "ISO-8859-1")
    query <- "UPDATE subjects SET questions_per_text = ? WHERE (subject_code = ?);"
    querySql <- sqlInterpolate(conn, query, nquestions, subject)
    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
  })
  
}

get_subject_data <- function(subject){
  
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "ISO-8859-1")
    query <- "SELECT * FROM subjects WHERE subject_code = ? "
    querySql <- sqlInterpolate(conn, query, subject)

    res <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(res)
  }, error = function(e) {
    message("OcurriÃ³ un error: ", e$message)
    return(NULL)
  })
  
  return(result)
}


################################################################################
################################################################################
# Funciones vista Alumno
################################################################################
################################################################################

get_test_attempt_table <- function(user, subject){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT content, score, correct, incorrect, date FROM test_attempts
        WHERE user = ? and subject = ?;"
    querysql <- sqlInterpolate(conn, sql, user, subject)
    result <- dbGetQuery(conn, querysql)
    result$content = iconv(result$content, from = "UTF-8" , to = "ISO-8859-1")
    dbDisconnect(conn)
    return(result)
  })

}

update_node_path <- function(subject, user, node){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "latin1")
    sql <- "INSERT IGNORE INTO visited_nodes (subject_code, user_id, nodeID) VALUES(?,?,?);"
    querySql <- sqlInterpolate(conn, sql, subject, user, node)
    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
  })
  
}

get_passed_contents_table <- function(user, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "latin1")
    sql <- "SELECT content, MAX(score) AS score, COUNT(*) AS attempt_count
  FROM test_attempts 
  WHERE user = ? AND subject = ? AND score > 0
  GROUP BY content 
  ORDER BY score DESC;"
    querySql <- sqlInterpolate(conn, sql, user, subject)
    contents <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    #content
    return(iconv(contents, from = "UTF-8", to = "latin1"))
  })
  return(result)
}

get_passed_contents <- function(user, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", 
                      encoding = "latin1")
    
    sql <- "SELECT content, min(date) AS date FROM test_attempts WHERE
          user = ? AND subject = ? AND score >= 5
          GROUP By content ORDER BY date ASC;"
    
    querySql <- sqlInterpolate(conn, sql, user, subject)
    contents <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    return(iconv(contents$content, from = "UTF-8", to = "latin1"))
  })
  return(result)
}

get_best_grade_content <- function(user, subject, t1 = 5, t2= 100){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    
    sql <- "WITH MaxScores AS (
    SELECT content, MAX(score) AS score
    FROM test_attempts
    WHERE user = ? AND subject = ? 
    GROUP BY content
  )
  
  SELECT content, score
  FROM MaxScores WHERE score >= ? and score < ?
  ORDER BY score ASC;"
    
    querySql <- sqlInterpolate(conn, sql, user, subject, t1,t2)
    contents <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    return(iconv(contents$content, from = "UTF-8", to = "latin1"))
  })
  return(result)
}

get_contents_to_improve <- function(user, subject, threshold){
  contents <- get_best_grade_content(user, subject, threshold)
  return(contents)
}

get_contents_to_test <- function(user, subject){
  current_node <- get_current_node(user, subject)
  children <- get_children(current_node, subject)
  
  contents <- unique(unlist(lapply(children, get_node_attb, subject = subject)))
  passed_contents <- get_passed_contents(user, subject)
  
  result <- if (length(passed_contents) > 0) {
    setdiff(contents, passed_contents)
  } else {
    contents
  }
  return(result)
  
}



update_current_node <- function(user, subject){
  current_node <- get_current_node(user, subject)
  update_node_path(subject,user,current_node)
  passed_contents <- get_passed_contents(user, subject)
  children <- get_children(current_node, subject)
  update_node <- FALSE
  i <- 1
  while (!update_node & i <= length(children)) {
    if(all(get_node_attb(children[i], subject)  %in% passed_contents )){

      update_node = TRUE
    }else{
      i <- i+1
    }
  }
  
  if(update_node){
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "UPDATE user_subjects SET current_node = ? WHERE user_id = ? and subject_code = ?;"
    
    querySql <- sqlInterpolate(conn, sql,  children[i], user, subject)
    dbSendQuery(conn, querySql)
    update_node_path(subject,user,children[i])
    dbDisconnect(conn)
  }
  
}

get_current_node <- function(user, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT current_node FROM user_subjects WHERE
          subject_code = ? AND user_id = ? ;"
    
    querySql <- sqlInterpolate(conn, sql,  subject, user)

    current_node <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(current_node$current_node)
  })
  return(result)
}

get_children <- function(node, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT c.childID
        FROM children c JOIN attributes a ON c.subject_code = a.subject_code AND c.childID = a.nodeID
        WHERE c.subject_code = ? AND c.nodeID = ?
        GROUP BY c.childID
        ORDER BY COUNT(a.attribute) ASC ;"
    
    querySql <- sqlInterpolate(conn, sql,  subject, node)
    children <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(children$childID)
  })
  return(result)
}

get_node_attb <- function(node, subject){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT attribute FROM attributes WHERE
          subject_code = ? AND nodeID = ? ;"
    
    querySql <- sqlInterpolate(conn, sql,  subject, node)
    attributes <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    
    return(iconv(attributes$attribute, from = "UTF-8", to = "latin1"))
  })
  return(result)
}




################################################################################
################################################################################
# Funciones vista Cuestionario
################################################################################
################################################################################
generate_attempt <- function(user, subject, content){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", encoding='latin1')
    
    
    # Convertir el UUID a un nÃºmero entero
    # Extraer los primeros 10 dÃ­gitos del UUID convertido
    random_id <- random_number <- sample(1000000000:9999999999, 1)

    #id aleatorio
    #escribo el intento relacionandolo con el usuario y la asignatura. aqui ira la nota
    sql <- "INSERT INTO test_attempts (id, user, subject,score, content, date)
          values
          (?id, ?user, ?subject,8, ?content, ?date);"
    content <- iconv(content, from = "latin1", to = "UTF-8")
    querySql <- sqlInterpolate(conn, sql, id = random_id, user = user, subject = subject,
                               content = content, date = as.character(now()))

    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
    return(random_id)
  }, error = function(e) {

    return(NULL)
  }) 
  return(result)
}

get_questions <- function(attemtp_id, subject, content){
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost", encoding = "latin1")
    #selecciono las preguntas. Escribo en la tabla de preguntas_cuestionario_intento
    sql <- "SELECT * FROM (
      SELECT q.*, ROW_NUMBER() OVER (ORDER BY RAND()) as row_num
      FROM questions q WHERE q.subject_code = ?subject AND q.content = ?content
    ) as subquery WHERE 
  row_num <= (
    SELECT s.questions_per_test 
    FROM appcuestionarios.subjects s 
    WHERE s.subject_code = ?subject
  );"
    content <- iconv(content, from = "latin1", to = "UTF-8")
    
    querySql <- sqlInterpolate(conn, sql, subject = subject, content = content)


    question_ids <- dbGetQuery(conn, querySql)
    
    for (question_id in question_ids$id) {
      
      querySql <- sqlInterpolate(
        conn,
        "INSERT INTO attempt_questions (attempt_id, question_id) VALUES (?, ?);",
        attemtp_id, question_id
      )

      dbSendQuery(conn, querySql)
    }
    
    question_ids$text <- iconv(question_ids$text, from = "UTF-8", to = "ISO-8859-1")
    return(question_ids)
    dbDisconnect(conn)
  })
  return(result)
  ## retornar lista id preguntas
}

get_answers <- function(attempt_id){
  
  result <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT * from attempt_questions WHERE attempt_id = ?;"
    querySql <- sqlInterpolate(conn, sql, attempt_id)
    question_ids <- dbGetQuery(conn, querySql)
    question_ids
    n_questions <- length(question_ids$question_id)
    answers <- lapply(question_ids$question_id, function(question) {
      sql <- "SELECT * FROM Answers WHERE question_id = ?question_id"
      querySql <- sqlInterpolate(conn, sql, question_id = question)
      question_answers <- dbGetQuery(conn, querySql)
      #### IMPORTANTE CHEQUEAR ICONV PARA RESOLVER EL ENCODING
      question_answers$text <- iconv(question_answers$text, from = "UTF-8", to = "latin1")
      
      sql <- "SELECT shuffle FROM questions WHERE id = ?question_id"
      querySql <- sqlInterpolate(conn, sql, question_id = question)
      shuffle <- dbGetQuery(conn, querySql)$shuffle
      if (shuffle==1) {
        question_answers <- question_answers[sample(nrow(question_answers)), ]
      }
      question_answers
    })
    return(answers)
    dbDisconnect(conn)
    
  })
  
  return(result)
}

calculate_results <- function(attempt_id){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT SUM(a.score_fraction) - SUM(a.penalty_fraction) AS total_score,
        COUNT(CASE WHEN a.penalty_fraction = 0 THEN 1 END) AS correct_answers,
        COUNT(CASE WHEN a.penalty_fraction <> 0 THEN 1 END) AS incorrect_answers
        FROM attempt_answers aa JOIN answers a ON aa.answer_id = a.id
        WHERE aa.attempt_id = ?;"
    
    sql <- "SELECT GREATEST(COALESCE(sc.score, 0), 0) / COALESCE((tdg.total_default_grade/10), 1) AS score, sc.correct_answers,
    sc.incorrect_answers, tdg.total_default_grade FROM (
      SELECT 
          SUM(a.score_fraction) - SUM(a.penalty_fraction) AS score,
          COUNT(CASE WHEN a.penalty_fraction = 0 THEN 1 END) AS correct_answers,
          COUNT(CASE WHEN a.penalty_fraction <> 0 THEN 1 END) AS incorrect_answers
      FROM attempt_answers aa JOIN answers a ON aa.answer_id = a.id
      WHERE aa.attempt_id = ?attempt_id
    ) sc CROSS JOIN (
    SELECT SUM(q.default_grade) AS total_default_grade
    FROM attempt_questions aq
    JOIN questions q ON aq.question_id = q.id
    WHERE aq.attempt_id = ?attempt_id) tdg;"

    querySql <- sqlInterpolate(conn, sql, attempt_id =attempt_id)
    results <- dbGetQuery(conn, querySql)

    sql <- "UPDATE test_attempts SET score = ? , correct = ? , incorrect = ?
      WHERE id = ?;"
    querySql <- sqlInterpolate(conn, sql, results$score, results$correct_answers, results$incorrect_answers, attempt_id)
    dbSendQuery(conn, querySql)
    dbDisconnect(conn)
    return(results$total_score)
  })
}

get_results <- function(attempt_id){
  score <- tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    sql <- "SELECT score FROM test_attempts WHERE id = ?;"
    querySql <- sqlInterpolate(conn, sql, attempt_id)
    res <- dbGetQuery(conn, querySql)
    dbDisconnect(conn)
    return(res)
  })
  return(score)
}


insert_attempt_answers <- function(attempt_id, answers){
  tryCatch({
    conn <- dbConnect(MySQL(), user = "root", password = "root", 
                      dbname = "appcuestionarios", host = "localhost")
    for (answer in answers) {
      querySql <- sqlInterpolate(
        conn,
        "INSERT INTO attempt_answers (attempt_id, answer_id) VALUES (?, ?);",
        attempt_id, answer
      )
      dbSendQuery(conn, querySql)
    }
    dbDisconnect(conn)
  })
  
}


finalizarCuestionario <- function(attempt_id, question_ids, user_answers){
  #subo todas las respuestas, relacionadas con el intento de cuestionario
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost")
  
  answer_ids <- lapply(1:nrow(user_answers), function(i) user_answers[[paste0("respuestas_", i)]])
  answer_ids <- Filter(Negate(is.null), answer_ids)  # Eliminar entradas NULL
  answer_ids <- unlist(answer_ids, recursive = TRUE)
  for (answer_id in answer_ids) {
    querySql <- sqlInterpolate(
      conn,
      "INSERT INTO attempt_answers (attempt_id, answer_id) VALUES (?, ?);",
      attempt_id, answer_id
    )
    dbSendQuery(conn, querySql)
  }
  
  #calculo la nota y hago upgrade en la tabla de intentos
  filtered_answers <- lapply(answers, function(df) {
    df %>% filter(id %in% user_answers)
  })
  nota <- 0
  for(i in 1:nrow(question_ids)){
    question_id <- question_ids$id[i]
    grade <- question_ids$default_grade[i]
    penalty <- question_ids$penalty[i]
    sql <- "select count(id) from answers where question_id = ? and fraction = 0;"
    querySql <- sqlInterpolate(
      conn,
      "SELECT COUNT(id) AS wrong_answers FROM answers WHERE question_id = ? AND fraction = 0;",
      #question_id
      713896
    )
    summary(answers)
    relative_penalty <- dbGetQuery(conn, querySql)$wrong_answers
    grade <- lapply(filtered_answers, function(df) {
      df %>%
        #Creo una col con la nota
        mutate(grade = ifelse(fraction == 0, -penalty, valor1 / fraction)) %>%
        #me quedo con el sum de la nota
        summarise(grade = sum(grade)) %>%
        pull(grade)
    })
    
    # Aplano y sumo
    grade <- sum(unlist(grade))
  }
}


################################################################################
# Funciones Auxiliares
################################################################################

create_conn <- function(){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost", encoding = "utf8mb4") ####
  return(conn)
}

close_conn <- function(conn){
  dbDisconnect(conn)
}


