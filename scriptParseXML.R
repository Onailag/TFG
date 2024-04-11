library(xml2)
library(XML)
library(dplyr)
library(RMySQL)
library(stringr)

################################### NO BORRAR #################################

extract_question_id <- function(comment_text) {
  trimws(gsub(".*: (\\d+)", "\\1", comment_text))
}

extract_answer_info <- function(question_node){
  answers <- xml_find_all(question_node, ".//answer")
  question_type <- xml_attr(question_node, "type")
  question_comment <- xml_text(xml_find_first(question_node, ".//preceding-sibling::comment()[1]"))
  question_comment <- extract_question_id(question_comment)
  
  answer_info <- function(answer_node, question_id, i){
    answer_fraction <- as.numeric(xml_attr(answer_node, "fraction"))
    answer_format <- xml_attr(answer_node, "format")
    answer_text <- xml_text(answer_node)
    answer_text <- stringr::str_replace_all(answer_text, pattern = stringr::fixed("\\"),
                                              replacement = "\\\\")
    # Concatenar question_id con el número de respuesta (i)
    id <- paste0(trimws(question_id), "_", i)    
    answer_df <- data.frame(
      id = id,
      question_id = question_id,
      text = answer_text,
      fraction = answer_fraction,
      stringsAsFactors = FALSE
    )
    return(answer_df)
  }
  
  if(question_type=="category"){
    answers_df <- data.frame()
  }else{
    # Utilizamos mapply con la secuencia numérica para el número de respuesta
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

# Conexión con la base de datos 
bd <- dbConnect(MySQL(), user = "root", password = "root", 
                dbname = "appcuestionarios",
                host = "localhost")


# Creo tabla questions
dbSendQuery(bd, "CREATE TABLE Questions (
  id VARCHAR(10) PRIMARY KEY,
  type VARCHAR(20) NOT NULL,
  name VARCHAR(50),
  text VARCHAR(250) NOT NULL,
  default_grade FLOAT NOT NULL,
  penalty FLOAT NOT NULL,
  single BOOLEAN,
  shuffle BOOLEAN
)")

# Creo tabla answers
dbSendQuery(bd, "CREATE TABLE Answers (
  id VARCHAR(10) PRIMARY KEY,
  question_id VARCHAR(10),
  text VARCHAR(250) NOT NULL,
  fraction FLOAT NOT NULL,
  FOREIGN KEY (question_id) REFERENCES Questions(id)
)")

quiz_xml <- read_xml("Calculo_Limites.xml")
questions <- xml_find_all(quiz_xml, "//question") 
question_df <- do.call(rbind,Map(extract_question_info, questions))
answer_df <- do.call(rbind, lapply(questions, extract_answer_info))
answer_df
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
question_queries
lapply(question_queries, dbSendQuery, conn = bd)

dbGetQuery(bd, "select * from Questions where shuffle = TRUE")

answer_queries <- apply(answer_df, 1, function(row) {
  build_insert_query(row, "Answers")
})
answer_queries
lapply(answer_queries, dbSendQuery, conn = bd)

dbDisconnect(bd)
################################### NO BORRAR #################################



preguntas_aleatorias <- dbGetQuery(bd, "SELECT * FROM Questions ORDER BY RAND() LIMIT 10;")

# Convertir los IDs de las preguntas a una cadena separada por comas
ids_preguntas_str <- paste(preguntas_aleatorias$id, collapse = ",")

# Construir la consulta SQL para obtener las respuestas correspondientes
consulta_respuestas <- paste("SELECT * FROM Answers WHERE question_id IN (", ids_preguntas_str, ")", sep = "")

# Obtener las respuestas correspondientes a las preguntas seleccionadas
respuestas <- dbGetQuery(bd, consulta_respuestas)



respuestas_por_pregunta <- split(respuestas, respuestas$question_id)
respuestas_por_pregunta
lista_respuestas <- lapply(respuestas_por_pregunta, function(df) df$text)
lista_respuestas
#Ordenado
lista_respuestas <- lapply(respuestas_por_pregunta, function(df) {
  lapply(1:nrow(df), function(i) {
    list(id = df$id[i], text = df$text[i])
  })
})
#Desordenado
lista_respuestas <- lapply(respuestas_por_pregunta, function(df) {
  indices_desordenados <- sample(1:nrow(df))
  lapply(indices_desordenados, function(i) {
    list(id = df$id[i], text = df$text[i])
  })
})
lista_respuestas[1]
