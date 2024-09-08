library(shiny)
library(bslib)
options(encoding = 'UTF-8')

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
}

get_passed_contents <- function(user, subject){
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
}

get_best_grade_content <- function(user, subject, t1 = 5, t2= 100){
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
}

get_contents_to_improve <- function(user, subject, threshold){
  contents <- get_best_grade_content(user, subject, threshold)
  return(contents)
}

get_contents_to_test <- function(user, subject){
  current_node <- get_current_node(user, subject)
  print(current_node)
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
    print("passed contents")
    print(passed_contents)
    print("Children_content")
    print(get_node_attb(children[i], subject))
    if(all(get_node_attb(children[i], subject)  %in% passed_contents )){
      print("Node update")
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
    
    ## insert into visited_nodes 
  }
  
}

get_current_node <- function(user, subject){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost")
  sql <- "SELECT current_node FROM user_subjects WHERE
          subject_code = ? AND user_id = ? ;"
  
  querySql <- sqlInterpolate(conn, sql,  subject, user)
  print(querySql)
  current_node <- dbGetQuery(conn, querySql)
  dbDisconnect(conn)
  return(current_node$current_node)
}

get_children <- function(node, subject){
  ## Ordenados por el numero de atributos
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
}

get_node_attb <- function(node, subject){
  conn <- dbConnect(MySQL(), user = "root", password = "root", 
                    dbname = "appcuestionarios", host = "localhost")
  sql <- "SELECT attribute FROM attributes WHERE
          subject_code = ? AND nodeID = ? ;"
  
  querySql <- sqlInterpolate(conn, sql,  subject, node)
  attributes <- dbGetQuery(conn, querySql)
  dbDisconnect(conn)
  
  return(iconv(attributes$attribute, from = "UTF-8", to = "latin1"))
}


######

mytabAlumno <- function(tabName, id, user){
  ns <- NS(id)
  contents <- get_contents_to_test(user$user_id,tabName)
  tabPanel(
    tabName,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns(paste0("tabla_container", tabName, sep="")))
      ),
      mainPanel(
        h4(paste0("Panel de la asignatura ", tabName)), 
        
        useShinyjs(),  # Inicializar shinyjs
        verbatimTextOutput(ns(paste0("subject_name_panel_", tabName))),
        verbatimTextOutput(ns(paste0("subject_description_panel_", tabName))),
        verbatimTextOutput(ns(paste0("subject_average_panel_", tabName))),
        hidden(
          div(id = ns(paste0("panel3",tabName)),         h3("Contenidos superados"),
              h2(HTML('<img src="three_stars.png" alt="estrella" style="height:20px;"> 3 Estrellas')),
              uiOutput(ns(paste0("3stars", tabName))),
              lapply(get_best_grade_content(user$user_id,tabName,9), function(content){
                actionButton(ns(paste0(paste0(content, "_btn3"),tabName)), label = content)
              })),
          div(id = ns(paste0("panel2",tabName)),         h3("Contenidos a mejorar"),
            h2(HTML('<img src="two_stars.png" alt="estrella" style="height:20px;"> 2 Estrellas')),
            uiOutput(ns(paste0("2stars", tabName))),
            lapply(get_best_grade_content(user$user_id,tabName,7,9), function(content){
              actionButton(ns(paste0(paste0(content, "_btn2"),tabName)), label = content)
            }),),
          div(id = ns(paste0("panel1",tabName)), h2(HTML('<img src="one_star.png" alt="estrella" style="height:20px;"> 1 Estrella')),
              uiOutput(ns(paste0("1star", tabName))),
              lapply(get_best_grade_content(user$user_id,tabName, 5,7), function(content){
                actionButton(ns(paste0(paste0(content, "_btn1"),tabName)), label = content)
              }),)

        ),
        
        



        h2("Contenidos por superar"),
        lapply(get_contents_to_test(user$user_id,tabName), function(content){
          actionButton(ns(paste0(paste0(content, "_btn"),tabName)), label = content)
        })
        
      )
    )
  )
}

tabCuestionario <- function(id){
  ns <- NS(id)
  tabPanel(
    "Cuestionario",
    tags$head(
      tags$meta(charset = "UTF-8")
    ),
    useShinyjs(),
    withMathJax(),
    titlePanel("Cuestionario"),
    fluidRow(
      column(width = 2,
             wellPanel(
               uiOutput(ns("selector_pregunta"))
             )
      ),
      column(width = 9,
             wellPanel(
               uiOutput(ns("pregunta")),
               uiOutput(ns("respuestas")),
               br(),
               fluidRow(
                 column(width = 6, align = "left",
                        uiOutput(ns("btn_anterior_ui"))
                 ),
                 column(width = 6, align = "right",
                        uiOutput(ns("btn_siguiente_ui"))
                 )
               ),
               br(),
               fluidRow(
                 column(width = 6,
                        actionButton(ns("btn_finalizar"), "Finalizar cuestionario", icon = icon("check"))
                 )
               )
             )
      )
    ),
    br(),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('reprocessMath', function(message) {
      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    });
  "))
  )
}


alumnoUI <- function(id, user){
  useShinyjs()
  tab_list <- get_subjects(user$user_id) ##
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Cuestionarios vista Alumno", 
      theme = bslib::theme_bootswatch("cerulean"),
      id=ns("alumnoTabSet"),
      tabPanel("Home",
               icon = icon("house"),
               mainPanel(
                 
                 verbatimTextOutput(ns("texto_bienvenida")),
                 verbatimTextOutput(ns("user_id")),
                 
               )
      ),
      !!!lapply(tab_list, function(x){
        mytabAlumno(x, id, user)
      })
    )
  )
}





alumnoServer <- function(id, user) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      print(user$user_id)
      currentTab <- reactive(input$alumnoTabSet)
      #currentTab <- debounce(currentTab, 500)
      #modoCuestionario <- reactiveVal(FALSE)
      subjects <- get_subjects(user$user_id)
      subject_data <- reactiveVal(get_subject_data(input$alumnoTabSet))
      attempt_id <- reactiveVal()

      preguntas <- reactiveVal()
      respuestas <- reactiveVal()

      num_preguntas <-reactiveVal()
      
      pregunta_actual <- reactiveVal(1)
      respuestas_seleccionadas <- reactiveValues() 
      
      
      output[[paste0("subject_name_panel_", isolate(currentTab()))]] <- renderText({
        print(subject_data())
        paste("Nombre: ", 
              iconv(isolate(subject_data())$name, from = "UTF-8", to = "latin1"))
      })
      
      output[[paste0("subject_description_panel_", isolate(currentTab()))]] <- renderText({
        paste("Descripción: ", 
              iconv(isolate(subject_data())$description, from = "UTF-8", to = "latin1"))
      })
      
      output[[paste0("subject_average_panel_", isolate(currentTab()))]] <- renderText({
        paste("Mi nota media: ", 
              get_average_score(user$user_id, isolate(currentTab())))
      })
      
      
      
      contents <- reactive({
        if (!is.null(currentTab()) && (!currentTab() %in% c("Home", "Cuestionario")))  {
          get_attb(isolate(currentTab()))
        } else {
          list("home")  # Devuelve una lista vacía si el tab es "Home"
        }
      })
      
      lapply(c("_btn","_btn1","_btn2","_btn3"), function(x){
        lapply(isolate(contents()), function(content){
          #content <- iconv(content,from = "UTF-8", to = "latin1")
          observeEvent(input[[paste0(paste0(content, x),isolate(currentTab()))]], {
            lapply(get_subjects(user$user_id), function(tab) {
              hideTab(inputId = "alumnoTabSet", target = tab)
            })
            isolate({
              test_id <- generateAttempt(user$user_id, isolate(currentTab()), content)
              test_questions <- get_questions(test_id, isolate(currentTab()), content)
              test_answers <- get_answers(test_id)
              test_questions <- test_questions[order(test_questions$id), ]
            })
            update_current_node(user$user_id, isolate(currentTab()))
            
            attempt_id(test_id)
            preguntas(test_questions)
            respuestas(test_answers)
            num_preguntas(nrow(isolate(preguntas())))
            appendTab(inputId = "alumnoTabSet", tabCuestionario(id))
            updateTabsetPanel(session, "alumnoTabSet", selected = "Cuestionario")
            

            for (i in 1:(num_preguntas())) {
              respuestas_seleccionadas[[paste0("respuestas_", i)]] <- NULL
            }

            output$pregunta <- renderUI({
              req(preguntas()) 
              question <- preguntas()$text[pregunta_actual()]
              #question <- iconv(question, from = "UTF-8", to = "ISO-8859-1")

              tagList(
                withMathJax(),
                HTML(paste0("<div style='font-size: 18px;'>", question, "</div>")),
                br(),
                #h3(preguntas()$id[pregunta_actual()])
              )
            })
            
            output$selector_pregunta <- renderUI({
              lista_botones <- lapply(1:isolate(num_preguntas()), function(i) {
                actionButton(inputId = ns(paste0("btn_pregunta_", i)),
                             label = as.character(i),
                             onclick = sprintf("Shiny.setInputValue('%s', %d);",ns('selected_pregunta'), i),
                             style = "border: 1px; font-size: 10px; width: 20px; height: 20px; line-height: 20px;
                   padding: 0; text-align: center; vertical-align: middle;")
              })
            })
            
            observeEvent(input$selected_pregunta, {
              pregunta_actual(input$selected_pregunta)
            })
            
            
            
            output$respuestas <- renderUI({
              pregunta_actual <- pregunta_actual()  # Almacenamos la pregunta actual
              withMathJax()
              checkboxGroupInput(inputId = ns("respuestas_seleccionadas"),
                                 label = NULL,
                                 #choices = respuestas[[pregunta_actual]]$text,
                                 choiceNames = respuestas()[[pregunta_actual]]$text,   
                                 choiceValues = respuestas()[[pregunta_actual]]$id,   
                                 selected = respuestas_seleccionadas[[paste0("respuestas_", pregunta_actual)]])
            })
            
            output$btn_anterior_ui <- renderUI({
              if (pregunta_actual() > 1) {
                actionButton(ns("btn_anterior"), "Pregunta anterior", icon = icon("chevron-left"))
              }
            })
            
            output$btn_siguiente_ui <- renderUI({
              if (pregunta_actual() < isolate(num_preguntas())) {
                actionButton(ns("btn_siguiente"), "Pregunta siguiente", icon = icon("chevron-right"))
              }
            })
            
            observeEvent(input$btn_anterior, {
              if (isolate(pregunta_actual()) > 1) {
                pregunta_actual(isolate(pregunta_actual()) - 1)
              }
            })
            
            observe({
              if (pregunta_actual() == 1) {
                shinyjs::hide("btn_anterior")
              } else {
                shinyjs::show("btn_anterior")
              }
              
              if (pregunta_actual() == isolate(num_preguntas())) {
                shinyjs::hide("btn_siguiente")
              } else {
                shinyjs::show("btn_siguiente")
              }
            })
            
            observe({
              for (i in 1:isolate(num_preguntas())) {
                runjs(sprintf('document.getElementById("%s%d").style.border = "none";',ns("btn_pregunta_"), i))
              }
              runjs(sprintf('document.getElementById("%s%d").style.border = "1px solid black";', ns("btn_pregunta_"),pregunta_actual()))
            })
            
            
            
            observeEvent(input$respuestas_seleccionadas, {
              selected_items <- input$respuestas_seleccionadas
              print(selected_items)
              respuestas_seleccionadas[[paste0("respuestas_", isolate(pregunta_actual()))]] <- selected_items
              
              ## funcion actualizar boton
              if(length(selected_items > 0)){
                runjs(sprintf('document.getElementById("%s%d").style.backgroundColor = "#A9CDF0";',
                              ns("btn_pregunta_"), isolate(pregunta_actual())))
              }else if(length(selected_items == 0)){
                print("no seleccionada resp")
                runjs(sprintf('document.getElementById("%s%d").style.backgroundColor = "#FFFFFF";',
                              ns("btn_pregunta_"), isolate(pregunta_actual())))
              }
              session$onFlushed(function() {
                session$sendCustomMessage(type = 'reprocessMath', message = 'reprocess')
              }, once = TRUE)
            })
            
            observeEvent(input$btn_siguiente, {
              if (isolate(pregunta_actual()) < num_preguntas()) {
                pregunta_actual(isolate(pregunta_actual()) + 1)
              }
            })
            
            observeEvent(input$btn_finalizar, {
              debounced_show_modal <- debounce(reactive(input$btn_finalizar), millis = 8000)
              hideTab(inputId = "alumnoTabSet", target = "Cuestionario")
              
              final_answers <- lapply(1:isolate(num_preguntas()), function(i) respuestas_seleccionadas[[paste0("respuestas_", i)]])
              final_answers <- Filter(Negate(is.null), final_answers) 
              final_answers <- unlist(final_answers, recursive = TRUE)
              insert_attempt_answers(attempt_id(), final_answers)
              calculate_results(attempt_id())
              score <- get_results(attempt_id())
              print(score)
              if((score >= 5) & (!is.na(score)) ){
                update_current_node(user$user_id, isolate(currentTab()))
              }
              showModal(modalDialog(
                title = "Cuestionario finalizado",
                paste("Tu puntuación es: ", score),
                easyClose = FALSE,
                footer = NULL,
                div(
                  actionButton(inputId = ns("close_results"), label = "Salir",
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  
                  
                  align = "center"),
              ))
              
              observeEvent(input$close_results, {
                session$reload() 
              })
              #session$reload() cuando se pulse el boton de cerrar el dialog de puntuacion, reload
            },  ignoreNULL = TRUE, ignoreInit = TRUE)
            
          })
          
        })
      })
      
      
      observe({
        if (!is.null(currentTab()) && (!currentTab() %in% c("Home", "Cuestionario")))  {
        show_panel3 <- length(get_best_grade_content(user$user_id, currentTab(), 9)) > 0
        show_panel2 <- length(get_best_grade_content(user$user_id, currentTab(), 7)) > 0
        show_panel1 <- length(get_best_grade_content(user$user_id, currentTab(), 5)) > 0
        
        # Mostrar u ocultar los paneles basados en las condiciones
        shinyjs::toggle(paste0("panel1",currentTab()), condition = show_panel1)
        shinyjs::toggle(paste0("panel2",currentTab()), condition = show_panel2)
        shinyjs::toggle(paste0("panel3",currentTab()), condition = show_panel3)
        }
      })
      
      
      
      output$texto_bienvenida <- renderText({
        paste("Bienvenido a la plataforma ", user$name)
      })
      
      output$user_id <- renderText({
        paste("Tu ID de usuario es:", user$user_id)
      })
      
      observeEvent(input[[paste0("add_tab_",isolate(currentTab()))]], {
        lapply(get_subjects(user$user_id), function(tab) {
          hideTab(inputId = "alumnoTabSet", target = tab)
        })
        appendTab(inputId = "alumnoTabSet", tabCuestionario(id))
        updateTabsetPanel(session, "alumnoTabSet", selected = "Cuestionario")
        
      })
      

      
      output[[paste0("tabla_container", currentTab(), sep="")]] <- renderUI({
        dataTableOutput(ns(paste0("tabla_resultados", currentTab(), sep="")))

      })
      
      output[[paste0("tabla_resultados", currentTab(), sep="")]] <- renderDataTable({
        #usuarios
        get_test_attempt_table(user$user_id, currentTab())
      }, selection = 'single', rownames = FALSE)
      
      
################################################################################
      #options(encoding = 'utf8')
      
      

      #preguntas <- dbGetQuery(bd, "SELECT * FROM Questions ORDER BY RAND() LIMIT 10")
      
      #### IMPORTANTE CHEQUEAR ICONV PARA RESOLVER EL ENCODING
      #preguntas()$text <- iconv(preguntas()$text, from = "UTF-8", to = "latin1")
      #num_preguntas <- nrow(preguntas())
      
      # Obtener respuestas de las preguntas seleccionadas de la base de datos

      
      
      
      
      

    }
  )
}