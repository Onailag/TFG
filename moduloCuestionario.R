library(shiny)
library(shinyjs)
library(RMySQL)


# Conexión a la base de datos
bd <- dbConnect(MySQL(), user = "root", password = "root", 
                dbname = "appcuestionarios",
                host = "localhost",
                encoding = "latin1")



ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  titlePanel("Cuestionario"),
  fluidRow(
    column(width = 2,
           wellPanel(
             uiOutput("selector_pregunta")
           )
    ),
    column(width = 9,
           wellPanel(
             uiOutput("pregunta"),
             uiOutput("respuestas"),
             br(),
             fluidRow(
               column(width = 6, align = "left",
                      uiOutput("btn_anterior_ui")
               ),
               column(width = 6, align = "right",
                      uiOutput("btn_siguiente_ui")
               )
             ),
             br(),
             fluidRow(
               column(width = 6,
                      actionButton("btn_finalizar", "Finalizar cuestionario", icon = icon("check"))
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

server <- function(input, output, session) {
  options(encoding = 'utf8')
  pregunta_actual <- reactiveVal(1)
  respuestas_seleccionadas <- reactiveValues() 
  for (i in 1:10) {
    respuestas_seleccionadas[[paste0("respuestas_", i)]] <- NULL
  }
  preguntas <- dbGetQuery(bd, "SELECT * FROM Questions ORDER BY RAND() LIMIT 10")
  
  #### IMPORTANTE CHEQUEAR ICONV PARA RESOLVER EL ENCODING
  preguntas$text <- iconv(preguntas$text, from = "UTF-8", to = "latin1")
  num_preguntas <- nrow(preguntas)
  
  # Obtener respuestas de las preguntas seleccionadas de la base de datos
  respuestas <- lapply(1:num_preguntas, function(i) {
    respuestas_pregunta <- dbGetQuery(bd, paste("SELECT * FROM Answers WHERE question_id =", preguntas$id[i]))
    #### IMPORTANTE CHEQUEAR ICONV PARA RESOLVER EL ENCODING
    respuestas_pregunta$text <- iconv(respuestas_pregunta$text, from = "UTF-8", to = "latin1")
    # Ordenar las respuestas si la columna shuffle está en TRUE
    if (preguntas$shuffle[i]) {
      respuestas_pregunta <- respuestas_pregunta[sample(nrow(respuestas_pregunta)), ]
    }
    respuestas_pregunta
  })
  
  
  
  output$selector_pregunta <- renderUI({
    lista_botones <- lapply(1:num_preguntas, function(i) {
      actionButton(inputId = paste0("btn_pregunta_", i),
                   label = as.character(i),
                   onclick = sprintf("Shiny.setInputValue('selected_pregunta', %d);", i),
                   style = "border: 1px; font-size: 10px; width: 20px; height: 20px; line-height: 20px;
                   padding: 0; text-align: center; vertical-align: middle;")
    })
  })
  
  observeEvent(input$selected_pregunta, {
    pregunta_actual(input$selected_pregunta)
  })
  
  output$pregunta <- renderUI({
    tagList(
      withMathJax(),
      HTML(paste0("<div style='font-size: 18px;'>", preguntas$text[pregunta_actual()], "</div>")),
      br(),
      #h3(preguntas$id[pregunta_actual()])
    )
  })
  
  output$respuestas <- renderUI({
    pregunta_actual <- pregunta_actual()  # Almacenamos la pregunta actual
    withMathJax()
    checkboxGroupInput(inputId = "respuestas_seleccionadas",
                       label = NULL,
                       choices = respuestas[[pregunta_actual]]$text,
                       selected = respuestas_seleccionadas[[paste0("respuestas_", pregunta_actual)]])
  })
  
  output$btn_anterior_ui <- renderUI({
    if (pregunta_actual() > 1) {
      actionButton("btn_anterior", "Pregunta anterior", icon = icon("chevron-left"))
    }
  })
  
  output$btn_siguiente_ui <- renderUI({
    if (pregunta_actual() < num_preguntas) {
      actionButton("btn_siguiente", "Pregunta siguiente", icon = icon("chevron-right"))
    }
  })
  
  observeEvent(input$btn_anterior, {
    if (pregunta_actual() > 1) {
      pregunta_actual(pregunta_actual() - 1)
    }
  })
  
  observe({
    if (pregunta_actual() == 1) {
      shinyjs::hide("btn_anterior")
    } else {
      shinyjs::show("btn_anterior")
    }
    
    if (pregunta_actual() == num_preguntas) {
      shinyjs::hide("btn_siguiente")
    } else {
      shinyjs::show("btn_siguiente")
    }
  })
  
  observe({
    for (i in 1:num_preguntas) {
      runjs(sprintf('document.getElementById("btn_pregunta_%d").style.border = "none";', i))
    }
    runjs(sprintf('document.getElementById("btn_pregunta_%d").style.border = "1px solid black";', pregunta_actual()))
  })
  
  observeEvent(input$respuestas_seleccionadas, {
    selected_items <- input$respuestas_seleccionadas
    respuestas_seleccionadas[[paste0("respuestas_", pregunta_actual())]] <- selected_items
    ## funcion actualizar boton
    if(length(selected_items > 0)){
      runjs(sprintf('document.getElementById("btn_pregunta_%d").style.backgroundColor = "#A9CDF0";',
                    pregunta_actual()))
    }
    session$onFlushed(function() {
      session$sendCustomMessage(type = 'reprocessMath', message = 'reprocess')
    }, once = TRUE)
  })
  
  observeEvent(input$btn_siguiente, {
    if (pregunta_actual() < num_preguntas) {
      pregunta_actual(pregunta_actual() + 1)
    }
  })
  
  observeEvent(input$btn_finalizar, {
    showModal(modalDialog(
      title = "Cuestionario finalizado",
      paste("Tu puntuación es:", 0),
      footer = NULL
    ))
    dbDisconnect(bd)
  })
}

shinyApp(ui, server)
