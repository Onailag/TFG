library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(shinybusy)
options(encoding = 'UTF-8')



mytab <- function(tabName, id){
  ns <- NS(id)
  tabPanel(
    tabName,
    sidebarLayout(
      sidebarPanel(
        h4("Calificaciones del grupo"),
        plotOutput( ns(paste0("grafico_resultados", tabName, sep=""))),
        selectInput( ns(paste0("tema_seleccionado", tabName, sep="")), "Selecciona un tema:",
                    choices = get_attb(tabName))
      ),
      mainPanel(
        h4(paste0("Panel de administración de la asignatura ", tabName)), ## resolver nombre asig by id
        verbatimTextOutput(ns(paste0("subject_name_panel_", tabName))),
        verbatimTextOutput(ns(paste0("subject_description_panel_", tabName))),
        verbatimTextOutput(ns(paste0("subject_course_panel_", tabName))),
        verbatimTextOutput(ns(paste0("subject_nquestions_panel_", tabName))),
        actionButton(
          inputId = ns(paste0("toggle_cargar_preguntas", tabName, sep="")),
          label = "Cargar preguntas",
          icon = icon("add"),
          width = "100%",
          class = "btn-primary", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(), br(),
        actionButton(
          inputId = ns(paste0("toggle_tabla_preguntas", tabName, sep="")),
          label = "Consultar preguntas y respuestas",
          icon = icon("load"),
          width = "100%",
          class = "btn-primary", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(),
        uiOutput(ns(paste0("tabla_preguntas_container", tabName, sep=""))),
        br(),
        actionButton(
          inputId = ns(paste0("toggle_configurar_cuestionario", tabName, sep="")),
          label = "Configuración de cuestionarios",
          icon = icon("settings"),
          width = "100%",
          class = "btn-primary", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(), br(),
        actionButton(
          inputId = ns(paste0("toggle_cargar_reticulo", tabName, sep="")),
          label = "Cargar retículo",
          icon = icon("add"),
          width = "100%",
          class = "btn-primary", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(), br(),
        actionButton(
          inputId = ns(paste0("toggle_tabla", tabName, sep="")),
          label = "Ver alumnos de la asignatura",
          icon = icon("load"),
          width = "100%",
          class = "btn-primary", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        br(),

        uiOutput(ns(paste0("tabla_container", tabName, sep=""))),
        br()
      )
    )
  )
}

profesorUI <- function(id, user) {
  useShinyjs()
  tab_list <- get_subjects(user$user_id) ##
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Cuestionarios vista Profesor", 
      theme = bslib::theme_bootswatch("cerulean"),
      id=ns("profTabSet"),
      tabPanel("Home",
               icon = icon("house"),
               mainPanel(
                 verbatimTextOutput(ns("texto_bienvenida")),
                 verbatimTextOutput(ns("user_id")),
               )
               ),
      !!!lapply(tab_list, function(x){
          mytab(x, id)
      })
    )
  )
}


profesorServer <- function(id, user) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      currentTab <- reactive(input$profTabSet)
      subjectData <- reactiveVal(get_subject_data(input$profTabSet))
      tabla_visible <- reactiveVal(FALSE)
      tabla_preguntas_visible <- reactiveVal(FALSE)
      
      observeEvent(input$profTabSet, {
        tabla_visible(FALSE)
        tabla_preguntas_visible(FALSE)
      })
      
      
      output$texto_bienvenida <- renderText({
        paste("Bienvenido a la plataforma ", user$name)
      })
      
      output$user_id <- renderText({
        paste("Tu ID de usuario es:", user$user_id)
      })
      
      output[[paste0("subject_name_panel_", isolate(currentTab()))]] <- renderText({
        paste("Nombre: ", 
        iconv(isolate(subjectData())$name, from = "UTF-8", to = "ISO-8859-1"))
        
      })
      
      output[[paste0("subject_course_panel_", isolate(currentTab()))]] <- renderText({
        paste("Curso: ", 
              iconv(isolate(subjectData())$course, from = "UTF-8", to = "ISO-8859-1"))
      })
      
      output[[paste0("subject_description_panel_", isolate(currentTab()))]] <- renderText({
        paste("Descripción: ", 
              iconv(isolate(subjectData())$description, from = "UTF-8", to = "ISO-8859-1"))
      })
      
      output[[paste0("subject_nquestions_panel_", isolate(currentTab()))]] <- renderText({
        paste("Número de preguntas: ", 
              iconv(subjectData()$questions_per_test, from = "UTF-8", to = "ISO-8859-1"))
      })
      
      
      observeEvent(input[[paste0("tema_seleccionado", isolate(currentTab()), sep="")]], {
        output[[paste0("grafico_resultados", currentTab(), sep="")]]  <- renderPlot({
          bars <- get_barplot_subject(isolate(currentTab()), input[[paste0("tema_seleccionado", isolate(currentTab()), sep="")]])
          barplot(bars, 
                  names.arg = c("Suspenso", "Aprobado", "Sobresaliente"),
                  ylim = c(0, sum(bars)),
                  col = "blue", main = paste("Resultados de", input[[paste0("tema_seleccionado", isolate(currentTab()), sep="")]]))
        })
      })
    
      
      output[[paste0("tabla_container", currentTab(), sep="")]] <- renderUI({
        #dataTableOutput(ns(paste0("tabla_resultados", currentTab(), sep="")))
      })
      
      output[[paste0("tabla_preguntas_container", currentTab(), sep="")]] <- renderUI({
        #dataTableOutput(ns(paste0("tabla_preguntas", currentTab(), sep="")))
      })
      
      ## Tabla alumnos matriculados
      output[[paste0("tabla_resultados", isolate(currentTab()), sep="")]] <- renderDataTable({
        #usuarios
        getEnroledStudentsTable(currentTab())
      }, selection = 'single', rownames = FALSE)
      
      output[[paste0("tabla_preguntas", isolate(currentTab()), sep="")]] <- renderDataTable({
        #usuarios
        get_subject_questions_table(currentTab())
      }, selection = 'single', rownames = FALSE)
      

      
      # Renderizar u ocultar la tabla al hacer clic en el botÃ³n
      observeEvent(input[[paste0("toggle_tabla", isolate(currentTab()), sep="")]], {
        if (tabla_visible()) {
          # Ocultar tabla
          output[[paste0("tabla_container", currentTab(), sep="")]] <- renderUI({})
          tabla_visible(FALSE)
        } else {
          # Mostrar tabla
          output[[paste0("tabla_container", currentTab(), sep="")]] <- renderUI({
            h4(paste0("Alumnos matriculados en ", currentTab()))
            dataTableOutput(ns(paste0("tabla_resultados", currentTab(), sep="")))
          })
          output[[paste0("tabla_resultados", currentTab(), sep="")]] <- renderDataTable({
            getEnroledStudentsTable(currentTab())
          }, selection = 'single', rownames = FALSE)
          tabla_visible(TRUE)
        }
      },ignoreInit = TRUE)
      
      # Renderizar u ocultar la tabla al hacer clic en el botÃ³n
      observeEvent(input[[paste0("toggle_tabla_preguntas", isolate(currentTab()), sep="")]], {
        if (tabla_preguntas_visible()) {
          # Ocultar tabla
          output[[paste0("tabla_preguntas_container", isolate(currentTab()), sep="")]] <- renderUI({})
          tabla_preguntas_visible(FALSE)
        } else {
          # Mostrar tabla
          output[[paste0("tabla_preguntas_container", isolate(currentTab()), sep="")]] <- renderUI({
            dataTableOutput(ns(paste0("tabla_preguntas", currentTab(), sep="")))
          })
          output[[paste0("tabla_preguntas_resultados", isolate(currentTab()), sep="")]] <- renderDataTable({
            get_subject_questions_table(currentTab())
          }, selection = 'single', rownames = FALSE)
          tabla_preguntas_visible(TRUE)
        }
      },ignoreInit = TRUE)
      
      observeEvent(input[[paste0("toggle_cargar_preguntas", isolate(currentTab()), sep="")]], {
        #req(input[[paste0("toggle_cargar_preguntas", isolate(currentTab()), sep="")]])
        showModal(modalDialog(
          title = paste("Preguntas de la asignatura", isolate(currentTab())),
          selectInput(
            inputId = ns(paste0("opciones_", isolate(currentTab()))),
            label = "Selecciona una opción:",
            choices = c("", get_attb(isolate(currentTab()))),
            selected = ""  
          ),
          fileInput(ns(paste0("cargar_preguntas", isolate(currentTab()))), label = "Subir preguntas"),
          div(
            actionButton(inputId = ns(paste0("load_questions", isolate(currentTab()))), label = "Cargar preguntas",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            
            
            align = "center"),
          shinyjs::hidden(
            tags$div(
              id = ns("errorNoContent"),
              tags$p(
                "Seleciona el tema al que agregar las preguntas.",
                style = "color: red; font-weight: bold; padding-top: 5px;",
                class = "text-center"
              )
            )
          ),
          easyClose = TRUE,
          size = "l",
        ))
      }, ignoreInit = TRUE)
      
      observe({
        req(input[[paste0("load_questions", isolate(currentTab()))]])
        req(input[[paste0("cargar_preguntas", isolate(currentTab()))]])
        content <- input[[paste0("opciones_", isolate(currentTab()))]]
        content <- iconv(content, from = "UTF-8", to = "ISO-8859-1")
        if(content != ""){
          show_modal_spinner(spin = "circle", color = "#337ab7")
          add_questions(input[[paste0("cargar_preguntas", isolate(currentTab()))]]$datapath, isolate(currentTab()), content)
          remove_modal_spinner()
          removeModal()
          showNotification("Acción completada")
        }else{
            shinyjs::toggle(id = "errorNoContent", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
        }

      })
      


      observeEvent(input[[paste0("toggle_cargar_reticulo", isolate(currentTab()), sep="")]], {

          showModal(modalDialog(
            title = paste("Cargar retículo de la asignatura ", isolate(currentTab())),
            fileInput(ns(paste0("reticulo", isolate(currentTab()))), label = "Subir retículo"),
            div(
              actionButton(ns(paste0("cargar_reticulo", isolate(currentTab()))), label = "Cargar retículo",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              
              align = "center"),
            easyClose = TRUE,
            size = "l"
          ))
        
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("cargar_reticulo", isolate(currentTab()))]], { 
        req(input[[paste0("reticulo", isolate(currentTab()))]])
        file_content <- readr::read_file(input[[paste0("reticulo", isolate(currentTab()))]]$datapath)
        show_modal_spinner(spin = "circle", color = "#337ab7")
        # Convierte el texto JSON a un dataframe
        json <- fromJSON(paste(file_content, collapse = ""))
        parseLattice(json, isolate(currentTab()))
        remove_modal_spinner()  
        removeModal()
        showNotification("Acción completada")
      }, ignoreInit = TRUE)
      
      
      observeEvent(input[[paste0("toggle_configurar_cuestionario", isolate(currentTab()), sep="")]], {
        showModal(modalDialog(
          title = paste("Configuración del cuestionario de ", isolate(currentTab())),
          h3("Selecciona el número de preguntas para los cuestionarios."),
          selectInput(ns(paste0("num_preguntas_cuestionario", isolate(currentTab()))), "Número de preguntas", 
                      choices = c(10:40)),
          div(
            actionButton(ns(paste0("update_nquestions", isolate(currentTab()))), label = "Aplicar cambios",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            align = "center"),
          size = "l"
          
        ))
        n_questions <- input[[paste0("num_preguntas_cuestionario", isolate(currentTab()))]]
        
      },
      ignoreInit = TRUE)

      observe({
        req(input[[paste0("update_nquestions", isolate(currentTab()))]])
        n_questions <- input[[paste0("num_preguntas_cuestionario", isolate(currentTab()))]]
        if(!is.null(n_questions)){
          update_test_question_number(isolate(currentTab()), n_questions)
        }
      })

      observeEvent(input[[paste0(paste0("tabla_resultados", currentTab(), sep=""),"_rows_selected")]], {
        selected_row <- input[[paste0(paste0("tabla_resultados", currentTab(), sep=""),"_rows_selected")]]
        if (length(selected_row)>0) {
          selected_user <- getEnroledStudentsTable(isolate(currentTab()))[selected_row, ]
          selected_user_id <- as.character(selected_user$user_id)
          output[[paste0(paste0("tabla_intentos",currentTab()), selected_user_id)]] <- renderDataTable({
            table <- get_subject_attempts_table(selected_user_id,currentTab())
            table$content <- iconv(table$content, from ="UTF-8",to="ISO-8859-1")
            table
          }, selection = 'none', rownames = FALSE)
          showModal(modalDialog(
            title = paste("Datos del usuario", selected_user$nombre),
            p(paste("ID:", selected_user$user_id)),
            p(paste("Temas Superados:", paste(unlist(get_passed_contents(selected_user_id, isolate(currentTab()))), collapse=", "))),
            p(paste("Nota media:", get_average_score(selected_user_id, isolate(currentTab())))),
            dataTableOutput(ns(paste0(paste0("tabla_intentos",isolate(currentTab())), selected_user_id))),
            easyClose = TRUE,
            footer = NULL, 
            size = "l"
          ))
        }
      }, ignoreInit = TRUE)
      
      #Pregutnas
      observeEvent(input[[paste0(paste0("tabla_preguntas", isolate(currentTab()), sep=""),"_rows_selected")]], {
        selected_row <- input[[paste0(paste0("tabla_preguntas", isolate(currentTab()), sep=""),"_rows_selected")]]
        if (length(selected_row)>0) {
          selected_question <- get_subject_questions_table(isolate(currentTab()))[selected_row, ]
          selected_question_id <- as.character(selected_question$id)
          selected_question_content <- selected_question$content
          
          output[[paste0(paste0("tabla_respuestas",isolate(currentTab())), selected_question_id)]] <- renderDataTable({
            get_question_answers_table(isolate(currentTab()),selected_question_id)
          }, selection = 'none', rownames = FALSE)
          
          showModal(modalDialog(
            title = paste(paste("Pregunta: ", selected_question_id), "       ", paste("Unidad de contenido: ", selected_question$content)),
            tagList(
              withMathJax(),
              HTML(paste0("<div style='font-size: 20px;'>", selected_question$text, "</div>")),
              tags$script(HTML("
                  Shiny.addCustomMessageHandler('reprocessMath', function(message) {
                    MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
                  });
                ")),
              br(),
              #h3(preguntas$id[pregunta_actual()])
            ),

            tagList(
              withMathJax(),
              dataTableOutput(ns(paste0(paste0("tabla_respuestas",isolate(currentTab())), selected_question_id))),
            ),
            

            session$sendCustomMessage("reprocessMath", list()),
            div(
              actionButton(ns(paste0(paste0("delete_question", isolate(currentTab())),selected_question_id)), label = "Eliminar pregunta",
                           style="color: #fff; background-color: #d9534f; border-color: #d43f3a;"),
              
              align = "center"),
            easyClose = TRUE,
            footer = NULL, 
            size = "l"
          ))
          
          observeEvent(input[[paste0(paste0("delete_question", isolate(currentTab())),selected_question_id)]], {
            delete_questions_and_answers(selected_question_id, isolate(currentTab()), selected_question_content)
            removeModal()
            output[[paste0("tabla_preguntas_resultados", isolate(currentTab()), sep="")]] <- renderDataTable({
              isolate(get_subject_questions_table(isolate(currentTab())))
            }, selection = 'single', rownames = FALSE)
          })
          
        }
      }, ignoreInit = TRUE)

      
    }
  )
}