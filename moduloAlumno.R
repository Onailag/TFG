library(shiny)
library(bslib)

alumnoUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Cuestionarios vista Alumno", 
      theme = bslib::theme_bootswatch("celurian"),
      tabPanel("Home",
               icon = icon("house"))
    )
  )
}

alumnoServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}