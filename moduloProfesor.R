library(shiny)
library(bslib)

profesorUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Cuestionarios vista Profesor", 
      theme = bslib::theme_bootswatch("celurian"),
      tabPanel("Home",
               icon = icon("house"))
    )
  )
}

profesorServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}