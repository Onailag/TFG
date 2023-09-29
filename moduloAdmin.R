library(shiny)
library(bslib)

adminUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Admin Mode", 
      theme = bslib::theme_bootswatch("celurian")
    )
  )
}

adminServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}