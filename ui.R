
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

url <- "https://analizadanychwpilce.wordpress.com/"

shinyUI(fluidPage(
  fluidRow(
    column(2,
           div(style = "text-align:left",
               tagList(
                 br(),
                 tags$img(src = "Logo.png", alt = "ADWPN logo", width = "100%"),
                 br()
               )
           )
    ),
    column(6,
           h1(style = "text-align:left", titlePanel("Analiza danych w piłce nożnej")),
           tags$a(href=url, "Przejdź do strony")
           )
  
    
    
),br(), br(),
  

  htmlOutput("page")
  
  
))
