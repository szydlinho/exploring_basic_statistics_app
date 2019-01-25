library(shiny)

shinyUI(fluidPage(
  
  htmlOutput("page")
  
  
))


output$page <- renderUI({
  
  tabsetPanel(id = "inTabset",
              
              ##############################################
              #Panel 1
              tabPanel("% końcowych wyników meczów", 
                       br(),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed", label = "Wybierz Lige", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_kolowy_butt", "Pokaż wykres kołowy", width = '80%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",1),
                                       
                                       sliderInput("sezonFromTo", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = "")
                       
                       ),
                       column(6, 
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_pie", style="width:100%;height:400px;"),
                              deliverChart(div_id = "interactive_pie")
                              
                              #plotOutput("plot_kolowy"
                                         )),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed2", label = "Wybierz Lige", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_kolowy_butt2", "Pokaż wykres kołowy", width = '80%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",sliderId = 2),
                                       
                                       sliderInput("sezonFromTo2", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = "")
                                       
                       ),
                       column(6, 
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_pie2", style="width:100%;height:400px;"),
                              deliverChart(div_id = "interactive_pie2")
                              
                              #plotOutput("plot_kolowy"
                       ))),
              ##############################################
              #Panel 2
              tabPanel("Liczebność goli w meczu", 
                       br(),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed_gole", label = "Wybierz Ligę", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_hist_butt", "Pokaż histogram", width = '80%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",sliderId = 3),
                                       
                                       sliderInput("sezonFromTo_gole", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = ""),
                                       uiOutput("secondSelection")
                                       
                       ),
                       column(9, 

                              #plotOutput("histogram_gole")
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_hist", style="width:80%;height:400px;"),
                              deliverChart(div_id = "interactive_hist")
                       ),
                       
                       fluidRow(column(3,
                                       radioButtons(inputId = "league_typed_gole2", label = "Wybierz Ligę", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_hist_butt2", "Pokaż histogram", width = '80%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",sliderId = 4),
                                       
                                       sliderInput("sezonFromTo_gole2", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = "")
                                       
                       ),
                       column(9, 
                              
                              #plotOutput("histogram_gole")
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_hist2", style="width:80%;height:400px;"),
                              deliverChart(div_id = "interactive_hist2")
                       )
                              ))
                     ),
              ##############################################
              #Panel 3
              tabPanel("Rozkład goli wg. wyniku", 
                       br(),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed_rozklad", label = "Wybierz Ligę", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_hist_rozklad_butt", "Pokaż histogramy", width = '90%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",sliderId = 5),
                                       
                                       sliderInput("sezonFromTo_rozklad", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = "")
                                       
                       ),
                       column(9, 
                              
                              
                              #plotOutput("histogram_rozklad")
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_hist_by_final_a", style="width:80%;height:400px;"),
                              deliverChart(div_id = "interactive_hist_by_final_a")
                              )),
                       fluidRow(
                              column(9,offset = 3, 
                              loadEChartsLibrary(),
                              tags$div(id="interactive_hist_by_final_b", style="width:80%;height:400px;"),
                              deliverChart(div_id = "interactive_hist_by_final_b")
                       ))
              ),
              ##############################################
              #Panel 4
              tabPanel("Najczęstszy wynik meczu", 
                       br(),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed_contingency", label = "Wybierz Ligę", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Premier League"),
                                       actionButton("plot_contingency_butt", "Pokaż wykres oraz tabelę", width = '100%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black"),
                                       chooseSliderSkin("Simple"),
                                       setSliderColor("olivedrab",sliderId = 5),
                                       
                                       sliderInput("sezonFromTo_contingency", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2018, value = c(2010, 2018), sep = "")
                                       
                       ),
                       column(7, 
                              plotOutput("plot_contingency")
      
                       )),
                       fluidRow(column(5, offset = 4, br(),  
                                       DT::dataTableOutput("freqency_table")
                       
              )
              
                       ))
                       
  )
              
})





#shinyApp(ui, server)
