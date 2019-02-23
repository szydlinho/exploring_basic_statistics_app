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
                                       chooseSliderSkin("Modern"),
                                       setSliderColor("olivedrab",1),
                                       
                                       sliderInput("sezonFromTo", label = h4("Zakres sezonów:"), min = 2010, 
                                                   max = 2019, value = c(2010, 2019), sep = ""),
                                       uiOutput("teams_selection_pie"),
                                       uiOutput("home_away"),
                                       textOutput("selected_teams_pie")
                                    
                       
                       ),
                       column(6, 
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_pie", style="width:100%;height:400px;"),
                              deliverChart(div_id = "interactive_pie")
                              
                              #plotOutput("plot_kolowy"
                                         ))
                     ),
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
                                                   max = 2019, value = c(2010, 2019), sep = ""),
                                       uiOutput("secondSelection"),
                                       textOutput("selected_teams")
                                       
                       ),
                       column(9, 

                              #plotOutput("histogram_gole")
                              loadEChartsLibrary(),
                              
                              tags$div(id="interactive_hist", style="width:80%;height:400px;"),
                              deliverChart(div_id = "interactive_hist")
                       )
                       
                    )
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
                                                   max = 2019, value = c(2010, 2019), sep = ""),
                                       uiOutput("teams_selection_hist_final"),
                                       textOutput("selected_teams_hist_final"),
                                       uiOutput("teamSelectionH2"),
                                       textOutput("selected_teams_h2")
                                 
                                       
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
                                                   max = 2019, value = c(2010, 2019), sep = "")
                                       
                       ),
                       column(7, 
                              plotOutput("plot_contingency")
      
                       )),
                       fluidRow(column(5, offset = 4, br(),  
                                       DT::dataTableOutput("freqency_table")
                       
              )
              
                       )),
              ##############################################
              #Panel 5
              tabPanel("Predykcja", 
                       br(),
                       fluidRow(column(3, 
                                       radioButtons(inputId = "league_typed_prediction", label = "Wybierz Ligę", 
                                                    choices = c("Premier League", "La Liga", "Seria A", "Bundesliga"), 
                                                    selected = "Seria A"),
                                       radioButtons(inputId = "model_typed_prediction", label = "Wybierz model", 
                                                    choices = c("Logistic Regression", "Decision Tree", "RandomForest"), 
                                                    selected = "Logistic Regression"),
                                       uiOutput("mwselection"),
                                       actionButton("prediction_butt", "Pokaż predykcję", width = '100%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black")
                                       
                       ),
                       column(7, 
                              DT::dataTableOutput("prediction_table")
                              
                       ))
                              
                       
                       
                       )
                       
  )
              
})





#shinyApp(ui, server)
