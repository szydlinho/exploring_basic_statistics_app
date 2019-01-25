library(shiny)
library(ECharts2Shiny)
library(shinyWidgets)

teams_df <- data.frame(teams = character(), 
                       league = character())
for (files in list.files(path = ".", "*.csv")){
  
  df <- read.csv(files)[,c("HomeTeam","AwayTeam")]
  
  teams <- unique(c(as.character(df$HomeTeam), as.character(df$AwayTeam)))
  
  if (grepl("bundesliga",files )){
    
    league <- rep("Bundesliga", length(teams))
    
  } else if(grepl("laliga",files )){
    
    league <- rep("La Liga", length(teams))
     
  } else if(grepl("premier",files )){
    
    league <- rep("Premier League", length(teams))
    
  } else if(grepl("seriaa",files )){
    
    league <- rep("Seria A", length(teams))
  }
  
  new <- data.frame(teams = teams, 
                    league = league)
  
  teams_df <- rbind(teams_df, new)
}

shinyServer(function(input, output) {
  
  source("page_ui.R", local = TRUE, encoding = "UTF-8")
  
  observeEvent(input$plot_kolowy_butt, {
    
   # output$plot_kolowy <- renderPlot({
   #   Kolowy(input$league_typed, input$sezonFromTo[1], input$sezonFromTo[2])
   #   })
    
    renderPieChart(div_id = "interactive_pie",  theme = 'jazz', 
                   data = Kolowy_dane(input$league_typed, input$sezonFromTo[1], input$sezonFromTo[2]))
    
  })
  
  observeEvent(input$plot_kolowy_butt2, {
    
    renderPieChart(div_id = "interactive_pie2",  theme = 'jazz', 
                   data = Kolowy_dane(input$league_typed2, input$sezonFromTo2[1], input$sezonFromTo2[2]))
    
  })

  output$secondSelection <- renderUI({
    selectInput("teams_h1", "Wybierz drużynę:", 
                choices = as.character(teams_df$teams[teams_df$league==input$league_typed_gole]), multiple = TRUE)
  })
  
  observeEvent(input$plot_hist_butt, {

    dane <- Histogram_dane(input$league_typed_gole, input$sezonFromTo_gole[1], 
                   input$sezonFromTo_gole[2],
                   team=input$teams_h1)
    if (nrow(dane)>0){
      # Call functions from ECharts2Shiny to render charts
      renderBarChart(div_id = "interactive_hist", grid_left = '10%', direction = "vertical", grid_right =  '15%',
                     axis.x.name = "Ilość goli", axis.y.name = "Liczebność wystąpień",
                     data = dane)
    }

    
  })
  
  observeEvent(input$plot_hist_butt2, {
    
    #output$histogram_gole <- renderPlot({
    # Histogram(input$league_typed_gole, input$sezonFromTo_gole[1], input$sezonFromTo_gole[2])
    # })
    
    # Call functions from ECharts2Shiny to render charts
    renderBarChart(div_id = "interactive_hist2", grid_left = '10%', direction = "vertical", grid_right =  '15%',
                   axis.x.name = "Ilość goli", axis.y.name = "Liczebność wystąpień",
                   data = Histogram_dane(input$league_typed_gole2, input$sezonFromTo_gole2[1], input$sezonFromTo_gole2[2]))
    
  })
  
  
  observeEvent(input$plot_hist_rozklad_butt, {
    
    #output$histogram_rozklad <- renderPlot({
    #  Histogramy_gole(input$league_typed_rozklad, input$sezonFromTo_rozklad[1], input$sezonFromTo_rozklad[2])
    #})
    
    # Call functions from ECharts2Shiny to render charts
    renderBarChart(div_id = "interactive_hist_by_final_a", grid_left = '10%', direction = "vertical", grid_right =  '27%',
                   axis.x.name = "Ilość goli gospodarzy", axis.y.name = "Liczebność wystąpień",
                   data = Histogram_dane_hda_FTHG(input$league_typed_rozklad, input$sezonFromTo_rozklad[1], input$sezonFromTo_rozklad[2]))
    
    
    # Call functions from ECharts2Shiny to render charts
    renderBarChart(div_id = "interactive_hist_by_final_b", grid_left = '10%', direction = "vertical", grid_right =  '27%',
                   axis.x.name = "Ilość goli gości", axis.y.name = "Liczebność wystąpień",
                   data = Histogram_dane_hda_FTAG(input$league_typed_rozklad, input$sezonFromTo_rozklad[1], input$sezonFromTo_rozklad[2]))
    
    
  })
  
  observeEvent(input$plot_contingency_butt, {
    
    output$plot_contingency <- renderPlot({
      Najczestszy_wynik(input$league_typed_contingency, input$sezonFromTo_contingency[1], input$sezonFromTo_contingency[2])
    })
    

    output$freqency_table <- DT::renderDataTable({ DT::datatable(Najczestszy_wynik_tabela(input$league_typed_contingency, input$sezonFromTo_contingency[1], input$sezonFromTo_contingency[2])
                                                                                    , options = list(
                                                                                      autoWidth = TRUE, searching = FALSE, columnDefs = list(list(width = '80px'))
                                                                                      ,scrollX = FALSE,  dom = "t", lengthChange = FALSE, 
                                                                                      ordering = FALSE), rownames = FALSE) })

  })
  
})


