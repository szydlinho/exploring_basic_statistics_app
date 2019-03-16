suppressMessages(library(shiny))
suppressMessages(library(ECharts2Shiny))
suppressMessages(library(shinyWidgets))

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

# 
# token <- readRDS("droptoken.rds")
# #datasets <- list()
# list_files <- c()
# for (i in 1:4) {
#   list_files[i] <- drop_search("raw_final.csv")$matches[[i]]$metadata$name
# }
# 
# list_files <- paste0("data_football/", list_files)
# 
# for (files in list_files){
# 
#   df <- drop_read_csv(files, dtoken = token)[,c("HomeTeam","AwayTeam")]
# 
#   teams <- unique(c(as.character(df$HomeTeam), as.character(df$AwayTeam)))
# 
#   if (grepl("bundesliga",files )){
# 
#     league <- rep("Bundesliga", length(teams))
# 
#   } else if(grepl("laliga",files )){
# 
#     league <- rep("La Liga", length(teams))
# 
#   } else if(grepl("premier",files )){
# 
#     league <- rep("Premier League", length(teams))
# 
#   } else if(grepl("seriaa",files )){
# 
#     league <- rep("Seria A", length(teams))
#   }
# 
#   new <- data.frame(teams = teams,
#                     league = league)
# 
#   teams_df <- rbind(teams_df, new)
# }
# 
# df_glm <- read.xlsx("predictions_to_analysis.xlsx", sheetIndex = 1, header = F)[,c(1:5,6 )]
# df_glm %>% mutate(model = "LogisticRegression") -> df_glm
# df_dt <- read.xlsx("predictions_to_analysis.xlsx", sheetIndex = 2, header = F)[,c(1:5 )]
# df_dt %>% mutate(X6 = ifelse(X5 == "0", " Remis_lub_gosc", "Gospodarz"),
#                  model = "DecisionTree") -> df_dt
# df_rf <- read.xlsx("predictions_to_analysis.xlsx", sheetIndex = 3, header = F)[,c(1:5 )]
# df_rf %>% mutate(X6 = ifelse(X5 == "0", " Remis_lub_gosc", "Gospodarz"),
#                  model = "RandomForest") -> df_rf
# df_razem <- rbind(df_glm, df_dt, df_rf)
# names(df_razem) <- c("Kolejka", "liga", "Gospodarz", "Gosc","pred_value", "pred_th", "model")

df_razem <- read.csv("pred/predictions.csv", header = T)
  #drop_read_csv("data_football/predictions.csv", dtoken = token)
  #
# mw_pl <- max(df_razem[df_razem$liga == "premier", "Kolejka"], na.rm =T)
# mw_ll <- max(df_razem[df_razem$liga == "laliga", "Kolejka"], na.rm =T)
# mw_sa <- max(df_razem[df_razem$liga == "seriaa", "Kolejka"], na.rm =T)
# mw_bu <- max(df_razem[df_razem$liga == "bundesliga", "Kolejka"], na.rm =T)
# 
# mw_pl_u <- unique(df_razem[df_razem$liga == "premier", "Kolejka"], na.rm =T)
# mw_ll_u <- unique(df_razem[df_razem$liga == "laliga", "Kolejka"], na.rm =T)
# mw_sa_u <- unique(df_razem[df_razem$liga == "seriaa", "Kolejka"], na.rm =T)
# mw_bu_u <- unique(df_razem[df_razem$liga == "bundesliga", "Kolejka"], na.rm =T)

shinyServer(function(input, output) {
  
  source("page_ui.R", local = TRUE, encoding = "UTF-8")
  
  output$teams_selection_pie <- renderUI({
    selectInput("teams_pie", "Wybierz drużynę:", 
                choices = as.character(teams_df$teams[teams_df$league==input$league_typed]), multiple = TRUE)
  })
  

  
  output$teams_selection_pie <- renderUI({
    selectInput("teams_pie", "Wybierz drużynę:", 
                choices = as.character(teams_df$teams[teams_df$league==input$league_typed]), multiple = TRUE)
  })

  output$home_away <- renderUI({
    radioButtons(inputId = "home_away_selection", label = "Mecze wybranej drużyny jako:", 
                 choices = c("Gospodarz", "Gość"),
                 selected = "Gospodarz")
  })
  
  observeEvent(input$plot_kolowy_butt, {
  
    
    
    dane_pie <- Kolowy_dane(input$league_typed, input$sezonFromTo[1], input$sezonFromTo[2], team = input$teams_pie,
                            homeaway = input$home_away_selection )
    
    if (!is.null(dane_pie)){
      
    renderPieChart(div_id = "interactive_pie",  theme = 'jazz', data = dane_pie) 
      
      if (is.null(input$teams_pie)){
        output$selected_teams_pie <- renderText({ 
          paste("Wybrane wszystkie mecze i drużyny z ", input$league_typed)
        })
      } else {
        output$selected_teams_pie <- renderText({ 
          paste("Wybrane drużyny:", paste0(input$teams_pie, collapse = ", "), "jako ", tolower(input$home_away_selection))
        })
      }

      
    } else {
      output$selected_teams_pie <- renderText({ 
        paste("Brak danych dla ", paste0(input$teams_pie, collapse = ", "), " w wybranych sezonach.")
      })
    
    }
    
    url <- a("Przewaga własnego boiska", href="https://analizadanychwpilce.wordpress.com/2018/08/14/przewaga-wlasnego-boiska")
    output$url_home_adv <- renderUI({
      tagList("Po więcej informacji:", url)
    })
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
    
      renderBarChart(div_id = "interactive_hist", grid_left = '10%', direction = "vertical", grid_right =  '15%',
                     axis.x.name = "Liczba goli", axis.y.name = "Liczebność wystąpień",
                     data = dane)
      
      if (is.null(input$teams_h1)){
        output$selected_teams <- renderText({ 
          paste("Wybrane wszystkie drużyny z ", input$league_typed_gole)
        })
      } else {
        output$selected_teams <- renderText({ 
          paste("Wybrane drużyny:", paste0(input$teams_h1, collapse = ", "))
        })
      }
    } else {
      output$selected_teams <- renderText({ 
        paste("Brak danych dla ", paste0(input$teams_h1, collapse = ", "), " w wybranych sezonach.")
      })
      
     
    }
    
    url <- a("Artykuł dotyczący rozkładu goli", href="https://analizadanychwpilce.wordpress.com/2018/08/16/gole-zdarzenia-rzadkie")
    output$url_goals <- renderUI({
      tagList("Po więcej informacji:", url)
    })
    
  })
  
  output$teamSelectionH2 <- renderUI({
    selectInput("teams_h2", "Wybierz drużynę:", 
                choices = as.character(teams_df$teams[teams_df$league==input$league_typed_rozklad]), multiple = TRUE)
  })

  observeEvent(input$plot_hist_rozklad_butt, {
    
    dane_h2 <- Histogram_dane_hda_FTHG(input$league_typed_rozklad, input$sezonFromTo_rozklad[1], input$sezonFromTo_rozklad[2], 
                                       team=input$teams_h2)
    
    dane_h21 <- Histogram_dane_hda_FTAG(input$league_typed_rozklad, input$sezonFromTo_rozklad[1], input$sezonFromTo_rozklad[2], 
                                       team=input$teams_h2)
    if (nrow(dane_h2)>0){
      
      
      # Call functions from ECharts2Shiny to render charts
      renderBarChart(div_id = "interactive_hist_by_final_a", grid_left = '10%', direction = "vertical", grid_right =  '27%',
                     axis.x.name = "Liczba goli jako gospodarz", axis.y.name = "Liczebność wystąpień",
                     data = dane_h2)
      
      
      # Call functions from ECharts2Shiny to render charts
      renderBarChart(div_id = "interactive_hist_by_final_b", grid_left = '10%', direction = "vertical", grid_right =  '27%',
                     axis.x.name = "Liczba goli jako gość", axis.y.name = "Liczebność wystąpień",
                     data = dane_h21)
      
      if (is.null(input$teams_h2)){
        output$selected_teams_h2 <- renderText({ 
          paste("Wybrane wszystkie drużyny z ", input$league_typed_rozklad)
        })
      } else {
        output$selected_teams_h2 <- renderText({ 
          paste("Wybrane drużyny:", paste0(input$teams_h2, collapse = ", "))
        })
      }
    } else {
      output$selected_teams_h2 <- renderText({ 
        paste("Brak danych dla ", paste0(input$teams_h2, collapse = ", "), " w wybranych sezonach.")
      })
      
      
    }
    
    url <- a("Artykuł dotyczący rozkładu goli", href="https://analizadanychwpilce.wordpress.com/2018/08/16/gole-zdarzenia-rzadkie")
    output$url_goals2 <- renderUI({
      tagList("Po więcej informacji:", url)
    })
    
    
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
  
  output$mwselection <- renderUI({
    selectInput("mwselection_values", "Wybierz kolejkę:", 
                choices = unique(df_razem[df_razem$league == ifelse(input$league_typed_prediction=="Premier League", 
                                                                  "premier", 
                                                                  tolower(gsub(" ", "", input$league_typed_prediction))), 
                                          "MW"], na.rm =T), selected = max(df_razem[df_razem$league == ifelse(input$league_typed_prediction=="Premier League", 
                                                                                                                    "premier", 
                                                                                                                    tolower(gsub(" ", "", input$league_typed_prediction))), 
                                                                                            "MW"], na.rm =T) ,
                multiple = FALSE)
  })
  observeEvent(input$prediction_butt, {
  
  output$prediction_table <- DT::renderDataTable({ DT::datatable(Predykcja_tabela(dane = df_razem,
                                                                                model = gsub(" ", "",input$model_typed_prediction),
                                                                                league =  ifelse(input$league_typed_prediction=="Premier League", 
                                                                                                 "premier", 
                                                                                                 tolower(gsub(" ", "", input$league_typed_prediction))),
                                                                                mw = input$mwselection_values)
                                                                ,options = list(
                                                                 autoWidth = TRUE, searching = FALSE, columnDefs = list(list(width = '80px'))
                                                                 ,scrollX = FALSE,  dom = "t", lengthChange = FALSE, 
                                                                 ordering = FALSE), rownames = FALSE) %>%  DT::formatStyle('Poprawność',
                      backgroundColor = DT::styleEqual(c("false", "true"), c('red', 'green')))
    })
  
  output$info_pred <- renderText({ 
    HTML(paste("1 - Wygrana gospodarza", " X2 - remis lub zwycięstwo gości", sep="<br/>"))
  })
  url1 <- a("Model regresji logistycznej", href="https://analizadanychwpilce.wordpress.com/2018/09/15/przewidywanie-wyniku-spotkania-z-wykorzystaniem-regresji-logistycznej")
  output$pred1 <- renderUI({
    tagList("Logistic Regression:", url1)
  })
  url2 <- a("Model drzewa decyzyjnego", href="https://analizadanychwpilce.wordpress.com/2018/10/15/wykorzystanie-modelu-drzewa-decyzyjnego-do-analizy-wynikow-spotkania")
  output$pred2 <- renderUI({
    tagList("Decission Tree:", url2)
  })
  url3 <- a("Model lasów losowych", href="https://analizadanychwpilce.wordpress.com/2018/11/18/model-lasow-losowych")
  output$pred3 <- renderUI({
    tagList("Random Forest:", url3)
    
  })
  })
  
 
  
})


