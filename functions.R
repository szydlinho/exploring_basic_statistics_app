library(ggplot2)
library(reshape2)
library(scales)
#library(plyr)
library(data.table)
library(dplyr)

# league <- "Premier League"
# sezon_start <- 2010
# sezon_end <- 2018

Kolowy <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  df <- data.frame(wynik = as.data.frame(table(dane$FTR))[,1],
             ilosc = as.data.frame(table(dane$FTR))[,2])
  
  df <- df[order(df$ilosc, decreasing = T),]
  df$prc <- round(df$ilosc/sum(df$ilosc)*100, 2)
 
  ggplot(df, aes(x="", y=ilosc, fill=wynik))+
    geom_bar(width = 1, stat = "identity") +  coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(prc, "%")), 
              position = position_stack(vjust = 0.5), size=5) + labs(x = NULL, y = NULL, fill = NULL, 
                           title = paste0("Częstotliwość wyniku końcowego - ", 
                                          league_name, ".\n Od sezonu 20", sezon_start, " do 20", sezon_end, "." )) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("#009E73", "#33FF99", "#ffa500"), 
                      name="", breaks=c("A", "D", "H"),
                                          labels=c("Gość", "Remis", "Gospodarz")) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#666666"))

}

Kolowy_dane <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  
 
  df <- data.frame(wynik = as.data.frame(table(dane$FTR))[,1],
                   ilosc = as.data.frame(table(dane$FTR))[,2])
  
  df$wynik <- as.character(df$wynik)
  for (i in 1:3){
    if (df[i, "wynik"] == "H"){
      df[i, "wynik"] <-  "Gospodarz"
    } else if (df[i, "wynik"] == "D"){
      df[i, "wynik"] <- "Remis"
    } else {
      df[i, "wynik"] <- "Gość"
    }
  }
  
  dane <-    c(rep(df[1, "wynik"], df[1, "ilosc"]),
                rep(df[2, "wynik"], df[2, "ilosc"]),
                rep(df[3, "wynik"], df[3, "ilosc"]))
  
  dane
}

Histogram <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]

  df2 <- dane[,c("FTHG", "FTAG")]
  df2 <-  suppressMessages(suppressWarnings(melt(df2)))

  dt <- data.table(df2)
  dt <- dt[, list(Freq =.N), by=list(variable,value)]
  df2 <- as.data.frame(dt)
  
  suppressWarnings(
    ggplot(data = df2, aes(x=value, y=Freq, fill=variable))+
    geom_histogram(stat="identity",position="dodge") +
    labs(title= paste0("Liczebność goli w meczu - ", 
                       league_name, ".\n Od sezonu 20", sezon_start, " do 20", sezon_end, "." )
         , x="Ilość  goli", y="LIczebność wystąpeń")+ 
    scale_fill_discrete(name="Gole", breaks=c("FTHG", "FTAG"),
                        labels=c("Bramki Gospodarzy", "Bramki Gości"))  +
    scale_x_continuous(limits=c(-1,9), breaks=c(0:9)) +     theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666"))
  )
}

Histogram_t <- function(league, sezon_start, sezon_end, team){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  dane <- dane[dane$HomeTeam %in% team | dane$AwayTeam %in% team, ]
  
  for (i in 1:nrow(dane)){
    if(dane[i, "HomeTeam"] %in% team){
      dane[i, "FTAG"] <- NA
    } else if(dane[i, "AwayTeam"] %in% team){
      dane[i, "FTHG"] <- NA
    }
  }
  
  df2 <- dane[,c("FTHG", "FTAG")]
  df2 <-  suppressMessages(suppressWarnings(melt(df2)))
  df2 <- df2[complete.cases(df2$value),]
  dt <- data.table(df2)
  dt <- dt[, list(Freq =.N), by=list(variable,value)]
  df2 <- as.data.frame(dt)
  
  suppressWarnings(
    ggplot(data = df2, aes(x=value, y=Freq, fill=variable))+
      geom_histogram(stat="identity",position="dodge") +
      labs(title= paste0("Liczebność goli w meczu - ", 
                         league_name," ", paste0(team, collapse = ", "), ".\n Od sezonu 20", sezon_start, " do 20", sezon_end, "." )
           , x="Ilość  goli", y="LIczebność wystąpeń")+ 
      scale_fill_discrete(name="Gole", breaks=c("FTHG", "FTAG"),
                          labels=c("Bramki Gospodarzy", "Bramki Gości"))  +
      scale_x_continuous(limits=c(-1,9), breaks=c(0:9)) +     theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666"))
  )
}


Histogram_dane <- function(league, sezon_start, sezon_end, team){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  dane <- dane[dane$HomeTeam %in% team | dane$AwayTeam %in% team, ]
  
  if (nrow(dane) > 0){
    for (i in 1:nrow(dane)){
      if(dane[i, "HomeTeam"] %in% team){
        dane[i, "FTAG"] <- NA
      } else if(dane[i, "AwayTeam"] %in% team){
        dane[i, "FTHG"] <- NA
      }
    }
  } else {
    dane <- data.frame(FTHG = numeric(), FTAG = numeric())
  }

  
  
  df2 <- dane[,c("FTHG", "FTAG")]
  #table(df2)
  df2 <-  suppressMessages(suppressWarnings(melt(df2)))
  
  dt <- t(table(df2))
 
  d <-  as.data.frame.array(dt)
  #d$Gole <- rownames(d)
  names(d) <- c("Gole gospodarzy", "Gole gości")
  d

}

Histogramy_gole <- function(league, sezon_start, sezon_end){
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  ###Rozk?ad bramek strzelonych przez gospodarza i go?cia
  
  p11 <- ggplot(dane, aes(x=FTHG, fill=FTR)) + 
    geom_histogram(binwidth=0.5, position="dodge")+ 
    scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) +   theme_minimal() +
    theme(legend.position="none", 
          axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5, color = "#666666")) +
    xlab("Bramki Gospodarzy") 
  
  p22<- ggplot(dane, aes(x=FTAG, fill=FTR)) + geom_histogram(binwidth=0.5, position="dodge")+ 
    scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9))+ 
    theme_minimal() +
    xlab("Bramki Gości") + theme(axis.title.y = element_blank(), 
                                 plot.title = element_text(hjust = 0.5, color = "#666666"),
                                 legend.position = "bottom")+ 
    scale_fill_discrete(name="Wynik Końcowy", breaks=c("A", "D", "H"), labels=c("Gość", "Remis", "Gospodarz")) 
  
  grid.arrange(p11, p22, nrow= 2,  top = paste0("Rozkład bramek wg. wyniku - ", 
                                                league_name, ".\n Od sezonu 20", 
                                                sezon_start, " do 20", sezon_end, "."))
}


Histogram_dane_hda_FTHG <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  
  df2 <- dane[,c("FTHG", "FTR")]

  df2 %>% group_by(FTHG,FTR) %>% dplyr::summarise(Freq=n()) %>% as.data.frame() %>% select(FTR, FTHG, Freq) -> df2_final 
  df2_final <- table(df2)
  
  df2_final <-  as.data.frame.array(df2_final)
  
  df2_final %>% select(H, D, A) -> df2_final
  
  names(df2_final) <- c("Wygrana gospodarzy", "Remis", "Wygrana gości")

  df2_final
  
}


Histogram_dane_hda_FTAG <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  
  df2 <- dane[,c("FTAG", "FTR")]
  
  df2 %>% group_by(FTAG,FTR) %>% dplyr::summarise(Freq=n()) %>% as.data.frame() %>% select(FTR, FTAG, Freq) -> df2_final 
  df2_final <- table(df2)
  
  df2_final <-  as.data.frame.array(df2_final)
  
  df2_final %>% select(H, D, A) -> df2_final
  
  names(df2_final) <- c("Wygrana gospodarzy", "Remis", "Wygrana gości")
  
  df2_final
  
  
  
}


Najczestszy_wynik <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  #disk <- "F:"
  #path <- paste0(disk, "/data_football/")
  league_name <- league
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")
  
  
  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane$Date <- as.Date(dane$Date, "%Y-%m-%d")
  
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  
  

  mytable <- table(dane$FTHG,dane$FTAG) 

  
  x <- melt(mytable)
  names(x) <- c("FTHG","FTAG","value")
  
  
  xx <- round(prop.table(mytable) * 100,2)
  xx
  y <- melt(xx)

  ggplot(y, aes(FTHG, FTAG)) +
    geom_point(aes(size = value), alpha=0.7, color="lightgreen", show.legend=FALSE) +
    geom_text(aes(label = value), color="black")  +
    scale_size(range = c(0,50)) +
    theme_bw() +  labs(title= paste0("Najczęstszy wynik spotkania w % - ", 
                                          league_name, ".\n Od sezonu 20", sezon_start, " do 20", sezon_end, "." )
                            , x="Ilość goli gospodarzy", y="Ilość goli gości") +
    scale_x_continuous(limits=c(0,9), breaks=c(0:max(FTAG))) + 
    scale_y_continuous(limits=c(0,9), breaks=c(0:max(FTHG)))  
  
 
  
}

Najczestszy_wynik_tabela <- function(league, sezon_start, sezon_end){
  
  sezon_start <- as.numeric(substr(sezon_start,3, 4))
  sezon_end <- as.numeric(substr(sezon_end,3, 4))
  
  league <- ifelse(league=="Premier League", "premier", league)
  path_dt <- paste0(tolower(gsub(" ", "", ifelse(league=="premier", "eng", league), )), "/")

  dane <- read.csv(paste0(
    #path_dt,  
    tolower(gsub(" ", "", league, )), "_raw_final.csv"), header=T)[,-1]
  dane <- dane[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "Sezon", "MW")]
  
  #dane <- dane[dane$HomeTeam == "Liverpool" | dane$AwayTeam == "Liverpool" , ]
  dane <- dane[dane$Sezon >= sezon_start  & dane$Sezon <= sezon_end, ]
  
  
  attach(dane)
  mytable <- table(FTHG,FTAG) 
  
  
  x <- melt(mytable)
  names(x) <- c("FTHG","FTAG","value")
  
  
  xx <- round(prop.table(mytable) * 100,2)
  xx
  y <- melt(xx)
  
  y_ordered <- y[order(y$value, decreasing = T) ,]
  names(y_ordered) <- c("Gole gospodarzy", "Gole gości", "Procent wystąpień")
  y_ordered$`Procent wystąpień` <- paste0(y_ordered$`Procent wystąpień`, "%")
  y_ordered[1:10,]
  
  
  
}
# 
# 
#  Predictions_table <- function(league, machine_learning ){
#    
#    if (league == "Premier League"){
#      league_name <- "premier"
#    } else if(league == "La Liga"){
#      league_name <- "laliga"
#    } else if(league == "Seria A"){
#      league_name <- "seriaa"
#    } else if(league == "Bundesliga"){
#      league_name <- "bundesliga"
#    }
#    
#    df <- readxl::read_xlsx("predictions_to_analysis.xlsx", sheet = 1) %>% as.data.frame()
#    
#    df <- df[df$liga == league_name,]
#    df <- df[order(df$Kolejka, decreasing = T),c("Kolejka", "Gospodarz", "Gosc", "pred","pred_05", "result_05")]
#    
#    df$pred <- round(df$pred, 2)
#  }

