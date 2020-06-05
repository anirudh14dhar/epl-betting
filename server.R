#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output){
  
  output$tab_league <- DT::renderDataTable({
    data <- data.leaguetable %>% filter(Season == input$season_selector)
    datatable(data, rownames=FALSE) 
  })
  
  output$tab_master_data <- DT::renderDataTable({
    datatable(data.masterdata, rownames=FALSE, filter = 'top',style = 'bootstrap',
              options = list(
                pageLength = 10, 
                autoWidth = TRUE,
                scrollX = TRUE,
                scrollY = TRUE)) 
  })
  
  output$tab_league_data <- DT::renderDataTable({
    datatable(data.leaguetable, rownames=FALSE, filter = 'top',style = 'bootstrap',
              options = list(
                pageLength = 10, 
                autoWidth = TRUE,
                scrollX = TRUE,
                scrollY = TRUE)) 
  })
  
  output$match_odds_chart <- renderPlotly({
    homeWinsbySeason <- data.masterdata %>% filter(FTR == 'H') %>% group_by(Season) %>% summarise(Count=n())
    awayWinsbySeason <- data.masterdata %>% filter(FTR == 'A') %>% group_by(Season) %>% summarise(Count=n())
    
    if(input$result_selector == 'Home Wins'){
      data_B365 <- data.masterdata %>% filter(FTR == 'H' & B365H < B365D & B365H < B365A) %>% group_by(Season) %>% summarise(Count = n())
      data_B365 <- data_B365 %>% mutate(PR_B365 = data_B365$Count/homeWinsbySeason$Count)
    } else {
      data_B365 <- data.masterdata %>% filter(FTR == 'A' & B365A < B365H & B365A < B365D) %>% group_by(Season) %>% summarise(Count = n())
      data_B365 <- data_B365 %>% mutate(PR_B365 = data_B365$Count/awayWinsbySeason$Count)
    }
    
    if(input$result_selector == 'Home Wins'){
      data_BW <- data.masterdata %>% filter(FTR == 'H' & BWH < BWD & BWH < BWA) %>% group_by(Season) %>% summarise(Count = n())
      data_BW <- data_BW %>% mutate(PR_BW = data_BW$Count/homeWinsbySeason$Count)
    } else {
      data_BW <- data.masterdata %>% filter(FTR == 'A' & BWA < BWH & BWA < BWD) %>% group_by(Season) %>% summarise(Count = n())
      data_BW <- data_BW %>% mutate(PR_BW = data_BW$Count/awayWinsbySeason$Count)
    }
    
    if(input$result_selector == 'Home Wins'){
      data_IW <- data.masterdata %>% filter(FTR == 'H' & IWH < IWD & IWH < IWA) %>% group_by(Season) %>% summarise(Count = n())
      data_IW <- data_IW %>% mutate(PR_IW = data_IW$Count/homeWinsbySeason$Count)
    } else {
      data_IW <- data.masterdata %>% filter(FTR == 'A' & IWA < IWH & IWA < IWD) %>% group_by(Season) %>% summarise(Count = n())
      data_IW <- data_IW %>% mutate(PR_IW = data_IW$Count/awayWinsbySeason$Count)
    } 
    
    if(input$result_selector == 'Home Wins'){
      data_PS <- data.masterdata %>% filter(FTR == 'H' & PSH < PSD & PSH < PSA) %>% group_by(Season) %>% summarise(Count = n())
      data_PS <- data_PS %>% mutate(PR_PS = data_PS$Count/homeWinsbySeason$Count)
    } else {
      data_PS <- data.masterdata %>% filter(FTR == 'A' & PSA < PSH & PSA < PSD) %>% group_by(Season) %>% summarise(Count = n())
      data_PS <- data_PS %>% mutate(PR_PS = data_PS$Count/awayWinsbySeason$Count)
    }
    
    if(input$result_selector == 'Home Wins'){
      data_WH <- data.masterdata %>% filter(FTR == 'H' & WHH < WHD & WHH < WHA) %>% group_by(Season) %>% summarise(Count = n())
      data_WH <- data_WH %>% mutate(PR_WH = data_WH$Count/homeWinsbySeason$Count)
    } else {
      data_WH <- data.masterdata %>% filter(FTR == 'A' & WHA < WHH & WHA < WHD) %>% group_by(Season) %>% summarise(Count = n())
      data_WH <- data_WH %>% mutate(PR_WH = data_WH$Count/awayWinsbySeason$Count)
    }
    
    if(input$result_selector == 'Home Wins'){
      data_VC <- data.masterdata %>% filter(FTR == 'H' & VCH < VCD & VCH < VCA) %>% group_by(Season) %>% summarise(Count = n())
      data_VC <- data_VC %>% mutate(PR_VC = data_VC$Count/homeWinsbySeason$Count)
    } else {
      data_VC <- data.masterdata %>% filter(FTR == 'A' & VCA < VCH & VCA < VCD) %>% group_by(Season) %>% summarise(Count = n())
      data_VC <- data_VC %>% mutate(PR_VC = data_VC$Count/awayWinsbySeason$Count)
    } 
    
    data <- merge(data_B365, data_BW, by = 'Season')
    data <- left_join(data, data_IW, by = 'Season')
    data <- left_join(data, data_PS, by = 'Season')
    data <- left_join(data, data_WH, by = 'Season')
    data <- left_join(data, data_VC, by = 'Season')
    
    P <- plot_ly(data, x = ~Season, y = ~PR_B365, type = 'bar', name = 'Bet365', width = 900, height = 350)
    P <- P %>% add_trace(y = ~PR_BW, name = 'Bet&Win')
    P <- P %>% add_trace(y = ~PR_IW, name = 'Pinnacle')
    P <- P %>% add_trace(y = ~PR_PS, name = 'Interwetten')
    P <- P %>% add_trace(y = ~PR_WH, name = 'William Hill')
    P <- P %>% add_trace(y = ~PR_VC, name = 'Bet Victor')
    P <- P %>% layout(
                      title = 'Ratio of Home and Away Win Odds to Actual Match Results',
                      xaxis = list(title = 'Season',autotick = FALSE,ticks = "outside",tickwidth = 2, tickcolor = toRGB("blue")), 
                      yaxis = list(title = 'Ratio', range = c(0,1),autotick = FALSE,ticks = "outside",tick0 = 0, dtick = 20, ticklen = 5, tickwidth = 2, tickcolor = toRGB("blue")),
                      barmode = 'group',
                      autosize = F, 
                      margin = list(l = 70, r = 70, b = 70, t = 70, pad = 4))
  })
  
  output$drawn_odds_chart <- renderPlotly({
    data_draws <- data.masterdata %>% filter(FTR == 'D') %>% group_by(Season) %>% summarise(Count = n())
    data_HomeWins <- data.masterdata %>% filter(FTR == 'D') %>% filter(B365H < B365A & BWH < BWA & IWH < IWA & PSH < PSA & WHH < WHA & VCH < VCA) %>% group_by(Season) %>% summarise(Count_Home = n())
    data_AwayWins <- data.masterdata %>% filter(FTR == 'D') %>% filter(B365H > B365A & BWH > BWA & IWH > IWA & PSH > PSA & WHH > WHA & VCH > VCA) %>% group_by(Season) %>% summarise(Count_Away = n())
    data <- merge(data_HomeWins,data_AwayWins, by = "Season")
    data <- merge(data,data_draws, by = 'Season')
    data$HomeWinPred = data$Count_Home/data_draws$Count
    data$AwayWinPred = data$Count_Away/data_draws$Count
    
    HWP_L <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = data$HomeWinPred[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Home Wins ', format(round(data$HomeWinPred[1], 2), nsmall = 2), '%'),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )
    
    AWP_L <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = data$AwayWinPred[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Away Wins ', format(round(data$AwayWinPred[1], 2), nsmall = 2), '%'),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )

    HWP_H <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.95,
      y = data$HomeWinPred[10],
      xanchor = 'left',
      yanchor = 'middle',
      text = ~paste('Home Wins ', format(round(data$HomeWinPred[10], 2), nsmall = 2), '%'),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )

    AWP_H <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.95,
      y = data$AwayWinPred[10],
      xanchor = 'left',
      yanchor = 'middle',
      text = ~paste('Away Wins ', format(round(data$AwayWinPred[10], 2), nsmall = 2), '%'),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )
    
    P <- plot_ly(data, x = ~Season, width = 900, height = 350)
    P <- P %>% add_trace(y = ~HomeWinPred, type = 'scatter', mode = 'lines', line = list(color = 'rgba(67,67,67,1)'))
    P <- P %>% add_trace(y = ~AwayWinPred, type = 'scatter', mode = 'lines', line = list(color = 'rgba(49,130,189, 1)'))
    P <- P %>% add_trace(x = ~c(Season[1], Season[10]),y = ~c(HomeWinPred[1], HomeWinPred[10]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', width = 2, size = 8))
    P <- P %>% add_trace(x = ~c(Season[1], Season[10]),y = ~c(AwayWinPred[1], AwayWinPred[10]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', width = 2, size = 8))
    P <- P %>% layout(
                      title = 'Home and Away Odds for Matches ending in a Draw',
                      annotations = HWP_L,
                      xaxis = list(title = 'Season',autotick = FALSE,ticks = "outside",tickwidth = 2, tickcolor = toRGB("blue"), showgrid = FALSE,linecolor = 'rgb(204, 204, 204)',linewidth = 2), 
                      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                      autosize = F, 
                      showlegend = FALSE,
                      margin = list(l = 70, r = 70, b = 70, t = 70, pad = 4))
    P <- P %>% layout(annotations = AWP_L)
    P <- P %>% layout(annotations = HWP_H)
    P <- P %>% layout(annotations = AWP_H)
  })
  
  output$goals_odds_chart <- renderGvis({
    data_base <- data.masterdata %>% mutate(TotalGoals = FTHG + FTAG) %>% select(Season, TotalGoals, B365.2.5, B365.2.5.1, P.2.5, P.2.5.1)
    data_season <- data_base %>% group_by(Season) %>% summarise(Count = n())
    
    if (input$market_selector01 == 'Bet365') {
      data_over <- data_base %>% filter(B365.2.5 < B365.2.5.1 & TotalGoals > 2) %>% group_by(Season) %>% summarise(Count = n())
      data_over <- data_over %>% add_row(Season = "2015-16", Count = 0)
      data_over <- data_over %>% add_row(Season = "2016-17", Count = 0)
      data_over <- data_over %>% add_row(Season = "2017-18", Count = 0)
      data_over <- data_over %>% add_row(Season = "2018-19", Count = 0)
      data_over <- data_over %>% mutate(GoalsOver_Odds_Ratio = Count/data_season$Count)
      data_under <- data_base %>% filter(B365.2.5 > B365.2.5.1 & TotalGoals < 3) %>% group_by(Season) %>% summarise(Count = n()) %>% mutate(GoalsUnder_Odds_Ratio = Count/data_season$Count)
    } else {
      data_over <- data_base %>% filter(P.2.5 < P.2.5.1 & TotalGoals > 2) %>% group_by(Season) %>% summarise(Count = n())
      data_over <- data_over %>% add_row(Season = "2015-16", Count = 0)
      data_over <- data_over %>% add_row(Season = "2016-17", Count = 0)
      data_over <- data_over %>% add_row(Season = "2017-18", Count = 0)
      data_over <- data_over %>% add_row(Season = "2018-19", Count = 0)
      data_over <- data_over %>% mutate(GoalsOver_Odds_Ratio = Count/data_season$Count)
      data_under <- data_base %>% filter(P.2.5 > P.2.5.1 & TotalGoals < 3) %>% group_by(Season) %>% summarise(Count = n()) %>% mutate(GoalsUnder_Odds_Ratio = Count/data_season$Count)
    }
 
    data <- merge(data_over, data_under, by = "Season") %>% select(Season, GoalsOver_Odds_Ratio, GoalsUnder_Odds_Ratio)
    
    P <- gvisBarChart(data, "Season", c("GoalsOver_Odds_Ratio","GoalsUnder_Odds_Ratio"),
                       options=list(
                         width = 700, 
                         height = 350, 
                         fontName = 'Helvetica',
                         hAxis="{title: 'Ratio', titleTextStyle: {color: 'black', fontSize: 15}}",
                         vAxis="{title: 'Season', titleTextStyle: {color: 'black', fontSize: 15}}",
                         legend="{position: 'top'}"))  
  })
  
  output$asian_handicap_odds_chart <- renderGvis({
    data_base <- data.masterdata %>% select(Season, FTR, B365AHH, B365AHA, PAHH, PAHA)
    data_season_H <- data.masterdata %>% filter(FTR == 'H') %>% group_by(Season) %>% summarise(Count = n())
    data_season_A <- data.masterdata %>% filter(FTR == 'A') %>% group_by(Season) %>% summarise(Count = n())
    
    if (input$market_selector02 == 'Bet365') {
      H <- data_base %>% filter(FTR == 'H' & B365AHH < B365AHA) %>% group_by(Season) %>% summarise(Count_H = n()) 
      A <- data_base %>% filter(FTR == 'A' & B365AHA < B365AHH) %>% group_by(Season) %>% summarise(Count_A = n()) %>% mutate(Ratio_AHA = Count_A/data_season_A$Count)
      H <- H %>% add_row(Season = "2010-11", Count_H = 0)
      H <- H %>% add_row(Season = "2011-12", Count_H = 0)
      H <- H %>% add_row(Season = "2012-13", Count_H = 0)
      H <- H %>% add_row(Season = "2015-16", Count_H = 0)
      H <- H %>% add_row(Season = "2016-17", Count_H = 0)
      H <- H %>% add_row(Season = "2017-18", Count_H = 0)
      H <- H %>% arrange(Season) %>% mutate(Ratio_AHH = Count_H/data_season_H$Count)
    } else {
      H <- data_base %>% filter(FTR == 'H' & PAHH < PAHA) %>% group_by(Season) %>% summarise(Count_H = n()) 
      A <- data_base %>% filter(FTR == 'A' & PAHA < PAHH) %>% group_by(Season) %>% summarise(Count_A = n()) 
      A <- A %>% add_row(Season = "2013-14", Count_A = 0)
      A <- A %>% add_row(Season = "2014-15", Count_A = 0)
      A <- A %>% mutate(Ratio_AHA = Count_A/data_season_A$Count)      
      H <- H %>% add_row(Season = "2010-11", Count_H = 0)
      H <- H %>% add_row(Season = "2011-12", Count_H = 0)
      H <- H %>% add_row(Season = "2012-13", Count_H = 0)
      H <- H %>% add_row(Season = "2015-16", Count_H = 0)
      H <- H %>% add_row(Season = "2016-17", Count_H = 0)
      H <- H %>% add_row(Season = "2017-18", Count_H = 0)
      H <- H %>% mutate(Ratio_AHH = Count_H/data_season_H$Count)
    }
    
    data <- merge(H, A, by = 'Season')
    
    P <- gvisBarChart(data, "Season", c("Ratio_AHH","Ratio_AHA"),
                       options=list(
                         width = 700, 
                         height = 350, 
                         fontName = 'Helvetica',
                         hAxis="{title: 'Ratio', titleTextStyle: {color: 'black', fontSize: 15}}",
                         vAxis="{title: 'Season', titleTextStyle: {color: 'black', fontSize: 15}}",
                         legend="{position: 'top'}"))
  })
  
  output$highest_odds_chart <- renderPlotly({
    if (input$category_selector == 'Home' & input$bet_selector == 'Match Result 1*2') {
      data <- data.masterdata %>% filter(FTR == 'H' & B365H != 'NA' & BWH != 'NA' & IWH != 'NA' & PSH != 'NA' & WHH != 'NA' & VCH != 'NA') %>% select(B365H, BWH, IWH, PSH, WHH, VCH)
      B365 <- 0
      BW <- 0
      IW <- 0
      PS <- 0
      WH <- 0
      VC <- 0
      i <- 1
      maximum <- 0
      
      while (i <= nrow(data)) {
        maximum = max(data[i,])
        if (data[i,1] == maximum){
          B365 = B365 + 1
        } else if (data[i,2] == maximum) {
          BW = BW + 1
        } else if (data[i,3] == maximum) {
          IW = IW + 1
        } else if (data[i,4] == maximum) {
          PS = PS + 1
        } else if (data[i,5] == maximum) {
          WH = WH + 1
        } else {
          VC = VC + 1
        } 
        i = i + 1
      }
      
      x <- c("Bet365", "Bet&Win", "Interwetten", "Pinnacle", "William Hill", "Bet Victor")
      y <- c(B365, BW, IW, PS, WH , VC) 
      
    } else if (input$category_selector == 'Away' & input$bet_selector == 'Match Result 1*2') {
      data <- data.masterdata %>% filter(FTR == 'A' & B365A != 'NA' & BWA != 'NA' & IWA != 'NA' & PSA != 'NA' & WHA != 'NA' & VCA != 'NA') %>% select(B365A, BWA, IWA, PSA, WHA, VCA)
      B365 <- 0
      BW <- 0
      IW <- 0
      PS <- 0
      WH <- 0
      VC <- 0
      i <- 1
      maximum <- 0
      
      while (i <= nrow(data)) {
        maximum = max(data[i,])
        if (data[i,1] == maximum){
          B365 = B365 + 1
        } else if (data[i,2] == maximum) {
          BW = BW + 1
        } else if (data[i,3] == maximum) {
          IW = IW + 1
        } else if (data[i,4] == maximum) {
          PS = PS + 1
        } else if (data[i,5] == maximum) {
          WH = WH + 1
        } else {
          VC = VC + 1
        } 
        i = i + 1
      }
      
      x <- c("Bet365", "Bet&Win", "Interwetten", "Pinnacle", "William Hill", "Bet Victor")
      y <- c(B365, BW, IW, PS, WH , VC)  
      
    } else if (input$category_selector == 'Home' & input$bet_selector == 'Asian Handicap') {
      data <- data.masterdata %>% filter(FTR == 'H' & B365AHH != 'NA', PAHH != 'NA') %>% select(B365AHH, PAHH)
      B365AH <- 0
      PSAH <- 0
      i <- 1
      maximum <- 0

      while (i <= nrow(data)) {
        maximum = max(data[i,])
        if (data[i,1] == maximum){
          B365AH = B365AH + 1
        } else {
          PSAH = PSAH + 1
        } 
        i = i + 1
      }
      
      x <- c("Bet365", "Pinnacle")
      y <- c(B365AH, PSAH) 
      
    } else {
      data <- data.masterdata %>% filter(FTR == 'A' & B365AHA != 'NA', PAHA != 'NA') %>% select(B365AHA, PAHA)
      B365AH <- 0
      PSAH <- 0
      i <- 1
      maximum <- 0
      
      while (i <= nrow(data)) {
        maximum = max(data[i,])
        if (data[i,1] == maximum){
          B365AH = B365AH + 1
        } else {
          PSAH = PSAH + 1
        } 
        i = i + 1
      }
      
      x <- c("Bet365", "Pinnacle")
      y <- c(B365AH, PSAH)
      
    }
    
    P <- data %>% plot_ly() 
    P <- P %>% add_trace(x = ~x, y = ~y, type = 'bar',
                         text = y, textposition = 'auto',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
    P <- P %>% layout(
                      xaxis = list(title = 'Bookmaker'), 
                      yaxis = list(title = 'Count'))
  })
  
  output$highest_goal_odds_chart <- renderPlotly({
    if (input$category_selector == 'Home') {
      data <- data.masterdata %>% filter(FTR == 'H' & B365.2.5 != 'NA'& B365.2.5.1 != 'NA' & P.2.5  != 'NA' & P.2.5.1 != 'NA') %>% select(B365.2.5, B365.2.5.1, P.2.5, P.2.5.1)
    } else {
      data <- data.masterdata %>% filter(FTR == 'A' & B365.2.5 != 'NA'& B365.2.5.1 != 'NA' & P.2.5  != 'NA' & P.2.5.1 != 'NA') %>% select(B365.2.5, B365.2.5.1, P.2.5, P.2.5.1)
    }
    
    B365GO <- 0
    B365GU <- 0
    PSGO <- 0
    PSGU <- 0
    i <- 1
    maximum <- 0
    
    while (i <= nrow(data)) {
      maxGO = max(data[i,c(1,3)])
      maxGU = max(data[i,c(2,4)])
      if (data[i,1] == maxGO){
        B365GO = B365GO + 1
      } else {
        PSGO = PSGO + 1
      } 
      if (data[i,2] == maxGU) {
        B365GU = B365GU + 1
      } else {
        PSGU = PSGU + 1
      }
      i = i + 1
    }
    
    x <- c("Bet365", "Pinnacle")
    y <- c(B365GO, PSGO) 
    y2 <- c(B365GU, PSGU)

    P <- data %>% plot_ly() 
    P <- P %>% add_trace(x = ~x, y = ~y, type = 'bar', name = 'Goals > 2.5',
                         text = y, textposition = 'auto',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
    P <- P %>% add_trace(x = ~x, y = ~y2, type = 'bar', name = 'Goals < 2.5', 
                             text = y2, textposition = 'auto',
                             marker = list(color = 'rgb(58,200,225)',
                                        line = list(color = 'rgb(8,48,107)', width = 1.5)))
    P <- P %>% layout(
                      barmode = 'group',
                      xaxis = list(title = 'Bookmaker'), 
                      yaxis = list(title = 'Count'))    
    
  })
  
  output$team_percent_win <- renderPlotly({
    data.home.matches <- data.masterdata %>% group_by(Team) %>% summarise(Count = n())
    data.away.matches <- data.masterdata %>% group_by(Opponent) %>% summarise(Count = n()) %>% rename(., Team = Opponent)
    data.matches <- rbind(data.home.matches, data.away.matches) %>% group_by(Team) %>% summarise(CountT = sum(Count))
    
    data <- merge(merge(data.masterdata %>% filter(FTR == 'H') %>% 
                          group_by(Team) %>% 
                          summarise(CountH = n()),data.masterdata %>% 
                          filter(FTR == 'A') %>% group_by(Team) %>% 
                          summarise(CountA = n()), by = "Team"),data.matches, by = "Team") %>% 
                          mutate(HWinPercent = CountH/CountT * 100, AWinPercent = CountA/CountT * 100)
    
    P <- data %>% plot_ly(width = 700, height = 450)
    P <- P %>% add_trace(x = ~HWinPercent, y = ~Team, type = 'bar', name = 'Home Win Percent')
    P <- P %>% add_trace(x = ~AWinPercent, y = ~Team, type = 'bar', name = 'Away Win Percent')
    P <- P %>% layout(
      barmode = 'group',
      xaxis = list(title = 'Percent'), 
      yaxis = list(title = 'Team'))
  })
  
  output$htr_eq_ftr <- renderPlotly({
    H <- data.masterdata %>% filter(FTR == HTR & FTR == 'H') %>% group_by(Team) %>% summarise(CountH = n())
    A <- data.masterdata %>% filter(FTR == HTR & FTR == 'A') %>% group_by(Opponent) %>% summarise(CountA = n()) %>% rename(., Team = Opponent)
    data <- merge(H, A, by = "Team")
    
    P <- data %>% plot_ly(width = 700, height = 350)
    P <- P %>% add_trace(x = ~Team, y = ~CountH, type = 'bar', name = 'Half Time = Full Time(Home)')
    P <- P %>% add_trace(x = ~Team, y = ~CountA, type = 'bar', name = 'Half Time = Full Time(Away)')
    P <- P %>% layout(
      barmode = 'group',
      xaxis = list(title = 'No. of Matches'), 
      yaxis = list(title = 'Team'))
  })
  
  output$htr_ne_ftr <- renderPlotly({
    H <- data.masterdata %>% filter(HTR == 'A' & FTR == 'H') %>% group_by(Team) %>% summarise(CountH = n()) %>% arrange(desc(CountH))
    A <- data.masterdata %>% filter(HTR == 'H' & FTR == 'A') %>%
                    group_by(Opponent) %>% summarise(CountA = n()) %>% 
                    rename(., Team = Opponent) %>% arrange(desc(CountA))
    data <- merge(H, A, by = "Team") %>% arrange(desc(CountH + CountA))
    
    P <- data %>% plot_ly(width = 700, height = 350)
    P <- P %>% add_trace(x = ~Team, y = ~CountH, type = 'bar', name = 'Home Comeback Wins')
    P <- P %>% add_trace(x = ~Team, y = ~CountA, type = 'bar', name = 'Away Comeback Wins')
    P <- P %>% layout(
      barmode = 'stack',
      xaxis = list(title = 'Team'), 
      yaxis = list(title = 'No. of Matches'))
  }) 
  
  output$top_btm_odds <- renderPlotly({
    Max4 <- data.leaguetable %>% group_by(Team) %>% summarise(MatchCount = sum(Matches.Played), WinCount = sum(Won)) %>% filter(MatchCount > 250) %>% top_n(4)
    Max4 <- data.frame(lapply(Max4, function(x) {gsub("Manchester City", "Man City", x)}))
    Max4 <- data.frame(lapply(Max4, function(x) {gsub("Manchester United", "Man United", x)}))
    Min3 <- data.leaguetable %>% group_by(Team) %>% summarise(MatchCount = sum(Matches.Played), WinCount = sum(Won)) %>% filter(MatchCount > 250) %>% arrange(WinCount) %>% head(3)
    Min3 <- data.frame(lapply(Min3, function(x) {gsub("Swansea City", "Swansea", x)}))
    
    H <- data.masterdata %>% filter(Team %in% c(Max4$Team,Min3$Team) & AvgH < AvgA & AvgH < AvgD) %>% group_by(Team) %>% summarise(CountH = n())
    A <- data.masterdata %>% filter(Opponent %in% c(Max4$Team,Min3$Team) & AvgA < AvgH & AvgA < AvgH) %>% group_by(Opponent) %>% summarise(CountA = n()) %>% rename(., Team = Opponent)
    HD <- data.masterdata %>% filter(Team %in% c(Max4$Team,Min3$Team) & AvgD < AvgH & AvgD < AvgA) %>% group_by(Team) %>% summarise(CountHD = n())
    HD <- HD %>% add_row(Team = "Aston Villa", CountHD = 0)
    HD <- HD %>% add_row(Team = "Sunderland", CountHD = 0)
    HD <- HD %>% add_row(Team = "Swansea", CountHD = 0)
    AD <- data.masterdata %>% filter(Opponent %in% c(Max4$Team,Min3$Team) & AvgD < AvgH & AvgD < AvgA) %>% group_by(Opponent) %>% summarise(CountAD = n()) %>% rename(., Team = Opponent)
    AD <- AD %>% add_row(Team = 'Liverpool', CountAD = 0)
    AD <- AD %>% add_row(Team = 'Man City', CountAD = 0)
    AD <- AD %>% add_row(Team = 'Aston Villa', CountAD = 0)
    AD <- AD %>% add_row(Team = 'Sunderland', CountAD = 0)
    AD <- AD %>% add_row(Team = 'Swansea', CountAD = 0)
    data <- merge(merge(H, A, by = "Team"),merge(HD, AD, by = "Team"), by = "Team") %>% mutate(CountD = CountHD + CountAD) %>% arrange(desc(CountH))
    
    P <- data %>% plot_ly(width = 700, height = 350)
    P <- P %>% add_trace(x = ~Team, y = ~CountH, type = 'bar', name = 'Odds = Home Win',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
    P <- P %>% add_trace(x = ~Team, y = ~CountA, type = 'bar', name = 'Odds = Away Win',
                         marker = list(color = 'rgb(131, 235, 162)',
                                       line = list(color = 'rgb(13, 166, 59)', width = 1.5)))
    P <- P %>% add_trace(x = ~Team, y = ~CountD, type = 'bar', name = 'Odds = Draw',
                         marker = list(color = 'rgb(227, 163, 136)',
                                       line = list(color = 'rgb(207, 95, 48)', width = 1.5)))
    P <- P %>% layout(
      barmode = 'group',
      xaxis = list(title = 'Team'), 
      yaxis = list(title = 'No. of Matches'))    
    
  })
  
  output$wins_by_opponents <- renderPlotly({
    H <- data.masterdata %>% filter(Team == input$team_selector & FTR == 'H') %>% group_by(Opponent) %>% summarise(CountH = n()) %>% rename(., Team = Opponent)
    A <- data.masterdata %>% filter(Opponent == input$team_selector & FTR == 'A') %>% group_by(Team) %>% summarise(CountA = n())
    data <- merge(H, A, by = "Team") %>% arrange(desc(CountH, CountA))
    
    P <- plot_ly(data, x = ~Team, y = ~CountH, type = 'bar', name = 'Home Wins', width = 700, height = 350,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)', width = 1.5)))
    P <- P %>% add_trace(x = ~Team, y = ~CountA, type = 'bar', name = 'Away Wins',
                         marker = list(color = 'rgb(131, 235, 162)',
                                       line = list(color = 'rgb(13, 166, 59)', width = 1.5)))                        
    P <- P %>% layout(
      barmode = 'stacked',
      xaxis = list(title = 'Team'), 
      yaxis = list(title = 'No. of Matches'))      
  })
  
  output$odds_by_opponent <- renderPlotly({
    H <- data.masterdata %>% filter(Team == input$team_selector01 & AvgH != 'NA') %>% group_by(Opponent) %>% summarise(RatioH = mean(AvgH)) %>% rename(., Team = Opponent)
    A <- data.masterdata %>% filter(Opponent == input$team_selector01 & AvgA != 'NA') %>% group_by(Team) %>% summarise(RatioA = mean(AvgA))
    data <- merge(H, A, by = "Team")
    
    P <- plot_ly(data, x = ~Team, y = ~RatioH, type = 'bar', name = 'Home Wins', width = 700, height = 350)
    P <- P %>% add_trace(x = ~Team, y = ~RatioA, type = 'bar', name = 'Away Wins')
    P <- P %>% layout(
      barmode = 'group',
      xaxis = list(title = 'Team'), 
      yaxis = list(title = 'Average'))
  })
  
  output$results_by_referee <- renderPlotly({
    Referee.data <- data.masterdata %>% group_by(Referee) %>% summarise(Count = n()) %>% top_n(10)
    Referee.HomeWins <- data.masterdata %>% filter(Referee %in% Referee.data$Referee & FTR == 'H') %>% group_by(Referee) %>% summarise(CountH = n())
    Referee.AwayWins <- data.masterdata %>% filter(Referee %in% Referee.data$Referee & FTR == 'A') %>% group_by(Referee) %>% summarise(CountA = n())
    Referee.Draws <- data.masterdata %>% filter(Referee %in% Referee.data$Referee & FTR == 'D') %>% group_by(Referee) %>% summarise(CountD = n())
    data <- merge(merge(Referee.HomeWins, Referee.AwayWins, by = "Referee"), Referee.Draws, by = "Referee")
    
    P <- plot_ly(data, x = ~Referee, y = ~CountH, type = 'bar', name = 'Home Wins', width = 700, height = 350, marker = list(color = 'rgb(49,130,189)'))
    P <- P %>% add_trace(y = ~CountA, name = 'Away Wins', marker = list(color = 'rgb(204,204,204)'))
    P <- P %>% add_trace(y = ~CountD, name = 'Draws', marker = list(color = 'rgb(207, 64, 64)'))
    P <- P %>% layout(
      margin = list(b = 100),
      barmode = 'group',
      xaxis = list(title = 'Referee', tickangle = -45), 
      yaxis = list(title = 'No. of Matches'))
  })
  
  output$bkngs_by_referee <- renderPlotly({
    if (input$card_selector == 'Yellow') {
      Referee.data <- data.masterdata %>% group_by(Referee) %>% summarise(Count = n()) %>% top_n(10)
      Referee.Home.Yellow <- data.masterdata %>% filter(Referee %in% Referee.data$Referee) %>% group_by(Referee) %>% summarise(CountH = sum(HY))
      Referee.Away.Yellow <- data.masterdata %>% filter(Referee %in% Referee.data$Referee) %>% group_by(Referee) %>% summarise(CountA = sum(AY))
      data <- merge(Referee.Home.Yellow, Referee.Away.Yellow, by = "Referee")
    } else {
      Referee.data <- data.masterdata %>% group_by(Referee) %>% summarise(Count = n()) %>% top_n(10)
      Referee.Home.Red <- data.masterdata %>% filter(Referee %in% Referee.data$Referee) %>% group_by(Referee) %>% summarise(CountH = sum(HR))
      Referee.Away.Red <- data.masterdata %>% filter(Referee %in% Referee.data$Referee) %>% group_by(Referee) %>% summarise(CountA = sum(AR))
      data <- merge(Referee.Home.Red, Referee.Away.Red, by = "Referee")
    }

    P <- plot_ly(data, x = ~Referee, y = ~CountH, type = 'bar', name = 'Home Wins', width = 700, height = 350, marker = list(color = 'rgb(207, 64, 64)'))
    P <- P %>% add_trace(y = ~CountA, name = 'Away Wins', marker = list(color = 'rgb(204,204,204)'))
    P <- P %>% layout(
      margin = list(b = 100),
      barmode = 'group',
      xaxis = list(title = 'Referee', tickangle = -45), 
      yaxis = list(title = 'Count'))
  })
  
  output$odds_by_ref <- renderPlotly({
    Referee.data <- data.masterdata %>% group_by(Referee) %>% summarise(Count = n()) %>% top_n(10)
    H <- data.masterdata %>% filter(Referee %in% Referee.data$Referee & B365H < B365A & BWH < BWA & IWH < IWA & PSH < PSA & WHH < WHA & VCH < VCA) %>% group_by(Referee) %>% 
                      summarise(Count = n()) %>% mutate(PercentH = (Count/Referee.data$Count)*100)
    A <- data.masterdata %>% filter(Referee %in% Referee.data$Referee & B365A < B365H & BWA < BWH & IWA < IWH & PSA < PSH & WHA < WHH & VCA < VCH) %>% group_by(Referee) %>% 
                      summarise(Count = n()) %>% mutate(PercentA = (Count/Referee.data$Count)*100)
    data <- data <- merge(H, A, by = "Referee")
    
    P <- plot_ly(data, x = ~Referee, y = ~PercentH, type = 'bar', name = 'Home Matches', width = 700, height = 350)
    P <- P %>% add_trace(y = ~PercentA, name = 'Away Matches')
    P <- P %>% layout(
      margin = list(b = 100),
      barmode = 'group',
      xaxis = list(title = 'Referee', tickangle = -45), 
      yaxis = list(title = 'Percent'))    
  })
})
