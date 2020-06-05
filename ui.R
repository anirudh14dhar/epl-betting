#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(dashboardPage(
    dashboardHeader(
        title = span(img(src="header-corner.jpg")),
        titleWidth = 350,
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 52px}"),
                tags$style(".main-header .logo {height: 52px; padding: 0px 0px}")
        )
    ),
    dashboardSidebar(width = 350,
        tags$style(".left-side, .main-sidebar {padding-top: 75px}"),
        sidebarUserPanel("Aniruddha Dhar",
                         subtitle = 'anirudh14.dhar@gmail.com',
                         image = "aniruddha_Dhar.jpg"
        ),
        
        sidebarMenu(
            id = "tabs",
            menuItem("About Premier League", tabName = "about-pl", icon = icon("info-circle")),
            menuItem("Odds by Bookmaker", tabName = "odds-bookie", icon = icon("address-book")),
            menuItem("Odds by Match Statistics", tabName = "odds-match", icon = icon("calculator")),
            menuItem("Data", tabName = "raw-data", icon = icon("archive"))
        )
    ),
    dashboardBody(
        tags$head( 
            tags$style(HTML(".main-sidebar { font-size: 15px }"))
        ),
        tabItems(
            tabItem(tabName = "about-pl",
                    fluidRow(column(8, offset = 2, align = 'center', 
                                    h1(tags$b("Insights into the Premier League Betting Patterns\n")),
                                    h2(tags$b("How accurate have the odds been over the years"),style = "font-size: 20px")
                            )
                    ),
                    
                    br(),
                    
                    br(),
                    
                    fluidRow(column(3, align = "right",offset = 1,img(style="width: 70%", src = "landing-mainpanel.jpg")),
                             column(6, align = "left",
                                    h4(p("The Premier League, often referred to as the English Premier League or the EPL outside England,
                                          is the top level of the English football league system. The competition was founded as the 
                                          FA Premier League on 20 February 1992 and is contested by 20 clubs operating on a system of promotion
                                          and relegation with the English Football League(EFL).With broadcasts in over 212 territories
                                          to over 643 million homes and a potential TV audience of over 4.7 billion people, it is one
                                          of the world's most popular sporting leagues, enjoying a cult following around the globe."), p("As per estimations, 
                                          the global sports betting industry is worth close to a a trillion dollars with 70% of that coming from 
                                          trading on football. By analyzing the data from the last 10 seasons (2010/11 - 2019/20), the project aims to gain a 
                                          deeper understanding of match predictions based on bookmaker odds and factors that possibly influence these numbers.")))
                    ),
                    
                    br(),
                    
                    fluidRow(column(10, offset = 1, align = "center",
                                    selectizeInput("season_selector", "Select the Season:",
                                                   choices = unique(data.leaguetable$Season), selected = "2019-20"),
                                    box(title = "League Table By Season" ,solidHeader = TRUE, status = "primary",
                                        DT::dataTableOutput('tab_league'),width = 12)
                    ))
                    
            ),
            
            tabItem(tabName = "odds-bookie",
                    fluidRow(column(8, offset = 2, align = 'center', 
                                    h2(tags$b("Exploratory analysis of bookmaker odds from 2010 to 2020"))
                        )
                    ),
                    
                    br(),
                    
                    fluidRow(column(4,
                                    box(width = 12, title="Glossary", solidHeader = TRUE, status = "primary",
                                        br(),
                                        p(tags$b("Bookmaker:"),("Person or establishment that takes bets from customers")),
                                        p(tags$b("Oddsmaker:"),("Person who sets the odds")),
                                        p(tags$b("Wager:"),("A bet")),
                                        p(tags$b("Chalk:"),("The favorite team in the game")),
                                        p(tags$b("Closing line:"),("The final line before the game or event begins")),
                                        p(tags$b("Dime:"),("Jargon for a $1,000 bet")),
                                        p(tags$b("Nickle:"),("Jargon for a $500 bet")),
                                        p(tags$b("Dollar:"),("Jargon for a $100 bet")),
                                        p(tags$b("Even:"),("Odds that are considered 50-50")),
                                        p(tags$b("Premier League Odds For Individual Matches:")),
                                        p(tags$b("Match Result 1X2:"),("Selecting the result (Home win, Draw, Away win) at the end of 90 minutes plus added time")),
                                        p(tags$b("Asian Handicap:"),("Selecting the winner with each competing team carrying a goals handicap")),
                                        p(tags$b("Goal Total Over/Under:"),("Selecting either over or under a specified number of goals")),
                                        p(tags$b("Premier League Title Odds:"),("Selecting the league winner, 
                                                                                market is available before and throughout the season")),
                                        p(tags$b("Premier League Top 4 Odds:"),("Selecting the top 4 teams in the league at the end of season")),
                                        p(tags$b("Premier League Relegation Odds:"),("Selecting the bottom 3 teams in the league at the end of season")),
                                        p(tags$b("Premier League Points Handicap Odds:"),("Selecting the team with the maximum points at the end of the season.
                                                                                          Each team in the league is given a points handicap,typically the club 
                                                                                          that is favourite to win the league is given mark of 0 (scratch) all 
                                                                                          the way down to the relegation candidates who are usually around the 
                                                                                          +40 points mark.At the completion of the season, each club has their 
                                                                                          league points added to their handicap, to calculate the total points
                                                                                          for the season")),
                                        p(tags$b("Premier League Top Goalscorer Odds:"),("Selecting the top goal scorer of the league at the end of season")),
                                        br(),
                                        br(),
                                        br(),
                                        br()
                                    )
                            ),
                            column(8,
                                   h4(tags$b("How accurately do the outright match result odds reflect on the final match outcome")),
                                   p("Bookmaker odds seem to be a fairly accurate reflection of the match result when a win is registered by the home team. There is
                                      a sharp drop is observed when predicting a win for the away team, inspite of a marginal increase during recent seasons.
                                      Calculating the odds for home and away wins against the actual results show a similar pattern for all six bookmakers over the 
                                      last 10 seasons. If we look into the Home and Away data for matches which ended in a draw, we can see that odds have
                                      mostly favored a win for the home team."),
                                   selectizeInput("result_selector", "Choose a Result:",
                                                  choices = c('Home Wins','Away Wins'),
                                                  selected = "Home Wins"),
                                   plotlyOutput("match_odds_chart"),
                                   plotlyOutput("drawn_odds_chart")
                            )
                    ),
                    fluidRow(column(6,
                                    h4(tags$b("Trend of Over/Under Goal Odds per Match")),
                                    p("When it comes to predicting the number of goals above/under 2.5 in a match, bookmaker odds seem 
                                      to be less helpful. The number of times when total goals in a match have been accurately reflected
                                      by the bookmaker odds is less than 50% of the total number of games played during the season.
                                      The data from the current season show a favourable outcome when betting on goals under 2.5 with Bet365 
                                      and better results when betting with Pinnacle for goals over 2.5"),
                                    br(),
                                    br(),
                                    box( width = 100,
                                         radioButtons("market_selector01", "Select a Bookmaker:",
                                                      choices = c('Bet365', 'Pinnacle'), 
                                                      selected = "Bet365", inline = TRUE),
                                         htmlOutput("goals_odds_chart") 
                                    )
                            ),
                            column(6,
                                   h4(tags$b("Trend of Asian Handicap Odds per Match")),
                                   p("Asian handicap, as the name suggests is a popular betting form in Asia and is designed to eliminate the possibility of a draw 
                                     in a match,reducing the match to two possible outcomes. The favourite in the match carries a handicap (negative goal 
                                     count of 0.5, 1.5 etc), thereby creating a level ground for bets on either teams. Looks like odds are more accurate for away
                                     teams, which is probably not surprising. The advantage of a home game will make most teams the favourite for the match and thereby
                                     result in a handicap being awarded in this market. This is probably not the case when involving the top teams since they still
                                     remain favourites when playing away from home against weaker oppositions"),
                                   box( width = 100,
                                        radioButtons("market_selector02", "Select a Bookmaker:",
                                                     choices = c('Bet365', 'Pinnacle'), 
                                                     selected = "Bet365", inline = TRUE),     
                                        htmlOutput("asian_handicap_odds_chart")   
                                   )
                            )
                    ),
                    fluidRow(column(6,
                                    h4(tags$b("Which bookmakers have provided the highest odds for Wins"))
                            )
                    ),
                    fluidRow(column(10,
                                    p("Bets placed on the highest odd will result in maximum returns. For the last 10 seasons, Bet365 has provided
                                      the highest odds the maximum number of times for home wins, followed by Interwetten. For away wins, the highest
                                      odds have been provided by Bet365 the maximum number of times, closely followed by William Hill. The data indicates
                                      that maximum profits have been made betting with Bet365 when predicting the outcome of a match, provided a clear
                                      winner was decided at the end of the match"),
                                    p("For Asian Handicap, a comparison between Bet365 and Pinnacle for home wins show profits were maximized almost an 
                                      equal number of times betting with either of the bookmakers, with Bet365 only having slight edge. However, this
                                      drastically changes for away wins, wherein Pinnacle has provided the highest odds on almost 75% of the maches during
                                      the past decade"),
                                    p("In case of bets on goals over 2.5 for a given match, Pinnacle is marginally ahead of Bet365 for home wins. For away
                                      wins, there is nothing much to differentiate between the two. For goals under 2.5, Bet365 seems to be the bookmaker
                                      to place bets with for home wins. Again the numbers are equally distributed between the two for away wins")
                            )
                    ),
                    fluidRow(column(2,
                                    radioButtons("category_selector", "Select a category:",
                                                 choices = c('Home', 'Away'), 
                                                 selected = "Home", inline = TRUE)
                            ),
                            column(4,
                                   radioButtons("bet_selector", "Select a Bet Type:",
                                                choices = c('Match Result 1*2','Asian Handicap'), 
                                                selected = "Match Result 1*2", inline = TRUE)
                            )
                    ),
                    fluidRow(column(6,
                                    plotlyOutput("highest_odds_chart")
                            ),
                            column(6,
                                    plotlyOutput("highest_goal_odds_chart")
                            )
                    ),
                    
                    br(),
                    
                    br(),
            ),
            
            tabItem(tabName = "odds-match",
                    fluidRow(column(8, offset = 2, align = 'center', 
                                    h2(tags$b("Factors that influence the setting of Odds"))
                            )
                    ),
                    
                    br(),
                    
                    fluidRow(column(6,
                                    h4(tags$b("Win %age for teams Home and Away")),
                                    plotlyOutput("team_percent_win")
                            ),
                            column(6,
                                   br(),
                                   
                                   br(),
                                   
                                   box(width = 100,
                                       p("The last decade has shown that the 'Big Six' teams have been formidable at home with
                                         a win percantage greater than 30%, led by Man City(38.37%) who have also won the maximum 
                                         number of league titles during this period.Playing away from home though,these teams havent 
                                         been that prolific with a win %age of less than 10%. Some of the lower rung teams like Aston Villa 
                                         and Crystal Palace who have played regularly over the decade actually boast much better away game 
                                         record"),
                                       p("For home matches, teams have been successful more often when leading at half time. The numbers 
                                         are less overwhelming for away matches with the exception of the top six, who seem to be equally
                                         strong in preserving their half time lead both at home and away"),
                                       p("Tottenham have the most number of comeback wins among the top teams, closely followed by 
                                         Man United. Surprisingly, Man City with the highest win percentage have made the least number of
                                         comeback wins over the years among the big teams, indicating that they are more comfortable when 
                                         making a strong start."),
                                       p("Historical data against an opponent is a strong factor when driving sentiments and therefore
                                         the odds in favour of a particular team. Some of the teams have been overwhelmingly strong against specific
                                         opponents over the years. Case in point, Man Utd against Stoke, with United having a 9-2 edge over their
                                         opponents. Similarly Liverpool have won 9 times against Newcastle, who have succeeded only 4 times(all at home)
                                         against their more illustrious opponents in the last decade")
                                   )
                            )
                    ),
                    
                    br(),
                    
                    br(),
                    
                    br(),
                    
                    fluidRow(column(6,
                                    h4(tags$b("Conversion of Half-time Lead to Wins")),
                                    plotlyOutput("htr_eq_ftr")
                            ),
                            column(6,
                                   h4(tags$b("Conversions of Half-time Deficit to Wins")),
                                   plotlyOutput("htr_ne_ftr")
                            )
                    ),
                    fluidRow(column(12, align = 'center',
                                    selectizeInput("team_selector", "Choose a Team:",
                                                   choices = unique(data.masterdata$Team),
                                                   selected = "Arsenal"),                                   
                                    plotlyOutput("wins_by_opponents")                                    
                            )
                    ),
                    fluidRow(column(6,
                                    h4(tags$b("How often have odds favored these teams")),
                                    h5(tags$b("Top 4 and bottom 3 by number of matches won (min 250 games)")),
                                    br(),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    p("Understandably, the odds have been heavily stacked up in favor of the big teams both home and away. Odds of
                                      matches involving these teams, ending in a draw have been almost negligible. For teams at the other
                                      end of the league table, the numbers have been far less favourable although bookmakers back them more 
                                      often when playing at home"),
                                    
                                    br(),
                                    
                                    plotlyOutput("top_btm_odds")
                            ),
                            column(6,
                                   h4(tags$b("How have the odds stacked up for teams against each opponent")),
                                   h5(tags$b("Mean of average odds among bookmakers per match")),
                                   selectizeInput("team_selector01", "Choose a Team:",
                                                  choices = unique(data.masterdata$Team),
                                                  selected = "Arsenal"),  
                                   p("Similar trend of odds favoring the top teams is observed when exploring odds for each team against their 
                                    opponents. When the top teams play against each other, bookmakers have traditionally favored the home team.This is consistent with the data that 
                                    we have seen above for wins against each opponent.When matches have involved the lower rung teams, 
                                    the have been marginally in favour of home teams, although no clear pattern emerge for specific 
                                    opponents.Some of the outliers do not reflect anything meaningful since these 
                                    teams have played very few games over the past 10 years"),
                                   plotlyOutput("odds_by_opponent")
                            )
                    ),
                    fluidRow(column(6,
                                    br(),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    box(width = 100,
                                        p("Another aspect that can have a significant impact on the outcome of a match is the match
                                          referee. The data for the top referees in the league during the last decade has some interesting
                                          observations. Referees Mike Dean and Michael Oliver have officiated in the maximum number of 
                                          drawn matches whereas the likes of Anthony Taylor lead the chart when it comes to wins by away teams"),
                                        p("When it comes to bookings, Mike Dean stands out for awarding the maximum number of red cards to away teams,
                                          which in most cases is a game changer.On the other hand Kevin Friend is least likely to pull out a red"),
                                        p("Dean has also shown the maximum number of yellow cards to home and away teams, making him an important factor that
                                          can potentially change the outcome of a game, even with an overwhelming favourite")
                                    )
                            ),
                            column(6,
                                   h4(tags$b("Match results by Referee")),
                                   h5(tags$b("The top 10 referees with the maximum number of matches")),
                                   plotlyOutput("results_by_referee")        
                            )

                    ),
                    fluidRow(column(6,
                                    h4(tags$b("Win odds for home and away teams by Referee")),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    br(),
                                    
                                    plotlyOutput("odds_by_ref")
                            ),
                            column(6,
                                   h4(tags$b("Cards by Referee")),
                                   radioButtons("card_selector", "Select a card:",
                                                choices = c('Yellow', 'Red'), 
                                                selected = "Yellow", inline = TRUE),                                    
                                   plotlyOutput("bkngs_by_referee")
                            )
                    )
            ),
            
            tabItem(tabName = "raw-data",
                    tabsetPanel(type = "tabs",
                                tabPanel("Master Table",
                                         fluidRow(box(DT::dataTableOutput('tab_master_data'),width = 12))       
                                ),
                                tabPanel("League Table",
                                         fluidRow(box(DT::dataTableOutput('tab_league_data'),width = 12))       
                                )
                    )
            )
        )
    )
))