library(shiny)

PL_10 <- read.csv('PL_10.csv')
PLXG.Model <- readRDS('PLXGModel.RData')

# Define UI for slider demo app ----
ui <- fluidPage(
  
  navbarPage('PLXG',
             tabPanel('About',
                      titlePanel('Premier League Expected Goals (PLXG)'),
                      p('This application estimates eXpected Goals (XG) values per match for each Premier League team'),
                      p('Average XG values for each team have already been calculated.'),
                      p('Users can input their own values to see how XG values would update for each team.'),
                      br(),
                      p('Match data for each team was web scraped for the following Premier League campaigns:'),
                      div(p('2017-2018'),
                          p('2018-2019'),
                          p('2019-2020'),
                          style = 'padding-left: 2em;'),
                      p('Code for how web scraping was done is available ', a('here', href = 'https://github.com/mrmorgan17/PLXG/blob/main/FBref_scraper.R'), 'on my GitHub profile'),
                      p('The dataset created from the web scraping is available ', a('here', href = 'https://github.com/mrmorgan17/PLXG/blob/main/Pl_team_match_data.csv'), 'on my GitHub profile as a .csv file'),
                      br(),
                      p('This dataset was then used to build various models in an effort to predict the number of goals that would be scored by a particular team in any given match.'),
                      p('Models were trained to minimize the Root Mean Square Error', strong('RMSE'), 'which is:'),
                      withMathJax(),
                      helpText('$$\\mathrm{RMSE}=\\sqrt{\\frac{\\sum_{i=1}^{N}\\left(\\mathrm{Actual}_{i} - \\mathrm{Predicted}_{i}\\right)^{2}}N}$$'),
                      p('The best model and the one used in this app is an eXtreme Gradient Boosted ', strong('XGB'), 'model. This model had RMSE values of approximately .3'),
                      p('The specifics of the XGB model along with the other models created are in this ', a('R script', href = 'https://github.com/mrmorgan17/PLXG/blob/main/PLXG_modeling.R'), 'on my GitHub profile'),
                      p('The best XGB model was built using these 10 variables:'), 
                      div(p(strong('SoT:'), 'Shots on target'),
                          p(strong('Opp_Saves:'), 'Opposing goalkeeper saves'),
                          p(strong('PKatt:'), 'Penalty Kicks Attempted'),
                          p(strong('SCA:'), 'The two offensive actions directly leading to a shot, such as passes, dribbles and drawing fouls.'),
                          p(strong('Short_Cmp:'), 'Passes completed between 5 and 15 yards'),
                          p(strong('TB:'), 'Completed passes sent between back defenders into open space'),
                          p(strong('Dead:'), 'Dead-ball passes (Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks)'),
                          p(strong('Clr:'), 'Opposing team clearances'),
                          p(strong('Dist:'), 'Average distance, in yards, from goal of all shots taken (Does not include penalty kicks)'),
                          p(strong('TklW:'), 'Tackles in which the opposing team won possession of the ball'),
                          style = 'padding-left: 2em;'),
                      p('These 10 variables were the 10 most important variables identified from an XGB model where all variables were used to build it.'),
                      br(),
                      div(p(strong('Built by'), a('Matthew Morgan', href = 'https://github.com/mrmorgan17'), 'using RStudio and Shiny'), 
                          p(strong('R Packages:'), 'caret, caTools, rvest, shiny, tidyverse, vroom'),
                          p(strong('Sources:'), a('FBref.com', href = 'https://fbref.com/en'), 'for data'),
                          style = 'text-align: right;')),
             tabPanel('Example',
                      titlePanel('Premier League Expected Goals (PLXG)'),
                      p('This is a walkthrough of how to update expected goals using data from a specific match.'),
                      p('For this example, we will look at Manchester City in their match against Chelsea on January 3rd 2021'),
                      div(p('To get to specific match pages on FBref from the homepage, go to the ', strong('Competitions'), 'tab and select ', strong('English Premier League.')),
                          p('Then, select a team from the ', strong('League Table')),
                          p('From there, select the ', strong('Match Logs'), 'tab and then select the ', strong('Scores & Fixtures'), 'link for the', strong('Premier League')),
                          p('Finally, select the date of the match you are interested in'),
                          style = 'padding-left: 2em;'),
                      p(a('Here', href = 'https://fbref.com/en/matches/85507602/Chelsea-Manchester-City-January-3-2021-Premier-League'), 'is the FBref page for that match'),
                      br(),
                      p('First, we need ', strong('Team'), 'which we can get from the drop-down menu for Team in the ', strong('Calculate'), 'tab of this app'),
                      div(p(strong('Team'), '= Manchester-City'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('SoT'), 'which we can get from looking at the last row of the ', strong('SoT'), 'column in the', strong('Summary'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('SoT'), '= 6'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('Opp_Saves'), 'which we can get from looking at the ', strong('Saves'), 'column in the', strong('Chelsea Goalkeeper Stats'), 'table'),
                      div(p(strong('Opp_Saves'), '= 3'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('PKatt'), 'which we can get from looking at the last row of the ', strong('PKatt'), 'column in the', strong('Summary'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('PKatt'), '= 0'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('SCA'), 'which we can get from looking at the last row of the ', strong('SCA'), 'column in the', strong('Summary'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('SCA'), '= 32'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('Short_Cmp'), 'which we can get from looking at the last row of the ', strong('Cmp'), 'column in the', strong('Short'), 'section of the', strong('Passing'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('Short_Cmp'), '= 256'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('TB'), 'which we can get from looking at the last row of the ', strong('TB'), 'column in the', strong('Pass Types'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('TB'), '= 1'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('Dead'), 'which we can get from looking at the last row of the ', strong('Dead'), 'column in the', strong('Pass Types'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
                      div(p(strong('Dead'), '= 43'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('Clr'), 'which we can get from looking at the last row of the ', strong('Clr'), 'column in the', strong('Defensive Actions'), 'tab of the ', strong('Chelsea Player Stats'), 'table'),
                      div(p(strong('Clr'), '= 7'),
                          style = 'padding-left: 2em;'),
                      p('Next, we need ', strong('Dist'), 'which we can get from looking at the ', strong('Dist'), 'column for the row of the date of the selected match in the', strong('Shooting'), 'tab of the ', strong('Manchester City Match Logs'), 'table'),
                      div(p(strong('Dist'), '= 14.6'),
                          p('This table is NOT on the match page, it is found after you select the Match Logs tab for a specific team.', a('Here', href = 'https://fbref.com/en/squads/b8fd03ef/2020-2021/matchlogs/s10728/shooting/Manchester-City-Match-Logs-Premier-League'), 'is the link to that table for Machester City.'),
                          style = 'padding-left: 2em;'),
                      p('Finally, we need ', strong('TklW'), 'which we can get from looking at the last row of the ', strong('TklW'), 'column in the', strong('Defensive Actions'), 'tab of the ', strong('Chelsea Player Stats'), 'table'),
                      div(p(strong('TklW'), '= 8'),
                          style = 'padding-left: 2em;'),
                      br(), 
                      p('Now, the values can be entered in the', strong('Calculate'), 'tab to get an updated XG value'),
                      p('For this match against Chelsea, Manchester City had an updated XG value of 2.91'),
                      p('For this match against Chelsea, Manchester City actually scored 3 goals')),
             tabPanel('Calculate',
                      # App title ----
                      titlePanel('Premier League Expected Goals (PLXG)'),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar to select input options for each variable in the model ----
                        sidebarPanel(
                          
                          # Select Premier League Team
                          selectInput('Team', 'Team:', 
                                      choices = unique(PL_10$Team)),
                          
                          # Input: Simple integer interval ----
                          numericInput('SoT', 'SoT:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Opp_Saves', 'Opp_Saves:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('PKatt', 'PKatt:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('SCA_Total', 'SCA:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Short_Cmp', 'Short_Cmp:', 
                                       0, 
                                       min = 0, 
                                       max = 1000),
                          
                          numericInput('TB', 'TB:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Dead', 'Dead:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                
                          numericInput('Clr', 'Clr:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Dist', 'Dist:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('TklW', 'TklW:', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          submitButton('Update XG', icon('refresh'))
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Table summarizing the values entered ----
                          helpText('Table of team averages for each of the 10 most important variables used in the XGB model'),
                          
                          tableOutput('TeamAvg'),
                          
                          helpText('Average Expected Goals per Match prediction for the selected team:'),
                          
                          textOutput('XG'),
                          
                          helpText('Updated Expected Goals per Match prediction for the selected team given user-entered inputs:'),
                          
                          textOutput('UpdatedXG'),
                          
                          helpText('Difference in Expected Goals per Match prediction (Updated XG - Average XG):'),
                          
                          textOutput('DiffXG')
                          
                        )
                      )))
  
)

# Define server logic ----
server <- function(input, output) {
  
  selectedTeam <- reactive({
    
    data.frame(Variable = c('Team',
                            'SoT',
                            'Opp_Saves',
                            'PKatt',
                            'SCA',
                            'Short_Cmp',
                            'TB',
                            'Dead',
                            'Clr',
                            'Dist',
                            'TklW'),
               Value = c(PL_10 %>% filter(Team == input$Team) %>% pull(Team),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(SoT), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(Opp_Saves), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(PKatt), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(SCA_Total), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(Short_Cmp), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(TB), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(Dead), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(Clr), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(Dist), digits = 2),
                         round(PL_10 %>% filter(Team == input$Team) %>% pull(TklW), digits = 2)))
      
  })
  
  selectedValues <- reactive({
    
    data.frame(Team = PL_10 %>% filter(Team == input$Team) %>% pull(Team),
               SoT = ifelse(input$SoT != PL_10 %>% filter(Team == input$Team) %>% pull(SoT), input$SoT, PL_10 %>% filter(Team == input$Team) %>% pull(SoT)),
               Opp_Saves = ifelse(input$Opp_Saves != PL_10 %>% filter(Team == input$Team) %>% pull(Opp_Saves), input$Opp_Saves, PL_10 %>% filter(Team == input$Team) %>% pull(Opp_Saves)),
               PKatt = ifelse(input$PKatt != PL_10 %>% filter(Team == input$Team) %>% pull(PKatt), input$PKatt, PL_10 %>% filter(Team == input$Team) %>% pull(PKatt)),
               SCA_Total = ifelse(input$SCA_Total != PL_10 %>% filter(Team == input$Team) %>% pull(SCA_Total), input$SCA_Total, PL_10 %>% filter(Team == input$Team) %>% pull(SCA_Total)),
               Short_Cmp = ifelse(input$Short_Cmp != PL_10 %>% filter(Team == input$Team) %>% pull(Short_Cmp), input$Short_Cmp, PL_10 %>% filter(Team == input$Team) %>% pull(Short_Cmp)),
               TB = ifelse(input$TB != PL_10 %>% filter(Team == input$Team) %>% pull(TB), input$TB, PL_10 %>% filter(Team == input$Team) %>% pull(TB)),
               Dead = ifelse(input$Dead != PL_10 %>% filter(Team == input$Team) %>% pull(Dead), input$Dead, PL_10 %>% filter(Team == input$Team) %>% pull(Dead)),
               Clr = ifelse(input$Clr != PL_10 %>% filter(Team == input$Team) %>% pull(Clr), input$Clr, PL_10 %>% filter(Team == input$Team) %>% pull(Clr)),
               Dist = ifelse(input$Dist != PL_10 %>% filter(Team == input$Team) %>% pull(Dist), input$Dist, PL_10 %>% filter(Team == input$Team) %>% pull(Dist)),
               TklW = ifelse(input$TklW != PL_10 %>% filter(Team == input$Team) %>% pull(TklW), input$TklW, PL_10 %>% filter(Team == input$Team) %>% pull(TklW)))
    
  })
  
  # Show the average values for the selected team in an HTML table ----
  output$TeamAvg <- renderTable({
      selectedTeam()
  })
  
  output$XG <- renderText({
    round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2)
  })
  
  output$UpdatedXG <- renderText({
    round(predict(PLXG.Model, selectedValues()), digits = 2)
  })
  
  output$DiffXG <- renderText({
    round(predict(PLXG.Model, selectedValues()), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)