library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)

PL_10 <- read.csv('PL_10.csv')
Full_PL_10 <- read.csv('Full_PL_10.csv')
PLXG.Model <- readRDS('PLXGModel.RData')

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = 'PLXG'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('About', tabName = 'about', icon = icon('info')),
      menuItem('Example', tabName = 'example', icon = icon('futbol')),
      menuItem('Calculate', tabName = 'calculate', icon = icon('calculator')),
      menuItem('Visualization', tabName = 'visualization', icon = icon('chart-area'))
    )
  ),
  dashboardBody(
    tags$head(tags$style("
                        #container * {
         display: inline;
                           }")),
    tabItems(
      tabItem(tabName = 'about',
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
              p('Models were trained to minimize the Root Mean Square Error', strong('RMSE'), 'which in this case is:'),
              withMathJax(),
              p('$$\\mathrm{RMSE}=\\sqrt{\\frac{\\sum_{i=1}^{N}\\left(\\mathrm{Actual\\,Goals}_{i} - \\mathrm{Predicted\\,Goals}_{i}\\right)^{2}}N}$$'),
              p('The best model and the one used in this app is an eXtreme Gradient Boosted ', strong('XGB'), 'model. This model had RMSE values of approximately .3'),
              p('The specifics of the XGB model along with the other models created are in this ', a('R script', href = 'https://github.com/mrmorgan17/PLXG/blob/main/PLXG_modeling.R'), 'on my GitHub profile'),
              p('The best XGB model was built using these 10 variables:'),
              div(p(strong('SoT:'), 'Shots on target'),
                  p(strong('Opp_Saves:'), 'Opposing goalkeeper saves'),
                  p(strong('PKatt:'), 'Penalty kicks attempted'),
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
                  p(strong('R Packages:'), 'caret, caTools, ggplot2, rvest, shiny, shinydashboard, shinyWidgets, tidyverse, vroom'),
                  p(strong('Sources:'), a('FBref.com', href = 'https://fbref.com/en'), 'for data'),
                  style = 'text-align: right;')
      ),
      tabItem(tabName = 'example',
              titlePanel('Premier League Expected Goals (PLXG)'),
              p('This is a walkthrough of how to get an expected goals prediction for a specific Premier League match.'),
              p('For this example, we will look at Manchester City in their match against Chelsea on January 3rd 2021'),
              p('To get to specific match pages on FBref from the ', a('homepage', href = 'https://fbref.com/en')),
              div(p('Go to the ', strong('Competitions'), 'tab and select ', strong('English Premier League.')),
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
              p('Now, the values can be entered in the', strong('Calculate'), 'tab to get an updated XG prediction'),
              p('For this match against Chelsea, Manchester City had an updated XG value of 2.95'),
              p('For this match against Chelsea, Manchester City actually scored 3 goals')
      ),
      tabItem(tabName = 'calculate',
              titlePanel('Premier League Expected Goals (PLXG)'),
              fluidRow(
                column(4,
                       selectInput('Team', 'Team:',
                                   choices = c('', unique(PL_10$Team)),
                                   selected = ''),
                       p('The 10 variables that appear are the ones used by the model to predict XG for a team'),
                       p('Initially shown are the average values of the 10 variables for the selected team'),
                       p('An average XG prediction is also calculated'),
                       p('The user may enter their own inputs for each variable to calculate an updated XG prediction for the team')),
                column(2,
                       conditionalPanel(condition = "input.Team != ''",
                                        numericInput('SoT', 'SoT:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('Opp_Saves', 'Opp_Saves:',  value = 0, min = 0, max = 100, step = .01),
                                        numericInput('PKatt', 'PKatt:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('SCA_Total', 'SCA:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('Short_Cmp', 'Short_Cmp:', value = 0, min = 0, max = 1000, step = .01)
                                        )),
                column(2,
                       conditionalPanel(condition = "input.Team != ''",
                                        numericInput('TB', 'TB:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('Dead', 'Dead:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('Clr', 'Clr:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('Dist', 'Dist:', value = 0, min = 0, max = 100, step = .01),
                                        numericInput('TklW', 'TklW:', value = 0, min = 0,  max = 100, step = .01)
                                        )),
                column(4,
                       align = 'center',
                       p('Click the button to update the team\'s XG prediction'),
                       actionBttn(inputId = 'updateButton', label = 'Update XG', color = 'default', style = 'fill'),
                       br(),
                       br(),
                       p('Click the button to reset the variables of the selected team back to their initial average values'),
                       actionBttn(inputId = 'resetButton', label = 'Reset', color = 'default', style = 'fill'))
                ),
              br(),
              fluidRow(
                conditionalPanel(
                  condition = "input.Team != ''",
                  infoBoxOutput('XGBox'),
                  infoBoxOutput('UpdatedXGBox'),
                  infoBoxOutput('DiffXGBox')
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.Team != ''",
                  column(4,
                         align = 'center',     
                         p('Average Expected Goals prediction for the selected team')
                  ),
                  column(4,
                         align = 'center',     
                         p('Updated Expected Goals prediction for the selected team')
                  ),
                  column(4,
                         align = 'center',     
                         p('Difference in Expected Goals'),
                         p(em('Updated XG - Average XG'))
                  )
                )
              )
      ),
      tabItem(tabName = 'visualization',
              titlePanel('Premier League Expected Goals (PLXG)'),
              fluidRow(
                column(3,
                       selectInput('PlotTeam', 'Team:',
                                   choices = c('', unique(sort(Full_PL_10$Team)))),
                       br(),
                       selectInput('Variable', 'Variable:',
                                   choices = c('', 'Goals', 'SoT', 'Opp_Saves', 'PKatt', 'SCA_Total', 'Short_Cmp', 'TB', 'Dead', 'Clr', 'Dist', 'TklW')),
                       br(),
                       sliderInput('nBins', 'Number of Bins', value = 10, min = 0, max = 50, step = 5, ticks = FALSE),
                       br()),
                # actionBttn(inputId = 'plotButton', label = 'Plot', color = 'primary', style = 'fill')),
                column(9,
                       plotOutput('dataPlot'))
                ),
              br(),
              fluidRow(
                conditionalPanel(
                  condition = "input.PlotTeam != '' & input.Variable != ''",
                  infoBoxOutput('AvgBox'),
                  infoBoxOutput('LeagueAvgBox'),
                  infoBoxOutput('DiffAvgBox')
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.PlotTeam != '' & input.Variable != ''",
                  column(4,
                    align = 'center',
                    div(id = 'container', strong(textOutput('Variable')), p('average for'), strong(textOutput('PlotTeam')))
                  ),
                  column(4,
                    align = 'center',
                    div(id = 'container', strong(textOutput('VariableCopy')), p('average across all Premier League Teams'))
                  ),
                  column(4,
                    align = 'center',
                    p(em('Team Average - League Average'))
                  )
                )
              ),
              br(),
              fluidRow(
                column(12,
                  div(p('Plots are created with Premier League data from the following campaigns:'),
                      p('2017-2018'),
                      p('2018-2019'),
                      p('2019-2020'),
                      style = 'text-align: right;')
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  observeEvent(input$Team, {
    updateNumericInput(session, 'SoT', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(SoT), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Opp_Saves', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Opp_Saves), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'PKatt', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(PKatt), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'SCA_Total', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(SCA_Total), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Short_Cmp', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Short_Cmp), digits = 2), min = 0, max = 1000)
    updateNumericInput(session, 'TB', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(TB), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dead', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Dead), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Clr', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Clr), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dist', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Dist), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'TklW', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(TklW), digits = 2), min = 0, max = 100)
  })
  
  output$XGBox <- renderInfoBox({
    infoBox(
      'Average XG', 
      ifelse(class(try(round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2), silent = TRUE)) == 'try-error', 
             0, 
             round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2)),
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  selectedValues <- eventReactive(input$updateButton, {
    data.frame(Team = input$Team, 
               SoT = input$SoT, 
               Opp_Saves = input$Opp_Saves, 
               PKatt = input$PKatt, 
               SCA_Total = input$SCA_Total, 
               Short_Cmp = input$Short_Cmp, 
               TB = input$TB, 
               Dead = input$Dead, 
               Clr = input$Clr, 
               Dist = input$Dist, 
               TklW = input$TklW) 
  })
  
  output$UpdatedXGBox <- renderInfoBox({
    infoBox(
      'Updated XG', 
      ifelse(class(try(round(predict(PLXG.Model, selectedValues()), digits = 2), silent = TRUE)) == 'try-error', 
             0, 
             round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), 
                   digits = 2)
      ),
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$DiffXGBox <- renderInfoBox({
    infoBox(
      'XG Difference', 
      ifelse(class(try(round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2), silent = TRUE)) == 'try-error',
             0,
             round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2)),
      icon = icon('futbol'),
      color = if (class(try(round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2), silent = TRUE)) == 'try-error') {
        'black'
      } else if (round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2) < 0) {
        'red'
      } else if (round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), digits = 2) - round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2) > 0) {
        'green'
      } else {
        'black'
      },
      fill = TRUE
    )
  })
  
  observeEvent(input$resetButton, {
    updateNumericInput(session, 'SoT', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(SoT), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Opp_Saves', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Opp_Saves), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'PKatt', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(PKatt), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'SCA_Total', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(SCA_Total), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Short_Cmp', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Short_Cmp), digits = 2), min = 0, max = 1000)
    updateNumericInput(session, 'TB', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(TB), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dead', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Dead), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Clr', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Clr), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dist', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(Dist), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'TklW', value = round(PL_10 %>% filter(Team == input$Team) %>% pull(TklW), digits = 2), min = 0, max = 100)
  })
  
  output$dataPlot <- renderPlot({
    # input$plotButton 
    # isolate(
      ggplot(data.frame(x = Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)), aes(x)) + 
        geom_histogram(aes(y = ..density..), 
                       bins = input$nBins,
                       color = 'black', 
                       fill = '#a9daff') +
        stat_function(fun = dnorm,
                      args = list(mean = mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)),
                                  sd = sd(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable))),
                      col = '#f04848',
                      size = 2) +
        xlab(input$Variable) +
        ylab('Density')
    # )
  })
  
  output$PlotTeam <- renderText({
    input$plotButton
    isolate(
      ifelse(input$PlotTeam == '', 'No Team Selected', input$PlotTeam)
    )
  })
  
  output$Variable <- renderText({
    ifelse(input$Variable == '', 'No Variable Selected', input$Variable)
  })
  
  output$VariableCopy <- renderText({
    ifelse(input$Variable == '', 'No Variable Selected', input$Variable)
  })
  
  output$AvgVariable <- renderText({
    ifelse(input$PlotTeam == '' | input$Variable == '', 0, round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)), digits = 2))
  })
  
  output$AvgBox <- renderInfoBox({
    infoBox(
      'Team Average', 
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)), digits = 2)),
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$LeagueAvgVariable <- renderText({
    ifelse(input$PlotTeam == '' | input$Variable == '', 0, round(mean(Full_PL_10 %>% pull(input$Variable)), digits = 2))
  })
  
  output$LeagueAvgBox <- renderInfoBox({
    infoBox(
      'League Average', 
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% pull(input$Variable)), digits = 2)),
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$DiffAvgBox <- renderInfoBox({
    infoBox(
      'Difference',
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), digits = 2)),
      icon = icon('futbol'),
      color = if (input$PlotTeam == '' | input$Variable == '') {
        'black'
      } else if (round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), digits = 2) < 0) {
        'red'
      } else if (round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), digits = 2) > 0) {
        'green'
      } else {
        'black'
      },
      fill = TRUE
    )
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)