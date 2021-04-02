library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)

PL_10 <- read.csv('PL_10.csv')
Full_PL_10 <- read.csv('Full_PL_10.csv')
PLXG.Model <- readRDS('PLXGModel.RData')

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = 'PLXG',
    dropdownMenu(
      type = 'messages',
      badgeStatus = NULL,
      icon = icon('address-card'),
      headerText = 'About this Shiny application:',
      messageItem(
        from = 'Author',
        message = helpText('Matthew Morgan'),
        time = '4/1/2021',
        href = 'https://github.com/mrmorgan17'
      ),
      messageItem(
        from = 'Data Source',
        message = helpText('FBref.com'),
        icon = icon('database'),
        href = 'https://fbref.com/en'
      ),
      messageItem(
        from = 'R Packages',
        message = div(
          helpText('caret, caTools, ggplot2, rvest,'),
          helpText('shiny, shinydashboard, shinyWidgets,'),
          helpText('tidyverse, vroom')
        ), 
        icon = icon('box')
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'introduction', icon = icon('info')),
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
      tabItem(
        tabName = 'introduction',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        fluidRow(
          column(1),
          box(
            title = 'Goal',
            width = 10,
            collapsible = TRUE,
            p(em('To predict Expected Goals (XG) per match for Premier League teams'))
          )
        ),
        fluidRow(
          column(1),
          box(
            title = 'Data Glossary',
            width = 10,
            collapsible = TRUE,
            p(strong('Team:'), 'A Premier League team', em('(2017-2020)')),
            p(strong('SoT:'), 'Shots on target'),
            p(strong('Opp_Saves:'), em('Opposing team'), 'goalkeeper saves'),
            p(strong('PKatt:'), 'Penalty kicks attempted'),
            p(strong('SCA:'), 'Live-ball passes, dead-ball passes, successful dribbles, shots, fouls drawn, and defensive actions that lead to a shot attempt'),
            p(strong('Short_Cmp:'), 'Passes completed between 5 and 15 yards'),
            p(strong('TB:'), 'Completed passes sent between back defenders into open space'),
            p(strong('Dead:'), 'Dead-ball passes', em('(Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks)')),
            p(strong('Clr:'), em('Opposing team'), 'clearances'),
            p(strong('Dist:'), 'Average distance, in yards, from goal of all shots taken', em('(Does not include penalty kicks)')),
            p(strong('TklW:'), 'Tackles in which the', em('opposing team'), 'won possession of the ball')
          )
        ),
        fluidRow(
          column(1),
          box(
            title = 'Web Scraping',
            width = 10,
            collapsible = TRUE,
            collapsed = TRUE,
            p('Match data for each team was web scraped for the following Premier League campaigns:'),
            div(
              p(em('2017-2018')),
              p(em('2018-2019')),
              p(em('2019-2020')),
              style = 'padding-left: 2em;'
            ),
            p('Code for how web scraping was done is available ', a('here', href = 'https://github.com/mrmorgan17/PLXG/blob/main/FBref_scraper.R'), 'on my GitHub profile'),
            p('The dataset created from the web scraping is available ', a('here', href = 'https://github.com/mrmorgan17/PLXG/blob/main/Pl_team_match_data.csv'), 'on my GitHub profile as a', em('.csv'), 'file')
          )
        ),
        fluidRow(
          column(1),
          box(
            title = 'Modeling',
            width = 10,
            collapsible = TRUE,
            collapsed = TRUE,
            p('The dataset was used to build various models in an effort to predict the number of goals that would be scored by a particular team in any given match'),
            p('Models were trained to minimize the Root Mean Square Error', strong('RMSE'), 'which in this case is:'),
            withMathJax(),
            p('$$\\mathrm{RMSE}=\\sqrt{\\frac{\\sum_{i=1}^{N}\\left(\\mathrm{Actual\\,Goals}_{i} - \\mathrm{Predicted\\,Goals}_{i}\\right)^{2}}N}$$'),
            p('The best model', em('and the one used in this application'), 'was an Extreme Gradient Boosted', strong('XGBoost'), 'model, it had an RMSE of approximately', strong('.3')),
            p('The specifics of the XGBoost model along with the other models created are in this', a('R script', href = 'https://github.com/mrmorgan17/PLXG/blob/main/PLXG_modeling.R'), 'on my GitHub profile'),
            p('The best XGBoost model was built using the 10 most important variables'), 
            p('These 10 variables were identified from an XGBoost model where all possible variables were used')
          )
        )
      ),
      tabItem(
        tabName = 'example',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        fluidRow(
          column(1),
          box(
            title = 'Goal',
            width = 10,
            collapsible = TRUE,
            p(em('To walkthrough how to get a team\'s predicted XG for a ceratin match'))
          )
        ),
        fluidRow(
          column(1),
          box(
            title = 'How to Find Specific Match Data on FBref',
            width = 10,
            collapsible = TRUE,
            p('Start on the', a('FBref homepage', href = 'https://fbref.com/en')),
            div(
              p('Find the', strong('Competitions'), 'tab and select ', strong('English Premier League')),
              p('Select a', strong('Team'), 'from the', strong('League Table')),
              p('Hover over the', strong('Match Logs'), 'tab and then select the', strong('Shooting'), 'link under the', strong('Match Logs (Premier League)'), 'header'),
              p('Select the', strong('Date'), 'of the match'),
              style = 'padding-left: 2em;'
            ),
            p(em('For this example, Manchester City\'s match against Chelsea on 1/3/2021 will be analyzed')),
            p(em(a('Link', href = 'https://fbref.com/en/matches/85507602/Chelsea-Manchester-City-January-3-2021-Premier-League'), 'to the FBref match page')),
          ),
        ),
        fluidRow(
          column(1),
          tabBox(
            width = 10,
            tabPanel(
              'Team',
              p(strong('Team'), 'is selected in the drop-down menu in the', strong('Calculate'), 'tab of this application'),
              div(
                p(em('Team = Manchester-City')),
                style = 'padding-left: 2em;'
              ),
              p(strong('Note:'), em('All other values are on the FBref match page'), align = 'right')
            ),
            tabPanel(
              'SoT',
              p(strong('SoT'), 'is in the last row of the', strong('SoT'), 'column in the', strong('Summary'), 'tab of the', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('SoT = 6')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'Opp_Saves',
              p(strong('Opp_Saves'), 'is in the', strong('Saves'), 'column in the', strong('Chelsea Goalkeeper Stats'), 'table'),
              div(
                p(em('Opp_Saves = 3')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'PKatt',
              p(strong('PKatt'), 'is in the last row of the', strong('PKatt'), 'column in the', strong('Summary'), 'tab of the', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('PKatt= 0')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'SCA',
              p(strong('SCA'), 'is in the last row of the', strong('SCA'), 'column in the', strong('Summary'), 'tab of the', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('SCA = 32')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'Short_Cmp',
              p(strong('Short_Cmp'), 'is in the last row of the', strong('Cmp'), 'column in the', strong('Short'), 'section of the', strong('Passing'), 'tab of the ', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('Short_Cmp = 256')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'TB',
              p(strong('TB'), 'is in the last row of the', strong('TB'), 'column in the', strong('Pass Types'), 'tab of the', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('TB = 1')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'Dead',
              p(strong('Dead'), 'is in the last row of the', strong('Dead'), 'column in the', strong('Pass Types'), 'tab of the', strong('Manchester City Player Stats'), 'table'),
              div(
                p(em('Dead = 43')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'Clr',
              p(strong('Clr'), 'is in the last row of the', strong('Clr'), 'column in the', strong('Defensive Actions'), 'tab of the', strong('Chelsea Player Stats'), 'table'),
              div(
                p(em('Clr = 7')),
                style = 'padding-left: 2em;'
              )
            ),
            tabPanel(
              'Dist',
              p(strong('Dist'), 'is in the ', strong('Dist'), 'column for the row of the date of the selected match in the', strong('Shooting'), 'tab of the', strong('Manchester City Match Logs'), 'table'),
              div(
                p(em('Dist = 14.6')),
                style = 'padding-left: 2em;'
              ),
              p(strong('Note:'), em('This table is NOT on the match page, it is in the'), strong('Match Logs (Premier League)'), em('tab for the specific team on FBref'), align = 'right'),
              p(em('The'), strong('Dist'), em('column is specifically in the'), strong('Shooting'), em('table within the'), strong('Match Logs (Premier League)'), em('tab'), align = 'right'),
              p(em('Return to the page just before the'), strong('Date'), em('of the match was selected'), align = 'right'),
              p(em(a('Link', href = 'https://fbref.com/en/squads/b8fd03ef/2020-2021/matchlogs/s10728/shooting/Manchester-City-Match-Logs-Premier-League'), 'to that table for Machester City\'s match against Chelsea on 1/3/2021'), align = 'right')
            ),
            tabPanel(
              'TklW',
              p(strong('TklW'), 'is in the last row of the', strong('TklW'), 'column in the', strong('Defensive Actions'), 'tab of the', strong('Chelsea Player Stats'), 'table'),
              div(
                p(em('TklW = 8')),
                style = 'padding-left: 2em;'
              )
            )
          )
        ),
        fluidRow(
          column(1),
          box(
            title = 'Predict XG',
            width = 10,
            collapsible = TRUE,
            p('Plug all values into the', strong('XG Variables'), 'section in the', strong('Calculate'), 'tab'),
            p('Click the', strong('Calculate XG'), 'button to get an XG prediction'),
            p('For this match against Chelsea:'),
            div(
              p('Manchester City had an XG prediction of 2.95 goals'),
              p('Manchester City actually scored 3 goals'),
              style = 'padding-left: 2em;')
          )
        )
      ),
      tabItem(
        tabName = 'calculate',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        fluidRow(
          box(
            width = 4,
            selectInput('Team', 'Team:',
                        choices = c('', unique(PL_10$Team)),
                        selected = ''),
            conditionalPanel(
              condition = "input.Team != ''",
              p('The 10 variables to the right are used by the XGBoost model to predict XG'),
              div(id = 'container', p('Shown are the average values of the 10 variables for'), strong(textOutput('TeamCopy2'))),
              br(),
              p('An XG prediction is instantly calculated given these averages'),
              p('Other values for each of the 10 variables may be entered to calculate a new XG prediction')
            )
          ),
          conditionalPanel(
            condition = "input.Team != ''",
            box(
              title = 'XG Variables',
              width = 4,
              column(6,
                numericInput('SoT', 'SoT:', value = 0, min = 0, max = 100, step = .01),
                numericInput('Opp_Saves', 'Opp_Saves:',  value = 0, min = 0, max = 100, step = .01),
                numericInput('PKatt', 'PKatt:', value = 0, min = 0, max = 100, step = .01),
                numericInput('SCA_Total', 'SCA:', value = 0, min = 0, max = 100, step = .01),
                numericInput('Short_Cmp', 'Short_Cmp:', value = 0, min = 0, max = 1000, step = .01)
              ),
              column(6,
                numericInput('TB', 'TB:', value = 0, min = 0, max = 100, step = .01),
                numericInput('Dead', 'Dead:', value = 0, min = 0, max = 100, step = .01),
                numericInput('Clr', 'Clr:', value = 0, min = 0, max = 100, step = .01),
                numericInput('Dist', 'Dist:', value = 0, min = 0, max = 100, step = .01),
                numericInput('TklW', 'TklW:', value = 0, min = 0,  max = 100, step = .01)
              )
            )
          ),
          conditionalPanel(
            condition = "input.Team != ''",
            box(
              width = 4,
              align = 'center',
              div(id = 'container', p('Click the button to calculate the a new XG prediction for'), strong(textOutput('Team'))),
              br(),
              actionBttn(inputId = 'calculateButton', label = 'Calculate XG', color = 'default', style = 'fill'),
              br(),
              br(),
              div(id = 'container', p('Click the button to reset variables for'), strong(textOutput('TeamCopy')), p('back to their initial values')),
              br(),
              actionBttn(inputId = 'resetButton', label = 'Reset', color = 'default', style = 'fill'),
              br()
            )
          )
        ),
        br(),
        fluidRow(
          conditionalPanel(
            condition = "input.Team != ''",
            infoBoxOutput('XGBox'),
            infoBoxOutput('CalculatedXGBox'),
            infoBoxOutput('DiffXGBox')
          )
        )
      ),
      tabItem(
        tabName = 'visualization',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        fluidRow(
          box(
            width = 3,
            selectInput('PlotTeam', 'Team:',
                        choices = c('', unique(sort(Full_PL_10$Team)))),
            conditionalPanel(
              condition = "input.PlotTeam != ''",
              selectInput('Variable', 'Variable:',
                          choices = c('', 'Goals', 'SoT', 'Opp_Saves', 'PKatt', 'SCA_Total', 'Short_Cmp', 'TB', 'Dead', 'Clr', 'Dist', 'TklW')),
            ),
            conditionalPanel(
              condition = "input.PlotTeam != '' & input.Variable != ''",
              sliderInput('nBins', 'Number of Bins', value = 5, min = 5, max = 30, step = 5, ticks = FALSE)
            )
            # conditionalPanel(
            #   condition = "input.PlotTeam != '' & input.Variable != '' & input.nBins >= 5",
            #   actionBttn(inputId = 'plotButton', label = 'Plot', color = 'default', style = 'fill')
            # )
            # input.plotButton != 0
          ),
          conditionalPanel(
            condition = "input.PlotTeam != '' & input.Variable != ''",
            box(
              title = div(id = 'container', p('Histogram & Density Plot of'), strong(textOutput('Variable')), p('for'), strong(textOutput('PlotTeam'))),
              background = 'light-blue',
              width = 9,
              plotOutput('dataPlot')
            )
          )
        ),
        br(),
        fluidRow(
          conditionalPanel(
            condition = "input.PlotTeam != '' & input.Variable != ''",
            infoBoxOutput('AvgBox'),
            infoBoxOutput('LeagueAvgBox'),
            infoBoxOutput('DiffAvgBox')
          )
        )
      )
    )  
  )
)  

# Define server
server <- function(input, output, session) {
  
  output$Team <- renderText(input$Team)
  output$TeamCopy <- renderText(input$Team)
  output$TeamCopy2 <- renderText(input$Team)
  
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
             round(predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), 
                   digits = 2)
      ),
      subtitle = input$Team,
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  selectedValues <- eventReactive(input$calculateButton, {
    data.frame(
      Team = input$Team, 
      SoT = input$SoT, 
      Opp_Saves = input$Opp_Saves, 
      PKatt = input$PKatt, 
      SCA_Total = input$SCA_Total, 
      Short_Cmp = input$Short_Cmp, 
      TB = input$TB, 
      Dead = input$Dead, 
      Clr = input$Clr, 
      Dist = input$Dist, 
      TklW = input$TklW
    ) 
  })
  
  output$CalculatedXGBox <- renderInfoBox({
    infoBox(
      'Calculated XG', 
      ifelse(class(try(round(predict(PLXG.Model, selectedValues()), digits = 2), silent = TRUE)) == 'try-error', 
             0, 
             round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())), 
                   digits = 2)
      ),
      subtitle = input$Team,
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
             round(ifelse(predict(PLXG.Model, selectedValues()) < 0, 0, predict(PLXG.Model, selectedValues())) - predict(PLXG.Model, PL_10 %>% filter(Team == input$Team)), digits = 2)),
      subtitle = em('Calculated XG - Average XG'),
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
    ggplot(data.frame(x = Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)), aes(x)) + 
      geom_histogram(aes(y = ..density..), 
                     bins = input$nBins,
                     color = 'black', 
                     fill = '#a9daff') +
      stat_function(fun = dnorm,
                    args = list(
                      mean = mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)),
                      sd = sd(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable))
                    ),
                    col = '#317196',
                    size = 2) +
      xlab(input$Variable) +
      ylab('Density')
  })
  
  output$PlotTeam <- renderText(input$PlotTeam)
  output$Variable <- renderText(input$Variable)
  
  state <- reactiveValues()
  
  observe({
    state$x <- input$nBins
  })
  
  output$pastnBins <- renderText(state$x)
  
  output$AvgBox <- renderInfoBox({
    infoBox(
      paste(input$Variable, 'Average'), 
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)), 
                   digits = 2)
      ),
      subtitle = input$PlotTeam,
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$LeagueAvgBox <- renderInfoBox({
    infoBox(
      'League Average', 
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% pull(input$Variable)), 
                   digits = 2)
      ),
      subtitle = input$Variable,
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$DiffAvgBox <- renderInfoBox({
    infoBox(
      paste(input$Variable, 'Difference'),
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), 
                   digits = 2)
      ),
      subtitle = em('Team Average - League Average'),
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