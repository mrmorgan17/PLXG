library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(caret)
library(ggplot2)
library(dplyr)
library(xgboost)

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
      headerText = 'About this Shiny application',
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
          helpText('caret, dplyr, ggplot2, rvest, xgboost'),
          helpText('shiny, shinydashboard, shinycssloaders,'),
          helpText('shinyjs, shinyWidgets')
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
    useShinyjs(),
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
        br(),
        fluidRow(
          column(1),
          box(
            title = p(icon('bullseye'), 'Goal'),
            width = 10,
            collapsible = TRUE,
            p(em('To predict Expected Goals (XG) per match for Premier League teams'))
          )
        ),
        fluidRow(
          column(1),
          box(
            title = p(icon('book'), 'Data Glossary'),
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
            title = p(icon('laptop-code'), 'Web Scraping'),
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
            title = p(icon('chart-line'), 'Modeling'),
            width = 10,
            collapsible = TRUE,
            collapsed = TRUE,
            p('The dataset was used to build various models in an effort to predict the number of goals that would be scored by a particular team in any given match'),
            p('Models were trained to minimize the Root Mean Square Error', strong('RMSE'), 'which in this case is:'),
            withMathJax(),
            p('$$\\mathrm{RMSE}=\\sqrt{\\frac{\\sum_{i=1}^{N}\\left(\\mathrm{Actual\\,Goals}_{i} - \\mathrm{Predicted\\,Goals}_{i}\\right)^{2}}N}$$'),
            p('The best model', em('and the one used in this Shiny application'), 'was an Extreme Gradient Boosted', strong('XGBoost'), 'model, it had an RMSE of approximately', strong('.3')),
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
        br(),
        fluidRow(
          column(1),
          box(
            title = p(icon('bullseye'), 'Goal'),
            width = 10,
            collapsible = TRUE,
            p(em('To walkthrough how to get a team\'s predicted XG for a certain match')),
            br(),
            p(icon('exclamation-triangle'), em('Only matches from the 2017-2018 Premier League campaign and onward can be used because the variables used to predict XG were not recorded')),
            div(
              em('previous to the 2017-2018 Premier League campaign'),
              style = 'padding-left: 1.3em;'  
            )
          )
        ),
        fluidRow(
          column(1),
          box(
            title = p(icon('search'), 'How to Find Specific Match Data on FBref'),
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
            p(em(a('Link', href = 'https://fbref.com/en/matches/85507602/Chelsea-Manchester-City-January-3-2021-Premier-League'), 'to the FBref match page'))
          ),
        ),
        fluidRow(
          column(1),
          tabBox(
            width = 10,
            tabPanel(
              'Team',
              p(strong('Team'), 'is selected in the drop-down menu in the', strong('Calculate'), 'tab of this Shiny application'),
              div(
                p(em('Team = Manchester-City')),
                style = 'padding-left: 2em;'
              ),
              br(),
              p(icon('exclamation-triangle'), em('All other values are on the FBref match page'))
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
              br(),
              p(icon('exclamation-triangle'), em('This table is NOT on the match page, it is on the'), strong('Match Logs (Premier League)'), em('tab for the specific team on FBref')),
              div(
                p(em('The'), strong('Dist'), em('column is specifically in the'), strong('Shooting'), em('table within the'), strong('Match Logs (Premier League)'), em('tab')),
                p(em('Return to the page just before the'), strong('Date'), em('of the match was selected')),
                p(em(a('Link', href = 'https://fbref.com/en/squads/b8fd03ef/2020-2021/matchlogs/s10728/shooting/Manchester-City-Match-Logs-Premier-League'), 'to the'), strong('Shooting'), em('table for Machester City\'s match against Chelsea on 1/3/2021')),
                style = 'padding-left: 1.3em;'
              )
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
            title = p(icon('calculator'), 'Predict XG'),
            width = 10,
            collapsible = TRUE,
            p('Plug all the values into the', strong('XG Variables'), 'section in the', strong('Calculate'), 'tab'),
            p('Click the', strong('Calculate'), 'button to get an XG prediction for the match'),
            p('For this match against Chelsea, Manchester City had an XG of', strong('2.95'), 'goals and actually scored', strong('3'), 'goals')
          )
        )
      ),
      tabItem(
        tabName = 'calculate',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        br(),
        fluidRow(
          column(
            width = 6,
            column(
              width = 12,
              conditionalPanel(
                condition = "input.Team != ''",
                dropdownButton(
                  div(id = 'container', p('The XGBoost model uses the'), strong('XG Variables'), p('to predict XG')),
                  br(),
                  div(id = 'container', p('Initially shown are the average values of the'), strong('XG Variables'), p('for'), strong(textOutput('Team3'))),
                  br(),
                  div(id = 'container', strong('Average XG'), p('is a prediction for how many goals'), strong(textOutput('Team4')), p('scores on average in all their matches')),
                  br(),
                  div(id = 'container', p('Click the'), icon('calculator'), p('button to see what the XG for'), strong(textOutput('Team5')), p('would be if they performed according to the values of the'), strong('XG Variables'), p('in a match')),
                  br(),
                  div(id = 'container', p('Values of the'), strong('XG Variables'), p('for a specific match will appear after selecting an'), strong('Opponent'), p('and a'), strong('Date'), p('for'), strong(textOutput('Team6'))),
                  br(),
                  div(id = 'container', p('Click the'), icon('history'), p('button to reset the'), strong('XG Variables'), p('back to the average values for'), strong(textOutput('Team7'))),
                  br(),
                  div(id = 'container', icon('exclamation-triangle'), strong('Average XG'), em('will not equal'), strong('Match XG'), em('for the average values of the'), strong('XG Variables')), 
                  div(
                    id = 'container',
                    em('because'), strong('Average XG'), em('is the average of every match XG prediction for'), strong(textOutput('Team8')), p('while'), strong('Match XG'), em('is an XG prediction for one match where'), strong(textOutput('Team9')), em('performs averagely'),
                    style = 'padding-left: 1.3em;'
                  ),
                  status = 'primary',
                  size = 'sm',
                  icon = icon('info'),
                  tooltip = tooltipOptions(placement = 'top', title = 'Info')
                )
              )
            ),
            column(
              width = 4,
              br(),
              br(),
              dropdownButton(
                h4(strong('Team')),
                selectInput('Team', label = NULL, choices = c('', unique(sort(Full_PL_10$Team)))),
                status = 'primary',
                size = 'lg',
                icon = icon('shield-alt'),
                tooltip = tooltipOptions(placement = 'top', title = 'Select a team'),
                right = TRUE
              ),
            ),
            column(
              width = 4,
              br(),
              br(),
              conditionalPanel(
                condition = "input.Team != ''",
                dropdownButton(
                  h4(strong('Opponent')),
                  uiOutput('select_Opponent'),
                  status = 'primary',
                  size = 'lg',
                  icon = icon('plus'),
                  tooltip = tooltipOptions(placement = 'top', title = 'Select an opponent'),
                  right = TRUE,
                  inputId = 'opponentButton'
                )
              )
            ),
            column(
              width = 4,
              br(),
              br(),
              conditionalPanel(
                condition = "input.Team != ''",
                dropdownButton(
                  h4(strong('Date')),
                  uiOutput('select_Date'),
                  status = 'primary',
                  size = 'lg',
                  icon = icon('calendar'),
                  tooltip = tooltipOptions(placement = 'top', title = 'Select a date'),
                  right = TRUE,
                  inputId = 'dateButton'
                )
              )
            ),
            column(
              br(),
              br(),
              br(),
              width = 12,
              conditionalPanel(
                condition = "input.Team != ''",
                dropdownButton(
                  div(id = 'container', p('A match XG prediction has been calculated for'), strong(textOutput('Team'))),
                  status = 'primary',
                  size = 'lg',
                  icon = icon('calculator'),
                  tooltip = tooltipOptions(placement = 'top', title = 'Calculate'),
                  right = TRUE,
                  inputId = 'calculateButton'
                )
              )
            ),
            column(
              br(),
              br(),
              br(),
              width = 12,
              conditionalPanel(
                condition = "input.Team != ''",
                dropdownButton(
                  div(id = 'container', strong('XG Variables'), p('have been reset to the averages for'), strong(textOutput('Team2'))),
                  status = 'primary',
                  size = 'lg',
                  icon = icon('history'),
                  tooltip = tooltipOptions(placement = 'top', title = 'Reset'),
                  right = TRUE,
                  inputId = 'resetButton'
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.Team != ''",
            box(
              title = strong('XG Variables'),
              width = 6,
              background = 'light-blue',
              column(
                width = 6,
                numericInput('SoT', 'SoT', value = 0, min = 0, max = 100, step = .01),
                numericInput('Opp_Saves', 'Opp_Saves',  value = 0, min = 0, max = 100, step = .01),
                numericInput('PKatt', 'PKatt', value = 0, min = 0, max = 100, step = .01),
                numericInput('SCA_Total', 'SCA', value = 0, min = 0, max = 100, step = .01),
                numericInput('Short_Cmp', 'Short_Cmp', value = 0, min = 0, max = 1000, step = .01)
              ),
              column(
                width = 6,
                numericInput('TB', 'TB', value = 0, min = 0, max = 100, step = .01),
                numericInput('Dead', 'Dead', value = 0, min = 0, max = 100, step = .01),
                numericInput('Clr', 'Clr', value = 0, min = 0, max = 100, step = .01),
                numericInput('Dist', 'Dist', value = 0, min = 0, max = 100, step = .01),
                numericInput('TklW', 'TklW', value = 0, min = 0,  max = 100, step = .01)
              )
            )
          )
        ),
        br(),
        fluidRow(
          conditionalPanel(
            condition = "input.Team != '' & input.calculateButton != 0",
            infoBoxOutput('MatchXGBox')
          ),
          conditionalPanel(
            condition = "input.Team != ''",
            infoBoxOutput('AvgXGBox')
          ),
          conditionalPanel(
            condition = "input.Team != '' & input.calculateButton != 0",
            infoBoxOutput('DiffXGBox')
          )
        )
        # fluidRow(
        #   conditionalPanel(
        #     condition = "input.Opponent != '' & input.opponentButton != 0 & input.Date != '' & input.dateButton != 0",
        #     infoBoxOutput('ActualGoalsBox')
        #   )
        # )
      ),
      tabItem(
        tabName = 'visualization',
        titlePanel(
          h1('Premier League Expected Goals (PLXG)', align = 'center')
        ),
        fluidRow(
          column(
            width = 2,
            offset = 1,
            br(),
            br(),
            br(),
            dropdownButton(
              h4(strong('Team')),
              selectInput('PlotTeam', label = NULL, 
                          choices = c('', unique(sort(Full_PL_10$Team)))),
              status = 'primary',
              size = 'lg',
              icon = icon('shield-alt'),
              tooltip = tooltipOptions(placement = 'top', title = 'Select a team'),
              right = TRUE
            ),
            br(),
            br(),
            br(),
            conditionalPanel(
              condition = "input.PlotTeam != ''",
              dropdownButton(
                h4(strong('Variable')),
                selectInput('Variable', label = NULL,
                            choices = c('', 'Goals', 'SoT', 'Opp_Saves', 'PKatt', 'SCA_Total', 'Short_Cmp', 'TB', 'Dead', 'Clr', 'Dist', 'TklW')),
                status = 'primary',
                size = 'lg',
                icon = icon('sitemap'),
                tooltip = tooltipOptions(placement = 'top', title = 'Select a variable'),
                right = TRUE
              )
            ),
            br(),
            br(),
            br(),
            conditionalPanel(
              condition = "input.PlotTeam != '' & input.Variable != ''",
              dropdownButton(
                h4(strong('Bins')),
                sliderInput('nBins', label = NULL, value = 5, min = 5, max = 30, step = 5, ticks = FALSE),
                status = 'primary',
                size = 'lg',
                icon = icon('chart-bar'),
                tooltip = tooltipOptions(placement = 'top', title = 'Adjust the number of bins'),
                right = TRUE
              )
            )
          ),
          br(),
          conditionalPanel(
            condition = "input.PlotTeam != '' & input.Variable != ''",
            box(
              title = div(id = 'container', p('Histogram and Density Plot of'), strong(textOutput('Variable')), p('for'), strong(textOutput('PlotTeam'))),
              background = 'light-blue',
              width = 9,
              plotOutput('dataPlot') %>% withSpinner(color = '#a9daff')
            )
          )
        ),
        br(),
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
  
  require(stats)
  
  output$Team <- renderText(input$Team)
  output$Team2 <- renderText(input$Team)
  output$Team3 <- renderText(input$Team)
  output$Team4 <- renderText(input$Team)
  output$Team5 <- renderText(input$Team)
  output$Team6 <- renderText(input$Team)
  output$Team7 <- renderText(input$Team)
  output$Team8 <- renderText(input$Team)
  output$Team9 <- renderText(input$Team)
  
  output$select_Opponent <- renderUI({
    selectInput('Opponent', label = NULL, choices = c('', unique(sort(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Opponent)))))
  })

  output$select_Date <- renderUI({
    selectInput('Date', label = NULL, choices = c('', unique(sort(Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent) %>% pull(Date)))))
  })

  output$Goals <- renderText({
    Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Goals)
  })

  observeEvent(input$Team, {
    updateNumericInput(session, 'SoT', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(SoT)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Opp_Saves', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Opp_Saves)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'PKatt', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(PKatt)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'SCA_Total', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(SCA_Total)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Short_Cmp', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Short_Cmp)), digits = 2), min = 0, max = 1000)
    updateNumericInput(session, 'TB', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(TB)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dead', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Dead)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Clr', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Clr)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dist', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Dist)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'TklW', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(TklW)), digits = 2), min = 0, max = 100)
  })

  observeEvent(input$Date, {
    updateNumericInput(session, 'SoT', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(SoT), min = 0, max = 100)
    updateNumericInput(session, 'Opp_Saves', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Opp_Saves), min = 0, max = 100)
    updateNumericInput(session, 'PKatt', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(PKatt), min = 0, max = 100)
    updateNumericInput(session, 'SCA_Total', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(SCA_Total), min = 0, max = 100)
    updateNumericInput(session, 'Short_Cmp', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Short_Cmp), min = 0, max = 1000)
    updateNumericInput(session, 'TB', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(TB), min = 0, max = 100)
    updateNumericInput(session, 'Dead', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Dead), min = 0, max = 100)
    updateNumericInput(session, 'Clr', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Clr), min = 0, max = 100)
    updateNumericInput(session, 'Dist', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Dist), min = 0, max = 100)
    updateNumericInput(session, 'TklW', value = Full_PL_10 %>% dplyr::filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(TklW), min = 0, max = 100)
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
  
  output$MatchXGBox <- renderInfoBox({
    infoBox(
      'Match XG', 
      ifelse(class(try(round(stats::predict(PLXG.Model, selectedValues()), digits = 2), silent = TRUE)) == 'try-error',
             0,
             round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2)
      ),
      subtitle = input$Team,
      icon = icon('futbol'),
      color = 'light-blue',
      fill = TRUE
    )
  })
  
  output$AvgXGBox <- renderInfoBox({
    infoBox(
      'Average XG', 
      ifelse(class(try(round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), silent = TRUE)) == 'try-error', 
             0, 
             round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2)
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
      ifelse(class(try(round(round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2) - round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), digits = 2), silent = TRUE)) == 'try-error',
             0,
             round(round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2) - round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), digits = 2)
      ),
      subtitle = em('Match XG - Average XG'),
      icon = icon('futbol'),
      color = if (class(try(round(round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2) - round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), digits = 2), silent = TRUE)) == 'try-error') {
        'black'
      } else if (round(round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2) - round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), digits = 2) < 0) {
        'red'
      } else if (round(round(ifelse(stats::predict(PLXG.Model, selectedValues()) < 0, 0, stats::predict(PLXG.Model, selectedValues())), digits = 2) - round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(XG)), digits = 2), digits = 2) > 0) {
        'green'
      } else {
        'black'
      },
      fill = TRUE
    )
  })
  
  # output$ActualGoalsBox <- renderInfoBox({
  #   infoBox(
  #     'Match Goals',
  #     ifelse(class(try(Full_PL_10 %>% filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Goals), silent = TRUE)) == 'try-error',
  #            0,
  #            Full_PL_10 %>% filter(Team == input$Team & Opponent == input$Opponent & Date == input$Date) %>% pull(Goals)
  #     ),
  #     subtitle = input$Team,
  #     icon = icon('futbol'),
  #     color = 'light-blue',
  #     fill = TRUE
  #   )
  # })
  
  observeEvent(input$resetButton, {
    updateNumericInput(session, 'SoT', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(SoT)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Opp_Saves', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Opp_Saves)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'PKatt', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(PKatt)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'SCA_Total', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(SCA_Total)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Short_Cmp', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Short_Cmp)), digits = 2), min = 0, max = 1000)
    updateNumericInput(session, 'TB', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(TB)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dead', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Dead)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Clr', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Clr)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'Dist', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(Dist)), digits = 2), min = 0, max = 100)
    updateNumericInput(session, 'TklW', value = round(mean(Full_PL_10 %>% dplyr::filter(Team == input$Team) %>% pull(TklW)), digits = 2), min = 0, max = 100)
    reset('Opponent')
  })
  
  output$dataPlot <- renderPlot({
    Sys.sleep(.5)
    ggplot(data.frame(x = Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)), aes(x)) + 
      geom_histogram(aes(y = ..density..), 
                     bins = input$nBins,
                     color = 'black', 
                     fill = '#a9daff') +
      stat_function(fun = stats::dnorm,
                    args = list(
                      mean = mean(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)),
                      sd = stats::sd(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable))
                    ),
                    col = '#317196',
                    size = 2) +
      xlab(input$Variable) +
      ylab('Density')
  })
  
  output$PlotTeam <- renderText(input$PlotTeam)
  output$Variable <- renderText(input$Variable)
  
  output$AvgBox <- renderInfoBox({
    infoBox(
      paste(input$Variable, 'Average'), 
      ifelse(input$PlotTeam == '' | input$Variable == '', 
             0, 
             round(mean(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)), 
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
             round(mean(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), 
                   digits = 2)
      ),
      subtitle = em('Team Average - League Average'),
      icon = icon('futbol'),
      color = if (input$PlotTeam == '' | input$Variable == '') {
        'black'
      } else if (round(mean(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), digits = 2) < 0) {
        'red'
      } else if (round(mean(Full_PL_10 %>% dplyr::filter(Team == input$PlotTeam) %>% pull(input$Variable)) - mean(Full_PL_10 %>% pull(input$Variable)), digits = 2) > 0) {
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