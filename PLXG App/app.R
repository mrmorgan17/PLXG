library(shiny)

PL_10 <- read.csv('PL_10.csv')
PLXG.Model <- readRDS('PLXGModel.RData')

# Define UI for slider demo app ----
ui <- fluidPage(
  
  navbarPage('PLXG',
             tabPanel('About',
                      titlePanel('Premier League Expected Goals (PLXG)'),
                      helpText('Ever wondered how to predict...'),
                      br(),
                      helpText('Match data for each team was scraped from ', a('FBref', href = 'https://fbref.com/en/'), 'for the following Premier League campaigns:'),
                      helpText('2017-2018'),
                      helpText('2018-2019'),
                      helpText('2019-2020'),
                      helpText('Code for scraping FBref is available ', a('here', href = 'https://fbref.com/en/'), 'on my GitHub profile'),
                      br(),
                      helpText('')),
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
                          numericInput('SoT', 'Shots on Target (SoT):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Opp_Saves', 'Opposing Goalkeeper Saves (Opp_Saves):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('PKatt', 'Penalty Kicks Attempted (PKatt):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('SCA_Total', 'The two offensive actions directly leading to a shot, such as passes, dribbles and drawing fouls. (SCA):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Short_Cmp', 'Passes completed between 5 and 15 yards (Short_Att):', 
                                       0, 
                                       min = 0, 
                                       max = 1000),
                          
                          numericInput('TB', 'Completed passes sent between back defenders into open space (TB):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Dead', 'Dead-ball passes (Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks) (Dead):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                
                          numericInput('Clr', 'Opposing Team Clearances (Clr):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('Dist', 'Average distance, in yards, from goal of all shots taken (Does not include penalty kicks) (Dist):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          numericInput('TklW', 'Tackles in which the opposing team won possession of the ball (TklW):', 
                                       0, 
                                       min = 0, 
                                       max = 100),
                          
                          submitButton('Update XG', icon('refresh'))
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Table summarizing the values entered ----
                          helpText('Table showing team averages in each variable that was used to build the expected goals model'),
                          
                          helpText('Averages were calculated using data from the following Premier League campaigns:'),
                          
                          helpText('2017-2018'),
                          
                          helpText('2018-2019'),
                          
                          helpText('2019-2020'),
                          
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