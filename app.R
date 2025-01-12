library(tidyverse)
library(broom)
library(bslib)
library(mixexp)
library(shiny)

pitchers <- read.csv("pitchers.csv") 

pitcher_list <- unique(pitchers$player_name)

descript <- c("The table shows the results of a linear regression of wOBA against pitch usage 
  in a given outing, weighted by pitches thrown (2024). Available pitchers had at least 100 innings 
  and 10% usage for each pitch type. The plot is a contour estimate of the modeled wOBA based on the blend of pitches. 
  Note that regions outside of the black dotted lines are extrapolations and therefore unreliable.") 

ui <- page_sidebar(
  title = "Pitch Mixture Design",
  sidebar = sidebar(
    selectInput("pitcher", label = "Pitcher", choices = pitcher_list) 
  ),
  textOutput("text"), 
  tableOutput("table"), 
  plotOutput("plot")
)

server <- function(input, output, session) {
  data <- reactive(
    pitchers %>% filter(player_name == input$pitcher) 
  )
  
  model <- reactive(
    lm(mean_woba ~ -1 + 
         fastball_pct + breaking_pct + offspeed_pct + 
         fastball_pct:breaking_pct + fastball_pct:offspeed_pct + 
         breaking_pct:offspeed_pct, 
       weights = pitches, data = data())
  )
  
  output$text <- renderText({
    strwrap(descript, width = 60, indent = 5)
  })
  
  output$table <- renderTable({
    summary(model()) %>% tidy() 
  })
  
  output$plot <- renderPlot({
    ModelPlot(model = model(), 
              dimensions = list(x1='fastball_pct', x2='breaking_pct', x3='offspeed_pct'), 
              lims = c(round(min(data()$fastball_pct),2), round(max(data()$fastball_pct),2), 
                       round(min(data()$breaking_pct),2), round(max(data()$breaking_pct),2),
                       round(min(data()$offspeed_pct),2), round(max(data()$offspeed_pct),2)), 
              constraints = T, pseudo = T, fill = T, labels = T, 
              at = c(.200, .300, .400, .500), contour = T, 
              cornerlabs = NULL, label.style = 'align', 
              axislabs = c('Fastball Usage', 'Breaking Ball Usage', 
                           'Offspeed Usage')) 
  })
}

shinyApp(ui, server)

