library(tidyverse)
library(broom)
library(bslib)
library(mixexp)
library(shiny)

mariners_sp <- c('Kirby, George', 'Castillo, Luis', 'Miller, Bryce',
                 'Gilbert, Logan', 'Woo, Bryan')

ui <- page_sidebar(
  title = "Pitch Mixture Analysis",
  sidebar = sidebar(
    selectInput("pitcher", label = "Pitcher", choices = mariners_sp) 
  ),
  textOutput("text"), 
  tableOutput("table"), 
  plotOutput("plot")
)

server <- function(input, output, session) {
  data <- reactive(
    statcast_24 %>% 
      filter(player_name == input$pitcher) %>% 
      mutate(
        pitch_category = case_when(
          pitch_type %in% c('FF','SI','FC') ~ 'fastball', 
          pitch_type %in% c('SL','CU','KC','ST') ~ 'breaking', 
          .default = 'offspeed'
        )
      ) %>%
      group_by(game_date) %>% 
      summarize(
        pitches = n(), 
        fb_usage = sum(pitch_category == 'fastball') / pitches, 
        br_usage = sum(pitch_category == 'breaking') / pitches, 
        os_usage = sum(pitch_category == 'offspeed') / pitches, 
        mean_woba = mean(woba_value, na.rm = T)
      )
  )
  
  model <- reactive(
    lm(mean_woba ~ -1+fb_usage+br_usage+os_usage+fb_usage:br_usage+ 
         fb_usage:os_usage+br_usage:os_usage, weights = pitches, data = data())
  )
  
  output$text <- renderText({
    "The table shows the results of a linear regression of wOBA against pitch usage 
    in a given outing, weighted by pitches thrown. The plot is a contour estimate of the 
    modeled wOBA based on the blend of pitches. Note that some regions of the graph are mere
    extrapolations and should not be relied on."
  })
  output$table <- renderTable({
    summary(model()) %>% tidy() 
  })
  output$plot <- renderPlot({
    ModelPlot(model = model(), 
              dimensions = list(x1='fb_usage', x2='br_usage', x3='os_usage'), 
              lims = c(round(min(data()$fb_usage),2), round(max(data()$fb_usage),2), 
                       round(min(data()$br_usage),2), round(max(data()$br_usage),2),
                       round(min(data()$os_usage),2), round(max(data()$os_usage),2)), 
              constraints = T, pseudo = T, fill = T, labels = T, 
              at = c(.200, .300, .400, .500), contour = T, 
              cornerlabs = NULL, 
              axislabs = c('Fastball Usage', 'Breaking Ball Usage', 
                           'Offspeed Usage')) 
  })
}

shinyApp(ui, server)
