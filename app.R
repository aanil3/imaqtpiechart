library(shiny)
library(tidyverse)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT) # For making fancy tables
library(dplyr)
library(scales)

#Load data of all the pro leagues
df <- read.csv("data/loldata.csv")

#Load Map 
mapData <- world[c(2,11)]

#Clean data
loldata <- df %>%
  
  filter((league=="LCS" & date >= "2022-02-05") | (league=="LEC" & date >= "2022-01-14") | (league=="LPL" & date >= "2022-01-10") | (league=="LCK" & date >= "2022-01-12")) %>% 
  filter(position != "team") %>%
  
  #get game number for each team
  group_by(playername) %>%
  mutate(kda = (sum(kills)+sum(assists))/sum(deaths))%>%
  mutate(kp = label_percent()((sum(kills)+sum(assists))/sum(teamkills))) %>%
  mutate(games_played=n()) %>%
  mutate(ckpm = sum(ckpm)/games_played) %>%
  mutate(dpm = mean(dpm)) %>%
  mutate(damageshare = mean(damageshare)) %>%
  mutate(vspm = mean(vspm)) %>%
  mutate(cspm = mean(cspm)) %>%
  mutate_at(vars(kda, ckpm, dpm, damageshare, vspm, cspm), funs(round(., 2)))%>%
  ungroup() %>%
  summarize(playername, league, position, games_played, kda, kp, ckpm, dpm, damageshare, vspm, cspm, golddiffat15, xpdiffat15, csdiffat15)

ui <- fluidPage(
  #CSS
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: 1500px; padding: 60px; }
      .topimg { width: 120px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; }
      .toprow { margin: 60px 0px; padding: 30px; background-color: #fae8bb; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 30px; margin-top: 30px; }
      .leaflet-top { z-index:999 !important; }
      "))
  ),
  
  img(class = "topimg", src = "data/logo2.png"), 
  
  h1("Player Tables", class = "title"),
  
  fluidRow(class = "toprow",
    fluidRow (class = "filters",
              column(6,
                     #Region Menu
                     selectInput("league", "league", c("All", "LCK", "LCS", "LEC", "LPL"))
                     ),
              column(6,
                     #Position Menu
                     selectInput("position", "position", c("All", "top", "jng", "mid", "bot", "sup"))
              ),
    )
  ),
  
  fluidRow (
    column(6, class = "bar",
           plotOutput("brandBar")
    ),
  ),
  
  fluidRow (class = "table",
            dataTableOutput("table")
  )
)

server <- function(input, output) {
  loldata <- distinct(loldata, playername, league, position, games_played, kda, kp, ckpm, dpm, damageshare, vspm, cspm, golddiffat15, xpdiffat15, csdiffat15)
  output$brandBar <- renderPlot( {
    if (input$league != "All") {
      loldata <- distinct(filter(loldata, league == input$league))
    }
    if (input$position != "All") {
      loldata <- distinct(filter(loldata, position == input$position))
    }
    
    validate (
      need(nrow(distinct(loldata)) > 0, "")
    )
    
  })
  
  # Create data table
  output$table <- renderDataTable({
    loldata <- distinct(loldata, playername, league, position, games_played, kda, kp, ckpm, dpm, damageshare, vspm, cspm, golddiffat15, xpdiffat15, csdiffat15)
    
    # Filter data based on selected region
    if (input$position != "All") {
      loldata <- distinct(filter(loldata, position == input$position))
    }
    
    # Filter data based on selected position
    if (input$league != "All") {
      loldata <- distinct(filter(loldata, league == input$league))
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(distinct(loldata)) > 0, "")
    )
    
    distinct(loldata[,1:11])
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
