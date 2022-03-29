library(shiny)
library(tidyverse)
library(spData) 
library(sf) 
library(DT)
library(scales)

#Load data of all the pro leagues
df <- read.csv("data/loldata.csv")

#Clean data
loldata <- df %>%
  
  filter((league=="LCS" & date >= "2022-02-05") | (league=="LEC" & date >= "2022-01-14") | (league=="LPL" & date >= "2022-01-10") | (league=="LCK" & date >= "2022-01-12")) %>% 
  filter(position != "team") %>%
  
  #get game number for each team
  group_by(playername, teamname, position) %>%
  mutate(kda = (sum(kills)+sum(assists))/sum(deaths), 
         kp = ((sum(kills)+sum(assists))/sum(teamkills)), 
         games_played=n(), 
         ckpm = sum(ckpm)/games_played, dpm = mean(dpm), 
         damageshare = mean(damageshare), 
         vspm = mean(vspm), 
         cspm = mean(cspm), 
         golddiffat15 = mean(golddiffat15), 
         xpdiffat15 = mean(xpdiffat15),
         csdiffat15 = mean(csdiffat15))%>%
  mutate_at(vars(kda, ckpm, dpm, kp, vspm, cspm, golddiffat15, xpdiffat15, csdiffat15, damageshare), funs(round(., 2)))%>%
  ungroup() %>%
  arrange(playername)%>%
  summarize(playername, teamname, league, position, games_played, kda, kp, ckpm, dpm, damageshare, vspm, cspm, golddiffat15, xpdiffat15, csdiffat15)
  loldata <- unique(loldata)
  print(loldata)

ui <- fluidPage(
  #CSS
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9;}
      .container-fluid { background-color: #262626; width: 1800px; padding: 60px;}
      .topimg { width: 400px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; color: #6A66F2;}
      .toprow { border-radius: 25px; margin: 60px 0px; padding: 15px; background-color: #6A66F2;}
      .filters { margin: 0px auto; border-radius: 25px; padding: 10px; color: #FFFFFF;}
      .shiny-input-container { width:100% !important; }
      .table { padding: 30px; margin-top: 30px; color: #ffffff;}
      .table th {color: #7A5FD9;}
      .table td {color: #FFFFFF; background-color: #262626;}
      .table tr::before {color: #FFFFFF;}
      .leaflet-top { z-index:999 !important; color: #ffffff;}
      .row.table::before {color: #ffffff;}
      "))
  ),
  
  img(class = "topimg", src = "https://i.ibb.co/3Bk7j3B/logo2.png"), 
  h1("IMAQTPIECHART", class = "title"),
  
  fluidRow(class = "toprow",
    fluidRow (class = "filters",
              column(6, selectInput("league", "Region", c("All", "LCK", "LCS", "LEC", "LPL"))),
              column(6, selectInput("position", "Position", c("All", "top", "jng", "mid", "bot", "sup")))
    )
  ),
  
  fluidRow(column(6, class = "bar", plotOutput("kdaBar"))),
  fluidRow (class = "table", dataTableOutput("table"))
  
  )

server <- function(input, output) {
  output$kdaBar <- renderPlot( {
    
    if (input$league != "All") {
      loldata <- filter(loldata, league == input$league)
    }
    
    if (input$position != "All") {
      loldata <- filter(loldata, position == input$position)
    }
    
    validate (
      need(nrow(distinct(loldata)) > 0, "No values match these filters")
    )
    
    topKDA <- group_by(loldata, playername) %>%
      summarise(avgKDA = mean(kda)) %>%
      arrange(desc(avgKDA)) %>%
      top_n(15)
    print(topKDA)
    
    ggplot(topKDA, aes(reorder(playername, avgKDA))) +
      geom_bar(aes(weight = avgKDA), fill = "#35F2BD") +
      coord_flip() +
      ggtitle("Top 15 players by KDA") +
      xlab("Player Name") +
      ylab("KDA") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5, color = "#FFFFFF")) +
      theme(axis.text.x = element_text(colour = "#FFFFFF")) +
      theme(axis.text.y = element_text(colour = "#FFFFFF")) +
      theme(axis.title = element_text(colour = "#FFFFFF")) +
      theme(plot.background = element_rect(fill = "#262626"))
  })
  
  # Create data table
  output$table <- renderDataTable({
    
    # Filter data based on selected position
    if (input$position != "All") {
      loldata <- filter(loldata, position == input$position)
    }
    
    # Filter data based on selected region
    if (input$league != "All") {
      loldata <- filter(loldata, league == input$league)
    }
    
    # Hide table when user has filtered out data
    validate (
      need(nrow(loldata) > 0, "")
    )
    
    loldata[,1:15]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
