library(ddplot)
library(transformr)
library(data.table)
library(gapminder)
library(ggplot2)
library(gganimate)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(sf)
library(DT)
library(scales)
library(av)
library(dplyr)
library(stringr)

loldf <- fread("data/imaqtpie_DB.csv")

loldata <- loldf %>%
  
  filter((league == "LCS") |
           (league == "LEC") |
           (league == "LPL") |
           (league == "LCK") |
           (league == "MSI") |
           (league == "WCS") |
           (league == "NA LCS") | (league == "EU LCS") |
           (league == "IEM")
  ) %>%
  filter(position != "team") %>%
  
  #get game number for each team
  group_by(playername, year, split, teamname, league, position) %>%
  mutate(
    kda = (sum(kills) + sum(assists)) / sum(deaths),
    kp = ((sum(kills) + sum(assists)) / sum(teamkills)),
    games_played = n(),
    ckpm = sum(ckpm) / games_played,
    dpm = mean(dpm),
    damageshare = mean(damageshare),
    vspm = mean(vspm),
    cspm = mean(cspm),
    golddiffat15 = mean(golddiffat15),
    xpdiffat15 = mean(xpdiffat15),
    csdiffat15 = mean(csdiffat15),
    kills = sum(kills),
    deaths = sum(deaths),
    assists = sum(assists),
    season = year - 2010,
    year = case_when(
      league == "IEM" ~ year + 0.2,
      league == "MSI" ~ year + 0.4,
      split == "Summer" ~ year + 0.6,
      league == "WCS" ~ year + 0.8,
      TRUE ~ as.numeric(year)
    )
  ) %>%
  filter(games_played >= 10) %>%
  mutate_at(
    vars(
      season,
      kda,
      ckpm,
      dpm,
      kp,
      vspm,
      cspm,
      golddiffat15,
      xpdiffat15,
      csdiffat15,
      damageshare,
      kills,
      deaths,
      assists
    ),
    funs(round(., 2))
  ) %>%
  ungroup() %>%
  arrange(playername) %>%
  summarize(
    playername,
    season,
    split,
    teamname,
    league,
    position,
    games_played,
    kda,
    kp,
    ckpm,
    dpm,
    damageshare,
    vspm,
    cspm,
    golddiffat15,
    xpdiffat15,
    csdiffat15,
    kills,
    deaths,
    assists,
    year
  )
loldata <- unique(loldata)

loldata$league[loldata$league == "NA LCS"] <- "LCS"
loldata$league[loldata$league == "EU LCS"] <- "LEC"

kda_df <- loldata %>%
  group_by(playername, league) %>%
  summarise(avg = mean(kda)) %>%
  arrange(desc(avg))

server <- function(input, output) {
  fig.height = 100
  fig.width = 5
  output$kdaBar <- renderPlot({
    if (input$season != "All") {
      loldata <- filter(loldata, season == input$season)
    }
    if (input$league != "All") {
      loldata <- filter(loldata, league == input$league)
    }
    if (input$position != "All") {
      loldata <- filter(loldata, position == input$position)
    }
    if (input$team != "All") {
      loldata <- filter(loldata, teamname == input$team)
    }
    if (input$split != "All") {
      loldata <- filter(loldata, split == input$split)
    }
    
    validate (need(nrow(distinct(loldata)) > 0, "No values match these filters"))
    
    topKDA <- group_by(loldata, playername) %>%
      summarise(avgKDA = mean(kda)) %>%
      arrange(desc(avgKDA)) %>%
      top_n(15)
    
    ggplot(topKDA, aes(reorder(playername, avgKDA))) +
      geom_bar(aes(weight = avgKDA), fill = "#35F2BD") +
      coord_flip() +
      ggtitle("Top 15 players by KDA") +
      xlab("Player Name") +
      ylab("KDA") +
      theme_minimal(base_size = 19) +
      theme(plot.background = element_rect(color = "white")) +
      theme(plot.title = element_text(hjust = 0.5, color = "#FFFFFF")) +
      theme(axis.text.x = element_text(colour = "#FFFFFF")) +
      theme(axis.text.y = element_text(colour = "#FFFFFF")) +
      theme(axis.title = element_text(colour = "#FFFFFF")) +
      theme(plot.background = element_rect(fill = "#262626"))
  })
  
  # Create data table
  output$table <- renderDataTable({
    # Filter data based on selected position
    if (input$season != "All") {
      loldata <- filter(loldata, season == input$season)
    }
    if (input$position != "All") {
      loldata <- filter(loldata, position == input$position)
    }
    
    # Filter data based on selected region
    if (input$league != "All") {
      loldata <- filter(loldata, league == input$league)
    }
    if (input$team != "All") {
      loldata <- filter(loldata, teamname == input$team)
    }
    if (input$split != "All") {
      loldata <- filter(loldata, split == input$split)
    }
    
    # Hide table when user has filtered out data
    validate (need(nrow(loldata) > 0, ""))
    
    loldata[, 1:15]
  })
  
  output$histogram <- renderPlot({
    if (input$season != "All") {
      loldata <- filter(loldata, season == input$season)
    }
    if (input$league != "All") {
      loldata <- filter(loldata, league == input$league)
    }
    
    if (input$position != "All") {
      loldata <- filter(loldata, position == input$position)
    }
    if (input$split != "All") {
      loldata <- filter(loldata, split == input$split)
    }
    
    validate (need(nrow(distinct(loldata)) > 0, "No values match these filters"))
    #hist(value)
    # Basic histogram
    mu <- kda_df %>%
      group_by(league) %>%
      mutate(avg_kda = mean(avg)) %>%
      summarise(league, avg_kda)
    mu <- unique(mu)
    
    p <- ggplot(kda_df, aes(
      x = avg,
      fill = league,
      color = league
    )) +
      geom_histogram(aes(y = ..density..),
                     alpha = 0.5,
                     bins = input$bins) +
      theme_dark(base_size = 15) +
      theme(plot.background = element_rect(fill = "#262626", color = "white")) +
      theme(panel.background = element_rect(fill = "#262626")) +
      labs(title = "Number of players vs KDA bracket",
           x = "KDA",
           y = "# of players") +
      geom_vline(data = mu,
                 aes(xintercept = avg_kda, color = league),
                 linetype = "dashed") +
      geom_density(alpha = 0.5) +
      theme(plot.title = element_text(hjust = 0.5, color = "#FFFFFF")) +
      theme(axis.text.x = element_text(colour = "#FFFFFF")) +
      theme(axis.text.y = element_text(colour = "#FFFFFF")) +
      theme(axis.title = element_text(colour = "#FFFFFF"))
    p
    
  })
  
  output$careerKDA <- renderPlot({
    tempdata <- loldata %>%
      filter(playername == input$playername)
    if (input$kdaHistoryFormat != "All") {
      tempdata <- filter(tempdata, league == input$kdaHistoryFormat)
    }
    ggplot(tempdata) +
      geom_line(aes(x = year, y = kills), color = "#35F2BD", size = 1) +
      geom_line(aes(x = year, y = deaths), color = "red", size = 1) +
      geom_line(aes(x = year, y = assists), color = "#7A5FD9", size = 1) +
      geom_point(aes(x = year, y = assists), color = "#7A5FD9", size = 5) +
      geom_point(aes(x = year, y = deaths), color = "red", size = 5) +
      geom_point(aes(x = year, y = kills), color = "#35F2BD", size = 5) +
      theme_dark(base_size = 15) +
      theme(plot.background = element_rect(fill = "#262626", color = "white")) +
      theme(panel.background = element_rect(fill = "#262626")) +
      theme(plot.title = element_text(hjust = 0.5, color = "#FFFFFF")) +
      theme(axis.text.x = element_text(colour = "#FFFFFF")) +
      theme(axis.text.y = element_text(colour = "#FFFFFF")) +
      theme(axis.title = element_text(colour = "#FFFFFF")) +
      xlim(2014, 2023)
  })
  
}

ui <- bootstrapPage(title = 'IMAQTPIECHART',
                    tags$head(tags$style(
                      HTML(
                        "
    body { background-color: #f2efe9;}
    .container-fluid {width: 100%; background-color: #262626; padding: 15px;}
    .topimg { width: 15%; display: block; margin: 0px auto 40px auto; }
    .title { text-align: center; color: #6A66F2;}
    .subtitle { text-align: center; color: #FFFFFF;}
    .toprow { border-radius: 20px; margin: 60px 0px; padding: 30px; background-color: #6A66F2;}
    .filters { margin: 0px auto; border-radius: 25px; padding: 10px; color: #FFFFFF;}
    .shiny-input-container { width:100% !important; }
    .table { width: 100%;margin-top: 30px; color: #ffffff;max-width: 100%; overflow: auto;}
    .table th {color: #7A5FD9;}
    .table td {color: #FFFFFF; background-color: #262626;}
    .table tr::before {color: #FFFFFF;}
    .leaflet-top { z-index:999 !important; color: #ffffff;}
    .row.table::before {color: #ffffff;}
    .nav {text-align: center;}
    "
                      )
                    )),
                    div(class = "container-fluid",
                        div(
                          class = "row",
                          navbarPage(
                            "IMAQTPIECHART",
                            fluid = TRUE,
                            tabPanel(
                              "Player Stats",
                              img(class = "topimg", src = "https://i.ibb.co/3Bk7j3B/logo2.png"),
                              h1("IMAQTPIECHART", class = "title"),
                              br(),
                              br(),
                              h2("Leaderboards", class = "subtitle"),
                              fluidRow(
                                class = "toprow",
                                fluidRow (
                                  class = "filters",
                                  column(4, selectInput(
                                    "season",
                                    "Season",
                                    c("All",
                                      "4",
                                      "5",
                                      "6",
                                      "7",
                                      "8",
                                      "9",
                                      "10",
                                      "11",
                                      "12")
                                  )),
                                  column(4, selectInput(
                                    "split",
                                    "Split",
                                    c("All", "Spring", "Summer")
                                  )),
                                  column(4, selectInput(
                                    "playoffs",
                                    "Include Playoffs?",
                                    c("All", "Regular Season", "Playoffs")
                                  )),
                                ),
                                fluidRow (
                                  class = "filters",
                                  column(3, selectInput(
                                    "league", "Region", c("All", "LCK", "LCS", "LEC", "LPL")
                                  )),
                                  column(3, selectInput(
                                    "position",
                                    "Position",
                                    c("All", "top", "jng", "mid", "bot", "sup")
                                  )),
                                  column(4, selectInput(
                                    "team", "Select Team",  choice = c("All", sort(loldata$teamname))
                                  )) #(filter(loldata, league == input$league))$teamname
                                )
                              ),
                              fluidRow(column(12, class = "bar", plotOutput("kdaBar"))),
                              fluidRow (class = "table", dataTableOutput("table")),
                              fluidRow(column(
                                12,
                                class = "bar",
                                sliderInput(
                                  "bins",
                                  "# Of Bins:",
                                  min = 10,
                                  max = 300,
                                  value = 90
                                ),
                                plotOutput("histogram")
                              )),
                              fluidRow(class = "toprow",
                                       column(
                                         6,
                                         selectInput(
                                           inputId = "playername",
                                           label = "Choose a player",
                                           choices = unique(loldata$playername)
                                         )
                                       ),
                                       column(
                                         6, selectInput(
                                           "kdaHistoryFormat",
                                           "Formats",
                                           c("All", "League", "Tournament")
                                         )
                                       ), ),
                              fluidRow(column(12,
                                              plotOutput("careerKDA")))
                            ),
                            
                            tabPanel(
                              "Team Stats",
                              img(class = "topimg", src = "https://i.ibb.co/3Bk7j3B/logo2.png"),
                              h1("IMAQTPIECHART", class = "title"),
                            ),
                            tabPanel(
                              "Champ Stats",
                              img(class = "topimg", src = "https://i.ibb.co/3Bk7j3B/logo2.png"),
                              h1("IMAQTPIECHART", class = "title"),
                            ),
                            tabPanel(
                              "Player Comparison",
                              img(class = "topimg", src = "https://i.ibb.co/3Bk7j3B/logo2.png"),
                              h1("IMAQTPIECHART", class = "title"),
                            )
                          )
                        )))

shinyApp(ui = ui, server = server)