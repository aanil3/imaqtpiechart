ui <- fluidPage(
  #CSS
  tags$head(
     tags$style(HTML("
    body { background-color: #f2efe9;}
    .container-fluid {width: 1800px; background-color: #262626; padding: 15px;}
    .topimg { width: 400px; display: block; margin: 0px auto 40px auto; }
    .title { text-align: center; color: #6A66F2;}
    .toprow { border-radius: 20px; margin: 60px 0px; padding: 30px; background-color: #6A66F2;}
    .filters { margin: 0px auto; border-radius: 25px; padding: 10px; color: #FFFFFF;}
    .shiny-input-container { width:100% !important; }
    .table { padding: 30px; margin-top: 30px; color: #ffffff;}
    .table th {color: #7A5FD9;}
    .table td {color: #FFFFFF; background-color: #262626;}
    .table tr::before {color: #FFFFFF;}=
    .leaflet-top { z-index:999 !important; color: #ffffff;}
    .row.table::before {color: #ffffff;}
      "))
  ),
  fluidRow(
    navbarPage("App Title",
               fluid = TRUE,
               tabPanel("Plot"),
               tabPanel("Summary"),
               tabPanel("Table")
    )
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