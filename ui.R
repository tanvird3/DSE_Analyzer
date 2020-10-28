library(shiny)
shinyUI(fluidPage (
  theme = shinytheme("superhero"),
  headerPanel("DSE Analyzer"),
  sidebarPanel(
    width = 2,
    selectInput(
      "instrument",
      label = "Select Instrument",
      choices =
        inst_name,
      selected = inst_name[t_default]
    ),
    textInput(
      "startdate",
      label = "Start Date (yyyy-mm-dd)",
      value =
        (Sys.Date() - 365)
    ),
    textInput("enddate",
              label = "End Date (yyyy-mm-dd)",
              value =
                Sys.Date()),
    submitButton(text = "View")
  ),
  mainPanel (h1(""),
             tabsetPanel(
               tabPanel("Candlestick Chart", plotlyOutput("output_candleplot")),
               tabPanel(
                 "Technical Indicators",
                 plotlyOutput("output_bbands"),
                 br(),
                 plotlyOutput("output_macd"),
                 br(),
                 plotlyOutput("output_rsi")
               ),
               tabPanel("VaR & CVaR",
                        #tableOutput("output_beta"),
                        plotlyOutput("output_vcvar"))
             ))
))
