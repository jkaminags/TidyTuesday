library(shiny)
library(tidytuesdayR)
library(tidyverse)
library(paletteer)
library(ggtext)
library(bslib)

source("dataHelper.R")

ui <- page_sidebar(
  title = "Gini Income Inequality by Year",

  sidebar = sidebar(
    "Select a year (1975-2023)",
    sliderInput(
      "year",
      label = "Year:",
      min = 1975,
      max = 2023,
      value = 1975,
      sep = ""
    )
  ),
  card(plotOutput("giniPlot"))

)

server <- function(input, output) {
  output$giniPlot <- renderPlot({
    make_gini_plot(full_gini, input$year)
  })
}

shinyApp(ui = ui, server = server)
