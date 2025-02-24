library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

movies <- read_csv("./netflix_titles.csv")

# Data preparation
movies <- movies %>%
  filter(!is.na(release_year), !is.na(type), !is.na(country)) %>%
  mutate(date_added = as.Date(date_added, format = "%B %d, %Y"))

# UI
ui <- fluidPage(
  titlePanel("Netflix Titles Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Select Type:", choices = unique(movies$type), selected = "Movie"),
      sliderInput("year", "Select Release Year:",
                  min = min(movies$release_year, na.rm = TRUE),
                  max = max(movies$release_year, na.rm = TRUE),
                  value = c(2010, 2020), step = 1,sep = "" )
    ),
    mainPanel(
      plotlyOutput("countryPlot"),
      textOutput("insights")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    movies %>%
      filter(type == input$type,
             release_year >= input$year[1],
             release_year <= input$year[2])
  })

  
  output$countryPlot <- renderPlotly({
    p <- filtered_data() %>%
      count(country, sort = TRUE, name = "count") %>%
      head(10) %>%
      ggplot(aes(x = reorder(country, count), y = count, fill = country)) +
      geom_col() +
      coord_flip() +
      labs(title = 'Top 10 Countries by Title Count',
           x = 'Country', y = 'Count')
    ggplotly(p)
  })

  output$insights <- renderText({
    "Explore the trends of Netflix titles by type and country. Use the controls to adjust the type and release year range."
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




















