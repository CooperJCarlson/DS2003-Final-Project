library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

matches <- read.csv("epl-allseasons-matchstats.csv")

matches$Year <- as.numeric(substr(matches$Season, 1, 4))

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Referees and Cards Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("referee", "Select Referee:",
                  choices = unique(matches$Referee),
                  selected = unique(matches$Referee)[1],
                  multiple = TRUE),  # Allow selecting multiple referees
      sliderInput("year", "Select Year Range:",
                  min = min(matches$Year, na.rm = TRUE),
                  max = max(matches$Year, na.rm = TRUE),
                  value = c(min(matches$Year, na.rm = TRUE), max(matches$Year, na.rm = TRUE)),
                  step = 1),
      selectInput("card_type", "Select Card Type:",
                  choices = c("Red Cards" = "total_red_cards", "Yellow Cards" = "total_yellow_cards"),
                  selected = "total_red_cards")
    ),
    mainPanel(
      plotlyOutput("interactiveCardPlot", width = "700px", height = "500px"),
      HTML("<p style='margin-top: 10px;'>Offers a different visual to see the stark contrast between referees who give out more cards over multiple seasons over referees who give less cards.</p>")
      
    )
  )
)

server <- function(input, output) {
  
  summarizedData <- reactive({
    matches %>%
      filter(Referee %in% input$referee,
             Year >= input$year[1],
             Year <= input$year[2]) %>%
      group_by(Referee) %>%
      summarise(total_red_cards = sum(HomeRedCards + AwayRedCards, na.rm = TRUE),
                total_yellow_cards = sum(HomeYellowCards + AwayYellowCards, na.rm = TRUE)) %>%
      pivot_longer(cols = c(total_red_cards, total_yellow_cards),
                   names_to = "card_type",
                   values_to = "count")
  })
  
  output$interactiveCardPlot <- renderPlotly({
    filtered_data <- summarizedData() %>% 
      filter(card_type == input$card_type)
    
    p <- ggplot(filtered_data, aes(x = Referee, y = count, fill = card_type, text = paste(
      "Referee: ", Referee, "<br>",
      "Card Type: ", card_type, "<br>",
      "Count: ", count
    ))) +
      geom_bar(stat = "identity") +
      labs(title = "Cards Given by Referees",
           x = "Referee",
           y = "Number of Cards",
           fill = "Card Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")  
  })
}

shinyApp(ui = ui, server = server)