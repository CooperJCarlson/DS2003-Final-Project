library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

epl_data <- read.csv("epl-allseasons-matchstats.csv")

ui <- fluidPage(
  titlePanel("Team Performance Circular Correlation Heatmap"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teams", "Select Teams:",
                  choices = unique(c(epl_data$HomeTeam, epl_data$AwayTeam)),
                  selected = c("Chelsea","Liverpool","Arsenal","Brighton"),
                  multiple = TRUE),
      checkboxGroupInput("variables", "Select Variables:",
                         choices = colnames(epl_data)[8:23],
                         selected = colnames(epl_data)[8:10]),
      sliderInput("threshold", "Correlation Threshold:",
                  min = 0, max = 1, value = 0.5, step = 0.1)
    ),
    mainPanel(
      plotOutput("heatmapPlot", height = "800px", width = "1000px"),
      tags$div(
        tags$p(
          tags$b("The heat map visualizes the pairwise correlations between selected performance metrics for specified teams in the English Premier League, highlighting the strength and direction of relationships using circle sizes and colors.")
        ),
        tags$p(
          "The heat map serves as an interactive tool designed to explore the correlations between various performance metrics for selected teams in the English Premier League. It allows users to investigate relationships between key indicators of team performance, such as goals scored, shots on target, fouls, and other match statistics. This visualization uses circles to represent the pairwise correlation values, where the size and color of each circle provide insights into the strength and direction of the relationships. Positive correlations, such as goals scored and shots on target, are shown in red, indicating that as one metric increases, the other tends to increase as well. Negative correlations, such as fouls committed and shots on target, are shown in blue, indicating an inverse relationship."
        )
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$teams, input$variables)
    epl_data %>% 
      filter(HomeTeam %in% input$teams | AwayTeam %in% input$teams)%>% 
      select(all_of(input$variables))%>% 
      na.omit()
  })
  
  output$heatmapPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 1) {
      cor_matrix <- cor(data, use = "complete.obs")
      cor_matrix[abs(cor_matrix) < input$threshold] <- NA
      
      cor_matrix[upper.tri(cor_matrix)] <- NA
      melted_cor <- melt(cor_matrix, na.rm = TRUE)
      
      ggplot(melted_cor, aes(Var1, Var2, size = abs(value), color = value)) +
        geom_point(alpha = 0.8) +
        geom_text(aes(label = round(value, 2)), color = "black", size = 4.5) + 
        scale_size(range = c(6, 15), guide = "none") +  
        scale_color_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1, 1), space = "Lab", 
                              name = "Correlation") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, size = 18, hjust = 1), 
          axis.text.y = element_text(size = 18),
          axis.title.x = element_text(size = 18, face = "bold"), 
          axis.title.y = element_text(size = 18, face = "bold"),
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
        ) +
        labs(title = "Variable Correlations for Top Four Teams in the 2010-2018 English Premier League",
             x = "Variables", y = "Variables")
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Not enough data for heatmap", size = 5) +
        theme_void()
    }
  })
}

shinyApp(ui = ui, server = server)
