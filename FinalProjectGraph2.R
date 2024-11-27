# DS 2003 Final Project Graph #2

data<-read.csv("epl-allseasons-matchstats.csv")
#library(tidyverse)
#library(shiny)
#library(shinythemes)
#library(ggwordcloud)


data<-mutate(data, 
             TotalGameCards = HomeRedCards+HomeYellowCards+AwayYellowCards+AwayRedCards,
             TotalGameFouls = HomeFouls+AwayFouls)

ui<-fluidPage(theme=shinytheme("cyborg"),
              titlePanel("Word Cloud of Referee vs. Given Red/Yellow Cards"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="season",
                              label="Select a Season:",
                              choices=c("All",unique(data$Season)),
                              selected="All"),
                  selectInput(inputId="team",
                              label="Select a Team:",
                              choices=c("All",unique(data$HomeTeam)),
                              selected="All"),
                  sliderInput("foul","Choose Minimum Number of Fouls:",min=0,max=50,value=5,step=5)
                ),
                mainPanel(plotOutput("graph"))
              )
)

server<-function(input,output){
  
  data1<-reactive({
    if (input$season == "All") {
      data
    } else {
      filter(data,Season %in% input$season)
    }
  })
  
  data2<-reactive({
    req(input$team)
    if (input$team == "All") {
      data1()
    } else {
      filter(data1(),HomeTeam %in% input$team | AwayTeam %in% input$team)
    }
  })
  
  data3<-reactive({
    req(input$foul)
    filter(data2(),TotalGameFouls >= input$foul)
  })
  
  graph_data<-reactive({
    data3()|>
      group_by(Referee)|>
      summarize(TotalCards=sum(TotalGameCards,na.rm=T))
    
  })
  
  output$graph<-renderPlot({
    ggplot(graph_data(),aes(label=graph_data()$Referee,
                            size=graph_data()$TotalCards,color=graph_data()$TotalCards)) + 
      geom_text_wordcloud_area() +
      scale_size_area(max_size = 40) +
      scale_color_gradient2(low="green",mid="red",high="darkred")
  })
}

shinyApp(ui,server)
