library(shiny)
library(engsoccerdata)
library(dplyr)


data(england)

# Front-end (UI)
ui <- fluidPage(
  titlePanel("Анализ английских футбольных данных"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Выберите команду", 
                  choices = unique(c(england$home, england$visitor))),
      sliderInput("seasonRange", "Выберите диапазон сезонов", 
                  min = min(england$Season), max = max(england$Season), 
                  value = c(2000, 2020), step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Результаты матчей", dataTableOutput("matchesTable")),
        tabPanel("Средние голы", plotOutput("avgGoalsPlot"))
      )
    )
  )
)

# Back-end (Server)
server <- function(input, output) {
  
  # Фильтруем данные по команде и диапазону сезонов
  filteredData <- reactive({
    england %>%
      filter((home == input$team | visitor == input$team) & 
               Season >= input$seasonRange[1] & Season <= input$seasonRange[2])
  })
  
  output$matchesTable <- renderDataTable({
    filteredData() %>%
      select(Date, Season, home, visitor, FT, hgoal, vgoal, result) %>%
      arrange(desc(Season), Date)
  })

  output$avgGoalsPlot <- renderPlot({
    avgGoals <- filteredData() %>%
      group_by(Season) %>%
      summarise(
        AvgHomeGoals = mean(hgoal),
        AvgAwayGoals = mean(vgoal)
      )
    
    barplot(
      t(as.matrix(avgGoals[, -1])),
      beside = TRUE,
      names.arg = avgGoals$Season,
      col = c("blue", "red"),
      legend.text = c("Средние голы хозяев", "Средние голы гостей"),
      main = paste("Средние голы по сезонам для", input$team),
      xlab = "Сезон",
      ylab = "Среднее число голов"
    )
  })
}

shinyApp(ui, server)
