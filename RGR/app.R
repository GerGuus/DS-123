library(shiny)
library(engsoccerdata)
library(dplyr)
library(ggplot2)

# Загружаем данные
data(england)

# Front-end (UI)
ui <- fluidPage(
  titlePanel("Анализ английских футбольных данных"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Выберите первую команду", 
                  choices = unique(c(england$home, england$visitor))),
      selectInput("team2", "Выберите вторую команду (для сравнения)", 
                  choices = unique(c(england$home, england$visitor))),
      sliderInput("seasonRange", "Выберите диапазон сезонов", 
                  min = min(england$Season), max = max(england$Season), 
                  value = c(2000, 2020), step = 1),
      checkboxGroupInput("location", "Выберите местоположение матча", 
                         choices = list("Домашние матчи" = "home", "Гостевые матчи" = "away"),
                         selected = c("home", "away"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Результаты матчей", dataTableOutput("matchesTable")),
        tabPanel("Средние голы", plotOutput("avgGoalsPlot")),
        tabPanel("Распределение результатов", plotOutput("resultsDistPlot")),
        tabPanel("Разница мячей", plotOutput("goalDiffPlot")),
        tabPanel("Тренды по голам", plotOutput("goalsTrendPlot"))
      )
    )
  )
)

# Back-end (Server)
server <- function(input, output) {
  
  # Фильтруем данные по командам, сезону и местоположению
  filteredData <- reactive({
    data <- england %>%
      filter((home == input$team1 | visitor == input$team1 | 
              home == input$team2 | visitor == input$team2) & 
             Season >= input$seasonRange[1] & Season <= input$seasonRange[2])
    
    if ("home" %in% input$location & "away" %in% input$location) {
      return(data)
    } else if ("home" %in% input$location) {
      return(data %>% filter(home == input$team1 | home == input$team2))
    } else {
      return(data %>% filter(visitor == input$team1 | visitor == input$team2))
    }
  })
  
  # Таблица с результатами матчей
  output$matchesTable <- renderDataTable({
    filteredData() %>%
      select(Date, Season, home, visitor, FT, hgoal, vgoal, result) %>%
      arrange(desc(Season), Date)
  })
  
  # График среднего числа голов
  output$avgGoalsPlot <- renderPlot({
    avgGoals <- filteredData() %>%
      group_by(Season) %>%
      summarise(
        AvgHomeGoals = mean(hgoal),
        AvgAwayGoals = mean(vgoal)
      )
    
    ggplot(avgGoals, aes(x = Season)) +
      geom_line(aes(y = AvgHomeGoals, color = "Средние голы хозяев")) +
      geom_line(aes(y = AvgAwayGoals, color = "Средние голы гостей")) +
      labs(title = paste("Средние голы по сезонам для", input$team1, "и", input$team2),
           x = "Сезон", y = "Среднее количество голов") +
      theme_minimal()
  })
  
  # Распределение результатов матчей
  output$resultsDistPlot <- renderPlot({
    resultsDist <- filteredData() %>%
      mutate(result = case_when(
        result == "H" ~ "Победа хозяев",
        result == "A" ~ "Победа гостей",
        TRUE ~ "Ничья"
      )) %>%
      group_by(result) %>%
      summarise(count = n())
    
    ggplot(resultsDist, aes(x = result, y = count, fill = result)) +
      geom_bar(stat = "identity") +
      labs(title = "Распределение результатов матчей", x = "Результат", y = "Количество") +
      theme_minimal()
  })
  
  # График разницы мячей
  output$goalDiffPlot <- renderPlot({
    goalDiff <- filteredData() %>%
      group_by(Season) %>%
      summarise(AvgGoalDiff = mean(goaldif))
    
    ggplot(goalDiff, aes(x = Season, y = AvgGoalDiff)) +
      geom_line(color = "green") +
      labs(title = "Средняя разница мячей по сезонам", x = "Сезон", y = "Средняя разница мячей") +
      theme_minimal()
  })
  
  # Временной ряд по голам и победам
  output$goalsTrendPlot <- renderPlot({
    goalsTrend <- filteredData() %>%
      group_by(Season) %>%
      summarise(TotalGoals = sum(hgoal + vgoal),
                Wins = sum(result == "H" | result == "A"))
    
    ggplot(goalsTrend, aes(x = Season)) +
      geom_line(aes(y = TotalGoals, color = "Голы"), size = 1) +
      geom_line(aes(y = Wins, color = "Победы"), size = 1) +
      labs(title = "Тренды по голам и победам", x = "Сезон", y = "Количество") +
      theme_minimal()
  })
}

# Запуск приложения
shinyApp(ui, server)
