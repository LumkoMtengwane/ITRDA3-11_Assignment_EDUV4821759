library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Importing Data
campus_df <- read_csv("graduate_survey.csv")

# Data Cleaning
campus_df2 <- campus_df %>% select("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment") %>%
  na.omit()

campus_df2 <- campus_df2 %>% 
  mutate(Campus = recode(Campus, "Umhlanga Campus" = "Durban Campus", 
                         "Mbombela" = "Nelspruit", 
                         "Nelson Mandela Bay Campus" = "Port Elizabeth Campus", 
                         "Mowbray Campus" = "Tyger Valley Campus"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Graduate Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Programming Languages", tabName = "proglang", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("AI Search", tabName = "aisearch", icon = icon("search")),
      menuItem("Web Frameworks", tabName = "webframework", icon = icon("globe")),
      menuItem("Platforms", tabName = "platforms", icon = icon("desktop")),
      menuItem("AI Tools", tabName = "aitools", icon = icon("robot")),
      menuItem("Industries by Study Field", tabName = "industries", icon = icon("briefcase")),
      menuItem("Roles by Study Field", tabName = "roles", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "proglang", plotOutput("progLangPlot")),
      tabItem(tabName = "databases", plotOutput("databasePlot")),
      tabItem(tabName = "aisearch", plotOutput("aisearchPlot")),
      tabItem(tabName = "webframework", plotOutput("webFrameworkPlot")),
      tabItem(tabName = "platforms", plotOutput("platformPlot")),
      tabItem(tabName = "aitools", plotOutput("aiToolsPlot")),
      tabItem(tabName = "industries", plotOutput("industryPlot")),
      tabItem(tabName = "roles", plotOutput("rolePlot"))
    )
  )
)

# Server
server <- function(input, output) {
  output$progLangPlot <- renderPlot({
    proglang2 <- separate_rows(campus_df2, ProgLang, sep = ";")
    sorted_prog_lang <- as.data.frame(sort(table(proglang2$ProgLang), decreasing = TRUE)[1:5])
    ggplot(sorted_prog_lang, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "lightblue", color = "magenta") +
      labs(title = "Top Programming Languages", x = "Language", y = "Count")
  })
  
  output$databasePlot <- renderPlot({
    database2 <- separate_rows(campus_df2, Databases, sep = ";")
    sorted_count_database <- as.data.frame(sort(table(database2$Databases), decreasing = TRUE)[1:5])
    ggplot(sorted_count_database, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "darkolivegreen3", color = "black") +
      labs(title = "Top Databases", x = "Database", y = "Count")
  })
  
  output$aisearchPlot <- renderPlot({
    aisearch2 <- separate_rows(campus_df2, AISearch, sep = ";")
    sorted_count_aisearch <- as.data.frame(sort(table(aisearch2$AISearch), decreasing = TRUE)[1:5])
    ggplot(sorted_count_aisearch, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "coral", color = "blue") +
      labs(title = "Top AI Search Engines", x = "AISearch", y = "Count")
  })
  
  output$webFrameworkPlot <- renderPlot({
    webf <- separate_rows(campus_df2, WebFramework, sep = ";")
    sorted_count_webframework <- as.data.frame(sort(table(webf$WebFramework), decreasing = TRUE)[1:5])
    ggplot(sorted_count_webframework, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "bisque2", color = "bisque4") +
      labs(title = "Top Web Frameworks", x = "Framework", y = "Count")
  })
  
  output$platformPlot <- renderPlot({
    plat <- separate_rows(campus_df2, Platform, sep = ";")
    sorted_count_platform <- as.data.frame(sort(table(plat$Platform), decreasing = TRUE)[1:5])
    ggplot(sorted_count_platform, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "darkorchid", color = "brown1") +
      labs(title = "Top Platforms", x = "Platform", y = "Count")
  })
  
  output$aiToolsPlot <- renderPlot({
    aitool2 <- separate_rows(campus_df2, AITool, sep = ";")
    sorted_count_aitool <- as.data.frame(sort(table(aitool2$AITool), decreasing = TRUE)[1:5])
    ggplot(sorted_count_aitool, aes(Var1, Freq)) +
      geom_bar(stat = "identity", fill = "cadetblue", color = "blue4") +
      labs(title = "Top AI Tools", x = "AI Tool", y = "Count")
  })
  
  output$industryPlot <- renderPlot({
    industry3 <- separate_rows(campus_df2, Industry, sep = ",") %>%
      group_by(StudyField, Industry) %>% count(Industry, sort = TRUE)
    ggplot(industry3, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Industries by Study Field") +
      theme_minimal()
  })
  
  output$rolePlot <- renderPlot({
    count_role <- campus_df2 %>% group_by(StudyField, Role) %>% count(Role, sort = TRUE)
    ggplot(count_role, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Top Roles by Study Field") +
      theme_minimal()
  })
}

# Run App
shinyApp(ui, server)
