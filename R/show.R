library(shinydashboard)
library(shiny)
#setwd('C:/Users/axn187/Documents/learn/home credit default risk')
#data = read.csv('application_test.csv')
#n = sapply(data, is.numeric)
library(readr)
library(ggplot2)
library(dplyr)
library(VIM)

#' Brief function
#'
#' A random brief function
#'
#' @param x a numerical vector
#' @keywords Brief
#' @export
#' @example
#' brief()

show_dash = function(){
  tab1 = tabItem('Numerical',
                 fluidRow(box(
                   selectInput("var",
                               label = "Choose a variable to display",
                               choices = 'Select a column')
                 ),
                 infoBoxOutput("mean"),
                 infoBoxOutput("sd")
                 ),
                 # Boxes need to be put in a row (or column)
                 fluidRow(
                   box(plotOutput("map")),
                   box(plotOutput("box"))
                 ))
  tab2 = tabItem('Categorical',
                 fluidRow(
                   selectInput("fac",
                               label = "Choose a variable to display",
                               choices = 'Select a column'),
                   infoBoxOutput("mode"),
                   infoBoxOutput("level")
                 ),
                 # Boxes need to be put in a row (or column)
                 fluidRow(
                   box(plotOutput("bar")),
                   box(
                     title = "Frequency Table",
                     dataTableOutput("freq")
                   )
                 ))
  tab3 = tabItem('missing',plotOutput('aggr'))
  ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      sidebarMenu(
        menuItem("Numerical", tabName = "Numerical", icon = icon("dashboard")),
        menuItem("Categorical", tabName = "Categorical", icon = icon("th")),
        menuItem('Missing values', tabName = 'missing', icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(tab1,tab2,tab3)
    ))

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 10000*1024^2)
    observe({
      req(myData())
      n = sapply(myData(), is.numeric)
      updateSelectInput(session, "var", choices = names(myData())[n],
                        selected = names(myData())[n][1])
      updateSelectInput(session, "fac",choices = names(myData())[!n],
                        selected = names(myData())[!n][1])
    })
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <- read.csv(inFile$datapath, header = TRUE)
      data
    })
    output$map = renderPlot({
      req(myData())
      myData() %>% ggplot(aes_string(x = input$var)) +
        geom_histogram(bins = 10,fill = 'steelblue')
    })
    output$bar = renderPlot({
      req(myData())
      myData() %>% ggplot(aes_string(x = input$fac, fill = input$fac)) +
        geom_bar()
    })
    output$freq = renderDataTable({
      req(myData())
      table(myData()[,input$fac]) %>% as.data.frame()
    })
    output$box = renderPlot({
      req(myData())
      boxplot(myData()[,input$var])
    })
    output$mean = renderInfoBox({
      req(myData())
      infoBox('Mean',mean(myData()[,input$var]))
    })
    output$sd = renderInfoBox({
      req(myData())
      infoBox('Standard Deviation',sd(myData()[,input$var]))
    })
    output$mode = renderInfoBox({
      req(myData())
      infoBox('Mode',names(sort(table(myData()[,input$fac]), decreasing = T))[1])
    })
    output$level = renderInfoBox({
      req(myData())
      infoBox('Number of Levels',length(unique(myData()[,input$fac])))
    })
    output$aggr = renderPlot({
      req(myData())
      aggr(myData(), numbers = TRUE, combined = TRUE)

    })
  }
  return(shinyApp(ui, server))
}
