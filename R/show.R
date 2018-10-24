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
#' show_dash()

show_dashboard = function(){
  tab1 = shinydashboard::tabItem('Numerical',
                 shiny::fluidRow(
                   shinydashboard::box(
                   shiny::selectInput("var",
                               label = "Choose a variable to display",
                               choices = 'Select a column')
                 ),
                 shinydashboard::infoBoxOutput("mean"),
                 shinydashboard::infoBoxOutput("sd")
                 ),
                 # Boxes need to be put in a row (or column)
                 shiny::fluidRow(
                   shinydashboard::box(shiny::plotOutput("map")),
                   shinydashboard::box(shiny::plotOutput("box"))
                 ))
  tab2 = shinydashboard::tabItem('Categorical',
                 shiny::fluidRow(
                   shiny::selectInput("fac",
                               label = "Choose a variable to display",
                               choices = 'Select a column'),
                   shinydashboard::infoBoxOutput("mode"),
                   shinydashboard::infoBoxOutput("level")
                 ),
                 # Boxes need to be put in a row (or column)
                 shiny::fluidRow(
                   shinydashboard::box(shiny::plotOutput("bar")),
                   shinydashboard::box(
                     title = "Frequency Table",
                     shiny::dataTableOutput("freq")
                   )
                 ))
  tab3 = shinydashboard::tabItem('missing',shiny::plotOutput('aggr'))
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Basic dashboard"),
    shinydashboard::dashboardSidebar(
      shiny::fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Numerical", tabName = "Numerical", icon = shiny::icon("dashboard")),
        shinydashboard::menuItem("Categorical", tabName = "Categorical", icon = shiny::icon("th")),
        shinydashboard::menuItem('Missing values', tabName = 'missing', icon = shiny::icon("th"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(tab1,tab2,tab3)
    ))

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 10000*1024^2)
    shiny::observe({
      req(myData())
      n = sapply(myData(), is.numeric)
      shiny::updateSelectInput(session, "var", choices = names(myData())[n],
                        selected = names(myData())[n][1])
      shiny::updateSelectInput(session, "fac",choices = names(myData())[!n],
                        selected = names(myData())[!n][1])
    })
    myData <- shiny::reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <- read.csv(inFile$datapath, header = TRUE)
      data
    })
    output$map = shiny::renderPlot({
      `%>%` <- dplyr::`%>%`
      req(myData())
      myData() %>% ggplot2::ggplot(ggplot2::aes_string(x = input$var)) +
        ggplot2::geom_histogram(bins = 10,fill = 'steelblue')
    })
    output$bar = shiny::renderPlot({
      `%>%` <- dplyr::`%>%`
      req(myData())
      myData() %>% ggplot2::ggplot(ggplot2::aes_string(x = input$fac, fill = input$fac)) +
        ggplot2::geom_bar()
    })
    output$freq = shiny::renderDataTable({
      `%>%` <- dplyr::`%>%`
      req(myData())
      table(myData()[,input$fac]) %>% as.data.frame()
    })
    output$box = shiny::renderPlot({
      req(myData())
      boxplot(myData()[,input$var])
    })
    output$mean = shinydashboard::renderInfoBox({
      req(myData())
      shinydashboard::infoBox('Mean',mean(myData()[,input$var]))
    })
    output$sd = shinydashboard::renderInfoBox({
      req(myData())
      shinydashboard::infoBox('Standard Deviation',sd(myData()[,input$var]))
    })
    output$mode = shinydashboard::renderInfoBox({
      req(myData())
      shinydashboard::infoBox('Mode',names(sort(table(myData()[,input$fac]), decreasing = T))[1])
    })
    output$level = shinydashboard::renderInfoBox({
      req(myData())
      shinydashboard::infoBox('Number of Levels',length(unique(myData()[,input$fac])))
    })
    output$aggr = shiny::renderPlot({
      req(myData())
      VIM::aggr(myData(), numbers = TRUE, combined = TRUE)

    })
  }
  return(shiny::shinyApp(ui, server))
}
