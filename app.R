###########################
# START LOADING LIBRARIES #
###########################

# Shiny Libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Data Manipulation Libraries
library(tidyverse)

# Graphinc Libraries
library(plotly)
library(quantmod)

# Data Table Libraries
library(DT)

source("functions.R")

########################
# END LOADING LIBARIES #
########################



#####################
# START UI FUNCTION #
#####################

ui <- dashboardPage(
  
  # Start Dashboard Header
  dashboardHeader(
    title = "Vanguard ETF Explorer",
    titleWidth = 300
  ),
  # End Dashboard Header
  
  # Start Dashboard Sidebar
  dashboardSidebar(
    
    width = 300,
    
    # Start Action Button To Refresh Data
    actionButton(inputId = "refreshData",
                 label = "Refresh ETF Data",
                 width = "90%"
    )
    # End Action Button To Refresh Data
    
  ),
  # End Dashboard Sidebar
  
  # Start Dashboard Body
  dashboardBody(
    
    withSpinner(
      size = 3,
      DT::dataTableOutput("etfDataTable")
    )
    
  )
  # End Dashboard Body
  
)

###################
# END UI FUNCTION #
###################



#########################
# START SERVER FUNCTION #
#########################

server <- function(input, output, session) {
  
  # Start Reactive Data Load
  parsedData <- eventReactive(input$refreshData, {
    
    # Scrapes Vanguard Page for Data on All ETFs
    jsScrape(url = "https://investor.vanguard.com/etf/list#/etf/asset-class/month-end-returns")
    
    # Parses Data from Scraped Vanguard Page
    df <- parseHTML()
    
    # Return Dataframe as Reactive Object
    df
    
  })
  # End Reactive Data Load
  
  
  # Start Raw DataTable Output
  output$etfDataTable <- DT::renderDataTable({
    
    req(parsedData())
    
    DT::datatable(parsedData(),
                  extensions = c("Scroller","ColReorder","KeyTable"),
                  options = list(pageLength = 20,
                                 paging = TRUE,
                                 searching = TRUE,
                                 scroller = TRUE,
                                 ordering = TRUE,
                                 searchHighlight = TRUE,
                                 scrollY = 700,
                                 scrollX = TRUE,
                                 colReorder = TRUE,
                                 keys = TRUE)
    )
    
  })
  # End Raw DataTable Output
  
}

#######################
# END SERVER FUNCTION #
#######################



#################
# START RUNNING #
#################

shinyApp(ui = ui, server = server)

###############
# END OF FILE #
###############