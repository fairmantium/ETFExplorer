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
                 label = "Refresh ETF Data from Vanguard",
                 width = "90%"
    )
    # End Action Button To Refresh Data
    
  ),
  # End Dashboard Sidebar
  
  # Start Dashboard Body
  dashboardBody(
    
    # Start Tabs
    tabsetPanel(
      
      # Start Dot Plot Panel
      tabPanel(
        title = "Dot Plot",
        
        selectInput(
          inputId = "dotInput",
          label = "Select Return Period",
          choices = c("YTD" = 'ytd',
                      "1 Year" = "one_yr",
                      "5 Year" = "five_yr",
                      "10 Year" = "ten_yr",
                      "Since Inception" = "since"),
          selected = "since",
          width = "25%"
        ),
        
        withSpinner(
          size = 3,
          plotOutput(outputId = "dotPlot",
                     width = "100%",
                     height = "900px"
                     )
        )
        
      ),
      # End Dot Plot Panel
      
      # Start Data Table Tab
      tabPanel(
        title = "Data Table",
        
        withSpinner(
          size = 3,
          DT::dataTableOutput("etfDataTable")
        )
        
      )
      # End Data Table Tab
      
    )
    # End Tabs
    
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
  
  # Start Reactive Data Load from Scraping Vanguard Websites
  parsedData <- eventReactive(input$refreshData, {
    
    # Scrapes Vanguard Page for Data on All ETFs
    jsScrape(url = "https://investor.vanguard.com/etf/list#/etf/asset-class/month-end-returns")
    
    # Parses Data from Scraped ETF Vanguard Page
    etfData <- parseHTML()
    etfData$fund_type <- "ETF"
    
    # Scrapes Vanguard Mutual Fund Data
    jsScrape(url = "https://investor.vanguard.com/etf/list#/mutual-funds/asset-class/month-end-returns")
    
    # Parses Data from Scraped Mutual Fund Vanguard Page
    mutualData <- parseHTML()
    mutualData$fund_type <- "Mutual"
    
    # Bind ETF and Mutual Dataframes Together
    df <- rbind(etfData, mutualData) %>% select(fund_type, fund_names, ticker,
                                                asset_class, expense_ratio, price,
                                                sec_yield_clean, ytd, one_yr, five_yr, ten_yr,
                                                since, inception)
    
    # Return Dataframe as Reactive Object
    df
    
  })
  # End Reactive Data Load
  
  
  # Start Graph
  output$dotPlot <- renderPlot({
    
    req(parsedData())
    
    df <- parsedData() %>% gather(return_type, return, -fund_type, -fund_names, -ticker,
                                  -asset_class, -expense_ratio, -price, -sec_yield_clean,
                                  -inception) %>%
      filter(return_type == input$dotInput)
    
    g <- ggplot(df, aes(x = expense_ratio, y = return, color = fund_type)) +
      geom_point(size = 5, alpha = 0.5 )
    
    g
    
  })
  # End Graph
  
  
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