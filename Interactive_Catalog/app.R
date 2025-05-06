#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Things to add:
# Make courses viewable
# link to download data?
# 

library(shiny)
library(tidyverse)

# Define UI for 
ui <- fluidPage(

    # Application title
    titlePanel("American University Catalog"),

    # Sidebar with course input
    sidebarLayout(
        sidebarPanel(
          h3("Advanced Search"),
            textInput("searchBox", label = "Keyword Search"),
            actionButton("searchButton", label = "Search"),
            selectInput("yearInput",label = "Academic Year", 
                        choices =c("2024-2025", "All", "2023-2024", "2022-2023", "2021-2022",
                                   "2020-2021", "2019-2020", "2018-2019", "2017-2018", 
                                   "2016-2017", "2015-2016", "2014-2015"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
          DT::dataTableOutput("searchResults"),
          textOutput("selectedCourse")
        )
    ),
    fluidRow(column(6, textOutput("selectedCourse")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    courses <- read_csv("data_placeholder.csv")
    #implement year = All, filter or don't filter years in search results
    
    
    #search table
    output$searchResults <- DT::renderDataTable(
      courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        select(title, academic_year),
      server = TRUE, selection = "single"
    )
    
    output$selectedCourse <- reactive(
      {validate(
        need(!is.null(input$searchResults_rows_selected), "No Course Selected.")
      )
        searched <- courses
        searched <- courses %>%
          filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
          select(title, academic_year)
        searched$title[[input$searchResults_rows_selected]]
        
        }
             
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
