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
# Make all option display all years

library(shiny)
library(tidyverse)
library(bslib)
library(shinydashboard)

# Define UI for 
ui <- fluidPage(titlePanel("American University Catalog"),
  tabsetPanel(id = "tabs",
                  tabPanel(title = "Search", fluidPage(
                    
                    # Application title
                    #titlePanel("American University Catalog"),
                    
                    # Sidebar with course input
                    sidebarLayout(
                      sidebarPanel(
                        h3("Advanced Search"),
                        textInput("searchBox", label = "Keyword Search"),
                        actionButton("searchButton", label = "Search"),
                        selectInput("yearInput",label = "Academic Year", 
                                    choices =c("2024-2025", "2023-2024", "2022-2023", "2021-2022",
                                               "2020-2021", "2019-2020", "2018-2019", "2017-2018", 
                                               "2016-2017", "2015-2016", "2014-2015")),
                        selectInput("courseGrad", label = "Course Level",
                                    choices = c("All","Undergraduate (100-499)","Graduate (500+)","Noncredit (000-099)"))
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        #plotOutput("distPlot")
                        DT::dataTableOutput("searchResults"),
                        textOutput("selectedCourse"),
                        actionButton("detailsButton", label = "Course Details")
                      )
                    ),
                    fluidRow(column(6, textOutput("selectedCourse"))),
                    
                  )),
                  tabPanel(title = "Course Details", value = "details", fluidPage(
                    h2(textOutput("selectedCourse")),
                    textOutput("selectedDescription"),
                    uiOutput("selectedLink")
                    #textOutput("moreInfo")
                  )),
              tabPanel(title = "Analysis")
              )
)

######## SERVER
server <- function(input, output) {
  courses <- read_csv("data_placeholder.csv")
  #implement year = All, filter or don't filter years in search results
  
  # Trying to find a way to just do the search once
  output$searched <- reactive(
    courses %>%
      filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
      filter(academic_year == input$yearInput) %>%
      filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                    "Undergraduate (100-499)" = c(100:499),
                                    "Graduate (500+)" = c(500:999),
                                    "Noncredit (000-099)" = c(0:99)))
  )
  
  #search table
  output$searchResults <- DT::renderDataTable(
    courses %>%
      filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
      filter(academic_year == input$yearInput) %>%
      filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                  "Undergraduate (100-499)" = c(100:499),
                                  "Graduate (500+)" = c(500:999),
                                  "Noncredit (000-099)" = c(0:99))) %>%
      select("Title" = title, "Academic Year" = academic_year),
    server = TRUE, selection = "single"
  )
  #select a course 
  output$selectedCourse <- reactive(
    {validate(
      need(!is.null(input$searchResults_rows_selected), "No Course Selected")
    )
      searched <- courses
      searched <- courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        filter(academic_year == input$yearInput) %>%
        filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                      "Undergraduate (100-499)" = c(100:499),
                                      "Graduate (500+)" = c(500:999),
                                      "Noncredit (000-099)" = c(0:99)))
      searched$title[[input$searchResults_rows_selected]]
      #searched$description[[input$searchResults_rows_selected]]
    }
    
      
    )
  #course selection server side
  #Currently filters the data again, same way search does it
  #could be updated so there's one reactive search results dataset, allowing advanced search
  output$selectedDescription <- reactive(
    {validate(
      need(!is.null(input$searchResults_rows_selected), "Please select a course.")
    )
      searched <- courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        filter(academic_year == input$yearInput) %>%
        filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                      "Undergraduate (100-499)" = c(100:499),
                                      "Graduate (500+)" = c(500:999),
                                      "Noncredit (000-099)" = c(0:99)))
      searched$description[[input$searchResults_rows_selected]]
    }
  )
  
  output$selectedLink <- renderUI(
    {validate(
      need(!is.null(input$searchResults_rows_selected), "")
    )
      searched <- courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        filter(academic_year == input$yearInput) %>%
        filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                      "Undergraduate (100-499)" = c(100:499),
                                      "Graduate (500+)" = c(500:999),
                                      "Noncredit (000-099)" = c(0:99)))
      url = a("Search Eagle Service",href = searched$eagle_service_url[[input$searchResults_rows_selected]],
              target = "_blank")
    }
  )
  

  
  #Details button switches pages
  observeEvent(input$detailsButton,{
    updateTabsetPanel(session = getDefaultReactiveDomain(), "tabs",
                      selected = "details")
  })
  
  
  output$moreInfo <- renderText(
    {
      validate(need(!is.null(input$searchResults_rows_selected), ""))
      searched <- courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        filter(academic_year == input$yearInput) %>%
        filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                      "Undergraduate (100-499)" = c(100:499),
                                      "Graduate (500+)" = c(500:999),
                                      "Noncredit (000-099)" = c(0:99)))
      course_title = searched$title[[input$searchResults_rows_selected]]
      past_versions <- courses %>%
        filter(title == course_title) %>%
        arrange(fall)
      #paste("First offered:", past_versions$academic_year[[1]])
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
