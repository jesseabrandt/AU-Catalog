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
library(DescTools)

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
                    uiOutput("selectedLink"),
                    #textOutput("moreInfo")
                    br(),
                    tags$h3("Visualizations"),
                    plotOutput("dept_class_num"),
                    plotOutput("school_class_num")
                  )),
              tabPanel(title = "Course Topics", fluidPage(
                h2("Course Topics"),
                p("Course descriptions were analyzed using Latent Dirichlet Allocation (LDA) to find 20 topics based on which words appeared together. These topics were manually classified based on top words."),
                p("Below are the results of ANOVA for the quantitative analysis topic by school."),
                tableOutput("anova_results"),
                tableOutput("pht")
                )),
              tabPanel(title = "Budget Visualizations", fluidPage(
                h2("Budget Analysis"),
                p("Note: budget for 2022 unavailable."),
                sliderInput("budget_years"),
                plotOutput("budget_comparison"),
                plotOutput("budget_by_school"),
                plotOutput(("class_budget"))
              )
                       ),
              tabPanel(title = "Yearly Analysis", fluidPage(
                h2("Choose a Year"),
                selectInput("yearInput2",label = "Academic Year", 
                            choices =c("2024-2025", "2023-2024", "2022-2023", "2021-2022",
                                       "2020-2021", "2019-2020", "2018-2019", "2017-2018", 
                                       "2016-2017", "2015-2016", "2014-2015")),
                plotOutput("class_num"),
                plotOutput("year_budget")
              ))
              
              )
)

######## SERVER
server <- function(input, output) {
  courses <- read_csv("data_placeholder3.csv")
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
  
  #Course charts
  output$school_class_num <- renderPlot(
    {
      validate(
        need(!is.null(input$searchResults_rows_selected), "No course selected.")
      )
      searched <- courses %>%
      filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
      filter(academic_year == input$yearInput) %>%
      filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                    "Undergraduate (100-499)" = c(100:499),
                                    "Graduate (500+)" = c(500:999),
                                    "Noncredit (000-099)" = c(0:99)))
    courses %>%
      filter(school == searched$school[[input$searchResults_rows_selected]]) %>%
      group_by(dept, fall) %>%
      summarize(n = n()) %>%
      ggplot(aes(x = fall, y = n, fill = dept)) +
      geom_col() +
      labs(
        title = "Number of Classes by Department (Selected School)",
        x = "Year",
        y = "Number of Classes"
      ) +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
    }
  )
  output$dept_class_num <- renderPlot(
    {
      validate(
        need(!is.null(input$searchResults_rows_selected), "")
      )
      searched <- courses %>%
        filter(grepl(tolower(input$searchBox), tolower(description))| grepl(tolower(input$searchBox), tolower(title))) %>%
        filter(academic_year == input$yearInput) %>%
        filter(course_num %in% switch(input$courseGrad, "All" = c(0:999),
                                      "Undergraduate (100-499)" = c(100:499),
                                      "Graduate (500+)" = c(500:999),
                                      "Noncredit (000-099)" = c(0:99)))
      courses %>%
        filter(dept == searched$dept[[input$searchResults_rows_selected]]) %>%
        group_by(dept, fall) %>%
        summarize(n = n()) %>%
        ggplot(aes(x = fall, y = n, fill = dept)) +
        geom_col() +
        labs(
          title = "Number of Classes in Department",
          x = "Year",
          y = "Number of Classes"
        ) +
        theme_minimal() +
        scale_fill_viridis_d() +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
    }
  )
  
  
  #Budget Analysis Section
  budget_allocation <- read_csv("budget_allocation.csv")
  budget_pct_change <- read_csv("budget_pct_change.csv")
  
  output$budget_ <- 
  output$budget_by_school <- renderPlot(
    ggplot(budget_allocation, aes(x = year, y = budget, color = school)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Budget Allocation by School (2014–2024)",
        x = "Year",
        y = "Budget (in thousands)",
        color = "School"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") + 
      scale_fill_brewer(palette = "Set2") +  
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
  )
  classes_by_school_2 <- read_csv("classes_by_school_2.csv")
  output$class_budget <- renderPlot(
    ggplot(classes_by_school_2, aes(x = year, y = budget_divided_by_classes, color = school, fill = school)) +  
      geom_line() +
      geom_point(shape = 21, size = 1) +  
      labs(
        title = "Budget Divided by Classes Over Time",
        x = "Year",
        y = "Budget per Class"
      ) +
      scale_color_brewer(palette = "Set2") + 
      scale_fill_brewer(palette = "Set2") +  
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1))  
  )
  
  ## Yearly Analysis Section
  output$class_num <- renderPlot(
    courses %>%
      filter(academic_year == input$yearInput2) |>
      group_by(school) |> 
      tally() |>
      ggplot( aes(x = reorder(school, n), y = n, fill = school)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Number of Classes by School",
        x = "School",
        y = "Number of Classes",
        fill = "School"
      ) +
      scale_color_brewer(palette = "BrBG") + 
      scale_fill_brewer(palette = "BrBG") + 
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)
      )
    
  )
  output$year_budget <- renderPlot(
    budget_allocation %>%
      filter(academic_year == input$yearInput2) %>%
    ggplot(aes(x = reorder(school, budget), y = budget, fill = school)) +
      geom_col() +
      labs(
        title = "Budget Allocation by School",
        x = "School",
        y = "Budget (in thousands)",
        color = "School"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") + 
      scale_fill_brewer(palette = "Set2") +  
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 12)
      )
  )
  ### Course Topics
  topics <- read_csv("topics_clean.csv")
  model <- aov(formula = quant~school, data = topics)
  # anova(model)
  pht <- DescTools::PostHocTest(model)
  output$anova_results <- renderTable({
    (anova(model))
  })
  output$pht <- renderTable(PostHocTest(model)$school, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
