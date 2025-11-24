###############################################################################
############################### JR Data Import ################################
###############################################################################
library(shiny)
library(tidyverse)
library(readr)

# Primary data
CT_data <- read_csv("CT_recruit_data.csv")

# Set correct data types
CT_data$age_class <- factor(CT_data$age_class, levels = c("Youth", "Adult", "Middle-aged", "Older-Adult"), ordered = TRUE)
CT_data <- CT_data %>% mutate(across(c("sex", "gender", "race", "ethnicity"), as.factor))
CT_data$age <- as.numeric(CT_data$age)
CT_data$screen_id <- as.character(CT_data$screen_id)

# Create washout column
CT_washout <- CT_data %>%
  mutate(phone_result = case_when(phone_result %in% c("Ineligible","Lost","Not interested") ~ "washout",
                                  TRUE ~ phone_result),
         zoom_result = case_when(zoom_result %in% c("Lost","Not interested") ~ "washout",
                                 TRUE ~ zoom_result),
         enrolled_result = case_when(is.na(zoom_result) ~ NA_character_,
                                     enrolled == "Yes" ~ "Eligible",
                                     enrolled == "No" ~ "washout",
                                     TRUE ~ enrolled))

###############################################################################
############################### LAS UI #########################################
###############################################################################

ui <- fluidPage(
  titlePanel("Project CannTalk Participant Enrollment & Washout"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        "age_class", "Age Group",
        choices = c("Older-Adult", "Middle-aged", "Adult","Youth"),
        selected = c("Older-Adult", "Middle-aged", "Adult","Youth"), 
        multiple = TRUE
      ),
      
      selectInput(
        "sex", "Sex",
        choices = c("Male", "Female"),
        selected = c("Male", "Female"),  # default all
        multiple = TRUE
      ),
      
      selectInput(
        "gender", "Gender",
        choices = c("Cis-Woman",
                    "Cis-Man",
                    "Prefer not to answer",
                    "Multiple Identities",
                    "Non-binary",
                    "Genderqueer/gender non-conforming",
                    "Transgender Woman/Trans Woman",
                    "Transgender Man/Trans Man"),
        selected = c("Cis-Woman",
                     "Cis-Man",
                     "Prefer not to answer",
                     "Multiple Identities",
                     "Non-binary",
                     "Genderqueer/gender non-conforming",
                     "Transgender Woman/Trans Woman",
                     "Transgender Man/Trans Man"),  # default all
        multiple = TRUE
      ),
      
      selectInput(
        "race", "Race",
        choices = c("White",
                    "Asian",
                    "Black or African American",
                    "Multiple Races",
                    "Unsure",
                    "American Indian or Alaska Native"),
        selected = c("White",
                     "Asian",
                     "Black or African American",
                     "Multiple Races",
                     "Unsure",
                     "American Indian or Alaska Native"),  # default all
        multiple = TRUE
      ),
      
      selectInput(
        "ethnicity", "Hispanic/Latinx:",
        choices = c("No", "Yes", "Unsure"),
        selected = c("No", "Yes", "Unsure"),  # default all
        multiple = TRUE
      )
      
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(4, plotOutput("plot1")),
        column(4, plotOutput("plot2")), 
        column(4, plotOutput("plot3"))
      ),
      
      fluidRow(
        column(4, plotOutput("plot4")),
        column(4, plotOutput("plot5")),
        column(4, plotOutput("plot6"))
      )
      
    )
  )
)

###############################################################################
############################### LAS SERVER ####################################
###############################################################################

server <- function(input, output, session) {
  
  # Reactive filtered data
  filteredData <- reactive({
    data <- CT_washout
    
    if (!is.null(input$age_class)) data <- subset(data, age_class %in% input$age_class)
    if (!is.null(input$sex)) data <- subset(data, sex %in% input$sex)
    if (!is.null(input$gender)) data <- subset(data, gender %in% input$gender)
    if (!is.null(input$race)) data <- subset(data, race %in% input$race)
    if (!is.null(input$ethnicity)) data <- subset(data, ethnicity %in% input$ethnicity)
    
    data
  })
  
  # Plot 1
  output$plot1 <- renderPlot({
    filteredData() %>%
      filter(!is.na(enrolled)) %>%
      count(enrolled, name = "n") %>% 
      ggplot(aes(x = "", y = n, fill = enrolled)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = "Total Enrolled") +
      theme_void() +
      scale_fill_manual(values = c("Yes" = "#1f77b4", "No" = "#ff7f0e"))
  })
  
  # Plot 2-6 placeholders
  output$plot2 <- renderPlot({
    # PLOT2_CODE(filteredData())
  })
  
  output$plot3 <- renderPlot({
    # YOUR_PLOT3_CODE(filteredData())
  })
  
  output$plot4 <- renderPlot({
    # YOUR_PLOT4_CODE(filteredData())
  })
  
  output$plot5 <- renderPlot({
    # YOUR_PLOT5_CODE(filteredData())
  })
  
  output$plot6 <- renderPlot({
    # YOUR_PLOT6_CODE(filteredData())
  })
  
}

# Run App
shinyApp(ui = ui, server = server)
