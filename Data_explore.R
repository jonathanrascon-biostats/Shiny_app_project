###############################################################################
#################################JR Data Import and Wrangling##################
################################################################################
library(shiny)
library(bslib)
library(tidyverse)
library(gmodels)

CT_washout <- read_csv("CT_washout.csv")
CT_data$age_class <- factor(CT_data$age_class, levels = c("Youth", "Adult", "Middle-aged", "Older-Adult"), ordered = TRUE)
CT_data <- CT_data %>% mutate(across(c("sex", "gender", "race", "ethnicity"), as.factor))

###############################################################################
#################################JR Updated App##################
################################################################################

#using page_sidebar for now, but there may be a better page format.....
ui <- page_sidebar(
  title = "CannTalk Recruitment",
  sidebar = sidebar(
    #Inputs are enrollment status and a categorical factor.
    selectInput("status", "Enrollment", choices = c("Total Enrollment" = "enrolled", 
                                                    "Online Screener" = "screen_result", "Phone Screening" = "phone_result", 
                                                    "Zoom Visit" = "zoom_result", "In-Person Visit" = "enrolled_result", 
                                                    "Recruitment Source" = "source"), multiple = FALSE, selected = "enrolled"),
    selectInput("factor", "Category", choices = c("Age Group" = "age_class", "Sex" = "sex","Race" = "race","Gender" = "gender", 
                                                  "Hispanic/Latinx" = "ethnicity"), multiple = FALSE, selected = "age_class"),
#WORK IN PROGRESS:  added checkboxes in order to run each of the tests. They are not "reactive yet. 
#Because each option in the CrossTable function needs a TRUE/FALSE input... I am not sure how to do this yet....
  #NOT ACTIVATED YET!!
    checkboxGroupInput(inputId = "chi_test", label = "Test Output", choiceNames = c("Expected Values", "Row Proportions", 
                                                                                    "Column Proportions", "Total Proportions", 
                                                                                    "Chi-sq Proportions", "Chi-square test", "Fisher Exact", "McNemar test"),
                       choiceValues = rep(TRUE, 8))
  ),
  #navset_card_table allows us to switch between pages, and card adds the full screen option
  card(navset_card_tab(
    nav_panel("Plots", plotOutput("plot1")),
    nav_panel("Contingency Table", verbatimTextOutput("contingency1"))
  ), full_screen = TRUE)
)
server <- function(input, output, session) {
#this reactive data frame removes NA values, and feeds into (1) summary data in the next step, and (2) into the contingency table.
  filter_data <- reactive({CT_washout %>% drop_na(.data[[input$status]])}) 
 #this reactive summarises the data into counts of the selected categories, and is called into the graph 
  data <- reactive({
    CT_summary <- filter_data() %>% 
      group_by(.data[[input$status]], .data[[input$factor]]) %>% summarise(count = n(), .groups = "drop")
  })
#simple faceted plot that displays the various categories. Some aspects such as the legend or labels on the x-axis need to be cleaned up
  output$plot1 <- renderPlot({ggplot(data(), aes(x = .data[[input$status]], y = count, fill = .data[[input$factor]]))+
      geom_col(width = .5) + scale_y_continuous(n.breaks = 10) + facet_wrap(~.data[[input$factor]], scales = "free_y")
  }) 
#output text that runs homogeneity or independence tests. Still need to add "switches" for true/false options in order to run different tests.
  output$contingency1 <- renderPrint({CrossTable(x = filter_data()[[input$status]], y = filter_data()[[input$factor]], expected = TRUE, prop.r = FALSE, 
                                                 prop.c = FALSE, prop.t = FALSE, prop.chisq = TRUE, chisq = TRUE, fisher = FALSE, mcnemar = FALSE, 
                                                 resid = FALSE, sresid = FALSE, asresid = FALSE, format = "SPSS")})
}

shinyApp(ui, server)



