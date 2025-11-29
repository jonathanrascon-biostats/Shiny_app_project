###############################################################################
#################################JR Data Import and Wrangling##################
################################################################################
library(shiny)
library(bslib)
library(tidyverse)
library(gmodels)
library(DT)
CT_washout <- read_csv("CT_washout.csv")
CT_washout$age_class <- factor(CT_washout$age_class, levels = c("Youth", "Adult", "Middle-aged", "Older-Adult"), ordered = TRUE)
CT_washout <- CT_washout %>% mutate(across(c("sex", "gender", "race", "ethnicity"), as.factor), screen_id = as.character(screen_id))

###############################################################################
#################################JR Updated App##################
################################################################################

#using page_sidebar for now, but there may be a better page format.....
ui <- page_sidebar(
  title = h2("CannTalk Recruitment"),
  sidebar = sidebar(
    #Inputs are enrollment status and a categorical factor.
    selectInput("status", "Enrollment", choices = c("Total Enrollment" = "enrolled", 
                                                    "Online Screener" = "screen_result", "Phone Screening" = "phone_result", 
                                                    "Zoom Visit" = "zoom_result", "In-Person Visit" = "enrolled_result", 
                                                    "Recruitment Source" = "source"), multiple = FALSE, selected = "enrolled"),
    selectInput("factor", "Category", choices = c("Age Group" = "age_class", "Sex" = "sex","Race" = "race","Gender" = "gender", 
                                                      "Hispanic/Latinx" = "ethnicity"), multiple = FALSE, selected = "age_class"),
#actionButton is linked to the contingency table; click to generate test results   

    actionButton("action", "Generate"),
#Switches for TRUE/FALSE for each test on the contingency table:
    
    input_switch(id = "expected", label = "Expected Values", value = FALSE),
    input_switch(id = "prop.r", label = "Row Proportions", value = FALSE),
    input_switch(id = "prop.c", label = "Column Proportions", value = FALSE),
    input_switch(id = "prop.t", label = "Total Proportions", value = FALSE),
    input_switch(id = "prop.chisq", label = "Chi-sq Proportions", value = FALSE),
    input_switch(id = "chisq", label = "Chi-square test", value = FALSE),
    input_switch(id = "fisher", label = "Fisher Exact", value = FALSE),
    input_switch(id = "resid", label = "Residuals", value = FALSE),
    input_switch(id = "sresid", label = "Standardized Resids", value = FALSE),
    input_switch(id = "asresid", label = "Adjusted Standarized", value = FALSE)
  ),
  #navset_card_table allows us to switch between pages
  navset_card_tab(
    nav_panel("Description", p("Project CannTalk is a longitudinal observational study tracking habitual cannabis users over the course of approximately 9 months. 
                               With the majority of US states having adopted legislation to medically and/or recreationally legalize cannabis, public perception of 
                               the drug is now overwhelmingly favorable. Increased access and prevalence of use are accompanied by perceptions of low health risk 
                               and/or of therapeutic benefits associated with cannabis use. Aside from evidence for symptom relief in certain medical conditions, 
                               evidence regarding therapeutic effects of cannabis for many conditions remains elusive, leaving the decision regarding when and how 
                               to use cannabis to the user. Both therapeutic and recreational reasons (motives) for CU (cannabis use) are largely shaped through 
                               exposure to messages about the effects of cannabis, yet little is known about the source of messaging, how it is transmitted to users, 
                               how it shapes their thinking, and ultimately its association with CU patterns. This study will gather critical information about
                               message sources, cannabis-promoting content, and risk warnings being disseminated to cannabis users as well as the messages being received, 
                               their effects on CU motives, and subsequent CU. This app is meant to track recruitment efforts of the study.
                               There are many burdens-to-entry in the study: Online screening, Phone screening, Zoom Visit, In-person Visit. Of the 450 people initially eligible, only about 30% made it to enrollment.
                               This app tracks the point at which a participant washes out, i.e. the point at which they no longer progressed in the study, and those who are fully enrolled."),
                            p("App Usage: Choose an enrollment stage (up to Total Enrollment), a Category (e.g. Age Group), and any tests to perform. Click 'Generate' to perform
                               the test(s). The term 'washout' refers to: Lost to follow up, Ineligible, Not Interested(withdrew). The majority of these were Lost.")),
    nav_panel("Plots", plotOutput("plot1")),
    nav_panel("Contingency Table", textOutput("CT_title"), verbatimTextOutput("contingency1"), h5("Click 'generate' to run tests")),
    nav_panel("Raw Data", dataTableOutput("raw_data"))
  )
)
server <- function(input, output, session) {
  
  output$raw_data <- renderDataTable(CT_washout, rownames = FALSE)
#this reactive data frame removes NA values, and feeds into (1) summary data (CT_data) in the next step, and (2) into the contingency table (CT_table).
  filter_data <- reactive({CT_washout %>% drop_na(all_of(input$status))}) 
 #this reactive summarises the data into counts of the selected categories, and is called into the graph 
  CT_data <- reactive({
    CT_summary <- filter_data() %>% 
      group_by(.data[[input$status]], .data[[input$factor]]) %>% summarise(count = n(), .groups = "drop")
  })
#this object is linked to the contingency table (contingency1), and is linked to the action button using eventReactive.
  CT_table <- eventReactive(input$action, {CrossTable(x = filter_data()[[input$status]], y = filter_data()[[input$factor]], expected = input$expected, prop.r = input$prop.r, 
                                                   prop.c = input$prop.c, prop.t = input$prop.t, prop.chisq = input$prop.chisq, chisq = input$chisq, fisher = input$fisher, 
                                                   resid = input$resid, sresid = input$sresid, asresid = input$asresid, format = "SPSS", dnn = c("Status", "Factor"))})
  
  CT_title_text <- eventReactive(input$action, {
    paste("Cross-Tabulation of ", input$status, " and ", input$factor, sep = "")
  })
#simple faceted plot that displays the various categories. Some aspects such as the legend or labels on the x-axis need to be cleaned up
  output$plot1 <- renderPlot({
    ggplot(CT_data(), aes(x = .data[[input$status]], y = count, fill = .data[[input$factor]]))+
      geom_col(width = .5) + scale_y_continuous(n.breaks = 10) + facet_wrap(~.data[[input$factor]], scales = "free_y")
  }) 
#output text that runs homogeneity or independence tests. Would like to add Text to the blank start screen, e.g. 'Need to select tests"
  output$contingency1 <- renderPrint({CT_table()})
  output$CT_title <-renderText({CT_title_text()})
}

shinyApp(ui, server)



