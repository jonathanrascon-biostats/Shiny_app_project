###############################################################################
#################################JR Data Import and Wrangling##################
################################################################################
library(tidyverse)
library(reshape2)
library(gmodels)
library(shiny)

#Primary data: cleaned and wrangled from multiple sources. After cleaning, I downloaded
#and re-uploaded into this new project.
CT_data <- read_csv("CT_recruit_data.csv")
table(CT_data$age_class)
summary(CT_data)

#Assigned all columns to an appropriate data type.
CT_data$age_class <- factor(CT_data$age_class, levels = c("Youth", "Adult", "Middle-aged", "Older-Adult"), ordered = TRUE)
CT_data <- CT_data %>% mutate(across(c("sex", "gender", "race", "ethnicity"), as.factor))
CT_data$age <- as.numeric(CT_data$age)
CT_data$screen_id <- as.character(CT_data$screen_id)

#Here for all screening types (phone, zoom, etc.) I generalized all participants who were either
#Lost, Not interested, etc. as "washout", i.e. to quantify, in general, the point at which a 
#participant did not progress further in the study.
CT_washout <- CT_data %>% mutate(phone_result = case_when(phone_result == "Ineligible" ~ "washout",
                                                          phone_result == "Lost" ~ "washout",
                                                          phone_result == "Not interested" ~ "washout",
                                                          TRUE ~ phone_result)) %>% 
  mutate(zoom_result = case_when(zoom_result == "Lost" ~ "washout",
                                 zoom_result == "Not interested" ~ "washout",
                                 TRUE ~ zoom_result)) %>% 
  mutate(enrolled_result = case_when(is.na(zoom_result) ~ NA_character_,
                              enrolled == "Yes" ~ "Eligible",
                              enrolled == "No" ~ "washout",
                              TRUE ~enrolled))

###############################################################################
######################LAS UI Creation##########################################
##############################################################################

#First I will make the user interface.
#UI
ui <- fluidPage(
#I will add a title panel that describes what we are looking at with this app
titlePanel("Project CannTalk Participant Enrollment & Washout"),
#I will add a side bar so that users can filter data based on participant groups  
  sidebarLayout(
    sidebarPanel(
#I want the sidebar to be located on the left side of the screen and take up
#about 25% of the total screen
      width = 3,
#I will add each variable the user could potentially filter by here.

      selectInput(
        "age_class", "Age Group",
        choices = c("Older-Adult", "Middle-aged", "Adult","Youth"),
        #I have given the option to select multiple groups within each category
        multiple = TRUE
      ),
      
      selectInput(
        "sex", "Sex",
        choices = c("Male", "Female"),
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
        multiple = TRUE
      ),
      
      selectInput(
        "race", "Race",
        choices = c("All", "White",
                    "Asian",
                    "Black or African American",
                    "Multiple Races",
                    "Unsure",
                    "American Indian or Alaska Native"),
        multiple = TRUE
      ),
      
      
      selectInput(
        "ethnicity", "Hispanic/Latinx:",
        choices = c("No", "Yes", "Unsure"),
        multiple = TRUE
      ),
      
    ),
    
#Now for our plots. We want the user to see 6 different plots that
#are arranged in two rows of 3 to the right of the category selection input pane
#I want the plots to take up about 75% of the screen
    
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
######################LAS Server###############################################
###############################################################################

#Now I will make the server
server <- function(input, output, session) {
  
#The server has two real funcions. The first is to apply filters and the second
#is to generate plots based on these filters.
#Here is we will filter data based on user inputs
  filteredData <- reactive({
    data <- CT_washout
#We will use "if" so that at baseline if nothing is selected by 
#the user plots will show data from all categories

if (!is.null(input$age_class) && length(input$age_class) < 
    length(unique(CT_washout$AgeGroup))) {
    data <- subset(data, AgeGroup %in% input$age_class)
}
    
if (!is.null(input$sex) && length(input$sex) < 
        length(unique(CT_washout$Sex))) {
      data <- subset(data, Sex %in% input$Sex)
    }
    
    
    # TEMPLATE filter
    if (!is.null(input$gender) && length(input$gender) < length(unique(CT_washout$Gender))) {
      data <- subset(data, Gender %in% input$gender)
    }
    

    
    data
  })

#Now we will generate plots based on the applied filters  
  
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
  
  
#Plot Template for others
  output$plot2 <- renderPlot({
    PLOT2_CODE(filteredData())
  })
  
  output$plot3 <- renderPlot({
    YOUR_PLOT3_CODE(filteredData())
  })
  
  output$plot4 <- renderPlot({
    YOUR_PLOT4_CODE(filteredData())
  })
  
  output$plot5 <- renderPlot({
    YOUR_PLOT5_CODE(filteredData())
  })
  
  output$plot6 <- renderPlot({
    YOUR_PLOT6_CODE(filteredData())
  })
  
}

    
# ----- Run App -----
shinyApp(ui = ui, server = server)
    
    
    
    
    
    
###############################################################################
#################JR original plot code#########################################
###############################################################################

#Next is plots of washout stages for each age group: plot 0 is total enrolled, yes or no. Plot
#1 shows the number who washed out and who moved forward at the online screening stage.
#Plot 2 shows the number who washed out at the phone screening stage. Plot 3 show the number
#who washed out at the Zoom screening stage. Plot 4 shows the number who washed out at 
#the in-person visit stage.

CT_plot0 <- CT_washout %>% filter(!is.na(enrolled)) %>% ggplot(aes(x = "", y = age_class, fill = age_class))

CT_plot0 + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
  labs(title ="Total Enrolled") + theme_void() + facet_wrap(~enrolled)

CT_plot1 <- CT_washout %>% filter(!is.na(screen_result)) %>%  ggplot(aes(x ="", y = age_class, fill = age_class))

CT_plot1 + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
  labs(title ="Completed Online Screen") + theme_void() + facet_wrap(~screen_result)

CT_plot2 <- CT_washout %>% filter(!is.na(phone_result)) %>% ggplot(aes(x = "", y = age_class, fill = age_class))

CT_plot2 + geom_bar(width = 1, stat ="identity") + coord_polar("y", start = 0) +
  labs(title = "Completed Phone Screen") + theme_void() + facet_wrap(~phone_result)

CT_plot3 <- CT_washout %>% filter(!is.na(zoom_result)) %>%  ggplot(aes(x = "", y = age_class, fill = age_class ))

CT_plot3 + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
  labs(title = "Completed Zoom") + theme_void() + facet_wrap(~zoom_result)

CT_plot4 <- CT_washout %>% filter(!is.na(enrolled_result)) %>% ggplot(aes(x="", y = age_class, fill = age_class))

CT_plot4 + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
  labs(title = "Completed Enrollment") + theme_void() + facet_wrap(~enrolled_result)

#you can see that the format for all these graphs are the same. What I am struggling with 
#now is turning these into a generalized plot function that will take as inputs the 
#columns I want to call (e.g. a factor variable such as age_class or gender, and a
#results column such as screen_result or enrolled)
