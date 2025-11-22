getwd()

library(tidyverse)
library(reshape2)
library(gmodels)

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
