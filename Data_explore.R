getwd()

library(tidyverse)


CT_data <- read_csv("CT_recruit_data.csv")
table(CT_data$age_class)
summary(CT_data)

CT_data$age_class <- factor(CT_data$age_class, levels = c("Youth", "Adult", "Middle-aged", "Older-Adult"), ordered = TRUE)
CT_data <- CT_data %>% mutate(across(c("sex", "gender", "race", "ethnicity"), as.factor))
CT_data$age <- as.numeric(CT_data$age)
CT_data$screen_id <- as.character(CT_data$screen_id)

summary(CT_data)

table(CT_data$screen_result)
