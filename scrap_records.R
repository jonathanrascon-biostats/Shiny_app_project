
###############################################################################
###########################Secondary Code-- Keep for records!##################
################################################################################

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
                                     zoom_result == "washout" ~ NA_character_,
                                     enrolled == "Yes" ~ "Eligible",
                                     enrolled == "No" ~ "washout",
                                     TRUE ~enrolled)) %>% 
  relocate(enrolled, .after = last_col())
###############################################################################
#################JR updated plot code#########################################
###############################################################################

#plotting function: this generalized plot function

CT_plot_summary <- function(cat_factor, status, title){
  CT_washout %>% drop_na({{status}}) %>% group_by({{status}}, {{cat_factor}}) %>% 
    summarize(count = n(), .groups = "drop") %>% ggplot(aes(x = {{status}}, y = count, fill = {{cat_factor}})) + 
    geom_col(width = .5) + labs(title = title, y = "Count", x = "Outcome")
}

#cat_factor can be: age_class, sex, gender, race, ethnicity.
#status can be: enrolled, screen_result, phone_result, zoom_result, enrolled_result

CT_plot_summary(cat_factor = age_class, status = enrolled, title = "Total Enrollment")
CT_plot_summary(cat_factor = age_class, status = screen_result, title = "Online Screening Result")
CT_plot_summary(cat_factor = age_class, status = phone_result, title = "Phone Screening Result")
CT_plot_summary(cat_factor = age_class, status = zoom_result, title = "Zoom Screening Result")
CT_plot_summary(cat_factor = age_class, status = enrolled_result, title = "In-person Outcome")

CT_plot_summary(cat_factor = sex, status = enrolled, title = "Total Enrollment")
CT_plot_summary(cat_factor = sex, status = screen_result, title = "Online Screening Result")
CT_plot_summary(cat_factor = sex, status = phone_result, title = "Phone Screening Result")
CT_plot_summary(cat_factor = sex, status = zoom_result, title = "Zoom Screening Result")
CT_plot_summary(cat_factor = sex, status = enrolled_result, title = "In-person Outcome")


write_csv(CT_washout, "CT_washout.csv")

my.table <-CrossTable(x = CT_washout$screen_result, CT_washout$age_class)
my.matrix <- as.data.frame.matrix(my.table$t)
my.matrix
