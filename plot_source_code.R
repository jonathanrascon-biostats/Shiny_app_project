library(tidyverse)
library(gmodels)

CT_plot_summary <- function(cat_factor, status, title){
  CT_washout %>% drop_na({{status}}) %>% group_by({{status}}, {{cat_factor}}) %>% 
    summarize(count = n(), .groups = "drop") %>% ggplot(aes(x = {{status}}, y = count, fill = {{cat_factor}})) + 
    geom_col(width = .5) + labs(title = title, y = "Count", x = "Outcome")
}