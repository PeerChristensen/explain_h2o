# feature importance
library(h2o)
library(tidyverse)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

h2o.varimp_plot(mod) 

h2o.varimp(mod) %>%
  as_tibble() %>%
  top_n(10) %>%
  dplyr::select(variable,scaled_importance) %>%
  ggplot(aes(reorder(variable,scaled_importance),scaled_importance)) +
  geom_col(fill="yellow4") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Feature Importance")
