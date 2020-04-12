# Evaluating feature importance in a binary H2O classification model
# Peer Christensen
# April 2020

# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------

library(h2o)
library(tidyverse)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

# ------------------------------------------------------
# PLOTS
# ------------------------------------------------------

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

h2o.varimp(mod) %>%
  as_tibble() %>%
  top_n(10) %>%
  dplyr::select(variable,scaled_importance) %>%
  ggplot(aes(reorder(variable,scaled_importance),scaled_importance)) +
  geom_col(fill="#BBC605",width=.7) +
  coord_flip() +
  theme_h2o_dark() +
  ggtitle("Feature Importance")

