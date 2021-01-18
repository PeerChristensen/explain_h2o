
library(h2o)
library(tidyverse)
library(ggbeeswarm)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

test_hf <- read_csv("test.csv") %>% as.h2o()


# # Methods for an AutoML object
# h2o.varimp_heatmap()
# h2o.model_correlation_heatmap()
# h2o.pd_multi_plot()


# Methods for an H2O model
h2o.feature_frequencies(mod,test_hf) %>%
  as_tibble() %>%
  gather() %>%
  group_by(key) %>%
  summarise(m=mean(value))%>%
  arrange(desc(m))

h2o.feature_interaction(mod,test_hf)

h2o.feature_interaction(mod,test_hf,  max_interaction_depth = 10,
                        max_tree_depth = 10)

h2o.partialPlot(mod,test_hf,cols="eqpdays")

h2o.shap_explain_row_plot(mod, test_hf, row_index = 510,top_n_features = 5)
h2o.shap_summary_plot(mod, test_hf, top_n_features = 10)
h2o.pd_plot(mod, test_hf,column = "eqpdays")
h2o.pd_plot(mod, test_hf, row_index = 5100, column = "eqpdays")
h2o.ice_plot(mod, test_hf, column = "eqpdays")

h2o.explain_row(mod, test_hf, row_index = 510,top_n_features = 2)

h2o.explain(mod, test_hf,top_n_features = 2)

