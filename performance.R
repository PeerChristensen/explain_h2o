
library(h2o)
library(tidyverse)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

test_hf <- read_csv("test.csv") %>% as.h2o()

# Performance metrics
perf <- h2o.performance(mod,test_hf)

# confusion matrices
cm <- h2o.confusionMatrix(mod,test_hf)
cm_f2 <- h2o.confusionMatrix(mod, test_hf, metric="f2")

# gains & lift charts
preds <- h2o.predict(mod,test_hf) %>% as_tibble()
churn <- test_hf %>% as_tibble() %>% dplyr::select(churn)
preds <- cbind(preds,churn)

library(yardstick)
options(yardstick.event_first = F)

gain <- gain_curve(preds, truth = factor(churn),p1)
autoplot(gain)

lift <- lift_curve(preds, truth = factor(churn),p1)
autoplot(lift)

lift %>%
  ggplot(aes(.n_events,.lift)) +
  geom_line(colour="yellow4") +
  theme_minimal()


h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  ggplot(aes(group,cumulative_lift)) +
  geom_line()

library(modelplotr)
test <- as_tibble(test_hf)
scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("test"),
                                               dataset_labels = list("test data"),
                                               models = list("mod"),  
                                               model_labels = list("XgBoost"), 
                                               target_column="churn",
                                               ntiles = 100)

scores_and_ntiles$ntl_0 <- scores_and_ntiles$ntl_p0 
scores_and_ntiles$ntl_1 <- scores_and_ntiles$ntl_p1 

plot_input <- plotting_scope(prepared_input = scores_and_ntiles)

plot_cumgains(data = plot_input)
plot_cumlift(data = plot_input)
