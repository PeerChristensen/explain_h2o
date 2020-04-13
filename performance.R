# Evaluating the performance of a binary H2O classification model
# Peer Christensen
# April 2020

# ------------------------------------------------------
# PACKAGES
# ------------------------------------------------------

library(tidyverse)
library(h2o)
library(yardstick)
library(modelplotr)

# ------------------------------------------------------
# LOAD MODEL AND DATA
# ------------------------------------------------------

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

test_hf <- read_csv("test.csv") %>% as.h2o()

# ------------------------------------------------------
# PERFORMANCE OBJECT AND METRICS
# ------------------------------------------------------

# Performance object 
perf <- h2o.performance(mod,test_hf)

# Plotting metrics and thresholds
metrics <- as.data.frame(h2o.metric(perf))
head(metrics)

# all metrics
metrics %>%
  select(-tns,-fps,-tps,-fns) %>%
  gather(metric, value, f1:tpr) %>%
  ggplot(aes(x = threshold, y = value, group = metric)) +
  facet_wrap(~ metric, ncol = 4, scales = "free") +
  geom_line(colour = "#BBC605") +
  theme_h2o_dark()

# AUC
h2o.auc(mod,xval=T)

options(yardstick.event_first = F)

roc <- roc_curve(preds,truth = factor(churn),p1)
autoplot(roc)

# AUCPR
h2o.aucpr(mod,xval=T)


# PR curve from H2O metrics
metrics %>%
  ggplot(aes(recall,precision)) + 
  geom_line(size = 1) +
  ylim(c(0,1)) +
  theme_minimal()

#PR curve with yardstick

# make "1" the prediction of interest
options(yardstick.event_first = F)

pr <- pr_curve(preds,truth = factor(churn),p1)
autoplot(pr)

# confusion matrices
cm    <- h2o.confusionMatrix(mod,test_hf)
cm_f2 <- h2o.confusionMatrix(mod, test_hf, metric="f2")

# ------------------------------------------------------
# GAINS AND LIFT CHARTS WITH YARDSTICK
# ------------------------------------------------------

# Get predictions
preds <- h2o.predict(mod,test_hf) %>% as_tibble()
churn <- test_hf %>% as_tibble() %>% dplyr::select(churn)
preds <- cbind(preds,churn)

# Predictions with max F2 threshold (not used)
f2_threshold <- h2o.F2(perf) %>% as_tibble() %>% filter(f2 == max(f2)) %>% pull(threshold)
preds_f2 <- preds %>%
  mutate(predict = if_else(p1 >= f2_threshold,1,0))

# make "1" the prediction of interest
options(yardstick.event_first = F)

# Cumulative gains with autoplot (yardstick)
gain <- gain_curve(preds, truth = factor(churn),p1)
autoplot(gain)

# Cumulative gains with ggplot
prop_churn <- sum(preds$churn)/nrow(preds)*100
coords <- tibble(x=c(0,prop_churn,100), y = c(0,100,100))

gain %>%
  ggplot(aes(.percent_tested,.percent_found)) +
  geom_line(colour="#BBC605",size=1) +
  theme_h2o_dark() +
  geom_polygon(data=coords, mapping=aes(x=coords$x,y=coords$y), alpha = .2,fill = "#BBC605")

# Cumulative lift with Yardstick
lift <- lift_curve(preds, truth = factor(churn),p1)
autoplot(lift)

# or with ggplot
lift %>%
  ggplot(aes(.percent_tested,.lift)) +
  geom_line(colour="yellow4", size=1) +
  theme_minimal()

# with h2o ??
h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  ggplot(aes(cumulative_data_fraction,cumulative_gain)) +
  geom_line()

h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  ggplot(aes(cumulative_data_fraction,cumulative_lift)) +
  geom_line()

# plot all 
# it looks like cum. gains is actually cum. capture rate
h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  select(-group) %>%
  gather(variable, value, lower_threshold:cumulative_gain) %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, group = variable)) +
  facet_wrap(~ variable, ncol = 4, scales = "free") +
  geom_line(colour = "#BBC605") +
  theme_h2o_dark()

# with modelplotr
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
