# calculate hit rates / tpr per percentile

library(tidyverse)
library(h2o)
library(yardstick)

# ------------------------------------------------------
# LOAD MODEL AND DATA
# ------------------------------------------------------

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

test_hf <- read_csv("test.csv") %>% as.h2o()

preds <- h2o.predict(mod,test_hf) %>% as_tibble()
preds$actual = test_hf %>% as_tibble() %>% pull(churn)
preds <- preds %>% select(-p0)

preds <- preds %>% arrange(p1)
preds <- preds %>%
  mutate(percentile = ntile(p1,10))

preds %>%
  mutate(hit = if_else(actual==1 & predict == 1,1,0)) %>%
  group_by(percentile) %>%
  summarise(P = sum(actual),
            TP = sum(hit)) %>%
  mutate(TPR = TP/P) %>%
  ggplot(aes(percentile,TPR)) +
  geom_col()
  

  