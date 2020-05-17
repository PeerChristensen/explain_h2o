# local explanations

library(h2o)
library(tidyverse)
library(lime)

h2o.init()

file <- list.files("model",pattern="GBM") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

train <- read_csv("train.csv") %>%
  mutate(churn = as.factor(churn)) %>%
  mutate_if(is.character,factor) 

test_hf <- read_csv("test.csv") %>% as.h2o() 

preds <-  h2o.predict(mod,test_hf) %>% 
  as_tibble() %>% 
  dplyr::mutate(case = row_number()) 

cases <- preds %>%
  filter(predict == "1") %>%
  pull(case)

test <- read_csv("test.csv") %>%
  dplyr::mutate(case = row_number(),
                churn = preds$predict) %>%
  filter(case %in% cases) %>%
  dplyr::select(-case) %>%
  dplyr::select(names(train))

# run lime() on training set
explainer <- lime::lime(x = train, 
                        model = mod)

# run explain() on the explainer
library(tictoc)

tic()
explanation <- lime::explain(x = test[1:5,], 
                             explainer = explainer, 
                             labels = 1,
                             n_features = 4,
                             n_permutations = 1000,
                             feature_select = "highest_weights",
                             kernel_width = 0.5)
toc()


