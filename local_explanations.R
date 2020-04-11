# local explanations

library(h2o)
library(tidyverse)
library(lime)

h2o.init()

file <- list.files("model") 
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

# 
# new_data_Churn <- new_data %>%
#   mutate(Churned30 = factor(new_predictions$predict)) %>%
#   select(Churned30,everything()) %>%
#   select(names(train_data)) %>%    # order columns
#   mutate(Customer_Key = Customer_Key) %>% # for joining later
#   filter(Churned30 == "1")  # select only churners
  
# run lime() on training set
explainer <- lime::lime(x = train, 
                        model = mod)

# run explain() on the explainer
library(tictoc)

tic()
explanation <- lime::explain(x = test, 
                             explainer = explainer, 
                             labels = 1,
                             n_features = 4,
                             n_permutations = 1000,
                             feature_select = "highest_weights",
                             kernel_width = 0.5)
toc()





library(foreach)
library(doParallel)
cores <- detectCores()
registerDoParallel(cores) 
cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)

tic()
k<- foreach (i=1:8, .combine=rbind) %dopar% {
  h2o.init()
  lime::explain(x = test[i, ], 
                explainer = explainer, 
                labels = 1,
                n_features = 4,
                n_permutations = 1000,
                kernel_width = 0.5)
}
toc()
