# iml

library(tidyverse)
library(h2o)
library(iml)


h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

train <- read_csv("train.csv")

# features
x <- train %>% dplyr::select(-churn)
# response
y <- as.numeric(train$churn)

# 3. Create custom predict function that returns the predicted values as a
#    vector (probability of purchasing in our example)
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

# example of prediction output
pred(mod, x) %>% head()

predictor <- Predictor$new(
  model = mod, 
  data = x, 
  y = y, 
  predict.fun = pred,
  class = "classification"
)

imp <- FeatureImp$new(predictor, loss = "f1")
plot(imp)

df <- rsample::attrition %>% 
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  mutate(Attrition = recode(Attrition, "Yes" = "1", "No" = "0") %>% factor(levels = c("1", "0")))

# convert to h2o object
df.h2o <- as.h2o(df)

# create train, validation, and test splits
set.seed(123)
splits <- h2o.splitFrame(df.h2o, ratios = c(.7, .15), destination_frames = c("train","valid","test"))
names(splits) <- c("train","valid","test")

# variable names for resonse & features
y <- "Attrition"
x <- setdiff(names(df), y) 

gbm <-  h2o.gbm(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 1000,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123
)

features <- as.data.frame(splits$valid) %>% dplyr::select(-Attrition)

# 2. Create a vector with the actual responses
response <- as.numeric(as.vector(splits$valid$Attrition))

# 3. Create custom predict function that returns the predicted values as a
#    vector (probability of purchasing in our example)
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

# example of prediction output
pred(gbm, features) %>% head()

predictor.gbm <- Predictor$new(
  model = gbm, 
  data = features, 
  y = response, 
  predict.fun = pred,
  class = "classification"
)

imp.gbm <- FeatureImp$new(predictor.gbm,loss="ce")
gbm.ot <- FeatureEffect$new(predictor.gbm, "OverTime") %>% plot() + ggtitle("GBM")

library(DALEX)
explainer_gbm <- explain(
  model = gbm,
  data = features,
  y = response,
  predict_function = pred,
  label = "h2o gbm",
  task="classification"
)

# this works
library(DALEXtra) ?
explainer <- explain_h2o(gbm,data = features,y = response,
                         predict_function = pred,
                         type = "classification")
            
pdp_h2o_gbm <- DALEX::variable_effect(explainer, variable = "YearsAtCompany",
                                      type="partial_dependency") 

ale_h2o_gbm <- DALEX::variable_effect(explainer, variable = "YearsAtCompany",
                                      type='accumulated_dependency') 
plot(ale_h2o_gbm)

vi<-variable_importance(explainer, type="difference")
plot(vi)



