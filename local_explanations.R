# local explanations

library(h2o)
library(tidyverse)
library(lime)

h2o.init()

file <- list.files("model",pattern="GBM") 
mod  <- h2o.loadModel(glue::glue("/Users/peerchristensen/Desktop/Projects/explain_h2o_models/model/{file}"))

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
 # filter(case %in% cases) %>%
  dplyr::select(-case) %>%
  dplyr::select(names(train))

# run lime() on training set
explainer <- lime::lime(x = train, 
                        model = mod)

# run explain() on the explainer
library(tictoc)

tic()
explanation <- lime::explain(x = test[10,], 
                             explainer = explainer, 
                             labels = 1,
                             n_features = 5,
                             n_permutations = 1000,
                             feature_select = "highest_weights",
                             kernel_width = 0.5)
toc()
 
plot_features(explanation) +
  theme_minimal(base_size=16) +
  scale_fill_manual(values = c("yellow4","grey45")) +
  theme(plot.background = element_rect(fill="#343E48",colour ="#343E48"),
      panel.background =  element_rect(fill="#343E48",colour ="#343E48"),
      panel.grid = element_line(size=.05,),
      axis.title = element_blank(),
      axis.text = element_text(colour="snow"),
      text = element_text(colour="snow"),
      strip.text = element_text(colour="snow",size=16),
      plot.title = element_text(colour="snow",size=18),
      plot.subtitle = element_text(colour="snow",size=16))

# USING DALEX

# variable names for resonse & features
y <- "churn"
x <- setdiff(names(train), y) 

# convert feature data to non-h2o objects
x_valid <- as.data.frame(test_hf)[, x]

# make response variable numeric binary vector
y_valid <- as.vector(as.numeric(as.character(test_hf$churn)))
head(y_valid)

# create custom predict function
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

# create explainer
explainer_gbm <- DALEX::explain(
  model = mod,
  type             = "classification",
  data = x_valid,
  y    = y_valid,
  predict_function = pred,
  label = "h2o gbm"
)

# create a single observation
new_cust <- test_hf[1, ] %>% as.data.frame()

# compute breakdown distances
new_cust_gbm <- iBreakDown::break_down(explainer_gbm, new_observation=new_cust)

plot(new_cust_gbm)

# ------------------------------------------------------------

library(iml)

y <- "churn"
x <- setdiff(names(train), y) 
# convert feature data to non-h2o objects
x_valid <- as.data.frame(test_hf)[, x]

# make response variable numeric binary vector
y_valid <- as.vector(as.numeric(as.character(test_hf$churn)))
head(y_valid)

predictor <- Predictor$new(mod, data = x_valid, y = test_hf$churn)
shapley <- Shapley$new(predictor, x.interest = x_valid[3, ])
#plot(shapley)

shapley2 <- shapley$results %>%
  filter(class=="p1") %>%
  mutate(phi_abs = abs(phi)) %>%
  arrange(desc(phi_abs)) %>%
  head(15)

p1_avg <- round(shapley$y.hat.average[2],3)
p1_obs <- round(shapley$y.hat.interest[2],3)

shapley2 %>%
  ggplot(aes(reorder(feature.value,phi_abs),phi)) +
  geom_col() +
  coord_flip() +
  ggtitle(paste0("Avg. prob. = ",p1_avg,"\nObs. prob. = ",p1_obs))

#  library(shapper)
# # shp <- shapper::shap(explainer_gbm, new_observation = new_cust[,-1],
# #                      data = x_valid,  predict_function = pred)
# 
# ive_mod <- individual_variable_effect(mod, 
#                                      data = x_valid, 
#                                      y = y_valid,
#                                      predict_function = pred,
#                                      new_observation = x_valid[3, ], 
#                                      nsamples = 50)
# 
# ive_mod <- individual_variable_effect(x=explainer_gbm, 
#                                       data = x_valid, 
#                                       new_observation = new_cust[,-1], 
                                     
                                      nsamples = 50)

