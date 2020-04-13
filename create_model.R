# creating a binary classification model with H2O AutoML
# Peer Christensen
# April 2020

# ------------------------------------------------------
# PACKAGES
# ------------------------------------------------------

library(tidyverse)
library(h2o)
library(caret)

# ------------------------------------------------------
# DATA PREPROCESSING
# ------------------------------------------------------

df <- read_csv("Telecom_customer_churn.csv") %>%
  select(-Customer_ID) %>%
  select(churn,everything()) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(churn = factor(churn))

# ------------------------------------------------------
# DATA PARTITIONING
# ------------------------------------------------------

ind <- createDataPartition(df$churn,p=.7,list=F)

train <- df[ind,]
write_csv(train, "train.csv")
test <- df[-ind,]
write_csv(test, "test.csv")

y <- "churn"
x <- setdiff(names(df),y)

# ------------------------------------------------------
# MODEL TRAINING
# ------------------------------------------------------

h2o.init(nthreads = -1, max_mem_size = "15G")

train_hf <- as.h2o(train)
test_hf <- as.h2o(test)

aml <- h2o.automl(x=x,y=y,training_frame = train_hf,
                  exploitation_ratio=0.1,
                  max_runtime_secs = 1000,stopping_metric = "AUCPR",
                  sort_metric = "AUCPR")

# ------------------------------------------------------
# SAVING A MODEL
# ------------------------------------------------------

aml@leaderboard

model <- h2o.getModel(aml@leaderboard[3,1]) # GBM 0.659 aucpr

unlink("model",recursive = T)

h2o.saveModel(model,"model")

h2o.shutdown(prompt = F)

