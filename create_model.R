# create model

library(tidyverse)
library(h2o)
library(caret)

df <- read_csv("Telecom_customer_churn.csv") %>%
  select(-Customer_ID) %>%
  select(churn,everything()) %>%
  sample_frac(.3) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(churn = factor(churn))

ind <- createDataPartition(df$churn,p=.7,list=F)

train <- df[ind,]
write_csv(train, "train.csv")
test <- df[-ind,]
write_csv(test, "test.csv")

y <- "churn"
x <- df %>% select(-churn) %>% names()

h2o.init(nthreads = -1, max_mem_size = "15G")

train_hf <- as.h2o(train)
test_hf <- as.h2o(test)

aml <- h2o.automl(x=x,y=y,training_frame = train_hf,
                  max_runtime_secs = 100)

aml@leaderboard

model <- h2o.getModel(aml@leaderboard[3,1]) # GBM

unlink("model",recursive = T)

h2o.saveModel(model,"model")


