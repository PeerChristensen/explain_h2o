# create data for prediction table


library(tidyverse)

df <- read_csv("test.csv") %>%
  select(income,eqpdays,totmrc_Mean,rev_Mean) %>%
  mutate(CustomerID = row_number()) %>%
  select(CustomerID,everything()) %>%
  mutate(ChurnProbability = round(preds$p1,2),
         ChurnPrediction = if_else(preds$predict == 1,"Yes","No"))

df <- df %>%
  mutate(incomeSegment = case_when(income <= 3 ~ "low",
                                   income >= 4 & income <=6 ~ "mid",
                                   income >= 7 ~"high"),
         CLVSegment = ntile(rev_Mean,3)) %>%
  mutate(CLVSegment = case_when(CLVSegment == 3 ~ "Gold",
                                CLVSegment == 2 ~ "Silver",
                                CLVSegment == 1 ~ "Bronze")) %>%
  mutate(recency = ntile(eqpdays,4),
         frequency = ntile(totmrc_Mean,4)) %>%
  mutate(RFSegment = case_when(recency >= 3  & frequency >= 3 ~ "Best",
                               recency >= 3  &  frequency == 2 ~ "Potential",
                               recency >= 3  &  frequency == 1 ~ "New",
                               recency <= 2  &  frequency >= 3 ~ "Don't lose",
                               recency <= 2  &  frequency == 2 ~ "At risk",
                               recency <= 2  &  frequency == 1 ~ "Lost"))

df <- df %>% 
  select(CustomerID,ChurnPrediction,ChurnProbability,RFSegment,incomeSegment)

write_csv(df,"prediction_table.csv")
