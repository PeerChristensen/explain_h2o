# Evaluating feature importance in a binary H2O classification model
# Peer Christensen
# April 2020

# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------
library(tidyverse)
library(h2o)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

train <- read_csv("train.csv")
train_hf <- h2o.uploadFile(path = "train.csv")

# ------------------------------------------------------
# PLOT WITH H2O
# ------------------------------------------------------
 # get variable classes
var_classes <- train %>% map_df(class) %>% gather(variable,class)

# select best numeric predictors
vars <- h2o.varimp(mod) %>%
  as_tibble() %>%
  top_n(6) %>%
  filter(variable != "crclscod") %>% #remove problematic variables
  inner_join(var_classes) %>%
  filter(class == "numeric") %>%
  pull(variable)

# select best character/factor predictors
# vars <- h2o.varimp(mod) %>%
#   as_tibble() %>%
#   top_n(6) %>%
#   filter(variable != "crclscod") %>% #remove problematic variables
#   inner_join(var_classes) %>%
#   filter(class == "character") %>%
#   pull(variable)

h2o.partialPlot(object = mod, data = train_hf, cols = vars)

# ------------------------------------------------------
# PLOT WITH GGPLOT
# ------------------------------------------------------

pdps <- h2o.partialPlot(object = mod, data = train_hf, cols = vars,plot=F) 

pdp_table <- tibble()
for (i in pdps) {
  
  i <- as_tibble(i)
  i <- i %>%
    mutate(upper = mean_response + stddev_response,
           lower = mean_response - stddev_response)
  
  i$variable <- names(i)[1]
  names(i)[1] <- "value"
  pdp_table <- rbind(pdp_table,i)
}

pdp_table %>%
  ggplot(aes(value,mean_response)) +
  geom_line(size=1,colour="yellow4") +
  geom_ribbon(aes(ymin=lower,ymax=upper),
              alpha=.2,colour=NA,fill="yellow4") +
  facet_wrap(~variable,scales="free_x") +
  theme_minimal() +
  # if dealing with long character values
  #theme(axis.text.x = element_text(angle=45)) +
  NULL

pdp_table %>%
  ggplot(aes(value,mean_response)) +
  geom_line(size=1,colour="#BBC605") +
  geom_ribbon(aes(ymin=lower,ymax=upper),
              alpha=.2,colour=NA,fill="#BBC605") +
  facet_wrap(~variable,scales="free_x") +
  theme_h2o_dark() +
  # if dealing with long character values
  #theme(axis.text.x = element_text(angle=45)) +
  NULL

  