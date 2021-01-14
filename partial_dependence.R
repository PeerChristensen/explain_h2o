# Evaluating feature importance in a binary H2O classification model
# Peer Christensen
# April 2020

# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------
library(tidyverse)
library(h2o)

h2o.init()

file <- list.files("model")[1] 
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
vars <- h2o.varimp(mod) %>%
  as_tibble() %>%
  filter(variable != "crclscod") %>% #remove problematic variables
  inner_join(var_classes) %>%
  filter(class == "character") %>%
  top_n(1,relative_importance) %>%
  pull(variable)

h2o.partialPlot(object = mod, data = train_hf, cols = vars)

# ------------------------------------------------------
# PLOT WITH GGPLOT
# ------------------------------------------------------
vars=c("mou_Mean","marital")
vars <- c("dwllsize")
pdps <- h2o.partialPlot(object = mod, data = train_hf, cols = vars,plot=F) 
 # cat example
pdps<-pdps %>% 
  as_tibble() %>% 
  mutate(upper = mean_response + stddev_response,
         lower = mean_response - stddev_response)

pdps %>%
  ggplot(aes(dwllsize,mean_response,group=1)) +
  geom_line()



#numeric
if (length(vars) == 1) {
  
  pdp_table <- pdps %>% 
    as_tibble() %>% 
    mutate(upper = mean_response + stddev_response,
           lower = mean_response - stddev_response)
  
  pdp_table$variable <- names(pdp_table)[1]
  names(pdp_table)[1] <- "value"
  
  pdp_table %>%
    ggplot(aes(value,mean_response,group=1)) +
    geom_line(size=1,colour="#BBC605") +
    geom_ribbon(aes(ymin=lower,ymax=upper),
                alpha=.2,colour=NA,fill="#BBC605") +
    facet_wrap(~variable,scales="free_x") +
    # theme_h2o_dark() +
    # if dealing with long character values
    #theme(axis.text.x = element_text(angle=45)) +
    NULL
}

if (length(vars) > 1) {
  
  pdp_table <- tibble()
  
  for (i in pdps) {
    
    i <- i %>%
      as_tibble(i) %>%
      mutate(upper = mean_response + stddev_response,
             lower = mean_response - stddev_response)
    
    i$variable <- names(i)[1]
    names(i)[1] <- "value"
    pdp_table <- rbind(pdp_table,i)
  }
  
  
  pdp_table %>%
    ggplot(aes(value,mean_response,group=1)) +
    geom_line(size=1,colour="yellow4") +
    geom_ribbon(aes(ymin=lower,ymax=upper),
                alpha=.2,colour=NA,fill="yellow4") +
    facet_wrap(~variable,scales="free_x") +
    theme_minimal() +
  # if dealing with long character values
  #theme(axis.text.x = element_text(angle=45)) +
  NULL
}
pdp_table

  