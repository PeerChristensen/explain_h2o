# Partial dependence
library(h2o)
library(tidyverse)

h2o.init()

file <- list.files("model") 
mod <- h2o.loadModel(glue::glue("model/{file}"))

train <- h2o.uploadFile(path = "train.csv")

# -------------------------------------------------

vars <- h2o.varimp(mod) %>%
  as_tibble() %>%
  top_n(5) %>%
  pull(variable)

h2o.partialPlot(object = mod, data = train, cols = vars)

pdps <- h2o.partialPlot(object = mod, data = train, cols = vars,plot=F) 

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
  ggplot(aes(value,mean_response, colour = variable)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=variable),
              alpha=.2,colour = NA) +
  facet_wrap(~variable,scales="free") +
  theme_minimal()
  