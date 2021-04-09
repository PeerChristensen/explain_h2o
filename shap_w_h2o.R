
library(h2o)
library(tidyverse)
library(ggbeeswarm)

h2o.init()

path <- here::here()
file <- list.files(pattern="GBM",recursive = T) 

mod <- h2o.loadModel(glue::glue("{path}/{file}"))

test_hf <- read_csv("test.csv") %>% as.h2o()

SHAP_values <- h2o.predict_contributions(mod, test_hf)

shap_df <- SHAP_values %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)),
         shap_force = mean(shap_value)) %>% 
  ungroup()

vars <- h2o.varimp(mod) %>%
  as_tibble() %>%
  slice(1:5) %>%
  pull(variable)

# SHAP contribution plot
shap_df %>%
  filter(feature %in% vars) %>%
  sample_frac(.1) %>%
  ggplot(aes(x = shap_value, y = reorder(feature, shap_importance))) +
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.9, alpha = 0.1, width = 0.15) +
  xlab("SHAP value") +
  ylab(NULL) +
  theme_minimal(base_size = 15)

# SHAP importance plot
shap_df %>% 
  filter(feature %in% vars) %>%
  sample_frac(.1) %>%
  select(feature, shap_importance) %>%
  distinct() %>% 
  ggplot(aes(x = reorder(feature, shap_importance), 
             y = shap_importance)) +
  geom_col(fill = 'black') +
  coord_flip() +
  xlab(NULL) +
  ylab("mean(|SHAP value|)") +
  theme_minimal(base_size = 15)

# Shapley-based dependence plots for a numerical feature
SHAP_values %>%
  as.data.frame() %>%
  select(-BiasTerm) %>% 
  mutate(eqpdays_feature_values = as.vector(test_hf$eqpdays)) %>% 
  sample_frac(.1) %>%
  ggplot(aes(x = eqpdays_feature_values, y = eqpdays)) +
  geom_point(aes(color = eqpdays), width = 0.1) +
  scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
  theme_minimal(base_size = 15) +
  geom_smooth()

