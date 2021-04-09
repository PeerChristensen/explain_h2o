
# how to extract and customise h2o shap plots

shap_data <- h2o.shap_explain_row_plot(mod, test_hf, 
                                       row_index = as.numeric(input$shap_cust),
                                       top_n_features = input$shap_n_feat) %>%
  ggplot_build()

g$data[[1]] %>% 
  mutate(x1 = str_extract(text,"(?<=Feature: ).+(?= \n)"),
         x2 = str_extract(text,"(?<=Feature Value: ).+(?= \n)")) %>%
  mutate(x2= gsubfn("([0-9.]+)", ~format(round(as.numeric(x))), x2)) %>%
  mutate(x =paste(x1,"=",x2)) %>%
  mutate(y=ymin+ymax) %>%
  select(x,y) %>%
  arrange(desc(y)) %>%
  ggplot(aes(reorder(x,y),y)) +
  geom_col() +
  coord_flip()

