
# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(h2o)
library(yardstick)
library(lime)
library(shinycssloaders)
library(gsubfn)

h2o.init()

back_colour <- "#343E48"
line_colour <- "yellow4"
spinner_colour <- "#DCDCDC"

mod  <- h2o.loadModel(path=glue::glue("/Users/peerchristensen/Desktop/Projects/explain_h2o_models/model/GBM_grid__1_AutoML_20210409_084451_model_6"))

test_hf <- h2o.uploadFile("/Users/peerchristensen/Desktop/Projects/explain_h2o_models/test.csv")

# ------------------------------------------------------
# PREDICTIONS
# ------------------------------------------------------

preds <-  h2o.predict(mod,test_hf) %>% 
  as_tibble() %>% 
  dplyr::mutate(case = row_number()) %>%
  mutate(churn = as.vector(test_hf$churn))

customers <- as.character(1:nrow(test_hf))

# ------------------------------------------------------
# PERFORMANCE METRICS
# ------------------------------------------------------

perf <- h2o.performance(mod,test_hf)

metrics_table <- perf@metrics$max_criteria_and_metric_scores %>%
  as_tibble() %>%
  slice(1:10) %>%
  select(-idx)

metrics <- as.data.frame(h2o.metric(perf)) %>%
  select(-tns,-fps,-tps,-fns)  

metrics_long <- metrics %>%
  gather(metric, value, f1:tpr)

metric_names <- unique(metrics_long$metric)

auc <- round(h2o.auc(perf),2)

aucpr <- round(h2o.aucpr(perf),2)

# ------------------------------------------------------
# PERFORMANCE CURVES
# ------------------------------------------------------

#options(yardstick.event_first = F)

#roc
roc_plot <- metrics %>%
  ggplot(aes(1-specificity,tpr)) + 
  geom_line(size = 1,colour=line_colour) +
  #geom_line(size = 5, alpha = .2,colour=line_colour) +
  #geom_line(size = 2.5, alpha = .4,colour=line_colour) +
  #geom_line(size = 1, alpha = .9,colour=line_colour) +
  ylim(c(0,1)) +
  geom_abline(slope = 1, intercept = 0,linetype="dashed",colour="snow")

#prroc
baseline_height <- nrow(preds[preds$churn==1,]) / nrow(preds)
prroc_plot <- metrics %>%
  ggplot(aes(recall,precision)) + 
  geom_line(size = 1,colour=line_colour) +
  ylim(c(0,1)) +
  geom_hline(yintercept=baseline_height, linetype='dashed',colour="snow")

# gains
gain <- gain_curve(preds, truth = factor(churn),p1,event_level = 'second')
prop_churn <- sum(preds$churn)/nrow(preds)*100
coords <- tibble(x=c(0,prop_churn,100), y = c(0,100,100))

gains_plot <- gain %>%
  ggplot(aes(.percent_tested,.percent_found)) +
  geom_line(colour=line_colour,size=1) +
  geom_polygon(data=coords, mapping=aes(x=coords$x,y=coords$y), alpha = .2,fill = line_colour)

# lift
lift_plot <- h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  ggplot(aes(cumulative_data_fraction,cumulative_lift)) +
  geom_line(colour = line_colour) +
  geom_hline(yintercept=1, linetype='dashed',colour="snow")

# ------------------------------------------------------
# UI
# ------------------------------------------------------

header <- dashboardHeader(title = "H2O Churn Model Explainer",titleWidth = "290px")

#####
sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Performance",icon = icon("dashboard"),
               menuSubItem("Curves",tabName = "curves")),
      menuItem("Local explanations", tabName = "local_expl", icon = icon("users"))

      )
)

#####
body <-  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
      tabItem(tabName = "curves",
              fluidRow(
                box(title="AUC",width=6,auc),
                box(title="AUCPR",width=6,aucpr)
                ),
              fluidRow(
                box(title="ROC",width=6,plotOutput("roc")),
                box(title="PRROC",width=6,plotOutput("prroc"))
                ),
              fluidRow(
                box(title="Cumulative Gains",width=6,plotOutput("gains")),
                box(title="Cumulative Lift",width=6,plotOutput("lift"))
              )),
      tabItem(tabName = "local_expl",
              fluidRow(
                box(title= "Individual explanations",width=8,
                    withSpinner(plotOutput("shap_plot"),color=spinner_colour)),
                box(title = "Options",width=4,
                    selectInput("shap_cust",label="CustomerID",
                                choices = customers,
                                selected = sample(customers,1),
                                selectize = T),
                    sliderInput("shap_n_feat",label="Number of features",
                                min = 1,
                                max = 15,
                                value = 5,
                                step = 1))
                ))
      )
)

ui <- dashboardPage(header,sidebar,body)

# ------------------------------------------------------
# SERVER
# ------------------------------------------------------

server <- function(input, output) { 
  
  observeEvent(input$tabs, { 
  
    # CURVES
    if (input$tabs=="curves") {
    
    output$roc   <- renderPlot({roc_plot})
    output$prroc <- renderPlot({prroc_plot})
    output$gains <- renderPlot({gains_plot})
    output$lift  <- renderPlot({lift_plot})
      }
    
    # LOCAL EXPLANATIONS
    if (input$tabs == "local_expl") {
      
      observeEvent(c(input$shap_cust,input$shap_n_feat),{
        
        shap_data <- h2o.shap_explain_row_plot(mod, test_hf, 
                                               row_index = as.numeric(input$shap_cust),
                                               top_n_features = input$shap_n_feat) %>%
          ggplot_build()

      output$shap_plot <- renderPlot({
        
        shap_data$data[[1]] %>%
        mutate(x1 = str_extract(text,"(?<=Feature: ).+(?= \n)"),
               x2 = str_extract(text,"(?<=Feature Value: ).+(?= \n)")) %>%
          mutate(x2= gsubfn("([0-9.]+)", ~format(round(as.numeric(x))), x2)) %>%
          mutate(x =paste(x1,"=",x2)) %>%
          mutate(y=ymin+ymax) %>%
          select(x,y) %>%
          arrange(desc(y)) %>%
          ggplot(aes(reorder(x,y),y,fill = y>=0.0)) +
          geom_col() +
          coord_flip() +
          scale_fill_manual(values= c("grey60","yellow4")) +
          theme_minimal(base_size=16) +
          theme(plot.background = element_rect(fill=back_colour,colour =back_colour),
                panel.background =  element_rect(fill=back_colour,colour =back_colour),
                panel.grid = element_line(size=.05,),
                axis.title = element_blank(),
                axis.text = element_text(colour="snow"),
                text = element_text(colour="snow"),
                strip.text = element_text(colour="snow",size=16),
                plot.title = element_text(colour="snow",size=18),
                plot.subtitle = element_text(colour="snow",size=16),
                legend.position = "none")
        })
      })
    }
  })
}

shinyApp(ui, server)
