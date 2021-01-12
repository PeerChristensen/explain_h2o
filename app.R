
# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(h2o)
library(lime)
library(yardstick)
library(data.tree)

h2o.init()
file <- list.files("model",pattern="GBM") 
mod <- h2o.loadModel(glue::glue("/Users/peerchristensen/Desktop/Projects/explain_h2o_models/model/{file}"))

train <- read_csv("train.csv") %>%
  mutate(churn = as.factor(churn)) %>%
  mutate_if(is.character,factor) 

train <- read_csv("train.csv")
train_hf <- h2o.uploadFile(path = "train.csv")

test_hf <- read_csv("test.csv") %>% as.h2o()

# ------------------------------------------------------
# PERFORMANCE
# ------------------------------------------------------

perf <- h2o.performance(mod,test_hf)

metrics_table <- perf@metrics$max_criteria_and_metric_scores %>%
  as_tibble() %>%
  slice(1:10) %>%
  select(-idx)

metrics <- as.data.frame(h2o.metric(perf)) %>%
  select(-tns,-fps,-tps,-fns) %>%
  gather(metric, value, f1:tpr) 

metric_names <- unique(metrics$metric)

# ------------------------------------------------------
# UI
# ------------------------------------------------------

header <- dashboardHeader(title = "H2O Churn Model Explainer",titleWidth = "270px")
#####
sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Performance",icon = icon("dashboard"),
               menuSubItem("Metrics",tabName = "metrics"), # anc aucpr curve
               menuSubItem("Confusion matrices",tabName = "cm"),
               menuSubItem("Gain and lift",tabName = "gl")),
      menuItem("Global explanations", icon = icon("globe"),
               menuSubItem("Feature Importance",tabName = "var_imp"),
               menuSubItem("Partial dependence",tabName = "pdp"),
               menuSubItem("Decision Tree",tabName = "tree")),
      menuItem("Local explanations", tabName = "local_expl", icon = icon("users")),
      menuItem("Predictions", tabName = "preds", icon = icon("address-book"),selected = TRUE),
      menuItem("Model details", tabName = "details", icon = icon("info-circle")))
      )
#####
body <-  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
      tabItem(tabName = "metrics",
              fluidRow(
                box(width=9, plotOutput("metrics_plot", height = 500)),
                box(width=3, title = "Metrics to plot",
                  pickerInput(
                    inputId  = "pick_metrics",
                    choices  = metric_names,
                    selected = metric_names,   
                    options  = list(`actions-box` = TRUE), 
                    multiple = T),
                  )
                ),
              fluidRow(
                box(width=9,"Key Metrics",tableOutput("metrics_table"))
                )
              ),
      tabItem(tabName = "cm",
              fluidRow(
                box(title="By Metric",width=7,tableOutput("cm1")),
                box(width=5, title = "Choose metric",
                    selectInput('cm_metric',label="", choices = metric_names,
                                selected = "f1", selectize=TRUE))
                ),
                fluidRow(
                  box(title="By Threshold",width=7,tableOutput("cm2")),
                  box(width=5, title = "Choose custom threshold",
                      sliderInput("cm_custom", label = "Custom threshold", 
                                  min = 0,
                                  max = 1,
                                  value = 0.5,
                                  step = 0.01
                      )
                      
                  )
              )),
      tabItem(tabName = "var_imp",
              fluidRow(
                box(title="Feature Importance",width=7,
                    plotOutput("var_imp")),
                box(width=5, title = "Number of predictors",
                    sliderInput("n_preds", label = "", 
                                min = 1,
                                max = length(mod@parameters$x),
                                value = 10,
                                step = 1
                    )))
      
              )
)
)

ui <- dashboardPage(header,sidebar,body)

# ------------------------------------------------------
# SERVER
# ------------------------------------------------------

server <- function(input, output) { 
  
  observeEvent(input$tabs, { 
    
  # METRICS  
  if (input$tabs=="metrics") {
  
    # METRICS TABLE
    output$metrics_table <- renderTable({
      metrics_table},
      striped = F,
      hover = T,
      width="100%"
    )
  
    # METRICS PLOT
    output$metrics_plot <- renderPlot({
    
    metrics %>%
      filter(metric %in% input$pick_metrics) %>%
      ggplot(aes(x = threshold, y = value, group = metric)) +
      facet_wrap(~ metric, ncol = 4, scales = "free") +
      geom_line(colour = "#BBC605")} 
  )}
  
  # CONFUSION MATRIX
  if (input$tabs=="cm") {
    
    output$cm1 <- renderTable({
             h2o.confusionMatrix(mod,test_hf,metric=input$cm_metric) 
    })
    output$cm2 <- renderTable({
          h2o.confusionMatrix(mod,test_hf,threshold=input$cm_custom)
    })
  }
  
  # FEATURE IMPORTANCE
  if (input$tabs=="var_imp") {
    
    output$var_imp <- renderPlot({
      
      h2o.varimp(mod) %>%
        as_tibble() %>%
        top_n(input$n_preds) %>%
        dplyr::select(variable,scaled_importance) %>%
        ggplot(aes(reorder(variable,scaled_importance),scaled_importance)) +
        geom_col(fill="yellow4") +
        coord_flip() +
        theme_minimal()
      
    })
    }
})
  }

shinyApp(ui, server)


# trying to make metric selection responsive for conf matrix

#cm_metric_vals = reactiveValues(metric=FALSE,custom=FALSE)
#values <- reactiveValues(inDir = NULL)
# observeEvent(input$cm_metric,{
#   cm_metric_vals$metric=TRUE
#   cm_metric_vals$custom=FALSE
# })
# 
# observeEvent(input$cm_custom,{
#   cm_metric_vals$metric=FALSE
#   cm_metric_vals$custom=TRUE
# })
#observeEvent(input$cm_metric, {values$metric <- input$cm_metric})
#observeEvent(input$cm_custom, {values$custom <- input$cm_custom})

# eventReactive(c(
#   #cm_metric_vals$metric,
#   #cm_metric_vals$custom,
#   values$metric,
#   values$custom,
#   input$cm_metric,
#   input$cm_custom),{
#   if(cm_metric_vals$metric){
#     output$cm <- renderTable({
#       h2o.confusionMatrix(mod,test_hf,metric=input$cm_metric) %>% as_tibble()
#     })}
#   if(cm_metric_vals$custom){
#     output$cm <- renderTable({
#       h2o.confusionMatrix(mod,test_hf,threshold=input$cm_custom) %>% as_tibble()
#     })}
#     }
# 
# )
