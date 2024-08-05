

library(rvest)
library(randomForest)
library(caret)
library(VGAM)
library(nnet)
library(cowplot)
library(patchwork)
library(class)
library(FNN)
library(dplyr)
library(tidyr)
library(janitor)
library(stringdist)
library(kableExtra)
library(fields)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(purrr)
library(fmsb)
library(ggradar)
library(rsconnect)
library(VIM)


app_directory <- "."  # Adjust this to your app's location

#Functions
player_comp <- readRDS(file.path(app_directory, "player_comp_function.rds"))
pos_percentiles <- readRDS(file.path(app_directory, "position_percentiles_function.rds"))
pos_probs <- readRDS(file.path(app_directory, "position_pred_function.rds"))
percentile_table <- readRDS(file.path(app_directory, "position_percentiles_table_function.rds"))

#Data
combine_data <- readRDS(file.path(app_directory, "combine_data.rds"))
scale_params <- readRDS(file.path(app_directory, "combine_scale_parameters.rds"))
train_pos_model <- readRDS(file.path(app_directory, "position_model_data.rds"))

#Models
position_model <- readRDS(file.path(app_directory, "position_pred_model.rds"))
ecdf_list <- readRDS(file.path(app_directory, "ecdf_list.rds"))







#User Interface
ui <- 
  
  
  fluidPage(theme = shinytheme("flatly"),
            tags$head(
              tags$style(HTML(
                "
      .input-container {
        margin-bottom: 8px;
      }
      .input-container label {
        font-size: 12px;
        display: block;
        margin-bottom: 5px;
      }
      .input-container .form-control {
        width: 100%;
      }
      .input-group {
        border: 1px solid #ddd;
        padding: 12px;
        margin-bottom: 17px;
        border-radius: 5px;
      }
      .input-group-label {
        font-size: 15px;
        font-weight: bold;
        margin-bottom: 12px;
        border-bottom: 2px solid #ddd;
        padding-bottom: 5px;
      }
    "
              ))
            ),
            
            
            
            
            
            # Add a div for the table with top margin
            
            
            
            ################
            
            
            
            
            navbarPage("NFL Combine Analysis",
                       #### Home Tab
                       tabPanel("Home",
                                mainPanel(
                                  h2("Welcome to the NFL Combine Comparison Tool"),
                                  h3("Feature Explanations"),
                                  h4("Custom Player Input:"),
                                  #Text explanation
                                  uiOutput("instructions"),
                                )
                       ),
                       
                       
                       
                       
                       #### Player Input Comparison/Position Prediction Tab
                       tabPanel("Custom Player Input",
                                sidebarLayout(
                                  sidebarPanel(
                                    h3("Comparison Metrics and Tools"),
                                    # checkboxGroupInput("selected_tools", "Tools:",
                                    #                    choices = list("Player Comparisons" = "comps",
                                    #                                   "Position Predictions" = "pos_pred"
                                    #                    ),
                                    #                    selected = NULL),
                                    checkboxGroupInput("selected_metrics", "Metrics:",
                                                       choices = list(
                                                         "Height(in.)" = "Height",
                                                         "Weight(lbs.)" = "Weight",
                                                         "40 Yard Dash" = "Forty",
                                                         "Vertical Jump" = "Vertical",
                                                         "Bench Press" = "Bench",
                                                         "Broad Jump" = "Broad.Jump",
                                                         "Three Cone Drill" = "Three.Cone",
                                                         "Shuttle Drill" = "Shuttle"),
                                                       selected = NULL),
                                    checkboxInput("check_all", "Select All Metrics", value = FALSE),
                                    width = 3
                                    #width = 2.5
                                  ),
                                  mainPanel(
                                    #Title
                                    h2("Custom Player Input"),
                                    
                                    
                                    textInput("player_name", label = "Player Name", value = "Player X"),
                                    
                                    
                                    #Inputs and filters
                                    uiOutput("comp_filters"),
                                    uiOutput("metricInputs"),
                                    
                                    #Submit button
                                    div(style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
                                        actionButton("input_submitbutton", "Submit", class = "btn-warning")
                                    ),
                                    
                                    #Outputs
                                    fluidRow(
                                      column(3, div(style = "margin-top: 30px;", uiOutput("pos_pred_table"))),
                                      column(4, div(style = "margin-top: 30px;", uiOutput("percentiles"))),
                                      column(5, div( plotOutput("spider_plot")))
                                      
                                    ),
                                    
                                    
                                    
                                    div(style = "margin-top: 30px;",
                                        uiOutput("comp_metrics_table")
                                    ),
                                    
                                    
                                    
                                    
                                    width = 9
                                  )
                                )
                       ),
                       
                       # Past Player Comparisons
                       tabPanel("Compare Past Combine Athletes",
                                # Content for this tab
                       )
            )
  )



server <- function(input, output, session) {
  

  
  stored_data <- reactiveVal(NULL)
  stored_filters <- reactiveVal(NULL)
  
  user_input_values <- reactiveVal(list())
  
  
  ### Output instructions
  
  output$instructions <- renderUI({
    # Render the RMarkdown file
    html_content <- rmarkdown::render("Combine App - Player Input Page.Rmd",
                                      output_format = "html_fragment",
                                      quiet = TRUE)
    
    # Read the rendered content
    instructions <- readLines(html_content)
    
    # Return the HTML content
    HTML(paste(instructions, collapse = "\n"))
  })
  
  
  
  
  
  ##################################################
  # Check all boxes for select all metrics #########
  observeEvent(input$check_all, {
    if (input$check_all) {
      updateCheckboxGroupInput(session, "selected_metrics",
                               selected = c("Height", "Weight", "Forty", "Vertical",
                                            "Broad.Jump", "Bench", "Three.Cone", "Shuttle"))
    } else {
      updateCheckboxGroupInput(session, "selected_metrics", selected = NULL)
    }
  }, ignoreInit = TRUE)
  
  # New observer to update "Select All" checkbox
  observe({
    all_metrics <- c("Height", "Weight", "Forty", "Vertical", "Broad.Jump", "Bench", "Three.Cone", "Shuttle")
    is_all_selected <- all(all_metrics %in% input$selected_metrics)
    updateCheckboxInput(session, "check_all", value = is_all_selected)
  })
  #####
  
  ##### Comp filters 
  output$comp_filters <- renderUI({
    
    tagList(
      div(class = "input-group",
          h3(class = "input-group-label", "Comparison Filters"),
          fluidRow(
            div(class = "col-md-2 input-container",
                numericInput("min_year", "Min. Year", value = 2000)
            ),
            div(class = "col-md-2 input-container",
                numericInput("max_year", "Max Year", value = 2024)
            ),
            div(class = "col-md-3 input-container",
                numericInput("n_comp", "# of Comparisons", value = 10)
            ),
            div(class = "col-md-3 input-container",
                selectInput("pos_input", label = "Position", 
                            choices = c("All","QB", "WR", "RB", "TE", "OL", "DB", "LB", "DE", "DT", "K"),
                            selected = "All",
                            multiple = FALSE))
          )
      )
    )
    
  })
  
  #### Metric inputs
  
  output$metricInputs <- renderUI({
    metric_inputs <- list(
      "Height" = list(id = "Height", label = "Height", value = 75),
      "Weight" = list(id = "Weight", label = "Weight", value = 220),
      "Forty" = list(id = "Forty", label = "40 Yard Dash", value = 4.34),
      "Vertical" = list(id = "Vertical", label = "Vertical Jump", value = 38.5),
      "Broad.Jump" = list(id = "Broad.Jump", label = "Broad Jump", value = 135),
      "Bench" = list(id = "Bench", label = "Bench Press", value = 17),
      "Three.Cone" = list(id = "Three.Cone", label = "Three Cone Drill", value = 6.66),
      "Shuttle" = list(id = "Shuttle", label = "Shuttle Drill", value = 4.25)
    )
    
    current_values <- user_input_values()
    
    if (length(input$selected_metrics) > 0) {
      tagList(
        div(class = "input-group",
            h3(class = "input-group-label", "Metrics"),
            fluidRow(
              lapply(input$selected_metrics, function(metric) {
                if (metric %in% names(metric_inputs)) {
                  div(class = "col-md-3 input-container",
                      numericInput(metric_inputs[[metric]]$id, 
                                   metric_inputs[[metric]]$label, 
                                   value = if (!is.null(current_values[[metric]])) current_values[[metric]] else metric_inputs[[metric]]$value)
                  )
                }
              })
            )
        )
      )
    }
  })
  
  
  observe({
    metric_names <- c("Height", "Weight", "Forty", "Vertical", "Broad.Jump", "Bench", "Three.Cone", "Shuttle")
    current_values <- user_input_values()
    
    for (metric in metric_names) {
      if (!is.null(input[[metric]])) {
        current_values[[metric]] <- input[[metric]]
      }
    }
    
    user_input_values(current_values)
  })
  
  
  
  
  current_metric_values <- reactive({
    stored_values <- user_input_values()
    metric_names <- input$selected_metrics  # Only consider selected metrics
    
    sapply(metric_names, function(metric) {
      if (!is.null(input[[metric]])) {
        input[[metric]]
      } else if (!is.null(stored_values[[metric]])) {
        stored_values[[metric]]
      } else {
        NA
      }
    })
  })
  
  
  ###################################################
  #Gather input data
  
  
  compInputData <- reactive({
    values <- current_metric_values()
    selected <- input$selected_metrics
    
    data <- data.frame(
      Player = input$player_name,
      Pos = input$pos_input,
      Height = NA_real_,
      Weight = NA_real_,
      Forty = NA_real_,
      Vertical = NA_real_,
      Broad.Jump = NA_real_,
      Three.Cone = NA_real_,
      Shuttle = NA_real_,
      Bench = NA_real_,
      stringsAsFactors = FALSE
    )
    
    for (metric in selected) {
      data[[metric]] <- as.numeric(values[metric])
    }
    
    return(data)
  })
  
  
  # comp_filter_values <- reactiveValues(
  #   
  #   min_year = 2000,
  #   max_year = 2024,
  #   n_comp = 10,
  #   
  #   
  # )
  
  
  
  
  ############## Submission and output
  
  
  
  observeEvent(input$input_submitbutton, {
    req(input$input_submitbutton)
    
    comp_filter_values$min_year <- input$min_year
    comp_filter_values$max_year <- input$max_year
    comp_filter_values$n_comp <- input$n_comp
    comp_filter_values$pos <- input$pos_input
    
    current_data <- compInputData()
    stored_data(current_data)
    
    #Comparison table
    output$comp_metrics_table <- renderUI({
      req(any(!is.na(current_data[, -c(1,2)])))  # Ensure there's at least one non-NA metric
      table <- player_comp(year_range = comp_filter_values$min_year:comp_filter_values$max_year,
                           n = comp_filter_values$n_comp, combine = combine_data, 
                           params = scale_params, player = current_data)
      HTML(table)
    }) 
    
    #Pos pred. table
    output$pos_pred_table <- renderUI({
      req(any(!is.na(current_data[, -c(1,2)])))  # Ensure there's at least one non-NA metric
      table <- pos_probs(model = position_model, impute_data = train_pos_model, player = current_data)
      HTML(table)
    })
    
    
    #Spider plot
    output$spider_plot <- renderPlot({
      req(any(!is.na(current_data[, -c(1,2)])))
      pos_spider_plot(player = current_data, position = comp_filter_values$pos, ecdf = ecdf_list)
      
    })
    
    output$percentiles <- renderUI({
      req(any(!is.na(current_data[, -c(1,2)])))  # Ensure there's at least one non-NA metric
      table <- percentile_table(player = current_data, position = comp_filter_values$pos, ecdf = ecdf_list)
      HTML(table)
    })
    
  })
  
  
  
  
  
  #####
  
  
  
  
}


shinyApp(ui = ui, server = server)
