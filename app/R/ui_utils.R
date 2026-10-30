
#### Main Page Function ====================================================================================
## CSS Main page Navigation Bar Function 
sidebar_nav <- function(id, pages) {
  # pages: character vector of page names, e.g. c("Overview", "Forecast")
  css <- "
    .sidebar-nav .radio, .sidebar-nav .form-check { padding-left: 0; margin: 0 0 2px 0; }
    .sidebar-nav input[type='radio'] { display: none; }
    .sidebar-nav label { display: block; width: 100%; margin: 0; cursor: pointer; }
    .sidebar-nav label span { display: block; padding: .55rem .8rem;
      border-radius: 8px; color: var(--bs-body-color); }
    .sidebar-nav label span:hover { background: rgba(var(--bs-primary-rgb), .08); }
    .sidebar-nav input:checked + span { background: rgba(var(--bs-primary-rgb), .14);
      color: var(--bs-primary); font-weight: 600; }
  "
  tagList(
    tags$style(HTML(css)),
    div(class = "sidebar-nav",
      radioButtons(id, label = NULL, choices = pages)
    )
  )
}

#### Overview Page ========================================================================================
#### Function Overview Page Container for Product Weekly sales ####
product_weekly_sales_ui <- function(
  header = "Product Weekly Sales", 
  height = 600,
  output_id = "ts_overview_product_weekly_sales"
){
  # Time series plot (One product Weekly Sales)
  card(
    card_header("Product Weekly Sales"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = "ts_overview_product_weekly_sales")  
  )
}
#### Function Overview Page Container for Product Weekly Price
product_weekly_price_ui <- function(
  header = "Product Weekly Price", 
  height = 600, 
  output_id = "ts_overview_product_weekly_price"
){
  ## Time Series Plot (Weekly Price)
  card(
    card_header(header),
    full_screen = TRUE,
    height      = height,
    plotOutput(outputId = output_id)  
  )
}
#### Functions Overview Page for Controller ####
product_overview_page_controller_ui <- function(output_id = "ts_overview_pick_product_ws_price"){
  card(
    card_header("Look up Product"),
    height = 600,
    pickerInput(
      inputId  = output_id,
      label    = "Product",
      choices  = c("A", "B", "C"),
      selected = "A",
      multiple = FALSE,
      options  =  pickerOptions(
        liveSearch = TRUE,
        size = 5
      )
    ) 
  )
}
#### Function Overview Page for Table plot ####
table_overview_page_ui <- function(
  header = "Table Overview", 
  output_id = "overview_table_plot", 
  height = 600 
){
  ## Table Overview Plot 
  card(
    card_header(header),
    full_screen = TRUE,
    height      = height,
    tableOutput(outputId = output_id)
  )
}

#### Function for Between Product Comparison Bar Chart ####
between_product_comparison_bar_chart_ui <- function(
  between_product_comparison_output_id = "between_product_comparison_output_id"
){
  ## Between Product Comparison Bar Chart
  card(
    card_header("Between Product Weekly Sales"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = between_product_comparison_output_id)
  )
}

#### Forecast Page ========================================================================================

#### Forecast Controller UI Container ####
forecast_options_controller_ui <- function(
  product_input_id  = "forecast_controller_product",
  forecast_model_input_id = "forecast_controller_model_type"
){
  card(
    height = 600, 
    card_header("Forecast Options"),
    pickerInput(
      label    = "Product",
      inputId  = product_input_id,
      choices  = c("Product", "Product_2.."),
      selected = "Product",
      multiple = FALSE,
      options = pickerOptions(liveSearch = TRUE, size = 5)
    ),
    pickerInput(
      label    = "Model",
      inputId  = forecast_model_input_id,
      choices  = c("Exponential Smoothing", "Hierarchical Bayesian..."),
      selected = "Hierarchical Bayesian...",
      multiple = FALSE,
      options  = pickerOptions(liveSearch = TRUE, size = 5) 
    )
  )
}

#### Forecast Plot Container ####
forecast_plot_container_ui <- function(forecast_plot_output_id = "forecast_plot"){
  card(
    card_header("Forecasting Product Weekly Sales"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = forecast_plot_output_id)  
  )
}

#### Forecast Table Horizon Output ####
forecast_table_horizon_ui <- function(forecast_table_output_id = "forecast_table_horizon"){
  card(
    card_header("Forecast Horizon Table"),
    full_screen = TRUE,
    height      = 400,
    tableOutput(outputId = forecast_table_output_id)
  )
} 

#### Simulation UI ==========================================================================================

#### Function for Simulation Forecast Controller 
simulation_forecast_controller_ui <- function(
  product_forecast_simulation_input_id  = "simulation_product_input_id",
  model_forecast_simulation_input_id    = "simulation_model_input_id",
  simulation_scenario_forecast_input_id = "simulation_scenario_input_id" 
){
  card(
    card_header("Simulation Forecast"),
    height = 600,
    # Product Picker
    pickerInput(
      label    = "Product", 
      inputId  = product_forecast_simulation_input_id,
      choices  = c("Product_1", "Product_2"),
      selected = "Product_1",
      multiple = FALSE,
      options  = pickerOptions(liveSearch = TRUE, size = 5)  
    ),
    # Model Picker
    pickerInput(
      label    = "Model",
      inputId  = model_forecast_simulation_input_id,
      choices  = c("Exponential Smoothing", "Bayesian Hierarchical"),
      selected = "Bayesian Hierarchical",
      multiple = FALSE,
      options  = pickerOptions(liveSearch = TRUE, size = 5)
    ),
    # Simulation Scenario Picker
    pickerInput(
      label   = "Simulation",
      inputId = simulation_scenario_forecast_input_id,
      choices = c("Price Hikes 10%", "Price Hiker Drop 10%"),
      selected = "Price Hikes 10%",
      multiple = FALSE,
      options  = pickerOptions(liveSearch = TRUE, size = 5)
    )
  )
}

#### Simulation Table Forecasts ###
simulation_table_forecast_ui <- function(
  simulation_forecast_table_output_id = "simulation_forecast_table_output_id"
){
  card(
    card_header("Simulation Forecast Table Overview"),
    full_screen = TRUE,
    height     = 400,
    tableOutput(outputId = simulation_forecast_table_output_id)  
  )
}

#### Simulation Forecast Time Series Plot ####
simulation_forecast_time_series_plot_ui <- function(
  simulated_forecast_ts_plot_output_id = "simulated_forecast_ts_plot_output_id"
){
  card(
    card_header("Simulated Forecast"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = simulated_forecast_ts_plot_output_id)
  )
}

#### Diagnostics Page ======================================================================================

## Function Forecast Diagnostics Controller 
diagnostics_controller_ui <- function(
  pick_model_diagnostics_input_id = "diagnostics_forecast_model_input_id",
  pick_loss_function_diagnostics  = "diagnostics_forecast_function_input_id",
  pick_product_diagnostics        = "diagnostics_forecast_product_input_id"
){
  card(
    card_header("Diagnostics Options"),
    height = 600,
    # Pick Models to compare
    prettyCheckboxGroup(
      label    = "Models", 
      inputId  = pick_model_diagnostics_input_id,
      choices  = c("Model_1", "Model_2", "Model_3"),
      selected = c("Model_1", "Model_2"),
      outline = TRUE,
      plain   = TRUE,
      icon = icon("check")  
    ),
    # Pick a metric to compare
    pickerInput(
      label   = "Loss Function", 
      inputId = pick_loss_function_diagnostics,
      choices = c("RMSE", "MAE", "SE"),
      selected = "RMSE",
      multiple = FALSE,
      options  = pickerOptions(size = 5)
    ),
    # Pick a Specific Product or Overall
    pickerInput(
      label    = "Product", 
      inputId  = pick_product_diagnostics,
      choices  = c("Overall", "Product_1", "Product_2.."),
      selected = "Overall",
      multiple = FALSE,
      options  = pickerOptions(liveSearch = TRUE, size = 5)
    )
  )
}

## Model Comparison Bar Chart Function 
model_comparison_bar_chart_ui <- function(model_bar_chart_comparison_output_id = "model_comparison_bar_chart_output_id"){
  card(
    card_header("Model Comparison Bar Chart"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = model_bar_chart_comparison_output_id) 
  )
}

## Model Comparison Loss Function Distribution 
model_comparison_loss_function_distribution_ui <- function(
  loss_function_distribution_output_id = "loss_function_distribution_output_id"
){
  ## Loss Function Distribution
  card(
    card_header("Loss Function Distribution"),
    full_screen = TRUE,
    height      = 600,
    plotOutput(outputId = loss_function_distribution_output_id)  
  )
}

## Model Comparison Diagnostics Table Overview
model_comparison_table_overview_ui <- function(
  loss_functions_table_overview_output_id = "loss_function_table_output_id"
){
  ## Table Overview Model Comparison by Chosen Metric
  card(
    card_header("Model Comparison Table Overview"),
    full_screen = TRUE,
    height = 600,
    tableOutput(outputId = loss_functions_table_overview_output_id)
  ) 
}
