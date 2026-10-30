
#### Shiny app Libraries ####
library(shiny)
library(bslib)
library(shinyWidgets)

data <- readRDS("app_data/data.rds")$train_set

## User Interface ================================================================================
ui <- page_sidebar(
  # Global Variables Title and Theme
  title   = "Forecast Future Sales",
  theme   = bslib::bs_theme(
    bootswatch = "minty",
    base_font = font_google("Inter")
  ),

  # Side Bar Navigation 
  sidebar = sidebar(
    title = "Explore",
    sidebar_nav(
      id    = "sidebar_pages",
      pages = c(
        Overview    = "overview_page",
        Forecast    = "forecast_page",
        Simulations = "simulation_page",
        Diagnostics = "diagnostics_page",
        Details     = "details_page"
      ) 
    ) 
  ),
  ## Specify Navigation Pages 
  navset_hidden(
    id       = "selected_page",
    selected = "overview_page",

    ## Overview Page -----------------------------------------------------------------------------
    nav_panel_hidden("overview_page",

      ## Containers for TS Series Plot + Tables
      layout_columns(
        col_widths = c(5, 5, 2),
        fillable   = FALSE,
        gap        = "14px",

        # Time series plot (Weekly Sales)
        product_weekly_sales_ui(output_id = "ts_overview_product_weekly_sales"),

        ## Time Series Plot (Weekly Price)
        product_weekly_price_ui(output_id = "ts_overview_product_weekly_price"),
        
        ## Controller for Weekly Sales, Weekly Price Plots and Tables
        product_overview_page_controller_ui(
          data,
          product_pick_output_id  = "ts_overview_pick_product_ws_price",
          category_pick_output_id = "ts_overview_pick_product_category_ws_price",
          supplier_pick_output_id = "ts_overview_pick_product_supplier_ws_price",
          featured_pick_output_id = "ts_overview_pick_product_featured_ws_price"
        )
      ),

      ### Second Layout Columns Overview Page 
      layout_columns(
        col_widths = c(6, 6),
        fillable   = FALSE,

        ## Table Overview Plot 
        table_overview_page_ui(output_id = "overview_table_plot"),

        ## Between Product Comparison Bar chart 
        between_product_comparison_bar_chart_ui(
          between_product_comparison_output_id = "between_product_comparison_output_id"
        )
      ),
    ),
    ## Forecast Page ----------------------------------------------------------------------------
    nav_panel_hidden("forecast_page",
      ## Layout for the Controller and Forecast Plot 
      layout_columns(
        col_widths = c(3, 9),
        fillable   = FALSE,
        gap        = "14px",

        ## Forecast Controller
        forecast_options_controller_ui(
          product_input_id        = "forecast_controller_product",
          forecast_model_input_id = "forecast_controller_model_type"
        ),

        # Forecast Plot 
        forecast_plot_container_ui(forecast_plot_output_id = "forecast_plot")
      ),

      ## Layout for Table View on the Forecast Horizon 
      layout_columns(
        col_widths = c(12),
        fillable   = FALSE,

        ## Forecast Horizon Table 
        forecast_table_horizon_ui(forecast_table_output_id = "forecast_table_horizon")
      ) 
    ),

    ## Simulation Forecast Page -----------------------------------------------------------------
    nav_panel_hidden("simulation_page",
      ## Layout for Simulation Forecast Controller and Simulation Forecast Plot  
      layout_columns(
        col_widths = c(3, 9),
        fillable   = FALSE,
        gap        = "14px",
        
        ## Simulation Forecast Controller
        simulation_forecast_controller_ui(
          product_forecast_simulation_input_id  = "simulation_product_input_id",
          model_forecast_simulation_input_id    = "simulation_model_input_id",
          simulation_scenario_forecast_input_id = "simulation_scenario_input_id" 
        ),

        ## Forecast Simulation Plot 
        simulation_forecast_time_series_plot_ui(
          simulated_forecast_ts_plot_output_id = "simulated_forecast_ts_plot_output_id" 
        )
      ),

      ## Simulation Forecast Table Layout 
      layout_columns(
        col_widths = c(12),
        fillable   = FALSE,

        ## Simulation Table Output 
        simulation_table_forecast_ui(simulation_forecast_table_output_id = "simulation_forecast_table_output_id")
      )
    ),

    ## Diagnostics Page ---------------------------------------------------------------------------
    nav_panel_hidden("diagnostics_page",
      
      ## Layout for Diagnostics Controller and Model RMSE Bar Chart Comparison
      layout_columns(
        col_widths = c(3, 9),
        fillable   = FALSE,
        gap        = "14px",

        ## Diagnostics Controller 
        diagnostics_controller_ui(
          pick_model_diagnostics_input_id = "diagnostics_forecast_model_input_id",
          pick_loss_function_diagnostics  = "diagnostics_forecast_function_input_id",
          pick_product_diagnostics        = "diagnostics_forecast_product_input_id"
        ),

        ## Model Comparison Bar Chart 
        model_comparison_bar_chart_ui(
          model_bar_chart_comparison_output_id = "model_comparison_bar_chart_output_id"
        )
      ),

      ## Layout for the Distribution of the Loss function and Table Overview
      layout_columns(
        col_widths = c(8, 4),
        fillable   = FALSE,
        gap        = "14px",
        
        ## Loss Function Distribution
        model_comparison_loss_function_distribution_ui(
          loss_function_distribution_output_id = "loss_function_distribution_output_id"
        ),

        ## Table Overview Model Comparison by Chosen Metric
        model_comparison_table_overview_ui(
          loss_functions_table_overview_output_id = "loss_function_table_output_id"
        )
      )
    )
  )
)

## Server ========================================================================================
server <- function(input, output, session){
  ## Switch pages in the sidebar
  switch_sidebar_pages(input)

  ## Overview Page -------------------------------------------------------------------------------

  ## # Time series plot (Weekly Sales)
  output$ts_overview_product_weekly_sales <- plotly::renderPlotly(
    plot_time_series_weekly_sales(
      data           = data,
      product_input  = input$ts_overview_pick_product_ws_price,
      category_input = input$ts_overview_pick_product_category_ws_price,
      supplier_input = input$ts_overview_pick_product_supplier_ws_price,
      featured_input = input$ts_overview_pick_product_featured_ws_price
    )
  )
}

shinyApp(ui, server)
