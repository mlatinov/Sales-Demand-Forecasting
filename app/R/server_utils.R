
#### Function to switch pages from the navigation bar ####
switch_sidebar_pages <- function(input, nav_id = "sidebar_pages", navset_id = "selected_page"){
    observeEvent(eventExpr = input[[nav_id]], {
      nav_select(id = navset_id, selected = input[[nav_id]])
  })
} 

#### Overview Plotting Functions ====================================================================================

## Render Time Series Plots 
render_overview_plot_sales_price <- function(
  input,
  output,
  output_id = "ts_overview_product_weekly_sales",
  data,
  var,
  product_input  = "ts_overview_pick_product_ws_price", 
  category_input = "ts_overview_pick_product_category_ws_price",
  supplier_input = "ts_overview_pick_product_supplier_ws_price",
  featured_input = "ts_overview_pick_product_featured_ws_price",
  time_input     = "ts_overview_pick_time_granularity"
){
  output[[output_id]] <- plotly::renderPlotly({
    plot_time_series_sales_price(
      data           = data,
      var            = var, 
      product_input  = input[[product_input]],
      category_input = input[[category_input]],
      supplier_input = input[[supplier_input]],
      featured_input = input[[featured_input]],
      time_input     = input[[time_input]] 
    )
  })
}

## Render Table Overview 
render_overview_table <- function(
  input, 
  output,
  output_id      = "overview_table_plot",
  product_input  = "ts_overview_pick_product_ws_price",
  category_input = "ts_overview_pick_product_category_ws_price",
  supplier_input = "ts_overview_pick_product_supplier_ws_price",
  featured_input = "ts_overview_pick_product_supplier_ws_price",
  time_input     = "ts_overview_pick_time_granularity"
){
  output[[output_id]] <- DT::renderDT({
    filter_time_series_table(
      data, 
      product_input  = input[[product_input]], 
      category_input = input[[category_input]],
      supplier_input = input[[supplier_input]],
      featured_input = input[[featured_input]],
      time_input     = input[[time_input]]
    ) %>%
    tibble::as_tibble() %>%
    mutate(across(where(~ inherits(.x, c("Date", "yearmonth", "yearquarter"))), ~ format(.x)))
  })
}


