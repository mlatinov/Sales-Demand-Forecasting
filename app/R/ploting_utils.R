#### General Helpers Functions ===================================================================
summarize_sales_price_months <- function(data){
  data %>%
    as_tibble(index = weekly) %>%
    mutate(
      monthly = yearmonth(weekly)
    ) %>%
    group_by(monthly, product, category, supplier, featured) %>%
    summarize(
      monthly_sales  = sum(weekly_sales),
      monthly_price  = mean(weekly_price),
      .groups = "drop"
    ) %>%
    as_tsibble(
      index = monthly,
      key   = c(product, category, supplier, featured)
    )
}
summarize_sales_price_quarters <- function(data){
  data %>%
    as_tibble(index = weekly) %>%
    mutate(
      quarterly = yearquarter(weekly)
    ) %>%
    group_by(quarterly, product, category, supplier, featured) %>%
    summarize(
      quarterly_sales  = sum(weekly_sales),
      quarterly_price  = mean(weekly_price),
      .groups = "drop"
    ) %>%
    as_tsibble(
      index = quarterly,
      key   = c(product, category, supplier, featured)
    )
}

### Theme Function for Interactive Plots ####
themed_plotly <- function(p) {
  p %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin        = list(l = 50, r = 10, t = 10, b = 30),
      xaxis         = list(domain = c(0, 1), automargin = TRUE),
      yaxis         = list(domain = c(0, 1), automargin = TRUE),
      legend        = list(orientation = "h", x = 0, y = 1.02,
                           yanchor = "bottom",
                           title = list(text = ""))
    ) %>%
    plotly::config(displaylogo = FALSE)
}
app_palette <- c("#0F766E", "#B45309", "#1F4E79", "#9D174D", "#4D7C0F", "#6B7280")

#### Overview Page ===============================================================================

### Change Time Index ###
summarize_time <- function(data, time_input){
  # The data is already weekly time stamped 
  if(time_input == "weekly"){
    return(data)

  # Summarize the weekly data column and impute it for month
  }else if (time_input == "monthly"){
    return(summarize_sales_price_months(data))
  
  }else if(time_input == "quarterly"){
    return(summarize_sales_price_quarters(data))
  }
  else{
    return(warning(paste0("Summarize Time Function Errored", time_input)))
  }
} 

# Overview Page Selection function 
overview_page_select <- function(product_input, category_input, supplier_input, featured_input) {
  
  # Check the Condition for Look up Products and Product descriptions
  if (!is.null(product_input) && length(product_input) > 0) {

    selected <- "product"
    value <- as.character(product_input)

  } else if (!is.null(category_input) && length(category_input) > 0) {

    selected <- "category"
    value <- as.character(category_input)

  } else if (!is.null(supplier_input) && length(supplier_input) > 0) {

    selected <- "supplier"
    value <- as.character(supplier_input)

  } else if (!is.null(featured_input) && length(featured_input) > 0) {

    selected <- "featured"
    value <- as.character(featured_input)
  
  } else {
    selected <- NULL
    value    <- NULL
  }

  return(list(selected = selected, value = value))
}

## Table Overview Filter Function ##
filter_time_series_table <- function(data, product_input, category_input, supplier_input, featured_input, time_input){

  # Get the Input 
  selection <- overview_page_select(product_input, category_input, supplier_input, featured_input)

  # Check whether a selection exists
  if (is.null(selection$selected)) {
    return(NULL)
  }

  # Check if product ID is specified and summarize the data
  sum_data <- data %>% summarize_time(time_input = time_input) %>%
    filter(as.character(.data[[unique(selection$selected)]]) %in% selection$value)
  
  # Return the Table
  return(sum_data)
}

## Time series plot (Sales and Price)
plot_time_series_sales_price <- function(
  data, 
  product_input, 
  category_input, 
  supplier_input, 
  featured_input, 
  var, 
  time_input,
  palette = app_palette,
  font = "Source Sans 3"
){

  # Get the filtered data
  filtered_data <- filter_time_series_table(data, product_input, category_input, supplier_input, featured_input, time_input) %>%
    mutate(product = factor(product))  

  # Get the correct var 
  time_var <- paste(time_input, var, sep = "_")
  y_lab    <- str_to_title(str_replace(time_var, pattern = "_", replacement = " "))
  n_series <- n_distinct(filtered_data$product)

  p <- filtered_data %>%
    plot_time_series(
      .date_var    = !!rlang::sym(time_input),
      .value       = !!rlang::sym(time_var),
      .color_var   = product,
      .smooth      = FALSE,
      .interactive = FALSE                            
    ) +
    scale_color_manual(values = grDevices::colorRampPalette(palette)(n_series)) +
    theme_minimal(base_family = font) +      
    theme(
      panel.grid.minor = element_blank()
    ) +
    labs(x = NULL, y = y_lab, color = NULL, title = NULL)

  plotly::ggplotly(p) %>% themed_plotly()
}

## Bar Char Comparison Plot
plot_products_comparison_bar_chart <- function(data, product_input, category_input, supplier_input, featured_input, var, time_input){
  
  # Get the Input 
  selection <- overview_page_select(product_input, category_input, supplier_input, featured_input)

  # Get the filtered data
  filtered_data <- filter_time_series_table(data, product_input, category_input, supplier_input, featured_input, time_input) %>%
    mutate(product = factor(product)) 

  # Get the correct var 
  time_var <- paste(time_input, var, sep = "_")

  # Plot the Bar chart 
  filtered_data %>%
    filter(.data[[time_input]] == max(.data[[time_input]], na.rm = TRUE)) %>%
    ggplot(aes(
      x = reorder(product, .data[[time_var]]),
      y = .data[[time_var]],
      fill = product
    )
  ) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "Product",
    y = str_to_title(str_replace(time_var, "_", " "))
  ) +
  theme_minimal()
}