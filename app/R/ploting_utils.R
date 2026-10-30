
#### Overview Page ===============================================================================

# Overview Page Selection function 
overview_page_select <- function(product_input, category_input, supplier_input, featured_input){
  # Selected var
  selected <- case_when(
    product_input  != "Multiple"  ~ "product",
    category_input != "Multiple"  ~ "category",
    supplier_input != "Multiple"  ~ "supplier",
    featured_input != "Multiple"  ~ "featured",
    TRUE ~ NA_character_
  )
  # Values 
  value <- case_when(
    product_input  != "Multiple" ~ as.character(product_input),
    category_input != "Multiple" ~ as.character(category_input),
    supplier_input != "Multiple" ~ as.character(supplier_input),
    featured_input != "Multiple" ~ as.character(featured_input),
    TRUE ~ NA_character_
  )
  return(list(selected = selected, value = value))
}

# Time series plot (One product Weekly Sales)
plot_time_series_weekly_sales <- function(data, product_input, category_input, supplier_input, featured_input){

  # Get the Input 
  input <- overview_page_select(product_input, category_input, supplier_input, featured_input)

  # Check if product ID is specified 
  p <- data %>%
    filter(as.character(.data[[unique(input$selected)]]) %in% input$value) %>%
    plot_time_series(
      .date_var  = week,
      .value     = weekly_sales,
      .color_var = product,
      .y_lab     = "Weekly Sales",
      .title     = " "
    )
  
  ## Return the Plot 
  return(p)
}

