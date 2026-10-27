
#### Function to Clean and process the Data for Modeling ####
process_retail_modeling <- function(data, forecast_window_weeks = 4){
  data_clean <- data %>%
    # Rename covariates 
    rename(
      product = sku,
      supplier = vendor,
      category = functionality,
      featured = feat_main_page
    ) %>%
    mutate(
      # Remove 0.number.name pattern to name of the category 
      category = str_remove(category, pattern = "^\\d+\\."),
      # Convert TRUE and FALSE to 0 and 1 
      featured = ifelse(featured == TRUE, 1, 0),
      # Recover Nas colors of the product by the product number
      color = case_when(product == 42 ~ "black", product == 9 ~ "black", product == 44 ~ "black", product == 43 ~ "gold", .default = color),
      # Lag 2 Price 
      lag2_price = lag(price, 2) 
    ) %>%
    # Transform into Time Series 
    tsibble::as_tsibble(index = week, key = c(product, supplier, category))
  
  # Split the data into Training and Testing with forecast window of ig 4 weeks
  data_split <- data_clean %>%
    group_by_key() %>%
    mutate(.row = row_number()) %>%
    mutate(.total = n()) %>%
    ungroup()
  
  train <- data_split %>%
    filter(.row <= .total - forecast_window_weeks) %>%
    select(-.row, -.total)

  test <- data_split %>%
    filter(.row > .total - forecast_window_weeks) %>%
    select(-.row, -.total)

  # Return a List with the Train and Test Sets 
  return(list(train_set = train, test_set = test))
}
