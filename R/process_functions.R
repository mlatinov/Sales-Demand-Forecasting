#### Function to get BF days ####
black_friday <- function(years) {
  nov1 <- as.Date(paste0(years, "-11-01"))
  wd   <- as.POSIXlt(nov1)$wday                 
  thanksgiving <- nov1 + (4 - wd) %% 7 + 21    
  thanksgiving + 1
}

#### Function to Clean and process the Data for Modeling ####
process_retail_modeling <- function(data, forecast_window_weeks = 4){
  data_clean <- data %>%
    # Rename covariates 
    rename(
      weekly   = week,
      product  = sku,
      supplier = vendor,
      category = functionality,
      featured = feat_main_page,
      weekly_price = price
    ) %>%
    mutate(
      # Remove 0.number.name pattern to name of the category 
      category = str_remove(category, pattern = "^\\d+\\."),
      # Convert TRUE and FALSE to 0 and 1 
      featured = ifelse(featured == TRUE, 1, 0),
      # Recover Nas colors of the product by the product number
      color = case_when(product == 42 ~ "black", product == 9 ~ "black", product == 44 ~ "black", product == 43 ~ "gold", .default = color),
      # BF day Indicator 
      BF = as.integer(weekly %in% tsibble::yearweek(black_friday(2016:2018))) 
    ) %>%
    # Transform into Time Series 
    tsibble::as_tsibble(index = weekly, key = c(product, supplier, category))
  
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

#### Functions to Export Objects to the app ####
export_object <- function(x, out_file) {
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(x, out_file)
  out_file
}
