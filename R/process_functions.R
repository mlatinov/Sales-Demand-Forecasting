#### Function to get BF days ####
in_week <- function(week, dates) {
  as.integer(sapply(week, \(w) any(dates >= w & dates < w + 7)))
}

black_friday_dates <- function(years) {
  nov1 <- as.Date(paste0(years, "-11-01"))
  nov1 + (4 - as.POSIXlt(nov1)$wday) %% 7 + 22          # Thanksgiving + 1
}

#### Function to get the CH days ####
christmas_flag <- function(week, years = 2016:2018) {
  christmas <- as.Date(paste0(years, "-12-25"))
  in_week <- function(dates) {
    as.integer(sapply(week, function(w) {
      any(dates >= w & dates < w + 7)
    }))
  }
  in_week(c(christmas - 7, christmas))
}

#### Function to get the New Years Days
new_year_flag <- function(week, years = 2016:2018) {
  new_year <- as.Date(paste0(years + 1, "-01-01"))
  in_week <- function(dates) {
    as.integer(sapply(week, function(w) {
      any(dates >= w & dates < w + 7)
    }))
  }
  in_week(new_year)
}

#### Function to Clean and process the Data for Modeling ####
process_retail_modeling <- function(data, forecast_window_weeks = 4, initial_training_window = 60){
  years <- 2016:2018
  xmas  <- as.Date(paste0(years, "-12-25"))

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
      # BF, New Year and Christmas day Indicator 
      BF = in_week(weekly, black_friday_dates(years)),
      CH = in_week(weekly, c(xmas - 7, xmas)),
      YN = in_week(weekly, as.Date(paste0(years + 1, "-01-01")))
    ) %>%
    # Transform into Time Series 
    tsibble::as_tsibble(index = weekly, key = c(product, supplier, category))
  
  # Split the data into Training and Testing with forecast window of ig 4 weeks
  return(data_clean)
}

#### Functions to Export Objects to the app ####
export_object <- function(x, out_file) {
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(x, out_file)
  out_file
}

#### Function to export fitted model draws ####
export_draws <- function(fit_file, out_file, variables = NULL) {
  fit   <- readRDS(fit_file)
  draws <- posterior::as_draws_df(fit$draws(variables = variables))
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(draws, out_file)
  out_file
}
