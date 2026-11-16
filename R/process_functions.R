#### Function to get BF days ####
black_friday_flag <- function(years) {
  nov1 <- as.Date(paste0(years, "-11-01"))
  wd   <- as.POSIXlt(nov1)$wday                 
  thanksgiving <- nov1 + (4 - wd) %% 7 + 21    
  thanksgiving + 1
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
process_retail_modeling <- function(data, forecast_window_weeks = 4, initial_training_window = 60, products = NULL){
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
      BF = as.integer(weekly %in% tsibble::yearweek(black_friday_flag(2016:2018))), 
      CH = as.integer(christmas_flag(weekly)),
      YN = as.integer(new_year_flag(weekly))
    ) %>%
    # Transform into Time Series 
    tsibble::as_tsibble(index = weekly, key = c(product, supplier, category))
  
  ## Temporary filter to handle to load of the fits before hierarchical models 
  if (!is.null(products)) {
    data_clean <- data_clean %>%
      filter(product %in% products)
  }
  
  # Split the data into Training and Testing with forecast window of ig 4 weeks
  cv_plan <- data_clean %>% 
    tibble::as_tibble() %>%
    arrange(weekly) %>%
    time_series_cv(
      date_var =  weekly,
      initial  = initial_training_window,
      assess   = forecast_window_weeks,
      skip     = 4,
      cumulative = TRUE,
      slice_limit = 10
    )
  return(cv_plan)
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

#### Functions to Score Model Candidates ####
score_forecast <- function(fit, y_test) {
  eta <- fit$draws("eta_f", format = "draws_matrix")      # draws × H
  yf  <- fit$draws("y_f",   format = "draws_matrix")
  phi <- as.vector(fit$draws("phi", format = "draws_matrix"))
  H   <- length(y_test)
  log_mean_exp <- \(x) max(x) + log(mean(exp(x - max(x))))

  tibble(
    h       = 1:H,
    y       = y_test,
    elpd    = sapply(1:H, \(k) log_mean_exp(dnbinom(y_test[k], size = phi, mu = exp(eta[, k]), log = TRUE))),
    crps    = sapply(1:H, \(k) mean(abs(yf[, k] - y_test[k])) - 0.5 * mean(abs(yf[, k] - sample(yf[, k])))),
    median  = apply(yf, 2, median),
    abs_err = abs(y_test - median),
    in_80   = y_test >= apply(yf, 2, quantile, 0.1) & y_test <= apply(yf, 2, quantile, 0.9)
  )
}
