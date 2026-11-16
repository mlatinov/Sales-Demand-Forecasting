
#### Function to fit M01 Initial Model with Laplace ####
fit_models <- function(
  data, 
  path = "Laplace/M01_local_level_test.laplace", 
  forecast_horizon = 4,
  seed        = 123,
  test_switch = 1,
  chains      = 4,
  iter_sampling = 3000,
  adapt_delta   = 0.99
){

  ## Sample the model 
  laplace_object <- cmdlaplacer::laplace_model(
    path    = path,
    out_dir = "Laplace/Gen_Stan/",
    split_functions = TRUE
  )

  # Run laplace model 
  laplace_samples <- laplace_object$sample(
    data = list(
      T = nrow(data$train),                              
      K = 4,                                      # Weeks in the reference price 
      H = forecast_horizon,                       # Forecast horizon
      weekly_sales = data$train$weekly_sales,     # Outcome
      weekly_price = data$train$weekly_price,
      F  = data$train$featured,                    # Featured product indicator
      BF = data$train$BF,                          # Black Friday indicator
      CH = data$train$CH,                          # Christmas Indicator
      YN = data$train$YN,                          # New Year Week Indicator 
      
      # Planned covariates for the forecast weeks or test covariates provided with the test data 
      weekly_price_new = data$test$weekly_price,
      BF_new           = data$test$BF, 
      F_new            = data$test$featured,
      CH_new           = data$test$CH,                          
      YN_new           = data$test$YN,                          
      
      # Test The model internally
      test_switch = test_switch, # ON
      y_test      = data$test$weekly_sales
    ),
    seed       = seed,
    output_dir = tempdir(),
    chains     = chains,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta
  )
}

#### Function to score fitted models ####
score_forecast <- function(fit, y_test) {
  eta <- fit$draws("eta_f", format = "draws_matrix")      # draws × H
  yf  <- fit$draws("y_f",   format = "draws_matrix")
  phi <- as.vector(fit$draws("phi", format = "draws_matrix"))
  H   <- length(y_test)
  log_mean_exp <- \(x) max(x) + log(mean(exp(x - max(x))))

  ## Return a Tibble with scores 
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

#### Custom Train Test Split Function ####
make_split <- function(data, product_id, origin, H = 4) {
  one <- data %>% tibble::as_tibble() %>% filter(product == product_id) %>% arrange(weekly)
  list(
    train = slice(one, 1:origin),
    test  = slice(one, (origin + 1):(origin + H)))
}

#### Function to fit and score model candidates 
fit_and_score <- function(data, model_id, path, product_id, origin, horizon = 4, seed = 42){
  ## Split into train and test 
  split <- make_split(data, product_id = product_id, origin = origin, H = horizon)
  ## Fit model 
  model <- fit_models(data = split, path = path, forecast_horizon = horizon, seed = seed)
  ## Get the diagnostics from the model 
  diag <- model$diagnostic_summary()

  ## Score the model and return 
  score_forecast(fit = model, y_test = split$test$weekly_sales) %>%
    mutate(
      model       = model_id,
      product     = product_id,
      origin      = origin,
      n_div       = sum(diag$num_divergent, na.rm = TRUE),
      n_treedepth = sum(diag$num_max_treedepth, na.rm = TRUE)
    )
}

#### Function to Summarize the Scores of the models 
summarise_cv <- function(scores) {
  scores %>%
    group_by(model) %>%
    summarise(
      elpd = sum(elpd, na.rm = TRUE), 
      crps = mean(crps, na.rm = TRUE), 
      mae = mean(abs_err, na.rm = TRUE),
      cover_80 = mean(in_80,na.rm = TRUE), 
      divergences = sum(n_div, na.rm = TRUE), 
    .groups = "drop"
  )
}