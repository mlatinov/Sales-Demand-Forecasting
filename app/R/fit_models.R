
#### Function to fit M01 Initial Model with Laplace ####
fit_mo1 <- function(data, product_id){

  ## Prepare the data for M01
  train_data <- data$train_set %>% filter(product == product_id)
  test_data  <- data$test_set  %>% filter(product == product_id) 

  ## Sample the model 
  laplace_object <- cmdlaplacer::laplace_model(
    path    = "Laplace/M01_explore.laplace",
    out_dir = "Laplace/Gen_Stan/",
    split_functions = TRUE
  )

  # Run laplace model 
  laplace_samples <- laplace_object$sample(
    data = list(
      T = nrow(train_data),                              
      K = 4,                                      # Weeks in the reference price 
      H = 4,                                      # Forecast horizon
      weekly_sales = train_data$weekly_sales,     # Outcome
      weekly_price = train_data$weekly_price,
      F  = train_data$featured,                    # Featured product indicator
      BF = train_data$BF,                          # Black Friday indicator
      
      # Planned covariates for the forecast weeks or test covarites provided with the test data 
      weekly_price_new = test_data$weekly_price,
      BF_new           = test_data$BF, 
      F_new            = test_data$featured,
      
      # Test The model internally
      test_switch = 1, # ON
      y_test      = test_data$weekly_sales
    ),
    seed       = 123,
    output_dir = "Laplace/Stan_csv_results/",
    chains     = 4,
    iter_sampling = 3000   
  )
}