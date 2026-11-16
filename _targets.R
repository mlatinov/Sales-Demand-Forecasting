#### TARGETS WORKFLOW PIPELINE ===================================================================================
library(targets)
library(tidyverse)
library(tsibble)
library(feasts)
library(timetk)

## Source Functions ##
tar_source("R/process_functions.R")
tar_source("R/fit_models.R")

## Main Pipeline =================================================================================================
list(
  ## Load the data from a csv file 
  tar_target(
    name = retail_data_raw,
    command = read_csv("data/retail.csv")
  ),
  ## Process the data for modeling 
  tar_target(
    name = retail_clean,
    command = process_retail_modeling(retail_data_raw, forecast_window_weeks = 4)
  ),
  ## Lock the models to be tested 
  tar_target(
    name = damped_trend_model_file,
    command = "Laplace/M01_damped_trend.laplace",
    format = "file"
  ),
  tar_target(
    name = local_level_model_file,
    command = "Laplace/M01_local_level_test.laplace",
    format = "file"
  ),
  ### Create a Grid with models and model ids
  tar_target(
    name = model_grid,
    command = tibble::tibble(
      model_id = c("Damped Trend", "Local Level"),
      path     = c(damped_trend_model_file, local_level_model_file)
    )
  ),
  ### Products for the models to be evaluated on 
  tar_target(
    name = cv_products,
    command = c(20, 35, 15)
  ),
  ## Create a CV Grid 
  tar_target(
    name = cv_grid,
    command = expand_grid(model_grid, product_id = cv_products, origin = seq(60, 96, by = 4))
  ),
  ## Fit and Score all models 
  tar_target(
    name = cv_scores,
    command = fit_and_score(
      data     = retail_clean,
      model_id = cv_grid$model_id,
      path     = cv_grid$path,
      product_id = cv_grid$product_id,
      origin     = cv_grid$origin
    ),
    pattern = map(cv_grid)  
  ),
  ## Summarize the Results
  tar_target(
    name = summary_scores,
    command = summarise_cv(cv_scores) 
  )
)

