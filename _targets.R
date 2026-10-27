#### TARGETS WORKFLOW PIPELINE ===================================================================================
library(targets)
library(tidyverse)
library(tsibble)
library(feasts)

## Source Functions ##
tar_source("R/process_functions.R")

## Main Pipeline =================================================================================================
list(
  ## Load the data from a csv file 
  tar_target(
    name = retail_data_raw,
    command = read_csv("data/retail.csv")
  ),
  ## Process the data for modeling 
  tar_target(
    name = retail_clean_split,
    command = process_retail_modeling(retail_data_raw, forecast_window_weeks = 4)
  )
)