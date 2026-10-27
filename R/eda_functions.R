#### Function to Filter and plot one product ####
plot_product_trend_seasonality <- function(
  data, 
  supplier_n    = NULL, 
  category_name = NULL, 
  color_name    = NULL, 
  product_n     = NULL,
  period        = "week"
){
  # Filter for specific product based on supplier and category Or the product number
  if(is.null(product_n)){
    one_product <- data %>% filter(supplier == supplier_n, category == category_name, color == color_name)
  }else{
    one_product <- data %>% filter(product == product_n)
  }

  # Plot the Specific Series 
  p <- autoplot(one_product) +
    theme_minimal() +
    labs(
      title = paste0("Category : ", one_product$category ," Supplier : ", one_product$supplier),
      x = "Weeks",
      y = "Weekly Sales"
    )+
    theme(
      title = element_text(size = 18, hjust =  0.5, face = "italic"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  
  ps <- gg_season(one_product, period = period) +
    theme_minimal() +
    labs(
      title = paste0("Category : ", one_product$category ," Supplier : ", one_product$supplier),
      x = "Weeks",
      y = "Weekly Sales"
    )+
    theme(
      title = element_text(size = 18, hjust =  0.5, face = "italic"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  
  return(list(trend = p, seasonality = ps))
}

#### Exploratory Model Function To Decompose The Series #### 
exploratory_model <- function(one, df = 5) {
  one <- one %>%
    arrange(week) %>%                       # lags assume rows in time order
    mutate(
      t       = row_number(),
      feat    = as.integer(featured),
      feat_l1 = lag(feat, 1, default = 0),
      feat_l2 = lag(feat, 2, default = 0),
      lp      = log(price),
      ref     = (lag(lp, 1) + lag(lp, 2) + lag(lp, 3) + lag(lp, 4)) / 4,
      disc    = lp - ref) %>%
    filter(!is.na(disc))

  # quasi-Poisson: Poisson mean structure, with overdispersion estimated
  fit <- glm(weekly_sales ~ splines::ns(t, df) + feat + feat_l1 + feat_l2 + lp + disc,family = quasipoisson, data = one)

  one %>% mutate(res  = residuals(fit, type = "pearson"),disp = summary(fit)$dispersion)
}
