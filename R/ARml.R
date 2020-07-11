ARml <- function(x, horizons = c(1, 6, 12),lookback = c(1:5), frequency, 
                 ctrl = caret::trainControl(method = "cv",number = 3,p = 0.75,
                                            search = "grid",verboseIter = T,
                                            trim = FALSE,allowParallel = TRUE), 
                 cb_grid = NULL, 
                 caret_method = "ranger", 
                 dynamic_features = c("month", "year", "week", "day", "quarter"),
                 set_seed = 42)
{
  
  set.seed(set_seed)
  # helper functions----------------------------
  fit_with_caret <- function(data, method = caret_method, my_ctrl = ctrl, my_cb_grid = cb_grid){
    
    form <- as.formula(y~.)
    
    fit <- caret::train(form = form,
                        data = data,
                        method = method, 
                        preProcess = NULL, 
                        weights = NULL, 
                        metric =  "RMSE",
                        trControl = my_ctrl, 
                        tuneGrid = my_cb_grid)
    
    return(fit)
  }
  
  prediction_function <- function(model, data_features) {
    
    data_pred <- data.frame("y_pred" = predict(model, data_features))  
    return(data_pred)
  }
  
  
  # AR----------------------------------
  
  stopifnot(dynamic_features %in% c("month", "year", "week", "day", "quarter"))
  
  
  
  freq_ts <- max(time(x))+1/frequency(x)
  freq <- frequency(x)
  y_return <- x
  
  
  data <- TSstudio::ts_to_prophet(x)
  
  ds <- data$ds
  
  data$ds <- NULL
  
  if("month" %in% dynamic_features){
    data$month <- lubridate::month(ds)
  }
  
  if("year" %in% dynamic_features){
    data$year <- (lubridate::year(ds))
  }
  
  if("week" %in% dynamic_features){
    data$week <- lubridate::week(ds)
  }
  
  if("day" %in% dynamic_features){
    data$day <- lubridate::mday(ds)
  }
  
  if("quarter" %in% dynamic_features){
    data$quarter <- lubridate::quarter(ds)
  }
  
  
  data_train <- forecastML::create_lagged_df(data, type = "train", 
                                             method = "direct",
                                             outcome_col = 1,
                                             horizons = horizons,
                                             lookback = lookback,
                                             dates = ds,
                                             frequency = frequency,
                                             dynamic_features = dynamic_features)
  
  windows <- create_windows(data_train, window_length = 0)
  
  model_results <- train_model(lagged_df = data_train,
                               windows = windows,
                               model_name = "caret",
                               model_function = fit_with_caret,
                               use_future = FALSE)
  
  
  fitted <- predict(model_results, prediction_function = list(prediction_function), data = data_train)
  
  
  data_forecast <- forecastML::create_lagged_df(data,
                                                type = "forecast",
                                                method = "direct",
                                                outcome_col = 1,
                                                horizons = horizons,
                                                lookback = lookback,
                                                dates = ds,
                                                frequency = frequency,
                                                dynamic_features = dynamic_features)
  
  
  for (i in seq_along(data_forecast)) {
    if("month" %in% dynamic_features){
      data_forecast[[i]]$month <- lubridate::month(data_forecast[[i]]$index)
    }
    if("year" %in% dynamic_features){
      data_forecast[[i]]$year <- lubridate::year(data_forecast[[i]]$index)
    }
    
    if("week" %in% dynamic_features){
      data_forecast[[i]]$week <- lubridate::week(data_forecast[[i]]$index)
    }
    if("day" %in% dynamic_features){
      data_forecast[[i]]$day <- lubridate::mday(data_forecast[[i]]$index)
    }
    
    if("quarter" %in% dynamic_features){
      data_forecast[[i]]$quarter <- lubridate::quarter(data_forecast[[i]]$index)
    }
  }
  
  forecast_all_horizones <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast)
  
  
  se <- function(x) sd(x)/sqrt(length(x))
  
  model_results <- list("model" = model_results, "fitted_velues" = fitted, 
                        "all_horizon" = forecast_all_horizones,
                        "data_train" = data_train, "data_forecast" = data_forecast)
  
  mean <- combine_forecasts(forecast_all_horizones, type = "horizon")$y_pred
  
  obj <- list(mean = ts(mean, frequency = freq, start = freq_ts),
              x = y_return,
              fitted = ts(fitted$y_pred, start = start(x), frequency = frequency(x)),
              upper = ts(data.frame(upper =mean + 1.96 * se(x)), frequency = freq, start = freq_ts),
              lower = ts(data.frame(lower =mean - 1.96 * se(x)), frequency = freq, start = freq_ts),
              method = paste("MLforecast: used method is", caret_method),
              level = 95,
              model = model_results,
              m = freq)
  
  
  class(obj) <- c("forecast", "MLforecast")
  
  return(obj)
  
  
  
}