# MLforecast Package: Installation and basic usage

### Required Packages For Installation of MLforecast

```r
if(!require(devtools)){
install.packages("devtools")
}

to_install <- c("forecast", "forecastML", "caret")



for (i in to_install) {
  message(paste("searching for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}


```
### Installation


```r

devtools::install_github("Akai01/MLforecast")

```


## Examples
#### Forecasting with crossectional cross-validation. 
```r
library(MLforecast)

cntrl <- caret::trainControl(method = "cv",
                             number = 3,
                             p = 0.75,
                             search = "grid",
                             verboseIter = T,
                             trim = FALSE,
                             allowParallel = TRUE)


fit <- ARml(AirPassengers, horizons = c(1, 6, 12), lookback = c(1:12), ctrl = cntrl,
            cb_grid = NULL,
            frequency = "1 month", dynamic_features = c("month", "year", "quarter"), 
            caret_method = "svmLinear2")

class(fit)

forecast::autoplot(fit)

get_var_imp(fit, horizone = 1, plot = T)


```

#### Forecasting with time series cross-validation. 
```r
cntrl2 <- caret::trainControl(method = "timeslice",
                                      initialWindow = 108,
                                      horizon = 12,
                                      fixedWindow = FALSE,
                                      verboseIter = T,
                                      allowParallel = TRUE)
 
 

fit2 <- ARml(AirPassengers, horizons = c(1, 6, 12), lookback = c(1:12), ctrl = cntrl2,
            frequency = "1 month", dynamic_features = c("month", "year", "quarter"), 
            caret_method = "svmLinear2")

class(fit2)

forecast::autoplot(fit2)

get_var_imp(fit2, horizone = 1, plot = T)

```

# To do: Adding external variables in model
