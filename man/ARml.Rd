\name{ARml}
\alias{ARml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Autoregressive forecasting using various Machine Learning models.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ARml(x, horizons = c(1, 6, 12), lookback = c(1:5), frequency, 
ctrl = caret::trainControl(method = "cv",number = 3,p = 0.75,
                                            search = "grid",verboseIter = T,
                                            trim = FALSE,allowParallel = TRUE),
cb_grid = NULL,  
caret_method = "ranger", dynamic_features = c("month", "year", "week", "day", "quarter"), set_seed = 42)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A univariate time series object
}
  \item{horizons}{
Forecasting horizon: A numeric vector of one or more forecast horizons, h, measured in dataset rows. If dates are given, a horizon of 1, for example, would equal 1 * frequency in calendar time.
}
  \item{lookback}{
A numeric vector giving the lags–in dataset rows–for creating the lagged features. All non-grouping, non-static, and non-dynamic features in the input dataset, data, are lagged by the same values. The outcome is also lagged by default. Either lookback or lookback_control need to be specified–but not both.
}
  \item{frequency}{
Date/time frequency. Required if dates are given. A string taking the same input as base::seq.Date(..., by = "frequency") or base::seq.POSIXt(..., by = "frequency") e.g., '1 hour', '1 month', '7 days', '10 years' etc. The highest frequency supported at present is '1 sec'.
}
  \item{ctrl}{ See \code{\link[caret]{trainControl}}
%%     ~~Describe \code{ctrl} here~~
}
  \item{cb_grid}{
See \code{\link[caret]{modelLookup}} to create a data frame for appropreate parameter for choosen model.
}
  \item{caret_method}{
See \code{\link[caret]{modelLookup}} to select a regression model.
}
  \item{dynamic_features}{
A character vector to include date features in modeling e.g., weekday or year.
}
  \item{set_seed}{
set random seeds for reproducing results
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Nickalus Redell (2020). forecastML: Time Series Forecasting with Machine Learning Methods. R package version 0.9.0. \url{https://CRAN.R-project.org/package=forecastML}.

Max Kuhn (2020). caret: Classification and Regression Training. R package version 6.0-86.
  \url{https://CRAN.R-project.org/package=caret}
  
}
\author{
Res Akay
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

fit <- ARml(AirPassengers, horizons = c(1, 6, 12), lookback = c(1:12), 
frequency = "1 month", dynamic_features = c("month", "year", "quarter"), 
caret_method = "cubist")


forecast::autoplot(fit)

get_var_imp(fit, horizone = 1, plot = T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
