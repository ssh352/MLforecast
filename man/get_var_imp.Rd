\name{get_var_imp}
\alias{get_var_imp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get variable importance (If selected model has)
}
\description{
Get variable importance (If selected model has
}
\usage{
get_var_imp(object, horizone = 1, plot = T)
}

\arguments{
  \item{object}{
MLforecast object
}
  \item{horizone}{
Model from a horizon
}
  \item{plot}{
\code{TRUE} will show variable importance plot
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
%% ~put references to the literature/web site here ~
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
