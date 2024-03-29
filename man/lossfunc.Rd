% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lossfunc.R
\name{lossfunc}
\alias{lossfunc}
\title{Loss Functions}
\usage{
lossfunc(obj = list(Loss = NULL, ES = NULL), beta = 1e-04)
}
\arguments{
\item{obj}{a list that contains the following elements:
\describe{
\item{\code{Loss}}{a numeric vector that contains the values of a loss series
ordered from past to present; is set to \code{NULL} by default}
\item{\code{ES}}{a numeric vector that contains the estimated values of the
ES for the same time points of the loss series \code{Loss}; is set to
\code{NULL} by default}
}
Please note that a list returned by the \code{varcast} function can be directly
passed to \code{lossfunc}.}

\item{beta}{a single numeric value; a measure for the opportunity cost of
capital; default is \code{1e-04}.}
}
\value{
an S3 class object, which is a list of
\describe{
\item{loss.func1}{Regulatory loss function.}
\item{loss.func2}{Firm's loss function following Sarma et al. (2003).}
\item{loss.func3}{Loss function following Abad et al. (2015).}
\item{loss.func4}{Feng's loss function. A compromise of regulatory and
firm's loss function.}
}
}
\description{
This functions allows for the calculation of loss functions for the
selection of models.
}
\details{
Given a negative return series \code{obj$Loss}, the corresponding Expected
Shortfall (ES) estimates \code{obj$ES} and a parameter \code{beta} that
defines the opportunity cost of capital, four different definitions of loss
functions are considered.

Let \eqn{K} be the number of observations and \eqn{r_t} the observed return series.
Following Sarma et al. (2003)

\deqn{l_{t,1} = \{\widehat{ES}_t (\alpha) + r_t \}^2,}

 if \eqn{-r_t > \widehat{ES}_t(\alpha)}

\deqn{l_{t,1} = \beta * \widehat{ES}_t (\alpha),}

otherwise,

is a suitable loss function (firm's loss function), where \eqn{\beta} is the
opportunity cost of capital. The regulatory loss function
is identical to the firm's loss function with the exception of
\eqn{l_{t,1} = 0} for \eqn{-r_t \leq \widehat{ES}_t (\alpha)}.

Abad et al. (2015) proposed another loss function

\deqn{l_{t,a} = \{\widehat{ES}_t(\alpha) + r_t\}^2,}
 if \eqn{-r_t > \widehat{ES}_t(\alpha)}

\deqn{l_{t,a} = \beta * (\widehat{ES}_t (\alpha) + r_t),}

otherwise,

that, however, also considers opportunity costs for \eqn{r_t > 0}. An adjustment has
been proposed by Feng. Following his idea,

\deqn{l_{t,2} = \{\widehat{ES}_t(\alpha) + r_t\}^2,}
if \eqn{-r_t > \widehat{ES}_t (\alpha)}

\deqn{l_{t,2} = \beta * \min\{\widehat{ES}_t(\alpha) + r_t, \widehat{ES}_t(\alpha)\},}
otherwise,

should be considered as a compromise of the regulatory and the firm's loss
functions. Note that instead of the ES, also a series of Value-at-Risk values
can be inserted for the argument \code{obj$ES}. However this is not possible if
a list returned by the \code{varcast} function is directly passed to
\code{lossfunc}.
}
\examples{

\donttest{
# Example for Walmart Inc. (WMT)
prices <- WMT$price.close
output <- varcast(prices)
Loss <- -output$ret.out
ES <- output$ES
loss.data <- list(Loss = Loss, ES = ES)
lossfunc(loss.data)

# directly passing an output object of 'varcast()' to 'lossfunc()'
x <- WMT$price.close
output <- varcast(prices)
lossfunc(output)
}
}
\references{
Abad, P., Muela, S. B., & Martín, C. L. (2015). The role of the loss function
in value-at-risk comparisons. The Journal of Risk Model Validation, 9(1), 1-19.

Sarma, M., Thomas, S., & Shah, A. (2003). Selection of Value-at-Risk models.
Journal of Forecasting, 22(4), 337-358.
}
\author{
\itemize{
\item Sebastian Letmathe (Scientific Employee) (Department of Economics,
Paderborn University) \cr
\item Dominik Schulz (Scientific Employee) (Department of Economics,
Paderborn University), \cr
}
}
