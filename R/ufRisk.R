#'ufRisk: A package for user friendly and practical usage of various
#'backtesting methods.
#'
#'The goal of the \code{ufRisk} package (univariate financial risk) is to
#'enable the user to compute one-step ahead forecasts of Value at Risk (VaR)
#'and Expected Shortfall (ES) by means of various parametric and semiparametric
#'GARCH-type models. For the latter the estimation of the nonparametric scale
#'function is carried out by means of a data-driven smoothing approach.
#'Currently the GARCH, the exponential GARCH (EGARCH), the Log-GARCH, the
#'asymmetric power ARCH (APARCH), the FIGARCH and FI-Log-GARCH can be employed
#'within the scope of \code{ufRisk}. Model quality, in terms of forecasting VaR
#'and ES, can be assessed by means of various backtesting methods.
#'
#'
#'@section Functions:
#'\code{varcast} is a function to calculate rolling one-step ahead forecasts
#'of VaR and ES for a selection of parametric and semiparametric GARCH-type models (see also
#'\code{\link{varcast}}).
#'
#'
#'\code{trafftest} is a function for backtesting VaR and ES. ES is backtested
#'via a newly developed traffic light approach. (see also
#'\code{\link{trafftest}}).
#'
#'\code{covtest} is a function for conducting the conditional and the
#'unconditional coverage tests introduced by Kupiec (1995) and Christoffersen
#'(1998). (see also \code{\link{covtest}}).
#'
#'@references
#' Basel Committee on Banking Supervision (1996). Supervisory Framework For The
#' Use of Back-Testing in Conjunction With The Internal Models Approach to
#' Market Risk Capital Requirements.
#' Available online: \url{https://www.bis.org/publ/bcbs22.htm} (accessed on 23 June
#' 2020).
#'
#' Beran, J., and Feng, Y. (2002). Local polynomial fitting with long-memory,
#' short-memory and antipersistent errors. Annals of the Institute of
#' Statistical Mathematics, 54(2), pp. 291-311.
#'
#' Constanzino, N., and Curran, M. (2018). A Simple Traffic Light Approach to
#' Backtesting Expected Shortfall. In: Risks 6.1.2.
#'
#' Feng, Y. (2004). Simultaneously modeling conditional heteroskedasticity and
#' scale change. In: Econometric Theory, pp. 563-596.
#'
#' Feng, Y., Beran, J., Letmathe, S., & Ghosh, S. (2020). Fractionally
#' integrated Log-GARCH with application to value at risk and expected
#' shortfall (No. 137). Paderborn University, CIE Center for
#' International Economics.
#'
#' Letmathe, S., Feng, Y., & Uhde, A. (2021). Semiparametric GARCH models with
#' long memory applied to Value at Risk and Expected Shortfall (No. 141).
#' Paderborn University, CIE Center for International Economics.
#'
#' McNeil, A.J., Frey, R., and Embrechts, P. (2015). Quantitative risk
#' management: concepts, techniques and tools - revised edition. Princeton
#' University Press.
#'
#'
#'@author
#'\itemize{
#'\item Yuanhua Feng (Department of Economics, Paderborn University), \cr
#'Author of the Algorithms \cr
#'Website: \url{https://wiwi.uni-paderborn.de/en/dep4/feng/}
#'\item Xuehai Zhang (Former research associate at Paderborn University),\cr
#'Author \cr
#'\item Shujie Li (Scientific Employee) (Department of Economics,
#'Paderborn University), \cr
#'Author \cr
#'\item Christian Peitz (Department of Economics, Paderborn University), \cr
#'Author \cr
#'\item Dominik Schulz (Scientific Employee) (Department of Economics,
#'Paderborn University), \cr
#'Author \cr
#'\item Sebastian Letmathe (Scientific Employee) (Department of Economics,
#'Paderborn University), \cr
#'Package Creator and Maintainer
#'}
#'
#'
#'@docType package
#'@name ufRisk
#'@aliases ufRisk-package
NULL

