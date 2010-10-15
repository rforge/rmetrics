\name{parameter-estimation}
\alias{fit.schwartz2f}
\title{Schwartz 1997 two factor parameter estimation}

% <---------------------------------------------------------------------->
\description{

  Fit the Schwartz 1997 two factor commodity model to futures data.

}

% <---------------------------------------------------------------------->
\usage{

fit.schwartz2f(data, ttm, deltat = 1 / 260,
               s0 = data[1,1], delta0 = 0,
               mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.3,
               rho = 0.7, lambda = 0,
               meas.sd = rep(0.1, ncol(data)),
               opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, sigmaS = TRUE,
                            kappa = TRUE, alpha = TRUE, sigmaE = TRUE,
                            rho = TRUE, lambda = FALSE),
               opt.meas.sd = c("scalar", "all", "none"),
               r = 0.03, silent = FALSE, \dots)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{data}{A matrix with futures prices. \code{NA}-values are allowed.}
  \item{ttm}{A matrix with the corresponding time to maturity (see
    \bold{Details}).}
  \item{deltat}{Time increment (see \bold{Details}).}
  \item{s0}{Initial value of the commodity spot price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{Initial value of the drift parameter of the commodity spot price.}
  \item{kappa}{Initial parameter of the speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Initial parameter of the mean-level of the convenience yield process.}
  \item{sigmaS}{Initial parameter of the diffusion parameter of the spot price process.}
  \item{sigmaE}{Initial parameter of the diffusion parameter of the convenience yield process.}
  \item{rho}{Initial parameter of the correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{meas.sd}{Initial parameter of the standard deviation of the measurement equation (see \bold{Details}).}
  \item{lambda}{Initial value of the market price of convenience yield risk.}
  \item{opt.pars}{A logical vector giving the parameters which
    shall be fitted. The order is as given in the function header,
    names are discarded.}
  \item{opt.meas.sd}{States how the standard deviation of the
    measurement equation should be treated (see \bold{Details}).}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{silent}{If \code{FALSE} the log-likelihood and parameters will
    be printed in each iteration.}
  \item{\dots}{Arguments passed to \code{\link{optim}}.}
}

% <---------------------------------------------------------------------->
\note{

  Parameter estimation is statistically fragile and computationally
  demanding. Multiple local maxima of the likelihood may exist which can
  result in absurd parameter estimates as, e.g., a yearly drift of 300\%
  and or a market price of convenience yield risk of -200\%. Therefore,
  a reasonable parameter estimation is most likely an iteration where
  several initial values are used and different combinations of
  parameters are held constant during estimation. Also, simulation
  studies showed that a fairly large sample is required to get adequate
  estimates (e.g. 20000 daily observations, depending on the number of
  parameters which shall be estimated). For this reason the default is
  to hold \code{s0}, \code{delta0}, and \code{lambda} constant.

  Several utility functions may help to investigate the fit (see
  e.g. \code{\link[=fitted.schwartz2f.fit]{fitted}},
  \code{\link[=resid.schwartz2f.fit]{resid}},
  \code{\link[=plot.schwartz2f.fit]{plot}},
  \code{\link[=coef.schwartz2f]{coef}}).

  The fitting procedure generally requires a large number of iterations
  to achieve a reasonable tolerance level. Each optimization iteration
  involves the filtering of the data set by the Kalman
  filter. Therefore, an efficient implementation of the Kalman filter is
  key. Hence, the \code{\link[FKF:fkf]{fkf}} function of the package
  \code{\link[FKF:fkf]{FKF}} can be considered as the backbone of the
  estimation procedure.


  
}
% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the futures price whose time
  to maturity was \code{ttm[i,j]} when it was observed (in units of
  \code{deltat}).

  The time increment between observation \code{data[i,j]} and
    \code{data[i + 1,j]} is denoted with \code{deltat}. Note that this
    specification requires a regularly spaced data series.

  \code{opt.meas.sd} specifies how measurement uncertainty is treated in
  the fit: According to the model there should be a one-to-one
  correspondence between the spot and the futures price. In reality, the
  term structure does not fully match for any set of parameters. This is
  reflected in the measurement uncertainty-vector \code{meas.sd}. All
  components of \code{meas.sd} can be fitted. However, it might be
  sufficient to fit only a scalar where the measurement uncertainty is
  parametrized by \code{scalar * meas.sd}. In this case define the
  vector \code{meas.sd} and set \code{opt.meas.sd} to
  \dQuote{scalar}. \code{meas.sd} can be set to a vector with each
  component set to, e.g., 2\%, giving each point in the term structure
  equal weight. Another reasonable specification takes open interest or
  volumes into account: The higher the volume, the higher the weight and
  therefore the lower the corresponding component of \code{meas.sd}.  If
  all components of \code{meas.sd} shall be fitted choose
  \dQuote{all}. If the measurement uncertainty is known set
  \code{meas.sd} to \dQuote{none}. Note that the measurement errors are
  assumed to be independent in this implementation (even though the
  model and the filter do not require independence).

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.
  
}

% <---------------------------------------------------------------------->
\value{

  An object of class \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}} class,
  \code{\link[=fitted.schwartz2f.fit]{fitted}},
  \code{\link[=resid.schwartz2f.fit]{resid}},
  \code{\link[=plot.schwartz2f.fit]{plot}},
  \code{\link[=coef.schwartz2f]{coef}}),
  \code{\link{pricefutures}},
  \code{\link{futures-data}}.

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\examples{

data(futures)

## Estimate parameters for wheat data.
## (little precision required with reltol = 1e-3)
fit.obj <- fit.schwartz2f(futures$wheat$price, futures$wheat$ttm / 260,
                          deltat = 1 / 260, control = list(reltol = 1e-3))

## See how parameter values evolved during the fit
plot(fit.obj, type = "trace.pars")

\dontrun{
## Do the same but with lower tolerance level:
high.precision.fit <- fit.schwartz2f(futures$wheat$price, futures$wheat$ttm / 260,
                                     control = list(maxit = 3000, reltol = 5e-8))

high.precision.fit

plot(high.precision.fit, type = "trace.pars") # ...concerning parameter evolution.

## Alpha becomes nonsensically high, kappa (speed of mean-reversion
## of the convenience yield) goes to zero. Solution: Choose different
## initial values and/or hold kappa constant at 1.

constrained.fit <- fit.schwartz2f(futures$wheat$price, futures$wheat$ttm / 260,
                                  opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, 
                                               sigmaS = TRUE, kappa = FALSE, alpha = TRUE, 
                                               sigmaE = TRUE, rho = TRUE, lambda = TRUE),
                                  alpha = 0, kappa = 1, 
                                  control = list(maxit = 3000, reltol = 5e-8))

constrained.fit

plot(constrained.fit, type = "trace.pars")

## The above parameters based on a fit - where kappa was held constant at 1 -
## look more reasonable.
}
## These residuals should be iid standard normal
model.resid <- resid(fit.obj, data = futures$wheat$price, ttm = futures$wheat$ttm / 260,
                     type = "filter.std")
acf(model.resid, na.action = na.pass)
par(mfrow = c(3, 2))
apply(model.resid, 2, function(x)plot(density(na.omit(x))))
## ...but are anything but iid standard normal.

## ...though the fitted values look fine:
fitted.futures <- fitted(fit.obj, futures$wheat$price, futures$wheat$ttm / 260)
par(mfrow = c(1, 3))
### Plot futures prices
plot(as.ts(futures$wheat$price), plot.type = "single", ylab = "Futures prices",
     col = gray(seq(0.1, 0.9, length = 4)))
## Plot fitted values
plot(as.ts(fitted.futures), plot.type = "single", ylab = "Fitted values",
     col = gray(seq(0.1, 0.9, length = 4)))
## Plot relative difference
plot(as.ts((fitted.futures - futures$wheat$price) / fitted.futures), plot.type = "single",
     ylab = "Relative difference", col = gray(seq(0.1, 0.9, length = 4)))


## Try with kappa = 1, alpha = 0, and flexible standard deviationss of
## the measurement errors. Stop at 200 iterations.
fit.obj.2 <- fit.schwartz2f(futures$wheat$price, futures$wheat$ttm / 260,
                            opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, 
                                         sigmaS = TRUE, kappa = FALSE, alpha = FALSE, 
                                         sigmaE = TRUE, rho = TRUE, lambda = TRUE),
                            alpha = 0, kappa = 1, opt.meas.sd = "all",
                            deltat = 1 / 260, control = list(maxit = 200))


model.resid.2 <- resid(fit.obj.2, data = futures$wheat$price, ttm = futures$wheat$ttm / 260,
                       type = "filter.std")
## Residuals seem to be better:
acf(model.resid.2, na.action = na.pass)
par(mfrow = c(3, 2))
apply(model.resid.2, 2, function(x)plot(density(na.omit(x))))


## The schwartz2f.fit-object 'fit.obj' can be used to do further calculations as
## pricing a put option on the wheat futures which matures in 1.1
## years. The option expires in 1 year. The strike price is the
## expected futures price in 1.1 years.
priceoption("put", time = 1, Time = 1.1, K = pricefutures(1.1, fit.obj),
            fit.obj)



## Parameter estimation for weekly soybean meal data:
## Get Wednesday observations:
futures.w <- rapply(futures, function(x)x[format(as.Date(rownames(x)), "\%w") == 3,],
                    classes = "matrix", how = "list")

## Estimate soybean meal parameters (stop after 500 iterations).
## ttm (time-to-maturity) is divided by 260 as it is in unit of days and
## deltat = 1/52 because weekly price observations are used.
## Estimate all measurement error standard deviations (opt.meas.sd == "all"),
## but hold kappa, alpha, and lambda constant.
soybean.meal.fit <- fit.schwartz2f(data = futures.w$soybean.meal$price,
                                   ttm = futures.w$soybean.meal$ttm / 260,
                                   opt.meas.sd = "all",
                                   mu = 0, kappa = 1, alpha = 0.03, r = 0.04,
                                   opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, 
                                     sigmaS = TRUE, kappa = FALSE, alpha = FALSE, 
                                     sigmaE = TRUE, rho = TRUE, lambda = FALSE),
                                   deltat = 1 / 52, control = list(maxit = 500))

soybean.meal.fit 

plot(soybean.meal.fit, type = "trace.pars") # plot the parameter evolution

## Plot real and predicted forward curves:
par(mfrow = c(1, 2))
futuresplot(futures.w$soybean.meal, type = "forward.curve")
plot(soybean.meal.fit, type = "forward.curve", data = futures.w$soybean.meal$price,
     ttm = futures.w$soybean.meal$ttm / 260)


}

% <---------------------------------------------------------------------->
\keyword{models}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
