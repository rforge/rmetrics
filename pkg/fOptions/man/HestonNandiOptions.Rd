\name{HestonNandiOptions}

\alias{HestonNandiOptions}

\alias{HNGOption}
\alias{HNGGreeks}
\alias{HNGCharacteristics}


\title{Option Price for the Heston-Nandi Garch Option Model}


\description{
  
    A collection and description of functions to valuate 
    Heston-Nandi options. Included are functions to compute 
    the option price and the delta and gamma sensitivities 
    for call and put options. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{HNGOption} \tab Heston-Nandi GARCH(1,1) option price, \cr
    \code{HNGGreeks} \tab Heston-Nandi GARCH(1,1) option sensitivities, \cr
    \code{HNGCharacteristics} \tab option prices and sensitivities. }
    
}


\usage{
HNGOption(TypeFlag, model, S, X, Time.inDays, r.daily)
HNGGreeks(Selection, TypeFlag, model, S, X, Time.inDays, r.daily)
HNGCharacteristics(TypeFlag, model, S, X, Time.inDays, r.daily)
}


\arguments{

    \item{model}{
        a list of model parameters with the following entries:
        \code{lambda}, \code{omega}, \code{alpha}, \code{beta}, 
        and \code{gamma}, numeric values.
        }
    \item{r.daily}{
        the daily rate of interest, a numeric value; 
        e.g. 0.25/252 means about 0.001\% per day.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{Selection}{
        sensitivity to be computed, one of \code{"delta"}, \code{"gamma"},
        \code{"vega"}, \code{"theta"}, \code{"rho"}, or \code{"CoC"}, 
        a string value.
        }
    \item{Time.inDays}{
        the time to maturity measured in days, a numerical 
        value; e.g. 5/252 means 1 business week.
        }
    \item{TypeFlag}{
        a character string either \code{"c"} for a call option or a 
        \code{"p"} for a put option.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
  
}


\value{

    \code{HNGOption}
    \cr
    returns a list object of class \code{"option"} with \code{$price}
    denoting the option price, a numeric value, and \code{$call} a 
    character string which matches the function call.
    \cr

    
    \code{HNGOGreeks} 
    \cr
    returns the option sensitivity for the selected Greek, either
    \code{"delta"} or \code{"gamma"}; a numeric value.
  
    
    \code{HNGCharacteristics} 
    \cr
    returns a list with the following entries:
    
    \item{premium}{
        the option price, a numeric value.}
    \item{delta}{
        the delta sensitivity, a numeric value.}
    \item{gamma}{
        the gamma sensitivity, a numeric value.}
    
}


\details{

    \bold{Option Values:}
    \cr\cr
    \code{HNGOption}calculates the option price, \code{HNGGreeks}
    allows to compute the option sensitivity Delta or Gamma, and 
    \code{HNGcharacterisitics} summarizes both in one function call.
    
}


\references{
Heston S.L., Nandi S. (1997);
    \emph{A Closed-Form GARCH Option Pricing Model},
    Federal Reserve Bank of Atlanta.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## model -
   # Define the Model Parameters for a Heston-Nandi Option:
   model = list(lambda = -0.5, omega = 2.3e-6, alpha = 2.9e-6, 
     beta = 0.85, gamma = 184.25) 
   S = X = 100
   Time.inDays = 252
   r.daily = 0.05/Time.inDays
   sigma.daily = sqrt((model$omega + model$alpha) /
     (1 - model$beta - model$alpha * model$gamma^2))
   data.frame(S, X, r.daily, sigma.daily)

## HNGOption -
   # Compute HNG Call-Put and compare with GBS Call-Put:
   HNG = GBS = Diff = NULL
   for (TypeFlag in c("c", "p")) {
     HNG = c(HNG, HNGOption(TypeFlag, model = model, S = S, X = X, 
       Time.inDays = Time.inDays, r.daily = r.daily)$price )
     GBS = c(GBS, GBSOption(TypeFlag, S = S, X = X, Time = Time.inDays, 
       r = r.daily, b = r.daily, sigma = sigma.daily)$price) }
   Options = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits=2))
   row.names(Options) <- c("Call", "Put")
   data.frame(Options)
     
## HNGGreeks -
   # Compute HNG Greeks and compare with GBS Greeks:
   Selection = c("Delta", "Gamma")
   HNG = GBS = NULL
   for (i in 1:2){
     HNG = c(HNG, HNGGreeks(Selection[i], TypeFlag = "c", model = model, 
       S = 100, X = 100, Time = Time.inDays, r = r.daily) ) 
     GBS = c(GBS, GBSGreeks(Selection[i], TypeFlag = "c", S = 100, X = 100, 
       Time = Time.inDays, r = r.daily, b = r.daily, sigma = sigma.daily) ) }
   Greeks = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits = 2))
   row.names(Greeks) <- Selection
   data.frame(Greeks)
}


\keyword{math}

