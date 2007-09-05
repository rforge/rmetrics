\name{LookbackOptions}

\alias{LookbackOptions}

\alias{FloatingStrikeLookbackOption}
\alias{FixedStrikeLookbackOption}
\alias{PTFloatingStrikeLookbackOption}
\alias{PTFixedStrikeLookbackOption}
\alias{ExtremeSpreadOption}


\title{Valuation of Lookback Options}


\description{
  
    A collection and description of functions to valuate 
    lookback options. The payoff from a pathdependent 
    lookback call (put) depends on the exercise price 
    being set to the minimum (maximum) asset price achieved 
    during the life of the option. Thus, a lookback call 
    (put) allows the purchaser to buy (sell) the asset at 
    its minimum (maximum) price.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{FloatingStrikeLookbackOption} \tab Floating Strike Lookback Option, \cr
    \code{FixedStrikeLookbackOption} \tab Fixed Strike Lookback Option, \cr
    \code{PTFloatingStrikeLookbackOption} \tab PT Floating Strike Lookback Option, \cr
    \code{PTFixedStrikeLookbackOption} \tab  PT Fixed Strike Lookback Option, \cr
    \code{ExtremeSpreadOption} \tab Extreme Spread Options.}
    
}


\usage{
FloatingStrikeLookbackOption(TypeFlag, S, SMinOrMax, Time, r, 
    b, sigma, title = NULL, description = NULL)
FixedStrikeLookbackOption(TypeFlag, S, SMinOrMax, X, Time, r, 
    b, sigma, title = NULL, description = NULL)
PTFloatingStrikeLookbackOption(TypeFlag, S, SMinOrMax, time1, 
    Time2, r, b, sigma, lambda, title = NULL, description = NULL) 
PTFixedStrikeLookbackOption(TypeFlag, S, X, time1, Time2, r, b, 
    sigma, title = NULL, description = NULL)
ExtremeSpreadOption(TypeFlag, S, SMin, SMax, time1, Time2, r, b, 
    sigma, title = NULL, description = NULL)
}


\arguments{

    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{lambda}{
        The \code{lambda} factor enables the creation of so-called
        "fractional" lookback options where the strike is fixed at
        some percentage or below the extremum, i.e. \code{lambda} 
        is greater than 1 for calls, and between 0 and 1 for puts.
        }
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% pa.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{sigma}{
        the annualized volatility of the underlying security, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }
    \item{SMax, SMin}{
        [ExtremeSpread*] - \cr
        the maximum (minimum) value of the underlying asset. Note, the
        payoff at maturity of the extreme spread call (put) equals the 
        positive part of the difference between the maximum (minimum)
        value of the underlying asset, \code{SMax}, of the second
        (first) period and the maximum (minimum) of the underlying
        asset of the first (second) period. Likewise, reverse conditions
        are valid for the reverse extreme spread option.
        }
    \item{SMinOrMax}{
        the lowest price observed of the underlying in the case of
        the coll, or the highest price in the case of the put. A 
        numeric value.
        }
    \item{Time}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{time1, Time2}{
        [PTFloatingStrikeLookback*] - \cr
        the time to the end of the lookback period \code{time1}, and
        the time to expiry \code{Time2} where \code{time1<Time2}, \cr
        [PTFixedStrikeLookback*] - \cr
        the predetermined time \code{time1} where the lookback 
        period starts, and the time to expiry \code{Time2}, \cr
        [ExtremeSpread*] - \cr
        the two time periods, one starting today and ending at 
        \code{time1}, and the other starting at \code{time1} and
        ending at the maturity time \code{Time2} of the option.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        usually a character string either \code{"c"} for a call option 
        or a \code{"p"} for a put option, except for \cr
        [ExtremeSpread*] - \cr
        a character string either, \cr
        \code{"c"} for the extreme call, \cr
        \code{"p"} for the extreme put, \cr
        \code{"cr"} for the reverse extreme call, \cr
        \code{"pr"} for the revers extreme put.
        }
    \item{X}{
        the exercise price, a numeric value.
        }

}


\details{
  
    \bold{Floating Strike Lookback Options:}
    \cr\cr
    The lookback call (put) option gives the holder the right to buy (sell) 
    an asset at its lowest (highest) price observed during the life of the 
    option. This observed price is applied as the strike price. The payout 
    for a call option is essentially the asset price minus the minimum spot 
    price observed during the life of the option. The payout for a put option 
    is essentially the maximum spot price observed during the life of the 
    option minus the asset price. Therefore, a floating strike lookback 
    option is always in the money and should always be exercised. Floating 
    strike options can be priced analytically using a model introduced by 
    Goldman, Sosin, and Gatto (1979). Monte Carlo simulation is used for 
    the numerical calculation of a European style floating strike options.
    \cr
    [Haug's Book, Chapter 2.9.1]
    \cr
  
    
    \bold{Fixed Strike Lookback Options:}
    \cr\cr
    For a fixed strike lookback option, the strike price is known in advance. 
    The call option payoff is given by the difference between the maximum 
    observed price of the underlying asset during the life of the option and 
    the fixed strike price. The put option payoff is given by the difference 
    between the fixed strike price and the minimum observed price of the 
    underlying asset during the life of the option. A fixed strike lookback 
    call (put) option payoff is equal to that of a standard plain call (put) 
    option when the final asset price is the maximum (minimum) observed value 
    during the options life. Fixed strike lookback options can be priced 
    analytically using a model introduced by Conze and Viswanathan (1991).
    \cr
    [Haug's Book, Chapter 2.9.2]
    \cr
  
    
    \bold{Partial-Time Floating Strike Options:}
    \cr\cr
    For a partial-time floating strike lookback option, the lookback period 
    starts at time zero and ends at an arbitrary date before expiration. 
    Except for the partial lookback period, the option is similar to a 
    floating strike lookback option. The partial-time floating strike 
    lookback option is cheaper than a similar standard floating strike 
    lookback option. Partial-time floating strike lookback options can be 
    priced analytically using a model introduced by Heynen and Kat (1994).
    \cr
    [Haug's Book, Chapter 2.9.3]
    \cr
  
    
    \bold{Partial-Time Fixed Strike Options:}
    \cr\cr
    For a partial-time fixed strike lookback option, the lookback period 
    starts at a predetermined date after the initialization date of the 
    option. The partial-time fixed strike lookback call option payoff is 
    given by the difference between the maximum observed price of the 
    underlying asset during the lookback period and the fixed strike 
    price. The partial-time fixed strike lookback put option payoff is 
    given by the difference between the fixed strike price and the 
    minimum observed price of the underlying asset during the lookback 
    period. The partial-time fixed strike lookback option is cheaper than 
    a similar standard fixed strike lookback option. Partial-time fixed 
    strike lookback options can be priced analytically using a model 
    introduced by Heynen and Kat (1994).
    \cr
    [Haug's Book, Chapter 2.9.4]
    \cr
  
    
    \bold{Extreme Spread Options:}
    \cr\cr
    The time to maturity of an extreme spread option is divided into two 
    periods: one period starting at time zero and ending at some arbitrary 
    date, and another starting at that arbitrary date and ending at the 
    expiration date. A payoff at maturity of an extreme spread call (put) 
    option equals the positive part of the difference between the maximum 
    (minimum) value of the underlying asset of the second (first) period 
    and the maximum (minimum) value of the underlying asset of the first 
    (second) period.[1] The payoff at expiration of a reverse extreme 
    spread call (put) option equals the positive part of the difference 
    between the minimum (maximum) of the underlying asset of the second 
    (first) period and the minimum (maximum) value of the underlying asset 
    of the first (second) period. Extreme spread options can be priced 
    analytically using a model introduced by Bermin (1996).
    \cr
    [Haug's Book, Chapter 2.9.5]

}


\note{
  
    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 2.9 of Haug's Book (1997).

}



\value{

    The option price, a numeric value.
    
}


\references{

Bermin H.P. (1996b);
    \emph{Exotic Lookback Options: The case of Extreme Spread
        Options},
    Department of Economics, Lund University, Sweden.

Conze A., Viswanathan R. (1991);
    \emph{Path Dependent Options: The Case of Lookback Options},
    Journal of Finance 46, 1893--1907.

Goldmann B.M., Sosin H.B., Gatto M.A. (1993);
    \emph{Path Dependent Options: Buy at the Low, Sell at the High},
    Journal of Finance 34, 1111.

Haug E.G. (1997); 
    \emph{The Complete Guide to Option Pricing Formulas}, 
    McGraw-Hill, New York.
    
Heynen R.C., Kat H.M. (1994);
    \emph{Selective Memory},
    Risk Magazine 7, 1994.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## Examples from Chapter 2.9 in E.G. Haug's Option Guide (1997)

## Floating Strike Lookback Option [2.9.1]:
   FloatingStrikeLookbackOption(TypeFlag = "c", S = 120, 
     SMinOrMax = 100, Time = 0.5, r = 0.10, b = 0.10-0.06, 
     sigma = 0.30)
  
## Fixed Strike Lookback Option [2.9.2]:
   FixedStrikeLookbackOption(TypeFlag = "c", S = 100, 
     SMinOrMax = 100, X = 105, Time = 0.5, r = 0.10, b = 0.10, 
     sigma = 0.30)
   
## Partial Time Floating Strike Lookback Option [2.9.3]:
   PTFloatingStrikeLookbackOption(TypeFlag = "p", S = 90, 
     SMinOrMax = 90, time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, 
     sigma = 0.20, lambda  = 1)
   
## Partial Time Fixed Strike Lookback Option [2.9.4]:
   PTFixedStrikeLookbackOption(TypeFlag = "c", S = 100, X = 90, 
     time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, sigma = 0.20)
     
## Extreme Spread Option [2.9.5]:
   ExtremeSpreadOption(TypeFlag = "c", S = 100, SMin = NA, 
     SMax = 110, time1 = 0.5, Time2 = 1, r = 0.1, b = 0.1, 
     sigma = 0.30)
   ExtremeSpreadOption(TypeFlag = "cr", S = 100, SMin = 90, 
     SMax = NA, time1 = 0.5, Time2 = 1, r = 0.1, b = 0.1, 
     sigma = 0.30)
        
}


\keyword{math}

