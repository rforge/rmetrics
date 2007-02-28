\name{AsianOptions}

\alias{AsianOptions}

\alias{GeometricAverageRateOption}
\alias{TurnbullWakemanAsianApproxOption}
\alias{LevyAsianApproxOption}


\title{Valuation of Asian Options}


\description{
  
    This is a collection of functions to valuate Asian 
    options. Asian options are path-dependent options, 
    with payoffs that depend on the average price of the 
    underlying asset or the average exercise price. 
    There are two categories or types of Asian options: 
    average rate options (also known as average price 
    options) and average strike options. The payoffs 
    depend on the average price of the underlying asset 
    over a predetermined time period. An average is less 
    volatile than the underlying asset, therefore making 
    Asian options less expensive than standard European 
    options. Asian options are commonly used in currency 
    and commodity markets. Asian options are of interest 
    in markets with thinly traded assets. Due to the 
    little effect it will have on the option's value, 
    options based on an average, such as Asian options, 
    have a reduced incentive to manipulate the underlying 
    price at expiration.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{GeometricAverageRateOption} \tab Geometric Average Rate Option, \cr
    \code{TurnbullWakemanAsianApproxOption} \tab Turnbull and Wakeman's Approximation, \cr
    \code{LevyAsianApproxOption} \tab Levy's Approximation. }
    
}


\usage{
GeometricAverageRateOption(TypeFlag, S, X, Time, r, b, sigma,
    title = NULL, description = NULL)
TurnbullWakemanAsianApproxOption(TypeFlag, S, SA, X, Time, time, 
    tau, r, b, sigma, title = NULL, description = NULL)
LevyAsianApproxOption(TypeFlag, S, SA, X, Time, time, r, b, 
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
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% pa.
        }
    \item{S, SA}{
        the asset price, a numeric value.
        }
    \item{sigma}{
        the annualized volatility of the underlying security, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.}
    \item{tau}{
        [TurnWakeAsianApprox*] - \cr
        is the time to the beginning of the average period.
        }
    \item{time, Time}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either \code{"c"} for a call option or 
        a \code{"p"} for a put option.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    
}


\details{

    The Geometric average is the nth root of the product of the n sample 
    points. The Arithmetic average is the sum of the stock values divided 
    by the number of sampling points. Although Geometric Asian options are 
    not commonly used in practice, they are often used as a good initial 
    guess for the price of arithmetic Asian options. This technique is 
    used to improve the convergence rate of the Monte Carlo model when 
    pricing arithmetic Asian options.
    \cr

    Two cases are considered, the geometric and the arithmetic average-rate 
    option. For the latter one can choose between three different kinds of 
    approximations: Turnbull and Wakeman's approximations, Levy's approximation 
    and Curran's approximation.
    \cr
    [Haug's Book, Chapter 2.12]
}

\note{

    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 2.12 of Haug's Book (1997).
    
}


\value{

    The option price, a numeric value.
    
}


\references{
Haug E.G. (1997); 
    \emph{The complete Guide to Option Pricing Formulas}, 
    Chapter 2.12, McGraw-Hill, New York.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## Examples from Chapter 2.12 in E.G. Haug's Option Guide (1997)

## Geometric Average Rate Option:
   GeometricAverageRateOption(TypeFlag = "p", S = 80, X = 85, 
     Time = 0.25, r = 0.05, b = 0.08, sigma = 0.20)

## Turnbull Wakeman Approximation:
   TurnbullWakemanAsianApproxOption(TypeFlag = "p", S = 90, SA = 88, 
     X = 95, Time = 0.50, time = 0.25, tau = 0.0, r = 0.07, 
     b = 0.02, sigma = 0.25)

## Levy Asian Approximation:   
   LevyAsianApproxOption(TypeFlag = "c", S = 100, SA = 100, X = 105, 
     Time = 0.75, time = 0.50, r = 0.10, b = 0.05, sigma = 0.15)
     
}


\keyword{math}

