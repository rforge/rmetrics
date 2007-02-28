\name{BasicAmericanOptions}

\alias{BasicAmericanOptions}

\alias{RollGeskeWhaleyOption}
\alias{BAWAmericanApproxOption}
\alias{BSAmericanApproxOption}


\title{Valuation of Basic American Options}


\description{

    A collection and description of functions to valuate 
    basic American options. Approximative formulas for 
    American calls are given for the Roll, Geske and 
    Whaley Approximation, for the Barone-Adesi and Whaley 
    Approximation, and for the Bjerksund and Stensland 
    Approximation.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{RollGeskeWhaleyOption} \tab Roll, Geske and Whaley Approximation, \cr
    \code{BAWAmericanApproxOption} \tab Barone-Adesi and Whaley Approximation, \cr
    \code{BSAmericanApproxOption} \tab Bjerksund and Stensland Approximation. }
    
}


\usage{
RollGeskeWhaleyOption(S, X, time1, Time2, r, D, sigma,
    title = NULL, description = NULL) 
BAWAmericanApproxOption(TypeFlag, S, X, Time, r, b, sigma,
    title = NULL, description = NULL)
BSAmericanApproxOption(TypeFlag, S, X, Time, r, b, sigma,
    title = NULL, description = NULL) 
}


\arguments{

    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{D}{
        a single dividend with time to dividend payout \code{t1}.
        }
    \item{description}{
        a character string which allows for a brief description.
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
    \item{Time}{
        the time to maturity measured in years, a numeric value.
        }
    \item{time1, Time2}{
        [RollGeskeWhaley*] - 
        the first value measures time to dividend payout in years, 
        e.g. 0.25 denotes a quarter, and the second value measures 
        time to maturity measured in years, a numeric value; e.g. 
        0.5 means 6 months.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either "c" for a call option or a "p" 
        for a put option.
        }
    \item{X}{
        the exercise price, a numeric value.
        }

}


\value{
  
    \code{RollGeskeWhaleyOption} \cr
    \code{BAWAmericanApproxOption}
    \cr
    return the option price, a numeric value.
    \cr
    
    \code{BSAmericanApproxOption}
    \cr
    returns a list with the following two elements: \code{Premium} the 
    option price, and \code{TriggerPrice} the trigger price.  
    \cr
 
}


\details{
    
    \bold{Roll-Geske-Whaley Option:}
    \cr\cr
    The function \code{RollGeskeWhaleyOption} valuates American calls 
    on a stock paying a single dividend with specified time to dividend 
    payout according to the pricing formula derived by Roll, Geske and 
    Whaley (1977).
    \cr
    
    
    \code{Approximations for American Options:} 
    \cr\cr
    The function \code{BSAmericanApproxOption} valuates American calls 
    or puts on an underlying asset for a given cost-of-carry rate 
    according to the quadratic approximation method due to Barone-Adesi 
    and Whaley (1987). The function \code{BSAmericanApproxOption} valuates 
    American calls or puts on stocks, futures, and currencies due to 
    the approximation method of Bjerksund and Stensland (1993).
  
}


\note{
  
    The functions implement the algorithms to valuate basic American 
    options as described in Chapter 1.4 of Haug's Option Guide (1997).
    
}


\references{

Barone-Adesi G., Whaley R.E. (1987);
    \emph{Efficient Analytic Approximation of American Option Values},
    Journal of Finance 42, 301--320.
    
Bjerksund P., Stensland G. (1993);
    \emph{Closed Form Approximation of American Options},
    Scandinavian Journal of Management 9, 87--99.
    
Geske R. (1979);
    \emph{A Note on an Analytical Formula for Unprotected
    American Call Options on Stocks with known Dividends},
    Journal of Financial Economics 7, 63--81.

Haug E.G. (1997); 
    \emph{The Complete Guide to Option Pricing Formulas}, 
    Chapter 1, McGraw-Hill, New York.

Roll R. (1977);
    \emph{An Analytic Valuation Formula for Unprotected
    American Call Options on Stocks with known Dividends},
    Journal of Financial Economics 5, 251--258.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## All the examples are from Haug's Option Guide (1997)

## CHAPTER 1.4: ANALYTICAL MODELS FOR AMERICAN OPTIONS
       
## Roll-Geske-Whaley American Calls on Dividend Paying 
   # Stocks [Haug 1.4.1]
   RollGeskeWhaleyOption(S = 80, X = 82, time1 = 1/4, 
     Time2 = 1/3, r = 0.06, D = 4, sigma = 0.30)
      
## Barone-Adesi and Whaley Approximation for American 
   # Options [Haug 1.4.2] vs. Black76 Option on Futures:
   BAWAmericanApproxOption(TypeFlag = "p", S = 100, 
     X = 100, Time = 0.5, r = 0.10, b = 0, sigma = 0.25)
   Black76Option(TypeFlag = "c", FT = 100, X = 100, 
     Time = 0.5, r = 0.10, sigma = 0.25)  
     
## Bjerksund and Stensland Approximation for American Options:
   BSAmericanApproxOption(TypeFlag = "c", S = 42, X = 40, 
     Time = 0.75, r = 0.04, b = 0.04-0.08, sigma = 0.35)
}


\keyword{math}

