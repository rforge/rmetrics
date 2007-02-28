\name{PlainVanillaOptions}

\alias{PlainVanillaOptions}

\alias{fOPTION}
\alias{fOPTION-class}

\alias{GBSOption}
\alias{GBSCharacteristics}
\alias{BlackScholesOption}
\alias{GBSGreeks}
\alias{GBSVolatility}

\alias{Black76Option}
\alias{MiltersenSchwartzOption}

\alias{NDF}
\alias{CND}
\alias{CBND}

\alias{print.option}
\alias{summary.option}

\alias{print.fOPTION}
\alias{summary.fOPTION}


\title{Valuation of Plain Vanilla Options}


\description{

    A collection and description of functions to valuate 
    plain vanilla options. Included are functions for the 
    Generalized Block-Scholes option pricing model, for 
    options on futures, some utility functions, and print 
    and summary methods for options.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{GBS*} \tab the generalized Block-Scholes option, \cr
    \code{BlackScholesOption} \tab a synonyme for the GBSOption, \cr
    \code{Black76Option} \tab options on Futures, \cr
    \code{MiltersenSchwartzOption} \tab options on commodity futures, \cr
    \code{NDF, CND, CBND} \tab distribution functions, \cr
    \code{print} \tab print method for Options, \cr
    \code{summary} \tab summary method for Options. }
    
}


\usage{
GBSOption(TypeFlag, S, X, Time, r, b, sigma, 
    title = NULL, description = NULL)
GBSGreeks(Selection, TypeFlag, S, X, Time, r, b, sigma)
GBSCharacteristics(TypeFlag, S, X, Time, r, b, sigma)
GBSVolatility(price, TypeFlag, S, X, Time, r, b, tol, maxiter)
BlackScholesOption(\dots)

Black76Option(TypeFlag, FT, X, Time, r, sigma, 
    title = NULL, description = NULL)

MiltersenSchwartzOption(TypeFlag, Pt, FT, X, time, Time, 
    sigmaS, sigmaE, sigmaF, rhoSE, rhoSF, rhoEF, KappaE, KappaF, 
    title = NULL, description = NULL)

NDF(x)
CND(x)
CBND(x1, x2, rho)

\method{print}{fOPTION}(x, \dots)
\method{summary}{fOPTION}(object, \dots)

\method{print}{option}(x, \dots)
\method{summary}{option}(object, \dots)
}


\arguments{

    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{FT}
        {[Black76*][MiltersenSchwartz*] - \cr
        the futures price, a numeric value.
        }
    \item{KappaE, KappaF}{
        [MiltersenSchwartz*] - \cr
        the speed of mean reversion of the forward interest rate (E),
        the speed of mean reversion of the convenience yield (F), 
        a numeric value.
        }
    \item{maxiter, tol}{
        [GBSVolatility*] - \cr
        the maximum number of iterations and the tolerance to compute
        the root of the GBS volatility equation, see \code{uniroot}.
        }
    \item{object}{
        an object of class \code{"option"}.
        }
    \item{price}{
        [GBSVolatility*] - \cr
        the price of the GBS option, a numerical value.
        }
    \item{Pt}{
        [MiltersenSchwartz*] - \cr
        the zero coupon bond that expires on the option
        maturity; a numeric value.
        }
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% pa.
        }
    \item{rhoSE, rhoSF, rhoEF}{
        [MiltersenSchwartz*] - \cr
        the correlations 
        between the spot commodity price and the future convenience yield (SE),
        between the spot commodity price and the forward interest rate (SF),
        between the forward interest rate and the future convenience yield (EF), 
        a numeric value.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{Selection}{
        [GBSGreeks] - \cr
        sensitivity to be computed, one of \code{"delta"}, \code{"gamma"},
        \code{"vega"}, \code{"theta"}, \code{"rho"}, or \code{"CoC"}, 
        a string value.
        }
    \item{sigma}{
        the annualized volatility of the underlying security, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }
    \item{sigmaS, sigmaE, sigmaF}{
        [MiltersenSchwartz*] - \cr
        numeric values, the annualized volatility 
        of the spot commodity price (S),
        of the future convenience yield (E), and
        of the forward interest rate (F),
        e.g. 0.25 means 25\% pa.
        }
    \item{time, Time}{
        the time to maturity measured in years, a numeric value.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either \code{"c"} for a call option or 
        a \code{"p"} for a put option.
        }
    \item{x, x1, x2, rho}{
        [NDF][CND][CBND] - \cr
        the function argument \code{x} for the normal distribution 
        function \code{NDF} and the cumulated normal distribution 
        \code{CND}. The arguments for the bivariate function are
        named \code{x1} and \code{x2}; \code{rho} is the correlation
        coefficient. \cr
        [print] - \cr
        the object \code{x} to be printed.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    \item{\dots}{
        arguments to be passed.
        }

}


\value{
  
    \code{GBSOption}\cr
    \code{BlackScholesOption}
    \cr
    returns an object of class \code{"fOption"}. 
    \cr

    \code{GBSGreeks}
    \cr
    returns the option sensitivity for the selected Greek, a 
    numeric value.
    \cr
  
    \code{GBSCharacteristics}
    \cr
    returns a list with the following entries:
    \code{premium}, the option price,
    \code{delta}, the delta sensitivity, 
    \code{gamma}, the gamma sensitivity,
    \code{theta}, the theta sensitivity,
    \code{vega}, the vega sensitivity, 
    \code{rho}, the rho sensitivity,
    \code{lambda}, the lambda sensitivity.
    \cr
    
    \code{GBSVolatility}
    \cr
    returns the GBS option implied volatility for a given price.
    \cr
    
    \code{Black76Option},\cr 
    \code{MiltersenSchwartzOption}
    \cr
    return an object of class \code{"fOption"}. 
    
    
    The option valuation programs return an object of class 
    \code{"fOPTION"} with the following slots:

    \item{@call}{
        the function call.      
        }
    \item{@parameters}{
        a list with the input parameters.
        }
    \item{@price}{
        a numeric value with the value of the option.
        }
    \item{@title}{
        a character string with the name of the test.
        }
    \item{@description}{
        a character string with a brief description of the
        test.
        }
 
}


\details{

    \bold{Generalized Black Scholes Options:}
    \cr\cr
    \code{GBSOption} calculates the option price, \code{GBSGreeks}
    calculates option sensitivities delta, theta, vega, rho, lambda 
    and gamma, and \code{GBScharacterisitics} does both.
    \code{GBSVolatility} computes the implied volatility.
    \cr
    Note, that setting \code{b = r} we get Black and Scholes' stock option 
    model, \code{b = r-q} we get Merton's stock option model with continuous 
    dividend yield \code{q}, \code{b = 0} we get Black's futures option 
    model, and \code{b = r-rf} we get Garman and Kohlhagen's currency 
    option model with foreign interest rate \code{rf}.
    \cr
  
        
    \bold{Options on Futures:}
    \cr\cr
    The \code{Black76Option} pricing formula is applicable for valuing 
    European call and European put options on commodity futures. The 
    exact nature of the underlying commodity varies and may be anything 
    from a precious metal such as gold or silver to agricultural products.
    \cr
    The \code{Miltersen Schwartz Option} model is a three factor model 
    with stochastic futures prices, term structures and convenience 
    yields, and interest rates. The model is based on lognormal 
    distributed commodity prices and normal distributed continuously 
    compounded forward interest rates and future convenience yields.
    \cr
    
    
    \bold{Miltersen Schwartz Options:}
    \cr\cr
    The \code{MiltersenSchwartzOption} function allows for pricing 
    options on commodity futures. The model is a three factor model
    with stochastic futures prices, term structures of convenience 
    yields, and interest rates. The model is based on lognormal
    distributed commodity prices and normal distributed continuously
    compounded forward interest rates and futures convenience yields.
    \cr
    
    
    \bold{Distribution Functions:}
    \cr\cr
    The functions \code{NDF}, \code{CND}, and \code{CBND} compute 
    vlues for the Normal density functions, for the normal 
    probability function, and for the bivariate normal probability
    functions. The functions are implemented as described in the
    book of E.G. Haug.
    \cr
    
    
    \bold{Print and Summary Method:}
    \cr\cr
    Thes are two methods to print and sumarize an object of class
    \code{"fOPTION"} or of class{"option"}. The second is used 
    for the older class representation.
  
}


\note{
  
    The functions implement algorithms to valuate plain vanilla 
    options and to compute option Greeks as described in Chapter 1 
    of Haug's Option Guide (1997).
    
}


\references{

Black F., Scholes M. (1973);
    \emph{The Pricing of Options and Corporate Liabilities},
    Journal of Political Economy 81, 637--654.

Haug E.G. (1997); 
    \emph{The Complete Guide to Option Pricing Formulas}, 
    Chapter 1, McGraw-Hill, New York.

Hull J.C. (1998);
    \emph{Introduction to Futures and Options Markets},
    Prentice Hall, London.
    
Miltersen K., Schwartz E.S. (1998);
    \emph{Pricing of Options on Commodity Futures with
    Stochastic Term Structuures of Convenience Yields and 
    Interest Rates},
    Journal of Financial and Quantitative Analysis 33, 33--59.

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## All the examples are from Haug's Option Guide (1997)

## CHAPTER 1.1: ANALYTICAL FORMULAS FOR EUROPEAN OPTIONS:

## Black Scholes Option [Haug 1.1.1]
   GBSOption(TypeFlag = "c", S = 60, X = 65, Time = 1/4, r = 0.08, 
     b = 0.08, sigma = 0.30)
        
## European Option on a Stock with Cash Dividends [Haug 1.1.2]
   S0  =  100; r  =  0.10; D1  =  D2  =  2; t1  =  1/4; t2  =  1/2
   S  =  S0 - 2*exp(-r*t1) - 2*exp(-r*t2)
   GBSOption(TypeFlag = "c", S = S, X = 90, Time = 3/4, r = r, b = r, 
     sigma = 0.25)
   
## Options on Stock Indexes [Haug 1.2.3]
   GBSOption(TypeFlag = "p", S = 100, X = 95, Time = 1/2, r = 0.10, 
     b = 0.10-0.05, sigma = 0.20)  
        
## Option on Futures [Haug 1.1.4]
   FuturesPrice  =  19
   GBSOption(TypeFlag = "c", S = FuturesPrice, X = 19, Time = 3/4, 
     r = 0.10, b = 0, sigma = 0.28)  
        
## Currency Option [Haug 1.1.5]
   r  =  0.06; rf  =  0.08
   GBSOption(TypeFlag = "c", S = 1.5600, X = 1.6000, 
     Time = 1/2, r = 0.06, b = 0.06-0.08, sigma = 0.12)
        
## Delta of GBS Option [Haug 1.3.1]
   GBSGreeks(Selection = "delta", TypeFlag = "c", S = 105, X = 100, 
     Time = 1/2, r = 0.10, b = 0, sigma = 0.36)

## Gamma of GBS Option [Haug 1.3.3]
   GBSGreeks(Selection = "gamma", TypeFlag = "c", S = 55, X = 60, 
     Time = 0.75, r = 0.10, b = 0.10, sigma = 0.30)
   
## Vega of GBS Option [Haug 1.3.4]
   GBSGreeks(Selection = "vega", TypeFlag = "c", S = 55, X = 60, 
     Time = 0.75, r = 0.10, b = 0.10, sigma = 0.30)
   
## Theta of GBS Option [Haug 1.3.5]
    GBSGreeks(Selection = "theta", TypeFlag = "p", S = 430, X = 405, 
     Time = 0.0833, r = 0.07, b = 0.07-0.05, sigma = 0.20)
   
## Rho of GBS Option [Haug 1.3.5]
   GBSGreeks(Selection = "rho", TypeFlag = "c", S = 72, X = 75, 
     Time = 1, r = 0.09, b = 0.09, sigma = 0.19)
   
## CHAPTER 1.3 OPTIONS SENSITIVITIES:
   
## The Generalized Black Scholes Option Formula
   GBSCharacteristics(TypeFlag = "p", S = 1.5600, X = 1.6000, 
     Time = 1, r = 0.09, b = 0.09, sigma = 0.19)
  
## CHAPTER 1.5: RECENT DEVELOPMENTS IN COMMODITY OPTIONS 

## Miltersen Schwartz Option vs. Black76 Option on Futures:
   MiltersenSchwartzOption(TypeFlag = "c", Pt = exp(-0.05/4), FT = 95, 
     X = 80, time = 1/4, Time = 1/2, sigmaS = 0.2660, sigmaE = 0.2490, 
     sigmaF = 0.0096, rhoSE = 0.805, rhoSF = 0.0805, rhoEF = 0.1243, 
     KappaE = 1.045, KappaF = 0.200)
   Black76Option(TypeFlag = "c", FT = 95, X = 80, Time = 1/2, r = 0.05, 
     sigma = 0.266)  

}


\keyword{math}

