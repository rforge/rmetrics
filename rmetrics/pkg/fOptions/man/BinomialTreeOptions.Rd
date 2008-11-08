\name{BinomialTreeOptions}

\alias{BinomialTreeOptions}

\alias{CRRBinomialTreeOption}
\alias{JRBinomialTreeOption}
\alias{TIANBinomialTreeOption}
\alias{BinomialTreeOption}
\alias{BinomialTreePlot}


\title{Binomial Tree Option Model}


\description{
  
    A collection and description of functions to valuate 
    options in the framework of the Binomial tree option 
    approach.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{CRRBinomialTreeOption} \tab CRR Binomial Tree Option, \cr
    \code{JRBinomialTreeOption} \tab JR Binomial Tree Option, \cr
    \code{TIANBinomialTreeOption} \tab TIAN Binomial Tree Option, \cr
    \code{BinomialTreeOption} \tab Binomial Tree Option, \cr
    \code{BinomialTreePlot} \tab Binomial Tree Plot. }
    
}


\usage{
CRRBinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, 
    Time, r, b, sigma, n, title = NULL, description = NULL)
JRBinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, 
    Time, r, b, sigma, n, title = NULL, description = NULL)
TIANBinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, 
    Time, r, b, sigma, n, title = NULL, description = NULL)

BinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, 
    Time, r, b, sigma, n, title = NULL, description = NULL)
BinomialTreePlot(BinomialTreeValues, dx = -0.025, dy = 0.4, 
    cex = 1, digits = 2, \dots)
}


\arguments{
  
    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{BinomialTreeValues}{
        the return value from the \code{BinomialTreeOption} function.
        }
    \item{cex}{
        a numerical value giving the amount by which the plotting text
        and symbols should be scaled relative to the default.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{digits}{
        an integer value, how many digits should be displayed in the
        option tree?
        }
    \item{dx, dy}{
        numerical values, an offset fine tuning for the placement of
        the option values in the option tree.
        }
    \item{n}{
        number of time steps; an integer value.
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
        the time to maturity measured in years, a numeric 
        value; e.g. 0.5 means 6 months.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either \code{"ce"}, \code{"ca"} for an 
        European or American call option or a \code{"pe"}, \code{"pa"} 
        for a put option, respectively.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    \item{\dots}{
        arguments to be passed.
        }

}


\details{
  
    \bold{CRR Binomial Tree Model:}
    \cr\cr
    Binomial models were first suggested by Cox, Ross and Rubinstein (1979),
    CRR, and then became widely used because of its intuition and easy 
    implementation. Binomial trees are constructed on a discrete-time 
    lattice. With the time between two trading events shrinking to zero, 
    the evolution of the price converges weakly to a lognormal diffusion. 
    Within this mode the European options value converges to the value 
    given by the Black-Scholes formula. 
    \cr
    
    
    \bold{JR Binomial Tree Model:}
    \cr\cr
    There exist many extensions of the CRR model. Jarrow and Rudd (1983), 
    JR, adjusted the CRR model to account for the local drift term. They 
    constructed a binomial model where the first two moments of the 
    discrete and continuous time return processes match. As a consequence 
    a probability measure equal to one half results. Therefore the CRR and
    JR models are sometimes atrributed as equal jumps binomial trees and
    equal probabilities binomial trees.
    \cr
    

    \bold{TIAN Binomial Tree Model:}
    \cr\crTian (1993) suggested to match discrete and continuous local 
    moments up to third order. 
    
    Leisen and Reimer (1996) proved that the order of convergence in 
    pricing European options for all three methods is equal to one, and 
    thus the three models are equivalent.

}


\note{

    Note, the \code{BinomialTree} and \code{BinomialTreePlot} are preliminary 
    implementations.
        
}


\value{

    The option price, a numeric value.
    
}


\references{
    
Broadie M., Detemple J. (1994);
    \emph{American Option Evaluation: New Bounds, Approximations, 
        and a Comparison of Existing Methods}, 
    Working Paper, Columbia University, New York. 

Cox J., Ross S.A., Rubinstein M. (1979);
    \emph{Option Pricing: A Simplified Approach}, 
    Journal of Financial Economics 7, 229--263. 

Haug E.G. (1997); 
    \emph{The complete Guide to Option Pricing Formulas}, 
    McGraw-Hill, New York.
    
Hull J.C. (1998);
    \emph{Introduction to Futures and Options Markets},
    Prentice Hall, London.
    
Jarrow R., Rudd A. (1983); 
    \emph{Option Pricing}, 
    Homewood, Illinois, 183--188. 
    
Leisen D.P., Reimer M., (1996); 
    \emph{Binomial Models for Option Valuation -- Examining and 
        Improving Convergence}, 
    Applied Mathematical Finanace 3, 319--346.

Tian Y. (1993);
    \emph{A Modified Lattice Approach to Option Pricing}, 
    Journal of Futures Markets 13, 563--577. 

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Cox-Ross-Rubinstein Binomial Tree Option Model:
   # Example 14.1 from Hull's Book:
   CRRBinomialTreeOption(TypeFlag = "pa", S = 50, X = 50, 
     Time = 5/12, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   # Example 3.1.1 from Haug's Book:
    CRRBinomialTreeOption(TypeFlag = "pa", S = 100, X = 95, 
        Time = 0.5, r = 0.08, b = 0.08, sigma = 0.3, n = 5)
   # A European Call - Compare with Black Scholes: 
   CRRBinomialTreeOption(TypeFlag = "ce", S = 100, X = 100, 
     Time = 1, r = 0.1, b = 0.1, sigma = 0.25, n = 50)
   GBSOption(TypeFlag = "c", S = 100, X = 100, 
     Time = 1, r = 0.1, b = 0.1, sigma = 0.25)@price
     
## CRR - JR - TIAN Model Comparison:  
   # Hull's Example as Function of "n":
   par(mfrow = c(2, 1), cex = 0.7)
   steps = 50
   CRROptionValue =  JROptionValue = TIANOptionValue = 
     rep(NA, times = steps)
   for (n in 3:steps) { 
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pa", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
     JROptionValue[n] = JRBinomialTreeOption(TypeFlag = "pa", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price 
     TIANOptionValue[n] = TIANBinomialTreeOption(TypeFlag = "pa", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price 
   }           
   plot(CRROptionValue[3:steps], type = "l", col = "red", ylab = "Option Value")
   lines(JROptionValue[3:steps], col = "green")
   lines(TIANOptionValue[3:steps], col = "blue")
   # Add Result from BAW Approximation:
   BAWValue =  BAWAmericanApproxOption(TypeFlag = "p", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
   abline(h = BAWValue, lty = 3)
   title(main = "Convergence")
   data.frame(CRROptionValue, JROptionValue, TIANOptionValue)
   
## Plot CRR Option Tree:
   # Again Hull's Example:
   CRRTree = BinomialTreeOption(TypeFlag = "pa", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
     xlab = "n", ylab = "Option Value")
   title(main = "Option Tree")

}


\keyword{math}

