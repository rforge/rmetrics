\name{EBMAsianOptions}

\alias{EBMAsianOptions}

\alias{MomentMatchedAsianOption}
\alias{MomentMatchedAsianDensity}

\alias{GramCharlierAsianOption}

\alias{AsianOptionMoments}
\alias{DufresneAsianOptionMoments}
\alias{AbrahamsonAsianOptionMoments}
\alias{TurnbullWakemanAsianOptionMoments}
\alias{TolmatzAsianOptionMoments}

%\alias{Schroeder1AsianDensity}
%\alias{Schroeder2AsianDensity}
%\alias{Yor1AsianDensity}
%\alias{Yor2AsianDensity}
%\alias{TolmatzAsianDensity}
%\alias{TolmatzAsianProbability}

\alias{ZhangAsianOption}
\alias{VecerAsianOption}
\alias{ZhangApproximateAsianOption} 

\alias{gGemanYor}
\alias{GemanYorAsianOption}
\alias{gLinetzky}
\alias{LinetzkyAsianOption}

\alias{BoundsOnAsianOption}
\alias{CurranThompsonAsianOption}
\alias{RogerShiThompsonAsianOption}   
\alias{ThompsonAsianOption}
\alias{TolmatzAsianOption}

\alias{CallPutParityAsianOption}
\alias{WithDividendsAsianOption}

\alias{FuMadanWangTable}
\alias{FusaiTaglianiTable}
\alias{GemanTable}
\alias{LinetzkyTable}
\alias{ZhangTable}
\alias{ZhangLongTable}
\alias{ZhangShortTable}


\title{Exponential Brownian Motion Distributions}


\description{

    A collection and description of functions 
    used in the theory of exponential Brownian 
    Motion and in the valuation of Asian options.
    \cr
    
    The functions for Moment matching and Series Expansions are:

    \tabular{ll}{
    \code{MomentMatchedAsianOption} \tab Valuate moment matched option prices, \cr
    \code{... method="LN"} \tab Log-Normal Approximation of Levy, Turnbull and Wakeman, \cr
    \code{... method="RG"} \tab Reciprocal-Gamma Approximation of Milevski and Posner, \cr
    \code{... method="JI"} \tab Johnson Type I Approximation of Posner and Milevsky, \cr
    \code{MomentMatchedAsianDensity} \tab Valuate moment matched option densities, \cr
    \code{... method="LN"} \tab Log-Normal Approximation, \cr
    \code{... method="RG"} \tab Reciprocal-Gamma Approximation, \cr
    \code{... method="JI"} \tab Johnson Type I Approximation, \cr
    \code{GramCharlierAsianOption} \tab Calculate Gram-Charlier option prices. }

    \tabular{ll}{
    \code{AsianOptionMoments} \tab Methods to calculate Asian Moments, \cr
    \code{... method="A"} \tab Moments from Abrahamson's Formula, \cr
    \code{... method="D"} \tab Moments from Dufresne's Formula, \cr
    \code{... method="TW"} \tab First 2 Moments from Turnbull-Wakeman, \cr
    \code{... method="T"} \tab Asymptotic Behavior after Tolmatz. }


    %\tabular{ll}{
    %\code{Schroeder1AsianDensity} \tab 
    %\code{Schroeder2AsianDensity} \tab 
    %\code{Yor1AsianDensity} \tab 
    %\code{Yor2AsianDensity} \tab 
    %\code{TolmatzAsianDensity} \tab 
    %\code{TolmatzAsianProbability} \tab 


    \tabular{ll}{
    \code{ZhangAsianOption} \tab Asian option price by Zhang's 1D PDE, \cr
    \code{VecerAsianOption} \tab Asian option price by Vecer's 1D PDE. }

    \tabular{ll}{
    \code{gGemanYor} \tab Function to be Laplace inverted, \cr
    \code{GemanYorAsianOption} \tab Asian option price by Laplace Inversion, \cr
    \code{gLinetzky} \tab Function to be integrated, \cr
    \code{LinetzkyAsianOption} \tab Asian option price by Spectral Expansion. }

    \tabular{ll}{
    \code{BoundsOnAsianOption} \tab Lower and upper bonds on Asian calls, \cr
    \code{CurranThompsonAsianOption} \tab From Thompson's continuous limit, \cr
    \code{RogerShiThompsonAsianOption} \tab From Thompson's single integral formula, \cr  
    \code{ThompsonAsianOption} \tab Thompson's upper bound, \cr
    \code{TolmatzAsianOption} \tab Lower Bound from Tolmatz' symptotics. }

    \tabular{ll}{
    \code{CallPutParityAsianOption} \tab Call-Put parity Relation, \cr
    \code{WithDividendsAsianOption} \tab Adds dividends to Asian option formula. }

    \tabular{ll}{
    \code{FuMadanWangTable} \tab Table from Fu, Madan and Wang's paper, \cr
    \code{FusaiTaglianiTable} \tab Table from Fusai und tagliani's paper, \cr
    \code{GemanTable} \tab Table from Geman's paper, \cr
    \code{LinetzkyTable} \tab Table from Linetzky's paper, \cr
    \code{ZhangTable} \tab Table from Zhang's paper, \cr
    \code{ZhangLongTable} \tab Long Table from Zhang's paper, \cr
    \code{ZhangShortTable} \tab Short Table from Zhang's paper. }
    
    Sorry - The Documentation is still Uncomplete.

}


\usage{
MomentMatchedAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, table = NA, method = c("LN", "RG", "JI"))
MomentMatchedAsianDensity(x, Time = 1, r = 0.09, sigma = 0.30, 
    method = c("LN", "RG", "JI"))
GramCharlierAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, table = NA, method = c("LN", "RG", "JI"))

AsianOptionMoments(M = 4, Time = 1, r = 0.045, sigma = 0.30, log = FALSE, 
    method = c("A", "D", "TW", "T"))

%Schroeder1AsianDensity()
%Schroeder2AsianDensity()
%Yor1AsianDensity()
%Yor2AsianDensity()
%TolmatzAsianDensity()
%TolmatzAsianProbability()

ZhangAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, table = NA, correction = TRUE, nint = 800, 
    eps = 1.0e-8, dt = 1.0e-10)
VecerAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, table = NA, nint = 800, eps = 1.0e-8, 
    dt = 1.0e-10)  

gGemanYor(lambda, S = 100, X = 100, Time = 1, r = 0.05, sigma = 0.30, 
    log = FALSE, doplot = FALSE)
GemanYorAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, doprint = FALSE)
gLinetzky(x, y, tau, nu, ip = 0)
LinetzkyAsianOption(TypeFlag = c("c", "p"), S = 2, X = 2, Time = 1, 
    r = 0.02, sigma = 0.1, table = NA, lower = 0, upper = 100, 
    method = "adaptive", subdivisions = 100, ip = 0, doprint = TRUE, 
    doplot = TRUE, \dots)

BoundsOnAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30, table = NA, method = c("CT", "RST", "T"))
CurranThompsonAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, 
    Time = 1, r = 0.09, sigma = 0.30)
RogerShiThompsonAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, 
    Time = 1, r = 0.09, sigma = 0.30)   
ThompsonAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30)
TolmatzAsianOption(TypeFlag = c("c", "p"), S = 100, X = 100, Time = 1, 
    r = 0.09, sigma = 0.30)

CallPutParityAsianOption(TypeFlag = "p", Price = 8.828759, S = 100, 
    X = 100, Time = 1, r = 0.09, sigma = 0.3, table = NA)
WithDividendsAsianOption(TypeFlag = "c", Dividends = 0.45, S = 100, 
    X = 100, Time = 1, r = 0.09, sigma = 0.3, 
    calculator = MomentMatchedAsianOption, method = "LN")

FuMadanWangTable()
FusaiTaglianiTable()
GemanTable()
LinetzkyTable()
ZhangTable()
ZhangLongTable()
ZhangShortTable()
}


\arguments{
  
    \item{calculator}{
        [WithDividendsAsianOption] - \cr
        the name of the function selecting the option calculator
        to be used.
        }
    \item{correction}{
        [ZhangAsianOption] - \cr
        xxx.
        }
    \item{Dividends}{
        [WithDividendsAsianOption] - \cr
        xxx.
        }
    \item{doplot}{
        [gGemanYor][LinetzkyAsianOption] - \cr
        xxx.
        }
    \item{doprint}{
        [GemanYorAsianOption][LinetzkyAsianOption] - \cr
        xxx.
        }
    \item{dt}{
        [VecerAsianOption][ZhangAsianOption] - \cr
        xxx.
        }
    \item{eps}{
        [VecerAsianOption][ZhangAsianOption] - \cr
        xxx.
        }
    \item{ip}{
        [gLinetzky] - \cr
        xxx.
        }
    \item{lambda}{
        [gGemanYor] - \cr
        xxx.
        }
    \item{log}{
        [AsianOptionMoments][gGemanYor] - \cr
        xxx.
        }
    \item{lower, upper}{
        [LinetzkyAsianOption] - \cr
        xxx.
        }
    \item{M}{
        [*] - \cr
        xxx.
        }
    \item{method}{
        [*] - \cr
        xxx.
        }
    \item{nint}{
        [*] - \cr
        xxx.
        }
    \item{nu}{
        [*] - \cr
        xxx.
        }
    \item{Price}{
        [*] - \cr
        xxx.
        }
    \item{r}{
        a numeric value, the annualized rate of interest; 
        e.g. 0.25 means 25\% pa.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{sigma}{
        a numeric value, the annualized volatility of the underlying 
        security; e.g. 0.3 means 30\% volatility pa.
        }
    \item{subdivisions}{
        [*] - \cr
        xxx.
        }
    \item{table}{
        [*] - \cr
        xxx.
        }
    \item{tau}{
        [*] - \cr
        xxx.
        }
    \item{Time}{
        a numeric value, the time to maturity measured in years; 
        e.g. 0.5 means 6 months.
        }
    \item{TypeFlag}{
        a character string either \code{"c"} for a call option or 
        a \code{"p"} for a put option.
        }
    \item{x}{
        [*] - \cr
        xxx.
        }
    \item{X}{
        a numeric value, the exercise price.
        }
    \item{y}{
        [*] - \cr
        xxx.
        }
    \item{\dots}{
        [*] - \cr
        xxx.
        }
        
        
}


%\details{
%
%   ...
%   
%}


%\value{
%  
%    The functions ...
%    
%}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Examples:
   # none ...
}


\keyword{math}

