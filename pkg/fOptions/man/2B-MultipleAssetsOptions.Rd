\name{MultipleAssetsOptions}

\alias{MultipleAssetsOptions}

\alias{TwoAssetCorrelationOption}
\alias{EuropeanExchangeOption}
\alias{AmericanExchangeOption}
\alias{ExchangeOnExchangeOption}
\alias{TwoRiskyAssetsOption}
\alias{SpreadApproxOption}


\title{Valuation of Mutiple Assets Options}


\description{

    A collection and deswcription of functions to valuate 
    multiple asset options. Multiple asset options, as 
    the name implies, are options whose payoff is based 
    on two (or more) assets. The two assets are associated 
    with one another through their correlation coefficient.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{TwoAssetCorrelationOption} \tab Two Asset Correlation Option, \cr
    \code{EuropeanExchangeOption} \tab Exchange-One-Asset-For-Another ..., \cr
    \code{AmericanExchangeOption} \tab ... European or American Option, \cr
    \code{ExchangeOnExchangeOption} \tab  Exchange Option on an Exchange Option, \cr
    \code{TwoRiskyAssetsOption} \tab Option on the Min/Max of 2 Risky Assets, \cr
    \code{SpreadApproxOption} \tab Spread Option Approximation. }
    
}


\usage{
TwoAssetCorrelationOption(TypeFlag, S1, S2, X1, X2, Time, r,
    b1, b2, sigma1, sigma2, rho, title = NULL, description = NULL) 
EuropeanExchangeOption(S1, S2, Q1, Q2, Time, r, b1, b2, 
    sigma1, sigma2, rho, title = NULL, description = NULL)
AmericanExchangeOption(S1, S2, Q1, Q2, Time, r, b1, b2, sigma1,
    sigma2, rho, title = NULL, description = NULL)
ExchangeOnExchangeOption(TypeFlag, S1, S2, Q, time1, Time2, r, 
    b1, b2, sigma1, sigma2, rho, title = NULL, description = NULL)
TwoRiskyAssetsOption(TypeFlag, S1, S2, X, Time, r, b1, 
    b2, sigma1, sigma2, rho, title = NULL, description = NULL)
SpreadApproxOption(TypeFlag, S1, S2, X, Time, r, sigma1, 
    sigma2, rho, title = NULL, description = NULL)
}


\arguments{    
    
    \item{b1, b2}{
        the annualized cost-of-carry rate for the first and second 
        asset, a numeric value; e.g. 0.1 means 10\% pa.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{Q, Q1, Q2}{
        additionally , quantity of the first and second asset.
        }
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% p.a.
        }   
    \item{rho}{
        the correlation coefficient between the returns on the two 
        assets.
        }
    \item{S1, S2}{
        the first and second asset price, numeric values.
        }   
    \item{sigma1, sigma2}{
        the annualized volatility of the first and second underlying 
        security, a numeric value; e.g. 0.3 means 30\% volatility p.a.
        }
    \item{Time}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{time1, Time2}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        usually a character string either \code{"c"} for a call option 
        or a \code{"p"} for a put option, except for \cr
        [ExchangeOnExchange*] -
        a character string either, \cr
        \code{"1"} denotes: option to exchange \code{Q*S2} for the option
        to exchange \code{S2} for \code{S1}, \cr
        \code{"2"} denotes option to exchange the option to exchange \code{S2} 
        for \code{S1}, in return for \code{Q*S2}, \cr
        \code{"3"} denotes: option to exchange \code{Q*S2} for the option
        to exchange \code{S1} for \code{S2}, \cr
        \code{"4"} denotes option to exchange the option to exchange \code{S1} 
        for \code{S2}, in return for \code{Q*S2}; \cr
        [TwoRiskyAssets*] -
        a character string either, \cr
        \code{"cmin"} denotes: call on the minimum, \cr
        \code{"cmax"} denotes: call on the maximum, \cr
        \code{"pmin"} denotes: call on the minimum, \cr
        \code{"pmax"} denotes: call on the maximum of two risky assets.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    \item{X1, X2}{
        the first and second exercise price, numeric values.
        }   
            
}


\details{

    \bold{Two-Asset Correlation Options:}
    \cr\cr
    A two asset correlation options have two underlying assets and two strike 
    prices. A two asset correlation call option on two assets S1 and S2 with 
    a strike prices X1 and X2 has a payoff of max(S2-X2,0) if S1>X1 and 0 
    otherwise, and a put option has a payoff of max(X2-S2,0) if S1<X1 and 0 
    otherwise. Two asset correlation options can be priced analytically using 
    a model introduced by Zhang (1995).
    \cr
    [Haug's Book, Chapter 2.8.1]
    \cr
    

    \bold{Exchange-One-Asset-For-Another Options:}
    \cr\cr
    The exchange option gives the holder the right to exchange one asset for 
    another. The payoff for this option is the difference between the prices 
    of the two assets at expiration. The analytical calculation of European 
    exchange option is based on a modified Black Scholes formula originally 
    introduced by Margrabe (1978). A binomial lattice is used for the numerical 
    calculation of an American or European style exchange option.
    \cr
    [Haug's Book, Chapter 2.8.2]
    \cr
    

    \bold{Exchange-On-Exchange Options:}
    \cr\cr
    Exchange options on exchange options can be found embedded in many 
    sequential exchange opportunities [1]. As an example, a bond holder 
    converting into a stock, later exchanging the shares received for stocks 
    of an acquiring firm. This complex option can be priced analytically 
    using a model introduced by Carr (1988).
    \cr
    [Haug's Book, Chapter 2.8.3]
    \cr
    

    \bold{Portfolio Options:}
    \cr\cr
    A portfolio option is an American (or European) style option on the 
    maximum of the sum of the prices of two assets and a fixed strike price. 
    A portfolio call option on two assets S1 and S2 with a strike price X 
    has a payoff of max((S1+S2)-X,0) and a put option has a payoff of 
    max((X-(S1+S2),0). A binomial lattice is used for the numerical 
    calculation of an American or European style portfolio options.
    \cr
    

    \bold{Rainbow Options:}
    \cr\cr
    A rainbow option is an American (or European) style option on the maximum 
    (or minimum) of two underlying assets. These types of rainbow options are 
    generally referred to as two-color rainbow options. There are four general 
    types of two-color rainbow options: maximum or best of two risky assets, 
    the minimum or worst of two risky assets, the better of two risky assets, 
    and the worse of two risky assets. A maximum rainbow call option on two 
    assets S1 and S2 with a strike price X has a payoff of max(max(S1,S2)-X,0) 
    and a put option has a payoff of max(X-max(S1,S2),0). A minimum rainbow 
    call option on two assets S1 and S2 with a strike X has a payoff of 
    max(min(S1,S2)-X,0) and a put option has a payoff of max(X-min(S1,S2),0). 
    Set the Strike parameter to a very small number (1e-8) to calculate better 
    and worse rainbow option types. The analytical calculation of European 
    rainbow option is based on Rubinstein's (1991) model. A binomial 
    lattice is used for the numerical calculation of an American or European 
    style rainbow options.
    \cr
    

    \bold{Spread Options:}
    \cr\cr
    A spread option is a standard option on the difference of the values of 
    two assets. Spread options a related to exchange options. If the strike 
    price is set to zero, a spread option is equivalent to an exchange option. 
    A spread call option on two assets S1 and S2 with a strike price X has 
    a payoff of max(S1-S2-X,0) and a put option has a payoff of max(X-S1+S2,0). 
    The analytical calculation of European spread option is based on 
    Gauss-Legendre integration and the Black-Scholes model. A binomial 
    lattice is used for the numerical calculation of an American or European 
    style spread options.
    \cr
    [Haug's Book, Chapter 2.8.5]
    \cr
    

    \bold{Dual Strike Options:}
    \cr\cr
    A dual strike option is an American (European) option whose payoff 
    involves receiving the best payoff of two standard American (European) 
    style plain options. These options have two underlying assets and two 
    strike prices. The payoff of a dual strike call option is the maximum 
    of asset one minus strike one or asset two minus strike two. The payoff 
    of a dual strike put option is the maximum of strike one minus asset one 
    or strike two minus asset two. The payoff of a reverse dual strike call 
    option is the maximum of asset one minus strike one or strike two minus 
    asset two. The payoff of a reverse dual strike put option is the maximum 
    of strike one minus asset one or asset two minus strike two. A binomial 
    lattice is used for the numerical calculation of an American or European 
    style dual strike and reverse dual strike options.

}


\note{
  
    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 2.8 of Haug's Book (1997).
    
}


\value{

    The option price, a numeric value.

}


\references{
Black F. (1976);
    \emph{The Pricing of Commodity Contracs},
    Journal of Financial Economics 3, 167--179.

Boyle P.P., Evnine J., Gibbs S. (1989);
    \emph{Numerical Evaluation of Multivariate Contingent Claims},
    Review of Financial Studies 2, 241--250.

Boyle P.P., Tse Y.K. (1990);
    \emph{An Algorithm for Computing Values of Options on the
        Maximum or Minimum of Several Assets},
    Journal of Financial and Quantitative Analysis 25, 215--227.

Carr P.P. (1988)
    \emph{The Valuation of Sequential Exchange Opportunities},
    Journal of Finance 43, 1235--1256.
    
Haug E.G. (1997); 
    \emph{The Complete Guide to Option Pricing Formulas}, 
    McGraw-Hill, New York.

Johnson H. (1987)
    \emph{Options on the Maximum or the Minimum of Several Assets},
    Journal of Financial and Quantitative Analysis 22, 277--283.
        
Kirk E. (1995);
    \emph{Correlation in the Energy Markets},
    in: Managing Energy Price Risk,
    Risk Publications and Enron, London, pp. 71--78.
    
Margrabe W. (1998);
    \emph{The Value of an Option to Exchange one Asset for Another},
    Journal of Finance 33, 177--186.
    
Rich D.R, Chance D.M. (1993);
    \emph{An Alternative Approach to the Pricing of Options on 
        Multiple Assets},
    Journal of Financial Engineering 2, 271--285.

Rubinstein M. (1991)
    \emph{Somewhere over the Rainbow},
    Risk Magazine 4, 10.
    
Stulz R.M. (1982);
    \emph{Options on the Minimum or Maximum of Two Risky Assets},
    Journal of Financial Economics 10, 161--185.
    
Zhang P.G. (1995); 
    \emph{Correlation Digital Options}
    Journal of Financial Engineering 3, 5. 
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## Examples from Chapter 2.8 in E.G. Haug's Option Guide (1997)

## Two Asset Correlation Options [2.8.1]:
   TwoAssetCorrelationOption(TypeFlag = "c", S1 = 52, S2 = 65, 
     X1 = 50, X2 = 70, Time = 0.5, r = 0.10, b1 = 0.10, b2 = 0.10, 
     sigma1 = 0.2, sigma2 = 0.3, rho = 0.75) 

## European Exchange Options [2.8.2]: 
   EuropeanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
     Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     sigma2 = 0.25, rho = -0.5)
     
## American Exchange Options [2.8.2]:
   AmericanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
     Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     sigma2 = 0.25, rho = -0.5)

## Exchange Options On Exchange Options [2.8.3]:
   for (flag in 1:4) print(
   ExchangeOnExchangeOption(TypeFlag = as.character(flag), 
     S1 = 105, S2 = 100, Q = 0.1, time1 = 0.75, Time2 = 1.0, r = 0.1, 
     b1 = 0.10, b2 = 0.10, sigma1 = 0.20, sigma2 = 0.25, rho = -0.5))

## Two Risky Assets Options [2.8.4]:
   TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = 100, S2 = 105, 
     X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)
   TwoRiskyAssetsOption(TypeFlag = "pmax", S1 = 100, S2 = 105, 
     X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)

## Spread-Option Approximation [2.8.5]:
   SpreadApproxOption(TypeFlag = "c", S1 = 28, S2 = 20, X = 7, 
     Time = 0.25, r = 0.05, sigma1 = 0.29, sigma2 = 0.36, rho = 0.42)
    
}


\keyword{math}

