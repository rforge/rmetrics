\name{CurrencyTranslatedOptions}

\alias{CurrencyTranslatedOptions}

\alias{FEInDomesticFXOption}
\alias{QuantoOption}
\alias{EquityLinkedFXOption}
\alias{TakeoverFXOption}


\title{Valuation of Currency Translated Options}


\description{
  
    This is a collection of functions to valuate currency 
    translated options. Currency translated options are 
    options on foreign assets where the payoff is exchanged 
    into domestic currency at expiration. For example, a US 
    investor is interested in buying an option that is linked 
    to the Nikkei index that is priced in yen. There are 
    two types or risks, changing prices and exchange rates, 
    to consider when valuing currency-translated options.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{FEInDomesticFXOption} \tab Foreign Exchange In Domestic Currency Option, \cr
    \code{QuantoOption} \tab Quanto Option, \cr
    \code{EquityLinkedFXOption} \tab Equity Linked FX Option, \cr
    \code{TakeoverFXOption} \tab Takeover FX Option. }
    
}


\usage{
FEInDomesticFXOption(TypeFlag, S, E, X, Time, r, q, sigmaS, 
    sigmaE, rho, title = NULL, description = NULL)
QuantoOption(TypeFlag, S, Ep, X, Time, r, rf, q, sigmaS, 
    sigmaE, rho, title = NULL, description = NULL) 
EquityLinkedFXOption(TypeFlag, E, S, X, Time, r, rf, q, 
    sigmaS, sigmaE,rho, title = NULL, description = NULL)
TakeoverFXOption(V, B, E, X, Time, r, rf, sigmaV, sigmaE, 
    rho, title = NULL, description = NULL)
}


\arguments{

    \item{B}{
        [TakeoverFX*] - \cr
        the value of the foreign firm in the foreign currency at 
        the option expiration, a numeric value.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{E}{
        [FEInDomesticFX*] - \cr
        the spot exchange rate specified in units of the domestic
        currency per unit of the foreign currency, a numeric value.\cr
        [TakeoverFX*] - \cr
        the currency price quoted in units of the domestic currency
        per unit of the foreign currency.
        }
    \item{Ep}{
        [Quanto*] - \cr
        the predetermined exchange rate specified in units of domestic
        currency per unit of foreign currency.
        }
    \item{q}{
        [FEInDomesticFX*][EquityLinkedFX*] - \cr
        the instantaneous proportional dividend payout rate of the 
        underlying asset, a numerical value.
        }   
    \item{r}{
        [FEInDomesticFX*][TakeoverFX*] - \cr
        the domestic interest rate, a numeric value. E.g. 0.25 means 
        25\% p.a.
        }
    \item{rf}{
        [TakeoverFX*] - \cr
        the foreign interest rate, a numeric value.
        }
    \item{rho}{
        [TakeoverFX*] - \cr
        the correlation between annualized volatility of the currency price 
        quoted in units of the domestic currency per unit of the foreign 
        currency and the annualized volatility of the value of the foreign 
        firm, a numeric value.
        }   
    \item{S}{
        [FEInDomesticFX*][EquityLinkedFX*] - \cr
        the underlying asset price in foreign currency, a numeric 
        value.
        }
    \item{sigmaE}{
        [TakeoverFX*] - \cr
        the annualized volatility of the currency price quoted in 
        units of the domestic currency per unit of the foreign currency,
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }   
    \item{sigmaS}{
        [Quanto*] - \cr
        the annualized volatility of the underlying asset,
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }
    \item{sigmaV}{
        [TakeoverFX*] - \cr
        the annualized volatility of the value of the foreign firm, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }   
    \item{Time}{
        the time to maturity, a numeric value.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either "c" for a call option or a "p" 
        for a put option.
        }
    \item{V}{
        [TakeoverFX*] - \cr
        the value of the foreign firm in the foreign currency, a 
        numeric value.
        }
    \item{X}{
        [FEInDomesticFX*] - \cr
        the strike (delivery) price in domestic currency, a numeric 
        value. \cr
        [TakeoverFX*] - \cr
        the strike price quoted in units of the domestic currency
        per unit of the foreign currency.
        }

}


\value{

    The option price, a numeric value.

}


\details{

    \bold{Equity Linked Foreign Exchange Options:}
    \cr\cr
    An equity-linked foreign-exchange option is an option on the foreign 
    exchange rate and is linked to the forward price of a stock or equity 
    index. This option can be priced analytically using a model introduced 
    by Reiner (1992).
    \cr
    
  
    \bold{Quanto Options:}
    \cr\cr
    A fixed exchange-rate foreign-equity option (Quanto) is denominated in 
    another currency than that of the underlying equity exposure. The face 
    value of the currency protection expands or contracts to cover changes 
    in the foreign currency value of the underlying asset. Quanto options 
    can be priced analytically using a model published by Dravid, Richardson, 
    and Sun (1993).
    \cr
    
    
    \bold{Foreign Equity Options:}
    \cr\cr
    A foreign equity option is an option on a foreign asset where the strike 
    price is specified in either domestic or foreign currency and the payoff 
    at expiration is valued in domestic currency. Foreign equity options 
    can be priced analytically using a model introduced by Reiner (1992).
    \cr
    
  
    \bold{Takeover Foreign Exchange Options:}
    \cr\cr
    A takeover foreign exchange call option gives the buyer the right purchase 
    a specified number of units of foreign currency at a strike price if the 
    corporate takeover is successful. This option can be priced analytically 
    using a model introduced by Schnabel and Wei (1994).
    \cr
    [Haug's Book, Chapter 2.13.4]

}


\note{

    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 1 of Haug's Book (1997).
    
}


\references{

Haug E.G. (1997); 
    \emph{The Complete Guide to Option Pricing Formulas}, 
    Chapter 2.13, McGraw-Hill, New York.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fOptions.2G-CurrencyTranslatedOptions")

## Examples from Chapter 2.13 in E.G. Haug's Option Guide (1997)

## Foreign Equity Options Struck in Domestic Currency [2.13.1]:
   FEInDomesticFXOption(TypeFlag = "c", S = 100, E = 1.5, 
     X = 160, Time = 0.5, r = 0.08, q = 0.05, sigmaS = 0.20, 
     sigmaE = 0.12, rho = 0.45)

## Fixed Exchange-Rate Foreign-Equity Option [2.13.2]: 
   QuantoOption(TypeFlag = "c", S = 100, Ep = 1.5, X = 105, 
     Time = 0.5, r = 0.08, rf = 0.05, q = 0.04, sigmaS= 0.2, 
     sigmaE = 0.10, rho = 0.30) 

## Equity Linked Foreign Exchange Option [2.13.3]:
   EquityLinkedFXOption(TypeFlag = "p", E = 1.5, S = 100, 
     X = 1.52, Time = 0.25, r = 0.08, rf = 0.05, q = 0.04, 
     sigmaS = 0.20, sigmaE = 0.12, rho = -0.40)

## Takeover Foreign-Exchange Option [2.13.4]:
   TakeoverFXOption(V = 100, B = 100, E = 1.5, X = 1.55, Time = 1, 
     r = 0.08, rf = 0.06, sigmaV = 0.20, sigmaE = 0.25, rho = 0.1)

}


\keyword{math}

