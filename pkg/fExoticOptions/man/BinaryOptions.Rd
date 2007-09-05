\name{BinaryOptions}

\alias{BinaryOptions}

\alias{GapOption}
\alias{CashOrNothingOption}
\alias{TwoAssetCashOrNothingOption}
\alias{AssetOrNothingOption}
\alias{SuperShareOption}
\alias{BinaryBarrierOption}


\title{Valuation of Binary Options}


\description{
  
    A collection and description of functions to valuate 
    binary options. Binary options, also known as digital 
    options, have discontinuous payoffs. They can be used 
    as building blocks to develop options with more 
    complicated payoffs. For example, a regular European 
    call option is equivalent to a long position in an 
    asset-or-nothing call and a short position in a 
    cash-or-nothing call, where the both options have the 
    same strike price and the cash payoff of the 
    cash-or-nothing option equals the strike price. Unlike 
    standard European style options, the payout for binary 
    options does not depend on how much it is in-the-money 
    but rather whether or not it is on the money. The option's 
    payoff is fixed at the options inception and is based 
    on the price of the underlying asset on the expiration 
    date. Binary options may also incorporate barriers, 
    as is the case with binary-barrier options.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{GapOption} \tab Gap Option, \cr
    \code{CashOrNothingOption} \tab Cash Or Nothing Option, \cr
    \code{TwoAssetCashOrNothingOption} \tab Two Asset Cash Or Nothing Option, \cr
    \code{AssetOrNothingOption} \tab Asset Or Nothing Option, \cr
    \code{SuperShareOption} \tab Super Share Option, \cr
    \code{BinaryBarrierOption} \tab Binary Barrier Option. }

}


\usage{
GapOption(TypeFlag, S, X1, X2, Time, r, b, sigma, title = NULL, 
    description = NULL)
CashOrNothingOption(TypeFlag, S, X, K, Time, r, b, sigma,
    title = NULL, description = NULL) 
TwoAssetCashOrNothingOption(TypeFlag, S1, S2, X1, X2, K, Time, r, 
    b1, b2, sigma1, sigma2, rho, title = NULL, description = NULL)
AssetOrNothingOption(TypeFlag, S, X, Time, r, b, sigma,
    title = NULL, description = NULL)
SuperShareOption(S, XL, XH, Time, r, b, sigma, title = NULL, 
    description = NULL)
BinaryBarrierOption(TypeFlag, S, X, H, K, Time, r, b, sigma, 
    eta, phi, title = NULL, description = NULL)
}


\arguments{

    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{b1, b2}{
        [TwoAssetCashOrNothing*] - \cr
        the annualized cost-of-carry rate for the first and second
        asset, a numeric value.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{eta, phi}{
        [BinaryBarrier*] - \cr
        a set of parameters to price 28 different types of Binary 
        Barrier options:\cr
        01: \code{eta=+1, phi=NA, [S>H]} down-and-in cash-at-hit-or-nothing, \cr
        02: \code{eta=-1, phi=NA, [S<H]} up-and-in cash-at-hit-or-nothing, \cr
        03: \code{eta=+1, phi=NA, [S>H]} down-and-in asset-at-hit-or-nothing, \cr
        04: \code{eta=-1, phi=NA, [S<H]} up-and-in asset-at-hit-or-nothing, \cr
        05: \code{eta=+1, phi=-1, [S>H]} down-and-in cash-at-expiry-or-nothing, \cr
        06: \code{eta=-1, phi=+1, [S<H]} up-and-in cash-at-expiry-or-nothing, \cr
        07: \code{eta=+1, phi=-1, [S>H]} down-and-in asset-at-expiry-or-nothing, \cr
        08: \code{eta=-1, phi=+1, [S<H]} up-and-in asset-at-expiry-or-nothing, \cr
        09: \code{eta=+1, phi=+1, [S>H]} down-and-out cash-or-nothing, \cr
        10: \code{eta=-1, phi=-1, [S<H]} up-and-out cash-or-nothing, \cr
        11: \code{eta=+1, phi=+1, [S>H]} down-and-out asset-or-nothing, \cr
        12: \code{eta=-1, phi=-1, [S<H]} up-and-out asset-or-nothing, \cr
        13: \code{eta=+1, phi=+1, [S>H]} down-and-in cash-or-nothing call, \cr
        14: \code{eta=-1, phi=+1, [S<H]} up-and-in cash-or-nothing call, \cr
        15: \code{eta=+1, phi=+1, [S>H]} down-and-in asset-or-nothing call, \cr
        16: \code{eta=-1, phi=+1, [S<H]} up-and-in asset-or-nothing call, \cr
        17: \code{eta=+1, phi=-1, [S>H]} down-and-in cash-or-nothing put, \cr
        18: \code{eta=-1, phi=-1, [S<H]} up-and-in cash-or-nothing put, \cr
        19: \code{eta=+1, phi=-1, [S>H]} down-and-in asset-or-nothing put, \cr
        20: \code{eta=-1, phi=-1, [S<H]} up-and-in asset-or-nothing put, \cr
        21: \code{eta=+1, phi=+1, [S>H]} down-and-out cash-or-nothing call, \cr
        22: \code{eta=-1, phi=+1, [S<H]} up-and-out cash-or-nothing call, \cr
        23: \code{eta=+1, phi=+1, [S>H]} down-and-out asset-or-nothing call, \cr
        24: \code{eta=-1, phi=-1, [S<H]} up-and-out asset-or-nothing call, \cr
        25: \code{eta=+1, phi=-1, [S>H]} down-and-out cash-or-nothing put, \cr
        26: \code{eta=-1, phi=-1, [S<H]} up-and-out cash-or-nothing put, \cr
        27: \code{eta=+1, phi=-1, [S>H]} down-and-out asset-or-nothing put, \cr
        28: \code{eta=-1, phi=-1, [S<H]} up-and-out asset-or-nothing put.
        }
    \item{H}{
        [BinaryBarrier*] - \cr
        the barrier value, a numeric value.
        }    
    \item{K}{
        [CashOrNothing*] - \cr
        the cash amount at expiry if the option is in the money, 
        a numerical value.\cr
        [TwoAssetCashOrNothing*] - \cr
        for the cash-or-nothing call the cash amount at expiry if
        asset \code{S1} is above the strike \code{X1} and asset 
        \code{S2} is above strike \code{X2} at expiration, 
        \cr
        for the cash-or-nothing put the cash amount at expiry if
        asset \code{S1} is below the strike \code{X1} and asset 
        \code{S2} is below strike \code{X2} at expiration, 
        \cr
        for the cash-or-nothing up-down the cash amount at expiry if
        asset \code{S1} is above the strike \code{X1} and asset 
        \code{S2} is below strike \code{X2} at expiration, 
        \cr
        for the cash-or-nothing down-up the cash amount at expiry if
        asset \code{S1} is below the strike \code{X1} and asset 
        \code{S2} is above strike \code{X2} at expiration.\cr
        [BinaryBarrier*] - \cr
        the prespecified cash amount, a numeric value.
        }
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% pa.
        }
    \item{rho}{
        [TwoAssetCashOrNothing*] - \cr
        the correlation of the volatility between the first and
        second asset, a numeric value.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{S1, S2}{
        [TwoAssetCashOrNothing*] - \cr
        the price of the first and second asset, a numeric value.
        }       
    \item{sigma}{
        the annualized volatility of the underlying security, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }
    \item{sigma1, sigma2}{
        [TwoAssetCashOrNothing*] - \cr
        the annualized volatility of the first and second underlying 
        security, numeric values.
        }
    \item{Time}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        a character string either \code{"c"} for a call option or 
        a \code{"p"} for a put option. \cr
        [TwoAssetCashOrNothing*] - \cr
        a character string either \code{"c"} for a call option, or 
        a \code{"p"} for a put option, or a \code{"ud"} for an 
        up-down option, or a \code{"du"} for a down-up option.\cr
        [BinaryBarrier*] - \cr
        an integer between 1 and 28, selecting one of the 28 types,
        for a definition lookup the arguments \code{eta} and 
        \code{phi}.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    \item{X1, X2}{
        [GapOption][TwoAssetCashOrNothing*]  -
        the first and the second exercise price, a numeric value.
        }
    \item{XL, XH}{
        [SuperShare*]  -
        the lower and upper boundary strike, a numeric value.
        }           

}


\details{

    \bold{Gap Options:}
    \cr\cr
    The payoff on a gap option depends on the usual factors of a plain option, 
    but is also affected by a "gap"  amount of exercise prices, which may be 
    positive or negative. Note, that a gap call (put) option is equivalent to 
    being long (short) an asset-or-nothing call (put) and short (long) a 
    cash-or-nothing call (put). The option price is calculated analytically 
    according to Reiner and Rubinstein (1991).
    \cr
    [Haug's Book, Chapter 2.11.1]
    \cr
    

    \bold{Cash-or-Nothing Options:}
    \cr\cr
    For this option a predetermined amount is paid at expiration if the 
    asset is above for a call or below for a put some strike level. The 
    amount independent of the path taken. These options require no payment 
    of an exercise price. The exercise price determines whether or not the 
    option returns a payoff. The value of a cash-or-nothing call (put) 
    option is the present value of the fixed cash payoff multiplied by 
    the probability that the terminal price will be greater than (less than) 
    the exercise price. The option price is calculated analytically 
    according to Reiner and Rubinstein (1991).
    \cr
    [Haug's Book, Chapter 2.11.2]
    \cr
    
 
    \bold{Two-Asset-Cash-Or-Nothing Options:}
    \cr\cr
    These options are building blocks for constructing more complex exotic 
    options. There are four types of two-asset cash-or-nothing options, the 
    first two situationsa are: A two-asset-cash-or-nothing call pays out a 
    fixed cash amount if the price of the first asset is above (below) the 
    strike price of the first asset and the price of the second asset is also 
    above (below) the strike price of the second asset at expiration. The 
    other two situations arise under the following conditions: A two-asset 
    cash-or-nothing down-up pays out a fixed cash amount if the price of the 
    first asset is is below (above) the strike price of the first asset and 
    the price of the second asset is above (below) the strike price of the 
    second asset at expiration. The option price is calculated analytically 
    according to Heynen and Kat (1996).
    \cr
    [Haug's Book, Chapter 2.11.3]
    \cr
    

    \bold{Asset-Or-Nothing Options:}
    \cr\cr
    In this option a predetermined asset value is paid if the asset is, at 
    expiration, above for a call or below for a put some strike level, 
    independent of the path taken. For a call (put) the terminal price is 
    greater than (less than) the exercise price, the call (put) expires 
    worthless. The exercise price is never paid. Instead, the value of the 
    asset relative to the exercise price determines whether or not the 
    option returns a payoff. The value of an asset-or-nothing call (put) 
    option is the present value of the asset multiplied by the probability 
    that the terminal price will be greater than (less than) the exercise 
    price. The option price is calculated analytically according to Cox 
    and Rubinstein (1985).
    \cr
    [Haug's Book, Chapter 2.11.4]
    \cr

    
    \bold{Supershare Options:}
    \cr\cr
    These options represents a contingent claim on a fraction of the 
    underlying portfolio. The contingency is that the value of the portfolio 
    must lie between a lower and an upper bound at expiration. If the value 
    lies within these boundaries, the supershare is worth a proportion of the 
    assets underlying the portfolio, else the supershare expires worthless. 
    A supershare has a payoff that is basically like a spread of two 
    asset-or-nothing calls, in which the owner of a supershare purchases an 
    asset-or-nothing call with an strike price of the lower strike and sells 
    an asset-or-nothing call with an strike price of the upper strike. The 
    option price is calculated analytically according to Hakansson (1976).
    \cr
    [Haug's Book, Chapter 2.11.5]
    \cr
  
    
    \bold{Binary Barrier Options:}
    \cr\cr
    These options combine characteristics of both binary and barrier 
    options. They are path dependent with a discontinuous payoff. Similar 
    to barrier options, the payoff depends on whether or not the asset 
    price crosses a predetermined barrier. There are 28 different types of 
    binary barrier options, which can be divided into two main categories: 
    Cash-or-nothing and Asset-or-nothing barrier options. Cash-or-nothing 
    barrier options pay out a predetermined cash amount or nothing, depending 
    on whether the asset price has hit the barrier. Asset-or-nothing barrier 
    options pay out the value of the asset or nothing, depending on whether 
    the asset price has crossed the barrier. The barrier monitoring frequency 
    can be adjusted to account for discrete monitoring using an approximation 
    developed by Broadie, Glasserman, and Kou (1995). Binary-barrier options 
    can be priced analytically using a model introduced by Reiner and 
    Rubinstein (1991).
    \cr
    [Haug's Book, Chapter 2.11.6]

}


\note{

    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 2.11 of Haug's Book (1997).
    
}


\value{

    The option price, a numeric value.
    
}


\references{

Cox J.C., Rubinstein M. (1985);
    \emph{Innovations in Option Markets},
    Prentice-Hall, New Jersey.

Hakkansson N.H. (1976);
    \emph{The Purchasing Power Fund: A New Kind of Financial
        Intermediary},
    Financial Analysts Journal 32, 49--59.

Haug E.G. (1997); 
    \emph{The complete Guide to Option Pricing Formulas}, 
    Chapter 2.11, McGraw-Hill, New York.
    
Heinen R.C., Kat H.M. (1996);
    \emph{Brick by Brick},
    Risk Magazine 9, 6.

Reiner E., Rubinstein M. (1991);
    \emph{Unscrambling the Binary Code};
    Risk Magazine 4, 9.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## Examples from Chapter 2.11 in E.G. Haug's Option Guide (1997)

## Gap Option [2.11.1]:
   GapOption(TypeFlag = "c", S = 50, X1 = 50, X2 = 57, Time = 0.5, 
     r = 0.09, b = 0.09, sigma = 0.20)

## Cash Or Nothing Option [2.11.2]:
   CashOrNothingOption(TypeFlag = "p", S = 100, X = 80, K = 10, 
     Time = 9/12, r = 0.06, b = 0, sigma = 0.35) 

## Two Asset Cash Or Nothing Option [2.11.3]:
   # Type 1 - call:
   TwoAssetCashOrNothingOption(TypeFlag = "c", S1 = 100, S2 = 100, 
     X1 = 110, X2 = 90, K = 10, Time = 0.5, r = 0.10, b1 = 0.05, 
     b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = 0.5)
   # Type 2 - put:
   TwoAssetCashOrNothingOption(TypeFlag = "p", S1 = 100, S2 = 100, 
     X1 = 110, X2 = 90, K = 10, Time = 0.5, r = 0.10, b1 = 0.05, 
     b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = -0.5)
   # Type 3 - down-up:
   TwoAssetCashOrNothingOption(TypeFlag = "ud", S1 = 100, S2 = 100, 
     X1 = 110, X2 = 90, K = 10, Time = 1, r = 0.10, b1 = 0.05, 
     b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = 0)
   # Type 4 - up-down:
   TwoAssetCashOrNothingOption(TypeFlag = "du", S1 = 100, S2 = 100, 
     X1 = 110, X2 = 90, K = 10, Time = 1, r = 0.10, b1 = 0.05, 
     b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = 0)

## Asset Or Nothing Option [2.11.4]: 
   AssetOrNothingOption(TypeFlag = "p", S = 70, X = 65, Time = 0.5, 
     r = 0.07, b = 0.07 - 0.05, sigma = 0.27)

## Super Share Option [2.11.5]:  
   SuperShareOption(S = 100, XL = 90, XH = 110, Time = 0.25, r = 0.10, 
     b = 0, sigma = 0.20)

## Binary Barrier Option [2.11.6]: 
   BinaryBarrierOption(TypeFlag = "6", S = 95, X=102, H = 100, 
     K = 15, Time = 0.5, r = 0.1, b = 0.1, sigma = 0.20)
   BinaryBarrierOption(TypeFlag = "12", S = 95, X = 98, H = 100, 
     K = 15, Time = 0.5, r = 0.1, b = 0.1, sigma = 0.20)
     
}


\keyword{math}

