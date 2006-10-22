\name{BarrierOptions}

\alias{BarrierOptions}

\alias{StandardBarrierOption}
\alias{DoubleBarrierOption}
\alias{PTSingleAssetBarrierOption}
\alias{TwoAssetBarrierOption}
\alias{PTTwoAssetBarrierOption}
\alias{LookBarrierOption}
\alias{DiscreteBarrierOption}
\alias{SoftBarrierOption}


\title{Valuation of Barrier Options}


\description{
  
    A collection and description of functions to valuate  
    barrier options. Barrier options are path-dependent options, 
    with payoffs that depend on the price of the underlying 
    asset at expiration and whether or not the asset price 
    crosses a barrier during the life of the option. There 
    are two categories or types of Barrier options: "knock-in" 
    and "knock-out". "Knock-in" or "in" options are paid 
    for up front, but you do not receive the option until 
    the asset price crosses the barrier. "Knock-out" or "out" 
    options come into existence on the issue date but becomes 
    worthless if the asset price hits the barrier before the 
    expiration date. If the option is a knock-in (knock-out), 
    a predetermined cash rebate may be paid at expiration if 
    the option has not been knocked in (knocked-out) during 
    its lifetime. The barrier monitoring frequency specifies 
    how often the price is checked for a breach of the barrier. 
    All of the analytical models have a flag to change the 
    monitoring frequency where the default frequency is 
    continuous.  
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{StandardBarrierOption} \tab Standard Barrier Option, \cr
    \code{DoubleBarrierOption} \tab Double Barrier Option, \cr
    \code{PTSingleAssetBarrierOption} \tab Partial Time Single Asset Barrier Option, \cr
    \code{TwoAssetBarrierOption} \tab Two Asset Barrier Option, \cr
    \code{PTTwoAssetBarrierOption} \tab Partial Time Two Asset Barrier Option, \cr
    \code{LookBarrierOption} \tab Lookback Barrier Option, \cr
    \code{DiscreteBarrierOption} \tab Discrete Barrier Option \cr
    \code{SoftBarrierOption} \tab Soft Barrier Option. }
    
}


\usage{
StandardBarrierOption(TypeFlag, S, X, H, K, Time, r, b, sigma,
    title = NULL, description = NULL)
DoubleBarrierOption(TypeFlag, S, X, L, U, Time, r, b, sigma, 
    delta1, delta2, title = NULL, description = NULL)
PTSingleAssetBarrierOption(TypeFlag, S, X, H, time1, Time2, r, 
    b, sigma, title = NULL, description = NULL)
TwoAssetBarrierOption(TypeFlag, S1, S2, X, H, Time, r, b1, b2,
    sigma1, sigma2, rho, title = NULL, description = NULL)
PTTwoAssetBarrierOption(TypeFlag, S1, S2, X, H, time1, Time2, r, 
    b1, b2, sigma1, sigma2, rho, title = NULL, description = NULL)
LookBarrierOption(TypeFlag, S, X, H, time1, Time2, r, b, sigma,
    title = NULL, description = NULL)
DiscreteBarrierOption(S, H, sigma, dt, title = NULL, 
    description = NULL)
SoftBarrierOption(TypeFlag, S, X, L, U, Time, r, b, sigma, 
    title = NULL, description = NULL)
}


\arguments{

    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{b1, b2}{
        [PTTwoAssetBarrier*] - \cr
        the annualized cost-of-carry rate for the first and second
        asset, a numeric value.
        }
    \item{delta1, delta2}{
        [DoubleBarrier*] - \cr
        numeric values which determine the curvature of the lower 
        \code{L} and upper \code{U} bounds. The case of \cr
        \code{delta1=delta2=0} corresponds to two flat boundaries, \cr
        \code{delta1<0<delta2} correponds to a lower boundary exponentially
        growing as time elapses, while the upper boundary will be 
        exponentially decaying, \cr
        \code{delta1>0>delta2} correponds to a convex downward lower 
        boundary and a convex upward upper boundary.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{dt}{
        [DiscreteBarrier*] - \cr
        time between monitoring instants, a numeric value.
        }
    \item{H}{
        [StandardBarrier*] - \cr
        the barrier value, a numeric value.
        }    
    \item{K}{
        [StandardBarrier*] - \cr
        for an "In"-Barrier a prespecified cash rebate which is paid 
        out at option expiration if the option has not been knocked 
        in during its lifetime,\cr
        for an "Out"-Barrier a prespecified cash rebate which is paid 
        out at option expiration if the option has not been knocked 
        out before its lifetime,
        a numerical value.
        }
    \item{L, U}{
        [DoubleBarrier*] - \cr
        the lower and upper boundary to be touched, numerical values.
        }
    \item{r}{
        the annualized rate of interest, a numeric value; 
        e.g. 0.25 means 25\% pa.
        }
    \item{rho}{
        [TwoAssetBarrier*] - \cr
        the correlation of the volatility between the first and
        second asset, a numeric value.
        }
    \item{S}{
        the asset price, a numeric value.
        }
    \item{S1, S2}{
        [PTTwoAssetBarrier*] - \cr
        the price of the first and second asset, a numeric value.
        }   
    \item{sigma}{
        the annualized volatility of the underlying security, 
        a numeric value; e.g. 0.3 means 30\% volatility pa.
        }
    \item{sigma1, sigma2}{
        [PTTwoAssetBarrier*] - \cr
        the annualized volatility of the first and second underlying 
        security, numeric values.
        }
    \item{Time}{
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{time1, Time2}{
        [PTSingleAssetBarrier*][PTTwoAssetBarrier*] - \cr
        so called type "A" options (see the \code{TypeFlag} argument)
        will have the location of the monitoring period starting at 
        the options starting date and ending at an arbitrary time 
        \code{time1} before expiration time \code{Time2}. 
        Partial-time-end-barrier options will have the location of 
        the monitoring period starting at an arbitrary time \code{time1} 
        before expiration time \code{Time2}, and ending at expiration 
        time.\cr
        [LookBarrier*] - \cr
        the lookbarrier option's barrier monitoring period starts at 
        the options starting date and ends at an arbitrary time 
        \code{time1} before expiration time \code{Time2}.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        usually a character string either \code{"c"} for a call option 
        or a \code{"p"} for a put option.\cr\cr\code{ }
        [StandardBarrier*] - \cr
        here\cr 
        \code{"cdi"} denotes a down-and-in call,\cr 
        \code{"cui"} denotes an up-and-in call,\cr
        \code{"cdo"} denotes a down-and-out call, and\cr
        \code{"cuo"} denotes an up-and-out call.\cr 
        Similarily, the type flags for the corresponding puts are
        \code{"pdi"}, \code{"pui"}, \code{"pdo"}, and \code{"puo"}.\cr\cr
        [DoubleBarrier*] - \cr
        here\cr 
        \code{"co"} denotes an up-and-out-down-and-out call,\cr
        \code{"ci"} denotes an up-and-in-down-and-in call,\cr
        \code{"po"} denotes an up-and-out-down-and-out put, and\cr
        \code{"pi"} denotes an up-and-in-down-and-in call.\cr\cr
        [PTSingleAssetBarrier*] - \cr
        here\cr
        \code{"cdoA"} denotes a down-and-out call of type "A",\cr
        \code{"cuoA"} denotes an up-and-out call of type "A",\cr
        \code{"pdoA"} denotes a down-and-out put of type "A",\cr
        \code{"puoA"} denotes an up-and-out put of type "A",\cr
        \code{"coB1"} denotes an out-call of type "B1",\cr
        \code{"poB1"} denotes an out-call of type "B1",\cr
        \code{"cdoB2"} denotes a down-and-out call of type "B2",\cr
        \code{"cuoB2"} denotes an up-and-out call of type "B2".\cr  
        Note, a partial-time-start-barrier option is called a type "A"
        option, a partial-time-end-out-call is a called a type "B"
        option. There are two types of "B" options: "B1" is defined
        such that only a barrier hit or crossed causes the option to be
        knocked out, and a "B2" is defined such that a down-and-out-call
        is knocked out as soon as the underlying price is below the 
        barrier.\cr\cr
        [TwoAssetBarrier*][PTTwoAssetBarrier*] - \cr
        here\cr
        \code{"cuo"} denotes an up-and-out call,\cr
        \code{"cui"} denotes an up-and-in call,\cr
        \code{"cdo"} denotes a down-and-out call,\cr
        \code{"cdi"} denotes a down-and-in call,\cr
        \code{"puo"} denotes an up-and-out put,\cr
        \code{"pui"} denotes an up-and-in put,\cr
        \code{"pdo"} denotes a down-and-out put,\cr
        \code{"pdi"} denotes a down-and-in put.\cr\cr
        [LookBarrier*][SoftBarrier*] - \cr
        here\cr
        \code{"cuo"} denotes an up-and-out call,\cr
        \code{"cui"} denotes an up-and-in call,\cr
        \code{"pdo"} denotes a down-and-out put,\cr
        \code{"pdi"} denotes a down-and-in put.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
        
}


\details{

    \bold{Single [Standard] Barrier Options:}
    \cr\cr
    There are four types of single barrier options. The type flag 
    \code{"cdi"} denotes a down-and-in call,
    \code{"cui"} denotes an up-and-in call,
    \code{"cdo"} denotes a down-and-out call, and
    \code{"cuo"} denotes an up-and-out call. 
    Similarily, the type flags for the corresponding puts are
    \code{cdi}, \code{cui}, \code{cdo}, and \code{cuo}.
    A down-and-in option comes into existence 
    and knocked-in only if the asset price falls to the barrier level. An 
    up-and- in option comes into existence and knocked-in only if the asset 
    price rises to the barrier level. A down-and-out option comes into 
    existence and knocked-out only if the asset price falls to the barrier 
    level. An up-and-in option comes into existence and knocked-out only if 
    the asset price rises to the barrier level. European single barrier 
    options can be priced analytically using a model introduced by Reiner 
    and Rubinstein (1991). A trinomial lattice is used for the numerical 
    calculation of an American or European style single barrier options.
    \cr
    [Haug's Book, Chapter 2.10.1]
    \cr
 
    
    \bold{Double Barrier Options:}
    \cr\cr
    A double barrier option is either knocked in or knocked out if the 
    asset price touches the lower or upper barrier during its lifetime. 
    The type flag
    \code{"co"} denotes an up-and-out-down-and-out call,
    \code{"ci"} denotes an up-and-in-down-and-in call,
    \code{"po"} denotes an up-and-out-down-and-out put, and
    \code{"pi"} denotes an up-and-in-down-and-in call.
    Once a barrier is crossed, the option comes into existence if it is 
    a knock-in barrier or becomes worthless if it is a knocked out barrier. 
    Double barrier options can be priced analytically using a model 
    introduced by Ikeda and Kunitomo (1992).
    \cr
    [Haug's Book, Chapter 2.10.2]
    \cr
 
    
    \bold{Partial-Time Barrier Options:}
    \cr\cr
    For single asset partial-time barrier options, the monitoring period 
    for a barrier crossing is confined to only a fraction of the option's 
    lifetime. There are two types of partial-time barrier options: 
    partial-time-start and partial-time-end. Partial-time-start barrier 
    options have the monitoring period start at time zero and end at an 
    arbitrary date before expiration. Partial-time-end barrier options 
    have the monitoring period start at an arbitrary date before expiration 
    and end at expiration. Partial-time-end barrier options are then broken 
    down again into two categories: B1 and B2. Type B1 is defined such that 
    only a barrier hit or crossed causes the option to be knocked out. 
    There is no difference between up and down options. Type B2 options 
    are defined such that a down-and-out call is knocked out as soon as 
    the underlying price is below the barrier. Similarly, an up-and-out 
    call is knocked out as soon as the underlying price is above the barrier.
    Partial-time barrier options can be priced analytically using a model 
    introduced by Heynen and Kat (1994).
    \cr
    [Haug's Book, Chapter 2.10.3]
    \cr
    
    
    \bold{Two-Asset Barrier Options:}
    \cr\cr
    The underlying asset, Asset 1, determines how much the option is in 
    or out-of-the-money. The other asset, Asset 2, is the trigger asset 
    that is linked to barrier hits. Two-asset barrier options can be 
    priced analytically using a model introduced by Heynen and Kat (1994).
    \cr
    [Haug's Book, Chapter 2.10.4]
    \cr
    
    
    \bold{Lookback Barrier Options:}
    \cr\cr
    A look-barrier option is the combination of a forward starting fixed 
    strike Lookback option and a partial time barrier option. The option's 
    barrier monitoring period starts at time zero and ends at an arbitrary 
    date before expiration. If the barrier is not triggered during this 
    period, the fixed strike Lookback option will be kick off at the end 
    of the barrier tenor. Lookback barrier options can be priced analytically 
    using a model introduced by Bermin (1996).
    \cr
    [Haug's Book, Chapter 2.10.6]
    \cr
 
    
    \bold{Partial-Time-Two-Asset Options:}
    \cr\cr
    Partial-time two-asset barrier options are similar to standard two-asset 
    barrier options, except that the barrier hits are monitored only for a 
    fraction of the option's lifetime. The option is knocked in or knocked 
    out is Asset 2 hits the barrier during the monitoring period. The payoff 
    depends on Asset 1 and the strike price. Partial-time two-asset barrier 
    options can be priced analytically using a model introduced by Bermin 
    (1996).
    \cr
    [Haug's Book, Chapter 2.10.5]
    \cr
    
    
    \bold{Soft Barrier Options:}
    \cr\cr
    A soft-barrier option is similar to a standard barrier option, except 
    that the barrier is no longer a single level. Rather, it is a soft 
    range between a lower level and an upper level. Soft-barrier options 
    are knocked in or knocked out proportionally. Introduced by Hart and 
    Ross (1994), the valuation formula can be used to price soft-down-and-in 
    call and soft-up-and-in put options. The value of the related "out" 
    option can be determined by subtracting the "in" option value from the 
    value of a standard plain option. Soft-barrier options can be priced 
    analytically using a model introduced by Hart and Ross (1994).
    \cr
    [Haug's Book, Chapter 2.10.8]

}


\note{

    The functions implement the algorithms to valuate plain vanilla 
    options as described in Chapter 2.10 of Haug's Book (1997).
    
}


\value{

    The option price, a numeric value.
    
}


\references{

Haug E.G. (1997); 
    \emph{The complete Guide to Option Pricing Formulas}, 
    Chapter 2.10, McGraw-Hill, New York.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
 
}


\examples{
## SOURCE("fOptions.2D-BarrierOptions")

## Examples from Chapter 2.10 in E.G. Haug's Option Guide (1997)

## Standard Barrier Option [2.10.1]:
   # down-and-out Barrier Call
   StandardBarrierOption(TypeFlag = "cdo", S = 100, X = 90, 
     H = 95, K = 3, Time = 0.5, r = 0.08, b = 0.04, sigma = 0.25)
   
## Double Barrier Option [2.10.2]:
   DoubleBarrierOption(TypeFlag = "co", S = 100, X = 100, L = 50, 
     U = 150, Time = 0.25, r = 0.10, b = 0.10, sigma = 0.15, 
     delta1 = -0.1, delta2 = 0.1)
   
## Partial Time Single-Asset Barrier Option [2.10.3]:
   PTSingleAssetBarrierOption(TypeFlag = "coB1", S = 95, X = 110, 
     H = 100, time1 = 0.5, Time2 = 1, r = 0.20, b = 0.20, 
     sigma = 0.25)
   
## Two Asset Barrier Option [2.10.4]:
   TwoAssetBarrierOption(TypeFlag = "puo", S1 = 100, S2 = 100, 
     X = 110, H = 105, Time = 0.5, r = 0.08, b1 = 0.08, b2 = 0.08, 
     sigma1 = 0.2, sigma2 = 0.2, rho = -0.5)
   
## PT Two Asset Barrier Option [2.10.5]:
   PTTwoAssetBarrierOption(TypeFlag = "pdo", S1 = 100, S2 = 100, 
     X = 100, H = 85, time1 = 0.5, Time2 = 1, r = 0.1, b1 = 0.1, 
     b2 = 0.1, sigma1 = 0.25, sigma2 = 0.30, rho = -0.5)
   
## Look Barrier Option [2.10.6]:
   LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 130, 
     time1 = 0.25, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.15)
   LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 110, 
     time1 = 1, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.30)
   
## Discrete Barrier Option [2.10.7]:  
   DiscreteBarrierOption(S = 100, H = 105, sigma = 0.25, dt = 0.1)
   
## Soft Barrier Option [2.10.8]:
   SoftBarrierOption(TypeFlag = "cdo", S = 100, X = 100, L = 70, 
     U = 95, Time = 0.5, r = 0.1, b = 0.05, sigma = 0.20)
  
}


\keyword{math}

