\name{MultipleExercisesOptions}

\alias{MultipleExercisesOptions}

\alias{ExecutiveStockOption}
\alias{ForwardStartOption}
\alias{RatchetOption}
\alias{TimeSwitchOption}
\alias{SimpleChooserOption}
\alias{ComplexChooserOption}
\alias{OptionOnOption}
\alias{WriterExtendibleOption}
\alias{HolderExtendibleOption}


\title{Valuation of Mutiple Exercises Options}


\description{
  
    A collection and description of functions to valuate 
    multiple exercise options. Multiple exercises options, 
    as the name implies, are options whose payoff is based 
    on multiple exercise dates.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{ExecutiveStockOption} \tab Executive Stock Option, \cr
    \code{ForwardStartOption} \tab Forward Start Option, \cr
    \code{RatchetOption} \tab Ratchet Option, \cr
    \code{TimeSwitchOption} \tab Time Switch Option, \cr
    \code{SimpleChooserOption} \tab Simple Chooser Option, \cr
    \code{ComplexChooserOption} \tab Complex Chooser Option, \cr
    \code{OptionOnOption} \tab Option On Option, \cr
    \code{WriterExtendibleOption} \tab Writer Extendible Option, \cr
    \code{HolderExtendibleOption} \tab Holder Extendible Option. }
    
}


\usage{
ExecutiveStockOption(TypeFlag, S, X, Time, r, b, sigma, lambda,
    title = NULL, description = NULL)
ForwardStartOption(TypeFlag, S, alpha, time1, Time2, r, b, sigma,
    title = NULL, description = NULL) 
RatchetOption(TypeFlag, S, alpha, time1, Time2, r, b, sigma,
    title = NULL, description = NULL)
TimeSwitchOption(TypeFlag, S, X, Time, r, b, sigma, A, m, dt,
    title = NULL, description = NULL)
SimpleChooserOption(S, X, time1, Time2, r, b, sigma,
    title = NULL, description = NULL) 
ComplexChooserOption(S, Xc, Xp, Time, Timec, Timep, r, b, sigma, 
    doprint = FALSE, title = NULL, description = NULL)
OptionOnOption(TypeFlag, S, X1, X2, time1, Time2, r, b, sigma, 
    doprint = FALSE, title = NULL, description = NULL)
WriterExtendibleOption(TypeFlag, S, X1, X2, time1, Time2, r, b, sigma,
    title = NULL, description = NULL)
HolderExtendibleOption(TypeFlag, S, X1, X2, time1, Time2, r, b, sigma, A,
    title = NULL, description = NULL)
}


\arguments{    
    
    \item{A}{
        [HolderExtendible*] - \cr
        defined by the amount \code{A*dt} the investor receives
        at maturity time \code{Time} for each time interval
        \code{deltat} the corresponding asset price has exceeded 
        the exercise price \code{X}, in the case of a call
        option, or the corresponding asset price has been below 
        the exercise price \code{X}, in the case of a put
        option. A numeric value.
        }
    \item{alpha}{
        [Ratchet*] - \cr
        the exercise price is \code{alpha} times the asset price
        \code{S} after the known time \code{time}. \code{alpha}
        is a numeric value. If \code{alpha} is less than unity,
        the call (put) will start \code{100*(1-alpha)} percent in 
        the money (out-of-the-money); if  \code{alpha} is unity, 
        the option will start at the money; and if \code{alpha} 
        is larger than unity, the call (put) will start 
        \code{100*(alpha-1)} percentage out of the money 
        (in-the-money).
        }
    \item{b}{
        the annualized cost-of-carry rate, a numeric value; 
        e.g. 0.1 means 10\% pa.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doprint}{
        a logical. Should the critical value \code{I} be printed?
        By defaut \code{FALSE}.
        }   
    \item{dt}{
        the time interval; a numeric value.
        }
    \item{lambda}{
        the jump rate pa.
        }
    \item{m}{
        defined by the number of time units where the option
        has already fulfilled the thresold condition. This
        applies to cases, for which some of the option's total
        lifetime has already passed. An integer value.
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
        the time to maturity measured in years, a numeric value; 
        e.g. 0.5 means 6 months.
        }
    \item{Timec, Timep}{
        [ComplexChooser*] - \cr
        decision time measured in years, e.g. 0.5 means 6 months.  
        \code{Timec}, is the time to maturity of the call option, 
        \code{Timep}, is the time to maturity of the put option, 
        both also measured in years. Numeric values.
        }
    \item{time1, Time2}{
        the time to maturity, \code{Time1}, measured in years, 
        e.g. 0.5 means 6 months, and the elapsed time in the 
        future, \code{Time2}. In detail, the forward start option 
        with time to maturity \code{Time1} starts at-the-money or 
        proportinally in-the-money or out-of-the-money after this
        elapsed time \code{Time2} in the future.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{TypeFlag}{
        usually a character string either \code{"c"} for a call option 
        or a \code{"p"} for a put option;\cr
        [OptionOnOption] - \cr
        a character string either 
        \code{"cc"} for a call-on-call option, or
        \code{"cp"} for a call-on-put option, or
        \code{"pc"} for a put-on-call option, or 
        \code{"pp"} for a put-on-put option.
        }
    \item{X}{
        the exercise price, a numeric value.
        }
    \item{Xc, Xp}{
        [ComplexChooser*] - \cr
        the exercise price of the call option, \code{Xc}, and the 
        exercise price of the put option, \code{Xp}, numeric 
        values.
        }
    \item{X1, X2}{
        the exercise price of the underlying option, \code{X1}, and
        the exercise price of the option on the option, \code{X2},
        numeric values.
        }
    
}


\details{

    \bold{Executive Stock Options:}
    \cr\cr
    Executive stock options are usually at-the-money options that are 
    issued to motivate employees to act in the best interest of the 
    company. They cannot be sold and often last as long as 10 or 15 
    years. The executive model takes into account that an employee often 
    looses their options when they leave the company before expiration. 
    The value of an executive option equals the standard Black-Scholes 
    model multiplied by the probability that the employee will stay with 
    the firm until the option expires. Executive stock options can be priced
    analytically using a model published by Jennergren and Naslund (1993). 
    \cr
    [Haug's Book, Chapter 2.1]
    \cr
    

    \bold{Forward Start Options:}
    \cr\cr
    A forward start option is an option which is paid for today, but 
    will start at some determined time in the future known as the issue 
    date. The option usually starts at-the-money or proportionally in 
    or out-of-the-money at a future date. The strike is set to a positive 
    constant a times the asset price S at a future date. If a is less 
    than one, the call (put) will start 1 - a percent in-the-money 
    (out-of-the-money); if a is one, the option will start at-the-money; 
    and if a is larger than one, the call (put) will start a - 1 percent 
    out-of-the-money (in-the-money).[1] Forward start options can be 
    priced analytically using a model published by Rubinstein (1990).
    \cr
    [Haug's Book, Chapter 2.2]
    \cr

    
    \bold{Ratchet [Compound] Options:}
    \cr\cr
    A compound option is an option on an option. Therefore, when one 
    option is exercised, the underlying security is another option. 
    There are four types of possible compound options: a call on a call, 
    a call on a put, a put on a call, and a put on a put. The owner of 
    a compound option has until the expiration date of the compound 
    option to determine whether to exercise the compound option. If 
    exercised, the owner will receive the underlying option with its 
    own exercise price and time until expiration. If the underlying 
    option is exercised, the owner will receive the underlying security. 
    European compound options can be priced analytically using a model 
    published by Rubinstein (1991). A binomial lattice is used for the 
    numerical calculation of an American or European style exchange option.
    A ratchet option is also called sometimes a "moving strike option" 
    or "cliquet option".
    \cr
    [Haug's Book, Chapter 2.3]
    \cr
    

    \bold{Time-Switch Options:}
    \cr\cr
    For a discrete time-switch call (put) option, the holder receives an 
    amount ADt at expiration for each time interval, Dt, the corresponding 
    asset price has been above (below) the strike price. If some of the 
    option's total lifetime has passed, it is required to add a fixed 
    amount to the pricing formula. Discrete time-switch options can be 
    priced analytically using a model published by Pechtl (1995).
    \cr
    [Haug's Book, Chapter 2.4]
    \cr
    

    \bold{Simple Chooser Options:}
    \cr\cr
    A chooser option allows the holder to determine at some date, after the 
    trade date, whether the option becomes a plain vanilla call or put. 
    Chooser options are also called "as you like it" options. Chooser 
    options are useful for hedging a future event that might not occur. 
    Due to their increased flexibility, chooser options are more expensive 
    than plain vanilla options. It is assumed at the options expiration 
    date that a holder of the chooser option will choose the more valuable 
    of the put or call option. The less valuable option that was not chosen 
    will become worthless. Chooser options can be priced analytically using 
    a model introduced by Rubinstein (1991).
    \cr
    [Haug's Book, Chapter 2.5.1]
    \cr

    
    \bold{Complex Chooser Options:}
    \cr\cr
    A complex chooser option allows the holder to determine at some date, 
    after the trade date, whether the option is to be a standard call 
    chooser model, a complex chooser option will be more expensive than 
    a plain vanilla option. Complex chooser options can be priced analytically 
    using a model introduced by Rubinstein (1991).
    \cr
    [Haug's Book, Chapter 2.5.2]
    \cr
  
    
    \bold{Option On Options:}
    \cr\cr
    This derivative prices options on options. An option on an option is more 
    expensive to purchase than the underlying option itself, as the purchaser 
    has received a price guarantee and effectively extended the life of the 
    option. These options provide the benefit of a guaranteed price for the 
    option at a date in the future. Options on Options can be prices as
    published by Geske (1977). His model was later extended and discussed 
    by Geske (1979), Hodges and Selby (1987), and Rubinstein (1991). 
    \cr
    [Haug's Book, Chapter 2.6]
    \cr

    
    \bold{Writer [Holder] Extendible Options:}
    \cr\cr
    Writer extendible options can be found embedded in various financial 
    contracts. For example, corporate warrants often give the issuing 
    firm the right to extend the life of the warrants. These options can 
    be exercised at their initial maturity, but are extended to a new 
    maturity if they are out-of-the-money at initial maturity. Discrete 
    time-switch options can be priced analytically using a model published 
    by Longstaff (1995).
    \cr
    [Haug's Book, Chapter 2.6]

}


\note{
    
    Options on options are also known as compound options or as 
    mother-and-daughter options. 
    
}


\value{

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


\references{
Geske R. (1977);
    \emph{The Valuation of Corporate Liabilities as Compound Options},
    Journal of Financial and Quantitative Analysis, 541--552.

Geske R. (1979);
    \emph{The Valuation of Compound Options},
    Journal of Financial Economics 7, 63--81.
    
Haug E.G. (1997);
    \emph{The complete Guide to Option Pricing Formulas}, 
    Chapter 2.8.1, McGraw-Hill, New York.
    
Hodges S.D., Selby J.P. (1987);
    \emph{On the Evaluation of Compound Options};
    Management Science 33, 347--355.

Jennergren L.P., Naslund B. (1993); 
    \emph{A Comment on Valuation of Executive Stock Options and the 
    FASB Proposal}, 
    The Accounting Review 68, 179, 1993.

Longstaff F.A. (1990);
    \emph{Pricing Options with Extendible Maturities: Analysis
    and Applications},
    Journal of Finance 45, 474--491.
    
Pechtl A. (1990); 
    \emph{Classified Information},
    Risk Magazine 8.
    
Rubinstein, M. (1990); 
    \emph{Pay Now, Choose Later}, 
    Risk Magazine 3.
    
Rubinstein M. (1991); 
    \emph{Options for the Undecide}, 
    Risk Magazine 4.
    
Rubinstein M. (1991);
    \emph{Double Trouble};
    Risk Magazine 5.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fOptions.2A-MultipleExercisesOptions")

## Examples from Chapter 2.1 - 2.7 in E.G. Haug's Option Guide (1997)

## ExecutiveStockOption [2.1]:
   ExecutiveStockOption(TypeFlag = "c", S = 60, X = 64, Time = 2, 
    r = 0.07, b = 0.07-0.03, sigma = 0.38, lambda = 0.15) 
    
## ForwardStartOption [2.2]:
   ForwardStartOption(TypeFlag = "c", S = 60, alpha = 1.1, 
     time1 = 1, Time2 = 1/4, r = 0.08, b = 0.08-0.04, sigma = 0.30)
     
## Ratchet Option [2.3]:
   RatchetOption(TypeFlag = "c", S = 60, alpha = 1.1, time1 = c(1.00, 0.75), 
     Time2 = c(0.75, 0.50), r = 0.08, b = 0.04, sigma = 0.30)
     
## Time Switch Option [2.4]:
   TimeSwitchOption(TypeFlag = "c", S = 100, X = 110, Time = 1, 
    r = 0.06, b = 0.06, sigma = 0.26, A = 5, m = 0, dt = 1/365)
    
## Simple Chooser Option [2.5.1]:
   SimpleChooserOption(S = 50, X = 50, time1 = 1/4, Time2 = 1/2, 
     r = 0.08, b = 0.08, sigma = 0.25)  
        
## Complex Chooser Option [2.5.2]:
   ComplexChooserOption(S = 50, Xc = 55, Xp = 48, Time = 0.25, 
     Timec = 0.50, Timep = 0.5833, r = 0.10, b = 0.1-0.05, 
     sigma = 0.35, doprint = TRUE)
     
## Option On Option [2.6]:
   OptionOnOption(TypeFlag = "pc", S = 500, X1 = 520, X2 = 50, 
     time1 = 1/2, Time2 = 1/4, r = 0.08, b = 0.08-0.03, sigma = 0.35)
    
## Holder Extendible Option [2.7.1]:
   HolderExtendibleOption(TypeFlag = "c", S = 100, X1 = 100, 
     X2 = 105, time1 = 0.50, Time2 = 0.75, r = 0.08, b = 0.08, 
     sigma = 0.25, A = 1)
     
## Writer Extendible Option [2.7.2]:
   WriterExtendibleOption(TypeFlag = "c", S = 80, X1 = 90, X2 = 82,
     time1 = 0.50, Time2 = 0.75, r = 0.10, b = 0.10, sigma = 0.30)
 
}


\keyword{math}

