\name{fOptions-package}

\alias{fOptions-package}
\alias{fOptions}


\docType{package}


\title{Basic Option Valuation}


\description{

    The Rmetrics "Options" package is a collection of functions to 
    valuate basic options.
    
}


\details{

    \tabular{ll}{
        Package:    \tab fOptions\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
  }

}


\section{1 Introduction}{

    The \code{fOptions} package provides function for pricing
    and evaluationg basic options.

}


\section{2 Plain Vanilla Option}{

    This section provides a collection of functions to valuate 
    plain vanilla options. Included are functions for the 
    Generalized Black-Scholes option pricing model, for options 
    on futures, some utility functions, and print and summary 
    methods for options.

    \preformatted{
    GBS*                      the generalized Black-Scholes option
    BlackScholesOption        a synonyme for the GBSOption
    Black76Option             options on Futures
    MiltersenSchwartzOption   options on commodity futures
    }
    
    \preformatted{
    NDF, CND, CBND  distribution functions
    }
    
    \preformatted{
    print                     print method for Options
    summary                   summary method for Options
    }

}


\section{3 Binomial Tree Options}{

    This section offers a collection of functions to valuate options in 
    the framework of the Binomial tree option approach.
    
    \preformatted{
    CRRBinomialTreeOption     CRR Binomial Tree Option
    JRBinomialTreeOption      JR Binomial Tree Option
    TIANBinomialTreeOption    TIAN Binomial Tree Option
    BinomialTreeOption        Binomial Tree Option
    BinomialTreePlot          Binomial Tree Plot
    }
    
}


\section{4 Monte Carlo Options}{
    
    In this section we provide functions to valuate options by Monte 
    Carlo methods. The functions include beside the main Monte Carlo 
    Simulator, example functions to generate Monte Carlo price paths 
    and to compute Monte Carlo price payoffs.
    
    \preformatted{
    sobolInnovations          Example for scrambled Sobol innovations
    wienerPath                Example for a Wiener price path
    plainVanillaPayoff        Example for the plain vanilla option's payoff
    arithmeticAsianPayoff     Example for the arithmetic Asian option's payoff
    MonteCarloOption          Monte Carlo Simulator for options
    }

}

 
\section{5 Low Discrepancy Sequences}{

    This section provides three types of random number generators for
    univorm and normal distributed deviates. These area pseudo random
    number generator and a halton and sobol generator for low discrepancy
    sequences.
    
    \preformatted{
    runif.pseudo    Uniform pseudo random numbers
    rnorm.pseudo    Normal pseudo random numbers
    }
    
    \preformatted{
    runif.halton    Uniform Halton sequence
    rnorm.halton    Normal Halton sequence
    }
    
    \preformatted{
    runif.sobol     Uniform scrambled Sobol sequence
    rnorm.sobol     Normal scrambled Sobol sequence
    }r
    
}


\section{6 Heston Nandi Garch Fit}{

    Her we provide functions to model the GARCH(1,1) price paths which 
    underly Heston and Nandi's option pricing model. The functions are:

    \preformatted{  
    hngarchSim           simulates a Heston-Nandi Garch(1,1) process
    hngarchFit           fits parameters of a Heston Nandi Garch(1,1) model
    }
    
    \preformatted{
    hngarchStats         returns true moments of the log-Return distribution
    }
    
    \preformatted{
    print.hngarch        print method, \cr
    summary.hngarch      diagnostic summary
    }
}


\section{7 Heston Nandi Garch Options}{

    This section comes with functions to valuate Heston-Nandi options. 
    Provided are functions to compute the option price and the delta 
    and gamma sensitivities for call and put options. 
    
    \preformatted{
    HNGOption            Heston-Nandi GARCH(1,1) option price
    HNGGreeks            Heston-Nandi GARCH(1,1) option sensitivities
    HNGCharacteristics   combines option prices and sensitivities
    }
    
}


\section{About Rmetrics}{

    The \code{fOptions} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


