\name{fAsianOptions-package}


\alias{fAsianOptions-package}
\alias{fAsianOptions}


\docType{package}


\title{EBM and Asian Option Valuation}


\description{

    The Rmetrics "fAsianOptions" package is a collection of functions to 
    analyze and model Exponential Brownian Motion and to valuate Asian
    options. 
    
}


\details{

    \preformatted{
        Package:    \tab fAsianOptions\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
        Repository: \tab R-FORGE\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{

    The \code{fAsianOptions} package provides functions for pricing
    and valuating Asian Options together with tools for analyzing
    and modeling Exponential Brownian Motion (EBM).
}


\section{2 Asian Option Pricing}{

    This is a collection of functions used in the theory of exponential 
    Brownian Motion, EBM, and in the valuation of Asian options.
    
    %The functions for Moment matching and Series Expansions are:
    
    \preformatted{
    MomentMatchedAsianOption     valuate moment matched option prices
    - "LN"                       Log-Normal Approx of Levy, Turnbull and Wakeman
    - "RG"                       Reciprocal-Gamma Approx of Milevski and Posner
    - "JI"                       Johnson Type I Approx of Posner and Milevsky
    MomentMatchedAsianDensity    valuate moment matched option densities
    - "LN"                       Log-Normal Approximation
    - "RG"                       Reciprocal-Gamma Approximatio
    - "JI"                       Johnson Type I Approximation
    GramCharlierAsianOption      calculates Gram-Charlier option prices
    }
    
    \preformatted{
    AsianOptionMoments           methods to calculate Asian Moments
    - "A"                        using moments from Abrahamson's Formula
    - "D"                        using moments from Dufresne's Formula
    - "TW"                       using first 2 Moments from Turnbull-Wakeman
    - "T"                        including asymptotic Behavior after Tolmatz
    }
    
    
    % \preformatted{
    % Schroeder1AsianDensity
    % Schroeder2AsianDensity
    % Yor1AsianDensity 
    % Yor2AsianDensity 
    % TolmatzAsianDensity 
    % TolmatzAsianProbability
    % }
    
    
    \preformatted{
    ZhangAsianOption             Asian option price by Zhang's 1D PDE
    VecerAsianOption             Asian option price by Vecer's 1D PDE
    }

    \preformatted{
    gGemanYor                   Function to be Laplace inverted
    GemanYorAsianOption         Asian option price by Laplace Inversion
    gLinetzky                   Function to be integrated
    LinetzkyAsianOption         Asian option price by Spectral Expansion
    }

    \preformatted{
    BoundsOnAsianOption          using lower and upper bonds on Asian calls
    CurranThompsonAsianOption    using Thompson's continuous limit
    RogerShiThompsonAsianOption  using Thompson's single integral formula
    ThompsonAsianOption          using Thompson's upper bound
    TolmatzAsianOption           using lower bound from Tolmatz' asymptotics
    }

    \preformatted{
    CallPutParityAsianOption     using Call-Put parity relation
    WithDividendsAsianOption     adding dividends to Asian option formula
    }

    \preformatted{
    FuMadanWangTable             returns table from Fu, Madan and Wang's paper
    FusaiTaglianiTable           returns able from Fusai und tagliani's paper
    GemanTable                   returns table from Geman's paper
    LinetzkyTable                returns table from Linetzky's paper
    ZhangTable                   returns table from Zhang's paper
    ZhangLongTable               returns long table from Zhang's paper
    ZhangShortTable              returns short table from Zhang's paper
    }
}


\section{2 EBM Distribution}{

    In this section we summarize distributions and related functions 
    which are useful in the theory of exponential Brownian motion 
    and Asian option valuation. The functions compute densities and 
    probabilities for the log-Normal distribution, the Gamma 
    distribution, the Reciprocal-Gamma distribution, and the Johnson 
    Type-I distribution. Functions are made available for the compution 
    of moments including the Normal, the log-Normal, the Reciprocal-Gamma, 
    and the Asian-Option Density. In addition a function is given to 
    compute numerically first and second derivatives of a given  
    function.
    
    \preformatted{
    dlognorm        returns the log-Normal density and derivatives
    plognorm        returns the log-Normal, a synonyme for R's plnorm
    }
    
    \preformatted{
    dgam            returns the Gamma density, a synonyme for R's dgamma
    pgam            returns the Gamma probability, a synonyme for R's pgamma
    }
    
    \preformatted{
    drgam           returns the Reciprocal-Gamma density
    prgam           returns the Reciprocal-Gamma probability
    }
    
    \preformatted{
    djohnson        returns the Johnson Type I density
    pjohnson        returns the Johnson Type I probability
    }
    
    \preformatted{
    mnorm           returns the moments of Normal density
    mlognorm        returns the moments of log-Normal density
    mrgam           returns the moments of reciprocal-Gamma density
    masian          returns the moments of Asian Option density
    derivative      returns the first and second numerical derivative
    }

}


\section{3 Bessel Functions}{

    This section offers special mathematical functions which compute the 
    modified Bessel functions of integer order of the first and second 
    kind as well as their derivatives.     

    \preformatted{
    BesselI         computes modified Bessel function of the 1st kind
    BesselDI        computes its derivative
    BesselK         computes the modified Bessel function of the 3nd kind
    BesselDK        computes its derivative
    }   
}


\section{4 Gamma Function}{

    This section offers special mathematical functions which compute the
    Gamma function and related functions. The functions include the error 
    function, the Psi function, the incomplete Gamma function, the 
    Gamma function for complex argument, and the Pochhammer symbol. The 
    Gamma function the logarithm of the Gamma function, their first four 
    derivatives, and the Beta function and the logarithm of the Beta 
    function are part of R's base package (marked by an asterisk). For 
    example, these functions are required to valuate Asian Options based 
    on the theory of exponential Brownian motion.
    
    \preformatted{
    erf             computes the Error function
    gamma*          computes the Gamma function
    lgamma*         the logarithm of the Gamma function
    digamma*        the first derivative of the Log Gamma function
    trigamma*       the second derivative of the Log Gamma function
    tetragamma*     the third derivative of the Log Gamma function
    pentagamma*     the fourth derivative of the Log Gammafunction
    beta*           the Beta function
    lbeta*          the logarithm of the Beta function
    Psi             computes the Psi or Digamma function
    igamma          computes the incomplete Gamma function
    cgamma          computes the Gamma function for complex argument
    Pochhammer      returns the Pochhammer symbol
    }
    
}


\section{5 Hypergeometric Function}{

    This section offers special mathematical functions which compute 
    the confluent hypergeometric and related  functions. These functions
    are required to valuate Asian Options based on the theory of 
    exponential Brownian motion.     

    \preformatted{
    kummerM         the Confluent Hypergeometric Function of the 1st kind
    kummerU         the Confluent Hypergeometric Function of the 2nd kind
    whittakerM      the Whittaker M Function
    whittakerW      the Whittaker W Function
    hermiteH        the Hermite Polynomials
    }   
    
}


\section{About Rmetrics}{

    The \code{fOptions} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


