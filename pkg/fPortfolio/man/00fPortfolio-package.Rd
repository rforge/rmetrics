\name{fPortfolio-package}


\alias{fPortfolio}


\docType{package}


\title{Portfolio Modelling, Optimization and Benchmarking}


\description{
...
}


\details{

    \tabular{ll}{
        Package:    \tab fPortfolio\cr
        Type:       \tab Package\cr
        Version:    \tab 261.73.1\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
}

   
\section{fPFOLIOSPEC: Portfolio Specification Class}{

    To optimize a portfolio of assets we first have to specify it. 
    
    \code{fPFOLIOSPEC}
    
        The portfolio specification is represented by a S4 object of class
        \code{fPFOLIOSPEC}.
        
        \preformatted{
        setClass("fPFOLIOSPEC", 
          representation(
            model = "list",
            portfolio = "list",
            optim = "list") )
        }
        
        The first slot \code{@model} holds the model information, the second
        slot \code{@portfolio} the portfolio information, and the last slot
        \code{optim} the information about the solver used for optimization.
        
        The default settings are as follows:
        
        \preformatted{
        model = list(
            type = "MV",    
            estimator = "covEstimator",     
            tailRisk = list(),               
            params = list()),
        portfolio = list(
            weights = NULL, 
            targetReturn = NULL, 
            targetRisk = NULL,
            targetAlpha = 0.05, 
            riskFreeRate = 0, 
            nFrontierPoints = 50,
            status = 0),
        optim = list(
            solver = "solveRquadprog",         
            trace = FALSE)
        }
        
        In the model slot \code{type} describes the type of the desired
        portfolio. This may be a Markowitz mean-variance portfolio named
        \code{"MV"}, a lower partial moment portfolio named \code{"LPM"},
        or a mean-CVaR condititional value-at-risk portfolio named 
        \code{"CVaR"}.
        
        The next list entry \code{estimator} is used for \code{"MV"} and
        \code{"LPM"} portfolio optimization. The string denotes a function
        name. For the default, the default function \code{covEstimator} 
        can be used in portfolio optimization to compute the standard mean 
        vector, and standard covariance matrix. The specification of other
        function names like \code{covMcdEstimator"}, or \code{"shrinkEstimator"}
        allow to to portfolio estimation with robust or shrinked mean/covariance
        estimates. 
                
    \code{getSpec}
    
    \code{setSpec}
    
}



\section{fPFOLIODATA: Portfolio Data and Statistics Class}{

    \code{fPFOLIODATA}
    
    \code{portfolioData}
    
    \code{getData}
    
    
}


\section{fPFOLIOCON: Portfolio Constraints Class}{

    \code{fPFOLIOCON}
    
    \code{portfolioConstraints}
    
    \code{setConstraints}
    
    \code{getConstraints}
    
    
}


\section{fPORTFOLIO: Portfolio Class}{

    \code{fPORTFOLIO}
    
    \code{feasiblePortfolio}
    
    \code{print} method
    
}    
    

\section{Efficient Portfolios}{

    
    \code{fficientPortfolio}
    
    \code{minvariancePortfolio}
    
    \code{cmlPortfolio}
    
    \code{tangencyPortfolio}  
    
} 


\section{Portfolio Frontier}{

    
    \code{portfolioFrontier} 
    
}


\section{Plotting a Portfolio}{

    
    \code{frontierPlot}
    
    \code{plot}
    
    \code{weightsPlot}
    
    \code{weightsPie}
    
    \code{frontierSlider}
    
    \code{weightsSlider}
    
}


\section{Portfolio Optimization: The Solvers}{

    \code{solveShortExact}\cr
    Unlimited short selling
    
    \code{solveRquadprog}\cr
    Quadratic objective with linear constraints
    
    \code{solveRlpSolve}\cr
    Linear objective with linear constraints 
    
    \code{solveRsocp}\cr
    Linear objective with linear/quadratic constraints
    
    \code{solveRdonlp2}\cr
    Non-linear objective with non-linear constraints
    
}


\section{Rolling Portfolio and Backtesting}{

    \code{rollingPortfolio}
    
    \code{portfolioBacktesting}
    
}
   

\keyword{models}
