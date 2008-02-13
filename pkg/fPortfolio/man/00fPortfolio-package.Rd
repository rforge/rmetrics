\name{fPortfolio-package}


\alias{fPortfolio}


\docType{package}


\title{Portfolio Modelling, Optimization and Benchmarking}


\description{
Portfolio Package
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
            params = list(a=2, targetAlpha=0.05)),
        portfolio = list(
            weights = NULL, 
            targetReturn = NULL, 
            targetRisk = NULL,
            riskFreeRate = 0, 
            nFrontierPoints = 50,
            status = NA),
        optim = list(
            solver = NULL,         
            trace = FALSE)
        }
        
        
        The list entry \code{type} from the \code{@model} slot describes 
        the type of the desired portfolio. The current implementation 
        supports three type of portfolios. This may be 
        a Markowitz mean-variance portfolio named \code{"MV"}, 
        a mean - lower partial moment portfolio named \code{"LPM"}, or a 
        mean - CVaR condititional value-at-risk portfolio named 
        \code{"CVaR"}. Note, it is possible that an experienced user can
        add his own portfolio models, so the validity of the input is not
        spell checked.
        Use the function \code{setType} to modify this selection.
         
        
        The list entry \code{estimator} from the \code{@model} slot requests 
        for a string that denotes the function name of the covariance 
        estimator which should be used for the estimation of risk in 
        Markowitz' mean-variance portfolio model. By default the function 
        \code{covEstimator} is used which computes the standard column means 
        of the multivariate assets data series and the standard covariance 
        matrix. Alternative robust estimators include 
            \code{covMcdEstimator}, 
            \code{covOGKEstimator},
            \code{mveEstimator},
            \code{nnveEstimator}, or
            \code{mcdEstimator}.
        In addition a shrinkage covariance estimator named
            \code{shrinkEstimator},
        and a bagged covariance estimator named
            \code{baggedEstimator} are also available.
        Note the experienced user can add his own function to estimate in any
        alternative way the mean and the covariance of the multivariate assets
        data series. In this case he has to write a function, e.g. named
            \code{myEstimator=function(x,spec=NULL,...)},
        where \code{x} is a multivariate time series, \code{spec} optionally 
        the portfolio specification, if rquired, and \code{...} additional
        arguments passed to the users code. Note, \code{myEstimator} must
        a return a named list, with at least the following two entries
        \code{\$mu} and \code{\$Sigma}, which represent estimators for the
        mean and covariance, respectively. The LPM portfolio is just a
        Markowitz mean-variance portfolio with a modified covariance 
        estimator. In this case the setting of the \code{@model\$type} 
        changes the covariance estimator function name \code{"covEstimator"} 
        or any other selection pfreviously made to the function automatically
        to \code{"assetsLPM"} which returns the LPM mean and covariance 
        estimates.   
        Be aware, that the validity of the input name of the estimator
        function is not spell checked.
        Use the function \code{setEstimator} to modify this selection.
         
         
        The list entry \code{tailRisk} from the \code{@model} slot is an empty 
        list. It can be used to add tail risk budget constrains to the 
        optimization. In this case a square matrix of the size of the 
        number of assets is expected as list entry, which contains bivariate 
        tail risk measures, i.e. the tail dependence coefficients estaimated 
        via a copulae approach.   
        Use the function \code{setType} to modify this selection.
        
        
        The list entry \code{parameters} from the \code{@model} slot is a 
        list with additional parameters used in different situations. It can 
        be ebhanced by the user if needed. 
        By default it contains the exponent \code{a=2}, the parameter needed 
            for "LPM" portfolio optimization,
        and it contains the \code{targetAlpha=0.05}, the confidence level
            for "CVaR" portfolio optimization.
        Use the function \code{setParams} to modify this selection.
        
        The values \code{weights}, \code{targetReturn}, and \code{targetRisk}
        from the \code{portfolio} slot have to be considered in common. By
        default all three are set to \code{NULL}. If this is the case, then 
        it is assumed that an equal weight portfolio should be calculated.
        If only one of the three values is different from \code{NULL} then
        the following procedure will be startet. 
        If the weights are specified then it is assumed that a feasible 
            portfolio should be considered.
        If the target return is fixed then it is assumed that the efficient 
            portfolio with the minimal risk will be considered. 
        And finally if the risk is fixed, then the return should be maximized.
        Use the functions \code{setWeights}, \code{setTargetReturn}, and
        \code{setTargetRisk} to modify this selection.Note, the change in of
        the three functions will influence the settings of the other two.
        
        
        The \code{riskFreeRate=0} is also stored in the \code{portfolio} slot.
        Its value defaults to zero. It can be changed by the user.
        Use the function \code{setRiskFreeRate} to modify this selection.
        
        
        The number of frontier points reqauired by the calculation of
        the \code{portfolioFrontier} is obtained from the value of
        \code{nFrontierPoints=50} hold in the \code{portfolio} slot.
        Its value defaults to 50. It can be changed by the user.
        Use the function \code{setNFrontierPoints} to modify this selection.
        
        
        The final \code{status} of portfolio optimization is returned 
        and stored in the \code{portfolio} slot. Before optimization the
        value is unset to \code{NA}, after optimization a value of 
        \code{status=0} means a successful termination. For other
        values we recommend to inspect the help page of the selected
        solver, the name of the solver can be returned by the function
        \code{getSolver}.
        Use the function \code{setSolver} to reset the value to \code{NA}
        if it should be required.
        
        The name of the default solver used for optimization can be retrieved
        calling the function \code{getSolver}. The default value for the
        value \code{solver} in the specification is set to \code{NULL} 
        which means that the best solver availalbe will be autoselected 
        and used. Before optimization the user can change the setting
        to another solver. Be aware, that a possible personal change will 
        be overwritten by the function \code{setType}, so call 
        \code{setSolver} after setting the type of the portfolio.
        
        
        The logical flag \code{trace} in the slot \code{optim} allows
        to trace optionally the portfolio optimization process. By 
        default this will not be the case since the default value is 
        \code{trace=FALSE}. Use the fanction \code{setTrace} to modify
        the selection.
        
                
    \code{print}
    
    
        There is a generic print function to print information from
        specification. What is printed depends on the values of the
        settings. For example \code{print(portfolioSpec())} returns
        the type of portfolio, the name of the covariance estimator,
        the portfolios risk free rate, and the desired solver.
    
    
    \code{getSpec}
    
        The function \code{get} ...
    
    \code{setSpec}
    
        The function \code{get} ...
    
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
