\name{fPortfolio-package}


\alias{fPortfolio}


\docType{package}


\title{Portfolio Modelling, Optimization and Backtesting}


\description{

    The Rmetrics "fPortfolio" package is a very powerful collection
    of functions to optimize portfolios and to analyze them from
    different points ov view.
    
    The implemented portfolio models include the traditional 
    mean--variance Markowitz portfolio, robust variants of
    the Markowitz portfoio, the mean--LPM lower partial moment
    portfolio, and the mean-CVaR conditional value-at-Risk portfolio.
    
    Optimization is possible either by minimizing the risk if the return
    is specified, or by maximizing the return if the risk is specified.
    
    Linear box/group constraints and quadratic covariance risk budgets 
    and/or copulae tail risk budgets can be specified.
    
    Depending on the model of the portfolio and the constraints
    powerful solvers are used for optimization including a linear
    programming solver, a quadratic programming solver, a 
    sequential quadratic programming solver, and a second order 
    programming solver.
    
    Several kinds of charts can be produced using powerful graphics 
    tools to visualize the results. 

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


\section{Overview:}{

    The following chapters give a brief introduction how to optimize
    and analyze portfolios.
    
    Table of Content:
    \tabular{rl}{
        1. \tab How to Specify a Portfolio ?\cr
        2. \tab How to Prepare the Data Input ?\cr
        3. \tab How to Define Portfolio Constraints ?\cr
        4. \tab How to Optimize a Portfolio ?\cr
        5. \tab Implemented Portfolios \cr
        6. \tab How to Analyze an Optimized Portfolio ?\cr
        7. \tab How to Analyze the Efficient Frontier ?\cr
        8. \tab How to Backtest a Rolling Portfolio ?
    }
    
}

   
\section{1. How to Specify a Portfolio?}{

    To optimize a portfolio of assets we first have to specify it. All
    settings which specify a portfolio of assets are respresented by a
    S4 object of \code{\link{class-fPFOLIOSPEC}}:
        
    \preformatted{
    setClass("fPFOLIOSPEC", 
      representation(
        model = "list",
        portfolio = "list",
        optim = "list") )
    }
        
    An object of class \code{fPFOLIOSPEC} has three slots, named
    \code{@model}, \@code{portfolio}, and \code{@optim}
    The first slot \code{@model} holds the model information, the second
    slot \code{@portfolio} the portfolio information, and the last slot
    \code{@optim} the information about the solver used for optimization.
    The default settings are as follows:
        
    \preformatted{
    model = list(
        type = "MV",
        optimize = "minRisk",   
        estimator = "covEstimator",     
        tailRisk = list(),               
        params = list(alpha=0.05, a=2)),
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
    
    To retrieve or modify portfolio specifications several 
    \code{\link{getSpec}} and \code{setSpec} function are
    available. To set a portfolio specification from scratch
    one can also use the function \code{\link{portfolioSpec}}.
    
    Note, there is a generic print function to print information from
    portfolio specifications. For example \code{print(portfolioSpec()}
    prints the default portfolio specification.
    
}


\section{2. How to Prepare the Data Input ?}{

    
    To optimize a portfolio or to compute the efficient frontier of
    a portfolio the assets which compose the portfolio must be available
    as a set of multivariate time series. These must be available in
    form of an S4 object of class \code{"timeSeries"}, the preferred
    time series representation in Rmetrics.
       
    Internally, the portfolio functions use the \code{\link{portfolioData}}
    function to generate an S4 object of \code{class-fPFOLIODATA}:
    
    \preformatted{
    setClass("fPFOLIODATA", 
      representation(
        data = "list",
        statistics = "list",
        tailRisk = "list") )
    }
    
    An object of class \code{fPFOLIODATA} has three slots repfresented
    by lists, named \code{@data}, \@code{@statistics}, and \code{@tailrisk}.
    The first slot \code{@data} holds the data information, the second
    slot \code{@statistics} the estimated mean and covariance together
    with optional robust estimates, and the last slot \code{@tailRisk} 
    optional information if we include copulae tail risk baskets into
    portfolio optimization.
    
    The function 
    
    \preformatted{
    portfolioData(data, spec = portfolioSpec())
    }
    
    creates an S4 object of class \code{fPFOLIODATA} using the
    portfolio specification to get the required information how to
    estimate mean and covariance matrix of the assets. 
    
    To retrieve information about the portfolios data set of assets
    several \code{\link{getData}} functions are available.
    
    Note, there is a generic print function to print information from
    a portfolio data object.
    
}


\section{3. How to Define Portfolio Constraints ?}{

    Portfolio optimization functions expect information about the
    constraints. These may be either box constraints, group constraints,
    covariance risk budget constraints or copulae tail risk budgets. 
    In addition \code{"LongOnly"} constraints generate box constraints
    for all weights ranging between zero and one, \code{"Short"}
    constraints allow for unlimited short selling.
    
    Weight constrained portfolios may be specified by a vector of
    character strings which describe executable code, setting values to
    vectors \code{minW}, \code{maxW}, \code{minsumW}, and \code{maxsumW}.
    The individual string elements of such vectors have the following 
    form for box and group constraints
    
    \description{
        \item{box constraints}{
            \code{"minW[Asset(s)]=Value(s)"}, and/or \cr
            \code{"maxW[Asset(s)]=Value(s)"}.
        }
        \item{group constraints}{
            \code{"minsumW[Asset(s)]=Value(s)"}, and/or \cr
            \code{"maxsumW[Asset(s)]=Value(s)"}. 
        }
    }
    
    \code{Asset(s)} is an index of one or more assets, and \code{Value(s)} 
    a numeric value or vector assigning the desired value. 
    By default, risk budgets are not included in the portfolio optimization.
    Covariance risk budget constraints have to be added explicitely, and 
    have the following form:
    
    \describe{
        \item{box constraints}{
            \code{"minB[Asset(s)]=Value(s)"}, and/or \cr
            \code{"minB[Asset(s)]=Value(s)"}.
        }
    }
    
    Again, \code{Asset(s)} is an index of one or more assets, and 
    \code{Value(s)} a numeric value or vector with numbers ranging between 
    zero and one, assigning the desired risk budgets. 
    %Copulae tail risk budget constraints have the same notation, but now 
    %\code{"minB"} and \code{"maxB"} replaced by \code{"minT"} and \code{"maxT"}.
    
    Internally, the portfolio functions use the 
    \code{\link{portfolioConstraints}} function to generate an S4 object 
    of \code{class-fPFOLIOCON}:
    
    \preformatted{
    setClass("fPFOLIOCON", 
      representation(
        stringConstraints = "character",
        boxgroupConstraints = "matrix",
        riskbudgetConstraints = "matrix") )
    }
    
    An object of class \code{fPFOLIOCON} has three slots represented
    by lists, named \code{@stringConstraints}, \@code{@boxgroupConstraints}, 
    and \code{@riskbudgetConstraints}.
    The first slot \code{@stringConstraints} holds the constraints 
    information given by the string specification, the second
    slot \code{@boxgroupConstraints} holds the box and group constraints
    in form of a matrix, and the last slot \code{@riskbudgetConstraints} 
    holds the covariance risk budget constraint also in form of a numerical
    matrix.
    
    The function which returns the constraints as an S4 object
    
    \preformatted{
    portfolioConstraints(data, spec = portfolioSpec(), constraints = "LongOnly")
    }
    
    requires as input the portfolio data set, the portfolio specification, 
    and the string of constraints. 
    
    To retrieve information about the portfolios constraints use the
    function \code{\link{getConstraints}}.
    
    Note, there is a generic print function to print information from
    a portfolio data object.
    
}


\section{4. How to Compute and Optimize Portfolios ?}{

    Rmetrics offers several functions to compute and optimize portfolios.
    These include the functions\cr
    
    \preformatted{
    feasiblePortfolio(data, spec, constraints) 
    efficientPortfolio(data, spec, constraints) 
    tangencyPortfolio(data, spec, constraints) 
    minvariancePortfolio(data, spec, constraints) 
    portfolioFrontier(data, spec, constraints) 
    }
    
    For all functions the user has to specifiy the \code{data} as an 
    S4 object of class \code{timeSeries}. The remaining two arguments 
    \code{spec=portfolioSpec()}, and \code{constraints="LongOnly"} have 
    default values.

    The portfolio functions return an S4 object of \code{class-fPORTFOLIO} 
    which is represented by the following slots:
    
    \preformatted{
    setClass("fPFOLIOCON", 
      representation(
        call = "call",
        data = "list",
        spec = "list",
        constraints = "character",
        portfolio = "list",
        title = "character",
        description = "character") )  
    }
    
    To retrieve information from a portfolio object use one of the
    extractor function, see \code{\link{getPortfolio}}. 
    
    Note, there is a generic print function to print information 
    from a portfolio data object and a plot function to display
    charts.
    
    The computation of a \code{\link{feasiblePortfolio}} doesn't require 
    portfolio optimization at all. Given the weights through portfolio
    specifiaction, use \code{setWeights}, the function 
    \code{\link{feasiblePortfolio}} directly computes the target return 
    and the target risk. The results are returned as an S4 object of
    class \code{fPortfolio}.
    
    The optimization of an efficient portfolio is done by the function
    \code{\link{efficientPortfolio}}. By default a mean--variance Markowitz
    optimization is performed with "LongOnly". If neither the target
    return is specified by the function \code{setTargetReturn} nor 
    the risk is pecified by the function \code{setTargetRisk}, then the
    results for the tangency portfolio with given risk free rate, will
    be returned.
    
    The \code{\link{tangencyPortfolio}} and \code{\link{minvariancePortfolio}} 
    are two special efficient portfolios, the first yields the tangency
    point on the efficient frontier with respect to the rosk free rate,
    and the second the point on the efficient frontier with the lowest
    risk.
        
    The function \code{\link{portfolioFrontier}} allows to compute points 
    along the efficient frontier, use \code{setNFrontierPoints}
    to modify the default value of 50. The points cover the whole range
    of feasible points on the efficient frontier with returns in equidistant 
    steps. 
    
    The solver used for portfolio optimization is selected automatically,
    but can also chosen by the experienced user. It is in the responsibility 
    of the user that he select a solver which is not in conflict with
    the specified portfolio model. Provided interfaces and solvers are:\cr
   
    \code{\link{solveRshortExact}} for unlimited short selling, \cr  
    \code{\link{solveRquadprog}} for quadratic objective with linear constraints, \cr 
    \code{\link{solveRglpk}} for linear objective with linear constraints, \cr 
    \code{\link{solveRsocp}} for linear objective with linear/quadratic constraints,\cr
    \code{\link{solveRdonlp2}} for non-linear objective with non-linear constraints. \cr
       
}


\section{5. Implemented Portfolios}{


    \bold{Optimize a Mean-Variance Portfolio:}
    
    By default the portfolio functions minimize the risk given the
    return of a mean-variance portfolio with long-only constraints. 
    Optinonally we can maximize the return given the risk, constraints
    are allowed for short selling if specified. Furthermore box,
    group, and covariance risk budget constraints can be added.
    
        \emph{Optimize a Mean-Variance Long-Only Portfolio:}
        
        This is the default case: 
        
        spec = portfolioSpec()\cr
        constraints = "Short"
        
        
        \emph{Optimize a Mean-Variance Short Selling Portfolio:}
        
        spec = portfolioSpec()\cr
        constraints = "LongOnly"
        
        
        \emph{Optimize a Mean-Variance Portfolio with Box Constraints:}
        
        spec = portfolioSpec()\cr
        constraints = c(minW, maxW)
        
        
        \emph{Optimize a Mean-Variance Portfolio with Group Constraints:}
        
        spec = portfolioSpec()\cr
        constraints = 
        
        
        \emph{Optimize a Mean-Variance Portfolio with Risk Budget Constraints:}
        
        spec = portfolioSpec()\cr
        constraints = 
    
    
    \bold{Optimize a Robust Mean-Variance Portfolio:}
    
    Mean-Variance portfolios can be robustified by replace the man
    and/or covariance matrix by robust estimators. Not only everal 
    choices are implemented, the user can also add his own estimator
    for the mean and covariance. In any case the use has tpo specify
    the name of the function which estimates mean /and or covaraince.
        
    spec = portfolioSpec()
    setEstimator <- "robEstimator"
    
    
    \bold{Optimize a Mean-LPM Portfolio:}
    
    The optimization of a "Mean - Lower Partial Moment" portfolio is done
    mapping the Mean-LPM portfolio on a quadratic form which looks formally
    identical to a Mean-Variance portfolio.
    
    \emph{Type:}
    We set the portfolio type to "LPM", and automatically the default
    LPM estimator "lpmEstimator" will be chosen. Note you can overwrite
    this choice using a robust LPM estimator for the portfolios "mu"
    and Sigma".
    
    setType <- "LPM"
    setEstimator <- "lpmEstimator"
    
    \emph{Constraints:}
    Concerning the constraints, they can be set in the same way as for
    a Mean-Variance portfolio. This includes box, group and LPM risk
    budget constraints.
    
    \emph{Optimize:}
    By default the "LPM" risk will be minimized for a given "Mean" return.
    Optionally one can maximize the target return for a predetermined
    value of the risk.
    For risk minimization the solver "solveRquadprog" is used, the 
    default, for return maximization the solver "solverRdonlp2" is 
    used.
    
    
    \bold{Optimize a Mean-CVaR Portfolio:}
    
    The optimization of a "Mean - CVaR" portfolio is done using the 
    approach of Uryasev and Rockafeller. The portfolio is mapped to
    a linear programming problem.
    
    \emph{Type:}
    We set the portfolio type to "CVaR",
    
    setType <- "CVaR"
    
    \emph{Constraints:}
    Concerning the constraints, they can be set in the same way as for
    a Mean-Variance portfolio. This includes box, group and LPM risk
    budget constraints.
    
    \emph{Optimize:}
    By default the "CVaR" risk will be minimized for a given "Mean" return.
    Optionally one can maximize the target return for a predetermined
    value of the risk.
    For risk minimization the solver "solveRglpk" is used, the 
    default, for return maximization the solver "solverRsocp" is 
    used.
    
}


\section{6. How to Analyze an Optimized Portfolio ?}{

    There exist several printouts and plots to display the results of an 
    optimized portfolio. For an individual portfolio ...
    
    \emph{Print}
    
    \emph{Plot}
    \code{weightsPie}
    
    \emph{Plot}
     
}


\section{7. How to Analyze the Efficient Frontier ?}{
    
    There exist several plots to display the results of an optimized
    portfolio or to display the portfolio frontier.
    
    For the efficient frontier ...
    
    \code{plot}\cr
    \code{frontierPlot}\cr
    \code{weightsPlot} 
    
    \code{frontierSlider}\cr
    \code{weightsSlider}
    
}



\section{8. How to Backtest a Rolling Portfolio ?}{

    Rmetrics offers functions for a rolling backtesting of 
    historical portfolios:\cr
    
    \code{rollingPortfolio} \cr
    \code{portfolioBacktesting} \cr
    \code{rollingPortfolio} \cr 
    \code{portfolioBacktesting} 
    
}
   

\note{

    With Rmetrics version 2.7.0 the function \code{cmlPortfolio}
    has been deprecated. Setting the risk free rate, sing the 
    function \code{setRiskFreeRate} as before and calling the 
    function \code{tangencyPortfolio} will yield the same results.
    
    The following functions and group of functions have been moved 
    to the \code{fAssets} package, since they rather deal with multivariate
    assets data and not with portfolios. \code{covEllipsesPlot},
    \code{VarModelling}.
    
    The function \code{maxxdd} which computes the theoretical density
    of drawdowns for random walks has been moved to the \code{fBasics}
    package which covers the field of distributions.
    
    The packages \code{Rdonlp2} with the non-linear programming solver
    and \code{Rsocp} swith the second order cone programming solver
    are suggested packages. The first can be downloaded from the website 
    \url{http://arumat.net/Rdonlp2/} or from the Rmetrics repository,
    for the second package please contact \emph{wuertz@phys.ethz.ch}.
    
}
    

\keyword{models}

