\name{garchFit}


\alias{garchFit}
\alias{garchKappa}


\title{Univariate GARCH Time Series Fitting}


\description{

    Estimates the parameters of an univariate GARCH process.

}


\usage{    
garchFit(formula, data, init.rec = c("mci", "uev"), 
    delta = 2, skew = 1, shape = 4, 
    cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL, 
        include.shape = NULL, leverage = NULL, trace = TRUE, 
    algorithm = c("nlminb", "mnfb", "sqp", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    hessian = c("cda", "fda"), control = list(), 
    title = NULL, description = NULL, \dots)
    
garchKappa(cond.dist = c("norm", "ged", "std", "snorm", "sged", "sstd"), 
    gamma = 0, delta = 2, skew = NA, shape = NA)
}


\arguments{

    \item{algorithm}{
        a string parameter that determines the algorithm used for maximum 
        likelihood estimation. Allowed values are \code{"nmfb"}, \code{"sqp"}, 
        \code{"nlminb"}, and \code{"bfgs"} where the third is the default
        setting. \code{"mnfb"} is a fully Fortran implemented and extremely
        fast version of the R-coded \code{"nlminb"} algorithm. 
        }
    \item{cond.dist}{
        a character string naming the desired conditional distribution.
        Valid values are \code{"dnorm"}, \code{"dged"}, \code{"dstd"}, 
        \code{"dsnorm"}, \code{"dsged"}, \code{"dsstd"}. The default value 
        is the normal distribution.
        }
    \item{control}{
        control parameters, the same as used for the functions from
        \code{nlminb}, and 'bfgs' and 'Nelder-Mead' from \code{optim}.
        }
    \item{data}{
        an optional timeSeries or data frame object containing the variables 
        in the model. If not found in \code{data}, the variables are taken 
        from \code{environment(formula)}, typically the environment from which
        \code{armaFit} is called. If \code{data} is an univariate series, then
        the series is converted into a numeric vector and the name of the
        response in the formula will be neglected.
        }
    \item{delta}{
        a numeric value, the exponent \code{delta} of the variance recursion.
        By default, this value will be fixed, otherwise the exponent will be 
        estimated together with the other model parameters if 
        \code{include.delta=FALSE}.
        }
    \item{description}{ 
        a character string which allows for a brief description. 
        }
    \item{formula}{ 
        formula object describing the mean and variance equation of the 
        ARMA-GARCH/APARCH model. A pure GARCH(1,1) model is selected 
        when e.g. \code{formula=~garch(1,1)}. To specify for example an 
        ARMA(2,1)-APARCH(1,1) use \code{formula = ~arma(2,1)+apaarch(1,1)}.
        }
    \item{gamma}{
        APARCH leverage parameter entering into the formula for calculating
        the expectation value.
        }
    \item{hessian}{
        a string denoting how the Hessian matrix should be evaluated,
        either \code{hessian ="cda"}, the default, or \code{"fda"}.
        The forward difference approximation, \code{"fda"} consumes less 
        computer time, but is usually not as precise as approximations 
        that use central difference approximation, \code{"cda"}, formula.
        }
    \item{include.delta}{
        a logical flag which determines if the parameter for the recursion
        equation \code{delta} will be estimated or not. If 
        \code{include.delta=FALSE} then the shape parameter will be kept 
        fixed during the process of parameter optimization.
        }
    \item{include.mean}{
        this flag determines if the parameter for the mean will be estimated
        or not. If \code{include.mean=TRUE} this will be the case, otherwise
        the parameter will be kept fixed durcing the process
        of parameter optimization.
        }
    \item{include.shape}{
        a logical flag which determines if the parameter for the shape
        of the conditional distribution will be estimated or not. If 
        \code{include.shape=FALSE} then the shape parameter will be kept 
        fixed during the process of parameter optimization.
        }
    \item{include.skew}{
        a logical flag which determines if the parameter for the skewness
        of the conditional distribution will be estimated or not. If 
        \code{include.skew=FALSE} then the skewness parameter will be kept 
        fixed during the process of parameter optimization.
        }
    \item{init.rec}{
        a character string indicating the method how to initialize the 
        mean and varaince recursion relation. 
        }
    \item{leverage}{
        a logical flag for APARCH models. Should the model be leveraged?
        By default \code{leverage=TRUE}.
        }  
    \item{shape}{
        a numeric value, the shape parameter of the conditional distribution.
        }
    \item{skew}{
        a numeric value, the skewness parameter of the conditional distribution.
        }
    \item{title}{
        a character string which allows for a project title.
        } 
    \item{trace}{
        a logical flag. Should the optimization process of fitting the
        model parameters be printed? By default \code{trace=TRUE}. 
        }
    \item{\dots}{
        additional arguments to be passed.
        }   

}

\value{
    
    returns an object of class \code{"fGARCH"}.
        
    The entries of the @fit slot show the results from the
    optimization.

}


\details{

    \bold{How to specify the formula for an univariate data input?}
    
    \code{formula} and \code{data} arguments are closely related. 
    Several different data types are allowed to be specified. Our 
    recommended data object would be a time
    series object of class \code{"timeSeries"}, but also other
    time series data objects are allowed: Simple vectors (usually used
    when missing time stamps), R's base time series objects of class 
    \code{"ts"}, and Z's ordered time series objects of class 
    \code{"zoo"}. Let us start to consider univariate objects, defining
    univariate time series objects from several classes:
    
        \preformatted{ 
        x.vec = 100*garchSim(model, n = 1000, returnClass = "numeric") 
        x.timeSeries = dummyDailySeries(matrix(x.vec), units = "GARCH11")  
        x.zoo = zoo(x.vec, order.by = as.Date(rownames(x.timeSeries)))  
        x.ts = as.ts(x.vec) 
        }
    
    Fitting a GARCH(1,1) is done in the following way:
    
        \preformatted{ 
        garchFit(~ garch(1,1), data = x.vec))  
        garchFit(~ garch(1,1), data = x.timeSeries))       
        garchFit(~ garch(1,1), data = x.zoo))  
        garchFit(~ garch(1,1), data = x.ts)) 
        }
    
    Note it is not necessary to specifiy a left hand side for the 
    formula expression.\cr
    
    
    \bold{How to specify the formula for a multivariate data input?}
    
    For multivariate time series one has to specify the left hand side
    of the formula, so that the \code{garchFit} function knows which
    column has to be taken for the modelling process. let us construct a
    bivariate time series where the first column is a GARCH(1,1) proces
    and the second coulmn random normal variates
    
        \preformatted{ 
        X.mat = cbind(GARCH11 = x.vec, R = rnorm(1000))  
        X.timeSeries = dummyDailySeries(X.mat, units = c("GARCH11", "R")) 
        X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.timeSeries)))  
        X.mts = as.ts(X.mat) 
        }
    
    To model the GARCH(1,1) process from the first column of the datasets 
    above, we have to express the left hand formula by the desired column
    name of the bivariate series:
    
        \preformatted{ 
        garchFit(GARCH11 ~ garch(1,1), data = X.mat))  
        garchFit(GARCH11 ~ garch(1,1), data = X.timeSeries))          
        garchFit(GARCH11 ~ garch(1,1), data = X.zoo))  
        garchFit(GARCH11 ~ garch(1,1), data = X.ts))  
        }
    
    
    \bold{Extracting the Environment of a Model Formula}
    
    The \code{garchFit} also allows to make use of the environment of the
    model formula. As an example we show how to model the spread of two
    series from a multivariate data set.
    Load the \code{LPP2005REC} "Swiss Pension Fund" benchmark portfolio time
    series, given by a data frame:
    
        \preformatted{ 
        data(LPP2005REC) 
        }
    
    This data frame consists of 10 columns of financial returns named 
    LPP25, LPP40, and LPP60, together with its 6 major asset classes 
    SBI (Swiss Bonds), 
    SPI (Swiss Eqiuities), 
    SII (Swiss Immo Funds), 
    LMI (Foreign Bonds), 
    MPI (Foreign Equities), and
    ALT (Foreign Alternative Instruments). 
    The first column holds the daily date stamps. From the data frame
    we can extract the core \code{data} matrix and the character vector
    \code{charvec} of time stamps:
    
        \preformatted{ 
        data = LPP.mat = as.matrix(LPP.df[, 2:10])  
        charvec = as.character(LPP.df[, 1]) 
        }

    From these two objects we can construct several multivariate time 
    series objects which can serve as input for the \code{garchFit} 
    function, e.g. a \code{"timeSeries"} object, objects of class 
    \code{"zoo"} or \code{"mts"}, or just a data \code{"matrix"}. Then 
    the percentual spread of the Swiss, SPI, and foreign equity, MPI,
    indexes can be modelled in the following way:
    
        \preformatted{ 
        LPP = as.timeSeries(data(LPP2005REC)) 
        garchFit(100*(SPI-MPI) ~ garch(1,1), data = LPP) 
        }
        
    As a second example we show how to model high/low spreads from 
    the Microsoft equity price series:
        
        \preformatted{ 
        MSFT = as.timeSeries(data(msft.dat))  
        garchFit(100*(High-Low) ~ garch(1,1), data = returns(MSFT)) 
        }
    
        
    \bold{How are time series slots returned by the function \code{garchFit}?}
   
    The value section describes the S4 object returned by the function 
    \code{garchFit}. Special attention require the four time series slots
    \code{@residuals},
    \code{@fitted},
    \code{@h.t}, and
    \code{@sigma.t}.
    Their output is always a numeric vector with optional time stamps, 
    if available. If we want to have the output as objects from the 
    same class as the data points we can use the extractor functions
    
        \preformatted{ 
        residuals(object)  
        fitted(object)  
        volatility(object, type = c("sigma", "h"))  
        }
    
    where object is a fitted model as returned by the function 
    \code{garchFit}.\cr

  
    \bold{GARCH Processes with non-normal distributions}
    
    Bollerslev [1987] was the first who modelled financial time series 
    for foreign exchange rates and stock indexes using GARCH(1,1) models 
    extended by the use of standardized Student-t distributions.
    It is straightforward with the function \code{garchFit} to model 
    GARCH processes with non-normal distribution functions, just 
    specifying the argument \code{cond.dist} by one of the alternatives.
    The argument \code{cond.dist} allows to select one from six conditional 
    distributions. Three of them are skewed distributions, (the skew-Normal, 
    the skew-Student, and the skew-GED), and four of them have an additional 
    shape parameter (the Student, the GED and their skewed versions). 
    
    The values for the skew and the shape are determined through the arguments 
    \code{skew} and \code{shape}. There are two additional undetermined
    arguments named \code{include.skew} and \code{include.shape}. They can 
    be set to \code{TRUE} or \code{FALSE}, then the distributional parameters 
    are included or kept fixed during parameter optimization, respectively. 
    If they are undetermined, then an automate selection is done depending 
    on the choice of the other parameters. The following example shows
    how to fit Bollerslev's t-GARCH(1,1) model:
    
        \preformatted{ 
        data(dem2gbp) 
        garchFit(~ garch(1,1), data = dem2gbp, cond.dist = "dstd")
        }
        
    Here the shape of the distribution, giving the number of degrees of 
    freedom of the Student-t distribution, has been also estimated. If we
    like to fix this value to 4, a number which is often observed for
    financial return series, we submit the command
    
        \preformatted{ 
        garchFit(~ garch(1,1), data = dem2gbp, cond.dist ="dstd",
            shape = 4, include.shape = FALSE)
        }
    
           
    \bold{The Solvers:}
       
    The reason why the function \code{garchFit} has implemented different 
    optimization algorithms has historical reasons.\cr
    
    
    \code{algorithm = "lbfgsb"}\cr
    When the first version 
    was written the only solver implemented in R was the AMC TOMS 778 
    algorithm \code{optim(method="L-BFGS-B")}.\cr 
    
    
    \code{algorithm = "nlminb"}\cr
    Later when the function 
    \code{nlminb} was added to R's base package, interfacing the PORT 
    Mathematical Software Library, we implemented it in \code{garchFit}.
    This is the default solver.\cr 
    
    
    \code{algorithm = "nlminb+nm"}\cr
    \code{algorithm = "lbfgsb+nm"}\cr
    Unfortunately, we found that for both solvers \code{"lbfgsb"} 
    and \code{"nlminb"} it was not always 
    possible to find the optimal values for the set of model parameters 
    even if we tried to adapt the control parameters. This problem could be 
    solved in many cases using a two stage approach: First find a (near 
    optimal) solution with one of the two constrained algorithms,
    and then start with this solution a second optimization step with 
    the (unconstrained) simplex algorithm of Nelder-Mead. 
    The result is that one finds with
    this "hybrid approach" or "mixed method" in many cases an improved 
    solution. The idea of two step approaches is also mentioned in 
    McCullough and Renfro [1999].\cr
    
    
    \code{algorithm = "sqp"}\cr
    The idea to implement the "Sequential 
    Quadratic Programming" algorithm, SQP, was inspired by the fact that 
    this algorithm is also part of the Ox, Doormik [1999], Matlab and Gauss,
    Schoenberg [1999], software packages. SQP based algorithms represent 
    very efficient nonlinear programming methods. SQP algorithms have been 
    implemented and tested, see e.g. Schittkowski [1999]. They outperform 
    often other methods in terms of accuracy and efficiency over a large 
    number of test problems. Based on work of Biggs [1999], Han [1999], and 
    Powell [1999], the method allows to closely mimic constrained 
    optimization just as it is done for unconstrained optimization. An 
    overview of SQP methods can be found in Fletcher [1999], Gill et 
    al. [1999], Powell [1999], and Schittkowski [1999]. We have interfaced 
    to Rmetrics the SQP Fortran program written by Luksan [1999], which 
    is a recursive quadratic programming method with the BFGS variable 
    metric update for general nonlinear programming problems.
  
    The following example shows how to fit a normal GARCH(1,1) model 
    using different types of solvers, let us compare the fitted
    coefficients:
    
        \preformatted{ 
        coef(garchFit(~ garch(1,1), dem2gbp, algorithm = "nlminb"))  
        coef(garchFit(~ garch(1,1), dem2gbp, algorithm = "nlminb+nm")) 
        coef(garchFit(~ garch(1,1), dem2gbp, algorithm = "lbfgsb"))  
        coef(garchFit(~ garch(1,1), dem2gbp, algorithm = "lbfgsb+nm")) 
        coef(garchFit(~ garch(1,1), dem2gbp, algorithm = "sqp"))  
        }
        
    The execution times range by a factor of about 4, the fastest is 
    \code{"sqp"}, and the slowest \code{"lbfgsb+nm"}.\cr
    
    
    \bold{Why does the function \code{garchFit} sometimes fails?}
    
    There might be several reasons why the GARCH modelling process can fail.
    
    First, in the most simple case, the \code{formula} argument is 
    misspecified, e.g. the left hand side of the formula is missing
    for multivariate time series.
    
    Second, in the multivariate time series case, column names my be
    missing, defined by empty strings, or the may have doubled entries.
    
    Third, the GARCH model can be misspecified, so that the log likelihood
    function does not converge. In this case use another model expressed
    by the \code{formula} specification and/or choose another conditional 
    distribution function \code{cond.dist}. Pretest the unconditional
    returns, to find out what would be an appropriate unconditional
    distribution function for the GARCH model.
    
    Fourth, the log-likelihood function and the parameters are not properly
    scaled. In this case you should scale the objective function and the
    parameters using the \code{control=list()} specification. For details
    we refer to the descriptions of the selected solver. This happens often
    using logarithmic returns. A simple trick may circumvent this problem 
    and help in many cases, use percentual returns, i.e. multiply the time 
    series with a factor of 100. 
      
}


\section{Tracing the Output}{

    to do ...
}


\references{

ATT (1984);
    \emph{PORT Library Documentation}, 
    http://netlib.bell-labs.com/netlib/port/.
    
Bera A.K., Higgins M.L. (1993);
    \emph{ARCH Models: Properties, Estimation and Testing},
    J. Economic Surveys 7, 305--362.
  
Bollerslev T. (1986);
    \emph{Generalized Autoregressive Conditional Heteroscedasticity},
    Journal of Econometrics 31, 307--327.

Byrd R.H., Lu P., Nocedal J., Zhu C. (1995);
    \emph{A Limited Memory Algorithm for Bound Constrained Optimization},
    SIAM Journal of Scientific Computing 16, 1190--1208.
    
Engle R.F. (1982);
    \emph{Autoregressive Conditional Heteroscedasticity with Estimates 
    of the Variance of United Kingdom Inflation},
    Econometrica 50, 987--1008.

Nash J.C. (1990);
    \emph{Compact Numerical Methods for Computers},
    Linear Algebra and Function Minimisation,
    Adam Hilger.

Nelder J.A., Mead R. (1965);
    \emph{A Simplex Algorithm for Function Minimization},
    Computer Journal 7, 308--313.

Nocedal J., Wright S.J. (1999);
    \emph{Numerical Optimization},
    Springer, New York.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port,\cr
    R Core Team for the 'optim' \R-port,\cr
    Douglas Bates and Deepayan Sarkar for the 'nlminb' \R-port,\cr
    Bell-Labs for the underlying PORT Library,\cr
    Ladislav Luksan for the underlying Fortran SQP Routine, \cr
    Zhu, Byrd, Lu-Chen and Nocedal for the underlying L-BFGS-B Routine.
    
}


\examples{  
## UNIVARIATE TIME SERIES INPUT:
   # In the univariate case the lhs formula has not to be specified ... 
   
   # A numeric Vector from default GARCH(1,1) - fix the seed:
   N = 200
   x.vec = garchSim(garchSpec(rseed = 1985), n = N, returnClass = "numeric")
   garchFit( ~ garch(1,1), data = x.vec, trace = FALSE)

   # An univariate timeSeries object with dummy dates:
   x.timeSeries = dummyDailySeries(matrix(x.vec), units = "GARCH11")
   garchFit(~ garch(1,1), data = x.timeSeries, trace = FALSE)         

   # An univariate zoo object:
   x.zoo = zoo(as.vector(x.vec), order.by = as.Date(rownames(x.timeSeries)))
   garchFit(~ garch(1,1), data = x.zoo, trace = FALSE) 

   # An univariate "ts" object:
   x.ts = as.ts(x.vec)
   garchFit(~ garch(1,1), data = x.ts, trace = FALSE)
    
## MULTIVARIATE TIME SERIES INPUT:
   # For multivariate data inputs the lhs formula must be specified ...
   
   # A numeric matrix binded with dummy random normal variates:
   X.mat = cbind(GARCH11 = x.vec, R = rnorm(N))
   garchFit(GARCH11 ~ garch(1,1), data = X.mat)
        
   # A multivariate timeSeries object with dummy dates:
   X.timeSeries = dummyDailySeries(X.mat, units = c("GARCH11", "R")) 
   garchFit(GARCH11 ~ garch(1,1), data = X.timeSeries)
    
   # A multivariate zoo object:
   X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.timeSeries)))
   garchFit(GARCH11 ~ garch(1,1), data = X.zoo)

   # A multivariate "mts" object:
   X.mts = as.ts(X.mat)
   garchFit(GARCH11 ~ garch(1,1), data = X.mts)
 
## MODELING THE PERCENTUAL SPI/SBI SPREAD FROM LPP BENCHMARK:
    
   X.timeSeries = as.timeSeries(data(LPP2005REC))
   X.mat = as.matrix(x.timeSeries)
   X.zoo = zoo(X.mat, order.by = as.Date(rownames(X.mat)))
   X.mts = ts(X.mat)
   garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.timeSeries)
   # The remaining are not yet supported ...
   # garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mat)
   # garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.zoo)
   # garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mts)
    
## MODELING HIGH/LOW RETURN SPREADS FROM MSFT PRICE SERIES:
    
   X.timeSeries = as.timeSeries(msft.dat)
   garchFit(Open ~ garch(1,1), data = returns(X.timeSeries)) 
   garchFit(100*(High-Low) ~ garch(1,1), data = returns(X.timeSeries)) 
}


\keyword{models}

