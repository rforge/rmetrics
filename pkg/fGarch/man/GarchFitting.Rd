\name{GarchFitting}

\alias{GarchFitting}

\alias{garchFit}

\alias{garchKappa}


\title{Univariate GARCH Time Series Fitting}


\description{

    Estimates the parameters of an univariate GARCH process.

}


\usage{    
garchFit(formula, data, init.rec = c("mci", "uev"), delta = 2, skew = 1, 
    shape = 4, cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", 
    "dsstd"), include.mean = TRUE, include.delta = NULL, include.skew = 
    NULL, include.shape = NULL, leverage = NULL, trace = TRUE, algorithm = 
    c("nlminb", "sqp", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), control = 
    list(), title = NULL, description = NULL, \dots)
    
garchKappa(cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
    gamma = 0, delta = 2, skew = NA, shape = NA)
}


\arguments{

    \item{algorithm}{
        a string parameter that determines the algorithm used for maximum 
        likelihood estimation. Allowed values are \code{"sqp"}, 
        \code{"nlminb"}, and \code{"bfgs"} where the first is the default
        setting.
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
    \item{delta, include.delta}{
        the exponent \code{delta} of the variance recursion. By default,
        this value will be fixed, otherwise the exponent will be estimated 
        together with the other model parameters if \code{include.delta=FALSE}.
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
    \item{include.mean}{
        this flag determines if the parameter for the mean will be estimated
        or not. If \code{include.mean=TRUE} this will be the case, otherwise
        the parameter will be kept fixed durcing the process
        of parameter optimization.
        }
    \item{include.skew, include.shape}{
        this flag determines if the parameters for the skew and shape
        of the conditional distribution will be estimated or not. If 
        \code{include.skew=TRUE} and/or \code{include.shape=TRUE} this will 
        be the case, otherwise the parameters will be kept fixed durcing 
        the process of parameter optimization.
        }
    \item{init.rec}{
        a character string indicating the method how to initialize the 
        mean and varaince recursion relation. 
        }
    \item{leverage}{
        a logical flag for APARCH models. Should the model be leveraged?
        By default \code{leverage=TRUE}.
        }  
    \item{skew, shape}{
        skewness and shape parameter of the conditional distribution.
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
    
    \code{garchFit}\cr
    \cr
    returns a S4 object of class \code{fGARCH} with the following slots:

    \item{@call}{
        the call of the \code{garch} function.
        }
    \item{@formula}{
        a list with two formula entries, one for the mean and the other
        one for the variance equation.
        }
    \item{@method}{
        a string denoting the optimization method, by default the 
        returneds string is "Max Log-Likelihood Estimation".
        }  
    \item{@data}{
        a list with one entry named \code{x}, containing the data of
        the time series to be estimated, the same as given by the
        input argument \code{series}.
        } 
    \item{@fit}{
        a list with the results from the parameter estimation. The entries
        of the list depend on the selected algorithm, see below.
        }
    \item{@residuals}{
        a numeric vector with the residual values.
        }
    \item{@fitted}{
        a numeric vector with the fitted values.
        }
    \item{@h.t}{
        a numeric vector with the conditional variances.
        }
    \item{@sigma.t}{
        a numeric vector with the conditional variances.
        }
    \item{@title}{
        a title string.
        }
    \item{@description}{
        a string with a brief description.
        } 
        
    The entries of the @fit slot show the results from the
    optimization.

}


\details{

    GARCH, Generalized Autoregressive Conditional Heteroskedastic, models 
    have become important in the analysis of time series data, particularly 
    in financial applications when the goal is to analyze and forecast 
    volatility. 
    
    For this purpose, the family of GARCH functions offers functions for 
    simulating, estimating and forecasting various univariate GARCH-type 
    time series models in the conditional variance and an ARMA specification 
    in the conditional mean. The function \code{garchFit} is a numerical
    implementation of the maximum log-likelihood approach under different 
    assumptions, Normal, Student-t, GED errors or their skewed versions. 
    The parameter estimates are checked by several diagnostic analysis tools 
    including graphical features and hypothesis tests. Functions to compute 
    n-step ahead forecasts of both the conditional mean and variance are also
    available.

    The number of GARCH models is immense, but the most influential models 
    were the first. Beside the standard ARCH model introduced by Engle [1982] 
    and the GARCH model introduced by Bollerslev [1986], the function
    \code{garchFit} also includes the more general class of asymmetric power
    ARCH models, named APARCH, introduced by Ding, Granger and Engle [1993]. 
    The APARCH models include as special cases the TS-GARCH model of 
    Taylor [1986] and Schwert [1989], the GJR-GARCH model of Glosten, 
    Jaganathan, and Runkle [1993], the T-ARCH model of Zakoian [1993], the 
    N-ARCH model of Higgins and Bera [1992], and the Log-ARCH model of
    Geweke [1986] and Pentula [1986].
    
    There exist a collection of review articles by Bollerslev, Chou and 
    Kroner [1992], Bera and Higgins [1993], Bollerslev, Engle and
    Nelson [1994], Engle [2001], Engle and Patton [2001], and Li, Ling 
    and McAleer [2002] that give a good overview of the scope of the 
    research.\cr


    \bold{How to specify the formula and data input?}
    
    \code{formula} and \code{data} arguments are closely related. 
    Several different data types are allowed to be specified. Our 
    recommended data object would be a time
    series object of class \code{"timeSeries"}, but also other
    time series data objects are allowed: Simple vectors, R's base 
    time series objects of class \code{"ts"}, and Z's ordered time 
    series objects of class \ code{"zoo"}.
    
    Load a data file from the demo example, e.g. the LPP2005REC Swiss
    Pension Fund benchmark portfolio, given by a data frame:
    
        \code{> data(LPP2005REC)}\cr
        \code{> LPP.df = LPP2005REC}
    
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
    \code{charvec} of time stamps.
    

        \code{data = LPP.mat = as.matrix(LPP.df[, 2:10])}\cr
        \code{charvec = as.character(LPP.df[, 1])}

    
    From these two objects we can construct several time series objects
    which can serve as input for the \code{garchFit} function.\cr
 
    
    %--------------------------------------------------------------------
    \emph{Case 1:}\cr
    The data will be a numeric vector of returns:  
    First we create a numeric vector for the Swiss Performance Index, 
    SPI, and then we estimate the parameters. Note, that the left 
    hand side of the formula has not to be specified! 
     
    \code{> SPI = as.vector(DATA[, "SPI"])}
    \code{garchFit(~ garch(1,1), data = SPI)}
    
    
    %--------------------------------------------------------------------
    \emph{Case 2:}\cr 
    The data is given by a numeric matrix of returns:
    It is important to note, that the matrix must have column names
    to specify the left hand side of the formula:
        
    \code{> garchFit(SPI ~ garch(1,1), data = LPP.mat)}\cr
        
        
    %--------------------------------------------------------------------
    \emph{The generic Case 3:}\cr
    The data is given by a \code{"timeSeries"} object:   
    First let us construct the \code{"timeSeries"} object from scratch
    using the core \code{data} matrix and the character vector
    \code{charvec} with the given time stamps.
    Alternatively, we can construct the time series directly from the
    loaded data frame LPP2005RET, using the function \code{as.timeSeries}.
    Finally, we proceed in the same way as for the numeric matrix to 
    estimate the parameters. 
    Note, this procedure works for both, univariate and multivariate
    \code{"timeSeries"} objects. Don't forget, that the left hand side
    has to be specified by the column name of the selected time series.  
    
    \code{> LPP.tS = timeSeries(data, charvec)}\cr 
    \code{> LPP.tS = as.timeSeries(LPP2005REC)}\cr
    \code{> garchFit(SPI ~ garch(1,1), data = LPP.tS)}\cr
    
       
    %--------------------------------------------------------------------
    \emph{Case 4:}\cr
    The data is given by a \code{"data.frame"} object:
    Again, we proceed in the same way as for the numeric matrix: 
    
    \code{> garchFit(SPI ~ garch(1,1), data = LPP.df)}\cr

        
    %---------------------------------------------------------------------------
    \emph{Case 5:}\cr 
    The data is given by a \code{"zoo"} object: First let 
    us construct an example \code{"zoo"} object from scratch using the core 
    \code{data} matrix and the character vector \code{charvec} with the 
    given time stamps. Again, we proceed in the same way as for the numeric 
    matrix: 
        
    \code{> LPP.zoo = zoo(data = LPP.mat, order.by = as.Date(charvec))}\cr
    \code{> garchFit(SPI ~ garch(1,1), data = LPP.zoo)}\cr
        
      
    % ==========================================================================
    \bold{How are time series returned by the function \code{garchFit}?}
   
    The value section describes the S4 object returned by the function 
    \code{garchFit}. Special attention require the four time series slots
    \code{@residuals},
    \code{@fitted},
    \code{@h.t}, and
    \code{@sigma.t}.
    Their output depends on the class of the \code{data} input, i.e. the
    class of the object of the above mentioned slots have the same mode
    as the input \code{data} argument.\cr
   
    
    % ==========================================================================
    \bold{GARCH Processes with non-normal distributions}
    
    Bollerslev [1987] was the first who modelled financial time series 
    for foreign exchange rates and stock indexes using GARCH(1,1) models 
    extended by the use of standardized Student-t distributions. In 
    comparison to conditionally normal errors he found that t-GARCH(1,1)
    errors much better capture the leptokurtosis seen in the data.
    It is straightforward to add non-normal distribution functions to 
    \code{garchFit} specifying the argument \code{cond.dist}.  
    For this up to two additional parameters which can be either kept 
    fixed or estimated have to be introduced, the \code{skew} and/or 
    the \code{shape} parameter. 
    
    The argument \code{cond.dist} allows to select one from six conditional 
    distributions. Three of them are skewed distributions, (the skew-Normal, 
    the skew-Student, and the skew-GED), and four of them have an additional 
    shape parameter (the Student, the GED and their skewed versions). The 
    values for the skew and the shape are determined through the arguments 
    \code{skew} and \code{shape}. There are two additional undetermined
    arguments named \code{include.skew} and \code{include.shape}. They can 
    be set to \code{TRUE} or \code{FALSE}, then the distributional parameters 
    are included or kept fixed during parameter optimization, respectively. 
    If they are undetermined, then an automate selection is done depending 
    on the choice of the other parameters. The following example shows
    how to fit Bollerslev’s t-GARCH(1,1) model:
    
    \code{> garchFit(x = "dem2gbp", cond.dist = "dst")}

    Note, that the model has not to be specified explicitly, since the 
    GARCH(1,1) model is the default.\cr
    
            
    % ==========================================================================
    \bold{The Solvers:}
       
    The reason why the function garchFit() has implemented different 
    optimization algorithms has historical reasons. 
    
    When the first version 
    was written the only solver implemented in R was the AMC TOMS 778 
    algorithm \code{optim(method="L-BFGS-B")}. 
    
    Later when the function 
    \code{nlminb()} was added to R's base package, interfacing the PORT 
    Mathematical Software Library, we implemented it in \code{garchFit}. 
    Unfortunately, we found that for both solvers it was not always 
    possible to find the optimal values for the set of model parameters 
    even if we tried to adapt the control parameters. This problem was 
    solved in many cases using a two stage approach: First find a (near 
    optimal) solution with one of the two constrained algorithms,
    and then start with this solution a second optimization step with 
    the (unconstrained) simplex algorithm of Nelder-Mead, 
    \code{optim(method="Nelder-Mead")}. The result is that one finds with
    this "hybrid approach" or "mixed method" in many cases an improved 
    solution. The idea of two step approaches is also mentioned in 
    McCullough and Renfro [1999]. 
    
    The idea to implement the Sequential 
    Quadratic Programming algorithm, SQP, was inspired by the fact that 
    this algorithm is also part of the Ox, Doormik [1999], Matlab and Gauss,
    Schoenberg [1999], software packages. SQP based algorithms represent 
    very efficient nonlinear programming methods. SQP algorithms have been 
    implemented and tested, Schittkowski [1999], that outperform other 
    methods in terms of accuracy and efficiency over a large number of 
    test problems. Based on work of Biggs [1999], Han [1999], and 
    Powell [1999], the method allows to closely mimic constrained 
    optimization just as it is done for unconstrained optimization. An 
    overview of SQP methods can be found in Fletcher [1999], Gill et 
    al. [1999], Powell [1999], and Schittkowski [1999]. We have interfaced 
    to Rmetrics the SQP Fortran program written by Luksan [1999], which 
    is a recursive quadratic programming method with the BFGS variable 
    metric update for general nonlinear programming problems.
  
    The following example shows how to fit a normal GARCH(1,1) model 
    using different types of solvers:
    
    \code{> garchFit(x = "dem2gbp", algorithm = "nlminb+nm")}\cr
    \code{> garchFit(x = "dem2gbp", algorithm = "lbfgsb+nm")}\cr
    \code{> garchFit(x = "dem2gbp", algorithm = "sqp")}\cr
    
    
    % ==========================================================================
    \bold{Why does the function \code{garchFit} sometimes fails?}
    
    There might be several reasons why the GARCH modelling process can fail.
    
    First, in the most simple case, the \code{formula} argument is 
    misspecified, e.g. the left hand side of the formula is missing.
    
    Second, the GARCH model can be misspecified, so that the log likelihood
    function does not converge. In this case use another model expressed
    by the \code{formula} specification and/or choose another conditional 
    distribution function \code{cond.dist}. Pretest the unconditional
    returns, to find out what would be an appropriate unconditional
    distribution function for the GARCH model.
    
    Third, the log-likelihood function and the parameters are not properly
    scaled. In this case you should scale the objective function and the
    parameters using the \code{control=list()} specification. For details
    we refer to the descriptions of the selected solver. This happens often
    using logarithmic returns. A simple trick may help in most cases, use 
    percentual returns, i.e. multiply the tme series with a factor of 100.\cr
      
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
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   x = garchSim(model = spec@model, n = 500)
   head(x) 

## garchFit - 
   # fit = garchFit(~garch(1, 1), data = x)
   # print(fit)
   ## Interactive Plot:
   ## plot(fit)
   ## Batch Plot:
   # plot(fit, which = 3)
   # summary(fit)
}


\keyword{models}

