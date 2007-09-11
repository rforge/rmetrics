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
    shape = 4, cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
    include.shape = NULL, leverage = NULL, trace = TRUE,  
    algorithm = c("nlminb", "sqp", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    control = list(), title = NULL, description = NULL, \dots)
    
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

