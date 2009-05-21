\name{garchFit}


\alias{garchFit}
\alias{garchKappa}


\title{Univariate GARCH Time Series Fitting}


\description{

    Estimates the parameters of an univariate ARMA-GARCH/APARCH process.
}


\usage{    
garchFit(formula ~ garch(1, 1), data = dem2gbp, 
    init.rec = c("mci", "uev"), 
    delta = 2, skew = 1, shape = 4, 
    cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", 
        "snig", "QMLE"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL, 
        include.shape = NULL, leverage = NULL, trace = TRUE, 
    recursuin = c("internal", "filter", "testing"),
    algorithm = c("nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    hessian = c("ropt", "rcd"), control = list(), 
    title = NULL, description = NULL, \dots)
    
garchKappa(cond.dist = c("norm", "ged", "std", "snorm", "sged", "sstd",
    "snig"), gamma = 0, delta = 2, skew = NA, shape = NA)
}


\arguments{

    \item{recursion}{
        a string parameter that determines the recursion used for calculating 
        the maximum log-likelihood function.
        % Allowed values are ...
    }
    \item{algorithm}{
        a string parameter that determines the algorithm used for maximum 
        likelihood estimation.
        % Allowed values are 
        %   \code{"nmfb"}, 
        %   \code{"sqp"}, 
        %   \code{"nlminb"}, and 
        %   \code{"bfgs"} where the third is the default
        % setting. \code{"mnfb"} is a fully Fortran implemented and extremely
        % fast version of the R-coded \code{"nlminb"} algorithm. 
    }
    \item{cond.dist}{
        a character string naming the desired conditional distribution.
        Valid values are \code{"dnorm"}, \code{"dged"}, \code{"dstd"}, 
        \code{"dsnorm"}, \code{"dsged"}, \code{"dsstd"} and
        \code{"QMLE"}. The default value is the normal distribution.  See
        Details for more information.
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
    % \item{hessian}{
        % a string denoting how the Hessian matrix should be evaluated,
        % either \code{hessian ="cda"}, the default, or \code{"fda"}.
        % The forward difference approximation, \code{"fda"} consumes less 
        % computer time, but is usually not as precise as approximations 
        % that use central difference approximation, \code{"cda"}, formula.
        % }
    \item{hessian}{
        a string denoting how the Hessian matrix should be evaluated,
        either \code{hessian ="rcd"}, or \code{"ropt"}, the default,
        \code{"rcd"}  is a central difference approximation implemented
        in R and \code{"ropt"} use the internal R function \code{optimhess}.
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
        a numeric value, the skewness parameter of the conditional 
        distribution.
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
  
    \code{garchFit} \cr
    \cr
    returns a S4 object of class \code{"fGARCH"} with the following slots: 
  
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
        a numeric vector with the (raw, unstandardized) residual values.
    }
    \item{@fitted}{
        a numeric vector with the fitted values.
    }
    \item{@h.t}{
        a numeric vector with the conditional variances
        (\eqn{h_t = \sigma_t^\delta}{h.t = sigma.t^delta}). 
    }
    \item{@sigma.t}{
        a numeric vector with the conditional standard deviation.
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
  
    \code{"QMLE"} stands for Quasi-Maximum Likelihood Estimation, which
    assumes normal distribution and uses robust standard errors for
    inference. Bollerslev and Wooldridge (1992) proved that if the mean
    and the volatility equations are correctly specified, the QML
    estimates are consistent and asymptotically normally
    distributed. However, the estimates are not efficient and \dQuote{the
    efficiency loss can be marked under asymmetric ... distributions}
    (Bollerslev and Wooldridge (1992), p. 166). The robust
    variance-covariance matrix of the estimates equals the (Eicker-White)
    sandwich estimator, i.e.
    
    \deqn{V = H^{-1} G^{\prime} G H^{-1},}{V = H^(-1) G' G H^(-1),}
    
    where \eqn{V}{V} denotes the variance-covariance matrix, \eqn{H}{H}
    stands for the Hessian and \eqn{G}{G} represents the matrix of
    contributions to the gradient, the elements of which are defined as
    
    \deqn{G_{t,i} = \frac{\partial l_{t}}{\partial \zeta_{i}},}{%
    G_{t,i} = derivative of l_{t} w.r.t. zeta_{i},}
    
    where \eqn{t_{t}}{l_{t}} is the log likelihood of the t-th observation
    and \eqn{\zeta_{i}}{zeta_{i}} is the i-th estimated parameter. See
    sections 10.3 and 10.4 in Davidson and MacKinnon (2004) for a more
    detailed description of the robust variance-covariance matrix.
  
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
    
    Bollerslev T., Wooldridge J.M. (1992);
    \emph{Quasi-Maximum Likelihood Estimation and Inference in Dynamic
    Models with Time-Varying Covariance},
    Econometric Reviews 11, 143--172.
    
    Byrd R.H., Lu P., Nocedal J., Zhu C. (1995);
    \emph{A Limited Memory Algorithm for Bound Constrained Optimization},
    SIAM Journal of Scientific Computing 16, 1190--1208.
    
    Davidson R., MacKinnon J.G. (2004);
    \emph{Econometric Theory and Methods},
    Oxford University Press, New York.
    
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
   x.vec = as.vector(garchSim(garchSpec(rseed = 1985), n = N)[,1])
   garchFit(~ garch(1,1), data = x.vec, trace = FALSE)

   # An univariate timeSeries object with dummy dates:
   x.timeSeries = dummyDailySeries(matrix(x.vec), units = "GARCH11")
   garchFit(~ garch(1,1), data = x.timeSeries, trace = FALSE)         

\dontrun{
   # An univariate zoo object:
   x.zoo = zoo(as.vector(x.vec), order.by = as.Date(rownames(x.timeSeries)))
   garchFit(~ garch(1,1), data = x.zoo, trace = FALSE) 
}

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

\dontrun{    
   # A multivariate zoo object:
   X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.timeSeries)))
   garchFit(GARCH11 ~ garch(1,1), data = X.zoo)
}

   # A multivariate "mts" object:
   X.mts = as.ts(X.mat)
   garchFit(GARCH11 ~ garch(1,1), data = X.mts)
 
## MODELING THE PERCENTUAL SPI/SBI SPREAD FROM LPP BENCHMARK:
    
   X.timeSeries = as.timeSeries(data(LPP2005REC))
   X.mat = as.matrix(x.timeSeries)
   \dontrun{X.zoo = zoo(X.mat, order.by = as.Date(rownames(X.mat)))}
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

