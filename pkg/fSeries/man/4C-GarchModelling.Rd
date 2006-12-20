\name{GarchModelling}

\alias{GarchModelling}


\alias{garchSpec-class}
\alias{garchSpec}

\alias{show.garchSpec}
\alias{show,garchSpec-method}

\alias{garchSim}

\alias{fGARCH-class}
\alias{garchFit}

\alias{show.fGARCH}
\alias{show,fGARCH-method}

\alias{summary.fGARCH}
\alias{plot.fGARCH}
\alias{residuals.fGARCH}
\alias{fitted.fGARCH}
\alias{predict.fGARCH}

\alias{garchKappa}


\title{Univariate GARCH Time Series Modelling}


\description{

    A collection and description of functions to 
    simulate artificial ARCH time series processes, 
    to fit the parameters of univariate time series 
    to ARCH models, to perform a diagnostic analysis of 
    the fit, and to predict future values of the time 
    series.
    \cr
    
    The family of GARCH time series models includes the following 
    processes:
    
    \tabular{ll}{
    \code{garch} \tab generalized AR conditional heteroskedastic models, \cr
    \code{aparch} \tab asymmetric power GARCH models.}
    
    Included functions are:
    
    \tabular{ll}{
    \code{garchSpec} \tab Specifies a GARCH model, \cr
    \code{garchSim} \tab simulates a GARCH process, \cr
    \code{garchFit} \tab fits parameters of a GARCH model, \cr
    \code{garchKappa} \tab computes ARCH expectation value.}
    
    Included methods are:
    
    \tabular{ll}{
    \code{print} \tab Prints results from a GARCH fit, \cr
    \code{summary} \tab creates a summary report from a GARCH fit, \cr
    \code{plot} \tab creates diagnostic plots for a fitted GARCH process, \cr
    \code{residuals} \tab returns residuals for a GARCH model, \cr
    \code{fitted} \tab returns fitted values for a GARCH model, \cr
    \code{predict} \tab predicts mean and variance from a GARCH model.}

    \bold{Note:} This collection is still under a complete
    reconstruction. 

}


\usage{
garchSpec(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
    "rsged", "rsstd"), rseed = NULL)
    
show.garchSpec(object)

garchSim(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), n = 100, 
    n.start = 100, presample = NULL, cond.dist = c("rnorm", "rged", "rstd", 
    "rsnorm", "rsged", "rsstd"), rseed = NULL)
    
garchFit(formula, data, init.rec = c("mci", "uev"), delta = 2, skew = 1, 
    shape = 4, cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
    include.shape = NULL, leverage = NULL, trace = TRUE,  
    algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    control = list(), title = NULL, description = NULL, \dots)
    
garchKappa(cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
    gamma = 0, delta = 2, skew = NA, shape = NA)

show.fGARCH(object)

\method{plot}{fGARCH}(x, which = "ask", \dots)
\method{summary}{fGARCH}(object, \dots)
\method{residuals}{fGARCH}(object, \dots)
\method{fitted}{fGARCH}(object, \dots)
\method{predict}{fGARCH}(object, n.ahead = 10, trace = FALSE, \dots)
}


\arguments{

    \item{algorithm}{
        [garchFit] - \cr
        a string parameter that determines the algorithm used for maximum 
        likelihood estimation. Allowed values are \code{"sqp"}, 
        \code{"nlminb"}, and \code{"bfgs"} where the first is the default
        setting.
        }
    \item{cond.dist}{
        [garchSpec, garchSim, garchFit] - \cr
        a character string naming the desired conditional distribution.
        Valid values are \code{"dnorm"}, \code{"dged"}, \code{"dstd"}, 
        \code{"dsnorm"}, \code{"dsged"}, \code{"dsstd"}. The default value 
        is the normal distribution.
        }
    \item{control}{
        [garchFit] - \cr
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
        [garchFit][kappa] - \cr
        the exponent \code{delta} of the variance recursion. By default,
        this value will be fixed, otherwise the exponent will be estimated 
        together with the other model parameters if \code{include.delta=FALSE}.
        }
    \item{description}{ 
        [garchFit] - \cr
        a character string which allows for a brief description. 
        }
    \item{formula}{ 
        [garchFit] - \cr
        formula object describing the mean and variance equation of the 
        ARMA-GARCH/APARCH model. A pure GARCH(1,1) model is selected 
        when e.g. \code{formula=~garch(1,1)}. To specify for example an 
        ARMA(2,1)-APARCH(1,1) use \code{formula = ~arma(2,1)+apaarch(1,1)}.
        }
    \item{gamma}{
        [kappa] - \cr
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
        [garchFit] - \cr
        a character string indicating the method how to initialize the 
        mean and varaince recursion relation. 
        }
    \item{leverage}{
        a logical flag for APARCH models. Should the model be leveraged?
        By default \code{leverage=TRUE}.
        }
    \item{model}{
        [garchSpec, garchSim] - \cr
        List of GARCH model parameters: \cr
        \code{omega} - the constant coefficient of the variance equation; \cr
        \code{alpha} - the vector of autoregressive coefficients; \cr
        \code{beta} - the vector of variance coefficients; \cr
        Further Optional Values: \cr
        \code{mu} - the mean value; \cr
        \code{ar} - the autoregressive ARMA coefficients; \cr
        \code{ma} - the moving average ARMA coefficients; \cr
        \cr
        The default model is Bollerslev's GARCH(1,1) model.
        }
    \item{n}{
        [garchSim] - \cr
        length of output series, an integer value. An integer value,
        by default \code{n=100}.
        }
    \item{n.ahead}{
        [predict] - \cr 
        number of steps to be forecasted, an integer value, by default 10.
        }
    \item{n.start}{
        [garchSim] - \cr
        length of "burn-in" period, by default 100.
        }
    \item{object}{
        [predict][summary] - \cr
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }   
    \item{presample}{
        a numeric three column matrix with start values for the series, 
        innovations, and conditional variances. For an ARMA(m,n)-GARCH(p,q)
        process the number of rows must be at least max(m,n,p,q), longer 
        presamples are cutted.
        }
    \item{rseed}{
        [garchSpec][garchSpec] - \cr
        single integer argument, the seed for the intitialization of
        the random number generator for the innovations.
        }
    \item{skew, shape}{
        [garchFit][kappa] - \cr
        skewness and shape parameter of the conditional distribution.
        }
    \item{title}{
        [garchFit][predict] - \cr
        a character string which allows for a project title.
        } 
    \item{trace}{
        [garchFit] - \cr
        a logical flag. Should the optimization process of fitting the
        model parameters be printed? By default \code{trace=TRUE}.\cr
        [predict] - \cr
        a logical flag. Should the prediction process be printed? By 
        default \code{trace=FALSE}.
        }  
    \item{which}{
        [plot] - \cr
        if \code{which} is set to \code{"ask"} the function will 
        interactively ask which plot should be displayed. This is
        the default value and then those plots will
        be displayed for which the elements in the logical vector
        \code{which} ar set to \code{TRUE}; by default all four
        elements are set to \code{"all"}.
        }
    \item{x}{
        [print][plot]  - \cr
        either an object of class \code{garchSpec} for printing specification
        structures, or an object of class \code{fGARCH} for printing fitted
        GARCH/APARCH models or plotting results from the diagnostic analysis
        of fitted models.
        } 
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\details{
  
    \bold{Parameter Estimation:}
    \cr\cr
    \code{garchFit} uses the \code{nlminb()} optimizer to find the maximum
    likelihood estimates.
    
    \bold{Residuals Methods:}
    \cr\cr
    The method for the computation of the residuals may have an additional
    argument named \code{standardize} which allows to standardize the 
    residuals by the conditional variances.
     
}


\note{

    \bold{What's next?} 
    \cr\cr
    The software will be extended to further GARCH models including 
    integrated GARCH models, EGARCH models, GARCH-in-Mean models as 
    well as fractionally integrated GARCH models. In the next version 
    also the fixing of individual ARMA-GARCH/APARCH coefficients will 
    become available. Furthermore, we will provide additional conditional 
    distribution functions including for example members from the family 
    of the hyperbolic distribution. Concerning performance analysis we 
    plan to offer in addition several performance measures for the 
    residuals like Theil's inequality coefficient, and Mincer and 
    Zarnowitz's R2 coefficient. Concerning hypothesis testing we will 
    add some specific tests including Engle's LM ARCH test to test 
    the presence of ARCH effects, Engle and Ng's test to investigate 
    possible misspecifications of the conditional variance equation, 
    and Nyblom's test to check the constancy of model parameters over 
    time.
    
}


\value{

    \code{garchSpec}\cr
    \cr
    returns a S4 object of class \code{fGARCH} with the following slots:
    
    \item{@call}{
        the call of the \code{garch} function.
        }
    \item{@formula}{
        a list with two formula entries for the mean and variance 
        equation.
        }
    \item{@model}{
        a list with the model parameters.
        }   
    \item{@presample}{
        a numeric matrix with presample values.
        }
    \item{@distribution}{
        a character string with the name of the conditional distribution.
        }
    
        
    \code{garchSim}\cr
    \cr
    returns an objects of class \code{ts} atrributed with an appropriate
    specification structure. The name of the attribute is \code{"spec"}. 

    
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
    \item{@fitted.values}{
        a numeric vector with the fitted values.
        }
    \item{@cvariances}{
        a numeric vector with the conditional variances.
        }
    \item{@title}{
        a title string.
        }
    \item{@description}{
        a string with a brief description.
        }
        
    The entries of the @fit slot in the case of the algorithms 
    \code{"nlminb"} and \code{"bfgs"} show the results from the
    second optimization step achieved through the Nelder-Mead 
    Algorithm:\cr
    
    \item{$par}{
        The best set of parameters found.
        }
    \item{$value}{
        The value of the max log likelihood function corresponding to 
        the optimal values of \code{$par}.
        }
    \item{$counts}{
        A two-element integer vector giving the number of calls to
        'fn' and 'gr' respectively. This excludes those calls needed
        to compute the Hessian, if requested, and any calls to 'fn'
        to compute a finite-difference approximation to the gradient.
        }
    \item{$convergence}{
        An integer code. '0' indicates successful convergence. Values 
        different from zero indicate an error code. For details, please 
        consult the help page of \code{optim}.
        }
    \item{message} {
        A character string giving any additional information returned
        by the optimizer, or \code{NULL}.
        }
    \item{hessian}{ 
        Only if control list argument \code{hessian} is true. A 
        symmetric matrix giving an estimate of the Hessian at the 
        solution found. Note that this is the Hessian of the 
        unconstrained problem even if the box constraints are active.
        }
        
    The entries of the @fit slot in the case of the sequencial
    quadratic programming algorithm \code{"sqp"} are:\cr
    
    \item{$par}{
        The best set of parameters found.
        }
    \item{$value}{
        The value of the max log likelihood function corresponding to 
        the optimal values of \code{$par}.
        }
        
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
    Ladislav Luksan for the underlying SQP Routine, \cr
    Zhu, Byrd, Lu-Chen and Nocedal for the underlying L-BFGS-B Routine.
    
}


\examples{
## SOURCE("fSeries1.4C-GarchModelling")

    # garchSpec -
    # garchSim - 
    # garchFit -

    # For examples we refer to: demo/xmpDWChapter34.R ...
    
}


\keyword{models}

