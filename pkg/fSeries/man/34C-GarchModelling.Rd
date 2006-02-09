\name{GarchModelling}

\alias{GarchModelling}


\alias{garchSpec-class}
\alias{garchSpec}
\alias{print.garchSpec}

\alias{garchSim}

\alias{fGARCH-class}
\alias{garchFit}
\alias{print.fGARCH}
\alias{plot.fGARCH}
\alias{summary.fGARCH}



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
    \code{aparch} \tab asymmetretic power ARCH models. }

    \bold{Note:} This collection is still under a complete
    reconstruction. 

}


\usage{
garchSpec(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
	presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
	"rsged", "rsstd"))
\method{print}{garchSpec}(x, \dots)

garchSim(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), n = 100, 
	presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
	"rsged", "rsstd"))

garchFit(formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
	series = x, presample =  NULL, 
	cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
	symmetric = TRUE, trace = TRUE, title = NULL, description = NULL, ...)
\method{print}{fGARCH}(x, \dots)
\method{plot}{fGARCH}(x, \dots)
\method{summary}{fGARCH}(object, \dots)
}


% Implemented but not yet tested:
%	init.mean = NULL, init.var = NULL, init.delta = NULL, init.dist = NULL,
%	fix.mean = NULL, fix.var = NULL, fix.delta = NULL, fix.dist = NULL,
%	h.start = NULL, llh.start = NULL,


\arguments{

    \item{cond.dist}{
    	[garchSpec, garchSim, garchFit] - \cr
      	a character string naming the desired conditional distribution.
      	Valid values are \code{"dnorm"}, \code{"dged"}, \code{"dstd"}, 
      	\code{"dsnorm"}, \code{"dsged"}, \code{"dsstd"}. The default value 
      	is the normal distribution.
        }
    \item{description}{ 
        [garchFit] - \cr
        a character string which allows for a brief description. 
        }
    %\item{fix.mean, fix.var, fix.delta, fix.dist}{
    %    Should parameters be fixed ?
    %    }
    \item{formula.mean, formula.var}{ 
        [garchFit] - \cr
        two formula objects describing the mean and variance equation
        of the ARMA-GARCH/APARCH model. By default a pure GARCH(1,1)
        mode is selected, this means: \code{formula.mean=~arma(0,0)}, 
        and \code{formula.var=~garch(1,1)}. To specify for example an 
        APARCH(1,1) use: \code{formula.var=~apaarch(1,1)}
        }
    %\item{h.start, llh.start}{
    %	Conditioning upon start.
    %	}
    %\item{init.mean, init.var, init.delta, init.dist}{
    %    Individual start values.
    %    }
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
    \item{object}{
    	[summary] - \cr
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }   
    \item{presample}{
        a numeric three column matrix with start values for the series, 
        innovations, and conditional variances. For an ARMA(m,n)-GARCH(p,q)
        process the number of rows must be at least max(m,n,p,q), longer 
        presamples are cutted.
        }
    \item{series}{
        [garchFit] - \cr
        a numeric vector or univariate \code{timeSeries} object to be 
        fitted. By default \code{series=x}.
        }
    \item{symmetric}{
      	a logical flag for APARCH models. Should the model be leveraged?
      	By default \code{symmetric=TRUE}.
        }
    \item{title}{
        [garchFit] - \cr
        a character string which allows for a project title.
        } 
    \item{trace}{
    	[garchFit] - \cr
        a logical flag. Should the optimization process of fitting the
        model parameters be printed. By default \code{trace=TRUE}.
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
        a list with two formula entries for the mean and variance 
        equation.
        }
    \item{@method}{
        a string denoting the optimization method.
        }   
    \item{@fit}{
        a list with the results from the parameter estimation.
        }
    \item{@residuals}{
        a numeric vector with the residual values.
        }
    \item{@fitted.values}{
        a numeric vector with the fitted values.
        }
    \item{@title}{
        a title string.
        }
    \item{@description}{
        a string with a brief description.
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

Engle R.F. (1982);
    \emph{Autoregressive Conditional Heteroscedasticity with Estimates 
    of the Variance of United Kingdom Inflation},
    Econometrica 50, 987--1008.

    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port. 
     
}


\examples{
## SOURCE("fSeries1.34C-GarchModelling")
\dontrun{
#	garchSpec -
#   garchSim - 
#	garchFit -

    # For examples we refer to: demo/xmpDWChapter34.R ...
}
}


\keyword{models}

