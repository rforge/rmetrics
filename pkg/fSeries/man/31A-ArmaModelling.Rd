\name{ArmaModelling}

\alias{ArmaModelling}

\alias{fARMA}
\alias{fARMA-class}

\alias{armaSim}
\alias{armaFit}

\alias{predict.fARMA}

\alias{print.fARMA}
\alias{plot.fARMA}
\alias{summary.fARMA}

\alias{fitted.fARMA}
\alias{residuals.fARMA}

\alias{armaRoots}
\alias{armaTrueacf}
\alias{armaToeplitz}
\alias{armaFischer}


\title{Integrated ARMA Time Series Modelling}


\description{

	A collection and description of simple to 
	use functions to model univariate autoregressive 
	moving average time series processes, including 
	time series simulation, parameter estimation, 
	diagnostic analysis of the fit, and predictions 
	of future values.
	\cr
		
	The functions are:
	
	\tabular{ll}{
	\code{armaSim} \tab Simulates an artificial ARMA time series process, \cr
	\code{armaFit} \tab Fits the parameters of an ARMA time series process,\cr
	\code{predict} \tab Forecasts an ARMA time series process, \cr
	\code{print} \tab S3 Print Method, \cr
	\code{plot} \tab S3 Plot Method, \cr
	\code{summary} \tab S3 Summary Method, \cr
	\code{fitted} \tab S3 Method, returns fitted values, \cr
	\code{residuals} \tab S3 Method, returns residuals. }
	
	The function \code{armaFit} allows for the following formula arguments:
	
	\tabular{ll}{
	\code{x ~ ar()} \tab autoregressive time series processes, \cr
	\code{x ~ arma()} \tab autoregressive moving average processes,\cr
	\code{x ~ arima()} \tab autoregressive integrated moving average processes, and \cr
	\code{x ~ fracdiff()} \tab fractionally integrated ARMA processes. }
	
	For the first and third selection \code{x ~ ar()} or \code{x ~ arima()}
	the function uses the AR and ARIMA modelling as implemented in 
	\R's \code{ts} package, for the second selection \code{x~arma()} 
	the function uses the ARMA modelling implemented in \R's 
	contributed \code{tseries} package, and for the last selection 
	\code{x~fracdiff()} the function uses fractional ARIMA modelling 
	from \R's contributed \code{fracdiff} package. Note that AR, MA,
  	and ARMA processes can all be modelled by the same algorithm
  	specifying the formula \code{x~arima(p,d,q)} in the proper way, 
  	i.e. setting \code{d=0} and choosing the orders of \code{p} and 
  	\code{q} as zero in agreement with the desired model specification.
  	
  	Alternatively, one can use the functions from R's \code{"stats"}
  	package:
  	
  	\tabular{ll}{
	\code{arima.sim} \tab Simulates from an ARIMA time series model, \cr
	\code{ar, arima, arima0} \tab Fits an AR, ARIMA model to a univariate time series,\cr
	\code{predict.ARIMA} \tab Forecast from models fitted by "arima", \cr
	\code{tsdiag} \tab Plots time-series diagnostics. }
	
	In addition there are two functions for the analysis of the true  
    ARMA process. The first investigates the characteristic polynomial
    and the second the true autocorrelation function:
		
	\tabular{ll}{
	\code{armaRoots} \tab Roots of the characteristic ARMA polynomial, \cr
	\code{armaTrueacf} \tab True autocorrelation function of an ARMA process, \cr
	\code{armaToeplitz} \tab True Toeplitz matrix of a vector, \cr
	\code{armaFischer} \tab Fischer Information Matrix of an ARMA Process. }
 	
}
	
	
\note{
	
	There is nothing really new in this package. The benefit you will 
	get with this collection is, that all functions have a common 
	argument list with a formula to specify the model and presetted 
	arguments for the specification of the algorithmic method. For 
	users who have already modeled GARCH processes with SPlus, this 
	approach will be quite natural.
	
	On the other hand, the user can still use the underlying functions 
	from the \code{ts}, \code{tseries}, or \code{fracdiff} packages. No 
	function from these packages is masked or overwritten.
	
	The output of the \code{predict}, \code{print} and \code{summary}
	methods have all the same style of format with some additional 
	algorithm specific printing. This makes it easier to interpret 
	the results obtained from different algorithms implemented in 
	different functions.
	
}


\usage{
armaSim(model = list(ar = c(0.5, -0.5), d = 0, ma = 0.1), n = 100, 
	innov = NULL, n.start = 100, start.innov = NULL, rand.gen = rnorm, 
	rseed = NULL, \dots) 

armaFit(formula = x ~ arima(2, 0, 1), 
	method = c("CSS-ML", "ML", "CSS", "yw", "burg1", "burg2", "ols", "mle"), 
	include.mean = TRUE, fixed = NULL, fracdiff.M = 100, fracdiff.h = -1, 
	title = NULL, description = NULL, \dots)

\method{predict}{fARMA}(object, n.ahead = 10, n.back = 50, conf = c(80, 95), 
	doplot = TRUE, doprint = TRUE, \dots)
	
\method{print}{fARMA}(x, \dots)
\method{plot}{fARMA}(x, gof.lag = 10, reference.grid = TRUE, 
	col = "steelblue", \dots)
\method{summary}{fARMA}(object, doplot = TRUE, \dots)

\method{fitted}{fARMA}(object, \dots)
\method{residuals}{fARMA}(object, \dots)

armaRoots(coefficients, n.plot = 400, digits = 4, \dots) 
armaTrueacf(model, lag.max = 20, type = "correlation", doplot = TRUE) 
armaToeplitz(x)
armaFischer(model = list(ar = c(0.5, -0.5), ma = 0.1))
}


\arguments{
 
	\item{coefficients}{	
	    [armaRoots] - \cr
		a numeric vector with the coefficients of the characterisitic 
		polynomial.
		}
	\item{col, reference.grid}{
		[plot] - \cr
		a character string to set the plot color, by default 
		\code{"steelblue4"}; and a logical flag, by default \code{TRUE},
		to draw a reference grid on the plot or not.
		} 
	\item{description}{
        a character string which allows for a brief description.
        }
	\item{digits}{	
		[armaRoots] - \cr
		output precision, an integer value.
		}
	\item{doplot}{
		[armaRoots] - \cr
		a logical. Should a plot be displayed?
		\cr
		[predict][summary] - \cr
	  	is used by the \code{predict} and \code{summary} methods. By default,
	  	this value is set to \code{TRUE} and thus the function calls generate 
	  	beside written also graphical printout.
	    Additional arguments required by underlying functions have 
	    to be passed through the \code{dots} argument.
	  	}	
	\item{doprint}{
		[predict] - \cr
		should intermediate results be printed? Arguments used by the 
		\code{predict} method.
		}
	\item{fixed}{
		[armaFit] - \cr
	    is an optional numeric vector of the same length as the total
	    number of parameters. If supplied, only \code{NA} entries in
	    \code{fixed} will be varied. In this way subset ARMA processes
	    can be modeled. ARIMA modelling supports this option. Thus
	    for estimating parameters of subset ARMA and AR models the 
	    most easiest way is to specify them by the formulas 
	    \code{x~ARIMA(p, 0, q)} and \code{x~ARIMA(p, 0, 0)}, respectively.
	    }
	 \item{formula}{
	 	[armaFit] - \cr
	  	a formula specifying the general structure of the ARMA form. 
	  	Can have one of the forms  
	  	\code{x ~ ar(q)}, 
	  	\code{x ~ arma(p, q)}, 
	  	\code{x ~ arima(p, d, q)}, or
	  	\code{x ~ fracdiff(p, q)}.
	  	\code{x} is the response variable and must appear in the 
	  	formula expression.
	  	In the first case R's function \code{ar} from the \code{ts}
	  	package will be used to estimate the parameters, in the second
	  	case R's function \code{arma} from the \code{tseries} package
	  	will be used, in the third case R's function \code{arima} from 
	  	the \code{ts} package will be used, and in the last case R's
	  	function \code{fracdiff} from the \code{fracdiff} package will
	  	be used. The state space modelling based \code{arima} function
	  	allows also to fit ARMA models using \code{arima(p, d=0, q)}, and
	  	AR models using \code{arima(q, d=0, q=0)}, or pure MA models
	  	using \code{arima(q=0, d=0, p)}. (Exogenous variables are also 
	  	allowed and can be passed through the \code{\dots} argument.)
	  	}
	\item{fracdiff.M, fracdiff.h}{
		two numeric parameter values for the \code{"fracdiff"} 
		estimator. \code{M} is the number of terms in the likelihood 
		approximation. \code{h} is the size of the finite difference 
		interval for numerical derivatives. By default, or if negative, 
		\code{h=min(0.1,eps.5*(1+ abs(cllf)))}, where 
		\code{clff:=log.max.likelihood}, as returned, and 
		\code{eps.5:=sqrt(.Machine$double.neg.eps)}, typically 
		\code{1.05e-8}.
		} 
	\item{gof.lag}{
		[print][plot][summary][predict] - \cr
    	the maximum number of lags for a goodness-of-fit test.
    	}
    \item{include.mean}{
    	[armaFit] - \cr
	  	Should the ARIMA model include a mean term? The default is 
	    \code{TRUE}, note that for differenced series a mean would 
	    not affect the fit nor predictions.
	    }
	\item{innov}{
		[armaSim] - \cr
		is a univariate time series or vector of innovations to produce 
		the series. If not provided, \code{innov} will be generated using 
		the random number generator specified by \code{rand.gen}. 
		Missing values are not allowed. By default the normal
		random number generator will be used.
		}
	\item{lag.max}{
		[armaTrueacf] - \cr
		maximum number of lags at which to calculate the acf or 
		pacf, an integer value by default 20.
		}
	\item{method}{
		[armaFit] - \cr
	    denotes the method used to fit the model by a character string.
	    \code{method} must be one of the strings in the default argument.
	    For ARIMA models a valid selection is one of \code{"CSS-ML"}, 
	    \code{"ML"}, \code{"CSS"}, 
	    for ARMA models only one valid selection is possible \code{"CSS"},
	    for AR models a valid selection is one of \code{"yw"}, \code{"burg1"}, 
	    \code{"burg2"}, \code{"ols"}, \code{"mle"}, and 
	    for FRACDIFF models only one valid selection is possible 
	    \code{"FRACDIFF"}. If no \code{method} argument specified the
	    following default settings are used: \code{"CSS-ML"} for ARIMA,
	    \code{"FRACDIFF"} for FRACDIFF, \code{"CSS"} for ARMA, and
	    \code{"yw"} for AR modelling. 
	    \cr
	    Additional arguments required by the optimization algorithm have 
	    to be passed through the \code{dots} argument.
    	} 
	\item{model}{
    	[armaSim] - \cr
		a list with one (AR), two (ARMA) or three (ARIMA, FRACDIFF) 
		elements . \code{ar} is a numeric vector giving the AR coefficients, 
		\code{d} is an integer value giving the degree of differencing,
		and \code{ma} is a numeric vector giving the MA coefficients.
		Thus the order of the time series process is (F)ARIMA(p, d, q)
		with \code{p=length(ar)} and \code{q=length(ma)}. \code{d} is
		a positive integer for ARIMA models and a numeric value for
		FRACDIFF models. By default an ARIMA(2, 0, 1) model with 
		coefficients \code{ar=c(0.5, -0.5)} and \code{ma=0.1} will be 
		generated.
		\cr
		[armaTrueacf][armaFischer] - \cr
		a specification of the ARMA model with two elements:
		\code{model$ar} is the vector of the AR coefficients, and 
		\code{model$ma} is the vector of the MA coefficients.
		}
   	\item{n}{
   		[armaSim] - \cr
	    is the length of the series to be simulated (optional 
	    if \code{innov} is provided). The default value is 100.
	    }
	\item{n.ahead, n.back, conf}{[print][plot][summary][predict] - \cr
		are presetted arguments for the \code{predict} method. \code{n.ahead}
		determines how far ahead forecasts should be evaluated together
		with errors on the confidence intervals given by the argument
		\code{conf}. If a forecast plot is desired, which is the
		default and expressed by \code{doplot=TRUE}, then \code{n.back} 
		sets the number of time steps back displayed in the graph.
		}
	\item{n.plot}{	
		[armaRoots] - \cr
		the number of data point to plot the unit circle;
		an integer value.
		}
	\item{n.start}{
		[armaSim] - \cr
	    gives the number of start-up values discarded when simulating 
	    non-stationary models. The start-up innovations will be generated 
	    by \code{rand.gen} if \code{start.innov} is not provided.
	    } 
  	\item{object}{
  		[summary][predict] - \cr
	    is an object of class \code{fARMA} returned by the fitting function 
	    \code{armaFit} and serves as input for the \code{summary}, and 
	    \code{predict} methods. Some methods allow for additional 
	    arguments.
	    }
	\item{rand.gen}{
		[armaSim] - \cr
	    is the function which is called to generate the innovations. 
	    Usually, \code{rand.gen} will be a random number generator. 
	    Additional arguments required by the random number generator 
	    \code{rand.gen}, usually the location, scale and/or shape 
	    parameter of the underlying distribution function, have to be 
	    passed through the \code{dots} argument.
	    }
	\item{rseed}{
   		[armaSim] - \cr
	    the random number seed, by default NULL. If this argument is
	    set to an integervalue, then the function \code{set.seed(rseed)}
	    will be called.
	    }
	\item{start.innov}{
		[armaSim] - \cr
	    is a univariate time series or vector of innovations to be used 
	    as start up values. Missing values are not allowed.
	    }
	\item{title}{
        a character string which allows for a project title.
        }
    \item{type}{
		[armaTrueacf] - \cr
		a character string, "correlation" to compute the true 
		autocorrelation function, "partial" to compute the true
		partial autocorrelation function, or "both" if both
		functions are desired. The start of one of the strings 
		will suffice.
		} 
	\item{x}{
  		[print][plot] - \cr
	    is an object of class \code{fARMA} returned by the
	    fitting function \code{armaFit} and serves as input for the 
	    \code{predict}, \code{print}, \code{print.summary},
	    and \code{plot} methods. Some methods allow for additional 
	    arguments. \cr
	    [armaToeplitz] - \cr
		a vector of autocovariances. The returned Toeplitz matrix is  
    	the corresponding covariance matrix of the observatons.
	    }
	\item{\dots}{
	    additional arguments to be passed.
	    }
	    
}


\values{

	\code{armaFit} 
  	\cr
  	returns an S4 object of class \code{"fARMA"}, with the folliwing 
  	slots:

  	\item{call}{
    	the matched function call.
    	}
  	\item{data}{
  		the input data in form of a data.frame.
  		}
  	\item{description}{
  		allows for a brief project description.
  		}
  	\item{fit}{
  		the results as a list returned from the underlying
  		time series model function. 
  		}
  	\item{method}{
    	the selected time series model naming the applied method.
		}
  	\item{formula}{
    	the formula expression describing the model.
    	}
    \item{parameters}{
    	named parameters or coefficients of the fitted model.
    	}
    \item{title}{
    	a title string.
    	}
    	
    \code{armaRoots} 
	\cr
	returns a three column data frame with the real, the imaginary part 
	and the radius of the roots. The number of rows corresponds
	to the coefficients.
	
	\code{armaTrueacf}
	\cr
	returns a two column data frame with the lag and the correlation 
	function. 
	
	\code{armaToeplitz}
	\cr
	returns the Toeplitz matrix of a given vector \code{x}. The 
	Toeplitz matrix is a square matrix of order length(x) and with 
	\code{[i,j]} entries given by \code{x[abs(i-j)]}. Note, if 
	\code{x} is a vector of autocovariances, the Toeplitz matrix is  
    the corresponding covariance matrix of the observatons.
    
    \code{armaFischer}
    \cr
    returns the Fischer information matrix for an ARMA time series
    process.

}
    	    	

\details{
  
	\bold{AR - Auto-Regressive Modelling:}
	\cr\cr
	The argument \code{x ~ar()} calls the underlying functions 
	\code{ar.yw}, \code{ar.burg1}, \code{ar.burg2}, code{\link{ar.ols}} 
	or \code{ar.mle}. 
	For definiteness, the AR models are defined through

  		\deqn{x_t - \mu = a_1(x_{t-1} - \mu) + \cdots +  a_p(x_{t-p} - \mu) + e_t}{\code{(x[t] - m) = a[1]*(x[t-1] - m) + \dots +  a[p]*(x[t-p] - m) + e[t]}}

  	Order selection can be achieved through the comparison of AIC
  	values for different model specifications. However this may be
    problematic, as of the methods here only \code{ar.mle} performs
    true maximum likelihood estimation. The AIC is computed as if 
    the variance estimate were the MLE, omitting the determinant 
    term from the likelihood. Note that this is not the same as the 
    Gaussian likelihood evaluated at the estimated parameter values. 
    With \code{method="yw"} the variance matrix of the innovations is
    computed from the fitted coefficients and the autocovariance of
    \code{x}. Burg's method allows for two alternatives 
    \code{method="burg1"} or \code{method="burg2"} to estimate the
    innovations variance and hence AIC. Method 1 is to use the update 
    given by the Levinson-Durbin recursion (Brockwell and Davis, 1991), 
    and follows S-PLUS. Method 2 is the mean of the sum of squares of 
    the forward and backward prediction errors (as in Brockwell and Davis, 
    1996). Percival and Walden (1998) discuss both. 
    \cr
    \code{[ts:ar]}
    \cr
    
    
    \bold{ARMA - Auto-Regressive Moving-Average Modelling:}
    \cr\cr   
	The argument \code{x ~ arma(p,q)} maps the call to the
	argument \code{x ~ arima(p, 0, q)}.
	\cr
	
  	 	
 	\bold{ARIMA - Integrated ARMA Modelling:}
 	\cr\cr
    The argument \code{x ~ arima()} calls the underlying function 
    \code{arima} from \R's \code{ts} package. For definiteness, the AR 
    models are defined through

  		\deqn{x_t = a_1x_{t-1} + \cdots + a_px_{t-p} + e_t + b_1e_{t-1} + \dots + b_qe_{t-q}}{\code{x[t] = a[1]x[t-1] + \dots + a[p]x[t-p] + e[t] + b[1]e[t-1] + \dots + b[q]e[t-q]}}

  	and so the MA coefficients differ in sign from those of
  	S-PLUS. Further, if \code{include.mean} is \code{TRUE}, this formula
    applies to \eqn{x-m} rather than \eqn{x}.  For ARIMA models with
    differencing, the differenced series follows a zero-mean ARMA model.
  	\cr
  	The variance matrix of the estimates is found from the Hessian of
  	the log-likelihood, and so may only be a rough guide.
 	\cr
 	Optimization is done by \code{\link{optim}}. It will work
  	best if the columns in \code{xreg} are roughly scaled to zero mean
  	and unit variance, but does attempt to estimate suitable scalings.
   	The exact likelihood is computed via a state-space representation 
   	of the ARIMA process, and the innovations and their variance found 
   	by a Kalman filter. The initialization of the differenced ARMA 
   	process uses stationarity. For a differenced process the 
   	non-stationary components are given a diffuse prior (controlled 
   	by \code{kappa}). Observations which are still controlled by the 
   	diffuse prior (determined by having a Kalman gain of at least 
   	\code{1e4}) are excluded from the likelihood calculations. (This 
   	gives comparable results to \code{\link{arima0}} in the absence
    of missing values, when the observations excluded are precisely those
    dropped by the differencing.)
  	\cr
    Missing values are allowed, and are handled exactly in method \code{"ML"}.
	\cr
    If \code{transform.pars} is true, the optimization is done using an
    alternative parametrization which is a variation on that suggested by
    Jones (1980) and ensures that the model is stationary.  For an AR(p)
    model the parametrization is via the inverse tanh of the partial
    autocorrelations: the same procedure is applied (separately) to the
    AR and seasonal AR terms.  The MA terms are not constrained to be
    invertible during optimization, but they will be converted to
    invertible form after optimization if \code{transform.pars} is true.
    \cr
  	Conditional sum-of-squares is provided mainly for expositional
  	purposes.  This computes the sum of squares of the fitted innovations
  	from observation \code{n.cond} on, (where \code{n.cond} is at least
  	the maximum lag of an AR term), treating all earlier innovations to
  	be zero.  Argument \code{n.cond} can be used to allow comparability
  	between different fits.  The ``part log-likelihood'' is the first
  	term, half the log of the estimated mean square.  Missing values
  	are allowed, but will cause many of the innovations to be missing.
	\cr
  	When regressors are specified, they are orthogonalized prior to
  	fitting unless any of the coefficients is fixed. It can be helpful to
  	roughly scale the regressors to zero mean and unit variance.
  	\cr
    Note from \code{arima}: The functions parse their arguments to the 
    original time series functions available in \R's time series library 
    \code{ts}. 
	\cr
  	The results are likely to be different from S-PLUS's
  	\code{arima.mle}, which computes a conditional likelihood and does
  	not include a mean in the model.  Further, the convention used by
  	\code{arima.mle} reverses the signs of the MA coefficients.
  	\cr
  	\code{[ts:arima]}
	\cr
  	   	
  	\bold{FRACDIFF Modelling:}
  	\cr\cr 	 	
    The argument \code{x ~ fracdiff()} calls the underlying functions from
    \R's \code{fracdiff} package. The estimator calculates the maximum 
    likelihood estimators of the parameters of a fractionally-differenced 
    ARIMA (p,d,q) model, together (if possible) with their estimated 
    covariance and correlation matrices and standard errors, as well 
    as the value of the maximized likelihood. The likelihood is 
    approximated using the fast and accurate method of Haslett and 
    Raftery (1989). Note, the number of AR and MA coefficients should 
    not be too large (say < 10) to avoid degeneracy in the model. 
  	\cr
  	The optimization is carried out in two levels: an outer univariate 
  	unimodal optimization in d over the interval [0,.5], and an inner 
  	nonlinear least-squares optimization in the AR and MA parameters to
  	minimize white noise variance.
  	\cr
  	\code{[fracdiff:fracdiff]}
  
}


\references{

Brockwell, P.J. and Davis, R.A. (1996); 
	\emph{Introduction to Time Series and Forecasting}, 
	Second Edition, Springer, New York. 
	
Durbin, J. and Koopman, S.J. (2001); 
	\emph{Time Series Analysis by State Space Methods},  
	Oxford University Press.
	
Gardner, G, Harvey, A.C., Phillips, G.D.A. (1980); 
	\emph{Algorithm AS154. An algorithm for exact maximum likelihood 
	estimation of autoregressive-moving average models by means of 
	Kalman filtering},
	Applied Statistics, 29, 311--322.
	
Hannan E.J. and Rissanen J. (1982);
  	\emph{Recursive Estimation of Mixed Autoregressive-Moving 
  	Average Order.}
  	Biometrika 69, 81--94. 
  	
Harvey, A.C. (1993); 
	\emph{Time Series Models},
	2nd Edition, Harvester Wheatsheaf, Sections 3.3 and 4.4.

Jones, R.H. (1980); 
	\emph{Maximum likelihood fitting of ARMA models to time
	series with missing observations},
	Technometrics, 20, 389--395.

Percival, D.P. and Walden, A.T. (1998); 
	\emph{Spectral Analysis for Physical Applications.} 
	Cambridge University Press.

Whittle, P. (1963); 
	\emph{On the fitting of multivariate autoregressions
	and the approximate canonical factorization of a spectral 
	matrix.}
	Biometrika 40, 129--134.
	
Haslett J. and Raftery A.E. (1989);
    \emph{Space-time Modelling with Long-memory Dependence: Assessing 
    Ireland's Wind Power Resource (with Discussion)},
	Applied Statistics 38, 1--50.
	
}


\author{

	M. Plummer and B.D. Ripley for \code{ar} functions and code,\cr
	B.D. Ripley for \code{arima} and \code{ARMAacf} functions and code, \cr
	C. Fraley and F. Leisch for \code{fracdiff} functions and code, and \cr
	Diethelm Wuertz for the Rmetrics \R-port.
	
}


\examples{
## SOURCE("fSeries.31A-ArmaModelling")

## armaSim/armaFit -
   xmpSeries("\nStart: Simulate an ARMA(2, 1) process > ")
   x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.1), n = 1000)
   Continue = xmpSeries("Press any key > ")
   # Estimate the parameters:
   fit = armaFit(x ~ arma(2, 1))
   print(fit)
   Continue = xmpSeries("Press any key > ") 
   # Diagnostic Analysis:
   par(mfrow = c(3, 2), cex = 0.7)
   summary(fit)
   Continue = xmpSeries("Press any key > ")
   # 5 Steps ahead Forecasts:
   predict(fit, 5)
     
## armaFit -
   xmpSeries("\nNext: Estimate the parameters > ")
   # Now use the arima fitting method with d=0:
   fit = armaFit(x ~ arima(2, 0, 1))
   print(fit)
   Continue = xmpSeries("Press any key > ") 
   # Diagnostic Analysis:
   par(mfrow = c(3, 2), cex = 0.7)
   summary(fit)
   Continue = xmpSeries("Press any key > ") 
   # 5 Step ahead Forecasts:
   predict(fit, n.ahead = 5)
   
## armaRoots -
   xmpSeries("\nNext: Roots of the Characteristic Polynomial > ")
   # Calculate and plot the  of an ARMA process:
   par(mfrow = c(2, 2), cex = 0.7)
   coefficients = c(-0.5, 0.9, -0.1, -0.5)
   armaRoots(coefficients)
   
## armaTrueacf -
   xmpSeries("\nNext: True ACF of an ARMA Process > ")
   model = list(ar = c(0.3, +0.3), ma = 0.1)
   armaTrueacf(model)
   model = list(ar = c(0.3, -0.3), ma = 0.1)
   armaTrueacf(model)
}


\keyword{models}

