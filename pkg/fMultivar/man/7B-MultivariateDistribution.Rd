\name{MultivariateDistribution}

\alias{MultivariateDistribution}

\alias{fMV}
\alias{fMV-class}

\alias{dmvsnorm}
\alias{pmvsnorm}
\alias{rmvsnorm}

\alias{dmvst}
\alias{pmvst}
\alias{rmvst}

\alias{mvFit}

\alias{print.fMV}
\alias{plot.fMV}
\alias{summary.fMV}


\title{Multivariate Skew Normal and Student-t Distributions}


\description{
  
    A collection and description of functions to compute 
    multivariate densities and probabilities from skew 
    normal and skew Student-t distribution functions. 
    Furthermore, multivariate random daviates can be 
    generated, and for multivariate data, the parameters 
    of the underlying distribution can be estimated 
    by the maximum log-likelihood estimation.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{dmvsnorm} \tab Multivariate Skew Normal Density, \cr
    \code{pmvsnorm} \tab Multivariate Skew Normal Probability, \cr
    \code{rmvsnorm} \tab Random Deviates from MV Skew Normal Distribution, \cr
    \code{dmvst} \tab Multivariate Skew Student Density, \cr
    \code{pmvst} \tab Multivariate Skew Student Probability, \cr
    \code{rmvst} \tab Random Deviates from MV Skew Student Distribution, \cr
    \code{mvFit} \tab Fits a MV Skew Normal or Student-t Distribution, \cr
    \code{print} \tab S3 print method for an object of class 'fMV', \cr
    \code{plot} \tab S3 Plot method for an object of class 'fMV', \cr
    \code{summary} \tab S3 summary method for an object of class 'fMV'. }
    
    These functions are useful for portfolio selection and optimization
    if one likes to model the data by multivariate normal, skew normal,
    or skew Student-t distribution functions.
    
}
    

\usage{
dmvsnorm(x, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim))
pmvsnorm(q, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim))
rmvsnorm(n, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim))

dmvst(x, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim), df = 4)
pmvst(q, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim), df = 4)
rmvst(n, dim = 2, mu = rep(0, dim), Omega = diag(dim), alpha = rep(0, dim), df = 4)


mvFit(x, method = c("snorm", "st"), fixed.df = NA, title = NULL, 
    description = NULL, trace = FALSE, \dots) 

\method{print}{fMV}(x, \dots)
\method{plot}{fMV}(x, which = "ask", \dots)
\method{summary}{fMV}(object, which = "ask", \dots)
}


\arguments{
  
    \item{description}{
        [mvFit] - \cr
        a character string, assigning a brief description to an 
        \code{"fMV"} object.
        }
    \item{dim}{
        [*mvsnorm][*mvst] - \cr
        the colum dimension of the matrix \code{x}. If \code{x} is 
        specified as a vector, \code{dim=1} must be set to one.
        }
    \item{fixed.df}{
        either \code{NA}, the default, or a numeric value assigning the
        number of degrees of freedom to the model. In the case that 
        \code{fixed.df=NA} the value of \code{df} will be included in the
        optimization process, otherwise not.
        }
    \item{method}{
        [mvFit] - \cr
        a string value specifying the method applied in the optimizing 
        process. This can be either \code{method="snorm"} or 
        \code{method="st"}, in the first case the parameters for a
        skew normal distribution will be fitted and in the second case
        the parameters for a skew Student-t distribution. 
        }
    \item{mu, Omega, alpha, df}{
        [*mvsnorm][*mvst] - \cr
        the model parameters: \cr
        \code{mu} a vector of mean values, one for each column, \cr
        \code{Omega} the covariance matrix, \cr
        \code{alpha} the skewness vector, and \cr
        \code{df} the number of degrees of freedom which is a measure for
            the fatness of the tails (excess kurtosis). \cr
        For a symmetric distribution \code{alpha} is a vector of zeros.
        For the normal distributions \code{df} is not used and set to 
        infinity, \code{Inf}. Note that all columns assume the same value 
        for \code{df}.
        }   
    \item{n}{
        [rmvsnorm][rmvst] - \cr
        number of data records to be simulated, an integer value.
        }
    \item{object}{
        [summary] - \cr
        an object of class \code{fMV}.
        }  
    \item{title}{
        [mvFit] - \cr
        a character string, assigning a title to an \code{"fMV"} object.
        }
    \item{trace}{
        a logical, if set to \code{TRUE} the optimization process will
        be traced, otherwise not. The default setting is \code{FALSE}.
        }
    \item{which}{
        which of the five plots should be displayed? \code{which} can 
        be either a character string, \code{"all"} (displays all plots) 
        or \code{"ask"} (interactively asks which one to display), or a 
        vector of 5 logical values, for those elements which are set 
        \code{TRUE} the correponding plot will be displayed.
        }
    \item{x, q}{
        [*mvsnorm][*mvst][mvFit] - \cr
        a numeric matrix of quantiles (returns) or any other rectangular 
        object like a data.frame or a multivariate time series objects 
        which can be transformed by the function \code{as.matrix} to an 
        object of class \code{matrix}. If \code{x} is a vector, it will 
        be transformed into a matrix object with one column.
        \cr
        [plot][print] - \cr
        An object of class \code{fMV}.
        }  
    \item{\dots}{
        optional arguments to be passed to the optimization or plotting
        functions.
        }
        
}


\details{

    These are "easy-to-use" functions which allow quickly to simulate 
    multivariate data sets and to fit their parameters assuming a 
    multivariate skew normal or skew Student-t distribution. The functions
    make use of the contributed R packages \code{sn} and \code{mtvnorm}.
    
    For an extended functionality in modelling multivariate skew normal
    and Student-t distributions we recommend to download and use the 
    functions from the original package \code{sn} which requires also the
    package \code{mtvnorm}.
    
    The algorithm for the computation of the normal and Student-t 
    distribution functions is described by Genz (1992) and (1993), 
    and its implementation by Hothorn, Bretz, and Genz (2001).
    
    The parameter estimation is done by the maximum log-likelihood
    estimation. The algorithm and the implemantation was done by
    Azzalini (1985-2003). 
    
    The multivariate skew-normal distribution is discussed in detail
    by Azzalini and Dalla Valle (1996); the \code{(Omega,alpha)} 
    parametrization adopted here is the one of Azzalini and 
    Capitanio (1999). 
    
    The family of multivariate skew-t distributions is an extension of 
    the multivariate Student's t family, via the introduction of a 
    shape parameter which regulates skewness; for a zero shape parameter
    the skew Student-t distribution reduces to the usual t distribution. 
    When \code{df = Inf} the distribution reduces to the multivariate 
    skew-normal one.
    
    The plot facilities have been completely reimplemented. The S3 plot
    method allows for selective batch and interactive plots. The 
    argument \code{which} takes care for the desired operation.
    
    The contributed R package \code{mvtnorm} is required, the contributed 
    R package \code{sn} is builtin, since it is not available on the
    Debian Software Server.   
}


\value{

    \code{[dp]mvsnorm} \cr
    \code{[dp]mvst}
    \cr
    return a vector of density and probability values computed from
    the matrix \code{x}.
    \cr
  
    \code{mvFit} 
    \cr
    returns a S4 object class of class \code{"fASSETS"}, with the following 
    slots:

    \item{@call}{
        the matched function call.
        }
    \item{@data}{
        the input data in form of a data.frame.
        }
    \item{@description}{
        allows for a brief project description.
        }
    \item{@fit}{
        the results as a list returned from the underlying
        fitting function. 
        }
    \item{@method}{
        the selected method to fit the distribution, either 
        \code{"snorm"}, or \code{"st"}.
        }
    \item{@model}{
        the model parameters describing the fitted parameters in 
        form of a list, \code{model=list(mu, Omega, alpha, df}.
        }
    \item{@title}{
        a title string.}
        
    The \code{@fit} slot is a list with the following compontents:
    (Note, not all are documented here).
    
    \item{@fit$dp}{
        a list containing the direct parameters beta, Omega, alpha. 
        Here, beta is a matrix of regression coefficients with 
        \code{dim(beta)=c(nrow(X), ncol(y))}, \code{Omega} is a 
        covariance matrix of order \code{dim}, \code{alpha} is 
        a vector of shape parameters of length \code{dim}.  
        }
    \item{@fit$se}{
        a list containing the components beta, alpha, info. Here, 
        beta and alpha are the standard errors for the corresponding 
        point estimates; info is the observed information matrix 
        for the working parameter, as explained below.
        }  
    \item{@fit$optim}{ 
        the list returned by the optimizer \code{optim}; see the 
        documentation of this function for explanation of its 
        components.  
        }

    \code{print} 
    \cr
    is the S3 print method for objects of class \code{"fMV"} returned
    from the function \code{mvFit}. If shows a summary report of
    the parameter fit.
    
    \code{plot} 
    \cr
    is the S3 plot method for objects of class \code{"fMV"} returned
    from the function \code{mvFit}. Five plots are produced. 
    The first plot produces a scatterplot and in one dimension an 
    histogram plot with the fitted distribution superimposed. 
    The second and third plot represent a QQ-plots of Mahalanobis 
    distances. The first of these refers to the fitting 
    of a multivariate normal distribution, a standard statistical 
    procedure; the second gives the corresponding QQ-plot of 
    suitable Mahalanobis distances for the multivariate skew-normal 
    fit. The fourth and fivth plots are similar to the previous ones, 
    except that PP-plots are produced. The plots can be displayed
    in several ways, depending an the argument \code{which}, for 
    details we refer to the arguments list above.

    \code{summary}
    \cr
    is the S3 summary method for objects of class \code{"fMV"} returned
    from the function \code{mvFit}. The summary method prints and plots
    in one step the results as done by the \code{print} and \code{plot} 
    methods.
  
}


\references{

Azzalini A. (1985);
    \emph{A Class of Distributions Which Includes the Normal Ones},
    Scandinavian Journal of Statistics 12, 171--178. 

Azzalini A. (1986);
    \emph{Further Results on a Class of Distributions Which Includes 
        the Normal Ones},
    Statistica 46, 199--208. 

Azzalini A., Dalla Valle A. (1996);
    \emph{The Multivariate Skew-normal Distribution},
    Biometrika 83, 715--726. 

Azzalini A., Capitanio A. (1999);
    \emph{Statistical Applications of the Multivariate Skew-normal 
        Distribution},
    Journal Roy. Statist. Soc. B61, 579--602. 

Azzalini A., Capitanio A. (2003);
    \emph{Distributions Generated by Perturbation of Symmetry with 
        Emphasis on a Multivariate Skew-t Distribution},
    Journal Roy. Statist. Soc. B65, 367--389. 
    
Genz A., Bretz F. (1999);
    \emph{Numerical Computation of Multivariate t-Probabilities
        with Application to Power Calculation of Multiple Contrasts}, 
    Journal of Statistical Computation and Simulation 63, 361--378.

Genz A. (1992);
    \emph{Numerical Computation of Multivariate Normal Probabilities},
    Journal of Computational and Graphical Statistics 1, 141--149.
    
Genz A. (1993); 
    \emph{Comparison of Methods for the Computation of Multivariate
        Normal Probabilities},
    Computing Science and Statistics 25, 400--405.
    
Hothorn T., Bretz F., Genz A. (2001);
    \emph{On Multivariate t and Gauss Probabilities in R},
    R News 1/2, 27--29.
    
}


\author{

    Torsten Hothorn for R's \code{mvtnorm} package, \cr
    Alan Ganz and Frank Bretz for the underlying Fortran Code, \cr
    Adelchi Azzalini for R's \code{sn} package, \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## rmvst - 
   par(mfcol = c(3, 1), cex = 0.7)
   r1 = rmvst(200, dim = 1)
   ts.plot(as.ts(r1), xlab = "r", main = "Student-t 1d")
   r2 = rmvst(200, dim = 2, Omega = matrix(c(1, 0.5, 0.5, 1), 2))
   ts.plot(as.ts(r2), xlab = "r", col = 2:3, main = "Student-t 2d")
   r3 = rmvst(200, dim = 3, mu = c(-1, 0, 1), alpha = c(1, -1, 1), df = 5)
   ts.plot(as.ts(r3), xlab = "r", col = 2:4, main = "Skew Student-t 3d")    
   
## mvFit - 
   # Generate Grid Points:
   n = 51
   x = seq(-3, 3, length = n)
   xoy = cbind(rep(x, n), as.vector(matrix(x, n, n, byrow = TRUE)))
   X = matrix(xoy, n * n, 2, byrow = FALSE)
   head(X)
   # The Bivariate Normal Case:
   Z = matrix(dmvsnorm(X, dim = 2), length(x))
   par (mfrow = c(2, 2), cex = 0.7)
   persp(x, x, Z, theta = -40, phi = 30, col = "steelblue") 
   title(main = "Bivariate Normal Plot")
   image(x, x, Z)
   title(main = "Bivariate Normal Contours")
   contour(x, x, Z, add = TRUE)
   # The Bivariate Skew-Student-t Case:
   mu = c(-0.1, 0.1)
   Omega = matrix(c(1, 0.5, 0.5, 1), 2)
   alpha = c(-1, 1)
   Z = matrix(dmvst(X, 2, mu, Omega, alpha, df = 3), length(x))
   persp(x, x, Z, theta = -40, phi = 30, col = "steelblue") 
   title(main = "Bivariate Student-t Plot")
   image(x, x, Z)
   contour(x, x, Z, add = TRUE)
   title(main = "Bivariate Student-t Contours")
   
## plot -
   # Student-t: Fixed number of degrees of freedo
   fit2 = mvFit(x = rmvst(100, 5), method = "st", fixed.df = NA)
   # Show Model Slot:
   fit2@model
   # Show Scatterplot:
   ## par(mfrow = c(1, 1), cex = 0.7)
   ## plot(fit2, which = c(TRUE, FALSE, FALSE, FALSE, FALSE))
   # Show QQ and PP Plots:
   ## par(mfrow = c(2, 2), cex = 0.7)
   ## plot(fit2, which = !c(TRUE, FALSE, FALSE, FALSE, FALSE))
   # Interactive Plots:
   # par(mfrow = c(1, 1))
   # plot(fit2, which = "ask")
}


\keyword{distribution}
