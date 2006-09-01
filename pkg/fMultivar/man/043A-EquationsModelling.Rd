\name{EquationsModelling}

\alias{EquationsModelling}

\alias{fSYSTEM}
\alias{fSYSTEM-class}

\alias{systemFit}
\alias{nlsystemFit}
\alias{SUR}

\alias{predict.fSYSTEM}

\alias{print.fSYSTEM}
%\alias{plot.fSYSTEM}
\alias{summary.fSYSTEM}

\alias{coef.fSYSTEM}
\alias{fitted.fSYSTEM}
\alias{residuals.fSYSTEM}
%\alias{vcov.fSYSTEM}

\alias{systemfitBuiltin}


\title{Equations Modelling}


\description{

    A collection and description of easy to use 
    functions to perform fits of systems of 
    regression equations. The underlying functions 
    are those from the contributed R-package 
    systemfit written by Jeff D. Hamann and Arne 
    Henningsen. The package offers functions for 
    fitting linear structural equations using 
    Ordinary Least Squares (OLS), Weighted Least 
    Squares (WLS), Seemingly Unrelated Regression 
    (SUR), Two-Stage Least Squares (2SLS), Weighted 
    Two-Stage Least Squares (W2SLS) or Three-Stage 
    Least Squares (3SLS). 
    \cr
    
    The wrapper fullfills the naming conventions of Rmetrics, returns a 
    S4 object named \code{fSYSTEM}, and allows for \code{timeSeries} objects 
    as input. In addition a S-Plus like FinMetrics function \code{SUR} 
    is made available.
    \cr

    The Models Based on 'systemfit' Include:
    
    \tabular{ll}{
    \code{"OLS"} \tab Ordinary Least Square Modelling, \cr
    \code{"WLS"} \tab Weighted Least Square Modelling, \cr
    \code{"SUR"} \tab Seemingly Unrelated Regression, \cr
    \code{"2SLS"} \tab Two-Stage Least Squares, \cr
    \code{"W2SLS"} \tab Weighted Two-Stage Least Squares, \cr
    \code{"3SLS"} \tab Three-Stage Least Squares. } 
            
    Further Functions and Methods are:
    
    \tabular{ll}{
    \code{print} \tab S3 Print method for objects of class 'fSYSTEM', \cr
    \code{summary} \tab S3 Summary method for objects of class 'fSYSTEM', \cr
    \code{predict} \tab S3 Predict method for objects of class 'fSYSTEM'. }
                
    S-Plus like Finmetrics Function:
    
    \tabular{ll}{
    \code{SUR} \tab A S-PLUS like function for \code{"SUR"} models. }
            
    Note, that the contributed R package \code{systemfit} is required!
    If the package \code{systemfit} is not installed on your computer
    or not availalble for your operating system, then you can load it 
    as builtin function calling the internal Rmetrics function 
    \code{systemfitBuiltin()}.
    
}
    

\usage{
systemFit(formula, data = list(), method = c("OLS", "WLS", "SUR", "2SLS", 
    "W2SLS", "3SLS", "W3SLS"), title = NULL, description = NULL, \dots)
    
nlsystemFit(formula, data = list(), method = c("OLS", "SUR", "2SLS", "3SLS"), 
    start = NULL, title = NULL, description = NULL, \dots)

\method{predict}{fSYSTEM}(object, newdata = object@data, se.fit = FALSE, 
    se.pred = FALSE, interval = "none", ci = 0.95, \dots)

\method{print}{fSYSTEM}(x, \dots)
\method{summary}{fSYSTEM}(object, \dots)

\method{coef}{fSYSTEM}(object, \dots)
\method{fitted}{fSYSTEM}(object, \dots)
\method{residuals}{fSYSTEM}(object, \dots)
%\method{vcov}{fSYSTEM}(object, \dots)

SUR(formula, data = list(), \dots)

%systemfitBuiltin(builtin = "/fMultivar/demo/funSystemfit.R")
} 


\details{

    Ordinary Least Squares (OLS) estimates are biased and inconsistent 
    when endogenous variables appear as regressors in other equations 
    in the system. Furthermore, one observes that the errors of a set 
    of related regression equations are often correlated. Then the 
    efficiency of the estimates can in many cases be improved including 
    the correlations into the parameter estimation procedure. The 
    function \code{eqnaFit} provides several methods which can 
    produce consistent and asymptotically efficient estimates for 
    systems of regression equations. 
    
    The variables in a system of equations can be characterized by  
    four types. These include \emph{Endogenous Variables} which are 
    the variables determined by the system, \emph{Exogenous Variables}  
    which are independent variables that do not depend on any of the 
    endogenous variables in the system, \emph{Predetermined Variables}  
    which include both the exogenous variables and lagged endogenous 
    variables, which are past values of endogenous variables determined  
    at previous time periods, and \emph{Instrumental Variables } which 
    are are predetermined variables used in obtaining predicted values 
    for the current period endogenous variables by a first-stage regression. 
    The use of instrumental variables characterizes estimation methods 
    such as two-stage least squares and three-stage least squares. 
    Instrumental variables estimation methods substitute these first-stage 
    predicted values for endogenous variables when they appear as 
    regressors in model equations.
    \cr
    
    \emph{Technical Details: 'systemfit'}
    
    The matrix \code{TX} transforms the regressor matrix (\eqn{X}) by
    \eqn{X^{*} = X *} \code{TX}. Thus, the vector of coefficients is now
    \eqn{b =} \code{TX} \eqn{\cdot b^{*}}, where \eqn{b} is the original 
    (stacked) vector of all coefficients and \eqn{b^{*}} is the new 
    coefficient vector that is estimated instead. Thus, the elements of 
    vector \eqn{b} are \eqn{b_i = \sum_j TX_{ij} \cdot b^{*}_j}. 
    
    The \code{TX} matrix can be used to change the order of the
    coefficients and also to restrict coefficients (if \code{TX} has 
    less columns than it has rows). However restricting coefficients
    by the \code{TX} matrix is less powerfull and flexible than the
    restriction by providing the \code{R.restr} matrix and the
    \code{q.restr} vector. The advantage of restricting the coefficients
    by the \code{TX} matrix is that the matrix that is inverted for
    estimation gets smaller by this procedure, while it gets larger
    if the restrictions are imposed by \code{R.restr} and \code{q.restr}.
    
    If iterated (WLS, SUR, W2SLS or 3SLS estimation with \code{maxit}>1),
    the convergence criterion is 
        
        \eqn{\sqrt{
            \sum_i (b_{i,g} - b_{i,g-1})^2 \left/ 
            \sum_i b_{i,g-1}^2 \right. }}
        < \code{tol}.
       
    Here, \eqn{b_{i,g}} is the ith coefficient of the g-th 
    iteration step.
    
    The formula to calculate the estimated covariance matrix of the 
    residuals, \eqn{\hat{\Sigma}}, can be one of the following, see 
    Judge et al., 1985, p. 469: 
    
        if \code{rcovformula=0:} \eqn{\hat{\sigma}_{ij} = 
            (\hat{e}_i' \hat{e}_j) / T}; \cr
    
        if \code{rcovformula=1:} \eqn{\hat{\sigma}_{ij} = 
            (\hat{e}_i' \hat{e}_j) / \sqrt{(T - k_i)*(T - k_j)}}; \cr
    
        if \code{rcovformula=2:} \eqn{\hat{\sigma}_{ij} = 
            (\hat{e}_i' \hat{e}_j) / (T - k_i - k_j + 
            tr[(X_i'X_i)^{-1}X_i'X_j(X_j'X_j)^{-1}X_j'X_i]}. \cr
    
    If \eqn{k_i = k_j}, formula 1 and 2 are equal and yield an unbiased 
    estimator for the residual covariance matrix.
    If \eqn{k_i \neq k_j}, only formula 2 yields an unbiased estimator 
    for the residual covariance matrix, but it is not neccessarily positive 
    semidefinit and its inverse is \bold{not} an unbiased estimator for 
    the inverse of the residual covariance matrix. Thus, it is doubtful 
    whether formula 2 is really superior to formula 1, see Theil, 1971, 
    p. 322.
    
    The formulas to calculate the 3SLS estimator lead to identical 
    results if the same instruments are used in all equations. If 
    different instruments are used in the different equations, only 
    the GMM-3SLS estimator, \code{"GMM"} and the 3SLS estimator proposed 
    by Schmidt (1990), \code{"Schmidt"} are consistent, whereas 
    \code{"GMM"} is efficient relative to \code{"Schmidt"}, see Schmidt,
    1990. 
    \cr
    
    \emph{Prediction:}
    
    The variance of the fitted values, used to calculate the standard 
    errors of the fitted values and the confidence interval, is 
    calculated by
     
        \eqn{Var[E[y^0]-\hat{y}^0]=x^0 \; Var[b] \; {x^0}'}\cr
        
    an the variances of the predicted values, used to calculate the 
    standard errors of the predicted values and the prediction intervals, 
    is calculated by
   
        \eqn{Var[y^0-\hat{y}^0]=\hat{\sigma}^2+x^0 \; Var[b] \; {x^0}'}
    
}


\note{

    It is worth to remark, that there are two more R-packages which are 
    of interest in this context: 
    
    The contributed R package \code{"sem"} offers functions for fitting general 
    structural equation models by the method of maximum likelihood (SEM) 
    and for fitting a model by two-stage least squares (TSLS). This package 
    was written by John Fox. 
    
    The contributed R package \code{"pls.pcr"} offers also functions for 
    multivariate regression. Principal Component Regression (PCR) and two 
    types of Partial Least Square Regression (PLS), simple-PLS and 
    kernel-PLS, are implemented by Ron Wehrens. 
    
    These two packages are not discussed here and are available from the
    CRAN server.
    
    Wrapper for the nonlinear case are not yet available.

}

 
\arguments{
    
    \item{ci}{
        [predict] - \cr
        the confidence interval, by default 0.95.
        }
    \item{formula}{
        [systemFit] - \cr
        the list of formulas describing the system of equations.
        }
    \item{data}{
        [systemFit] - \cr
        the input data set in form of a \code{data.frame} or 
        \code{timeSeries} object.
        }
    \item{description}{
        [systemFit] - \cr
        a character string which allows for a brief description.
        }   
    \item{interval}{
        [predict] - \cr
        Type of interval calculation, one of \code{"none"}, 
        \code{"confidence"}, or \code{"prediction"}.
        }
    \item{method}{
        [systemFit] - \cr
        a character string describing the desired method, one of:
        \code{"OLS"}, \code{"WLS"}, \code{"SUR"}, \code{"2SLS"}, 
        \code{"W2SLS"}, \code{"3SLS"}, or \code{"W3SLS"}.
        }
    \item{newdata}{
        [predict] - \cr
        a new input data set in form of a \code{data.frame} or 
        \code{timeSeries}to be predicted.
        }
    \item{object}{
        [predict][summary] - \cr
        [coef][fitted][residuals][vcov] - \cr
        an object of class \code{fSYSTEM}.
        }
    \item{se.fit}{
        [predict] - \cr
        a logical, should the standard error of the fitted values 
        be returned?
        }
    \item{se.pred}{
        [predict] - \cr
        a logical, should the standard error of the prediction
        be returned?
        }
    \item{start}{
        start values.
        }
    \item{title}{
        [systemFit] - \cr
        a character string which allows for a project title.
        }   
    \item{x}{
        [plot][print] - \cr
        an object of class \code{fSYSTEM}.
        }
    \item{\dots}{
        [systemFit] - \cr
        additional optional arguments to be passed to the underlying 
        function \code{systemfit} for the "OLS", "WLS", "SUR", "2SLS", 
        "W2SLS", "3SLS", or "W3SLS" method. \cr
        These include: 
        \cr     
        \code{eqnlabels} - \cr
            an optional list of character vectors of names for the 
            equation labels. 
            \cr
        \code{formula3sls} - \cr
            formula for calculating the 3SLS estimator, one of 
            \code{"GLS"}, \code{"IV"}, \code{"GMM"}, \code{"Schmidt"} 
            or \code{"EViews"}, see 'systemfit' details. 
            \cr
        \code{inst} - \cr
            one-sided model formula specifying instrumental variables 
            or a list of one-sided model formulas if different instruments 
            should be used for the different equations, only needed for 
            \code{"2SLS"}, \code{"W2SLS"} and \code{"3SLS"} estimations. 
            \cr
        \code{maxiter} - \cr
            maximum number of iterations for \code{"WLS"}, \code{"SUR"}, 
            \code{"W2SLS"} and \code{"3SLS"} estimations. 
            \cr 
        \code{probdfsys} - \cr
            use the degrees of freedom of the whole system (in place of
            the degrees of freedom of the single equation) to calculate 
            prob values for the t-test of individual parameters. 
            \cr
        \code{q.restr} - \cr
            an optional \code{j x 1} matrix to impose linear restrictions, 
            see \code{R.restr}; default is a \code{j x 1} matrix that 
            contains only zeros. 
            \cr
        \code{R.restr} - \cr
            an optional \code{j x k} matrix to impose linear restrictions  
            on the parameters by \code{R.restr} * \eqn{b} = \code{q.restr},
            \code{j} = number of restrictions, \code{k} = number of all 
            parameters, \eqn{b} = vector of all parameters. 
            \cr
        \code{rcovformula} - \cr
            formula to calculate the estimated residual covariance 
            matrix, see 'systemfit' details. 
            \cr
        \code{saveMemory} - \cr
            save memory by omitting some calculation that are not
            crucial for the basic estimation, e.g. McElroy's 
            \eqn{R^2}. 
            \cr
        \code{single.eq.sigma} - \cr
            use different \eqn{\sigma^2}s for each single equation to
            calculate the covariance matrix and the standard errors of 
            the coefficients, only \code{"OLS"} and \code{"2SLS"}. 
            \cr
        \code{solvetol} - \cr
            tolerance level for detecting linear dependencies when
            inverting a matrix or calculating a determinant, see
            see \code{\link{solve}} and \code{\link{det}}. 
            \cr
        \code{tol} - \cr
            tolerance level indicating when to stop the iteration,
            only \code{"WLS"}, \code{"SUR"}, \code{"W2SLS"} and 
            \code{"3SLS"} estimations. 
            \cr
        \code{TX} - \cr
            an optional matrix to transform the regressor matrix 
            and, hence, also the coefficient vector, see 'systemfit'
            details.                
        }
}


\value{

    \bold{Fit: Parameter Estimation}
    \cr
    
    The function \code{systemFit} returns an object of class \code{"fSYSTEM"} 
    with the following slots:
    
    \item{@call}{
        the matched function call.
        }
    \item{@data}{
        the input data in form of a \code{data.frame} or a 
        \code{timeSeries} object.
        }
    \item{@description}{
        a character string which allows for a brief project description.
        }
    \item{@fit}{
        a summary of the  results as a list returned from the underlying
        functions from the \code{systemfit} package.
        }   
    \item{@formulas}{
        the list of formulas describing the system of equations.
        }
    \item{@method}{
        a character string describing the desired method, one of:
        \code{"OLS"}, \code{"WLS"}, \code{"SUR"}, \code{"2SLS"}, 
        \code{"W2SLS"}, \code{"3SLS"}, or \code{"W3SLS"}.
        }
    \item{@title}{
        a character string which allows for a project title.
        }
    
  
    The \code{@fit} slot is a list with entries returned from the
    underlying fitting function. The function returns a list of class 
    code{systemfit}. The list contains one special object: \code{eq}. This 
    object is also a list and contains one object for each estimated equation. 
    These objects are of the class \code{systemfit.equation} and contain 
    the results that belong only to the regarding equation. The objects 
    of the class \code{systemfit} and \code{systemfit.equation} have the 
    following components (the elements of the latter are marked with an 
    asterisk (\eqn{*})):
    
    The major elements of the list are:
    
    \item{coef}{
        the coefficients from an object of class \code{fSYSTEM}. A 
        one-column data frame of all estimated coefficients.
        }
    \item{confint}{
        the confidence intervals of the coefficients of one equation 
        from an object of class \code{fSYSTEM}. 
        }
    \item{fitted}{
        the fitted values of all equations from an object of class 
        \code{fSYSTEM}. 
        }
    \item{residuals}{
         the residuals from an object of class \code{fSYSTEM}. 
         }
    \item{vcov}{
        the variance covariance matrix of all coefficients from an 
        object of class \code{fSYSTEM}. 
        }
           
    The remaining elements of the slot \code{@fit$fit} are:
    
    \item{method}{
        estimation method. 
        }
    \item{g}{
        number of equations. 
        }
    \item{n}{
        total number of observations. 
        }
    \item{k}{
        total number of coefficients. 
        }
    \item{ki}{
        total number of linear independent coefficients. 
        }
    \item{df}{
        degrees of freedom of the whole system. 
        }
    \item{iter}{
        number of iteration steps. 
        }
    \item{b}{
        vector of all estimated coefficients. }
    \item{bt}{
        coefficient vector transformed by \code{TX}. 
        }
    \item{se}{
        estimated standard errors of \code{b}. 
        }
    \item{t}{
        t values for \code{b}. 
        }
    \item{p}{
        p values for \code{b}. 
        }
    \item{bcov}{
        estimated covariance matrix of \code{b}. 
        }
    \item{btcov}{
        covariance matrix of \code{bt}. 
        }
    \item{rcov}{
        estimated residual covariance matrix. 
        }
    \item{drcov}{
        determinant of \code{rcov}. 
        }
    \item{rcovest}{
        residual covariance matrix used for estimation, 
        only "SUR" and "3SLS". 
        }
    \item{rcor}{
        estimated residual correlation matrix. 
        }
    \item{olsr2}{
        System OLS R-squared value. 
        }
    \item{mcelr2}{
        McElroys R-squared value for the system, only "SUR" and "3SLS". 
        }
    \item{y}{
        vector of all (stacked) endogenous variables.
        }
    \item{x}{
        matrix of all (diagonally stacked) regressors.
        }
    \item{h}{
        matrix of all (diagonally stacked) instrumental variables, 
        only "2SLS" and "3SLS". 
        }
    \item{data}{
        data frame of the whole system including instruments. 
        }
    \item{R.restr}{
        the restriction matrix. 
        }
    \item{q.restr}{
        the restriction vector. 
        }
    \item{TX}{
        matrix used to transform the regressor matrix. 
        }
    \item{maxiter}{
        maximum number of iterations. 
        }
    \item{tol}{
        tolerance level indicating when to stop the iteration. 
        }
    \item{rcovformula}{
        formula to calculate the estimated residual covariance matrix. 
        }
    \item{formula3sls}{
        formula for calculating the "3SLS" estimator. 
        }
    \item{probdfsys}{
        system degrees of freedom to calculate prob values?
        }
    \item{single.eq.sigma}{
        different \eqn{\sigma^2}s for each single equation?
        }
    \item{solvetol}{
        tolerance level when inverting a matrix or calculating 
        a determinant. 
        }
    
    The elements of the class \code{systemfit.eq} are:
    
    \item{eq}{
        a list that contains the results that belong to the individual 
        equations. 
        }
    \item{eqnlabel*}{
        the equation label of the i-th equation (from the labels list). 
        }
    \item{formula*}{
        model formula of the i-th equation. 
        }
    \item{inst*}{
        instruments of the i-th equation, only 2SLS and 3SLS. 
        }
    \item{n*}{
        number of observations of the i-th equation. 
        }
    \item{k*}{
        number of coefficients/regressors in the i-th equation (including 
        the constant). 
        }
    \item{ki*}{
        number of linear independent coefficients in the i-th equation 
        (including the constant differs from \code{k} only if there are 
        restrictions that are not cross-equation). 
        }
    \item{df*}{
        degrees of freedom of the i-th equation. 
        }   
    \item{b*}{
        estimated coefficients of the i-th equation. 
        }
    \item{se*}{
        estimated standard errors of \code{b}. 
        }
    \item{t*}{
        t values for \code{b}. 
        }
    \item{p*}{
        p values for \code{b}. 
        }
    \item{covb*}{
        estimated covariance matrix of \code{b}. 
        }
    \item{y*}{
        vector of endogenous variable (response values) of the i-th 
        equation. 
        }
    \item{x*}{
        matrix of regressors (model matrix) of the i-th equation. 
        }
    \item{h*}{
        matrix of instrumental variables of the i-th equation, 
        only "2SLS" and "3SLS". 
        }
    \item{data*}{
        data frame (including instruments) of the i-th equation. 
        }
    \item{fitted*}{
        vector of fitted values of the i-th equation. 
        }
    \item{residuals*}{
        vector of residuals of the i-th equation. 
        }   
    \item{ssr*}{
        sum of squared residuals of the i-th equation. 
        }
    \item{mse*}{
        estimated variance of the residuals (mean of squared errors)  
        of the i-th equation. 
        }
    \item{s2*}{
        estimated variance of the residuals (\eqn{\hat{\sigma}^2}) of 
        the i-th equation. 
        }
    \item{rmse*}{
        estimated standard error of the residulas (square root of mse) 
        of the i-th equation. 
        }
    \item{s*}{
        estimated standard error of the residuals (\eqn{\hat{\sigma}}) 
        of the i-th equation. 
        }
    \item{r2*}{
        R-squared (coefficient of determination). 
        }
    \item{adjr2*}{
        adjusted R-squared value. 
        }
        

    \bold{S3 Methods:}
    \cr
    
    The output from the S3 \code{summary} method prints the results
    in form of a detailed report together with optional plots. 
    \cr
    The output from the S3 \code{print} method prints on object
    of class \code{fSYSTEM}. 
    \cr
    The output from the S3 \code{plot} method returns some diagnostic
    plots.
    \cr
     
    
    \bold{S-Plus like SUR Function:}
    \cr
    
    The function \code{SUR} returns an object of class \code{"fSYSTEM"} 
    with the same slots returned by the function \code{systemFit} for
    method \code{"SUR"}.
         
}


\author{

    Jeff D. Hamann and Arne Henningsen for the \code{systemfit} package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Greene W.H., (1993);
    \emph{Econometric Analysis}, 
    Second Edition, Macmillan.

Greene W.H., (2002);
    \emph{Econometric Analysis} 
    Fifth Edition, Prentice Hall.

Judge G.G., Griffiths W.E., Hill R.C, Lütkepohl H., Lee T.C., (1985);
    \emph{The Theory and Practice of Econometrics},
    Second Edition, Wiley.

Kmenta J., (1997);
    \emph{Elements of Econometrics}, 
    Second Edition, University of Michigan Publishing.

Schmidt P., (1990);
    \emph{Three-Stage Least Squares with different Instruments for 
        different equations},
    Journal of Econometrics 43, p. 389--394.

Theil H., (1971);
    \emph{Principles of Econometrics}, 
    Wiley, New York.
    
}


\examples{
## SOURCE("fMultivar.3A-EquationsModelling")

## Note, "systemfit" is required:
   SYSTEMFIT = require(systemfit)
   if (SYSTEMFIT) {
   
## Examples from the 'systemfit' Package:
   data(kmenta)
   
## OLS Estimations:   
   formulas = list(demand = q ~ p + d, supply = q ~ p + f + a )
   FITOLS = systemFit(formulas, data = kmenta)
   FITOLS
   
## OLS Estimation with 2 Restrictions:
   Rrestr <- matrix(0, 2, 7)
   qrestr <- matrix(0, 2, 1)
   Rrestr[1,3] =  1
   Rrestr[1,7] = -1
   Rrestr[2,2] = -1
   Rrestr[2,5] =  1
   qrestr[2,1] =  0.5
   FITOLS2 = systemFit(formulas, data = kmenta, R.restr = Rrestr, 
     q.restr = qrestr)
   FITOLS2
   
## Iterated SUR Estimation:
   FITSUR = systemFit(formulas, data = kmenta, method = "SUR", maxit = 100)
   FITSUR
   # Coefficients, Fitted Values, Residuals and Variance-Covariance Matrix:
   # Call by Method:
   coef(FITSUR)
   fitted(FITSUR)
   residuals(FITSUR)

## 2SLS Estimation:
   inst = ~ d + f + a
   FIT2SLS = systemFit(formulas, data = kmenta, method = "2SLS", inst = inst)
   FIT2SLS
   # Coefficients, Fitted Values, Residuals and Variance-Covariance Matrix:
   # Call by Slot:
   FIT2SLS@fit$coef
   FIT2SLS@fitted.values
   FIT2SLS@residuals

## 2SLS Estimation with Different Instruments in Each Equation:
   insts = list( ~ d + f, ~ d + f + a)
   FIT2SLS2 = systemFit(formulas, data = kmenta, method = "2SLS", inst = insts)
   FIT2SLS2

## 3SLS Estimation with GMM-3SLS Formula:
   instruments = ~ d + f + a
   FIT3SLS = systemFit(formulas, data = kmenta, method = "3SLS", 
     inst = instruments, formula3sls = "GMM")
   FIT3SLS
   
   } # if (SYSTEMFIT)
   
## SEE ALSO:
   # Demo File: xmpEqnsGrunfeld.R
   # Estimation of Grunfeld's Model Data with OLS and SUR
}


\seealso{

    \code{lm},
    \code{regFit}. 
    
}


\keyword{models}

