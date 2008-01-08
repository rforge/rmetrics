\name{regFit}


\alias{regFit}
\alias{gregFit}


\title{Regression Modelling}


\description{

    Estimates the parameters of a regression model.

}


\usage{    
regFit(formula, data, family = gaussian, 
    use = c("lm", "rlm", "glm","gam", "ppr", "nnet", "polymars"), 
    title = NULL, description = NULL, \dots)
}


\arguments{
    
    \item{data}{
        \code{data} is the data frame containing the variables in the 
        model. By default the variables are taken from 
        \code{environment(formula)}, typically the environment from 
        which \code{lm} is called.
        }
    \item{description}{
        a brief description of the porject of type character.
        }
    \item{family}{
        a description of the error distribution and link function to be 
        used in \code{glm} and \code{gam} models. See \code{\link{glm}} 
        and \code{\link{family}} for more details.
        }
    \item{formula}{
        a symbolic description of the model to be fit.
        \cr
        A typical \code{glm} predictor has the form \code{response ~ terms} 
        where \code{response} is the (numeric) response vector and \code{terms}
        is a series of terms which specifies a (linear) predictor for 
        \code{response}. For \code{binomial} models the response can also 
        be specified as a \code{factor}.
        \cr
        A \code{gam} formula, see also \code{gam.models}, allows
        that smooth terms can be added to the right hand side of the 
        formula. See \code{gam.side.conditions} for details and 
        examples.
        }  
    \item{use}{
        denotes the regression method by a character string used to fit 
        the model.
        \code{method} must be one of the strings in the default argument.\cr
        \code{"LM"}, for linear regression models, \cr
        \code{"GLM"} for generalized linear modelling,\cr
        \code{"GAM"} for generalized additive modelling,\cr
        \code{"PPR"} for projection pursuit regression,\cr
        \code{"POLYMARS"} for molytochomous MARS, and\cr
        \code{"NNET"} for feedforward neural network modelling.
        }    
    \item{title}{
        a character string which allows for a project title.
        }
    \item{\dots}{
        additional optional arguments to be passed to the underlying 
        functions. For details we refer to inspect the following help 
        pages: \code{\link{lm}}, \code{\link{glm}}, \code{gam},
        \code{\link{ppr}}, \code{polymars}, 
        or \code{nnet}. 
        }
        
}


\value{

    returns an S4 object of class \code{"fREG"}.
            
}


\details{

    The function \code{regFit} was created to provide a selection of
    regression models working together with Rmetrics' \code{"timeSeries"}
    objects and providing a common S4 object as the returned value. These
    models include linear modeling, robust linear modeling, generalized
    linear modeling, generalized additive modelling, projection pursuit
    regression, neural networks, and polytochomous MARS models.\cr
    

    % -----------------------------------------------
    \bold{LM -- Linear Modelling:}
    \cr\cr  
    Univariate linear regression analysis is a statistical methodology 
    that assumes a linear relationship between some predictor variables 
    and a response variable. The goal is to estimate the coefficients 
    and to predict new data from the estimated linear relationship.

    R's base function

    \code{lm(formula, data, subset, weights, na.action, method = "qr", }\cr
    \code{ model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, }\cr 
    \code{ contrasts = NULL, offset, ...) }\cr

    is used to fit linear models. It can be used to carry out regression, 
    single stratum analysis of variance and analysis of covariance, although 
    \code{aov} may provide a more convenient interface for these. 
    
    Rmetrics' function 
    
    \code{regFit(formula, data, use = "lm", ...) }\cr
    
    calls R's base function \code{lm} but with the difference that the
    \code{data} argument, may be any rectangular object which can be 
    transferred by the function \code{as.data.frame} into a data frame
    with named columns, e.g. an object of class \code{"timeSeries"}. 
    The function \code{regFit} returns an S4 object of class \code{"fREG"} 
    whose slot \code{@fit} is the object as returned by the function 
    \code{"lm"}. In addtion we have S4 methods \code{fitted} and 
    \code{residuals} which allow to retrieve the fitted values and the 
    residuals as objects of same classe as defined by the argument 
    \code{data}.
    
    The function \code{plot.lm} provides four plots: a plot of residuals 
    against fitted values, a Scale-Location plot of sqrt{| residuals |} 
    against fitted values, a normal QQ plot, and a plot of Cook's 
    distances versus row labels.\cr 
    \code{[stats:lm]}
    \cr
    
    
    % -----------------------------------------------
    \bold{LM -- Robust Linear Modelling:}
    \cr\cr  
    
    To fit a linear model by robust regression using an M estimator R
    offers the function 
    
    \code{rlm(formula, data, weights, ..., subset, na.action, }\cr
    \code{  method = c("M", "MM", "model.frame"), }\cr
    \code{  wt.method = c("inv.var", "case"), }\cr
    \code{  model = TRUE, x.ret = TRUE, y.ret = FALSE, contrasts = NULL) }\cr

    from package \code{MASS}. Again we can use the Rmetrics' wrapper
    
    \code{regFit(formula, data, use = "rlm", ...) }\cr
    
    which allows us to use for example S4 \code{timeSeries} objects as
    input and to get the output as an S4 object with the known slots.\cr
    \code{[MASS::rlm]}
    \cr
    
    
    % -----------------------------------------------
    \bold{GLM -- Generalized Linear Models:}
    \cr\cr
    Generalized linear modelling extends the linear model in two directions.
    (i) with a monotonic differentiable link function describing how the 
    expected values are related to the linear predictor, and (ii) with 
    response variables having a probability distribution from an exponential 
    family.\cr
    
    R's base function from package \code{stats} comes with the function
    
    \code{glm(formula, family = gaussian, data, weights, subset, }\cr 
    \code{  na.action, start = NULL, etastart, mustart, offset, }\cr
    \code{  control = glm.control(...), model = TRUE, method = "glm.fit", }\cr 
    \code{  x = FALSE, y = TRUE, contrasts = NULL, ...) }\cr

    Again we can use the Rmetrics' wrapper
    
    \code{regFit(formula, data, use = "gam", ...) }\cr
    
    \code{[stats::glm]}
    \cr
    
    % -----------------------------------------------
    \bold{GAM -- Generalized Additive Models:}
    \cr\cr  
    An additive model generalizes a linear model by smoothing individually
    each predictor term. A generalized additive model extends the additive
    model in the same spirit as the generalized liner amodel extends the 
    linear model, namely for allowing a link function and for allowing 
    non-normal distributions from the exponential family.\cr
    \code{[mgcv:gam]}
    \cr
    
    
    % -----------------------------------------------
    \bold{PPR -- Projection Pursuit Regression:}
    \cr\cr  
    The basic method is given by Friedman (1984), and is essentially 
    the same code used by S-PLUS's \code{ppreg}. It is observed that 
    this code is extremely sensitive to the compiler used. The algorithm 
    first adds up to \code{max.terms}, by default \code{ppr.nterms},
    ridge terms one at a time; it will use less if it is unable to find 
    a term to add that makes sufficient difference. The levels of 
    optimization, argument \code{optlevel}, by default 2, differ in 
    how thoroughly the models are refitted during this process.
    At level 0 the existing ridge terms are not refitted.  At level 1
    the projection directions are not refitted, but the ridge
    functions and the regression coefficients are. Levels 2 and 3 refit 
    all the terms; level 3 is more careful to re-balance the contributions
    from each regressor at each step and so is a little less likely to
    converge to a saddle point of the sum of squares criterion. The 
    \code{plot} method plots Ridge functions for the projection pursuit 
    regression fit.\cr
    \code{[stats:ppr]}
    \cr
    
    
    % -----------------------------------------------
    \bold{POLYMARS -- Polytochomous MARS:}
    \cr\cr
    The algorithm employed by \code{polymars} is different from the 
    MARS(tm) algorithm of Friedman (1991), though it has many similarities. 
    Also the name \code{polymars} has been used for this algorithm well 
    before MARS was trademarked.\cr
    \code{[polyclass:polymars]}
    \cr
    
    
    % -----------------------------------------------
    \bold{NNET -- Feedforward Neural Network Regression:}
    \cr\cr  
    If the response in \code{formula} is a factor, an appropriate 
    classification network is constructed; this has one output and 
    entropy fit if the number of levels is two, and a number of 
    outputs equal to the number of classes and a softmax output 
    stage for more levels. If the response is not a factor, it is 
    passed on unchanged to \code{nnet.default}. A quasi-Newton 
    optimizer is used, written in \code{C}. \cr
    \code{[nnet:nnet]}
    
}


\references{

Belsley D.A., Kuh E., Welsch R.E. (1980);
    \emph{Regression Diagnostics};
    Wiley, New York.
    
Dobson, A.J. (1990);
    \emph{An Introduction to Generalized Linear Models};
    Chapman and Hall, London.

Draper N.R., Smith H. (1981);
    \emph{Applied Regression Analysis}; 
    Wiley, New York.

Friedman, J.H. (1991); 
    \emph{Multivariate Adaptive Regression Splines (with discussion)},
    The Annals of Statistics 19, 1--141.
    
Friedman J.H., and Stuetzle W. (1981); 
    \emph{Projection Pursuit Regression}; 
    Journal of the American Statistical Association 76, 817-823.

Friedman J.H. (1984);
    \emph{SMART User's Guide}; 
    Laboratory for Computational Statistics, 
    Stanford University Technical Report No. 1.
    
Green, Silverman (1994);
    \emph{Nonparametric Regression and Generalized Linear Models};
    Chapman and Hall.

Gu, Wahba (1991); 
    \emph{Minimizing GCV/GML Scores with Multiple
        Smoothing Parameters via the Newton Method};
    SIAM J. Sci. Statist. Comput. 12, 383-398.

Hastie T., Tibshirani R. (1990);
    \emph{Generalized Additive Models};
    Chapman and Hall, London.

Kooperberg Ch., Bose S., and  Stone C.J. (1997);
    \emph{Polychotomous Regression},
    Journal of the American Statistical Association 92, 117--127.

McCullagh P., Nelder, J.A. (1989);
    \emph{Generalized Linear Models};
    Chapman and Hall, London.

Myers R.H. (1986);
    \emph{Classical and Modern Regression with Applications}; 
    Duxbury, Boston.

Rousseeuw P.J., Leroy, A. (1987);
    \emph{Robust Regression and Outlier Detection};
    Wiley, New York.

Seber G.A.F. (1977);
    \emph{Linear Regression Analysis}; 
    Wiley, New York.

Stone C.J., Hansen M., Kooperberg Ch., and Truong Y.K. (1997);
    \emph{The use of polynomial splines and their tensor products 
        in extended linear modeling (with discussion)}.

Venables, W.N., Ripley, B.D. (1999);
    \emph{Modern Applied Statistics with S-PLUS}; 
    Springer, New York.
    
Wahba (1990); 
    \emph{Spline Models of Observational Data};
    SIAM.

Weisberg S. (1985);
    \emph{Applied Linear Regression};  
    Wiley, New York.
    
Wood (2000); 
    \emph{Modelling and Smoothing Parameter Estimation  with
    Multiple  Quadratic Penalties};
    JRSSB 62, 413-428.

Wood (2001); 
    \emph{mgcv: GAMs and Generalized Ridge Regression for \R}.
    R News 1, 20-25.

Wood (2001);
    \emph{Thin Plate Regression Splines}.


    There exists a vast literature on regression. The references listed 
    above are just a small sample of what is available. The book by 
    Myers' is an introductory text book that covers discussions of much 
    of the recent advances in regression technology. Seber's book is 
    at a higher mathematical level and covers much of the classical theory 
    of least squares.
    
}


\author{

    The R core team for the \code{lm} functions from R's \code{base} package, \cr
    B.R. Ripley for the \code{glm} functions from R's \code{base} package, \cr
    S.N. Wood for the \code{gam} functions from R's \code{mgcv} package, \cr
    N.N. for the \code{ppr} functions from R's \code{modreg} package, \cr
    M. O' Connors for the \code{polymars} functions from R's \code{?} package, \cr
    The R core team for the \code{nnet} functions from R's \code{nnet} package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 100)
  
   # LM       
   regFit(Y ~ X1 + X2 + X3, data = x, use = "lm") 
   
   # RLM      
   regFit(Y ~ X1 + X2 + X3, data = x, use = "rlm") 
   # AM       
   regFit(Y ~ X1 + X2 + X3, data = x, use = "gam")                
   # PPR      
   regFit(Y ~ X1 + X2 + X3, data = x, use = "ppr") 
   # NNET     
   regFit(Y ~ X1 + X2 + X3, data = x, use = "nnet") 
   
   # POLYMARS
   if (require(polspline)) {
   regFit(Y ~ X1 + X2 + X3, data = x, use = "polymars")
   }
}


\keyword{models}

