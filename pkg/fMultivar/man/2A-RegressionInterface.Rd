\name{RegressionInterface}

\alias{RegressionInterface}

\alias{fREG}
\alias{fREG-class}

\alias{regSim}
\alias{regFit}
\alias{gregFit}

\alias{predict.fREG}

\alias{show.fREG}
\alias{show,fREG-method}
\alias{plot.fREG}
\alias{summary.fREG}

\alias{coef.fREG}
\alias{fitted.fREG}
\alias{residuals.fREG}
\alias{vcov.fREG}

\alias{OLS}
\alias{print.OLS}
\alias{plot.OLS}
\alias{summary.OLS}


\title{Univariate Regression Modelling}


\description{

    A collection and description of easy to use 
    functions to perform an univariate regression 
    analysis from several methods, to analyse and 
    summarize the fit, and to predict for new 
    data records.
    \cr
    
    The models include:
    
    \tabular{ll}{
    \code{"LM"} \tab Linear Modelling, \cr
    \code{"GLM"} \tab Generalized Linear Modelling, \cr
    \code{"GAM"} \tab Generalized Additive Modelling, \cr
    \code{"PPR"} \tab Projection Pursuit Regression, \cr
    \code{"MARS"} \tab Multivariate Adaptive Regression Splines, \cr
    \code{"POLYMARS"} \tab Polytochomous MARS, and \cr
    \code{"NNET"} \tab Feedforward Neural Network Modelling. }
            
    Available methods are:
    
    \tabular{ll}{
    \code{predict} \tab Predict method for objects of class 'fARMA', \cr
    \code{print} \tab Print method for objects of class 'fARMA', \cr
    \code{plot} \tab Plot method for objects of class 'fARMA', \cr
    \code{summary} \tab Summary method for objects of class 'fARMA', \cr
    \code{fitted.values} \tab Fitted values method for objects of class 'fARMA', \cr
    \code{residuals} \tab Residuals method for objects of class 'fARMA'. }
            
    The print method prints the returned object from a regression 
    fit, and the summary method performs a diagnostic analysis and 
    summarizes the results of the fit in a detailed form. The plot
    method produces diagostic plots. The predict method forecasts 
    from new data records. Two other methods to print the fitted 
    values, and the residuals are available.
    
    Furthermore, a S-Plus Finmetrics like ordinary least square 'OLS' 
    function has been added including S3 print, plot and summary methods.
    
    \tabular{ll}{
    \code{OLS} \tab Predict method for objects of class 'fARMA', \cr
    \code{print} \tab Print method for objects of class 'fARMA', \cr
    \code{plot} \tab Plot method for objects of class 'fARMA', \cr
    \code{summary} \tab Summary method for objects of class 'fARMA'. } 
    
}
    

\note{  
    
    Note, \code{"POLYMARS"} requires contributed package \code{"polspline"}
    \cr
    
    This \code{regFit} function offers for several regression models
    an easy to use wrapper. There is nothing really new in this package. 
    However, the benefit you will get is, that all regression 
    models got a common argument list with a formula to desribe 
    the input data to be used, if rquired with an argument to specify
    the family function, and with a string to name the type of the 
    desired regression model. On the other hand, the user can pass 
    additional arguments to the underlying functions. This allows to 
    tailor the modelling process.
    \cr
    
    The output of the \code{print}, \code{plot}, \code{summary}, and 
    \code{predict} methods have all the same style of format. This 
    makes it very easy to compare and to interpret the results 
    obtained from different algorithms implemented in different 
    functions.
    \cr
    
    All beside two of the underlying functions are available in a default
    R Installation. They are already available or loaded when Rmetrics 
    is invoked. For \code{MARS} and \code{POLYMARS} we have implemented
    BUILTIN functions for parameter estimation and prediction, so that it
    is becomes not necessary to install the contribute \code{mars} and
    \code{polyspline} packages. Nevertheless, if loaded you can use them 
    in your environment, they don't intervene with Rmetrics.
    \cr
    
    For further information we refer to the original help pages of the
    functions 
    \code{lm}, 
    \code{glm},
    \code{gam},
    \code{ppr},
    \code{mars},
    \code{polymars}, and
    \code{nnet}. 
    
}


\usage{
regSim(model = c("LM3", "LOGIT3", "GAM3"), n = 100)

regFit(formula, data, use = c("lm", "rlm", "am", "ppr", "mars", "nnet", 
    "polymars"), title = NULL, description = NULL, \dots)
gregFit(formula, family, data, use = c("glm", "gam"), 
    title = NULL, description = NULL, \dots)
    
\method{predict}{fREG}(object, newdata, se.fit = FALSE, type = "response", \dots)

show.fREG(object)
\method{plot}{fREG}(x, \dots)
\method{summary}{fREG}(object, \dots)

\method{coef}{fREG}(object, \dots)
\method{fitted}{fREG}(object, \dots)
\method{residuals}{fREG}(object, \dots)
\method{vcov}{fREG}(object, \dots)

OLS(formula, data, \dots)

\method{print}{OLS}(x, \dots)
\method{plot}{OLS}(x, \dots)
\method{summary}{OLS}(object, \dots)
}


\arguments{
    
    \item{data, newdata}{
        \code{data} is the data frame containing the variables in the 
        model. By default the variables are taken from 
        \code{environment(formula)}, typically the environment from 
        which \code{LM} is called. \code{newdata} is the data frame 
        from which to predict.
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
        \code{"MARS"} for multivariate adaptive regression splines,\cr
        \code{"POLYMARS"} for molytochomous MARS, and\cr
        \code{"NNET"} for feedforward neural network modelling.
        }
    \item{model}{
        [regSim] - \cr
        a character string selecting one from three two-dimensional
        benchnmark models: \code{"LM2"}, \code{"LOGIT2"}, or
        \code{"GAM2"}.
        }
    \item{n}{
        [regSim] - \cr
        an integer value setting the length of the series to be simulated. 
        The default value is 100.
        }
    \item{object, x}{
        [regFit] - \cr
        is an object returned by the regression function \code{regFit} 
        and serves as input for the  \code{predict}, \code{print}, 
        \code{summary}, \code{print.summary}, and \code{plot} methods. 
        Some methods allow for additional arguments to be passed.
        \cr
        [OLS] - \cr
        is an object returned by the 'OLS' function \code{OLS} 
        and serves as input for the  \code{print}, \code{plot}, 
        and \code{summary} methods.
        } 
   \item{se.fit}{
        [predict] - \cr
        ...
        }    
    \item{title}{
        a character string which allows for a project title.
        }
    \item{type}{
        a character string, the type of prediction.
        }
    \item{\dots}{
        additional optional arguments to be passed to the underlying 
        functions. For details we refer to inspect the following help 
        pages: \code{\link{lm}}, \code{\link{glm}}, \code{gam},
        \code{\link{ppr}}, \code{mars}, \code{polymars}, 
        or \code{nnet}. 
        }
        
}


\value{

    \bold{Function regFit:} 
    \cr
    returns an S4 object of class \code{"fREG"}, with the folliwing 
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
        regression model function, e.g.\cr
        \code{fit$parameters} - the fitted model parameters,\cr
        \code{fit$residuals} - the model residuals, \cr
        \code{fit$fitted.values} - the fitted values of the model,\cr
        and many more. For details we refer to the help pages of
        the selected regression model. 
        }
    \item{method}{
        the selected regression model naming the applied method.
        }
    \item{formula}{
        the formula expression describing the model.
        }
    \item{family}{
        the selected family and link name if available, otherwise
        a string vector with to empty strings.
        }
    \item{parameters}{
        named parameters or coefficients of the fitted model.
        }
    \item{title}{
        a title string.
        }
            
    \bold{Methods:}
    \cr\cr
    
    The output from the \code{print} method gives information at 
    least about the function call, the fitted model parameters,
    and the residuals variance.
    \cr\cr
    
    The \code{plot} method produces three figures, the first plots
    the series of residuals, the second does a quantile-quantile plot
    of the residual plot, and the third plots the fitted values vs.
    the residuals. Additional plots can be generated from the plot
    method (if available) of the underlying model, see the example below. 
    \cr\cr
    
    The \code{summary} method provides additional information,
    like errors on the model parameters as far as available, and adds 
    additional information about the fit.  
    \cr\cr
    
    The \code{predict} method forecasts from a fitted model. The
    returned values are the same as produced by the prediction
    function of the selected regression model. Especially, \code{$fit} 
    returns the forecast vector.
    \cr\cr
     
    The \code{residuals} and \code{fitted.values} methods return
    the residuals and the fitted values as numeric vectors.
    \cr
    
    \bold{Function OLS:} 
    \cr
    returns an S3 object of class \code{"OLS"} that represents an 
    ordinary least squares fit. The list has the same elements like
    an object of class \code{"lm"}, and additionally the elements
    \code{$call}, \code{$formula} and \code{$data}. 

}


\details{

    \bold{LM -- Linear Modelling:}
    \cr\cr  
    Univariate linear regression analysis is a statistical methodology 
    that assumes a linear relationship between some predictor variables 
    and a response variable. The goal is to estimate the coefficients 
    and to predict new data from the estimated linear relationship.
    The function \code{plot.lm} provides four plots: a plot of residuals 
    against fitted values, a Scale-Location plot of sqrt{| residuals |} 
    against fitted values, a normal QQ plot, and a plot of Cook's 
    distances versus row labels.
    \cr 
    \code{[stats:lm]}
    \cr

    \bold{GLM -- Generalized Linear Models:}
    \cr\cr
    Generalized linear modelling extends the linear model in two directions.
    (i) with a monotonic differentiable link function describing how the 
    expected values are related to the linear predictor, and (ii) with 
    response variables having a probability distribution from an exponential 
    family. 
    \cr
    \code{[stats:glm]}
    \cr
    
    \bold{GAM -- Generalized Additive Models:}
    \cr\cr  
    An additive model generalizes a linear model by smoothing individually
    each predictor term. A generalized additive model extends the additive
    model in the same spirit as the generalized liner amodel extends the 
    linear model, namely for allowing a link function and for allowing 
    non-normal distributions from the exponential family. 
    \cr
    \code{[mgcv:gam]}
    \cr
    
    \bold{PPR -- Projection Pursuit Regression:}
    \cr\cr  
    The basic method is given by Friedman (1984), and is essentially 
    the same code used by S-PLUS's \code{ppreg}. It is observed that 
    this code is extremely sensitive to the compiler used. The algorithm 
    first adds up to \code{max.terms}, by default \code{ppr.nterms},
    ridge terms one at a time; it will use less if it is unable to find 
    a term to add that makes sufficient difference. The levels of 
    optimization (argument \code{optlevel}), by default 2, differ in 
    how thoroughly the models are refitted during this process.
    At level 0 the existing ridge terms are not refitted.  At level 1
    the projection directions are not refitted, but the ridge
    functions and the regression coefficients are. Levels 2 and 3 refit 
    all the terms; level 3 is more careful to re-balance the contributions
    from each regressor at each step and so is a little less likely to
    converge to a saddle point of the sum of squares criterion. The 
    \code{plot} method plots Ridge functions for the projection pursuit 
    regression fit. 
    \cr
    \code{[stats:ppr]}
    \cr
    
    \bold{MARS -- Multivariate Adaptive Regression Splines:}
    \cr\cr
    This function was coded from scratch, and did not use any of
    Friedman's mars code.  It gives quite similar results to  Friedman's
    program in our tests, but not exactly the same results.  We have not 
    implemented Friedman's anova decomposition nor are categorical
    predictors handled properly yet.  Our version does handle multiple
    response variables, however.  As it is not well-tested, we would like
    to hear of any bugs. 
    \cr
    Additional arguments which can be passed to the \code{"mars"}
    estimator are:
    \cr
    \code{w} - an optional vector of observation weights.
    \cr 
    \code{wp} - an optional vector of response weights. 
    \cr
    \code{degree} - an optional integer specifying maximum interaction 
    degree, default is 1. 
    \cr
    \code{nk} - an optional integer specifying the maximum number of model 
    terms. 
    \cr
    \code{penalty} - an optional value specifying the cost per degree of 
    freedom charge, default is 2.
    \cr 
    \code{thresh} - an optional value specifying forward stepwise stopping 
    threshold, default is 0.001. 
    \cr
    \code{prune} - an optional logical value specifying whether the model 
    should be pruned in a backward stepwise fashion, default is \code{TRUE}. 
    \cr
    \code{trace.mars} - an optional logical value specifying whether info 
    should be printed along the way, default is \code{FALSE}. 
    \cr
    \code{forward.step} - an optional logical value specifying whether 
    forward stepwise process should be carried out, default is \code{TRUE}. 
    \cr
    \code{prevfit} - optional data structure from previous fit. To see the 
    effect of changing the penalty paramater, one can use prevfit with 
    \code{forward.step = FALSE}. 
    \cr
    \code{[mda:mars]}
    \cr
    
    \bold{POLYMARS -- Polytochomous MARS:}
    \cr\cr
    The algorithm employed by \code{polymars} is different from the 
    MARS(tm) algorithm of Friedman (1991), though it has many similarities. 
    Also the name \code{polymars} has been used for this algorithm well 
    before MARS was trademarked. 
    %Two of the main differences are:
    %\code{polymars} requires linear terms of a predictor to be in the model
    %before nonlinear terms using the same predictor can be added; and
    %\code{polymars} requires a univariate basis function to be in the model
    %before a tensor-product basis function involving the univariate ...
    \cr
    Additional arguments which can be passed to the \code{"polymars"}
    estimator are:
    \cr
    \code{maxsize} - the maximum number of basis functions that the model is 
        allowed to grow to in the stepwise addition procedure. Default is 
        \eqn{\min(6*(n^{1/3}),n/4,100)}, where \code{n} is the number of 
        observations. 
        \cr
    \code{gcv} - parameter used to find the overall best model from a 
        sequence of fitted models. The residual sum of squares of a model 
        is penalized by dividing by the square of 
        \code{1-(gcv x model size)/cases}.   
        A larger gcv value would tend to produce a smaller model.
        \cr 
    \code{additive} - Should the fitted model be additive in the predictors? 
        \cr 
    \code{startmodel} - the first model that is to be fit by \code{polymars}. 
        It is either an object of the class \code{polymars} or a model 
        dreamed up by the user. In that case, it takes the form of a 
        \code{4 x n} matrix, where \code{n} is the  number of basis 
        functions in the starting model excluding the intercept. Each 
        row corresponds to one basis function (with two possible components). 
        Column 1 is the index of the first predictor involved. Column 2 is 
        a possible knot in this predictor. If column 2 is \code{NA}, the 
        first component is linear. Column 3 is the possible second predictor 
        involved (if column 3 is \code{NA} the basis function only depends 
        on one predictor). Column 4 contains the possible knot for the 
        predictor in column 3, and it is \code{NA} when this component is 
        linear.  Example: if a row reads \code{3 NA 2 4.7}, the corresponding 
        basis function is \eqn{[X_3 * (X_2-4.7)_+]}; if a row reads 
        \code{2 4.3 NA NA} the corresponding basis function is 
        \eqn{[(X_2-4.3)_+]}. 
        A fifth column can be added with 1s and 0s, The 1s specify which 
        basis functions of the startmodel must be in each model. Thus, these 
        functions stay in the model during the whole stepwise fitting 
        procedure. If \code{startmodel} is not specified \code{polymars} 
        starts with a model that only contains  the intercept. 
        \cr
    \code{weights} - optional vector of observation weights; if supplied, 
        the algorithm fits to minimize the sum of the weights multiplied 
        by the squared residuals. The length of weights must be the same 
        as the number of observations. The weights must be nonnegative. 
        \cr
    \code{no.interact} - an optional matrix used if certain predictor 
        interactions are not allowed in the model. It is given as a 
        matrix of size \code{2 x m}, with predictor indices as entries.  
        The two predictors of any row cannot have interaction terms with 
        each other. 
        \cr
    \code{knots} - defines how the function is to find potential knots 
        for the spline basis functions.  This can be set to the maximum 
        number of knots you would like to be considered for each predictor. 
        Usually, to avoid the design matrix becoming singular the actual 
        number of knots produced is constrained to at most every third 
        order statistic in any predictor. This constraint can be adjusted 
        using the \code{knot.space} argument. It can also be a vector with 
        the number of potential knots for each predictor. Again the actual 
        number of knots produced is constrained to be at most every 
        third order statistic any predictor.  
        A third possibility is to provide a matrix where each columns 
        corresponds to the ordered knots you would like to have considered 
        for that predictor. 
        This matrix should be filled out to a rectangular data structure 
        with NAs. 
        The default is \code{min(20, round(n/4))} knots per predictor. 
        When specifying knots as a vector an entry of \code{-1} indicates 
        that the predictor is a categorical variable and each unique entry 
        in it's column is treated as a  level. 
        When specifying knots as a single number or a matrix and there are 
        categorical variables these are specified separately as such using 
        the factor argument. 
        \cr
    \code{knot.space} - is an integer describing the minimum number of 
        order statistics apart that two knots can be. Knots should not 
        be too close to insure numerical stability. 
        \cr
    \code{ts.resp} - testset responses for model selection. Should have 
        the same number of columns as the training set response. A testset 
        can be used for the model selection. Depending on the value of 
        classify, either the model with the smallest testset residual 
        sum of squares or the smallest testset classification error is 
        provided. Overrides \code{gcv}. 
        \cr
    \code{ts.pred} - testset predictors. Should have the same number of 
        columns as the training set predictors. 
        \cr
    \code{ts.weights} -
        testset observation weights. A vector of length equal to the number 
        of cases of the testset. All weights must be non-negative. 
        \cr
    \code{classify} - when the response is discrete (categorical), polymars 
        can be used for classification. In particular, when 
        \code{classify = TRUE}, a discrete response with \code{K} levels 
        is replaced by \code{K} indicator variables as response. Model 
        selection is still being carried out using gcv, except when a 
        testset is provided, in which case testset misclassification is 
        used to select the best model. 
        \cr
    \code{factors} - used to indicate that certain variables in the predictor 
        set are categorical variables. Specified as a vector containing the 
        appropriate predictor indices (column numbers of categorical 
        variables in predictors matrix). Factors can also be set when the 
        \code{knots} argument is given as a vector, with \code{-1} as  
        the appropriate entries for factors. 
        \cr
    \code{tolerance} - for each possible candidate to be added/deleted 
        the resulting residual sums of squares of the model, with/without 
        this candidate, must be calculated. The inversion of of 
        the "X-transpose by X" matrix, X being the design matrix,  
        is done by an updating procedure c.f. C.R. Rao - Linear 
        Statistical Inference and Its Applications, 2nd. edition, page 33.   
        In the inversion the size of the bottom right-hand entry of this 
        matrix is critical. If it\code{s value is near zero or the value 
        of it}s inverse is almost zero then the  inversion procedure 
        becomes somewhat inaccurate. The lower the tolerance value the  
        more careful the procedure is in selecting candidates for addition 
        to the model but it may exclude too conservatively. And the other 
        hand if the tolerance is set too high a spurious result with a 
        singular or otherwise sub-optimal model may occur. By default 
        tolerance is set to 1.0e-5. 
        \cr
    \code{verbose} - when set  to \code{TRUE}, the function will print 
        out a line for each addition or deletion stage. For 
        example, " + 8 : 5 3.25 2 NA" means adding interaction basis 
        function of predictor 5 with knot at 3.25 and predictor 2 (linear), 
        to make a model of size 8, including intercept. 
    \cr
    \code{[polyclass:polymars]}
    \cr\cr
    
    \bold{NNET -- Feedforward Neural Network Regression:}
    \cr\cr  
    If the response in \code{formula} is a factor, an appropriate 
    classification network is constructed; this has one output and 
    entropy fit if the number of levels is two, and a number of 
    outputs equal to the number of classes and a softmax output 
    stage for more levels. If the response is not a factor, it is 
    passed on unchanged to \code{nnet.default}. A quasi-Newton 
    optimizer is used, written in \code{C}. 
    \cr
    \code{[nnet:nnet]}
    \cr\cr
    
    \bold{OLS -- Ordinary Least Square Fit:}
    \cr\cr
    This function was introduced to mimc the Finmetrics S-Plus
    function \code{OLS}. The function wraps R's \code{"lm"}.
    Currently it does not support the full functionality of
    Finmetrics' \code{OLS} function.
    \cr
    
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
    T. Hastie, R. Tibshirani for the \code{mars} functions from R's \code{?} package, \cr
    M. O' Connors for the \code{polymars} functions from R's \code{?} package, \cr
    The R core team for the \code{nnet} functions from R's \code{nnet} package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
\dontrun{
## regFit -
   data(recession) 
   recession[,1] = paste(recession[,1], "28", sep = "")
   
## myPlot -
   myPlot = function(recession, in.sample) {
     recession = as.timeSeries(recession)[, "recession"]
     in.sample = as.timeSeries(recession)[, "recession"]
     Date = recession[, "date"]
     Date = trunc(Date/100) + (Date-100*trunc(Date/100))/12
     Recession = recession[, "recession"]
     inSample = as.vector(in.sample)
     plot(Date, Recession, type = "n", main = "US Recession")
     grid()
     lines(Date, Recession, type = "h", col = "steelblue")
     lines(Date, inSample) 
   }
   
## Generalized Additive Modelling:
   require(mgcv)
   par(mfrow = c(2, 2))
   fit = gregFit(formula = recession ~ s(tbills3m) + s(tbonds10y),
     family = gaussian(), data = recession, use = "gam")
   # In Sample Prediction:
   in.sample = predict(fit, newdata = recession)$fit  
   myPlot(recession, in.sample)
   # Summary:
   summary(fit)
   # Add plots from the original plot method:
   gam.fit = fit@fit
   class(gam.fit) = "gam"
   plot(gam.fit)
}
}


\keyword{models}

