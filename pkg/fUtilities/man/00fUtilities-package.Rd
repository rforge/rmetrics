\name{fUtilities-package}


\alias{fUtilities-package}


\alias{fUtilities}


\docType{package}


\title{Utilities and Tools Package}


\description{

    Package of basic utilities and general tools for Rmetrics.
}


\details{

    \tabular{ll}{
        Package:    \tab fUtilities\cr
        Type:       \tab Package\cr
        Version:    \tab 261.73.1\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }

}


\section{Overview of Topics:}{

    Basic Function Extensions\cr
    Column and Row Statistics for Rectangular Objects\cr
    Skewness and Kurtosis Statistics\cr
    Bivariate Interpolation and Kriging\cr
    Code Tables, Color Selection and Palettes\cr
    Vector/Matrix Arithmetics and Linear Algebra Addons\cr
    Special Functions Addon\cr
    
    
}  


\section{Basic Function Extensions}{

    Several functions are added by Rmetrics which are missing 
    in R's basic packages.
    
    The first set of these functions are concerned with R functions
    which were made generic, so that they could be used to add
    additional methods:
    
    \tabular{ll}{
    \code{align} \tab adds align function, \cr
    \code{as.POSIXlt} \tab adds POSIXlt function, \cr                
    \code{atoms} \tab adds atoms function, \cr
    \code{attach} \tab extends attach function, \cr
    \code{colnames<-} \tab adds colnames assignment, \cr
    \code{cor} \tab extends cor function, \cr
    \code{cov} \tab extends cov function, \cr
    \code{log} \tab extends log function, \cr
    \code{outlier} \tab adds outlier function, \cr
    \code{rank} \tab extends rank function, \cr
    \code{rownames<-} \tab adds rownames assigment, \cr
    \code{sample} \tab extends sample function, \cr
    \code{stdev} \tab adds stdev function, \cr
    \code{termPlot} \tab adds term plot function, \cr
    \code{var} \tab extends var function, \cr
    \code{volatility} \tab adds volatility function.}
    
    All these functions have now default methods. Furthermore
    the \code{fUtilities} package also provides additional methods.
    These include:
    
    \tabular{ll}{
    \code{as.matrix.ts} \tab adds as.matrix.ts method, \cr
    \code{as.matrix.mts} \tab adds as.matrix.mts method, \cr
    \code{print.control} \tab adds print.control method.}
    
}


\section{Statistical Function Extensions}{   
    
    Beside the basic function extensions also statistical function
    extensions are provided. This concerns the missing
    "skewness" and "kurtosis" functions in R
    
    \tabular{ll}{
    \code{skewness} \tab returns value of skewness, \cr
    \code{kurtosis} \tab returns value of kurtosis.}
    
    and functions for column and row statistics. The \code{colStats} 
    and \code{rowStats} are quite general functions which allow to 
    specify the function to compute the desired statistics by the user.
    The remaining column and row statistics functions are thought to
    compute often used time series statistics. This includes the sum(,
    mean, standard deviations, variance, skewness, kurtosis, maximum,
    minimum, product, and quantile value.
    
    Column Statistics:
    
    \tabular{ll}{  
    \code{colStats}\tab calculates column statistics, \cr
    \code{colSums} \tab calculates column sums, \cr
    \code{colMeans} \tab calculates column means, \cr
    \code{colSds} \tab calculates column standard deviations, \cr
    \code{colVars} \tab calculates column variances, \cr 
    \code{colSkewness} \tab calculates column skewness, \cr
    \code{colKurtosis} \tab calculates column kurtosis, \cr
    \code{colMaxs} \tab calculates maximum values in each column, \cr
    \code{colMins} \tab calculates minimum values in each column, \cr
    \code{colProds} \tab computes product of all values in each column, \cr
    \code{colQuantiles} \tab computes quantiles of each column. }

    Row Statistics:

    \tabular{ll}{  
    \code{rowStats}\tab calculates row statistics, \cr
    \code{rowSums} \tab calculates row sums, \cr
    \code{rowMeans} \tab calculates row means, \cr
    \code{rowSds} \tab calculates row standard deviations, \cr
    \code{rowVars} \tab calculates row variances, \cr 
    \code{rowSkewness} \tab calculates row skewness, \cr
    \code{rowKurtosis} \tab calculates row kurtosis, \cr
    \code{rowMaxs} \tab calculates maximum values in each row, \cr
    \code{rowMins} \tab calculates minimum values in each row, \cr
    \code{rowProds} \tab computes product of all values in each row, \cr
    \code{rowQuantiles} \tab computes quantiles of each row. }
    

    For hypothesis testing Rmetrics offers a new S4 class and
    print method:
    
    \tabular{ll}{  
    \code{fHTEST} \tab Representation for an S4 object of class "fHTEST", \cr
    \code{show} \tab S4 print method.}
    
    
}
    

\section{Graph and Plot Tools}{


    Character, symbol and color tables are useful tools
    if one is concerned with graphs and charts:
    
    \tabular{ll}{
    \code{characterTable} \tab Table of Numerical Equivalents to Latin Characters, \cr
    \code{symbolTable} \tab Table of plot characters, plot symbols, \cr
    \code{colorTable} \tab Table of Color Codes and Plot Colors itself, \cr
    \code{colorLocator} \tab Plots R's 657 named colors for selection, \cr
    \code{colorMatrix} \tab Returns matrix of R's color names. }
       
    Many wrapper functions to create color palettes are also added, 
    all following the same naming conventions:

    \tabular{ll}{
    \code{rainbowPalette} \tab Contiguous rainbow color palette, \cr
    \code{heatPalette} \tab Contiguous heat color palette, \cr
    \code{terrainPalette} \tab Contiguous terrain color palette, \cr
    \code{topoPalette} \tab Contiguous topo color palette, \cr
    \code{cmPalette} \tab Contiguous cm color palette, \cr
    \code{greyPalette} \tab R's gamma-corrected gray palette, \cr
    \code{timPalette} \tab Tim's Matlab like color palette, \cr
    \code{rampPalette} \tab Color ramp palettes, \cr
    \code{seqPalette} \tab Sequential color brewer palettes, \cr
    \code{divPalette} \tab Diverging color brewer palettes, \cr
    \code{qualiPalette} \tab Qualified color brewer palettes, \cr
    \code{focusPalette} \tab Red, green blue focus palettes, \cr
    \code{monoPalette} \tab Red, green blue mono palettes. }
    
    An interactive plot function allows to create easily interactive
    plots:
    
    \tabular{ll}{
    \code{interactivePlot} \tab a framework for interactive plot displays.}
    
}


\section{Bivariate Interpolation and Kriging}{
    
    Functions which allow to interpolate and smooth bivariate irregular 
    data sets including linear interpolation, Akima spline interpolation, 
    and kriging:

    \tabular{ll}{
    \code{linearInterp} \tab performs linear spline interpolation, \cr
    \code{akimaInterp} \tab performs Akima spline interpolation, \cr
    \code{krigeInterp} \tab performs krige interpolation.}
    
}


\section{Vector/Matrix Arithmetics and Linear Algebra Addons}{
    
    Functions for matrix arithmetics and linear algebra. These functions 
    are often  very useful for the manipulation of the data slot of 
    multivariate financial time series.
    
    General Matrix Functions:
    
    \tabular{ll}{  
    \code{triang} \tab Extracs the lower tridiagonal part from a matrix, \cr
    \code{Triang} \tab Extracs the upper tridiagonal part from a matrix, \cr
    \code{pascal} \tab Creates a Pascal matrix, \cr
    \code{hilbert} \tab Creates a Hilbert matrix, \cr
    \code{colVec} \tab Creates a column vector from a vector, \cr
    \code{rowVec} \tab Creates a row vector from a vector, \cr
    \code{isPositiveDefinite} \tab Checks if a matrix is positive definite, \cr
    \code{makePositiveDefinite} \tab Forces a matrix to be positive definite, \cr
    \code{colIds} \tab Retrieves or sets the colnames of an object, \cr
    \code{rowIds} \tab Retrieves or sets the rowumn names.}
    
    Linear algebra functions in R's base package include the
    \code{\%*\%} product of two matrices, the
    \code{\%x\%} Kronecker product, the
    \code{det} determinant of a matrix, and the
    \code{t} transposed matrix. 
    
    Rmetrics adds the foloowing functions:
        
    \tabular{ll}{
    \code{inv} \tab Returns the inverse of a matrix, \cr
    \code{norm} \tab Returns the norm of a matrix, \cr 
    \code{rk} \tab Returns the rank of a matrix, \cr
    \code{tr} \tab Returns trace of a matrix, \cr 
    \code{vech} \tab Is the operator that stacks the lower triangle, \cr
    \code{vec} \tab Is the operator that stacks a matrix.}
    
    Note, additional linear algebra functionality is provided in R 
    through the functions
    \code{chol} which returns the Cholesky factor matrix,
    \code{eigen} which computes eigenvalues and eigenvectors,  
    \code{svd} which does singular value decomposition,
    \code{kappa} which determines the condition number of a matrix, 
    \code{qr} which performs the QR decomposition of a matrix, 
    \code{solve} which solves a system of linear equations, 
    together with the functions
    \code{backsolve} used when the matrix is upper triangular, and
    \code{forwardsolve} used when the matrix is lower triangular.
        
}
    
    
\section{Time Series Generation}{

    For the computation of lagged or leading series the following
    two functions are provided by Rmetrics:
    
    \tabular{ll}{
    \code{tslag} \tab Lagged or leading vector/matrix of selected order(s), \cr
    \code{pdl} \tab Regressor matrix for polynomial distributed lags. } 
     
}


\section{Special Functions Addon}{

    Functions which compute special functions missing in R's base
    package include:

    Heaviside and Related Functions:
    
    \tabular{ll}{
    \code{Heaviside} \tab Computes Heaviside unit step function, \cr
    \code{Sign} \tab Just another signum function, \cr
    \code{Delta} \tab Computes delta function, \cr
    \code{Boxcar} \tab Computes boxcar function, \cr
    \code{Ramp} \tab Computes ramp function. }
            
    Generator for Portable Random Innovations:
    
    \tabular{ll}{
    \code{set.lcgseed} \tab Set initial random seed, \cr
    \code{get.lcgseed} \tab Get the current valus of the random seed, \cr
    \code{runif.lcg} \tab Uniform linear congruational generator, \cr
    \code{rnorm.lcg} \tab Normal linear congruational generator, \cr
    \code{rt.lcg} \tab Student-t linear congruential generator. }
    
}


\section{Some utility functions}{

    Finally we like to mention some further utility functions:

    \tabular{ll}{
    \code{gridVector} \tab creates from two vectors x and y all grid points.}
  
}  


\author{

    The \code{fUtilities} package was originally written by Diethelm Wuertz
    and is since 2007 maintained and further developed by the Rmetrics
    core team.

}


\keyword{package}

