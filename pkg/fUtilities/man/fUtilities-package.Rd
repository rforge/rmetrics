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

    Functions which add and/or modify S function 
    functionality:
    
    Extensions:
    
    \tabular{ll}{
    \code{align} \tab adds align function, \cr
    \code{as.POSIXlt} \tab adds as.POSIXlt function, \cr
    \code{atoms} \tab adds atoms function, \cr
    \code{attach} \tab extends attach function, \cr
    \code{cor} \tab extends cor function, \cr
    \code{cov} \tab extends cov function, \cr
    \code{log} \tab extends log function, \cr
    \code{outlier} \tab adds outlier function, \cr
    \code{rank} \tab extends rank function, \cr
    \code{sample} \tab extends sample function, \cr
    \code{stdev} \tab adds stdev function, \cr
    \code{var} \tab extends var function, \cr
    \code{volatility} \tab adds volatility function.}
    
    Assignment Functions:
    
    \tabular{ll}{
    \code{rownames<-} \tab adds rownames assigment, \cr
    \code{colnames<-} \tab adds colnames assignment, \cr
    \code{as.matrix.ts} \tab adds as.matrix.ts method, \cr
    \code{as.matrix.mts} \tab adds as.matrix.mts method, \cr
    \code{print.control} \tab adds print.control method. }
    
    Tests Class Representation and Utilities:

    \tabular{ll}{
    \code{fHTEST} \tab Representation for an S4 object of class "fHTEST", \cr
    \code{show} \tab S4 print method. }
        
    Interactive Plot Function:
    
    \tabular{ll}{
    \code{interactivePlot} \tab a framework for interactive plot displays, \cr
    \code{gridVector} \tab creates from two vectors x and y all grid points. }
    
}


\section{Column and Row Statistics for Rectangular Objects}{

    Functions to compute column and row statistical properties 
    of financial and economic time series data.
    
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

}


\section{Skewness and Kurtosis Statistics}{

    Functions to compute skewness and kurtosis:
 
    \tabular{ll}{
    \code{skewness} \tab returns value of skewness, \cr
    \code{kurtosis} \tab returns value of kurtosis. }

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


\section{Code Tables, Color Selection and Palettes}{
    
    Functions which are useful utilities and 
    tools in selecting characters, symbols and 
    colors for plotting.
    
    Functions to display code tables:
    
    \tabular{ll}{
    \code{characterTable} \tab Table of Numerical Equivalents to Latin Characters, \cr
    \code{symbolTable} \tab Table of plot characters, plot symbols, \cr
    \code{colorTable} \tab Table of Color Codes and Plot Colors itself. }
        
    Functions to display R's 657 named colors for selection:
    
    \tabular{ll}{
    \code{colorLocator} \tab Plots R's 657 named colors for selection, \cr
    \code{colorMatrix} \tab Returns matrix of R's color names. }
       
    Functions to create color palettes:

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
    
}


\section{Vector/Matrix Arithmetics and Linear Algebra Addons}{
    
    Functions for matrix arithmetics and linear algebra. These functions 
    are often  very useful for the manipulation of the data slot of 
    multivariate financial time series. The listing includes also functions 
    from R's base package.
 
    General Matrix Functions:
    
    \tabular{ll}{  
    \code{matrix} \tab Creates a matrix from the given set of values, \cr
    \code{diag} \tab Creates a diagonal matrix or extracts diagonals, \cr
    \code{triang} \tab Extracs the lower tridiagonal part from a matrix, \cr
    \code{Triang} \tab Extracs the upper tridiagonal part from a matrix, \cr
    \code{pascal} \tab Creates a Pascal matrix, \cr
    \code{hilbert} \tab Creates a Hilbert matrix, \cr
    \code{colVec} \tab Creates a column vector from a vector, \cr
    \code{rowVec} \tab Creates a row vector from a vector, \cr
    \code{as.matrix} \tab Attempts to turn its argument into a matrix, \cr
    \code{is.matrix} \tab Tests if its argument is a (strict) matrix, \cr
    \code{isPositiveDefinite} \tab Checks if a matrix is positive definite, \cr
    \code{makePositiveDefinite} \tab Forces a matrix to be positive definite, \cr
    \code{dimnames} \tab Retrieves or sets the dimnames of an object, \cr
    \code{colnames} \tab Retrieves or sets the column names, \cr 
    \code{colIds} \tab ... use alternatively, \cr 
    \code{rownames} \tab Retrieves or sets the row names, \cr   
    \code{rowIds} \tab ... use alternatively. }
    
    Simple Matrix Operations:
            
    \tabular{ll}{ 
    \code{dim} \tab Returns the dimension of a matrix object, \cr
    \code{ncol} \tab Counts columns of a matrix object, \cr 
    \code{nrow} \tab Counts rows of a matrix object, \cr 
    \code{length} \tab Counts elements of a matrix object, \cr 
    \code{"["} \tab Subsets a matrix object, \cr
    \code{"[["} \tab Subsets a matrix object, \cr
    \code{cbind} \tab Augments a matrix object by columns, \cr 
    \code{rbind} \tab Augments a matrix object by rows. } 

    Basic Statistical Functions:
            
    \tabular{ll}{
    \code{var} \tab Returns the variance matrix, \cr
    \code{cov} \tab Returns the covariance matrix, \cr 
    \code{colStats} \tab Calculates column statistics, \cr
    \code{rowStats} \tab Calculates row statistics, \cr
    \code{colMeans} \tab Calculates column means, \cr 
    \code{rowMeans} \tab Calculates row means, \cr 
    \code{colAvgs} \tab Calculates column averages, \cr
    \code{rowAvgs} \tab Calculates row averages, \cr
    \code{colVars} \tab Calculates column variances, \cr 
    \code{rowVars} \tab Calculates row variances, \cr 
    \code{colStdevs} \tab Calculates column standard deviations, \cr
    \code{rowStdevs} \tab Calculates row standard deviations, \cr
    \code{colSkewness} \tab Calculates column skewness, \cr
    \code{rowSkewness} \tab Calculates row skewness, \cr
    \code{colKurtosis} \tab Calculates column kurtosis, \cr 
    \code{rowKurtosis} \tab Calculates row kurtosis, \cr 
    \code{colCumsums} \tab Calculates column cumulated sums, \cr  
    \code{rowCumsums} \tab Calculates row cumulated sums. }  
            
    Linear algebra:
    
    \tabular{ll}{
    \code{\%*\%} \tab Returns the product of two matrices, \cr
    \code{\%x\%}, \code{kron} \tab Returns the Kronecker product, \cr 
    \code{det} \tab Returns the determinante of a matrix, \cr
    \code{inv} \tab Returns the inverse of a matrix, \cr
    \code{norm} \tab Returns the norm of a matrix, \cr 
    \code{rk} \tab Returns the rank of a matrix, \cr
    \code{tr} \tab Returns trace of a matrix, \cr 
    \code{t} \tab Returns the transposed matrix, \cr
    \code{vech} \tab Is the operator that stacks the lower triangle, \cr
    \code{vec} \tab Is the operator that stacks a matrix.}
    
    More linear algebra:
    
    \tabular{ll}{
    \code{chol} \tab Returns the Cholesky factor matrix, \cr
    \code{eigen} \tab Returns eigenvalues and eigenvectors, \cr 
    \code{svd} \tab Returns the singular value decomposition, \cr
    \code{kappa} \tab Returns the condition number of a matrix, \cr 
    \code{qr} \tab Returns the QR decomposition of a matrix, \cr
    \code{solve} \tab Solves a system of linear equations, \cr
    \code{backsolve} \tab ... use when the matrix is upper triangular, \cr
    \code{forwardsolve} \tab ... use when the matrix is lower triangular. } 
    
    Time Series Generation:
    
    \tabular{ll}{
    \code{tslag} \tab Lagged or leading vector/matrix of selected order(s), \cr
    \code{pdl} \tab Regressor matrix for polynomial distributed lags. } 
     
}


\section{Special Functions Addon}{

    Functions which compute special functions missing in R's base
    package.

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



\keyword{package}

