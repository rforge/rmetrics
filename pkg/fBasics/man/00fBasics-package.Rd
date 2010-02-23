\name{fBasics-package}


\alias{fBasics}


\docType{package}


\title{Portfolio Modelling, Optimization and Backtesting}


\description{

    The Rmetrics "fBasics" package is a collection of functions to 
    explore and to investigate basic properties of financial returns 
    and related quantities.
    
    The covered fields include techniques of explorative data analysis
    and the investigation of distributional properties, including
    parameter estimation and hypothesis testing. Evenmore there are
    several utility functions for data handling and managemnet.
    
}


\details{

    \tabular{ll}{
        Package:    \tab fBasics\cr
        Type:       \tab Package\cr
        Version:    \tab 270.73\cr
        Date:       \tab 2009\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2010 Rmetrics Assiciation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
}


\section{A. Overview:}{

    1. Basic Statistics Functions
    
    2. Financial Return Distributions
    
    3. Hypothesis Testing
    
    4. Plotting Routines
        
    5. Matrix Computations and Linear Algebra

    6. Utility Functions for Data Management
    
    7. Other Utility Functions   
}


   
\section{1. Basic Statistics Functions}{


    \bold{Financial Return Statistics:}

        \tabular{ll}{ 
        \emph{Function ________________} 
        \tab Description _________________________________________\cr
            \code{basicStats} \tab Returns a basic statistics summary. \cr
            \tab \emph{File: stats-basicStats.R}  \cr
            
        \tab \cr
        \emph{Function ________________} 
        \tab Description _________________________________________\cr
            \code{dmaxdd} \tab Density function of mean Max-Drawdowns \cr
            \code{pmaxdd} \tab Probability function of mean Max-Drawdowns \cr
            \code{rmaxdd} \tab Random Variates of mean Max-Drawdowns \cr
            \code{maxddStats} \tab Expectation of Drawdowns for BM with drift. \cr
            \tab \emph{File: stats-maxdd.R} }
          
              
    \bold{Calculation of Sample Moments:}
    
        \tabular{ll}{
        \emph{Function ________________} 
        \tab Description _________________________________________\cr    
            \code{sampleLmoments} \tab Computes sample L-moments. \cr 
            \tab \emph{File: stats-sampleLMoments.R} \cr
            
        \tab \cr   
        \emph{Function ________________} 
        \tab Description _________________________________________\cr
            \code{sampleMED} \tab Returns sample median \cr
            \code{sampleIQR} \tab returns sample inter quartal range \cr
            \code{sampleSKEW} \tab returns robust sample skewness \cr
            \code{sampleKURT} \tab returns robust sample kurtosis. \cr
            \tab \emph{File: stats-sampleRobMoments.R} }
        
        
    \bold{Bivariate Interpolation:}
       
        \tabular{ll}{ 
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{akimaInterp} \tab Interpolates irregularly spaced points \cr
            \code{akimaInterpp} \tab Interpolates and smoothes pointwise. \cr         
            \tab \emph{File: stats-interpAkima.R} \cr
            
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{krigeInterp} \tab Kriges irregularly spaced data points. \cr 
            \tab \emph{File: stats-interpKrige.R} \cr            
        
        \tab \cr 
        \emph{Function ________________}
        \tab Description _________________________________________\cr 
            \code{linearInterp} \tab Interpolates irregularly spaced points \cr
            \code{linearInterpp} \tab Interpolates linearly pointwise. \cr
            \tab \emph{File: stats-interpLinear.R} }
        
            
    \bold{Utility Functions:}
    
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr 
            \code{colStats} \tab Computes sample statistics by col \cr
            \code{colSums} \tab Computes sums of values in each col \cr
            \code{colMeans} \tab Computes means of values in each col \cr
            \code{colSds} \tab Computes standard deviation of each col \cr
            \code{colVars} \tab Computes sample variance by col \cr
            \code{colSkewness} \tab Computes sample skewness by col \cr
            \code{colKurtosis} \tab Computes sample kurtosis by col \cr
            \code{colMaxs} \tab Computes maximum values in each col \cr
            \code{colMins} \tab Computes minimum values in each col \cr
            \code{colProds} \tab Computes product of values in each col \cr
            \code{colQuantiles} \tab Computes product of values in each col. \cr
            \tab \emph{File: has been moved tp package timeSeries} \cr
            
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr 
            \code{rowStats} \tab Computes sample statistics by row \cr
            \code{rowSums} \tab Computes sums of values in each row \cr
            \code{rowMeans} \tab Computes means of values in each row \cr
            \code{rowSds} \tab Computes standard deviation of each row \cr
            \code{rowVars} \tab Computes sample variance by row \cr
            \code{rowSkewness} \tab Computes sample skewness by row \cr
            \code{rowKurtosis} \tab Computes sample kurtosis by row \cr
            \code{rowMaxs} \tab Computes maximum values in each row \cr
            \code{rowMins} \tab Computes minimum values in each row \cr
            \code{rowProds} \tab Computes product of values in each row \cr
            \code{rowQuantiles} \tab Computes product of values in each row. \cr
            \tab \emph{File: stats-rowStats.R} }
        
}


\section{2. Financial Return Distributions}{

   
    \bold{Generalized Hyperbolic Distribution:}
    
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr 
            \code{dgh} \tab Returns density for the GH distribution \cr
            \code{pgh} \tab returns probability for the GH distribution \cr
            \code{qgh} \tab returns quantiles for the GH distribution \cr
            \code{rgh} \tab returns random variates for the GH distribution. \cr
            \tab \emph{File: dist-gh.R} \cr
            
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghFit} \tab Fits parameters of the GH distribution. \cr
            \tab \emph{File: dist-ghFit.R} \cr
            
        \tab \cr
        \emph{Function: ________________} 
        \tab Description _________________________________________\cr
            \code{ghMode} \tab Computes mode of the GH distribution. \cr 
            \tab \emph{File: dist-ghMode.R} \cr
            
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghMean} \tab Returns true mean of the GH distribution \cr
            \code{ghVar} \tab  returns true variance of the GH distribution \cr
            \code{ghSkew} \tab returns true skewness of the GH distribution \cr
            \code{ghKurt} \tab returns true kurtosis of the GH distribution \cr
            \code{ghMoments} \tab returns true n-th moment of the GH distribution. \cr
            \tab \emph{File: dist-ghMoments.R} \cr
            
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr 
            \code{ghMED} \tab Returns true median of te GH distribution \cr
            \code{ghIQR} \tab returns true inter quartal range of te GH \cr
            \code{ghSKEW} \tab returns true robust skewness of te GH \cr
            \code{ghKURT} \tab returns true robust kurtosis of te GH. \cr
            \tab \emph{File: dist-ghRobMoments.R} }
        
        
    \bold{Hyperbolic Distribution:}
    
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dhyp} \tab Returns density for the HYP distribution \cr
            \code{phyp} \tab returns probability for the HYP distribution \cr
            \code{qhyp} \tab returns quantiles for the HYP distribution \cr
            \code{rhyp} \tab returns random variates for the HYP distribution. \cr
            \tab \emph{File: dist-hyp.R} \cr  

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{hypFit} \tab Fits parameters of the HYP distribution.\cr
            \tab \emph{File: dist-hypFit.R} \cr 
             
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{hypMode} \tab Computes mode of the HYP distribution.\cr
            \tab \emph{File: dist-hypMode.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{hypMean} \tab Returns true mean of the HYP distribution \cr
            \code{hypVar} \tab  returns true variance of the HYP distribution \cr
            \code{hypSkew} \tab returns true skewness of the HYP distribution \cr
            \code{hypKurt} \tab returns true kurtosis of the HYP distribution \cr
            \code{hypMoments} \tab returns true n-th moment of the HYP distribution.\cr
            \tab \emph{File: dist-hypMoments.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{hypMED} \tab Returns true median of the HYP distribution \cr
            \code{hypIQR} \tab returns true inter quartal range of the HYP \cr
            \code{hypSKEW} \tab returns true robust skewness of the HYP \cr
            \code{hypKURT} \tab returns true robust kurtosis of the HYP.\cr
            \tab \emph{File: dist-hypRobMoments.R} }
        
        
    \bold{Normal Inverse Gaussian:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dnig} \tab Returns density for the NIG distribution \cr
            \code{pnig} \tab returns probability for the NIG distribution \cr
            \code{qnig} \tab returns quantiles for the NIG distribution \cr
            \code{rnig} \tab returns random variates for the NIG distribution \cr
            \code{.pnigC} \tab fast C implementation of function pnig() \cr
            \code{.qnigC} \tab fast C implementation of function qnig().\cr
            \tab \emph{File: dist-nig.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{nigFit} \tab Fits parameters of a NIG distribution \cr
            \code{.nigFit.mle} \tab uses max Log-likelihood estimation \cr
            \code{.nigFit.gmm} \tab uses generalized method of moments \cr
            \code{.nigFit.mps} \tab maximum product spacings estimation \cr
            \code{.nigFit.vmps} \tab minimum variance mps estimation. \cr
            \tab \emph{File: dist-nigFit.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{nigMode} \tab Computes mode of the NIG distribution.\cr
            \tab \emph{File: dist-nigMode.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{nigMean} \tab Returns true mean of the NIG distribution \cr
            \code{nigVar} \tab  returns true variance of the NIG distribution \cr
            \code{nigSkew} \tab returns true skewness of the NIG distribution \cr
            \code{nigKurt} \tab returns true kurtosis of the NIG distribution \cr
            \code{nigMoments} \tab returns true n-th moment of the NIG distribution.\cr
            \tab \emph{File: dist-nigMoments.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{nigMED} \tab Returns true median of the NIG distribution \cr
            \code{nigIQR} \tab returns true inter quartal range of the NIG \cr
            \code{nigSKEW} \tab returns true robust skewness of the NIG \cr
            \code{nigKURT} \tab returns true robust kurtosis of the NIG.\cr
            \tab \emph{File: dist-nigRobMoments.R} }
        
            
    \bold{Generalized Hyperbolic Student-t Distribution:}
               
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dght} \tab Returns density for the GHT distribution \cr
            \code{pght} \tab returns probability for the GHT distribution \cr
            \code{qght} \tab returns quantiles for the GHT distribution \cr
            \code{rght} \tab returns random variates for the GHT distribution.\cr
            \tab \emph{File: dist-ght.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghtFit} \tab Fits parameters of the GHT distribution.\cr
            \tab \emph{File: dist-ghtFit.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghtMode} \tab Computes mode of the GHT distribution.\cr
            \tab \emph{File: dist-ghtMode.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghtMean} \tab Returns true mean of the NIG distribution \cr
            \code{ghtVar} \tab  returns true variance of the GHT distribution \cr
            \code{ghtSkew} \tab returns true skewness of the GHT distribution \cr
            \code{ghtKurt} \tab returns true kurtosis of the GHT distribution \cr
            \code{ghtMoments} \tab returns true n-th moment of the GHT distribution.\cr
            \tab \emph{File: dist-ghtMoments.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ghtMED} \tab Returns true median of the GHT distribution \cr
            \code{ghtIQR} \tab returns true inter quartal range of the GHT \cr
            \code{ghtSKEW} \tab returns true robust skewness of the GHT \cr
            \code{ghtKURT} \tab returns true robust kurtosis of the GHT.\cr
            \tab \emph{File: dist-ghtRobMoments.R} }
        
    \bold{Stable Distribution:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dstable} \tab Returns density for the stable distribution \cr
            \code{pstable} \tab returns probability for the stable distribution \cr
            \code{qstable} \tab returns quantiles for the stable distribution \cr
            \code{rstable} \tab returns random variates for the dtsble distribution.\cr
            \tab \emph{File: dist-stable.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{stableFit} \tab Fits parameters of a the stable distribution \cr
            \code{.phiStable} \tab Creates contour table for McCulloch estimators \cr
            \code{.PhiStable} \tab Contour table created by function .phiStable() \cr
            \code{.qStableFit} \tab Estimates parameters by McCulloch's approach \cr
            \code{.mleStableFit} \tab Estimates stable parameters by MLE approach \cr
            \code{.stablePlot} \tab Plots results of stable parameter estimates.\cr
            \tab \emph{File: dist-stableFit.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{stableMode} \tab Computes mode of the stable distribution.\cr
            \tab \emph{File: dist-stableMode.R} }
  
              
    \bold{Generalized Lambda Distribution:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dgld} \tab Returns density for the GLD distribution \cr
            \code{pgld} \tab returns probability for the GLD distribution \cr
            \code{qgld} \tab returns quantiles for the GLD distribution \cr
            \code{rgld} \tab returns random variates for the GLD distribution.\cr
            \tab \emph{File: dist-gld.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{gldFit} \tab Fits parameters of the GLD distribution \cr
            \code{.gldFit.mle} \tab fits GLD using maximum log-likelihood \cr
            \code{.gldFit.mps} \tab fits GLD using maximum product spacings \cr 
            \code{.gldFit.gof} \tab fits GLD using Goodness of Fit statistics \cr
            %\code{.ksGLD} \tab Kolmogorov-Smirnov Statistics \cr
            %\code{.cvmGLD} \tab Cramer-vonMise Statistics \cr
            %\code{.adGLD} \tab Anderson-Darling Statistics \cr
            \code{.gldFit.hist} \tab fits GLD using a histogram fit \cr
            \code{.gldFit.rob} \tab  fits GLD using robust moments fit.\cr
            \tab \emph{File: dist-gldFit.R} \cr  

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{gldMode} \tab Computes mode of the GLD distribution.\cr
            \tab \emph{File: dist-gldMode.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{gldMED} \tab Returns true median of the GLD distribution \cr
            \code{gldIQR} \tab returns true inter quartal range of the GLD \cr
            \code{gldSKEW} \tab returns true robust skewness of the GLD \cr
            \code{gldKURT} \tab returns true robust kurtosis of the GLD.\cr
            \tab \emph{File: dist-gldRobMoments.R} }
       
             
    \bold{Spline Smoothed Distribution:}
            
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{dssd} \tab Returns spline smoothed density function \cr
            \code{pssd} \tab returns spline smoothed probability function \cr
            \code{qssd} \tab returns spline smoothed quantile function \cr
            \code{rssd} \tab returns spline smoothed random variates.\cr
            \tab \emph{File: dist-ssd.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ssdFit} \tab Fits parameters for a spline smoothed distribution.\cr
            \tab \emph{File: dist-ssdFit.R} }
}


\section{3. Hypthesis Testing}{
  

    \bold{One Sample Tests:} 
        
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ksnormTest} \tab One sample Kolmogorov-Smirnov normality test \cr
            \code{shapiroTest} \tab Shapiro-Wilk normality test \cr
            \code{jarqueberaTest} \tab Jarque-Bera normality test \cr
            \code{normalTest} \tab Normality tests S-Plus compatible call \cr
            \code{dagoTest} \tab D'Agostino normality test \cr
            \code{adTest} \tab Anderson-Darling normality test \cr
            \code{cvmTest} \tab Cramer-von Mises normality test \cr
            \code{lillieTest} \tab Lilliefors (KS) normality test \cr 
            \code{pchiTest} \tab Pearson chi-square normality test \cr 
            \code{sfTest} \tab Shapiro-Francia normality test \cr     
            \code{jbTest} \tab Finite sample adjusted JB LM and ALM test \cr
            \tab \emph{File: test-normalTest.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{locationTest} \tab Performs locations tests on two samples \cr
            \code{.tTest} \tab Unpaired t test for differences in mean \cr
            \code{.kw2Test} \tab Kruskal-Wallis test for differences in locations \cr
            \tab \emph{File: test-locationTest.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{scaleTest} \tab Performs scale tests on two samples \cr
            \code{.ansariTest} \tab Ansari-Bradley test for differences in scale \cr
            \code{.moodTest} \tab Mood test for differences in scale \cr
            \tab \emph{File: test-scaleTest.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{varianceTest} \tab Performs variance tests on two samples \cr
            \code{.varfTest} \tab F test for differences in variances \cr
            \code{.bartlett2Test} \tab Bartlett's test for differences in variances \cr
            \code{.fligner2Test} \tab Fligner-Killeen test for differences in variances \cr
            \tab \emph{File: test-variance Test.R} }
      
          
    \bold{Two Sample Tests:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{ks2Test} \tab Performs a two sample Kolmogorov-Smirnov test \cr
            \code{correlationTest} \tab Performs correlation tests on two samples \cr
            \code{pearsonTest} \tab Pearson product moment correlation coefficient \cr
            \code{kendallTest} \tab Kendall's tau correlation test \cr
            \code{spearmanTest} \tab Spearman's rho correlation test \cr
            \tab \emph{File: test-ks2Test.R} }
        
    \bold{Test Utilities:}
          
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{'fHTEST'} \tab S4 Class Representation \cr
            \code{show.fHTEST} \tab S4 Print Method \cr
            \code{.jbALM} \tab Jarque Bera Augmented Lagrange Multiplier Data \cr
            \code{.jbLM} \tab Jarque-Bera Lagrange Multiplier Data \cr
            \code{.jbTable} \tab Finite sample p values for the Jarque Bera test \cr
            \code{.jbPlot} \tab Plots probabilities \cr
            \code{.pjb} \tab Returns probabilities for JB given quantiles \cr
            \code{.qjb} \tab Returns quantiles for JB given probabilities \cr
            \tab \emph{File: test-fHTEST.R} }
            
}


\section{4. Plotting Routines}{


    \bold{Financial Time Series Plots:}
  
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{seriesPlot} \tab Dispalys a time series plot \cr           
            \code{cumulatedPlot} \tab displays cumulated series give returns \cr
            \code{returnPlot} \tab displays returns given cumulated series \cr
            \code{drawdownPlot} \tab displays drawdown series from returns.\cr
            \tab \emph{File: plot-seriesPlot.R} }
        
        
    \bold{Correlation Plots:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{acfPlot} \tab Displays tailored ACF plot \cr
            \code{pacfPlot} \tab displays tailored partial ACF plot \cr
            \code{teffectPlot} \tab displays the Taylor effect \cr
            \code{lacfPlot} \tab displays lagged autocorrelations.\cr
            \tab \emph{File: plot-acfPlot.R} }

        
    \bold{Distribution Plots:}
    
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{histPlot} \tab Returns tailored histogram plot \cr
            \code{densityPlot} \tab returns tailored density plot \cr 
            \code{logDensityPlot} \tab returns tailored log density plot.\cr
            \tab \emph{File: plot-histPlot.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{boxPlot} \tab Returns side-by-side standard box plot \cr
            \code{boxPercentilePlot} \tab returns box-percentile plot.\cr
            \tab \emph{File: plot-boxPlot.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{qqnormPlot} \tab Returns normal quantile-quantile plot \cr
            \code{qqnigPlot} \tab returns NIG quantile-quantile plot \cr
            \code{qqghtPlot} \tab returns  GHT quantile-quantile plot \cr
            \code{qqgldPlot} \tab returns GLD quantile-quantile plot.\cr
            \tab \emph{File: plot-qqPlot.R} }
        
        
    \bold{Aggregation Plots:}
            
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{scalinglawPlot} \tab displays scaling law behavior.\cr
            \tab \emph{File: plot-scalinglawPlot.R} }
}
        

\section{5. Matrix Computations and Linear Algebra}{


    \bold{Matrix Operations:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{kron} \tab Returns the Kronecker product.\cr
            \tab \emph{File: matrix-kron.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{vec} \tab Stacks a matrix as column vector \cr
            \code{vech} \tab stacks a lower triangle matrix.\cr
            \tab \emph{File: matrix-vech.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{pdl} \tab Returns regressor matrix for polynomial lags.\cr
            \tab \emph{File: matrix-pdl.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{tslag} \tab Returns Lagged/leading vector/matrix.\cr
            \tab \emph{File: matrix-tslag.R} }
        
        
    \bold{Linear Algebra:}
    
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{inv} \tab Returns the inverse of a matrix.\cr
            \tab \emph{File: matrix-inv.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{norm} \tab Returns the norm of a matrix.\cr
            \tab \emph{File: matrix-norm.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{rk} \tab Returns the rank of a matrix.\cr
            \tab \emph{File: matrix-rk.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{tr} \tab Returns the trace of a matrix.\cr
            \tab \emph{File: matrix-tr.R} }
 
               
    \bold{Matrix Utilities:}
       
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{isPositiveDefinite} \tab Checks if a matrix is positive definite \cr
            \code{makePositiveDefinite} \tab forces a matrix to be positive definite.\cr
            \tab \emph{File: matrix-posDefinite.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{colVec} \tab Creates a column vector from a data vector \cr
            \code{rowVec} \tab creates a row vector from a data vector.\cr
            \tab \emph{File: matrix-colVec.R} \cr 
        
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{gridVector} \tab Creates from two vectors rectangular grid.\cr
            \tab \emph{File: matrix-gridVector.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{triang} \tab Extracs lower tridiagonal part from a matrix \cr
            \code{Triang} \tab Extracs upper tridiagonal part from a matrix.\cr
            \tab \emph{File: matrix-triang.R} }
          
            
    \bold{Matrix Examples:}
            
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{hilbert} \tab Creates a Hilbert matrix.\cr
            \tab \emph{File: matrix-hilbert.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{pascal} \tab Creates a Pascal matrix.\cr
            \tab \emph{File: matrix-pascal.R} }         
}


\section{6. Utility Functions}{


    \bold{Color Utilities:}
               
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{colorLocator} \tab Plots Rs 657 named colors for selection \cr
            \code{colorMatrix} \tab returns matrix of R's color names.\cr
            \tab \emph{File: utils-colorLocator.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{colorTable} \tab Table of Color Codes and Plot Colors itself.\cr
            \tab \emph{File: utils-colorTable.R} \cr 
        
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{rainbowPalette} \tab Contiguous rainbow color palette \cr
            \code{heatPalette} \tab Contiguous heat color palette \cr
            \code{terrainPalette} \tab Contiguous terrain color palette \cr
            \code{topoPalette} \tab Contiguous topo color palette \cr
            \code{cmPalette} \tab Contiguous cm color palette \cr
            \code{greyPalette} \tab R's gamma-corrected gray palette \cr
            \code{timPalette} \tab Tim's Matlab like color palette \cr
            \code{rampPalette} \tab Color ramp palettes \cr
            \code{seqPalette} \tab Sequential color brewer palettes \cr
            \code{divPalette} \tab Diverging color brewer palettes \cr
            \code{qualiPalette} \tab Qualified color brewer palettes \cr
            \code{focusPalette} \tab Red, green blue focus palettes \cr
            \code{monoPalette} \tab Red, green blue mono palettes.\cr
            \tab \emph{File: utils-colorPalette.R} }

    
    \bold{Plot Utilities:}
               
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{symbolTable} \tab Shows a table of plot symbols.\cr
            \tab \emph{File: utils-symbolTable.R} \cr 
             
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{characterTable} \tab Shows a table of character codes.\cr
            \tab \emph{File: utils-characterTable.R} \cr 
 
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{decor} \tab Adds horizontal grid and L shaped box \cr
            \code{hgrid} \tab adds horizontal grid lines \cr
            \code{vgrid} \tab adds vertical grid lines \cr
            \code{boxL} \tab adds L-shaped box \cr
            \code{box_} \tab adds unterlined box \cr
            \code{.xrug} \tab adds rugs on x axis \cr
            \code{.yrug} \tab adds rugs on y axis \cr
            \code{copyright} \tab adds copyright notice.\cr
            \tab \emph{File: utils-decor.R} \cr 

        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{interactivePlot} \tab Plots several graphs interactively.\cr
            \tab \emph{File: utils-interactivePlot.R} }
        
    
    \bold{Special Function Utilities:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{Heaviside} \tab Computes Heaviside unit step function \cr
            \code{Sign} \tab  another signum function \cr
            \code{Delta} \tab computes delta function \cr
            \code{Boxcar} \tab computes boxcar function \cr
            \code{Ramp} \tab computes ramp function.\cr
            \tab \emph{File: utils-heaviside.R} \cr 
 
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{tsHessian} \tab Computes Two Sided Hessian matrix.\cr
            \tab \emph{File: utils-hessian.R} }
        
        
    \bold{Other Utilities:}
           
        \tabular{ll}{
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{.unirootNA} \tab Computes zero of a function without error exit.\cr
            \tab \emph{File: utils-unitrootNA.R} \cr 
 
        \tab \cr
        \emph{Function ________________}
        \tab Description _________________________________________\cr
            \code{getCall} \tab Extracts the call slot from a S4 object \cr
            \code{getModel} \tab extracts the model slot from a S4 object \cr
            \code{getTitle} \tab extracts the title slot from a S4 object \cr
            \code{getDescription} \tab extracts the description slot \cr
            \code{getSlot} \tab extracts a specified slot from a S4 object.\cr
            \tab \emph{File: utils-getS4.R} }
    
}


\section{B. Code and Function Dependencies:}{

    1. Dependences and Suggested Packages
    
    2. Builtin Functions
    
    3. Compiled Fortran and C Code
    
}
   

\section{1. Dependences and Suggested Packages:}{


    Required packages  include:

    \tabular{ll}{
    \emph{Dependences _________________}
    \tab Description _________________________________________\cr
        \code{MASS} \tab R base package \cr
        \code{methods} \tab R base package \cr
        \code{timeDate} \tab Rmetrics package \cr
        \code{timeSeries} \tab Rmetrics package \cr

    \tab \cr   
    \emph{Suggested ___________________}
    \tab Description _________________________________________\cr
        \code{akima} \tab contributed R package \cr
        \code{spatial} \tab contributed R package \cr
        \code{RUnit} \tab contributed R package \cr
        \code{tcltk} \tab contributed R package.}

}           
    

\section{2. Builtin Functions:}{


    Builtin functions are borrowed from contributed R packages and other 
    sources.
    There are several reasons why we have modified and copied code from 
    other sources and included in this package.
    
    *   The builtin code is not available on Debian, so that Linux users
        have no easy acces to this code.
        
    *   The code conflicts with other code from this package or conflicts
        with Rmetrics design objectives.
        
    *   We only need a very small piece of functionality from the original
        package which may depend on other packages which are not needed.
        
    *   The package from which we builtin the code is under current 
        development, so that the functions often change and thus leads
        to unexpectect behavior in the Rmetrics packages
    
    *   The package may be incompatible since it uses other time date
        and time series classes than the timDate and timeSeries objects
        and methods from Rmetrics.
        
    We put the code in script files named \emph{builtin-funPackage.R}
    where "fun" denotes the (optional) major function name, and 
    "Package" the name of the contributed package from which we 
    copied the original code.
    
    Builtin functions include:
    
    \tabular{ll}{
    \emph{Builtin _________________}
    \tab Description _________________________________________\cr
        \code{gelGmm}       \tab gll function from gmm package \cr
        \code{gmmGMM}       \tab gmm function from gmm package \cr
        \code{kweightsSandwhich} \tab kweights from sandwhich package \cr
        \code{glGld}        \tab glfunctions from gld package \cr
        \code{ssdenGss}     \tab ssden from the gss package\cr
        \code{hypHyperbolicDist} \tab hyp from HyperbolicDist package.}

}
        

\section{3. Compiled Fortran and C Code:}{


    \tabular{ll}{
    \emph{Fortran/C _________________}
    \tab Description _________________________________________\cr
        \code{gld.c} \tab source code from gld package \cr
        \code{nig.c} \tab source code from Kersti Aas \cr
        \code{gss.f} \tab source code fromsandwhich package.}

}


\section{C. Copyright and Licencies:}{

    Copyright Rmetrics Association
    
}


\section{D. Contact Address:}{

    www.rmetrics.org
    
    Addresses:
    
}


\keyword{models}

