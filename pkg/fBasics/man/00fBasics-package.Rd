\name{fBasics-package}


\alias{fBasics-package}
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

    \preformatted{
        Package:    \tab fBasics\cr
        Type:       \tab Package\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
        Repository: \tab R-FORGE\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{

  The fBasics package contains \emph{basics tools} often required
  in computational finance and financial engineering. The topics
  are: basic statistics functions, financial return distributions,
  hypothesis testing, plotting routines, matrix computations and 
  linear algebra, and some usefule utility functions.
}


   
\section{2 Basic Statistics Functions}{


    \emph{Financial Return Statistics}
    
    % stats-basicStats.R
    \preformatted{ 
    basicStats            Returns a basic statistics summary
    }
    
    
    \emph{Distribution Function of Maximum Drawdowns}
    
    % stats-maxdd.R
    \preformatted{
    dmaxdd                Density function of mean Max-Drawdowns
    pmaxdd                Probability function of mean Max-Drawdowns
    rmaxdd                Random Variates of mean Max-Drawdowns
    maxddStats            Expectation of Drawdowns for BM with drift
    }
    
    
    \emph{Calculation of Sample Moments}
    
    % stats-sampleLMoments.R | stats-sampleRobMoments.R
    \preformatted{ 
    sampleLmoments        Computes sample L-moments
    sampleMED             Returns sample median
    sampleIQR             returns sample inter quartal range
    sampleSKEW            returns robust sample skewness
    sampleKURT            returns robust sample kurtosis
    }
    
    
    \emph{Bivariate Interpolation:}
    
    % stats-interpAkima.R | stats-interpKrige.R | stats-interpLinear.R
    \preformatted{ 
    akimaInterp           Interpolates irregularly spaced points
    akimaInterpp          Interpolates and smoothes pointwise
    krigeInterp           Kriges irregularly spaced data points
    linearInterp          Interpolates irregularly spaced points
    linearInterpp         Interpolates linearly pointwise
    }
    
    \emph{Utility Statistics Functions:}
    
    \preformatted{
    colStats              Computes sample statistics by col
    colSums               Computes sums of values in each col
    colMeans              Computes means of values in each col
    colSds                Computes standard deviation of each col
    colVars               Computes sample variance by col
    colSkewness           Computes sample skewness by col
    colKurtosis           Computes sample kurtosis by col
    colMaxs               Computes maximum values in each col
    colMins               Computes minimum values in each col
    colProds              Computes product of values in each col
    colQuantiles          Computes product of values in each col
    }
    
    % stats-rowStats.R
    \preformatted{
    rowStats              Computes sample statistics by row
    rowSums               Computes sums of values in each row
    rowMeans              Computes means of values in each row
    rowSds                Computes standard deviation of each row
    rowVars               Computes sample variance by row
    rowSkewness           Computes sample skewness by row
    rowKurtosis           Computes sample kurtosis by row
    rowMaxs               Computes maximum values in each row
    rowMins               Computes minimum values in each row
    rowProds              Computes product of values in each row
    rowQuantiles          Computes product of values in each row
    }
}


\section{3 Financial Return Distributions}{

    \emph{Generalized Hyperbolic Distribution:}
    
    % dist-gh.R | dist-ghFit.R | dist-ghMode.R | dist-ghMoments.R | 
    % dist-ghRobMoments.R
    \preformatted{
    dghReturns            Density for the GH distribution
    pghreturns            Probability for the GH distribution
    qghreturns            Quantiles for the GH distribution
    rghreturns            Random variates for the GH distribution
    ghFitFits             Fits parameters of the GH distribution
    ghMode                Computes mode of the GH distribution. 
    ghMean                Returns true mean of the GH distribution
    ghVar                 Returns true variance of the GH distribution
    ghSkew                Returns true skewness of the GH distribution
    ghKurt                Returns true kurtosis of the GH distribution
    ghMoments             Returns true n-th moment of the GH distribution
    ghMED                 Returns true median of te GH distribution
    ghIQR                 Returns true inter quartal range of te GH
    ghSKEW                Returns true robust skewness of te GH
    ghKURT                Returns true robust kurtosis of te GH
    } 
    
    
    \emph{Hyperbolic Distribution:}
    
    % dist-hyp.R | dist-hypFit.R | dist-hypMode.R | dist-hypMoments.R
    % dist-hypRobMoments.R
    \preformatted{
    dhyp                  Returns density for the HYP distribution
    phyp                  Returns probability for the HYP distribution
    qhyp                  Returns quantiles for the HYP distribution
    rhyp                  Returns random variates for the HYP distribution
    hypFit                Fits parameters of the HYP distribution
    hypMode               Computes mode of the HYP distribution
    hypMean               Returns true mean of the HYP distribution
    hypVar R              Returns true variance of the HYP distribution
    hypSkew               Returns true skewness of the HYP distribution
    hypKurt               Returns true kurtosis of the HYP distribution
    hypMoments            Returns true n-th moment of the HYP distribution
    hypMED                Returns true median of the HYP distribution
    hypIQR                Returns true inter quartal range of the HYP
    hypSKEW               Returns true robust skewness of the HYP
    hypKURT               Returns true robust kurtosis of the HYP
    }
    
    
    \emph{Normal Inverse Gaussian:}
    
    % dist-nig.R | dist-nigFit.R | dist-nigMode.R | dist-nigMoments.R
    % dist-nigRobMoments.R
    \preformatted{
    dnig                  Returns density for the NIG distribution
    pnig                  Returns probability for the NIG distribution
    qnig                  Returns quantiles for the NIG distribution
    rnig                  Returns random variates for the NIG distribution
      .pnigC              fast C Implementation of function pnig()
      .qnigC              fast CImplementation of function qnig()
    nigFit                Fits parameters of a NIG distribution
      .nigFit.mle         Uses max Log-likelihood estimation
      .nigFit.gmm         Uses generalized method of moments
      .nigFit.mps         Maximum product spacings estimation
      .nigFit.vmps        Minimum variance mps estimation
    nigMode               Computes mode of the NIG distribution
    nigMean               Returns true mean of the NIG distribution
    nigVar                Returns true variance of the NIG distribution
    nigSkew               Returns true skewness of the NIG distribution
    nigKurt               Returns true kurtosis of the NIG distribution
    nigMoments            Returns true n-th moment of the NIG distribution
    nigMED                Returns true median of the NIG distribution
    nigIQR                Returns true inter quartal range of the NIG
    nigSKEW               Returns true robust skewness of the NIG
    nigKURT               Returns true robust kurtosis of the NIG
    }
    
    
    \emph{Generalized Hyperbolic Student-t Distribution:}
    
    % dist-ght.R | dist-ghtFit.R | dist-ghtMode.R | dist-ghtMoments.R | 
    % dist-ghtRobMoments.R
    \preformatted{
    dght                  Returns density for the GHT distribution
    pght                  Returns probability for the GHT distribution
    qght                  Returns quantiles for the GHT distribution
    rght                  Returns random variates for the GHT distribution
    ghtFit                Fits parameters of the GHT distribution
    ghtMode               Computes mode of the GHT distribution
    ghtMean               Returns true mean of the NIG distribution
    ghtVar                Returns true variance of the GHT distribution
    ghtSkew               Returns true skewness of the GHT distribution
    ghtKurt               Returns true kurtosis of the GHT distribution
    ghtMoments            Returns true n-th moment of the GHT distribution
    ghtMED                Returns true median of the GHT distribution
    ghtIQR                Returns true inter quartal range of the GHT
    ghtSKEW               Returns true robust skewness of the GHT
    ghtKURT               Returns true robust kurtosis of the GHT
    }
    
    
    \emph{Stable Distribution:}
    
    % dist-stable.R | dist-stableFit.R | dist-stableMode.R
    \preformatted{
    dstable               Returns density for the stable distribution
    pstable               Returns probability for the stable distribution
    qstable               Returns quantiles for the stable distribution
    rstable               Returns random variates for the dtsble distribution
    stableFit             Fits parameters of a the stable distribution
      .phiStable          Creates contour table for McCulloch estimators
      .PhiStable          Contour table created by function .phiStable()
      .qStableFit         Estimates parameters by McCulloch's approach
      .mleStableFit       Estimates stable parameters by MLE approach
      .stablePlot         Plots results of stable parameter estimates
    stableMode            Computes mode of the stable distribution
    }
    
    
    \emph{Generalized Lambda Distribution:}
          
    % dist-gld.R | dist-gldFit.R | dist-gldMode.R |
    % dist-gldRobMoments.R
    \preformatted{
    dgld                  Returns density for the GLD distribution
    pgld                  Returns probability for the GLD distribution
    qgld                  Returns quantiles for the GLD distribution
    rgld                  Returns random variates for the GLD distribution
    gldFit                Fits parameters of the GLD distribution
      .gldFit.mle         fits GLD using maximum log-likelihood
      .gldFit.mps         fits GLD using maximum product spacings 
      .gldFit.gof         fits GLD using Goodness of Fit statistics
                          % .ksGLDKolmogorov-Smirnov Statistics
                          % .cvmGLDCramer-vonMise Statistics
                          % .adGLDAnderson-Darling Statistics
      .gldFit.hist        fits GLD using a histogram fit
      .gldFit.rob         fits GLD using robust moments fit
    gldMode               Computes mode of the GLD distribution.
    gldMED                Returns true median of the GLD distribution
    gldIQR                Returns true inter quartal range of the GLD
    gldSKEW               Returns true robust skewness of the GLD
    gldKURT               Returns true robust kurtosis of the GLD
    }
    
    
    \emph{Spline Smoothed Distribution:}
          
    % dist-ssd.R | dist-ssdFit.R
    \preformatted{
    dssd                  Returns spline smoothed density function
    pssd                  Returns spline smoothed probability function
    qssd                  Returns spline smoothed quantile function
    rssd                  Returns spline smoothed random variates.
    ssdFit                Fits parameters for a spline smoothed distribution
    }
}


\section{4 Hypthesis Testing}{


    \emph{One Sample Nornality Tests:}
    
    % test-normalTest.R
    \preformatted{
    ksnormTest            One sample Kolmogorov-Smirnov normality test
    shapiroTest           Shapiro-Wilk normality test
    jarqueberaTest        Jarque-Bera normality test
    normalTest            Normality tests S-Plus compatible call
    dagoTest              D'Agostino normality test
    adTest                Anderson-Darling normality test
    cvmTest               Cramer-von Mises normality test
    lillieTest            Lilliefors (KS) normality test 
    pchiTest              Pearson chi-square normality test 
    sfTest                Shapiro-Francia normality test     
    jbTest                Finite sample adjusted JB LM and ALM test
    }
    
    
    \emph{One Sample Location, Scale and variance Tests:}
    
    % test-locationTest.R | test-scaleTest.R | test-variance Test.R
    \preformatted{
    locationTest          Performs locations tests on two samples
      .tTest              Unpaired t test for differences in mean
      .kw2Test            Kruskal-Wallis test for differences in locations
    scaleTest             Performs scale tests on two samples
      .ansariTest         Ansari-Bradley test for differences in scale
      .moodTest           Mood test for differences in scale
    varianceTest          Performs variance tests on two samples
      .varfTest           F test for differences in variances
      .bartlett2Test      Bartlett's test for differences in variances
      .fligner2Test       Fligner-Killeen test for differences in variances
    }
    
    
    \emph{Two Sample Tests:}
           
    % test-ks2Test.R
    \preformatted{
    ks2Test               Performs a two sample Kolmogorov-Smirnov test
    correlationTest       Performs correlation tests on two samples
    pearsonTest           Pearson product moment correlation coefficient
    kendallTest           Kendall's tau correlation test
    spearmanTest          Spearman's rho correlation test
    }
    
    
    \emph{Test Utilities:}
            
    % test-fHTEST.R
    \preformatted{
    'fHTEST'              S4 Class Representation
    show.fHTEST           S4 Print Method
      .jbALM              Jarque Bera Augmented Lagrange Multiplier Data
      .jbLM               Jarque-Bera Lagrange Multiplier Data
      .jbTable            Finite sample p values for the Jarque Bera test
      .jbPlot             Plots probabilities
      .pjb                Returns probabilities for JB given quantiles
      .qjb                Returns quantiles for JB given probabilities
    }
    
}


\section{5 Plotting Routines}{


    \emph{Financial Time Series Plots:}
    
    % plot-seriesPlot.R
    \preformatted{
    seriesPlot            Dispalys a time series plot
    cumulatedPlot         Displays cumulated series give returns
    returnPlot            Displays returns given cumulated series
    drawdownPlot          Displays drawdown series from returns
    }
    
    
    \emph{Correlation Plots:}
             
    % plot-acfPlot.R
    \preformatted{
    acfPlot               Displays tailored ACF plot
    pacfPlot              Displays tailored partial ACF plot
    teffectPlot           Displays the Taylor effect
    lacfPlot              Displays lagged autocorrelations
    }
    
    
    \emph{Distribution Plots:}
      
    % plot-histPlot.R | plot-qqPlot.R plot-boxPlot.R
    \preformatted{
    histPlot              Returns tailored histogram plot
    densityPlot           Returns tailored density plot 
    logDensityPlot        Returns tailored log density plot
    boxPlot               Returns side-by-side standard box plot
    boxPercentile         Plotreturns box-percentile plot
    qqnormPlot            Returns normal quantile-quantile plot
    qqnigPlot             Returns NIG quantile-quantile plot
    qqghtPlot             Rreturns  GHT quantile-quantile plot
    qqgldPlot             Returns GLD quantile-quantile plot
    }
    
    
    \emph{ Time Series Aggregation Plots:}
              
    % plot-scalinglawPlot.R
    \preformatted{
    scalinglawPlot        Displays scaling law behavior
    }

}


\section{5. Matrix Computations and Linear Algebra}{


    \emph{Elementar Matrix Operation Addons:}
             
    % matrix-kron.R | matrix-vech.R | matrix-pdl.R | matrix-tslag.R
    \preformatted{
    kron                  Returns the Kronecker product
    vec                   Stacks a matrix as column vector
    vech                  Stacks a lower triangle matrix
    pdl                   Returns regressor matrix for polynomial lags
    tslag                 Returns Lagged/leading vector/matrix
    }
    
    
    \emph{Linear Algebra Addons:}
     
    % matrix-inv.R | matrix-norm.R | matrix-rk.R | matrix-tr.R
    \preformatted{
    inv                   Returns the inverse of a matrix
    norm                  Returns the norm of a matrix
    rk                    Returns the rank of a matrix
    tr                    Returns the trace of a matrix
    }
    
    
    \emph{General Matrix Utility Addons:}
     
    % matrix-posDefinite.R | matrix-colVec.R | matrix-gridVector.R |
    % matrix-triang.R
    \preformatted{
    isPositiveDefinite    Checks if a matrix is positive definite
    makePositiveDefinite  Forces a matrix to be positive definite
    colVec                Creates a column vector from a data vector
    rowVec                Creates a row vector from a data vector
    gridVector            Creates from two vectors rectangular grid
    triang                Extracs lower tridiagonal part from a matrix
    Triang                Extracs upper tridiagonal part from a matrix
    }
    
    
    \emph{Selected Matrix Examples:}
              
    % matrix-hilbert.R | matrix-pascal.R
    \preformatted{
    hilbert               Creates a Hilbert matrix
    pascal                Creates a Pascal matrix
    }
  
}


\section{6 Utility Functions}{


    \emph{Color Utilities:}
    
    % utils-colorLocator.R | utils-colorTable.R | utils-colorPalette.R
    \preformatted{
    colorLocator            Plots Rs 657 named colors for selection
    colorMatrix             Returns matrix of R's color names.
    colorTable              Table of Color Codes and Plot Colors itself
    rainbowPalette          Contiguous rainbow color palette
    heatPalette             Contiguous heat color palette
    terrainPalette          Contiguous terrain color palette
    topoPalette             Contiguous topo color palette
    cmPalette               Contiguous cm color palette
    greyPalette             R's gamma-corrected gray palette
    timPalette              Tim's Matlab like color palette
    rampPalette             Color ramp palettes
    seqPalette              Sequential color brewer palettes
    divPalette              Diverging color brewer palettes
    qualiPalette            Qualified color brewer palettes
    focusPalette            Red, green blue focus palettes
    monoPalette             Red, green blue mono palettes
    }
    
    
    \emph{Graphics Utilities:}
    
    
    % utils-symbolTable.R | utils-characterTable.R | utils-decor.R |
    % utils-interactivePlot.R
    \preformatted{
    symbolTable             Shows a table of plot symbols
    characterTable          Shows a table of character codes
    decor                   Adds horizontal grid and L shaped box
    hgrid                   Adds horizontal grid lines
    vgrid                   Adds vertical grid lines
    boxL                    Adds L-shaped box
    box                     Adds unterlined box
      .xrug                 Adds rugs on x axis
      .yrug                 Adds rugs on y axis
    copyright               Adds copyright notice
    interactivePlot         Plots several graphs interactively
    }
    
    
    \emph{Special Function Utilities:}
    
    
    %  utils-heaviside.R | utils-hessian.R
    \preformatted{
    Heaviside               Computes Heaviside unit step function
    Sign                    Another signum function
    Delta                   Computes delta function
    Boxcar                  Computes boxcar function
    Ramp                    Computes ramp function
    tsHessian               Computes Two Sided Hessian matrix
    }
    
    
    \emph{Other Utilities:}
    
    
    % utils-unitrootNA.R | utils-getS4.R
    \preformatted{
    .unirootNA              Computes zero of a function without error exit
    getModel                Extracts the model slot from a S4 object
    getTitle                Extracts the title slot from a S4 object
    getDescription          Extracts the description slot
    getSlot                 Extracts a specified slot from a S4 object
    }
    
}


\section{About Builtin Functions}{


    Builtin functions are borrowed from contributed R packages and other 
    sources.
    There are several reasons why we have modified and copied code from 
    other sources and included in this package.
    
    * The builtin code is not available on Debian, so that Linux users
      have no easy acces to this code.
      
    * The original code conflicts with other code from this package or 
      conflicts with Rmetrics design objectives.
      
    * We only need a very small piece of functionality from the original
      package which may depend on other packages which are not needed.
      
    * The package from which we builtin the code is under current 
      development, so that the functions often change and thus leads
      to unexpectect behavior in the Rmetrics packages.
    
    * The package may be incompatible since it uses other time date
      and time series classes than the 'timDate' and 'timeSeries' objects
      and methods from Rmetrics.
        
    We put the code in script files named \emph{builtin-funPackage.R}
    where "fun" denotes the (optional) major function name, and 
    "Package" the name of the contributed package from which we 
    copied the original code.
    
    Builtin functions include:
    
    \preformatted{
    gelGmm                gll function from gmm package
    gmmGMM                gmm function from gmm package
    kweightsSandwhich     kweights from sandwhich package
    glGld                 gl functions from gld package
    ssdenGss              ssden from the gss package
    hypHyperbolicDist     hyp from HyperbolicDist package
    }
}
        

\section{Compiled Fortran and C Code:}{

    \preformatted{
    gld.c                 source code from gld package
    nig.c                 source code from Kersti Aas
    gss.f                 source code fromsandwhich package
    }

}


\section{About Rmetrics:}{

    The \code{fBasics} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}

