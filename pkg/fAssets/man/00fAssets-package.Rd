\name{fAssets-package}


\alias{fAssets-package}
\alias{fAssets}


\docType{package}


\title{Analysing and Modelling Financial Assets}


\description{

  The Rmetrics \code{fAssets} package is a  collection of functions 
  to manage, to investigate and to analyze data sets of financial 
  assets from different points of view.

}


\details{

  \tabular{ll}{
    Package:    \tab fAssets\cr
    Type:       \tab Package\cr
    Date:       \tab 2014\cr
    License:    \tab GPL Version 2 or later\cr
    Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
    Repository: \tab R-FORGE\cr
    URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\keyword{models}


\section{1 Introduction}{

  The package \code{fAssets} was written to explore and investigate
  data sets of financial asssets
  
  Included are functions to make the the asset selection process easier,
  to robustify return and covariances for modeling portfolios, to test
  financial returns for multivariate normality, and to measure in a 
  simple way performance and risk of funds and portfolios.
  
  Beside this many functions for graphs and plots, and for a more 
  sophisticated explorative data analysis are provided. They range
  from simple time series plots to more elaborated statisitical
  chart tools: 
  histogram, density, boxplots, and QQ plots; 
  pairs,similaries, and covarinace ellipses plots;
  star plots, and risk/reward graphs.

}


\section{2 Assets Selection}{

  The assets selection chapter containts functions which arrange assets
  from a data set according to different measaures applying ideas from
  principal component analysis, from hierarchical clustering, or by a
  user defined statistical measure: 

  % assets-arrange.R
  \preformatted{
  assetsArrange               Rearranges the columns in a data set of assets
   pcaArrange                  Returns PCA correlation ordered column names
   hclustArrange               Returns hierarchical clustered column names 
   abcArrange                  Returns assets sorted by column names 
   orderArrange                Returns assets ordered by column names 
   sampleArrange               Returns a re-sampled set of assets 
   statsArrange                Returns statistically rearranged column names
  }
  
  In addition we have summarized and bundle of distance measure functions
  to determine the similarity or dissimilarity of individual assets from
  a set of multivariate financial return series. 
  
  % assets-distance.R
  \preformatted{
  assetsDist                  Computes the distances between assets
   corDist                     Returns correlation distance measure
   kendallDist                 Returns kendalls correlation distance measure        
   spearmanDist                Returns spearmans correlation distance measure
   mutinfoDist                 Returns mutual information distance measure
   euclideanDist               Returns Euclidean distance measure
   maximumDist                 Returns maximum distance measure
   manhattanDist               Returns Manhattan distance measure
   canberraDist                Returns Canberra distance measure
   binaryDist                  Returns binary distance measure
   minkowskiDist               Returns Minkowsky distance measure
   braycurtisDist              Returns Bray Curtis distance measure
   mahalanobisDist             Returns Mahalanobis distance measure
   jaccardDist                 Returns Jaccard distance mesaure
   sorensenDist                Returns Sorensen distance measure
  }

  A last group of functions allows to select assets by concepts from
  hierarchical or k-means clustering:
  
  % assets-selection.R
  \preformatted{
  assetsSelect                Selects similar or dissimilar assets
  .hclustSelect               Selects due to hierarchical clustering
  .kmeansSelect               Selects due to k-means clustering
  }

}


\section{3 Assets Covariance Robustification}{

  We provide several functions to compute robust measures for mean and/or
  covariance estimates which can be used for example in robustified
  Markowitz portfolio Optimization.

  % assets-meancov.R
  \preformatted{
  assetsMeanCov               Estimates mean and variance for a set of assets
  .covMeanCov                  uses sample covariance estimation
  .mveMeanCov                  uses "cov.mve" from [MASS]
  .mcdMeanCov                  uses "cov.mcd" from [MASS]
  .studentMeanCov              uses "cov.trob" from [MASS]
  .MCDMeanCov                  requires "covMcd" from [robustbase]  
  .OGKMeanCov                  requires "covOGK" from [robustbase] 
  .nnveMeanCov                 uses builtin from [covRobust]
  .shrinkMeanCov               uses builtin from [corpcor]
  .baggedMeanCov               uses builtin from [corpcor]
  .arwMeanCov                  uses builtin from [mvoutlier]
  .donostahMeanCov             uses builtin from [robust]
  .bayesSteinMeanCov           uses builtin from Alexios Ghalanos
  .ledoitWolfMeanCov           uses builtin from [tawny]
  .rmtMeanCov                  uses builtin from [tawny]
  }

  An additional function allows to detect outliers from a PCA outlier
  analysis.
  
  % assets-outliers.R
  \preformatted{
  assetsOutliers              Detects outliers in multivariate assets sets
  }

}


\section{4 Modelling Assets Sets and Testing for Normality}{

  Functions are available to fit the parameters for skew-Normal and
  related financial return distributions. 

  % assets-modelling.R
  \preformatted{
  assetsFit                   Fits the parameters of a set of assets
  assetsSim                   Simulates a set of artificial assets
  }
  
  The multivariate Shapiro test and the E-Statistic Energy Test 
  allow to test multivariate Normality of financial returns.
  
  % assets-testing.R
  \preformatted{
  assetsTest                  Tests for multivariate Normal Assets
  mvshapiroTest               Multivariate Shapiro Test
  mvenergyTest                Multivariate E-Statistic (Energy) Test
  }
  
}


\section{5 Portfolio Measures}{

  The \code{fAssets} package provides selected function to compute
  diverse risk Measure, Like VaR, CVaR, CVaR-Plus, and lambda-CVaR:

  % assets-portfolio.R
  \preformatted{
  pfolioVaR                   Computes VaR for a portfolio of assets
  pfolioCVaR                  Computes CVaR for a portfoluio of assets
  pfolioCVaRplus              Computes CVaR-Plus for a portfolio of assets
  lambdaCVaR                  Computes CVaR's atomic split value lambda
  }
  
  Also some simple fund and portfolio performance measures are included: 
  
  % assets-portfolio.R
  \preformatted{
  pfolioMaxLoss               Computes maximum loss for a portfolio 
  pfolioReturn                Computes return series for a portfolio
  pfolioTargetReturn          Computes target return for a portfolio
  pfolioTargetRisk            Computes target risk for a portfolio
  pfolioHist                  Plots a histogram of portfolio returns
  }
  
  The following functions compute marginal contributions to covariance
  risk and covariance risk budgets:
  
  % assets-mcr.R
  \preformatted{
  covarRisk                   Computes covariance portfolio risk
  mcr                         Computes marginal contribution to cov risk
  mcrBeta                     Computes beta, the rescaled mcr to cov risk
  riskContributions           Computes covariance risk contributions
  riskBudgets                 Computes covariance risk budgets
  }
  
  The computation of Lower partial moments is done by the follooing
  two functions:
  
  % assets-lpm.R
  \preformatted{
  assetsLPM                   Computes asymmetric lower partial moments
  assetsSLPM                  Computes symmetric lower partial moments
  }
  
}


\section{6 Assets Time Series and Density Plot Functions}{

  Dozens of tailored plot functions are included in the \code{fAssets}
  package. This makes it very easy to visualize properties and to 
  perform an explorative data analysis. Starting from simple time 
  series functions.

  % plotSeries.R
  \preformatted{
  assetsReturnPlot            Displays time series of individual assets
  assetsCumulatedPlot         Displays time series of individual assets
  assetsSeriesPlot            Displays time series of individual assets
  }
  
  we can also explore the distributional properties of the returns by
  histogram, density, boxplots, and QQ Plots:

  % plot-hist.R | plot-binning.R | % plot-boxplot.R | % plot-qqplot.R
  \preformatted{
  assetsHistPlot              Displays a histograms of a single asset 
  assetsLogDensityPlot        Displays a pdf plot on logarithmic scale
  assetsHistPairsPlot         Displays a bivariate histogram plot 
  assetsBoxPlot               Displays a standard box plot 
  assetsBoxPercentilePlot     Displays a side-by-side box-percentile plot
  assetsQQNormPlot            Displays normal qq-plots of individual assets
  }
  
}


\section{7 Assets Dependency and Structure Plot Functions}{


  Corellation and similarities are another source of information about 
  the dependence structure of individual financial returns. The functions
  which help us to detect those properties in data sets of financial
  assets include:

  % plot-pairs.R and plot-panels.R | plot-similaries.R | plot-ellipses.R
  \preformatted{
  assetsPairsPlot             Displays pairs of scatterplots of assets
  assetsCorgramPlot           Displays pairwise correlations between assets
  assetsCorTestPlot           Displays and tests pairwise correlations
  assetsCorImagePlot          Displays an image plot of a correlations
  covEllipsesPlot             Displays a covariance ellipses plot
  assetsDendrogramPlot        Displays hierarchical clustering dendrogram
  assetsCorEigenPlot          Displays ratio of the largest two eigenvalues
  }
  

  Beside correlations und dependencies also risk/reward graphs give 
  additional insight into the structure of assets.

  % plot-risk.R
  \preformatted{
  assetsRiskReturnPlot        Displays risk-return diagram of assets
  assetsNIGShapeTrianglePlot  Displays NIG Shape Triangle
  assetsTreePlot              Displays a minimum spanning tree of assets
  }
  
  Statistic visualized by star plots is a very appealing tool for 
  characterization and classification of assets by eye:

  % plot-stars.R
  \preformatted{
  assetsStarsPlot             Draws segment/star diagrams of asset sets
  assetsBasicStatsPlot        Displays a segment plot of basic return stats
  assetsMomentsPlot           Displays a segment plot of distribution moments
  assetsBoxStatsPlot          Displays a segment plot of box plot statistics
  assetsNIGFitPlot            Displays a segment plot NIG parameter estimates
  }
  
}


\section{About Rmetrics:}{

    The \code{fAssets} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


