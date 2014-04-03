\name{fExtremes-package}


\alias{fExtremes-package}
\alias{fExtremes}


\docType{package}


\title{Modelling Extreme Events in Finance}


\description{

    The Rmetrics "fExtemes" package is a collection of functions to 
    analyze and model extreme events in Finance and Insurance.
    
}


\details{

    \preformatted{
        Package:    \tab fExtremes\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{
}


\section{2 Data and their Preprocessing}{

    \emph{Data Sets}
    
    Data sets used in the examples of the timeSeries packages.
        
    \emph{Data Preprocessing}
    
    These are tools for data preprocessing, including functions to 
    separate data beyond a threshold value, to compute blockwise data 
    like block maxima, and to decluster point process data.
    
    \preformatted{
    blockMaxima     extracts block maxima from a vector or a time series 
    findThreshold   finds upper threshold for a given number of extremes  
    pointProcess    extracts peaks over Threshold from a vector or a time series 
    deCluster       de-clusters clustered point process data
    }
    
}


\section{2 Explorative Data Analysis of Extremes}{

    This section contains a collection of functions for explorative data 
    analysis of extreme values in financial time series. The tools include 
    plot functions for emprical distributions, quantile plots, graphs 
    exploring the properties of exceedences over a threshold, plots for 
    mean/sum ratio and for the development of records. The functions are:
    
    \preformatted{
    emdPlot         plots of empirical distribution function
    qqparetoPlot    exponential/Pareto quantile plot
    mePlot          plot of mean excesses over a threshold
    mrlPlot         another variant, mean residual life plot
    mxfPlot         another variant, with confidence intervals
    msratioPlot     plot of the ratio of maximum and sum
    }
    
    \preformatted{
    recordsPlot     Record development compared with iid data
    ssrecordsPlot   another variant, investigates subsamples
    sllnPlot        verifies Kolmogorov's strong law of large numbers
    lilPlot         verifies Hartman-Wintner's law of the iterated logarithm
    }
    
    \preformatted{
    xacfPlot        plots ACF of exceedences over a threshold
    }
    
    
    \emph{Parameter Fitting of Mean Excesses}
    
    \preformatted{
    normMeanExcessFit    fits mean excesses with a normal density
    ghMeanExcessFit      fits mean excesses with a GH density   
    hypMeanExcessFit     fits mean excesses with a HYP density   
    nigMeanExcessFit     fits mean excesses with a NIG density  
    ghtMeanExcessFit     fits mean excesses with a GHT density
    }
    
}


\section{3 GPD Peak over Threshold Modeling}{

    \emph{GPD Distribution}
    
    A collection of functions to compute the generalized Pareto 
    distribution. The functions compute density, distribution 
    function, quantile function and generate random deviates 
    for the GPD. In addition functions to compute the true 
    moments and to display the distribution and random variates 
    changing parameters interactively are available.
    
    \preformatted{
    dgpd            returns the density of the GPD distribution
    pgpd            returns the probability function of the GPD
    qgpd            returns quantile function of the GPD distribution
    rgpd            generates random variates from the GPD distribution
    gpdSlider       displays density or rvs from a GPD
    }
    
    
    \emph{GPD Moments}
    
    \preformatted{
    gpdMoments      computes true mean and variance of GDP
    }
    
    
    \emph{GPD Parameter Estimation}
    
    This section contains functions to fit and to simulate processes that
    are generated from the generalized Pareto distribution. Two 
    approaches for parameter estimation are provided: Maximum likelihood 
    estimation and the probability weighted moment method.
     
    \preformatted{
    gpdSim          generates data from the GPD distribution
    gpdFit          fits data to the GPD istribution
    }
    
    
    \emph{GPD print, plot and summary methods}
    
    \preformatted{
    print           print method for a fitted GPD object
    plot            plot method for a fitted GPD object
    summary         summary method for a fitted GPD object
    }
    
    
    \emph{GDP Tail Risk}
    
    The following functions compute tail risk under the GPD approach.
    
    \preformatted{
    gpdQPlot        estimation of high quantiles
    gpdQuantPlot    variation of high quantiles with threshold
    gpdRiskMeasures prescribed quantiles and expected shortfalls
    gpdSfallPlot    expected shortfall with confidence intervals
    gpdShapePlot    variation of GPD shape with threshold
    gpdTailPlot     plot of the GPD tail
    }
    
    % \preformatted{
    % tailPlot
    % tailSlider
    % tailRisk
    % }
  
}


\section{4 GEV Block Maxima Modeling}{

    \emph{GEV Distribution}
    
    This section contains functions to fit and to simulate processes 
    that are generated from the generalized extreme value distribution
    including the Frechet, Gumbel, and Weibull distributions.
    
    \preformatted{
    dgev            returns density of the GEV distribution
    pgev            returns probability function of the GEV
    qgev            returns quantile function of the GEV distribution
    rgev            generates random variates from the GEV distribution
    gevSlider       displays density or rvs from a GEV
    }
    
    
    \emph{GEV Moments}
    
    \preformatted{
    gevMoments      computes true mean and variance
    }
    
    
    \emph{GEV Parameter Estimation}
    
    
    A collection to simulate and to estimate the parameters of
    processes generated from GEV distribution.
    
    \preformatted{
    gevSim          generates data from the GEV distribution
    gumbelSim       generates data from the Gumbel distribution
    gevFit          fits data to the GEV distribution
    gumbelFit       fits data to the Gumbel distribution
    }
    
    \preformatted{
    print           print method for a fitted GEV object
    plot            plot method for a fitted GEV object
    summary         summary method for a fitted GEV object
    }
    
    
    \emph{GEV MDA Estimation}
    
    Here we provide Maximum Domain of Attraction estimators and visualize 
    the results by a Hill plot and a common shape parameter plot from
    the Pickands, Einmal-Decker-deHaan, and Hill estimators.
    
    \preformatted{
    hillPlot        shape parameter and Hill estimate of the tail index
    shaparmPlot     variation of shape parameter with tail depth
    }
    
    
    \emph{GEV Risk Estimation}
    
    \preformatted{
    gevrlevelPlot   k-block return level with confidence intervals
    }
    
}


\section{4 Value at Risk}{

    Two functions to compute Value-at-Risk and conditional 
    Value-at-Risk.
    
    \preformatted{
    VaR             computes Value-at-Risk
    CVaR            computes conditional Value-at-Risk
    }
    
}


\section{5 Extreme Index}{

    A collection of functions to simulate time series with a known extremal 
    index, and to estimate the extremal index by four different kind of 
    methods, the blocks method, the reciprocal mean cluster size method, 
    the runs method, and the method of Ferro and Segers.
    
    \preformatted{
    thetaSim             simulates a time Series with known theta
    blockTheta           computes theta from Block Method
    clusterTheta         computes theta from Reciprocal Cluster Method
    runTheta             computes theta from Run Method
    ferrosegersTheta     computes theta according to Ferro and Seegers
    }
    
    \preformatted{
    exindexPlot          calculatess and plots Theta(1,2,3)
    exindexesPlot        calculates Theta(1,2) and plots Theta(1)
    }
    
}


\section{About Rmetrics:}{

    The \code{fExtremes} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


