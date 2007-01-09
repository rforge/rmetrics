\name{LongRangeDependence}

\alias{LongRangeDependence}

\alias{fbmSim}
\alias{fgnSim}
\alias{farimaSim}

\alias{fgnTrueacf}
\alias{fgnTruefft}
\alias{farimaTrueacf}
\alias{farimaTruefft}

\alias{fHURST}
\alias{fHURST-class}
\alias{show.fHURST}
\alias{show,fHURST-method}

\alias{aggvarFit}
\alias{diffvarFit}
\alias{absvalFit}
\alias{higuchiFit}
\alias{pengFit}
\alias{rsFit}
\alias{perFit}
\alias{boxperFit}
\alias{whittleFit}

\alias{waveletFit}

\alias{hurstSlider}

\title{Long Range Dependence Modelling}


\description{

    A collection and description of functions 
    to investigate the long range dependence or 
    long memory behavior of an univariate time 
    series process. Included are functions to 
    simulate fractional Gaussian noise and 
    fractional ARMA processes, functions to model 
    the true autocorrelations and the spectrum of 
    these processes, and functions to compute the 
    Hurst exponent by several different methods.
    \cr
    
    The Functions and methods are:
    
    Functions to simulate long memory time series processes:
    
    \tabular{ll}{
    \code{fnmSim} \tab Simulates fractional Brownian motion, \cr
    \code{- mvn} \tab from the numerical approximation of the stochastic integral, \cr
    \code{- chol} \tab from the Choleski's decomposition of the covariance matrix, \cr
    \code{- lev} \tab using the method of Levinson, \cr
    \code{- circ} \tab using the method of Wood and Chan, \cr
    \code{- wave} \tab using the wavelet synthesis, \cr
    \code{fgnSim} \tab Simulates fractional Gaussian noise, \cr
    \code{- beran} \tab using the method of Beran, \cr
    \code{- durbin} \tab using the method Durbin and Levinson, \cr
    \code{- paxson} \tab using the method of Paxson, \cr
    \code{farimaSim} \tab simulates FARIMA time series processes. }
       
    Functions to model the true autocorrelation function and spectrum:
    
    \tabular{ll}{ 
    \code{fgnTrueacf} \tab Returns true FGN covariances, \cr
    \code{fgnTruefft} \tab returns true FGN fast Fourier transform, \cr  
    \code{farimaTrueacf} \tab returns true FARIMA covariances, \cr
    \code{farimaTruefft} \tab returns true FARIMA fast Fourier transform. }
        
    Functions to estimate the Hurst exponent:
    
    \tabular{ll}{
    \code{aggvarFit} \tab Aggregated variance method, \cr
    \code{diffvarFit} \tab Differenced aggregated variance method, \cr
    \code{absvalFit} \tab aggregated absolute value (moment) method, \cr
    \code{higuchiFit} \tab Higuchi's or fractal dimension method, \cr
    \code{pengFit} \tab Peng's or variance of residuals method, \cr
    \code{rsFit} \tab R/S Rescaled Range Statistic method, \cr
    \code{perFit} \tab periodogram method, \cr
    \code{boxperFit} \tab boxed (modified) periodogram method, \cr
    \code{whittleFit} \tab Whittle estimator, \cr
    \code{hurstSlider} \tab Interactive Display of Hurst Estimates. }
       
    Function for the wavelet estimator:
    \tabular{ll}{
    \code{waveletFit} \tab wavelet estimator. }
    
}


\usage{
fbmSim(n = 100, H = 0.7, method = c("mvn", "chol", "lev", "circ", "wave"),
    waveJ = 7, seed = NULL, doplot = TRUE, fgn = FALSE)
fgnSim(n = 1000, H = 0.7, method = c("beran", "durbin", "paxson"))
farimaSim(n = 1000, model = list(ar = c(0.5, -0.5), d = 0.3, ma = 0.1),
    method = c("freq", "time"), \dots) 
    
fgnTrueacf(n, H) 
fgnTruefft(n, H) 
farimaTrueacf(n, H) 
farimaTruefft(n, H)
   
aggvarFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL)    
diffvarFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL) 
absvalFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), moment = 1, 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL) 
higuchiFit(x, levels = 50, minnpts = 2, cut.off = 10^c(0.7, 2.5), 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
pengFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
    method = c("mean", "median"), 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
rsFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
perFit(x, cut.off = 0.1, method = c("per", "cumper"), 
    doplot = FALSE, title = NULL, description = NULL)
boxperFit(x, nbox = 100, cut.off = 0.10, 
    doplot = FALSE, trace = FALSE, title = NULL, description = NULL)      
whittleFit(x, order = c(1, 1), subseries = 1, method = c("fgn", "farma"), 
    trace = FALSE, spec = FALSE, title = NULL, description = NULL)
hurstSlider(x = fgnSim())
  
waveletFit(x, length = NULL, order = 2, octave = c(2, 8), 
    doplot = FALSE, title = NULL, description = NULL)
        
show.fHURST(object)
}


\arguments{
    
    \item{cut.off}{
        [*Fit] - \cr
        a numeric vector with the lower and upper cut-off 
        points for the estimation. They should be chosen 
        to define a linear range. The default values are 
        c(0.7, 2.5), i.e. 10\^0.7 and 10\^2.5, respectively.
        }
    \item{description}{
        [*Fit] - \cr
        a character string which allows for a brief description.
        }
    \item{doplot}{
        [*Fit] - \cr
        a logical flag, by default FALSE. Should a plot be displayed?
        }
    \item{fgn}{
        [fbmSim] - \cr
        a logical flag, if \code{FALSE}, the functions returns a FBM 
        series otherwise a FGN series.
        }
    \item{H}{
        [fgnSim] - \cr
        the Hurst exponent, a numeric value between 0.5 and 1,
        by default 0.7.
        }
   \item{length}{  
        [waveletFit] - \cr
        the length of data to be used, must be power of 2.
        If set to \code{NULL}, the previous power will be used.
        }
    \item{levels}{
        [*Fit] - \cr
        the number of aggregation levels or number of blocks from 
        which the variances or moments are computed.
        }
    \item{method}{
        [fbmSim] - \cr
        the method how to generate the FBM time series sequence, one
        of the following five character strings: \code{"mvn"}, 
        \code{"chol"}, \code{"lev"}, \code{"circ"}, or \code{"wave"}.
        [fgnSim] - \cr
        the method how to generate the FGN time series sequence, one
        of the following three character strings: \code{"beran"}, 
        \code{"durbin"}, or \code{"paxson"}.
        \cr
        [farimaSim] - \cr
        the method how to generate the time series sequence, one
        of the following tow character strings: \code{"freq"}, or
        \code{"time"}.
        \cr
        [pengFit] - \cr
        a string naming the method how to do the averaging, either
        calculating the \code{"mean"} or the \code{"median"}. 
        \cr
        [perFit] - \cr
        a string naming the method how to fit the data, either
        using the peridogram itself \code{"per"}, or using the
        cumulated periodogram \code{"cumper"}. 
        \cr
        [whittleFit] - \cr
        a string naming the underlying time series process to 
        be estimated, either \code{"fgn"} for FGN processes, or
        \code{"farima"}for FARIMA models.
        }
    \item{minnpts}{
        [*Fit] - \cr
        the minimum number of points or blocksize to be used to 
        estimate the variance or moments at any aggregation level.
        }
    \item{model}{
        a list with model parameters \code{ar}, \code{ma} and \code{d}.
        \code{ar} is a numeric vector giving the AR coefficients, 
        \code{d} is an integer value giving the degree of differencing,
        and \code{ma} is a numeric vector giving the MA coefficients.
        Thus the order of the time series process is FARMA(p, d, q)
        with \code{p=length(ar)} and \code{q=length(ma)}. \code{d} is
        a fractional value for FARMA models. By default an 
        FARMA(2, d, 1) model with coefficients \code{ar=c(0.5, -0.5)},
        \code{ma=0.1}, and \code{d=0.3} will be generated.
        }
    \item{moment}{
        [absvalHurst] - \cr
        an integer value, by default 1 which denotes absolute values. 
        For values larger than one this argument determines what 
        absolute moment should be calculated.
        }
    \item{n}{
        [fgnSim][farimaSim] - \cr
        number of data points to be simulated, a numeric value, 
        by default 1000.
        }
    \item{nbox}{
        [boxperFit] - \cr
        is the number of boxes to divide the data into. A numeric value,
        by default 100.
        }
    \item{object}{
        an object of class \code{fHurst}.
        }
    \item{octave}{  
        [waveletFit] - \cr
        beginning and ending octave for estimation. An integer
        vector with two elements. By default \code{c(2, 8)}. If the
        upper value is too large, it will be replaced by the maximum
        allowed value.
        }
    \item{order}{   
        [waveletFit] - \cr
        the order of the wavelet. An integer value, by default 
        \code{2}.
        }
    \item{seed}{
        [fbmSim] - \cr
        an integer value setting the seed for the random number generation.
        }
    \item{spec}{
        [whittleFit] - \cr
        Should the periodogram be returned? A logical flag, by default 
        FALSE.
        }
    \item{subseries}{
        [whittleFit] - \cr
        allows optionally to subdivide the series into subseries. A
        numeric value, by default 1.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{trace}{
        a logical value, by defaul FALSE. Should the estimation 
        process be traced?
        }
    \item{waveJ}{
        [fbmSim] - \cr
        an integer parameter for the simulation of FBM using the wavelet 
        method.
        }
    \item{x}{
        [*Fit] - \cr
        the numeric vector of data, an object of class \code{timeSeries},
        or any other object which can be transofrmed into a numeric
        vector by the function \code{as.vector}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{   

    \code{fgnSim} and \code{farimaSim} return a numeric vector of length 
    \code{n}, the FGN or FARIMA series.
    \cr
    
    \code{fgnTrueacf(n, H)},
    \code{fgnTruefft(n, H)}, 
    \code{farimaTrueacf(n, H)}, and
    \code{farimaTruefft(n, H)}
    return the true covariance and the true spectrum of the FGN
    or FARIMA time series process.
    \cr
    
    \code{*Fit} returns an S4 object of class \code{fHURST} with the
    following slots:
    
    \item{@call}{
       the function call.      
       }
    \item{@method}{
       a character string with the selected method string.      
       }
    \item{@hurst}{
       a list with at least one element, the Hurst exponent named
       \code{H}. Optional values may be the value of the fitted 
       slope \code{beta}, or information from the fit.      
       }
    \item{@parameters}{
       a list with a varying number of elements describing
       the input parameters from the argument list.      
       }
    \item{@data}{
       a list holding the input data.
       }
    \item{@fit}{
       a list object with all information of the fit.
       }
    \item{@plot}{
       a list object which holds information to create a plot
       of the fit.
       }
    \item{@title}{
       a character string with the name of the test.
       }
    \item{@description}{
       a character string with a brief description of the test.
       }
       
    \code{waveletFit}
    \cr
    
}


\details{

    \bold{I: Functions to Simulate Long Memory Processes:}
    \cr
    
    \emph{Fractional Gaussian Noise:}
    \cr
    The function \code{fgnSim} simulates a series of fractional
    Gaussian noise, FGN. FGN provides a parsimonious model for 
    stationary increments of a self-similar process parameterised 
    by the Hurst exponent H and variance. Fractional Gaussian noise 
    with H < 0.5 demonstrates negatively autocorrelated or 
    anti-persistent behaviour, and FGN with H > 0.5 demonstrates 
    1/f , long memory or persistent behaviour, and the special 
    case. The case H = 0.5 corresponds to the classical Gaussian 
    white noise. One can select from three different 
    methods. The first generator named \code{"beran"} uses
    the fast Fourier transform to generate the series based on 
    SPLUS code written originally by J. Beran [1994]. The second
    generator named \code{"durbin"} produces a FGN series by 
    using the Durbin-Levinson coefficients. The algorithm was
    reimplemented in pure S based on the C source code written by
    V. Teverovsky [199x]. The third generator named 
    \code{"paxson"} was proposed by V. Paxson [199x], this
    approaximate method is a very fast and requires low storage. 
    However, the algorithm reveals some weakness in the method
    which was discussed by D.A. Rolls [2001].
    \cr
    
    \emph{Fractional ARIMA Processes:}
    \cr
    The function \code{farimaSim} is a generator for fractional
    ARIMA time series processes. A Gaussian FARIMA(0,d,0) series
    can be created, where \emph{d } is related to the the Hurst 
    exponent \emph{H} through \emph{d=H-0.5}. This is a particular 
    case of the more general Gaussian FARIMA(p,d,q) process which 
    follows the same asymptotic relations for their autocovariance 
    and the spectral density as do the Gaussian FARIMA(0,d,0) 
    processes. Two different generators are implement in S. The 
    first named \code{"freq"} works in the frequence domain and 
    generates the series from the fast Fourier transform based on 
    SPLUS code written originally by J. Beran [1994]. The second 
    method creates the series in the time domain, therefore named 
    \code{"time"}. The algorithm was reimplemented in pure S based 
    on the Fortran source code from the R's \code{fracdiff} package 
    originally written by C. Fraley [1991]. Details for the algorithm 
    are given in J Haslett and A.E. Raftery [1989]. 
    \cr
    
    
    \bold{II: Functions to Model True Correlations and Spectrum:}
    \cr
    
    The functions \code{fgnTrueacf} and \code{farimaTrueacf} return the
    true covariances of an FGN and Gaussian FARIMA(0,d,0) time 
    series process.
    The functions \code{fgnTruefft} and \code{farimaTruefft} return the
    true fast Fourier transform of an FGN and Gaussian FARIMA(0,d,0) 
    time series process.
    The R functions are implemented from SPlus code written by
    J. Beran [1994].
    \cr

    
    \bold{III: Functions to Estimate the Hurst Exponent:}
    \cr
    
    These are 9 functions as described by M.S. Taqqu, V. Teverovsky,
    and W. Willinger [1995] to estimate the self similarity parameter 
    and/or the intensity of long-range dependence in a time series.
    \cr
    
    
    %% 3.1
    \emph{Aggregated Variance Method:}
    \cr
    The function \code{aggvarFit} computes the Hurst exponent from 
    the variance of an aggregated FGN or FARIMA time series process. 
    The original time series is divided into blocks of size \code{m}. 
    Then the sample variance within each block is computed. The slope 
    \code{beta=2*H-2} from the least square fit of the logarithm of 
    the sample variances versus the logarithm of the block sizes 
    provides an estimate for the Hurst exponent \code{H}. 
    \cr
    
    
    %% 3.2
    \emph{Differenced Aggregated Variance Method:}
    \cr
    To distinguish jumps and slowly decaying trends which are two
    types of non-stationary, from long-range dependence, the function 
    \code{diffvarFit} differences the sample variances of successive 
    blocks. The slope \code{beta=2*H-2} from the least square fit of 
    the logarithm of the differenced sample variances versus the 
    logarithm of the block sizes provides an estimate for the Hurst 
    exponent \code{H}. 
    \cr
 
       
    %% 3.3
    \emph{Aggregated Absolute Value/Moment Method:}
    \cr
    The function \code{absvalFit} computes the Hurst exponent from
    the moments \code{moment=M} of absolute values of an aggregated 
    FGN or FARIMA time series process. The first moment \code{M=1}
    coincides with the absolute value method, and the second moment 
    \code{M=2} with the aggregated variance method. Again, the slope 
    \code{beta=M*(H-1)} of the regression line of the logarithm of 
    the statistic versus the logarithm of the block sizes provides 
    an estimate for the Hurst exponent \code{H}. 
    \cr
    
      
    %% 3.4
    \emph{Higuchi or Fractal Dimension Method:}
    \cr
    The function \code{higuchiFit} implements a technique which is 
    very similar to the absolute value method. Instead of blocks a 
    sliding window is used to compute the aggregated series. The 
    function involves the calculation the calculation of the length 
    of a path and, in principle, finding its fractal Dimension \code{D}. 
    The slope \code{D=2-H} from the least square fit of the logarithm 
    of the expected path lengths versus the logarithm of the block 
    (window) sizes provides an estimate for the Hurst exponent \code{H}. 
    \cr
    
    
    %% 3.5
    \emph{Peng or Variance of Residuals Method:}
    \cr
    The function \code{pengFit} uses the method described by peng.
    In Peng's variance of residuals method the series is also divided
    into blocks of size \code{m}. Within each block the cumulated
    sums are computed up to \code{t} and a least-squares line 
    \code{a+b*t} is fitted to the cumulated sums. Then the sample 
    variance of the residuals is computed which is proportional to
    \code{m^(2*H)}. The \code{"mean"} or \code{"median"} are
    computed over the blocks. The slope \code{beta=2*H} from the 
    least square provides an estimate for the Hurst exponent \code{H}. 
    \cr
    
    
    %% 3.6
    \emph{The R/S Method:}
    \cr
    The function \code{rsFit} implements the algorithm named 
    \emph{rescaled range analysis} which is dicussed for example 
    in detail by B. Mandelbrot and Wallis [199x], B. Mandelbrot [199x] 
    and B. Mandelbrot and M.S. Taqqu [199x].
    \cr
    
    
    %% 3.7
    \emph{The Periodogram Method:}
    \cr
    The function \code{perFit} estimates the Hurst exponent from the
    periodogram. In the finite variance case, the periodogram is an
    estimator of the spectral density of the time series. A series
    with long range dependence will show a spectral density with a
    lower law behavior in the frequency. Thus, we expect that a
    log-log plot of the periodogram versus frequency will display
    a straight line, and the slopw can be computed as \emph{1-2H}.
    In practice one uses only the lowest 10\% of the frequencies, 
    since the power law behavior holds only for frequencies close to
    zero. Varying this cut off may provide additional information.
    Plotting \emph{H} versus the cut off, one should select that
    cut off where the curve flattens out to estimate \code{H}.
    This approach can be selected by the argument \code{method="per"}.
    Alternatively we can select \code{method="cumper"}. In this case,
    instead of using the periodgram itself, the cmulative periodgram
    will be investigated. The slope of the double logarithmic fit
    is given by \emph{2-2H}. More details can be found in the work
    of J. Geweke and S. Porter-Hudak [1983] and in Taqqu [?].
    \cr
    
    
    %% 3.8
    \emph{The Boxed or Modified Periodogram Method:}
    \cr
    The function \code{boxperFit} is a modification of the periodogram
    method. The algorithm devides the frequency axis into logarithmically
    equally spaced boxes and averages the periodogram values corresponding 
    to the frequencies inside the box.
    \cr
    
    
    %% 3.9
    \emph{The Whittle Estimator:}
    \cr
    The function \code{whittleFit} performs also a periodogram analysis.
    The algorithm is based on the minimization of a likelihood function
    defined in the frequency domain. For FGN and FARIMA(0,d,0) processes
    the parameter \emph{H} or \emph{d} is the unknown parameter which
    minimizes the function. This approach also allows to compute confidence
    intervals. Unlike the previous eight estimators the Whittle estimator
    is not a graphical method, it just returns the values of \emph{H}
    or \emph{d} together with their confidence intervals. The function
    allows also to investigate FARIMA(p,d,q) models, then the parameter
    set to be optimized is enlarged by the AR and MA coefficients. It
    is worth to remark, that the empirical series is required to be a 
    Gaussian process and that the underlying form must be specified. 
    \cr
       
    The original functions were written by V. Teverovsky and W. Willinger
    for SPLUS calling internal functions written in C. The software can 
    be found on M. Taqqu's home page:\cr
    \emph{http://math.bu.edu/people/murad/} 
    \cr
    In addition the Whittle estimator uses SPlus functions written 
    by J. Beran. They can be found in the appendix of his book or on 
    the StatLib server:\cr
    \emph{http://lib.stat.cmu.edu/S/}
    \cr
    Note, all nine R functions and internal utility functions are 
    reimplemented entirely in S.
    \cr
    
    
    \bold{IV: Functions to perform a Wavelet Analysis:}
    \cr

    The function \code{waveletFit} computes the Discrete Wavelet 
    Transform, averages the squares of the coefficients of the transform, 
    and then performs a linear regression on the logarithm of the 
    average, versus the log of the scale parameter of the transform. 
    The result should be directly proportional to \code{H} providing
    an estimate for the Hurst exponent. 
    %\cr
    
    
    %\bold{V: Statistical Tests:}
    %\cr
    
    %The ...
    
}


\author{

    V. Paxson, code as listed in the Appendix of his paper 1995, \cr
    J. Beran, ported by Maechler, code as listed in the Appendix of his Book, \cr
    M.S. Taqqu et al. for the S-Plus and C code concerned with the Hurst exponent, \cr
    C. Fraley for the FARIMA simulation code, \cr
    Guy Nason for the functions from the R package 'wavethresh', \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Beran J. (1992);
    \emph{Statistics for Long-Memory Processes},
    Chapman and Hall, New York, 1994.
    
%Harte D.S. (1987);
%   \emph{Test for Hurst Effect},
%    Biometrica 74, pp. 95-102.
    
Haslett J., Raftery A.E. (1989);
    \emph{Space-Time Modelling with Long-Memory Dependence: 
        Assessing Ireland's Wind Power Resource},
    Applied Statistics 38, pp. 1--50.

Paxson V. (1995); 
    \emph{Fast Approximation of Self-Similar Network Traffic},
    Technical report, LBL-36750/UC-405, Berkeley, and
    Computer Communcation Review27, p.5--18, 1997.
    
Rolls D.A. (2001);
    \emph{Improved Fast Approximate Synthesis of Fractional Gaussian Noise},
    Thesis, Department of Mathematics and Statistics,
    Queen's University at Kingston, Kingston, Ontario, Canada, 5 pages.
    
Taqqu M., et al.
    \emph{Hurst Exponent},
    Several Preprints.
    
}


\examples{
## SOURCE("fSeries.3A-LongRangeDependence")

## fgnSim -
   par(mfrow = c(3, 1), cex = 0.75)  
   
   # Beran's Method:
   plot(fgnSim(n = 200, H = 0.75), type = "l",  
     ylim = c(-3, 3), xlab = "time", ylab = "x(t)", main = "Beran")
   
     # Durbin's Method:
   plot(fgnSim(n = 200, H = 0.75, method = "durbin"), type = "l",
     ylim = c(-3, 3), xlab = "time", ylab = "x(t)", main = "Durbin")
   
     # Paxson's Method:
   plot(fgnSim(n = 200, H = 0.75, method = "paxson"), type = "l",
     ylim = c(-3, 3), xlab = "time", ylab = "x(t)", main = "Paxson")
}


\keyword{models}

