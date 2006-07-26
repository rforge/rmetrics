\name{ArfimaOxInterface}

\alias{ArfimaOxInterface}

\alias{arfimaOxFit}


\title{R Interface for Garch Arfima}


\description{

    A collection and description of functions to 
    fit the parameters of an univariate time 
    series to ARFIMA models interfacing the ARFIMA
    Ox Package. 
    \cr
    
    The ARFIMA time series models include the following 
    functiona:
    
    \tabular{rll}{
    1   \tab arfimaOxFit  \tab Fits parameters of ARFIMA models, \cr
    2   \tab print \tab S3 print method for ARFIMA models, \cr
    3   \tab plot \tab S3 plot method for ARFIMA models, \cr
    4   \tab summary \tab S3 summary method for ARFIMA models, \cr
    5   \tab fitted  \tab S3 method to extract the fitted values, \cr
    6   \tab residuals  \tab S3 method to extract the resaidual values, \cr
    7   \tab predict  \tab S3 method to predict from an ARFIMA model, \cr
    8   \tab predictPlot \tab  S3 method to plot ARFIMA predictions. } 
    
}


\usage{
arfimaOxFit(formula = x ~ arfima(1, 1), method = c("mle", "nls", "mplik"),
    trace = TRUE, title = NULL, description = NULL)
}


\arguments{

    
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{formula}{
        a formula object which specifies the ARFIMA process. 
        }
    \item{method}{
        a  character string denoting the method used for parameter
        estimation.
        }   
    \item{title}{
        a character string which allows for a project title.
        }
    \item{trace}{
        a logical flag. Should the estimation process be ttraced? 
        By default TRUE.
        }
    
}   
    

\details{

    \bold{Ox Interface:}
    \cr 
    The function \code{garchOxFit} interfaces a subset of the 
    functionality of the ARFIMA 1.01 Package written in Ox. 
    ARFIMA is one of the most sophisticated packages for modelling 
    univariate ARFIMA processes.
    \cr
    
    \bold{About Ox:}
    \cr
    Ox (tm) is an object-oriented matrix language with a comprehensive 
    mathematical and statistical function library. Many packages were 
    written for Ox including software mainly for econometric modelling. 
    The Ox packages for time series analysis and forecasting, Arfima,
    Doornik and Ooms [2003], Garch, Laurent and Peters [2005], and State 
    Space Modelling, Koopman, Shepard and Doornik [1998], are especially worth 
    to note. Since most of the R-users wan't to change to another Statistical 
    Computing environment, we made selected parts of the ARFIMA Ox software 
    available for them through an R-Interface. What you have to do, is 
    to read carefully the "Ox citation and copyright" rules and if you
    agree and fullfill the conditions, then download the OxConsole Software 
    together with the "Arfima" Package, currently implemented Arfima 1.01. 
    If you are not qualified for a free license, order your copy from 
    Timberlake Consultants. We recommend to install the "Setup.exe" 
    for "Ox3" under the path "C:\\Ox\\Ox3" and to unzip the "Arfima" Package 
    in the directory "C:\\Ox\\Ox3\\Packages". 
    \cr
    
    \bold{Distribution:}
    \cr
    Ox and Arfima are distributed by Timberlake Consultants Ltd. Timberlake 
    Consultants can be contacted through the following web site: 
    \emph{www.timberlake.co.uk}.
    \cr
    
    \bold{Installation of the Interface:}
    \cr
    In addition you have to copy the files "ArfimaOxFit.ox" and
    "ArfimaOxPredict.ox" (which is the interface written especially 
    for Rmetrics) from the "fSeries/data/" directory to the Ox library 
    directory "C:\\Ox\\Ox3\\lib".
    \cr
    
    \bold{Ox Citation and Copyright Rules:}
    \cr
    Ox and all its components are copyright of Jurgen A. Doornik. The 
    Console (command line) versions may be used freely for academic 
    research and teaching purposes only. Commercial users and others 
    who do not qualify for the free version must purchase the Windows 
    version of Ox and GiveWin with documentation, regardless of which 
    version they use (so even when only using Ox on Linux or Unix). 
    Ox should be cited whenever it is used. Refer to the two references 
    given below. Note, failure to cite the use of Ox in published work 
    may result in loss of the right to use the free version, and an 
    invoice at the full commercial price. Ox is available from Timberlake 
    Consultants. The Ox syntax is public, and you may do with your own 
    Ox code whatever you wish, including the files "ArfimaOx*.ox".
    \cr
    
    \bold{Work to do:}
    \cr
    Note, only a small part of the functionalities are interfaced until
    now to Rmetrics. But, principally it would be possible to interface 
    also other functionalities offered by the Ox Arfima Package. This 
    work is left to the Rmetrics/Ox user.
    
}


\value{

    \code{arfimaOxFit} 
    \cr
    returns an S4 object of class \code{"fARMA"}, with the following 
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
        time series model function. 
        }
    \item{method}{
        the selected time series model naming the applied method.
        }
    \item{formula}{
        the formula expression describing the model.
        }
    \item{parameters}{
        named parameters or coefficients of the fitted model.
        }
    \item{title}{
        a title string.
        }
        
    For details please consult \code{help(ArmaModelling)}.

}


\references{

Doornik J.A. (2002), 
    Object-Oriented Matrix Programming Using Ox, 
    London, 3rd ed.: Timberlake Consultants Press and Oxford: 
    \emph{www.doornik.com}. 

Doornik J.A., Ooms M. (2001),   
    A Package for Estimating, Forecasting and Simulating Arfima
    Models, Arfima Package 1.01 for Ox, pp. 32.
    
Doornik J.A., Ooms M. (2003),
    Computational Aspects of Maximum Likelihood Estimation of 
    Autoregressive Fractionally Integrated Moving Average Models,
    Computational Statistics and Data Analysis 42, 333--348.
 
Koopman J.S., Shepard N., Doornik J.A. (1999),
    Statistical Algorithms for Models in State Space using SsfPack 2.2,
    Econometrics Journal 2, 113--166.
    
Laurent S., Peters J.P., [2005], 
    G@RCH 4.0, Estimating and Forecasting ARCH Models, 
    Timberlake Consultants, www.timberlake.co.uk 
       
}


\author{

    Jurgen A. Doornik for the Ox Environment, \emph{www.doornik.com}, \cr 
    Jurgen A. Doornik and Marius Ooms for the Arfima Ox package, \emph{www.doornik.com}, \cr 
    Diethelm Wuertz for R's Ox Arfima interface. 
    
}


\note{

    The R functions were tested only under MS Windows XP. Please report
    your experience with other operating systems to \emph{info@metrics.org}.
    The functions should also run under the newer version "Ox4".

}
    

\seealso{

    \code{ArmaModelling}.
    
}

\examples{
\dontrun{
## SOURCE("fSeries1.1B-ArfimaOxInterface")

## arfimaOxFit -
   x = armaSim(model = list(ar = c(0.5, - 0.5), d = 0.3, ma = 0.1), n = 500)
   x
   object = armaOxFit(formula = x ~ arfima(2,1))
   object
    
   print(object)
   plot(object)
   summary(object)
    
   fitted(object)
   residuals(object)
    
   predict(object)
   predictPlot(object)
}
}


\keyword{models}
