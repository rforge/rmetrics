\name{AssetsModelling}

\alias{AssetsModelling}

\alias{fASSETS}
\alias{fASSETS-class}

\alias{assetsSim}
\alias{assetsFit}
\alias{assetsSelect}
\alias{assetsStats}

\alias{print.fASSETS}
\alias{plot.fASSETS}
\alias{summary.fASSETS}


\title{Modelling of Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    generate multivariate artificial data sets of assets, 
    which fit the parameters to a multivariate normal, 
    skew normal, or (skew) Student-t distribution and 
    which compute some benchmark statistics. In addition 
    a function is provided which allows for the selection 
    and clustering of individual assets from portfolios 
    using hierarchical and k-means clustering approaches.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsSim} \tab Simulates a data set of assets, \cr
    \code{assetsSelect} \tab Asset Selection from Portfolios, \cr
    \code{assetsFit} \tab Fits the parameter of a data set of assets, \cr
    \code{assetsStats} \tab Computes benchmark statistics of asset sets, \cr
    \code{print} \tab S3 print method for an object of class 'fASSETS', \cr
    \code{plot} \tab S3 Plot method for an object of class 'fASSETS", \cr
    \code{summary} \tab S3 summary method for an object of class 'fASSETS'. }
    
}
    

\usage{
assetsSim(n, dim = 2, model = list(mu = rep(0, dim), Omega = diag(dim), 
    alpha = rep(0, dim), df = Inf), assetNames = NULL) 
assetsSelect(x, method = c("hclust", "kmeans"), 
    kmeans.centers = 5, kmeans.maxiter = 10, doplot = TRUE, \dots)
assetsFit(x, method = c("st", "snorm", "norm"), title = NULL, 
    description = NULL, fixed.df = NA, \dots)
assetsStats(x)

\method{print}{fASSETS}(x, \dots)
\method{plot}{fASSETS}(x, which = "ask", \dots)
\method{summary}{fASSETS}(object, which = "all", \dots)
}


\arguments{
  
    \item{assetNames}{
        [assetsSim] - \cr
        a vector of character strings of length \code{dim} allowing
        for modifying the names of the individual assets.
        }
    \item{description}{
        [assetsFit] - \cr
        a character string, assigning a brief description to an 
        \code{"fASSETS"} object.
        }
    \item{doplot}{
        [assetsSelect] - \cr
        a logical, should a plot be displayed?
        }
    \item{fixed.df}{
        [assetsFit] - \cr
        either \code{NA}, the default, or a numeric value assigning the
        number of degrees of freedom to the model. In the case that 
        \code{fixed.df=NA} the value of \code{df} will be included in the
        optimization process, otherwise not.
        }
    \item{kmeans.centers}{
        [assetsSelect] - \cr
        either the number of clusters or a set of initial cluster centers. 
        If the first, a random set of rows in \code{x} are chosen as the 
        initial centers.   
        }
    \item{kmeans.maxiter}{
        [assetsSelect] - \cr
        the maximum number of iterations allowed.
        }
    \item{method}{
        [assetsFit] - \cr
        a character string, which type of distribution should be fitted? \cr
        \code{"st"} a multivariate skew-Student-t, \cr
        \code{"snorm"} a multivariate skew-normal, or \cr
        \code{"norm"} a multivariate normel. \cr
        By default a multivariate normal distribution will be fitted to the
        empirical market data. \cr
        [assetsSelect] - \cr
        a character string, which clustering method should be applied? 
        Either \code{hclust} for hierarchical clustering of dissimilarities,
        or \code{kmeans} for k-means clustering.
        }
    \item{model}{
        [assetsSim] - \cr
        a list of model parameters: \cr
        \code{mu} a vector of mean values, one for each asset series, \cr
        \code{Omega} the covariance matrix of assets, \cr
        \code{alpha} the skewness vector, and \cr
        \code{df} the number of degrees of freedom which is a measure for
            the fatness of the tails (excess kurtosis). \cr
        For a symmetric distribution \code{alpha} is a vector of zeros.
        For the normal distributions \code{df} is not used and set to 
        infinity, \code{Inf}. Note that all assets have the same value 
        for \code{df}.
        }   
    \item{n, dim}{
        [assetsSim] - \cr
        integer values giving the number of data records to be simulated, 
        and the dimension of the assets set.
        }
    \item{object}{
        [summary] - \cr
        An object of class \code{fASSETS}.
        }  
    \item{title}{
        [assetsFit] - \cr
        a character string, assigning a title to an 
        \code{"fASSETS"} object.
        }
    \item{which}{
        which of the five plots should be displayed? \code{which} can 
        be either a character string, \code{"all"} (displays all plots) 
        or \code{"ask"} (interactively asks which one to display), or a 
        vector of 5 logical values, for those elements which are set 
        \code{TRUE} the correponding plot will be displayed.
        }
    \item{x}{
        [assetsFit][assetsStats] - \cr
        a numeric matrix of returns or any other rectangular object like
        a data.frame or a multivariate time series objects which can be 
        transformed by the function \code{as.matrix} to an object of 
        class \code{matrix}.
        \cr
        [plot][print] - \cr
        An object of class \code{fASSETS}.
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\details{

    Data sets of assets \code{x} can be expressed as multivariate 
    'timeSeries' objects, as 'data.frame' objects, or any other rectangular 
    object which can be transformed into an object of class 'matrix'.
    
    The functions \code{assetsFit} for the parameter estimation and 
    \code{assetsSim} for the simulation of assets sets use code based on 
    functions from the mentioned package \code{"mtvnorm"}  and \code{"sn"}. 
    The required functionality from the contributed R package \code{"sn"}
    for fitting data to a multivariate Normal, skew-Normal, or skew-Student-t
    is available from builtin functions, so it is not necessary to load
    the packages \code{"mtvnorm"} and \code{"sn"}. 
    
    The function \code{assetsStats} implements benchmark formulas and
    statistics as reported in the help page of the hedge fund software 
    from \emph{www.AlternativeSoft.com}. The computed statistics are listed
    in the 'Value' section below. Note, that the functions were written for
    monthly recorded data sets. Be aware of this when you use or generate 
    asset sets on different time scales, then you have them to scale
    properly.
    
    The function \code{assetsSelect} calls the functions \code{hclust}
    and \code{kmeans} from R's \code{"stats"} package. \code{hclust}
    performs a hierarchical cluster analysis on the set of dissimilarities 
    \code{hclust(dist(t(x)))} and \code{kmeans} performs a k-means
    clustering on the data matrix itself.  

}


\notes{

    The 'Rmetrics' packages \code{fbasics} and \code{fExtremes} are
    required.
    
}

\value{

    \code{assetsSim} 
    \cr
    returns a matrix, the artifical data records represent the assets 
    of the portfolio. Row names and column names are not created, they
    have to be added afterwards.
    \cr
    
    \code{assetsSelects}
    if \code{method="hclust"} was selected then the function returns a
    S3 object of class "hclust", otherwise if \code{method="kmeans"} was 
    selected then the function returns an obkject of class list. For
    details we refer to the help pages of \code{hclust} and \code{kmeans}.
  
    \code{assetsFit} 
    \cr
    returns a S4 object class of class \code{"fASSETS"}, with the following 
    slots:

    \item{@call}{
        the matched function call.
        }
    \item{@data}{
        the input data in form of a data.frame.
        }
    \item{@description}{
        allows for a brief project description.
        }
    \item{@fit}{
        the results as a list returned from the underlying
        fitting function. 
        }
    \item{@method}{
        the selected method to fit the distribution, one 
        of \code{"norm"}, \code{"snorm"}, \code{"st"}.
        }
    \item{@model}{
        the model parameters describing the fitted parameters in 
        form of a list, \code{model=list(mu, Omega, alpha, df}.
        }
    \item{@title}{
        a title string.
        }
    
    The \code{@fit} slot is a list with the following compontents:
    (Note, not all are documented here).
    
    \item{@fit$dp}{
        a list containing the direct parameters beta, Omega, alpha. 
        Here, beta is a matrix of regression coefficients with 
        \code{dim(beta)=c(nrow(X), ncol(y))}, \code{Omega} is a 
        covariance matrix of order \code{dim}, \code{alpha} is 
        a vector of shape parameters of length \code{dim}.  
        }
    \item{@fit$se}{
        a list containing the components beta, alpha, info. Here, 
        beta and alpha are the standard errors for the corresponding 
        point estimates; info is the observed information matrix 
        for the working parameter, as explained below.
        }  
    \item{fit@optim}{ 
        the list returned by the optimizer \code{optim}; see the 
        documentation of this function for explanation of its 
        components.  
        }
    Note that the \code{@fit$model} slot can be used as input to the 
    function \code{assetsSim} for simulating a similar portfolio of 
    assets compared with the original portfolio data, usually market
    assets.
    \cr
    
  
    \code{assetsStats}
    \cr
    returns a data frame with the following entries per column and asset: 
    \cr
    \code{Records} - number of records (length of time series), \cr
    \code{paMean} - annualized (pa, per annum) Mean of Returns, \cr
    \code{paAve} - annualized Average of Returns, \cr
    \code{paVola} - annualized Volatility (standard Deviation), \cr
    \code{paSkew} - Skewness of Returns, \cr
    \code{paKurt} - Kurtosis of Returns, \cr
    \code{maxDD} - maximum Drawdown, \cr
    \code{TUW} - Time under Water, \cr
    \code{mMaxLoss} - Monthly maximum Loss, \cr
    \code{mVaR} - Monthly 99% Value-at-Risk, \cr
    \code{mModVaR} - Monthly 99% Modified Value-at-Risk, \cr 
    \code{mSharpe} - Monthly Sharpe Ratio, \cr
    \code{mModSharpe} - Monthly Modified Sharpe Ratio, and \cr
    \code{skPrice} - Skewness/Kurtosis Price.
  
}


\references{

The references are listed in the \code{MultivariateDistribution}
collection.
    
}


\seealso{

    \code{MultivariateDistribution}, \cr
    \code{hclust} and \code{kmeans}.

}


\author{

    Adelchi Azzalini for R's \code{sn} package, \cr
    Torsten Hothorn for R's \code{mtvnorm} package, \cr
    Alan Ganz and Frank Bretz for the underlying Fortran Code, \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## SOURCE("fPortfolio.A1-AssetsModelling")

## berndtInvest -
   xmpPortfolio("\nStart: Load monthly data set of returns > ")
   data(berndtInvest)
   # Exclude Date, Market and Interest Rate columns from data frame,
   # then multiply by 100 for percentual returns ...
   berndtAssets = berndtInvest[, -c(1, 11, 18)]
   rownames(berndtAssets) = berndtInvest[, 1]
   head(berndtAssets)
    
## assetsSelect -
   xmpPortfolio("\nNext: Select 4 most dissimilar assets from hclust > ")
   clustered = assetsSelect(berndtAssets, doplot = FALSE)
   myAssets = berndtAssets[, c(clustered$order[1:4])]
   colnames(myAssets)
   # Scatter and time series plot:
   par(mfrow = c(2, 1), cex = 0.7)
   plot(clustered)  
   myPrices = apply(myAssets, 2, cumsum)
   ts.plot(myPrices, main = "Selected Assets", 
     xlab = "Months starting 1978", ylab = "Price", col = 1:4)
   legend(0, 3, legend = colnames(myAssets), pch = "----", col = 1:4, cex = 1)
   
## assetsStats -
   if (require(fBasics)) assetsStats(myAssets)
   
## assetsSim -
   xmpPortfolio("\nNext: Fit a Skew Student-t > ")
   fit = assetsFit(myAssets)
   # Show Model Slot:
   fit @model
   # Simulate set with same properties:
   set.seed(1953)
   simAssets = assetsSim(n = 120, dim = 4, model = fit@model)
   head(simAssets)
   simPrices = apply(simAssets, 2, cumsum)
   ts.plot(simPrices, main = "Simulated Assets", 
     xlab = "Number of Months", ylab = "Simulated Price", col = 1:4)
   legend(0, 3, legend = colnames(simAssets), pch = "----", col = 1:4, cex = 1)
   
## plot -
   xmpPortfolio("\nNext: Show Simulated Assets Plots > ")
   if (require(fExtremes)) {
     # Show Scatterplot:
     par(mfrow = c(1, 1), cex = 0.7)
     plot(fit, which = c(TRUE, FALSE, FALSE, FALSE, FALSE))
     # Show  QQ and PP Plots:
     par(mfrow = c(2, 2), cex = 0.7)
     plot(fit, which = !c(TRUE, FALSE, FALSE, FALSE, FALSE))
   }
}


\keyword{models}
