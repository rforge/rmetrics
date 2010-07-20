\name{fitted-futures}
\alias{fitted.schwartz2f.fit}
\alias{fitted,schwartz2f.fit-method}
\title{Extract Model Fitted Values}
% <---------------------------------------------------------------------->
\description{

    Function to extract fitted values from
    \code{\link{schwartz2f.fit}}-objects.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{fitted}{schwartz2f.fit}(object, data, ttm)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{object}{A \code{\link{schwartz2f.fit}}-object returned from \code{\link{schwartz2f.fit}}.}
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the price of the futures
  contract with time to maturity \code{ttm[i,j]}. \cr

}

% <---------------------------------------------------------------------->
\value{

  A matrix containing the fitted values.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.schwartz2f}}, \code{\link{schwartz2f.fit}}-class,
  \code{\link[=resid.schwartz2f.fit]{resid}}.

}
% <---------------------------------------------------------------------->
% \examples{

% }

% <---------------------------------------------------------------------->
