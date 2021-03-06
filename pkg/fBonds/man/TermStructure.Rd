\name{TermStructure}

\alias{TermStructure}


\alias{NelsonSiegel}
\alias{Svensson}


\title{Term Structure Modelling}


\description{

    A collection and description of functions
    for term structure modelling.
    \cr

    The functions are:

    \tabular{ll}{
    \code{NelsonSiegel} \tab Nelson-Siegel Term Structure, \cr
    \code{Svensson} \tab Nelson-Siegel-Svensson Term Structure. }

}


\usage{
NelsonSiegel(rate, maturity, doplot = TRUE)
Svensson(rate, maturity, doplot = TRUE)
}


\arguments{

    \item{doplot}{
        a logical. Should a plot be displayed?
        }
    \item{maturity}{
        a numeric vector of maturities on an annual scale.
        }
    \item{rate}{
        a numeric vector of forward rates.
        }

}



\value{

    a list object with entries returned from the optimization function
    \code{nlminb}.

}


\references{

McCulloch J. H. (1990);
    \emph{US Term Structure Data: 1946-87},
    Handbook of Monetary Economics, Friedman B.M. and Hahn F.H. (eds.),
    Elsevier Science.

McCulloch J. H. and Kwon, H.C. (1993);
    \emph{US Term Structure Data: 1947-1991},
    Working Paper No. 93-6, Department of Economics,
    Ohio State University.

Zivot E., Wang J.;
    \emph{Modeling Financial Time Series with S-Plus}.

}

\examples{

Yield = c(
0.04984, 0.05283, 0.05549, 0.05777, 0.05961, 0.06102, 0.06216, 0.06314,
0.06403,
0.06488, 0.06568, 0.06644, 0.06717, 0.06786, 0.06852, 0.06913, 0.06969,
0.07020,
0.07134, 0.07205, 0.07339, 0.07500, 0.07710, 0.07860, 0.08011, 0.08114,
0.08194,
0.08274, 0.08355, 0.08434, 0.08512, 0.08588, 0.08662, 0.08731, 0.08794,
0.08851,
0.08900, 0.08939, 0.08967, 0.08980, 0.08976, 0.08954, 0.08910, 0.08843,
0.08748,
0.08626, 0.08474, 0.08291)

Maturity = c(
  0.083,  0.167,  0.250,  0.333,  0.417,  0.500,  0.583,  0.667,
0.750,  0.833,
  0.917,  1.000,  1.083,  1.167,  1.250,  1.333,  1.417,  1.500,
1.750,  2.000,
  2.500,  3.000,  4.000,  5.000,  6.000,  7.000,  8.000,  9.000, 10.000,
11.000,
 12.000, 13.000, 14.000, 15.000, 16.000, 17.000, 18.000, 19.000, 20.000,
21.000,
 22.000, 23.000, 24.000, 25.000, 26.000, 27.000, 28.000, 29.000)

NelsonSiegel(Yield, Maturity)

par(mfrow = c(2, 2))
Svensson(Yield, Maturity)

}

\keyword{datasets}
