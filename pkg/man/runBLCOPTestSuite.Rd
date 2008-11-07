\name{runBLCOPTestSuite}
\alias{runBLCOPTestSuite}

\title{Execute the BLCOP unit tests}
\description{
  Uses the \code{RUnit} package to execute a series of unit tests.
}
\usage{
runBLCOPTestSuite(testPath = BLCOPOptions("unitTestPath"))
}

\arguments{
  \item{testPath} {Location of the unit tests.}
}

\value{
The summary of an object returned by RUnit's \code{runTestSuite}
}

\section{Warning}{
These unit tests are in need of additional test cases, and should not be regarded as exhaustive in their current state.
}


\author{Francisco Gochez <fgochez@mango-solutions.com> }

\examples{
\dontrun{
    runBLCOPTestSuite()
    }
}
\keyword{misc}

