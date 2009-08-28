\name{is.na-methods}


\docType{methods}


\alias{is.na-methods}
\alias{is.na,timeDate-method}

\title{is.na Methods}


\description{

    is.na methods for 'timeDate' objects. 

}


\examples{
# Create a timeCalendar sequence
(td <- timeCalendar())
is.na(td)

# insert NA's
is.na(td) <- 2:3
td

# test of NA's
is.na(td)
}


\keyword{chron}

