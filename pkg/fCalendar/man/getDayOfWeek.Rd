\name{getDayOfWeek}


\alias{getDayOfWeek}
\alias{getDayOfYear}


\title{The Day of the Week}


\description{
    
    Returns the day of the week or the day of the 
    week.
 
    \tabular{ll}{
    \code{getDayOfWeek} \tab Returns the day of the week to a 'timeDate' object, \cr
    \code{getDayOfYear} \tab Returns the day of the year to a 'timeDate' object. }
      
}


\usage{
getDayOfWeek(x)
getDayOfYear(x)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }  
       
}


\value{


    the function \code{getDayOfWeek} returns a three letter character 
    string with the names of the day of the week, and the function 
    \code{getDayOfYear} returns the day count as integer value starting
    January, 1st. 

}


\seealso{

    \code{timeDate}
}



\examples{  
## Dates in April, currentYear: 
   tS = timeSequence(
      from = paste(currentYear, "-03-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS
       
## Subset Wednesdays:
   getDayOfWeek(tS)
   tS[getDayOfWeek(tS) == "Wed"]
}


\keyword{chron}

