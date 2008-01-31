\name{getS4}

\alias{getS4}

\alias{isS4}

\alias{getCall}
\alias{getModel}
\alias{getTitle}
\alias{getDescription}
\alias{getSlot}


\title{General S4 Class Extractor Functions}


\description{

    A collection and description of functions to extract
    slots from S4 class objects.
    \cr
      
    The extractor functions are:
    
    \tabular{ll}{
    \code{isS4} \tab Checks if an object is a S4 object, \cr
    \code{getCall} \tab Extracts the call slot from a S4 object, \cr
    \code{getModel} \tab Extracts the model slot from a S4 object, \cr
    \code{getTitle} \tab Extracts the title slot from a S4 object, \cr
    \code{getDescription} \tab Extracts the description slot from a S4 object, \cr
    \code{getSlot} \tab Extracts a specified slot from a S4 object. }
    
}


\usage{
isS4(object)

getCall(object)
getModel(object)
getTitle(object)
getDescription(object)

getSlot(object, slotName)
}


\arguments{

    \item{object}{
        an object of class S4.
        }
    \item{slotName}{
        [getSlot] - \cr
        a character string, the name of the slot to be extracted from the 
        S4 object.
        } 
}


\author{

    Diethelm Wuertz for this R port.
    
}


\examples{
## Example S4 Representation:
   # Hyothesis Testing with Control Settings 
   setClass("hypTest", 
     representation(
       call = "call",
       data = "numeric",
       test = "list",
       description = "character")  
   )
   
## Shapiro Wilk Normaility Test
   swTest = function(x, description = "") {
     ans = shapiro.test(x)
     class(ans) = "list"
     new("hypTest", 
       call = match.call(), 
       data = x, 
       test = ans,
       description = description)
   }
   test = swTest(x = rnorm(500), description = "500 RVs")
   
## Extractor Functions:
   isS4(test)
   getCall(test)
   getDescription(test)
}


\keyword{programming}

