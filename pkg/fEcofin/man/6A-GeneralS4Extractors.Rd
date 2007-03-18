\name{GeneralS4Extractors}

\alias{GeneralS4Extractors}

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


\keyword{programming}

