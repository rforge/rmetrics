\name{getS4}

\alias{getS4}

\alias{getCall}
\alias{getModel}
\alias{getTitle}
\alias{getDescription}
\alias{getSlot}
\alias{getCall,ANY-method}
\alias{getArgs}


\title{General S4 Class Extractor Functions}


\description{

    A collection and description of functions to extract
    slots from S4 class objects.
    \cr

    The extractor functions are:

    \tabular{ll}{
    \code{getCall} \tab Extracts the call slot from a S4 object, \cr
    \code{getModel} \tab Extracts the model slot from a S4 object, \cr
    \code{getTitle} \tab Extracts the title slot from a S4 object, \cr
    \code{getDescription} \tab Extracts the description slot from a S4 object, \cr
    \code{getSlot} \tab Extracts a specified slot from a S4 object, \cr
    \code{getArgs} \tab Shows the arguments of a S4 function. }

}


\usage{
getCall(x, \dots)
getModel(object)
getTitle(object)
getDescription(object)

getSlot(object, slotName)

getArgs(f, signature)
}


\arguments{
  \item{f}{a generic function or the character-string name of one.}
  \item{object, x}{
    an object of class S4.
    (For \code{getCall()} and \R 2.14.0 and newer: \dQuote{any} object.)
  }
    \item{signature}{
        the signature of classes to match to the arguments of \code{f}
        }
    \item{slotName}{
        a character string, the name of the slot to be extracted from the
        S4 object.
      }
      \item{\dots}{Further arguments to be passed to function.}
}


\value{

    \code{getCall}\cr
    \code{getModel}\cr
    \code{getTitle}\cr
    \code{getDescription}\cr
    \code{getSlot}\cr
    return the content of the slot.

    \code{getArgs}
    returns the names of the arguments.

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

