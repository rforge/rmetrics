\name{attributes}


\title{Get and Set Optional Attributes of a 'timeSeries'}


\alias{attributes}
\alias{documentation}

\alias{getAttributes}  
\alias{setAttributes<-}


\description{

    Extracts or assigns optional attributes from or to a 
    \code{timeSeries} object.
    
}


\usage{
getAttributes(obj)         
setAttributes(obj) <- value
}


\arguments{
  
    \item{obj}{
        a \code{timeSeries} object whose optional attributes 
        are to be accessed.
        }
    \item{value}{
        an object, the new value of the attribute, or NULL 
        to remove the attribute.
        }
}


\details{

Each \code{timeSeries} object is documented. By default a time series
object holds in the documentation slot a string with creation time and the
user who has defined it. But this is not all. Optionally the whole creation
process and history can be recorded. For this the \code{@documentation} 
slot may have an optional \code{"Attributes"} element. This attribute is 
tracked over the whole life time of the object whenever the time series
is changed. Whenever you like to be informed about the optional attributes, 
or you like to recover them you can dot it, and evenmore, whenever you like 
to add information as an addiitonal attribute you can also do it.

The two functions \code{getAttributes} and \code{setAttributes} provide 
access to and allow to modify the optional attributes of a \code{timeSeries} 
object. 

%The replacement form causes the named attribute to take the value 
%specified (or create a new attribute with the value given).
%Optional attributes are attached to the \code{@documentation} slot of the
%S4 \code{timeSeries} object. These attributes are preserved during operations
%on \code{timeSeries} objects using the internal function \code{.appendList}.

}


\examples{
## Create an artificial timeSeries Object - 
   tS <- dummySeries()
   tS

## Get Optional Attributes -
   getAttributes(tS)
   tS@documentation
   
## Set a new Optional Attribute -
   setAttributes(tS) <- list(what="A dummy Series")
   tS
   getAttributes(tS)
   tS@documentation
}


\keyword{programming}

