\name{attach}
  

\title{Attach a timSeries to the search path}


\alias{attach}
\alias{attach,timeSeries-method}


\description{
    
    Attaches a 'timeSeries' object to the search path.
    
}


\usage{
\S4method{attach}{timeSeries}(what, pos = 2, name = deparse(substitute(what)), 
    warn.conflicts = TRUE)
}


\note{

    Note, the function \code{detach} from the \code{base} package can be
    used to detach the attached objects.
}


\arguments{

    \item{name}{
        alternative way to specify the database to be attached.
        See for details \code{help(attach,package=base)}.
        }
    \item{pos}{ 
        an integer specifying position in \code{search()} where to attach
        the database. 
        See for details \code{help(attach,package=base)}.
        } 
    \item{warn.conflicts}{  
        a logical value. If \code{TRUE}, warnings are printed about 
        conflicts from attaching the database, unless that database 
        contains an object \code{.conflicts.OK}. A conflict is a 
        function masking a function, or a non-function masking a 
        non-function. 
        See for details \code{help(attach,package=base)}.
        }
    \item{what}{
        [attach] - \cr
        database to be attached. This may currently be a timeSeries object,
        a data.frame or a list or a R data file created with save or NULL 
        or an environment. 
        See for details \code{help(attach, package=base)}.
        }
        
}


\value{

    The environment is returned invisibly with a \code{name} attribute.
    
}


\examples{
## Load Microsoft Data Set - 
   x <- MSFT[1:10, ]
   colnames(x)
   
## Attach the Series and Compute the Range - 
   attach(x)
   range <- High - Low
   range
   
## Convert Vector to a timeSeries Object -
   timeSeries(data=range, charvec=time(x), units="Range")
   
## Detach the series from the search path -
   detach("x")
   ans <- try(High, silent=TRUE)
   cat(ans[1])
}

   
\keyword{chron}

