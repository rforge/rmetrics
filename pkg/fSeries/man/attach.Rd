\name{attach}
  

\title{Attach a timSeries to the search path}


\alias{attach}
\alias{attach.timeSeries}


\description{
    
    Attach a 'timeSeries' object to the search path.        
    \cr
    
    \tabular{ll}{
    \code{attach} \tab attaches a 'timeSeries' object, \cr
    \code{detach} \tab detaches a 'timeSeries' object [see base package]. } 
    
}


\usage{
\method{attach}{timeSeries}(what, pos = 2, name = deparse(substitute(what)), 
    warn.conflicts = TRUE)
}


\note{

    Preliminary, further work has to be done.
    
}


\arguments{
  
    \item{name}{
        [attach] - \cr    
        alternative way to specify the database to be attached.
        See for details \code{help(attach,package=base)}.
        }
    \item{pos}{
        [attach] - \cr    
        an integer specifying position in \code{search()} where to attach
        the database. 
        See for details \code{help(attach,package=base)}.
        } 
    \item{warn.conflicts}{
        [attach] - \cr    
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
        See for details \code{help(attach,package=base)}.
        }
       
}


\examples{
## data - 
   x = as.timeSeries(data(msft.dat))[1:10, ]
   
## attach -
   attach(x)
   High - Low
}

   
\keyword{chron}

