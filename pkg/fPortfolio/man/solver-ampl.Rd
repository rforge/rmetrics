\name{solver-ampl}
 

\alias{amplModelOpen}       
\alias{amplModelAdd}    
\alias{amplModelShow}       

\alias{amplDataOpen}  
\alias{amplDataAdd}      
\alias{amplDataAddValue}   
\alias{amplDataAddVector}  
\alias{amplDataAddMatrix}  
\alias{amplDataSemicolon}  
\alias{amplDataShow}        

\alias{amplRunOpen}         
\alias{amplRunAdd}         
\alias{amplRunShow}         

\alias{amplOutShow}        


\title{AMPL Interface}


\description{
  
    R/AMPL Interface functions.
    
}
    

\usage{
amplModelOpen(project)  
amplModelAdd(model, project)      
amplModelShow(project)       

amplDataOpen(project)     
amplDataAdd(name, data, type, project) 
amplDataAddValue(data, value, project)  
amplDataAddVector(data, vector, project)  
amplDataAddMatrix(data, matrix, project) 
amplDataSemicolon(project)  
amplDataShow(project)        

amplRunOpen(project)         
amplRunAdd(run, project)          
amplRunShow(project)        

amplOutShow(project)      
}


\arguments{
  
    \item{project}{
        a character string, the AMPL project name. 
        }   
    \item{model}{...}
    \item{data}{...}
    \item{run}{...}
    \item{type}{...}
    \item{name}{...}
    \item{value}{...}
    \item{vector}{...}
    \item{matrix}{...}
        
}


\value{
  
    returns AMPL files.
    
}


\author{

    Diethelm Wuertz.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{optim}

