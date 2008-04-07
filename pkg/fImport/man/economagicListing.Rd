\name{economagicListing}


\alias{economagicListing}


\title{Categries of symbols from Economagic}


\description{
    
     Lists categries of symbols from www.economagic.com
   
}


\usage{ 
economagicListing(category = c(
    "fedbog", "frbg17", "frbz1", "frbg19", "frbfor", "frbsls", "fedstl",   
    "feddal", "fedny", "fedphl", "blslf", "blsint", "cbo", "treas",    
    "doeme", "doewkly", "libor", "aus", "japan", "ecb", "crb", "ism",      
    "sp"), abbreviate = 60)
}


\arguments{

    \item{category}{
        a character string with filename, usually
        having extension ".csv", where to save the downloaded data.
        }
    \item{abbreviate} {
        ...
        }
}


\examples{
\dontrun{
## economagicListing -
   economagicListing()
}
}


\keyword{data}

