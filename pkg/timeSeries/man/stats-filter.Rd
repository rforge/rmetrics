
\name{filter}


\title{Linear Filtering on a Time Series}


\alias{filter,timeSeries-method}


\description{

     Applies linear filtering to a univariate 'timeSeries'.
      
}


\value{

  A 'timeSeries' object without missing values.
  
}


\examples{
## Creata a Dummy Signal 'timeSeries' - 
   data <- matrix(rnorm(100), ncol = 2)
   s <- timeSeries(data, units=c("A", "B"))
   head(s)
   
## Filter the series - 
   f <- filter(s, rep(1, 3))
   head(f)
   
## Plot and Compare the first series - 
   plot(cbind(s[, 1], f[, 1]), plot.type="s")
}
