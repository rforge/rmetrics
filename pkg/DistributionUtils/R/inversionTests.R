inversionTestpq <- function(densFn = "norm", n = 10, ...)
{
  ## Purpose: Evaluate applying q then p to randomly chosen observations
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date:  9 Feb 2010, 13:45

  CALL <- match.call()
  dfun <- match.fun(paste("d", densFn, sep = ""))
  pfun <- match.fun(paste("p", densFn, sep = ""))
  qfun <- match.fun(paste("q", densFn, sep = ""))
  rfun <- match.fun(paste("r", densFn, sep = ""))

  x <- rfun(n, ...)
  qpx <- qfun(pfun(x, ...), ...)
  diffs <- qpx - x
  result <- list(qpx = qpx, x = x, diffs = diffs, n = n) 
  
  return(result)
}

inversionTestqp <- function(densFn = "norm",
                            qs = c(0.001,0.01,0.025,0.05,0.1,0.2,0.4,0.5,
                                   0.6,0.8,0.9,0.95,0.975,0.99,0.999), ...)
{
  ## Purpose: Evaluate applying p then q to a fixed set of quantiles
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date:  9 Feb 2010, 13:45

  CALL <- match.call()
  dfun <- match.fun(paste("d", densFn, sep = ""))
  pfun <- match.fun(paste("p", densFn, sep = ""))
  qfun <- match.fun(paste("q", densFn, sep = ""))
  rfun <- match.fun(paste("r", densFn, sep = ""))


  pqqs <- pfun(qfun(qs, ...), ...)
  diffs <- pqqs - qs
  result <- list(pqqs = pqqs, qs = qs, diffs = diffs) 
  
  return(result)
}

