### Functions for the normal inverse Gaussian distribution

### Density of the normal inverse Gaussian distribution
dnig <- function(x, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  dghyp(x, param = c(param, -1/2))
} ## End of dnig()


### Cumulative distribution function of the normal inverse Gaussian distribution

pnig <- function(q, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta),
                    small = 10^(-6), tiny = 10^(-10),
                    deriv = 0.3, subdivisions = 100,
                    accuracy = FALSE, ...) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  pghyp(q, param = c(param, -1/2), small = small, tiny = tiny, deriv = deriv,
        subdivisions = subdivisions, accuracy = accuracy, ...)
} ## End of pnig()

### Quantiles function of the normal inverse Gaussian distribution
qnig <- function(p, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta),
                    small = 10^(-6), tiny = 10^(-10),
                    deriv = 0.3, nInterpol = 100, subdivisions = 100, ...) {

  if (length(param) != 4)
   stop("param vector must contain 4 values")

  param <- as.numeric(param)

  qghyp(p, param = c(param, -1/2), small = small, tiny = tiny, deriv = deriv,
        nInterpol = nInterpol, subdivisions = subdivisions, ...)
} # End of qnig()

### Function to generate random observations from a
### normal inverse Gaussian distribution using the
### mixing property of the generalized inverse
### Gaussian distribution and Dagpunar's algorithm
### for the generalized inverse Gaussian
rnig <- function(n, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  rghyp(n, param = c(param, -1/2))
} ## End of rnig()

### Derivative of the density
ddnig <- function(x, mu = 0, delta = 1, alpha = 1, beta = 0,
                     param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  ddghyp(x, param = c(param, -1/2))
} ## End of ddnig()

### Function to set up breaks for pnig and qnig
nigBreaks <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                         param = c(mu, delta, alpha, beta),
                         small = 10^(-6), tiny = 10^(-10),
                         deriv = 0.3, ...) {

  if (length(param) != 4)
   stop("param vector must contain 4 values")

  param <- as.numeric(param)

  ghypBreaks(param = c(param, -1/2), small = small, tiny = tiny, 
            deriv = deriv, ...)
} ## End of nigBreaks()
