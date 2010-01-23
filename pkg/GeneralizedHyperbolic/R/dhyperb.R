### Functions for the hyperbolic distribution
### Density of the hyperbolic distribution
dhyperb <- function(x, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  dghyp(x, param = c(param, 1))
} ## End of dhyperb()


### Cumulative distribution function of the hyperbolic distribution
### New version intended to give guaranteed accuracy
### Uses exponential approximation in tails
### Integrate() is used over four parts in the middle of the distribution
### This version calls hyperbBreaks to determine the breaks
###
### DJS 05/09/06
phyperb <- function(q, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta),
                    small = 10^(-6), tiny = 10^(-10),
                    deriv = 0.3, subdivisions = 100,
                    accuracy = FALSE, ...) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  pghyp(q, param = c(param, 1), small = small, tiny = tiny, deriv = deriv,
        subdivisions = subdivisions, accuracy = accuracy, ...)
} ## End of phyperb()

### qhyperb using breaks as for phyperb and splines as in original qhyperb
###
### DJS 06/09/06
qhyperb <- function(p, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta),
                    small = 10^(-6), tiny = 10^(-10),
                    deriv = 0.3, nInterpol = 100, subdivisions = 100, ...) {

  if (length(param) != 4)
   stop("param vector must contain 4 values")

  param <- as.numeric(param)

  qghyp(p, param = c(param, 1), small = small, tiny = tiny, deriv = deriv,
        nInterpol = nInterpol, subdivisions = subdivisions, ...)
} # End of qhyperb()

### Function to generate random observations from a
### hyperbolic distribution using the
### mixing property of the generalized inverse
### Gaussian distribution and Dagpunar's algorithm
### for the generalized inverse Gaussian
rhyperb <- function(n, mu = 0, delta = 1, alpha = 1, beta = 0,
                    param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  rghyp(n, param = param)
} ## End of rhyperb()

### Derivative of the density
ddhyperb <- function(x, mu = 0, delta = 1, alpha = 1, beta = 0,
                     param = c(mu, delta, alpha, beta)) {

  if (length(param) != 4)
    stop("param vector must contain 4 values")

  param <- as.numeric(param)

  ddghyp(x, param = c(param, 1))
} ## End of ddhyperb()

### Function to set up breaks for phyperb and qhyperb
hyperbBreaks <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                         param = c(mu, delta, alpha, beta),
                         small = 10^(-6), tiny = 10^(-10),
                         deriv = 0.3, ...) {

  if (length(param) != 4)
   stop("param vector must contain 4 values")

  param <- as.numeric(param)

  ghypBreaks(param = c(param, 1), small = small, tiny = tiny, deriv = deriv, ...)
} ## End of hyperbBreaks()
