### small shape
mu <- 0
delta <- 1
beta <- c(0,2)
nu <- c(1,5)
skewhypSmallShape <-
    expand.grid(mu = mu, delta = delta, beta = beta, nu = nu)

### large shape
mu <- 0
delta <- 1
beta <- c(-5,0,1,2,5)
nu <- c(1,2,5,10,20)
skewhypLargeShape <-
    expand.grid(mu = mu, delta = delta, beta = beta, nu = nu)

### small param
mu <- c(0,2)
delta <- c(1,5)
beta <- c(0,2)
nu <- c(1,5)
skewhypSmallParam <-
    expand.grid(mu = mu, delta = delta, beta = beta, nu = nu)

### large param
mu <- c(-2,0,1,2)
delta <- c(0.5,1,2,5)
beta <- c(-5,0,1,2,5)
nu <- c(1,2,5,10,20)
skewhypLargeParam <-
    expand.grid(mu = mu, delta = delta, beta = beta, nu = nu)


