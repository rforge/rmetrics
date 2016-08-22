library(randtoolbox)

print(halton(1, 5))

options(digits=15)
n <- 30
cbind( 1/get.primes(n), as.vector(halton(1, n) ) )

n <- 5
d <- 4
halton(n, d, method="C")
halton(n, d, method="Fortran")

if(FALSE)
{
n <- 10
d <- 10000
check <- all(halton(n, d, method="C") == halton(n, d, method="Fortran"))
check

n <- 1e3
d <- 1e3
system.time(halton(n, d, method="C"), gcFirst = TRUE)
system.time(halton(n, d, method="Fortran"), gcFirst = TRUE)
}