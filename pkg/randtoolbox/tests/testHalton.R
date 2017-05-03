library(randtoolbox)

print(halton(1, 5))

options(digits=15)
n <- 30
cbind( 1/get.primes(n), as.vector(halton(1, n) ) )

#pure QMC (same sequence)
halton(5)
halton(5)


#QMC with time machine start
halton(5, usetime=TRUE)
halton(5, usetime=TRUE)

#hybrid QMC with SFMT : test continuing the sequence (bug reported by Hiroyuki Kawakatsu)
setSeed(1234); 
print( halton(3, init=TRUE, mixed=TRUE, mexp=607) );
print( halton(3, init=FALSE, mixed=TRUE) );


setSeed(1234); 
print( halton(3, init=TRUE, mixed=TRUE, mexp=607) );
print( halton(3, init=FALSE, mixed=TRUE) )

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