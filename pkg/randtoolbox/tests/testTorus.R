library(randtoolbox)

frac <- function(x) x-floor(x)

torusR <- function(n, k)
	frac(1:n %o% sqrt(get.primes(k)))

torusR(10, 5) - torus(10, 5) 


#pure QMC (same sequence)
torus(5)
randtoolbox:::.getrandtoolboxEnv(".torus.seed")
torus(5)


#QMC with time machine start
torus(5, usetime=TRUE)
torus(5, usetime=TRUE)

#hybrid QMC with SFMT : test continuing the sequence (bug reported by Hiroyuki Kawakatsu)

setSeed(1234); 
print( torus(3, init=TRUE, mixed=TRUE, mexp=607) );
print( torus(3, init=FALSE, mixed=TRUE) );


setSeed(1234); 
print( torus(3, init=TRUE, mixed=TRUE, mexp=607) );
print( torus(3, init=FALSE, mixed=TRUE) )
