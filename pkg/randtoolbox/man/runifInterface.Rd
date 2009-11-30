\name{runifinterface}
\alias{runifinterface}
\alias{set.generator}
\alias{put.state}
\alias{get.state}
\title{TODO}
\description{
TODO
}
\usage{
set.generator <- function(name=c("congruRand", "WELL", "default"), 
parameters=NULL, seed=NULL, ..., only.dsc=FALSE)
put.description(description)		
get.description()
}
\arguments{
	\item{name}{a character string for the RNG name.}
	\item{parameters}{a numeric or character vector/matrix for the RNG.}
	\item{seed}{a numeric or character vector}	
	\item{...}{arguments to pass to the RNG if \code{parameters} is NULL.}	
	\item{only.dsc}{a logical to (just) show the description of a RNG.}	
	\item{description}{}	
}
\details{
TODO
}
\value{
TODO
}
\seealso{
TODO
}
\author{
 Petr Savicky
}
\examples{

# (1) the Park Miller sequence
#

#should be [1] "Mersenne-Twister" "Inversion"   
RNGkind()

#parameters for Park Miller congruential generator
paramParkMiller <- c(mod=2^31-1, mult=16807, incr=0)
set.generator(name="congruRand", parameters=paramParkMiller, seed=1)

#should be [1] "user-supplied" "Inversion"
RNGkind() 

#description of the RNG supplied by the user, i.e. Park Miller
get.description()

#generate 10 random points from the Park-Miller sequence
x1 <- runif(10)
print(x1)

#the seed has changed
get.description()

# (2) the Knuth Lewis RNG
#

paramKnuthLewis <- c(mod="4294967296", mult="1664525", incr="1013904223")
set.generator(name="congruRand", parameters= paramKnuthLewis, seed=1)

#description of the RNG supplied by the user, i.e. Knuth Lewis
print(KLwithseed1 <- get.description())

x2 <- runif(10)

#the seed has changed
get.description()

#reinitiate the RNG setting
put.description(KLwithseed1)

#the same as x2
x3 <- runif(10)

# (3) WELL RNG
#

print(version.randtoolbox())

#set WELL RNGs
set.generator("WELL", seed=12345, order=1024, version="a")
get.description()

x0 <- runif(10)

print(x0)


# (4) R default RNG
#

#get back to original R setting
set.generator("default")
RNGkind()

}
\keyword{distribution}

