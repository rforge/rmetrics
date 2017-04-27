library(randtoolbox)

p13 <- int2bit(13)
prevterm <- sapply(c(1,3,3), int2bit)

#page 307 of Glassermann
trueval <- c(16, 24,  8, 12, 28, 20,  4, 30, 14,  6, 22, 18,  2, 10, 26,  5, 21, 29, 13,  9, 25, 17,  1, 27, 11,  3, 19, 23,  7, 15, 31)/32
cbind(sobol.basic(31, p13, 3, prevterm, echo=FALSE, output="r") , trueval)


sobol.directions(p13, 3, sapply(c(1,1,1), int2bit), 0, echo=FALSE)
sobol.directions(p13, 3, sapply(c(1,3,5), int2bit), 0, echo=FALSE)
sobol.directions(p13, 3, sapply(c(1,1,7), int2bit), 0, echo=FALSE)

sobol(1, 3)
