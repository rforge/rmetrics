### Gamma distribution
k <- 4
shape <- 2
oldAbout <- 0
newAbout <- 1
sampSize <- 1000000

### Calculate raw moments up to order k
m <- numeric(k)
for (i in 1:k){
   m[i] <- gamma(shape + i)/gamma(shape)
}
m

### Calculate moments up to order k about newAbout
momChangeAbout(k, m, old, new)
### Can also calculate moments about newAbout up to orders less than k
momChangeAbout(3, m, oldAbout, newAbout)
momChangeAbout(2, m, oldAbout, newAbout)

### Approximate kth moment about newAbout using sampling
x <- rgamma(sampSize, shape)
mean((x - newAbout)^k)
