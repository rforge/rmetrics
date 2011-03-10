require("stabledist")

stopifnot(0 == dstable(4000., alpha=1.00001, beta=0.6))
## gave error in fBasics::dstable()
x <- 2^seq(0, 20, length= 200)
fx <- dstable(x, alpha = 1.0001, beta = 0.6)

plot(x,fx, log="x", type="l")# looks good
plot(x,fx, log="xy", type="l")# hmm... see noise -- FIXME eventually
i <- x < 260
plot(x[i],fx[i], log="xy", type="o")# hmm... see the jump at around 75
i <- 80 < x & x < 320
plot(x[i],fx[i], log="xy", type="o")# see a 2nd problem at  x =~ 270

