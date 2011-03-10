require("stabledist")

stopifnot(0 <= dstable(4000., alpha=1.00001, beta=0.6))
## gave error in fBasics::dstable()

x <- 2^seq(0, 20, length= 200)
fx <- dstable(x, alpha = 1.0001, beta = 0.6)

plot(x,fx, log="x", type="l")# looks good
plot(x,fx, log="xy", type="l")# --- perfect now
stopifnot((dlx <- diff(log(fx))) < 0,
          abs(dlx[-(1:99)] - -0.13938) < 4e-4)

zeta <- function(alpha,beta) if(alpha==1) 0 else -beta*tan(pi/2*alpha)

## negative beta:
curve(dstable(x, 0.75, -.5), -.5, 1.5, n=501)# ok, now
m <- stableMode(0.75, -.5, tol=1e-14)
stopifnot(all.equal(m, 0.35810298366))

###-------- "small" alpha -----------------
## alpha --> 0 --- very heavy tailed -- and numerically challenging.

## symmetric (beta = 0)
(x0 <- (-16:16)/256)
fx0 <- stabledist::dstable(x0, alpha = 0.1, beta=0, gamma = 1e6)
plot(x0, fx0, type = "o",
     main = expression(f(x, alpha== 0.1, beta == 0, gamma == 10^6)))

## beta > 0
r3 <- curve(stabledist::dstable(x, alpha = 0.3, beta = 0.5, tol=1e-7),
	    -1, 1)
m3 <- stableMode(0.3, 0.5, tol=1e-14)# still with 3 warnings
stopifnot(all.equal(m3, -0.2505743952946, tol = 1e-10))
r3. <- curve(stabledist::dstable(x, alpha = 0.3, beta = 0.5, tol=1e-7),
	    -.27, -.22)

r1 <- curve(stabledist::dstable(x, alpha = 0.1, beta = 0.5, tol=1e-7),
	    -.4, .2, n = 512, ylim = c(0, 10))
m1 <- stableMode(0.1, 0.5, tol=1e-15)# still with 10 warnings
abline(v=m1, h=0, col="gray40", lty=2)
stopifnot(all.equal(m1, -0.079192175764))
title(main = expression(f(x, alpha== 0.1, beta == 0.5)))
## check mode *and* unimodality
i. <- r1$x > m1
stopifnot(## decreasing to the right:
	  diff(r1$y[ i.]) < 0,
	  ## increasing on the left:
	  diff(r1$y[!i.]) > 0)


cat('Time elapsed: ', proc.time(),'\n') # "stats"
