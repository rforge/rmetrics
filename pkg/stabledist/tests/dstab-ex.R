require("stabledist")
dPareto <- stabledist:::dPareto

source(system.file("test-tools.R", package = "Matrix"))
                                        #-> identical3(), showProc.time(),...

stopifnot(0 <= print(dstable(4000., alpha=1.00001, beta=0.6)))
## gave error in fBasics::dstable()
## currently 3  NA/Inf warnigns

x <- 2^seq(0, 20, length= 200)
fx <- dstable(x, alpha = 1.0001, beta = 0.6)

plot(x,fx, log="x", type="l")# looks good
plot(x,fx, log="xy", type="l")# --- perfect now
stopifnot((dlx <- diff(log(fx))) < 0,
          abs(dlx[-(1:99)] - -0.13938) < 4e-4)

zeta <- function(alpha,beta) if(alpha==1) 0 else -beta*tan(pi/2*alpha)

## negative beta:
cx <- curve(dstable(x, 0.75, -.5), -.5, 1.5, n=501)# ok, now
m <- stableMode(0.75, -.5, tol=1e-14)
stopifnot(all.equal(m, 0.35810298366, tol = 1e-7))

showProc.time()

###-------- "small" alpha -----------------
## alpha --> 0 --- very heavy tailed -- and numerically challenging.

## symmetric (beta = 0)
(x0 <- (-16:16)/256)
fx0 <- stabledist::dstable(x0, alpha = 0.1, beta=0, gamma = 1e6)
fx0 <-             dstable(x0, alpha = 0.1, beta=0, gamma = 1e6)
plot(x0, fx0, type = "o",
     main = expression(f(x, alpha== 0.1, beta == 0, gamma == 10^6)))
stopifnot(all.equal(fx0[17],1.15508291498374),
          all.equal(fx0[ 1],0.02910420736536),
          all.equal(range(diff(fx0[1:8])),
                    c(0.0011871409, 0.0025179435), tol=1e-6)
          )

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
stopifnot(all.equal(m1, -0.0791922, tol=1e-6)) # -0.07919221927, # was -0.07919217576
title(main = expression(f(x, alpha== 0.1, beta == 0.5)))
## check mode *and* unimodality
i. <- r1$x > m1
stopifnot(## decreasing to the right:
	  diff(r1$y[ i.]) < 0,
	  ## increasing on the left:
	  diff(r1$y[!i.]) > 0)

showProc.time()

## alpha ~= 1  ---- and x ~ zeta(a,b):
f1 <- dstable(6366.197,  alpha= 1.00001, beta= .1)
f2 <- dstable(-50929.58, alpha= 1.00001, beta= -.8)

## these all work (luck):
curve(dstable(-50929+x, alpha= 1.00001, beta= -.8), 0,1, n=200)
## and now look good -- no longer __ (FIXME)
showProc.time()

### ---- alpha == 1 ---------

curve(dstable(x, alpha = 1, beta = 0.3), -20, 20,
      log="y", n= 256)
## works, but discontinuous --- FIXME

## large x gave problems at times:
dstable(-1e20, alpha = 0.9,  beta = 0.8)

chkUnimodal <- function(x) {
    ## x = c(x1, x2)  and  x1 is *increasing*  and x2 is *decreasing*
    stopifnot((n <- length(x)) %% 2 == 0,
              (n2 <- n %/% 2) >= 2)
    if(is.unsorted(x[seq_len(n2)])) stop("first part is *not* increasing")
    if(is.unsorted(x[n:(n2+1)]))    stop("seconds part is *not* decreasing")
    invisible(x)
}

showProc.time()

xLrg <- c(10^c(10:100,120, 150, 200, 300), Inf)
xLrg <- sort(c(-xLrg, xLrg))
d <- dstable(xLrg, alpha = 1.8,   beta = 0.3 ); chkUnimodal(d)
d <- dstable(xLrg, alpha = 1.01,  beta = 0.3 ); chkUnimodal(d) # (slow!)
## look at the problem (this is *S.L.O.W.* now [2010-03-28] !)
r <- curve(dstable(-x, alpha = 1.01, beta = 0.3, log=TRUE), 1e10, 1e20,
           log="x", n=512)
curve(dPareto(-x, alpha = 1.01, beta = 0.3, log=TRUE), add=TRUE,
      col=2, lwd=2, lty=2)

showProc.time()


d <- dstable(xLrg, alpha = 1.001, beta = -0.9) # >= 50 warnings
try( chkUnimodal(d) ) # FIXME
## look at the problem:
curve(dstable(-x, alpha = 1.001, beta = -0.9, log=TRUE), 1e10, 1e20,
      log="x", n=512)
curve(dPareto(-x, alpha = 1.001, beta = -0.9, log=TRUE), add=TRUE,
      col=2, lwd=2, lty=2)

d <- dstable(xLrg, alpha = 1. ,   beta = 0.3 ); chkUnimodal(d) # "ok" now
d <- dstable(xLrg, alpha = 0.9,   beta = 0.3 ) # 11 warnings
try( chkUnimodal(d) ) # FIXME
d <- dstable(xLrg, alpha = 0.5,   beta = 0.3 ) # 22 warnings
chkUnimodal(d)
d <- dstable(xLrg, alpha = 0.1,   beta = 0.3 ) # 26 warnings -- *NOT* decreasing
chkUnimodal(d)

showProc.time()

##-------------  beta = 1  ---------------------
options(dstable.debug = TRUE)
dstable(1, alpha=1.2,   beta= 1 - 1e-7)#ok
dstable(1, alpha=1.2,   beta= 1)# gave error, because   g(pi/2) < 0
dstable(0, alpha=13/16, beta= 1 -2^-52)# had error as   g(-theta0)  |->  NaN
dstable(0, alpha=19/16, beta= 1)       # had error as   g(pi/2)     |->  NaN

options(dstable.debug = FALSE)

if(Sys.getenv("USER") == "maechler" &&
   !nzchar(Sys.getenv("R_quick_check"))) {
 ## This needs 65 seconds (nb-mm3: 54*32*11 dstable() values)

 ep <- 2^-(1:54)## beta := 1 - ep ---> 1  {NB: 1 - 2^-54 == 1  numerically}
 alph.s <- (1:32)/16   # in (0, 2]
 f.b1 <- sapply(alph.s, function(alf)
                sapply(1 - ep, function(bet)
                       dstable(0:10, alpha = alf, beta = bet)),
                simplify = if(getRversion() >= "2.13") "array" else TRUE)
 print(summary(f.b1))
 r.b1 <- range(f.b1)
 stopifnot(0 < r.b1[1], r.b1[2] < 0.35)
 ## "FIXME" test more: monotonicity in x {mode is < 0 = min{x_i}}, beta, alpha, ...
 showProc.time()

} else message("expensive dstable() checks  have been skipped")

cat('Time elapsed: ', proc.time(),'\n') # "stats"
