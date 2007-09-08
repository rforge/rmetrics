## simple2.c
p <- c(10,10)
par.l <- c(-100,-100); par.u <- c(100,100)
nlin.l <- nlin.u <- 1
fn <- function(x){
  x[1]^2+x[2]^2
}
dfn <- function(x){
  c(2*x[1], 2*x[2])
}
attr(fn, "gr") <- dfn
nlcon <- function(x){
  x[1]*x[2]
}
dnlcon <- function(x){
  c(x[2], x[1])
}
attr(nlcon, "gr") <- dnlcon

ctrl=donlp2.control(del0=1,tau0=10,tau=0.1)
ret <- donlp2(p, fn, par.u=par.u, par.l=par.l,control=ctrl,
              nlin=list(nlcon), nlin.u=nlin.u, nlin.l=nlin.l)
print(ret)
