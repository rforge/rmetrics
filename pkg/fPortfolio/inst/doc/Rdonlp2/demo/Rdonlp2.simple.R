p <- c(-10,10)
par.l <- c(0,0); par.u <- c(100,100)

lin.u <- 1; lin.l <- 1
A <- t(c(1,1))

fn <- function(x){
  x[1]^2+x[2]^2
}
ret <- donlp2(p, fn, par.lower=par.l, par.upper=par.u,
              A=A, lin.u=lin.u, lin.l=lin.l)
