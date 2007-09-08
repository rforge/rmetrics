a <- 0.99; b <- 0.9
##
## Objective Function
##
fn <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  5.04*x1 + 0.035*x2 + 10*x3 +3.36*x5 - 0.063*x4*x7
}
attr(fn,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  c(5.04,0.035,10,-0.063*x7,3.36,0,-0.063*x4,0,0,0)
}


##
## Parameter Bounds
##
par.l <- c(rep(1e-5, 5), 85, 90, 3, 1.2, 145)
par.u <- c(2000, 16000, 120, 5000, 2000, 93, 95, 12, 4, 162)


##
## Constraints
##
linbd  <- matrix(0, nr=5, nc=2)
nlinbd <- matrix(0, nr=6, nc=2)

## linear equality 
linbd[1,] <- c(-35.82, Inf)  # g1
linbd[2,] <- c(133, Inf)     # g2
linbd[3,] <- c(35.82,Inf)    # g3
linbd[4,] <- c(-133, Inf)    # g4
linbd[5,] <- c(0,0)          # h1

## A is 5(linear constraints) x 10(params) matrix

A <- rbind(c( 0, 0, 0,    0, 0, 0,  0, 0,       -b,    -0.222), #g1
           c( 0, 0, 0,    0, 0, 0,  3, 0,        0,        -a), #g2
           c( 0, 0, 0,    0, 0, 0,  0, 0,      1/b,     0.222), #g3
           c( 0, 0, 0,    0, 0, 0, -3, 0,        0,       1/a), #g4
           c(-1, 0, 0, 1.22,-1, 0,  0, 0,        0,         0)) #h1



## nonlinear equality
h2 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  98000*x3/(x4*x9+1000*x3)-x6
}
attr(h2,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  s <- 1/(x4*x9+1000*x3)
  g3 <- (98000*s) * (1-1000*x3*s)
  g4 <- -98000*x3*x9*s^2
  g9 <- -98000*x3*x4*s^2
  c(0,0,g3,g4,0,-1,0,0,g9,0)
}
nlinbd[1,] <- c(0,0)

## nonlinear equality
h3 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  (x2+x5)/x1 - x8
}
attr(h3,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  g1 <- -(x2+x5)/x1^2
  c(g1,1/x1,0,0,1/x1,0,0,-1,0,0)
}
nlinbd[2,] <- c(0,0)

## nonlinear inequality
g5 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  1.12*x1 + 0.13167*x1*x8 - 0.00667*x1*x8^2 - a*x4
}
attr(g5,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]
  g1 <- 1.12+0.13167*x8-0.00667*x8^2
  g8 <- 0.13167*x1-2*0.00667*x1*x8
  c(g1,0,0,-a,0,0,0,g8,0,0)
}
nlinbd[3,] <- c(0,Inf)

## nonlinear inequality
g6 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  1.098*x8 - 0.038*x8^2 + 0.325*x6 - a*x7
}
attr(g6,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]
  
  g8 <- 1.098 - 2*0.038*x8
  c(0,0,0,0,0,0.325,-a,g8,0,0)
}
nlinbd[4,] <- c(-57.425,Inf)

## nonlinear inequality
g7 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  -g5(par) + (1/a-a)*x4
}
attr(g7,"gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]
  v <- -g5@gr(par)
  v[4] <- v[4] + (1/a-a)
  v
}
nlinbd[5,] <- c(0,Inf)

## nonlinear inequality
g8 <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]
  -g6(par) + (1/a-a)*x7
}
attr(g8, "gr") <- function(par){
  x1 <- par[1]; x2 <- par[2]; x3 <- par[3]; x4 <- par[4]; x5 <- par[5]
  x6 <- par[6]; x7 <- par[7]; x8 <- par[8]; x9 <- par[9]; x10 <- par[10]

  v <- -g6@gr(par)
  v[7] <- v[7] + (1/a-a)
  v
}
nlinbd[6,] <- c(0,Inf)

## initial values
p0 <- c(1745, 12e3, 11e1, 3048, 1974,
        89.2, 92.8, 8, 3.6, 145)
## control variables
cntl <- donlp2.control(del0=0.2, tau0=1.0, tau=0.1, epsdif=1e-16)

nlinbd <- rbind(nlinbd[c(3,5,4,6,1,2),])
## start constrained optimization
ret <- donlp2(par=p0, fn=fn,
              par.u=par.u, par.l=par.l,
              A=A,
              lin.u=linbd[,2], lin.l=linbd[,1],
              nlin=list(g5,g7,g6,g8,h2,h3),
              nlin.upper=nlinbd[,2], nlin.lower=nlinbd[,1],
              control=cntl, control.fun=cfn)
print(ret)
