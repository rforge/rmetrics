## fmincon_test.c

p <- rep(0.1,5)

## half open
par.l = rep(1e-5,5)
lin.u = c(19, 360, 800, -1e-4, -1e-4)

A <- cbind(c( 1, -1, -1, -1, -1),
           c( 1,  1, -1, -1, -1),
           c( 1,  0,  1, -1, -1),
           c( 1,  0,  0,  1, -1),
           c( 1,  0,  0,  0,  1))

fn <- function(par){
  a=par[1]; b=par[2]; c=par[3]; d=par[4]; e=par[5]
  c1 = 8*((120-a)/120)^2
  c2 = 7.75*(1.64*a+360)*((a+360-b)/(a+360))^4
  c3 = 3*(2.69*a+2.98*b+800)*((a+b+800-c)/(a+b+800))^4
  c4 = 5.2*(4.41*a+8.88*b+2.94*c)
  c5 = ((a+b+c-d)/(a+b+c))^5
  c6 = 10.6*(7.23*a+26.42*d+8.64*c+d)
  c7 = ((a+b+c+d-e)/(a+b+c+d))^5

  ans = c1+c2+c3+c4*c5+c6*c7
}

accfn <- function(lst){
  TRUE
}

cntl <- donlp2.control(taubnd=1, del0=0.2, tau0=1e-5, tau=0.1, dif=3)

ret <- donlp2(p, fn, par.lower=par.l, control=cntl,
              control.fun=accfn,
              A = A, lin.u=lin.u)

print(ret)
