################################################################################
##
##   R package Rsolnp by Alexios Ghalanos and Stefan Theussl Copyright (C) 2009
##   This file is part of the R package Rsolnp.
##
##   The R package Rsolnp is free software: you can redistribute it and/or 
##   modify it under the terms of the GNU General Public License as published 
##   by the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   The R package Rsolnp is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
################################################################################


benchmarkids <- 
function()
{
    return(c("Powell", "Wright4", "Wright9", "Alkylation", "Entropy", "Box", 
        "RosenSuzuki", "RachevRatio", "KappaRatio"))
}


# ------------------------------------------------------------------------------


benchmark <- 
function( id = "Powell")
{
  if( !any(benchmarkids() == id[ 1L ]) )
    stop( "invalid benchmark id" )
    ans = switch(id,
            Powell = .powell(),
            Wright4 = .wright4(),
            Wright9 = .wright9(),
            Alkylation = .alkylation(),
            Entropy = .entropy(),
            Box = .box(),
            RosenSuzuki = .rosensuzuki(),
            RachevRatio = .rachevratio(),
            KappaRatio = .kapparatio())
    return(ans)
}


# ------------------------------------------------------------------------------


.powell <- 
function()
{
    .fn1 <- function(x)
    {
        exp(x[1]*x[2]*x[3]*x[4]*x[5])
    }
    
    .eqn1 <- function(x){
        z1=x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
        z2=x[2]*x[3]-5*x[4]*x[5]
        z3=x[1]*x[1]*x[1]+x[2]*x[2]*x[2]
        return(c(z1,z2,z3))
    }

    .x0 = c(-2, 2, 2, -1, -1)
    
    ctrl = list(trace=0)
    
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = c(10, 0, -1), 
        control = ctrl)
    
    minos = list()
    minos$fn = 0.05394985
    minos$pars = c(-1.717144, 1.595710, 1.827245, 0.763643, 0.763643)
    minos$nfun = 524
    minos$iter = 12
    minos$elapsed = 0.2184
    
    bt = data.frame( 
        solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
        minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("Powell's exponential problem is a function of five variables with three nonlinear equality constraints on the variables.")
    
    return(bt)
}


# ------------------------------------------------------------------------------


.wright4 <- 
function()
{
    .fn1 <- function(x)
    {
        (x[1]-1)^2+(x[1]-x[2])^2+(x[2]-x[3])^3+(x[3]-x[4])^4+(x[4]-x[5])^4
    }
    
    .eqn1 <- function(x){
        z1=x[1]+x[2]*x[2]+x[3]*x[3]*x[3]
        z2=x[2]-x[3]*x[3]+x[4]
        z3=x[1]*x[5]
        return(c(z1,z2,z3))
    }
    
    .x0 = c(1, 1, 1, 1, 1)
    ctrl=list(trace=0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = c(2+3*sqrt(2),-2+2*sqrt(2),2), control=ctrl)
    minos = list()
    minos$fn = 0.02931083
    minos$pars = c(1.116635, 1.220442, 1.537785, 1.972769, 1.791096)
    minos$nfun = 560
    minos$iter = 9
    minos$elapsed = 0.249
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("Wright's fourth problem is a function of five variables with three non linear equality constraints on the variables. This popular test problem has several local solutions and taken from Wright (1976).")
    return(bt)
}


# ------------------------------------------------------------------------------


.wright9 <-
function()
{
    .fn1 <- function(x)
    {
        10*x[1]*x[4]-6*x[3]*x[2]*x[2]+x[2]*(x[1]*x[1]*x[1])+
                9*sin(x[5]-x[3])+x[5]^4*x[4]*x[4]*x[2]*x[2]*x[2]
    }
    
    .ineqn1 <- function(x){
        z1=x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
        z2=x[1]*x[1]*x[3]-x[4]*x[5]
        z3=x[2]*x[2]*x[4]+10*x[1]*x[5]
        return(c(z1,z2,z3))
    }
    ineqLB = c(-100, -2, 5)
    ineqUB = c(20, 100, 100)
    .x0 = c(1, 1, 1, 1, 1)
    ctrl=list(trace=0)
    ans = solnp(.x0, fun = .fn1, ineqfun = .ineqn1, ineqLB = ineqLB, ineqUB = ineqUB, control=ctrl)
    minos = list()
    minos$fn = -210.4078
    minos$pars = c(-0.08145219, 3.69237756, 2.48741102,  0.37713392, 0.17398257)
    minos$nfun = 794
    minos$iter = 11
    minos$elapsed = 0.281
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("Wright's ninth problem is a function of five variables with three non linear inequality constraints on the variables. This popular test problem has several local solutions and taken from Wright (1976).")
    return(bt)
}


# ------------------------------------------------------------------------------


.alkylation <-
function()
{
    .fn1 <- function(x)
    {
        -0.63*x[4]*x[7]+50.4*x[1]+3.5*x[2]+x[3]+33.6*x[5]
    }
    
    .eqn1 <- function(x){
        z1=98*x[3]-0.1*x[4]*x[6]*x[9]-x[3]*x[6]
        z2=1000*x[2]+100*x[5]-100*x[1]*x[8]
        z3=122*x[4]-100*x[1]-100*x[5]
        return(c(z1,z2,z3))
    }
    .ineqn1 <- function(x){
        z1=(1.12*x[1]+0.13167*x[1]*x[8]-0.00667*x[1]*x[8]*x[8])/x[4]
        z2=(1.098*x[8]-0.038*x[8]*x[8]+0.325*x[6]+57.25)/x[7]
        z3=(-0.222*x[10]+35.82)/x[9]
        z4=(3*x[7]-133)/x[10]
        return(c(z1,z2,z3,z4))
    }
    ineqLB = c(0.99,0.99,0.9,0.99)
    ineqUB = c(100/99,100/99,10/9,100/99)
    eqB = c(0,0,0)
    LB = c(0,0,0,10,0,85,10,3,1,145)
    UB = c(20,16,120,50,20,93,95,12,4,162)
    .x0 = c(17.45,12,110,30,19.74,89.2,92.8,8,3.6,155)
    ctrl = list(rho = 0, trace=0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = eqB, 
            ineqfun = .ineqn1, ineqLB = ineqLB, 
            ineqUB = ineqUB, LB = LB, UB = UB, 
            control = ctrl)
    minos = list()
    minos$fn = -172.642
    minos$pars = c(16.996427, 16.000000, 57.685751, 30.324940, 
        20.000000, 90.565147, 95.000000, 10.590461, 1.561636, 153.535354)
    minos$nfun = 2587
    minos$iter = 13
    minos$elapsed = 0.811
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("The Alkylation problem models a simplified alkylation process. It is a function of ten variables with four non linear inequality and three non linear equality constraints as well as variable bounds. The problem is taken from Locke and Westerberg (1980).")
    return(bt)
}


# ------------------------------------------------------------------------------


.entropy <- 
function()
{
    .fn1 <- function(x)
    {
        m = length(x)
        f = 0
        for(i in 1:m){
            f = f-log(x[i])
        }
        ans = f-log(.vnorm(x-1) + 0.1)
        ans
    }
    
    .eqn1 <- function(x){
        sum(x)
    }
    eqB = 10
    LB = rep(0,10)
    UB = rep(1000,10)
    set.seed(1953)
    .x0 = runif(10, 0, 1000)
    ctrl=list(trace=0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = eqB, LB = LB, UB = UB, 
        control=ctrl)
    minos = list()
    minos$fn = 0.1854782
    minos$pars = c(2.2801555, 0.8577605, 0.8577605, 0.8577605, 0.8577605, 0.8577605, 
            0.8577605, 0.8577605, 0.8577605, 0.8577605)
    minos$nfun = 886
    minos$iter = 4
    minos$elapsed = 0.296
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("The Entropy problem is non convex in n variables with one linear equality constraint and variable positivity bounds.")
    return(bt)
}


# ------------------------------------------------------------------------------


.box <-
function()
{
    .fn1 <- function(x)
    {
        -x[1]*x[2]*x[3]
    }
    
    .eqn1 <- function(x){
        4*x[1]*x[2]+2*x[2]*x[3]+2*x[3]*x[1]
    }

    eqB = 100
    LB = rep(1, 3)
    UB = rep(10, 3)
    
    .x0 = c(1.1, 1.1, 9)
    ctrl=list(trace=0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = eqB, LB = LB, UB = UB, 
        control=ctrl)
    minos = list()
    minos$fn = -48.11252
    minos$pars = c(2.886751, 2.886751, 5.773503)
    minos$nfun = 394
    minos$iter = 9
    minos$elapsed = 0.156
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("The box problem is a function of three variables with one non linear equality constraint and variable bounds.")
    return(bt)
}


# ------------------------------------------------------------------------------


.rosensuzuki <- 
function()
{
    .fn1 <- function(x)
    {
        x[1]*x[1]+x[2]*x[2]+2*x[3]*x[3]+x[4]*x[4]-5*x[1]-5*x[2]-21*x[3]+7*x[4]
    }

    .ineqn1 <- function(x){
        z1=8-x[1]*x[1]-x[2]*x[2]-x[3]*x[3]-x[4]*x[4]-x[1]+x[2]-x[3]+x[4]
        z2=10-x[1]*x[1]-2*x[2]*x[2]-x[3]*x[3]-2*x[4]*x[4]+x[1]+x[4]
        z3=5-2*x[1]*x[1]-x[2]*x[2]-x[3]*x[3]-2*x[1]+x[2]+x[4]
        return(c(z1,z2,z3))
    }
    ineqLB = rep(0, 3)
    ineqUB = rep(1000, 3)
    .x0 = c(1, 1, 1, 1)
    ctrl=list(trace=0)
    ans = solnp(.x0, fun = .fn1, ineqfun = .ineqn1, ineqLB = ineqLB, ineqUB = ineqUB, 
        control=ctrl)
    minos = list()
    minos$fn = -44
    minos$pars = c(2.502771e-07, 9.999997e-01, 2.000000e+00, -1.000000e+00)
    minos$nfun = 527
    minos$iter = 12
    minos$elapsed = 0.203
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            minos =  rbind(round(minos$fn, 5L),
                    round(minos$iter, 0L),
                    round(0, 0L),
                    round(minos$nfun, 0L),
                    round(minos$elapsed, 3L),
                    matrix(round(minos$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    attr(bt, "description") = paste("The Rosen-Suzuki problem is a function of four variables with three nonlinear inequality constraints on the variables. It is taken  from Problem 43 of Hock and Schittkowski (1981).")
    return(bt)
}


#-------------------------------------------------------------------------------
# portfolio optimization problems / benchmarked against SNOPT (SOL) - with tomlab 
# interface for matlab


.rachevratio <-
function()
{
    data(dji30ret)
    dj30=as.matrix(dji30ret)
    
    .VaR <- function(x, alpha = 0.05)
    { 
        x = as.matrix(x)
        VaR = quantile(x, probs = alpha, type = 1)
        VaR
    }
    
    .CVaR <- function(x, alpha = 0.05)  
    {   
        x = as.matrix(x)
        VaR = .VaR(x, alpha)
        X = as.vector(x[, 1])
        CVaR = VaR - 0.5 * mean(((VaR-X) + abs(VaR-X))) / alpha
        CVaR
    }
    .fn1 <- function(x,ret)
    {
        port=ret%*%x
        obj=-.CVaR(-port)/.CVaR(port)
        return(obj)
    }
    
    # abs(sum) of weights ==1
    .eqn1  <- function(x,ret)
    {
        sum(abs(x))
    }
    LB=rep(0,30)
    UB=rep(0.1,30)
    pars=rep(1/30,30)
    
    .x0 = rep(1/30,30)
    ctrl = list(delta = 1e-10, tol = 1e-8, trace = 0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = 1, LB = LB, UB = UB, 
        control = ctrl, ret = dj30)
    snopt = list()
    snopt$fn = -1.002162
    snopt$pars = c(0, 0.08128943, 0, 0, 0, 0.01997529, 0, 0, 0.04208868, 
        0, 0, 0.1, 0.04206685, 0, 0.1, 0.1, 0.1, 0, 0.1, 0, 0, 0, 0.09698527, 
        0, 0, 0.1, 0, 0.01759449, 0.1, 0)
    snopt$nfun = 3867
    snopt$iter = 8
    snopt$elapsed = 7.534
        
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            snopt =  rbind(round(snopt$fn, 5L),
                    round(snopt$iter, 0L),
                    round(0, 0L),
                    round(snopt$nfun, 0L),
                    round(snopt$elapsed, 3L),
                    matrix(round(snopt$pars, 5L), ncol = 1L)) )
    rownames(bt) = c("funcValue", "majorIter", "exitFlag", "nfunEval", 
        "time(sec)", paste("par.", 1L:length(ans$pars), sep = "") )
    colnames(bt) = c("solnp", "snopt") 
    attr(bt, "description") = paste("The Rachev Ratio problem minimizes a portfolio's Rachev ratio. It has one linear equality constraint and variable bounds. See Rachev (2000) for details.")
    return(bt)
}


# Kappa Optimization (Kaplan and Knowles...subsumes omega and sortino measures among others)
# It is in fact the excess to benchmark return divided by the standardized Lower Partial Moment 
# measure and as such Kaplan and Knowles are not entirely entitled to claim a special measure 
# for this as it was described among others by Fishburn in the 70's.
#----------------------------------------------------------------------------------
# setup the required sample functions:
# r is the threshold, n is the power (n=1 + 1 is the omega measure of Shadwick and Keating,
# while n=2 is the sortino measure.)


.kapparatio <- 
function()
{
    data(dji30ret)
    dj30=as.matrix(dji30ret)
    
    .kappa <- function(port, r, n)
    {
        z = mean((port< r) * (r-port)^n)
        sg = sign(z)
        (mean(port) - r) / (sg*abs(z)^(1/n))
    }
    
    
    .fn1 <- function(x, ret, r, n)
    {
        port = ret%*%x
        obj = -.kappa(port,r,n)
        return(obj)
    }
    
    # abs(sum) of weights ==1
    .eqn1  <- function(x, ret, r, n)
    {
        sum(abs(x))
    }
    
    LB = rep(0,30)
    UB = rep(0.1,30)    
    .x0 = rep(1/30,30)
    ctrl = list(delta = 1e-10, tol = 1e-8, trace = 0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = 1, LB = LB, UB = UB, 
        control=ctrl, ret=dj30, r = 0, n = 2)
    snopt = list()
    snopt$fn = -0.06160942
    snopt$pars = c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0,
        0.08561978, 0.1, 0, 0, 0.1, 0, 0.1, 0.01628157, 0.09809865, 0, 0.1, 0.1)
    snopt$nfun = 1234
    snopt$iter = 3
    snopt$elapsed = 0.983
    
    bt = data.frame( solnp = rbind(round(ans$values[length(ans$values)], 5L),
                    round(ans$outer.iter, 0L),
                    round(ans$convergence, 0L),
                    round(ans$nfuneval, 0L),
                    round(ans$elapsed, 3L),
                    matrix(round(ans$pars, 5L), ncol = 1L)),
            snopt =  rbind(round(snopt$fn, 5L),
                    round(snopt$iter, 0L),
                    round(0, 0L),
                    round(snopt$nfun, 0L),
                    round(snopt$elapsed, 3L),
                    matrix(round(snopt$pars, 5L), ncol = 1L)) )
    rownames(bt) <- c("funcValue", "majorIter", "exitFlag", "nfunEval", "time(sec)",
            paste("par.", 1L:length(ans$pars), sep = "") )
    colnames(bt) = c("solnp", "snopt") 
    attr(bt, "description") = paste("The PortKappa problem minimizes a portfolio's Kappa ratio. It has one linear equality constraint and variable bounds. See Kaplan and Knowles (2004) for details.")
    return(bt)
}


# ------------------------------------------------------------------------------

