

# Package: Rsolnp
# Type: Package
# Title: Non-linear Programming with non-linear Constraints
# Version: 0.3
# Date: 2009-09-15
# Author: Alexios Ghalanos and Stefan Theussl
# Maintainer: Alexios Ghalanos <alexios@4dscape.com>
# Depends: stats
# Description: Non-linear Optimization Using Augmented Lagrange Multiplier 
#    Method Version for Rmetrics Portfolio Optimization
# LazyLoad: yes
# License: GPL


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


# Based on the original solnp by Yinyu Ye
# http://www.stanford.edu/~yyye/Col.html


#-------------------------------------------------------------------------------
# The Function SOLNP solves nonlinear programs in standard form:
#
#        minimize              J(P)
#        subject to            EC(P)  =0
#                   IB(:,1)<=  IC(P)  <=IB(:,2)
#                   PB(:,1)<=    P    <=PB(:,2).
# where
#
#  J       : Cost objective scalar function
#  EC      : Equality constraint vector function
#  IC      : Inequality constraint vector function
#  P       : Decision parameter vector
#  IB, PB  : lower and upper bounds for IC and P.
#-------------------------------------------------------------------------------


# control list
#           RHO  : penalty parameter
#           MAJIT: maximum number of major iterations
#           MINIT: maximum number of minor iterations
#           DELTA: relative step size in forward difference evaluation
#           TOL  : tolerance on feasibility and optimality
# defaults RHO=1, MAJIT=10, MINIT=10, DELTA=1.0e-5, TOL=1.0e-4


solnp <-
function(pars, fun, grad = NULL, eqfun = NULL, eqB = NULL, 
    eqgrad = NULL, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, 
    ineqgrad = NULL, LB = NULL, UB = NULL, 
    trace = FALSE, control = list(), ...)
{
    TRACE = trace
    
    # start timer
    tic = Sys.time()
    # get environment
    .solnpenv <- environment()
    .solnp_nfn <<- 0
    # [1] length of pars
    # [2] has function gradient?
    # [3] has hessian?
    # [4] has ineq?
    # [5] ineq length
    # [6] has jacobian (inequality)
    # [7] has eq?
    # [8] eq length
    # [9] has jacobian (equality)
    # [10] has upper / lower bounds
    # [11] has either lower/upper bounds or ineq
    ind = rep(0, 11)
    np = ind[1]  = length(pars)
    # lower parameter bounds - indicator
    # lpb[1]=1 means lower/upper bounds present
    # lpb[2]=1 means lower/upper bounds OR inequality bounds present
    
    # do parameter and LB/UB checks
    check1 = .checkpars(pars, LB, UB, .solnpenv)
    # .solnp_LB and .solnp_UB assigned
    if( !is.null(.LB) || !is.null(.UB) ) ind[10] = 1
    
    # do function checks and return starting value
    funv = .checkfun(pars, fun, .solnpenv, ...)
    #.solnp_fun assigned
    
    # gradient and hessian checks
    if(!is.null(grad)){
        gradv = .checkgrad(pars, grad, .solnpenv, ...)
        ind[2] = 1
    } else{
        .solnp_gradfun = function(pars, ...) .fdgrad(pars, fun = .solnp_fun, ...)
        ind[2] = 0
        gradv = .solnp_gradfun(pars, ...)
    }
    # .solnp_gradfun(pars, ...) assigned

    .solnp_hessfun = NULL
    ind[3] = 0
    #hessv = NULL
    # .solnp_hessfun(pars, ...) assigned

    # do inequality checks and return starting values
    
    if(!is.null(ineqfun)){
        ineqv =.checkineq(pars, ineqfun, ineqLB, ineqUB, .solnpenv, ...)
        ind[4] = 1
        nineq = length(ineqLB)
        ind[5] = nineq
        ineqx0 = (.ineqLB + .ineqUB)/2
        if(!is.null(ineqgrad)){
            ineqjacv = .cheqjacineq(pars, gradineq, .ineqUB, .ineqLB, .solnpenv, ...)
            ind[6] = 1
        } else{
            .solnp_ineqjac = function(pars, ...) .fdjac(pars, fun = .solnp_ineqfun, ...)
            ind[6] = 0
            ineqjacv = .solnp_ineqjac(pars, ...)
        }
    } else{
        .solnp_ineqfun = function(pars, ...) .emptyfun(pars, ...)
        .solnp_ineqjac = function(pars, ...) .emptyjac(pars, ...)
        ineqv = NULL
        ind[4] = 0
        nineq = 0
        ind[5] = 0
        ind[6] = 0
        ineqx0 = NULL
        .ineqLB = NULL
        .ineqUB = NULL
    }
    # .solnp_ineqfun and .solnp_ineqjac assigned
    # .solnp_ineqLB and .solnp_ineqUB assigned

    # equality checks
    if(!is.null(eqfun)){
        eqv = .checkeq(pars, eqfun, eqB, .solnpenv, ...)
        ind[7] = 1
        neq = length(.eqB)
        ind[8] = neq
        if(!is.null(eqgrad)){
            eqjacv = .cheqjaceq(pars, gradeq, .solnpenv, ...)
            ind[9] = 1
        } else{
            .solnp_eqjac = function(pars, ...) .fdjac(pars, fun = .solnp_eqfun, ...)
            eqjacv = .solnp_eqjac(pars, ...)
            ind[9] = 0
        }
    } else {
        eqv = NULL
        eqjacv = NULL
        .solnp_eqfun = function(pars, ...) .emptyfun(pars, ...)
        .solnp_eqjac = function(pars, ...) .emptyjac(pars, ...)
        ind[7] = 0
        neq = 0
        ind[8] = 0
        ind[9] = 0
    }
    # .solnp_eqfun(pars, ...) and .solnp_eqjac(pars, ...) assigned
    # .solnp_eqB assigned

    if( ind[ 10 ] || ind [ 4 ]) ind[ 11 ] = 1
        
    # parameter bounds (pb)
    pb  = rbind( cbind(.ineqLB, .ineqUB), cbind(.LB, .UB) )
    
    # check control list
    ctrl  = .solnpctrl( control )
    rho   = ctrl[[ 1 ]]
    # maxit = outer iterations
    maxit = ctrl[[ 2 ]]
    # minit = inner iterations
    minit = ctrl[[ 3 ]]
    delta = ctrl[[ 4 ]]
    tol   = ctrl[[ 5 ]]
    trace = TRACE # DW ctrl[[ 6 ]]
    
    # total constraints (tc) = no.inequality constraints + no.equality constraints
    tc = nineq + neq
    
    # initialize fn value and inequalities and set to NULL those not needed
    j  = jh = funv
    tt = 0 * .ones(3, 1)
    
    if( tc > 0 ) {
        # lagrange multipliers (lambda)
        lambda = 0 * .ones(tc, 1)
        # constraint vector = [1:neq 1:nineq]
        constraint = c(eqv, ineqv)
        if( ind[4] ) {
            tmpv = cbind(constraint[ (neq + 1):tc ] - .ineqLB, .ineqUB - constraint[ (neq + 1):tc ] )
            testmin = apply( tmpv, 1, FUN = function( x ) min(x[ 1 ], x[ 2 ]) )
            if( all(testmin > 0) ) ineqx0 = constraint[ (neq + 1):tc ]
            constraint[ (neq + 1):tc ] = constraint[ (neq + 1):tc ] - ineqx0
        }
        tt[ 2 ] = .vnorm(constraint)
        if( max(tt[ 2 ] - 10 * tol, nineq) <= 0 ) rho = 0
    } else{
        lambda = 0
    }
    # starting augmented parameter vector
    p  = c(ineqx0, pars)
    hessv  = diag(np + nineq)
    mu = np
    .solnp_iter = 0
    ob = c(funv, eqv, ineqv)
    
    while( .solnp_iter < maxit ){
        .solnp_iter = .solnp_iter + 1
        .subnp_ctrl = c(rho, minit, delta, tol)
        
        # make the scale for the cost, the equality constraints, the inequality
        # constraints, and the parameters
        if( ind[7] ) {
            # [1 neq]
            vscale = c( ob[ 1 ], rep(1, neq) * max( abs(ob[ 2:(neq + 1) ]) ) )
        } else {
            vscale = 1
        }
        
        if( !ind[ 11 ] ) {
            vscale = c(vscale, p)
        } else {
            # [ 1 neq np]
            vscale = c(vscale, rep( 1, length = length(p) ) )
        }
        
        vscale = apply( matrix(vscale, ncol = 1), 1, FUN = function( x ) min( max( abs(x), tol ), 1/tol ) )
        
        res   = .subnp(pars = p, yy = lambda, ob = ob, hessv = hessv, lambda = mu, vscale = vscale, 
                ctrl = .subnp_ctrl, .env = .solnpenv, ...)
        
        p  = res$p
        lambda  = res$y
        hessv  = res$hessv
        mu = res$lambda
        temp = p[ (nineq + 1):(nineq + np) ]
        funv = .solnp_fun(temp, ...)
        .solnp_nfn <<- .solnp_nfn + 1
        
        tempdf = cbind(temp, funv)
        
        if( trace ){
            .report(.solnp_iter, funv, temp)
        }
        
        eqv = .solnp_eqfun(temp, ...)       
        ineqv = .solnp_ineqfun(temp, ...)
        
        ob = c(funv, eqv, ineqv)
        
        tt[ 1 ] = (j - ob[ 1 ]) / max(abs(ob[ 1 ]), 1)
        j = ob[ 1 ]
        
        if( tc > 0 ){
            constraint = ob[ 2:(tc + 1) ]
            
            if( ind[ 4 ] ){
                tempv = rbind( constraint[ (neq + 1):tc ] - pb[ 1:nineq, 1 ],
                              pb[ 1:nineq, 2 ] - constraint[ (neq + 1):tc ] )
                              
                if( min(tempv) > 0 ) {
                    p[ 1:nineq ] = constraint[ (neq + 1):tc ]
                }
                
                constraint[ (neq + 1):tc ] = constraint[ (neq + 1):tc ] - p[ 1:nineq ]
            }
            
            tt[ 3 ] = .vnorm(constraint)
            
            if( tt[ 3 ] < 10 * tol ) { 
                rho = 0
                mu  = min(mu, tol)
            }
            
            if( tt[ 3 ] < 5 * tt[ 2 ]) {
                rho = rho/5
            }
            
            if( tt[ 3 ] > 10 * tt[ 2 ]) {
                rho = 5 * max( rho, sqrt(tol) )
            }
            
            if( max( c( tol + tt[ 1 ], tt[ 2 ] - tt[ 3 ] ) ) <= 0 ) { 
                lambda = 0
                hessv = diag( diag ( hessv ) )
            }

            tt[ 2 ] = tt[ 3 ]
        }
        
        if( .vnorm( c(tt[ 1 ], tt[ 2 ]) ) <= tol ) {
            maxit = .solnp_iter
        }
        
        jh = c(jh, j)
    }
    
    if( ind[ 4 ] ) {
        ineqx0 = p[ 1:nineq ]
    }
    
    p = p[ (nineq + 1):(nineq + np) ]
    
    if( .vnorm( c(tt[ 1 ], tt[ 2 ]) ) <= tol ) {
        convergence = 0
        if (TRACE) cat( paste( 
            "\nsolnp--> Completed in ", .solnp_iter, 
            " iterations\n", sep="" ) )
    } else{
        convergence = 1
        if (TRACE) cat( paste( 
            "\nsolnp--> Exiting after maximum number of iterations\n",
            "Tolerance not achieved\n", sep="" ) )
    }
    # end timer
    toc = Sys.time() - tic
    ans = list(pars = p, convergence = convergence, values = jh, lagrange = lambda, 
            hessian = hessv, ineqx0 = ineqx0, nfuneval = .solnp_nfn, outer.iter = .solnp_iter, 
            elapsed = toc)
    return( ans )
}


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


# Based on the original subnp Yinyu Ye
# http://www.stanford.edu/~yyye/Col.html


.subnp <-
function(pars, yy, ob, hessv, lambda, vscale, ctrl, .env, ...)
{
    .solnp_fun = get(".solnp_fun", envir = .env)
    .solnp_eqfun = get(".solnp_eqfun", envir = .env)
    .solnp_ineqfun = get(".solnp_ineqfun", envir = .env)
    ineqLB = get(".ineqLB", envir = .env)
    ineqUB = get(".ineqUB", envir = .env)
    LB = get(".LB", envir = .env)
    UB = get(".UB", envir = .env)
    .solnp_gradfun = get(".solnp_gradfun", envir = .env)
    .solnp_eqjac = get(".solnp_eqjac", envir = .env)
    .solnp_ineqjac = get(".solnp_ineqjac", envir = .env)
    ind = get("ind", envir = .env)
    # pars [nineq + np] 
    rho   = ctrl[ 1 ]
    maxit = ctrl[ 2 ]
    delta = ctrl[ 3 ]
    tol   = ctrl[ 4 ]
    
    # [1] length of pars
    # [2] has function gradient?
    # [3] has hessian?
    # [4] has ineq?
    # [5] ineq length
    # [6] has jacobian (inequality)
    # [7] has eq?
    # [8] eq length
    # [9] has jacobian (equality)
    # [10] has upper / lower bounds
    # [11] has either lower/upper bounds or ineq
    
    
    neq   = ind[ 8 ]
    nineq = ind[ 5 ]
    np    = ind[ 1 ]
    ch    = 1
    alp   = c(0,0,0)
    nc    = neq + nineq
    npic  = np + nineq
    p0    = pars
    
    # pb [ 2 x (nineq + np) ]
    pb    = rbind( cbind(ineqLB, ineqUB), cbind(LB,UB) )
    sob   = numeric()
    ptt   = matrix()
    sc    = numeric()
    
    # scale the cost, the equality constraints, the inequality constraints, 
    # the parameters (inequality parameters AND actual parameters), 
    # and the parameter bounds if there are any
    # Also make sure the parameters are no larger than (1-tol) times their bounds
    # ob [ 1 neq nineq]
    ob = ob / vscale[ 1:(nc + 1) ]
    # p0 [np]
    p0 = p0 / vscale[ (neq + 2):(nc + np + 1) ]
    if( ind[ 11 ] ) {
        
        if( !ind[ 10 ] ) {
            mm = nineq
        } else {
            mm = npic
        }
        
        pb = pb / cbind(vscale[ (neq + 2):(neq + mm + 1) ], vscale[ (neq + 2):(neq + mm + 1) ])
    }

    # scale the lagrange multipliers and the Hessian
    
    if( nc > 0) {
        # yy [total constraints = nineq + neq]
        # scale here is [tc] and dot multiplied by yy
        yy = vscale[ 2:(nc + 1) ] * yy / vscale[ 1 ]
    }
    
    # yy = [zeros 3x1]
    
    # h is [ (np+nineq) x (np+nineq) ]
    # columnvector %*% row vector (size h) then dotproduct h then dotdivide scale[1]
    
    hessv = hessv * (vscale[ (neq + 2):(nc + np + 1) ] %*% 
        t(vscale[ (neq + 2):(nc + np + 1)]) ) / vscale[ 1 ]
    
    # h[ 8x8 eye]
    
    j = ob[ 1 ]
    
    if( ind[4] ) {
        
        if( !ind[7] ) {
            # [nineq x (nineq+np) ]
            a = cbind( -diag(nineq), matrix(0, ncol = np, nrow = nineq) ) 
        } else {
            # [ (neq+nineq) x (nineq+np)]
            a = rbind( cbind( 0 * .ones(neq, nineq), matrix(0, ncol = np, nrow = neq) ), 
                    cbind( -diag(nineq), matrix(0, ncol = np, nrow = nineq) ) )
        }
        
    }
    if( ind[7] && !ind[4] ) {
        a = .zeros(neq, np)
    }
    
    if( !ind[7] && !ind[4] ) {
        a = .zeros(1, np)
    }
    
    # gradient
    g= 0 * .ones(npic, 1)
    
    if( nc > 0 ) {
        # [ nc ]
        constraint = ob[ 2:(nc + 1) ]
        # constraint [5 0 11 3x1]
        for( i in 1:np ) {
            # scale the parameters (non ineq)
            p0[ nineq + i ] = p0[ nineq + i ] + delta
            tmpv = p0[ (nineq + 1):npic ] * vscale[ (nc + 2):(nc + np + 1) ]
            funv = .solnp_fun(tmpv, ...)
            eqv = .solnp_eqfun(tmpv, ...)
            ineqv = .solnp_ineqfun(tmpv, ...)
            .solnp_nfn <<- .solnp_nfn + 1

            ob = c(funv, eqv, ineqv) / vscale[ 1:(nc + 1) ]
            g[ nineq + i ]   = (ob[ 1 ] - j) / delta
            a[ , nineq + i ] = (ob[ 2:(nc + 1) ] - constraint) / delta
            p0[ nineq + i ]  = p0[ nineq + i ] - delta
        }
        
        if( ind[4] ) {
            constraint[ (neq + 1):(neq + nineq) ] = 
                constraint[ (neq + 1):(neq + nineq) ] - p0[ 1:nineq ]
        }
        
        # solver messages
        if( .solvecond(a) > 1 / .eps ) { 
            .subnpmsg( "m1" )
        }

        # a(matrix) x columnvector - columnvector
        # b [nc,1]
        b  = a %*% p0 - constraint
        ch = -1
        alp[ 1 ] = tol - max( abs(constraint) )
        
        if( alp[ 1 ] <= 0 ) {
            ch = 1
            
            if( !ind[11] ) {
                # a %*% t(a) gives [nc x nc]
                # t(a) %*% above gives [(np+nc) x 1]
                p0 = p0 - t(a) %*% solve(a %*% t(a), constraint)
                alp[ 1 ] = 1
            }

        }
        
        if( alp[ 1 ] <= 0 ) {
            # this expands p0 to [nc+np+1]
            p0[ npic + 1 ] = 1
            a  = cbind(a, -constraint)
            # cx is rowvector
            cx = cbind(.zeros(1,npic), 1)
            dx = .ones(npic + 1, 1)
            go = 1 
            minit = 0
            
            while( go >= tol ) {
                minit = minit + 1
                # gap [(nc + np) x 2]
                gap = cbind(p0[ 1:mm ] - pb[ , 1 ], pb[ , 2 ] - p0[ 1:mm ] )
                # this sorts every row
                gap = t( apply( gap, 1, FUN=function( x ) sort(x) ) )
                dx[ 1:mm ] = gap[ , 1 ]
                # expand dx by 1
                dx[ npic + 1 ] = p0[ npic + 1 ]
                
                if( !ind[10] ) {
                    dx[ (mm + 1):npic ] = max( c(dx[ 1:mm ], 100) ) * .ones(npic - mm, 1)
                }
                # t( a %*% diag( as.numeric(dx) ) ) gives [(np+nc + 1 (or more) x nc]
                # dx * t(cx) dot product of columnvectors
                # qr.solve returns [nc x 1]
                y = qr.solve( t( a %*% diag( as.numeric(dx) ) ), dx * t(cx) )
                v = dx * ( dx *(t(cx) - t(a) %*% y) )
                
                if( v[ npic + 1 ] > 0 ) {
                    z = p0[ npic + 1 ] / v[ npic + 1 ]
                    
                    for( i in 1:mm ) {
                    
                        if( v[ i ] < 0 ) {
                            z = min(z, -(pb[ i, 2 ] - p0[ i ]) / v[ i ])
                        } else if( v[ i ] > 0 ) { 
                            z = min( z, (p0[ i ] - pb[ i , 1 ]) / v[ i ]) 
                        }
                    }
                    
                    if( z >= p0[ npic + 1 ] / v[ npic + 1 ] ) {
                        p0 = p0 - z * v
                    } else {
                        p0 = p0 - 0.9 * z * v 
                    }
                    go = p0[ npic + 1 ]
                    
                    if( minit >= 10 ) {
                        go = 0 
                    }
                    
                } else {
                    go = 0
                    minit = 10
                }
                
            }
            
            if( minit >= 10 ) {
                .subnpmsg( "m2" )
            }
            
            a = matrix(a[ , 1:npic ], ncol = npic)
            b = a %*% p0[ 1:npic ]
        }
        
    }
    
    p = p0 [ 1:npic ]
    y = 0
    
    if( ch > 0 ) {
        
        tmpv = p[ (nineq + 1):npic ] * vscale[ (nc + 2):(nc + np + 1) ]
        funv = .solnp_fun(tmpv, ...)
        eqv = .solnp_eqfun(tmpv, ...)
        ineqv = .solnp_ineqfun(tmpv, ...)
        .solnp_nfn <<- .solnp_nfn + 1
        ob = c(funv, eqv, ineqv) / vscale[ 1:(nc + 1) ]
    }
    
    j = ob[ 1 ]
    
    if( ind[4] ) {
        ob[ (neq + 2):(nc + 1) ] = ob[ (neq + 2):(nc + 1) ] - p[ 1:nineq ]

    }
    
    if( nc > 0 ) {
        ob[ 2:(nc + 1) ] = ob[ 2:(nc + 1) ] - a %*% p + b
        j = ob[ 1 ] - t(yy) %*% matrix(ob[ 2:(nc + 1) ],ncol=1) + rho * .vnorm(ob[ 2:(nc + 1) ]) ^ 2
    }
    
    minit = 0
    while( minit < maxit ) {
        minit = minit + 1
        
        if( ch > 0 ) {
        
            for( i in 1:np ) {
                
                p[ nineq + i ] = p[ nineq + i ] + delta
                tmpv = p[ (nineq + 1):npic ] * vscale[ (nc + 2):(nc + np + 1) ]
                funv = .solnp_fun(tmpv, ...)
                eqv = .solnp_eqfun(tmpv, ...)
                ineqv = .solnp_ineqfun(tmpv, ...)
                .solnp_nfn <<- .solnp_nfn + 1
                obm = c(funv, eqv, ineqv) / vscale[ 1:(nc + 1) ]
                
                if( ind[4] ) {
                    obm[ (neq + 2):(nc + 1)] = obm[ (neq + 2):(nc + 1) ] - p[ 1:nineq ]
                }
                
                if( nc > 0 ) {
                    
                    obm[ 2:(nc + 1) ] = obm[ 2:(nc + 1) ] - a %*% p + b
                    obm = obm[ 1 ] - t(yy) %*% obm[ 2:(nc + 1) ] + rho * .vnorm(obm[ 2:(nc + 1 ) ]) ^ 2
                }
                
                g[ nineq + i ] = (obm - j) / delta
                p[ nineq + i ] = p[ nineq + i ] - delta
            }
            
            if( ind[4] ) {
                g[ 1:nineq ] = 0
            }
            
        }
        
        if( minit > 1 ) {
            yg = g - yg
            sx = p - sx
            sc[ 1 ] = t(sx) %*% hessv %*% sx
            sc[ 2 ] = t(sx) %*% yg
            
            if( (sc[ 1 ] * sc[ 2 ]) > 0 ) {
                sx = hessv %*% sx
                hessv  = hessv - ( sx %*% t(sx) ) / sc[ 1 ] + ( yg %*% t(yg) ) / sc[ 2 ]
            }
            
        }
        
        dx = 0.01 * .ones(npic, 1)
        if( ind[11] ) {
            
            gap = cbind(p[ 1:mm ] - pb[ , 1 ], pb[ , 2 ] - p[ 1:mm ])
            gap = t( apply( gap, 1, FUN = function( x ) sort(x) ) )
            gap = gap[ , 1 ] + sqrt(.eps) * .ones(mm, 1)
            dx[ 1:mm, 1 ] = .ones(mm, 1) / gap
            if( !ind[10] ){
                dx[ (mm + 1):npic, 1 ] = min (c( dx[ 1:mm, 1 ], 0.01) ) * .ones(npic - mm, 1)
            }
            
        }
        # sunday until here
        go = -1
        lambda = lambda / 10
        while( go <= 0 ) {
            cz = chol( hessv + lambda * diag( as.numeric(dx * dx) ) )
            cz = solve(cz)
            yg = t(cz) %*% g
            
            if( nc == 0 ) {
                u = -cz %*% yg
            } else{
                y = qr.solve(t(cz) %*% t(a), yg)
                u = -cz %*% (yg - ( t(cz) %*% t(a) ) %*% y)
            }
            
            p0 = u[ 1:npic ] + p
            if( !ind[ 11 ] ) {
                go = 1
            } else {
                go = min( c(p0[ 1:mm ] - pb[ , 1 ], pb[ , 2 ] - p0[ 1:mm ]) )
                lambda = 3 * lambda
            }
            
        }
        
        alp[ 1 ] = 0
        ob1 = ob
        ob2 = ob1
        sob[ 1 ] = j
        sob[ 2 ] = j
        ptt = cbind(p, p)
        alp[ 3 ] = 1.0
        ptt = cbind(ptt, p0)
        tmpv = ptt[ (nineq + 1):npic, 3 ] * vscale[ (nc + 2):(nc + np + 1) ]
        funv = .solnp_fun(tmpv, ...)
        eqv = .solnp_eqfun(tmpv, ...)
        ineqv = .solnp_ineqfun(tmpv, ...)
        .solnp_nfn <<- .solnp_nfn + 1
        
        ob3 = c(funv, eqv, ineqv) / vscale[ 1:(nc + 1) ]
        sob[ 3 ] = ob3[ 1 ]
        
        if( ind[4] ) {
            ob3[ (neq + 2):(nc + 1) ] = ob3[ (neq + 2):(nc + 1) ] - ptt[ 1:nineq, 3 ]
        }
        
        if( nc > 0 ) {
            ob3[ 2:(nc + 1) ] = ob3[ 2:(nc + 1) ] - a %*% ptt[ , 3 ] + b
            sob[ 3 ] = ob3[ 1 ] - t(yy) %*% ob3[ 2:(nc + 1) ] + rho * .vnorm(ob3[ 2:(nc + 1) ]) ^ 2
        }
        
        go = 1
        while( go > tol ) {
            alp[ 2 ] = (alp[ 1 ] + alp[ 3 ]) / 2
            ptt[ , 2 ] = (1 - alp[ 2 ]) * p + alp[ 2 ] * p0
            tmpv = ptt[ (nineq + 1):npic, 2 ] * vscale[ (nc + 2):(nc + np + 1) ]
            funv = .solnp_fun(tmpv, ...)
            eqv = .solnp_eqfun(tmpv, ...)
            ineqv = .solnp_ineqfun(tmpv, ...)
            .solnp_nfn <<- .solnp_nfn + 1
            
            ob2 = c(funv, eqv, ineqv) / vscale[ 1:(nc + 1) ]
            
            sob[ 2 ] = ob2[ 1 ]

            if( ind[4] ) {
                ob2[ (neq + 2):(nc + 1) ] = ob2[ (neq + 2):(nc + 1) ] - ptt[ 1:nineq , 2 ]
            }
            
            if( nc > 0 ) {
                ob2[ 2:(nc + 1) ] = ob2[ 2:(nc + 1) ] - a %*% ptt[ , 2 ] + b
                sob[ 2 ] = ob2[ 1 ] - t(yy) %*% ob2[ 2:(nc + 1) ] + rho * .vnorm(ob2[ 2:(nc + 1) ]) ^ 2
            }
            
            obm = max(sob)
            
            if( obm < j ) {
                obn = min(sob)
                go = tol * (obm - obn) / (j - obm)
            }
            # monday
            condif1 = sob[ 2 ] >= sob[ 1 ]
            condif2 = sob[ 1 ] <= sob[ 3 ] && sob[ 2 ] < sob[ 1 ]
            condif3 = sob[ 2 ] <  sob[ 1 ] && sob[ 1 ] > sob[ 3 ]
            
            if( condif1 ) {
                sob[ 3 ] = sob[ 2 ]
                ob3 = ob2
                alp[ 3 ] = alp[ 2 ]
                ptt[ , 3 ] = ptt[ , 2 ]
            }
            
            if( condif2 ) {
                sob[ 3 ] = sob[ 2 ]
                ob3 = ob2
                alp[ 3 ] = alp[ 2 ]
                ptt[ , 3 ] = ptt[ , 2 ]
            }
            
            if( condif3 ) {
                sob[ 1 ] = sob[ 2 ]
                ob1 = ob2
                alp[ 1 ] = alp[ 2 ]
                ptt[ , 1 ] = ptt[ , 2 ]
            }
            
            if( go >= tol ) {
                go = alp[ 3 ] - alp[ 1 ]
            }
            
        }
        
        sx = p
        yg = g
        ch = 1
        obn = min(sob)
        if( j <= obn ) {
            maxit = minit
        }
        
        reduce = (j - obn) / ( 1 + abs(j) )
        
        if( reduce < tol ) {
            maxit = minit
        }
        
        condif1 = sob[ 1 ] <  sob[ 2 ]
        condif2 = sob[ 3 ] <  sob[ 2 ] && sob[ 1 ] >= sob[ 2 ]
        condif3 = sob[ 1 ] >= sob[ 2 ] && sob[ 3 ] >= sob[ 2 ]
        
        if( condif1 ) {
            j = sob[ 1 ]
            p = ptt[ , 1 ]
            ob = ob1
        }
        
        if( condif2 ) {
            j = sob [ 3 ]
            p = ptt[ , 3 ]
            ob = ob3
        }
        
        if( condif3 ) {
            j = sob[ 2 ]
            p = ptt[ , 2 ]
            ob = ob2
        }
        
    }
    
    p = p * vscale[ (neq + 2):(nc + np + 1) ]  # unscale the parameter vector
    
    if( nc > 0 ) {
        y = vscale[ 1 ] * y / vscale[ 2:(nc + 1) ] # unscale the lagrange multipliers
    }
    
    hessv = vscale[ 1 ] * hessv / (vscale[ (neq + 2):(nc + np + 1) ] %*% 
        t(vscale[ (neq + 2):(nc + np + 1) ]) )
    
    if( reduce > tol ) {
        .subnpmsg( "m3" )
    }
    # tuesday
    ans = list(p = p, y = y, hessv = hessv, lambda = lambda)
    return( ans )
}


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


.eps = .Machine$double.eps


# ------------------------------------------------------------------------------


.subnpmsg <-
function(m)
{
    g1=c("solnp-->")
    m1=paste("\n",g1,"Redundant constraints were found. Poor\n",
            g1,"intermediate results may result.Suggest that you\n",
            g1,"remove redundant constraints and re-OPTIMIZE\n",sep="")
    m2=paste("\n",g1,"The linearized problem has no feasible\n",
            g1,"solution.  The problem may not be feasible.\n",sep="")
    m3=paste("\n",g1,"Minor optimization routine did not converge in the \n",
            g1,"specified number of minor iterations.  You may need\n",
            g1,"to increase the number of minor iterations.        \n",sep="")
    ans=switch(m,
            m1=m1,
            m2=m2,
            m3=m3)
    cat(ans)
}


# ------------------------------------------------------------------------------


.checkpars <- 
function(pars, LB, UB, .env)
{
    if(is.null(pars))
        stop("\nsolnp-->error: must supply starting parameters\n", call. = FALSE)
    if(!is.null(LB)){
        if(length(pars)!=length(LB))
            stop("\nsolnp-->error: LB length not equal to parameter length\n", call. = FALSE)
        if(is.null(UB)) UB = rep(.Machine$double.xmax/2, length(LB))
    } else{
        LB = NULL
    }
    if(!is.null(UB)){
        if(length(pars)!=length(UB))
            stop("\nsolnp-->error: UB length not equal to parameter length\n", call. = FALSE)
        if(is.null(LB)) LB = rep(-.Machine$double.xmax/2, length(UB))
    } else{
        UB = NULL
    }
    if(!is.null(UB) && any(LB>UB))
        stop("\nsolnp-->error: UB must be greater than LB\n", call. = FALSE)
    
    if(!is.null(UB) && any(LB==UB))
        warning("\nsolnp-->warning: Equal Lower/Upper Bounds Found. Consider\n
                        excluding fixed parameters.\n", call. = FALSE)
    # deal with infinite values as these are not accepted by solve.QP
    if(!is.null(LB) && !any(is.finite(LB))){
        idx = which(!is.finite(LB))
        LB[idx] = sign(LB[idx])*.Machine$double.xmax/2
    }
    if(!is.null(UB) && !any(is.finite(UB))){
        idx = which(!is.finite(UB))
        UB[idx] = sign(UB[idx])*.Machine$double.xmax/2
    }   
    assign(".LB", LB, envir = .env)
    assign(".UB", UB, envir = .env)
    return(1)
}


# ------------------------------------------------------------------------------


.checkfun <- 
function(pars, fun, .env, ...)
{
    if(!is.function(fun))
        stop("\nsolnp-->error: fun does not appear to be a function\n", call. = FALSE)
    val = fun(pars, ...)
    if(length(val)!=1)
        stop("\nsolnp-->error: objective function returns value of length greater than 1!\n", call. = FALSE)
    assign(".solnp_fun", fun, envir = .env)
    .solnp_nfn <<- .solnp_nfn + 1
    return(val)
}


# ------------------------------------------------------------------------------


.checkgrad <- 
function(pars, fun, .env, ...)
{
    n = length(pars)
    val = fun(pars, ...)
    if(length(val)!=n)
        stop("\nsolnp-->error: gradient vector length must be equal to length(pars)\n", call. = FALSE)
    assign(".solnp_gradfun", fun, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


.checkhess <- 
function(pars, fun, .env, ...)
{
    n = length(pars)
    val = fun(pars, ...)
    if(length(as.vector(val))!=(n*n))
        stop("\nsolnp-->error: hessian must be of length length(pars) x length(pars)\n", call. = FALSE)
    assign(".solnp_hessfun", fun, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


.checkineq <- 
function(pars, fun, ineqLB, ineqUB, .env, ...)
{
    val = fun(pars, ...)
    n = length(val)
    if(!is.null(ineqLB)){
        if(length(ineqLB)!=n)
            stop("\nsolnp-->error: inequality function returns vector of different length to
                            inequality lower bounds\n", call. = FALSE)
    } else{
        stop("\nsolnp-->error: inequality function given without lower bounds\n", call. = FALSE)
    }
    if(!is.null(ineqUB)){
        if(length(ineqUB)!=n)
            stop("\nsolnp-->error: inequality function returns vector of different length to
                            inequality upper bounds\n", call. = FALSE)
    } else{
        stop("\nsolnp-->error: inequality function given without upper bounds\n", call. = FALSE)
    }
    if(any(ineqLB>ineqUB))
        stop("\nsolnp-->error: ineqUB must be greater than ineqLB\n", call. = FALSE)
    # transform from:
    # ineqLB =< ineqfun(x) =< ineqUB
    # to:
    # ineqfun(x) >= 0
    assign(".ineqLB", ineqLB, envir = .env)
    assign(".ineqUB", ineqUB, envir = .env)
    assign(".solnp_ineqfun", fun, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


.checkeq <- 
function(pars, fun, eqB, .env, ...)
{
    n = length(eqB)
    val = fun(pars, ...) - eqB
    if(length(val)!=n)
        stop("\nsolnp-->error: equality function returns vector of different length
                        to equality value\n", call. = FALSE)
    .eqB = eqB
    assign(".eqB", .eqB, envir = .env)
    .solnp_eqfun <- function(x, ...) fun(x, ...) - .eqB
    assign(".solnp_eqfun", .solnp_eqfun, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


# check the jacobian of inequality
.cheqjacineq <- 
function(pars, fun, .env,  ...)
{
    # must be a matrix -> nrows = no.inequalities, ncol = length(pars)
    val = fun(pars, ...)
    if(!is.matrix(val))
        stop("\nsolnp-->error: Jacobian of Inequality must return a matrix type object\n", call. = FALSE)
    nd = dim(val)
    if(nd[2]!=length(pars))
        stop("\nsolnp-->error: Jacobian of Inequality column dimension must be equal to length
                        of parameters\n", call. = FALSE)
    if(nd[1]!=length(.solnp_ineqUB))
        stop("\nsolnp-->error: Jacobian of Inequality row dimension must be equal to length
                        of inequality bounds vector\n", call. = FALSE)
    # as in inequality function, transforms from a 2 sided inequality to a one sided inequality
    # (for the jacobian).
    .solnp_ineqjac <- function(x, ...) { retval = fun(x, ...); rbind( - retval, retval ) }
    assign(".solnp_ineqjac", .solnp_ineqjac, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


# check the jacobian of equality
.cheqjaceq <- 
function(pars, fun, .env, ...)
{
    # must be a matrix -> nrows = no.equalities, ncol = length(pars)
    val = fun(pars, ...)
    if(!is.matrix(val))
        stop("\nsolnp-->error: Jacobian of Equality must return a matrix type object\n", call. = FALSE)
    nd = dim(val)
    if(nd[2]!=length(pars))
        stop("\nsolnp-->error: Jacobian of Equality column dimension must be equal to length
            of parameters\n", call. = FALSE)
    if(nd[1]!=length(.solnp_eqB))
        stop("\nsolnp-->error: Jacobian of Equality row dimension must be equal to length
            of equality bounds vector\n", call. = FALSE)
    assign(".solnp_eqjac", fun, envir = .env)
    return(val)
}


# ------------------------------------------------------------------------------


# reporting function
.report <- 
function(iter, funv, pars)
{
    cat( paste( "\nIter: ", iter ," fn: ", 
        format(funv, digits = 4, scientific = 5, nsmall = 4, zero.print = TRUE), 
            "\t Pars: ", sep=""), 
        format(pars, digits = 4, scientific = 6, nsmall = 5, zero.print = TRUE))
}


# finite difference gradient
.fdgrad <- 
function(pars, fun, ...)
{
    if(!is.null(fun)){
        
        y0 = fun(pars, ...)
        nx = length(pars)
        grd = rep(0, nx)
        deltax = sqrt(.eps)
        for(i in 1:nx)
        {
            init = pars[i]
            pars[i]= pars[i] + deltax
            grd[i] = (fun(pars, ...) - y0) / deltax
            pars[i] = init
        }
    }
    else
    {
        grd = 0
    }
    return(grd)
}


# ------------------------------------------------------------------------------


# finite difference jacobian
.fdjac <- 
function(pars, fun, ...)
{
    nx = length(pars)
    if(!is.null(fun))
    {
        y0 = fun(pars, ...)
        nf = length (y0)
        jac = matrix(0, nrow = nf, ncol= nx)
        deltax = sqrt (.eps)
        for(i  in 1:nx)
        {
            init = pars[i]
            pars[i]= pars[i] + deltax
            jac[,i] = (fun(pars, ...) - y0) / deltax
            pars[i] = init
        }
    } else{
        jac = rep(0, nx)
    }
    return(jac)
}


# ------------------------------------------------------------------------------


.emptygrad <- 
function(pars, ...)
{
    matrix(0, nrow = 0, ncol = 1)
}


# ------------------------------------------------------------------------------


.emptyjac <- 
function(pars, ...)
{
    matrix(0, nrow = 0, ncol = length(pars))
}


# ------------------------------------------------------------------------------


.emptyfun <- 
function(pars, ...)
{
    NULL
}


# ------------------------------------------------------------------------------


.ineqlbfun <- 
function(pars, .env, ...)
{
    LB = get(".solnp_LB", envir = .env)
    UB = get(".solnp_UB", envir = .env)
    .solnp_ineqfun = get(".solnp_ineqfun", envir = .env)
    res = c(pars - LB,  UB - pars)
    if(!is.null(.solnp_ineqfun)) res = c(.solnp_ineqfun(pars, ...), res)
    res
}


# ------------------------------------------------------------------------------


.ineqlbjac <- 
function(pars, .env, ...)
{
    .solnp_ineqjac = get(".solnp_ineqjac", envir = .env)
    n = length(pars)
    res = rbind(diag(n), -diag(n))
    if(!is.null(.solnp_ineqjac)) res = rbind(.solnp_ineqjac(pars, ...), res)
    res
}


# ------------------------------------------------------------------------------


.solnpctrl <- 
function(control)
{
    # parameters check is now case independent
    ans = list()
    params = unlist(control)
    if(is.null(params)) {
        ans$rho = 1
        ans$outer.iter = 400
        ans$inner.iter = 800
        ans$delta = 1.0e-8
        ans$tol = 1.0e-6
        ans$trace = 1
    } else{
        npar = tolower(names(unlist(control)))
        names(params) = npar
        if(any(substr(npar, 1, 3) == "rho")) ans$rho = as.numeric(params["rho"]) else ans$rho = 1
        if(any(substr(npar, 1, 10) == "outer.iter")) ans$outer.iter = as.numeric(params["outer.iter"]) else ans$outer.iter = 400
        if(any(substr(npar, 1, 10) == "inner.iter")) ans$inner.iter = as.numeric(params["inner.iter"]) else ans$inner.iter = 800
        if(any(substr(npar, 1, 5) == "delta")) ans$delta = as.numeric(params["delta"]) else ans$delta = 1.0e-8
        if(any(substr(npar, 1, 3) == "tol")) ans$tol = as.numeric(params["tol"]) else ans$tol = 1.0e-6
        if(any(substr(npar, 1, 5) == "trace")) ans$trace = as.numeric(params["trace"]) else ans$trace = 1
    }
    return(ans)
}


# ------------------------------------------------------------------------------


.zeros <- 
function( n = 1, m = 1)
{
    if(missing(m)) m = n
    sol = matrix(0, nrow = n, ncol = m)
    return(sol)
}


# ------------------------------------------------------------------------------


.ones <- 
function(n = 1, m = 1)
{
    if(missing(m)) m = n
    sol = matrix(1, nrow = n, ncol = m)
    return(sol)
}


# ------------------------------------------------------------------------------


.vnorm <- 
function(x)
{
    sum((x)^2)^(1/2)
}


# ------------------------------------------------------------------------------


.solvecond <- 
function(x)
{
    z = svd(x)$d
    if(any( z == 0 )) ret = Inf else ret = max( z ) / min( z )
    return(ret)
}


################################################################################

