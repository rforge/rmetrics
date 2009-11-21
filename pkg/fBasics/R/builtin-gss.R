
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:            DESCRIPTION        
################################################################################


# Code borrowed from:


# Package: gss
# Version: 1.0-5
# Depends: R (>= 1.7.0)
# Title: General Smoothing Splines
# Author: Chong Gu <chong@stat.purdue.edu>
# Maintainer: Chong Gu <chong@stat.purdue.edu>
# Description: A comprehensive package for structural multivariate
#   function estimation using smoothing splines.
# License: GPL
# Packaged: Mon Mar  9 15:55:09 2009




## -----------------------------------------------------------------------------
## Fit density model


.ssden <- function(formula,type=NULL,data=list(),alpha=1.4,
    weights=NULL,subset,na.action=na.omit,
    id.basis=NULL,nbasis=NULL,seed=NULL,
    domain=as.list(NULL),quadrature=NULL,
    prec=1e-7,maxiter=30)
{
    ## Obtain model frame and model terms
    mf <- match.call()
    mf$type <- mf$alpha <- NULL
    mf$id.basis <- mf$nbasis <- mf$seed <- NULL
    mf$domain <- mf$quadrature  <- NULL
    mf$prec <- mf$maxiter <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf,sys.frame(sys.parent()))
    cnt <- model.weights(mf)
    mf$"(weights)" <- NULL
    ## Generate sub-basis
    nobs <- dim(mf)[1]
    if (is.null(id.basis)) {
        if (is.null(nbasis))  nbasis <- max(30,ceiling(10*nobs^(2/9)))
        if (nbasis>=nobs)  nbasis <- nobs
        if (!is.null(seed))  set.seed(seed)
        id.basis <- sample(nobs,nbasis,prob=cnt)
    }
    else {
        if (max(id.basis)>nobs|min(id.basis)<1)
            stop("gss error in .ssden: id.basis out of range")
        nbasis <- length(id.basis)
    }
    ## Set domain and/or generate quadrature
    if (is.null(quadrature)) {
        ## Set domain and type
        fac.list <- NULL
        for (xlab in names(mf)) {
            x <- mf[[xlab]]
            if (is.factor(x)) {
                fac.list <- c(fac.list,xlab)
                domain[[xlab]] <- NULL
            }
            else {
                if (!is.vector(x))
                    stop("gss error in .ssden: no default quadrature")
                if (is.null(domain[[xlab]])) {
                    mn <- min(x)
                    mx <- max(x)
                    domain[[xlab]] <- c(mn,mx)+c(-1,1)*(mx-mn)*.05
                }
                else domain[[xlab]] <- c(min(domain[[xlab]]),max(domain[[xlab]]))
                if (is.null(type[[xlab]]))
                    type[[xlab]] <- list("cubic",domain[[xlab]])
                else {
                    if (length(type[[xlab]])==1)
                        type[[xlab]] <- list(type[[xlab]][[1]],domain[[xlab]])
                }
            }
        }
        ## Generate numerical quadrature
        domain <- data.frame(domain)
        mn <- domain[1,]
        mx <- domain[2,]
        if (ncol(domain)==1) {
            ## Gauss-Legendre quadrature
            quad <- .gauss.quad(200,c(mn,mx))
            quad$pt <- data.frame(quad$pt)
            colnames(quad$pt) <- colnames(domain)
        }
        else {
            ## Smolyak cubature
            if (ncol(domain)>4)
                stop("gss error in .ssden: dimension higher than 4 unsupported")
            code <- c(15,14,13)
            quad <- .smolyak.quad(ncol(domain),code[ncol(domain)-1])
            for (i in 1:ncol(domain)) {
                xlab <- colnames(domain)[i]
                wk <- mf[[xlab]]
                jk <- .ssden(~wk,domain=data.frame(wk=domain[,i]),alpha=2,
                            id.basis=id.basis)
                quad$pt[,i] <- .qssden(jk,quad$pt[,i])
                quad$wt <- quad$wt/.dssden(jk,quad$pt[,i])
            }
            jk <- wk <- NULL
            quad$pt <- data.frame(quad$pt)
            colnames(quad$pt) <- colnames(domain)
        }
        ## Incorporate factors in quadrature
        if (!is.null(fac.list)) {
            for (i in 1:length(fac.list)) {
                wk <-
                  expand.grid(levels(mf[[fac.list[i]]]),1:length(quad$wt))
                quad$wt <- quad$wt[wk[,2]]
                col.names <- c(fac.list[i],colnames(quad$pt))
                quad$pt <- data.frame(wk[,1],quad$pt[wk[,2],])
                colnames(quad$pt) <- col.names
            }
        }
        quadrature <- list(pt=quad$pt,wt=quad$wt)
    }
    else {
        for (xlab in names(mf)) {
            x <- mf[[xlab]]
            if (is.vector(x)&!is.factor(x)) {
                mn <- min(x,quadrature$pt[[xlab]])
                mx <- max(x,quadrature$pt[[xlab]])
                range <- c(mn,mx)+c(-1,1)*(mx-mn)*.05
                if (is.null(type[[xlab]]))
                    type[[xlab]] <- list("cubic",range)
                else {
                    if (length(type[[xlab]])==1)
                        type[[xlab]] <- list(type[[xlab]][[1]],range)
                    else {
                        mn0 <- min(type[[xlab]][[2]])
                        mx0 <- max(type[[xlab]][[2]])
                        if ((mn0>mn)|(mx0<mx))
                            stop("gss error in .ssden: range not covering domain")
                    }
                }
            }
        }
    }
    ## Generate terms
    term <- .mkterm(mf,type)
    term$labels <- term$labels[term$labels!="1"]
    ## Generate s, r, and q
    qd.pt <- quadrature$pt
    qd.wt <- quadrature$wt
    nmesh <- length(qd.wt)
    s <- qd.s <- r <- qd.r <- q <- NULL
    nq <- 0
    for (label in term$labels) {
        x <- mf[,term[[label]]$vlist]
        x.basis <- mf[id.basis,term[[label]]$vlist]
        qd.x <- qd.pt[,term[[label]]$vlist]
        nphi <- term[[label]]$nphi
        nrk <- term[[label]]$nrk
        if (nphi) {
            phi <- term[[label]]$phi
            for (i in 1:nphi) {
                s <- cbind(s,phi$fun(x,nu=i,env=phi$env))
                qd.s <- cbind(qd.s,phi$fun(qd.x,nu=i,env=phi$env))
            }
        }
        if (nrk) {
            rk <- term[[label]]$rk
            for (i in 1:nrk) {
                nq <- nq+1
                r <- array(c(r,rk$fun(x.basis,x,nu=i,env=rk$env,out=TRUE)),c(nbasis,nobs,nq))
                qd.r <- array(c(qd.r,rk$fun(x.basis,qd.x,nu=i,env=rk$env,out=TRUE)),
                              c(nbasis,nmesh,nq))
                q <- array(c(q,rk$fun(x.basis,x.basis,nu=i,env=rk$env,out=TRUE)),
                           c(nbasis,nbasis,nq))
            }
        }
    }
    if (!is.null(s)) {
        nnull <- dim(s)[2]
        ## Check s rank
        if (qr(s)$rank<nnull)
            stop("gss error in .ssden: fixed effect MLE is not unique")
        s <- t(s)
        qd.s <- t(qd.s)
    }
    ## Fit the model
    if (nq==1) {
        r <- r[,,1]
        qd.r <- qd.r[,,1]
        q <- q[,,1]
        z <- .sspdsty(s,r,q,cnt,qd.s,qd.r,qd.wt,prec,maxiter,alpha)
    }
    else z <- .mspdsty(s,r,q,cnt,qd.s,qd.r,qd.wt,prec,maxiter,alpha)
    ## Brief description of model terms
    desc <- NULL
    for (label in term$labels)
        desc <- rbind(desc,as.numeric(c(term[[label]][c("nphi","nrk")])))
    desc <- rbind(desc,apply(desc,2,sum))
    rownames(desc) <- c(term$labels,"total")
    colnames(desc) <- c("Unpenalized","Penalized")
    ## Return the results
    obj <- c(list(call=match.call(),mf=mf,cnt=cnt,terms=term,desc=desc,alpha=alpha,
                  domain=domain,quad=quadrature,id.basis=id.basis),z)
    class(obj) <- ".ssden"
    obj
}


## -----------------------------------------------------------------------------


.dssden <- ## Evaluate density estimate
function(object,x) {
    if (class(object)!=".ssden") stop("gss error in .dssden: not a .ssden object")
    if (dim(object$mf)[2]==1&is.vector(x)) {
        x <- data.frame(x)
        colnames(x) <- colnames(object$mf)
    }
    s <- NULL
    r <- matrix(0,dim(x)[1],length(object$id.basis))
    nq <- 0
    for (label in object$terms$labels) {
        xx <- object$mf[object$id.basis,object$terms[[label]]$vlist]
        x.new <- x[,object$terms[[label]]$vlist]
        nphi <- object$terms[[label]]$nphi
        nrk <-  object$terms[[label]]$nrk
        if (nphi) {
            phi <-  object$terms[[label]]$phi
            for (i in 1:nphi) {
                s <- cbind(s,phi$fun(x.new,nu=i,env=phi$env))
            }
        }
        if (nrk) {
            rk <- object$terms[[label]]$rk
            for (i in 1:nrk) {
                nq <- nq + 1
                r <- r + 10^object$theta[nq]*rk$fun(x.new,xx,nu=i,env=rk$env,out=TRUE)
            }
        }
    }
    as.vector(exp(cbind(s,r)%*%c(object$d,object$c))/object$int)
}


.pssden <- ## Compute cdf for univariate density estimate
function(object,q) {
    if (class(object)!=".ssden") stop("gss error in .pssden: not a .ssden object")
    if (dim(object$mf)[2]!=1) stop("gss error in .pssden: not a 1-D density")
    mn <- min(object$domain)
    mx <- max(object$domain)
    order.q <- rank(q)
    p <- q <- sort(q)
    q.dup <- duplicated(q)
    p[q<=mn] <- 0
    p[q>=mx] <- 1
    kk <- (1:length(q))[q>mn&q<mx]
    for (i in kk) {
        if (q.dup[i]) {
            p[i] <- p.dup
            next
        }
        nqd.l <- max(20,ceiling((q[i]-mn)/(mx-mn)*200))
        qd.l <- .gauss.quad(nqd.l,c(mn,q[i]))
        p.l <- sum(.dssden(object,qd.l$pt)*qd.l$wt)
        nqd.u <- max(20,ceiling((mx-q[i])/(mx-mn)*200))
        qd.u <- .gauss.quad(nqd.u,c(q[i],mx))
        p.u <- sum(.dssden(object,qd.u$pt)*qd.u$wt)
        p[i] <- p.dup <- p.l/(p.l+p.u)
    }
    p[order.q]
}


.qssden <- ## Compute quantiles for univariate density estimate
function(object,p) {
    if (class(object)!=".ssden") stop("gss error in .qssden: not a .ssden object")
    if (dim(object$mf)[2]!=1) stop("gss error in .qssden: not a 1-D density")
    mn <- min(object$domain)
    mx <- max(object$domain)
    order.p <- rank(p)
    q <- p <- sort(p)
    p.dup <- duplicated(p)
    q[p<=0] <- mn
    q[p>=1] <- mx
    kk <- (1:length(p))[p>0&p<1]
    q.wk <- object$quad$pt[,1]
    p.wk <- cumsum(object$quad$wt*.dssden(object,q.wk))
    for (i in kk) {
        if (p.dup[i]) {
            q[i] <- q.dup
            next
        }
        j <- which.min(abs(p[i]-p.wk))
        q0 <- q.wk[j]
        p0 <- .pssden(object,q0)
        if (p0==p[i]) {
            q[i] <- q0
            next
        }
        if (p0<p[i]) {
            q.l <- q0
            p.l <- p0
            while (p0<p[i]) {
                j <- j+1
                q0 <- ifelse(is.null(q.wk[j]),mx,q.wk[j])
                p0 <- .pssden(object,q0)
            }
            q.u <- q0
            p.u <- p0
        }
        else {
            q.u <- q0
            p.u <- p0
            while (p0>p[i]) {
                j <- j-1
                q0 <- ifelse(is.null(q.wk[j]),mn,q.wk[j])
                p0 <- .pssden(object,q0)
            }
            q.l <- q0
            p.l <- p0
        }
        while (abs(p0-p[i])>1e-10) {
            q0 <- q.l+(p[i]-p.l)/(p.u-p.l)*(q.u-q.l)
            p0 <- .pssden(object,q0)
            if (p0>p[i]) {
                q.u <- q0
                p.u <- p0
            }
            else {
                q.l <- q0
                p.l <- p0
            }
        }
        q[i] <- q.dup <- q0
    }
    q[order.p]
}


## -----------------------------------------------------------------------------


.gauss.quad <- ## Generate Gauss-Legendre quadrature
function(size,interval) {
    if (interval[1]>=interval[2])
        warning("gss warning in .gauss.quad: interval limits swapped")
    z <- .Fortran("gaussq",
                  as.integer(1),
                  as.integer(size),
                  as.double(0), as.double(0),
                  as.integer(0),
                  as.double(c(-1,1)), double(size),
                  t=double(size), w=double(size),
                  PACKAGE="fBasics")
    mn <- min(interval[1:2])
    range <- abs(interval[1]-interval[2])
    pt <- mn+range*(z$t+1)/2
    wt <- range*z$w/2
    list(pt=pt,wt=wt)
}


## -----------------------------------------------------------------------------


.smolyak.quad <- ## Generate delayed Smolyak cubature
function(d, k) {
    size <- .C("size_smolyak",
               as.integer(d),
               as.integer(d+k),
               size=integer(1),
               PACKAGE="fBasics")$size
    z <- .C("quad_smolyak",
            as.integer(d),
            as.integer(d+k),
            pt=double(d*size),
            wt=as.double(1:size),
            PACKAGE="fBasics")
    list(pt=t(matrix(z$pt,d,size)),wt=z$wt)
}

.smolyak.size <- ## Get the size of delayed Smolyak cubature
function(d, k) {
    .C("size_smolyak",
       as.integer(d),
       as.integer(d+k),
       size=integer(1),
       PACKAGE="fBasics")$size
}


# ------------------------------------------------------------------------------


## Make phi and rk for model terms
.mkterm <- function(mf,type)
{
    ## Obtain model terms
    mt <- attr(mf,"terms")
    xvars <- as.character(attr(mt,"variables"))[-1]
    xfacs <- attr(mt,"factors")
    term.labels <- labels(mt)
    if (attr(attr(mf,"terms"),"intercept"))
        term.labels <- c("1",term.labels)
    ## Backward compatibility
    vlist <- xvars[as.logical(apply(xfacs,1,sum))]
    if (!is.null(type)&&!is.list(type)&&(type%in%c("cubic","linear","tp"))) {
        type.wk <- type
        type <- NULL
        for (xlab in vlist) type[[xlab]] <- type.wk
    }
    ## Set types for marginals    
    var.type <- NULL
    for (xlab in vlist) {
        x <- mf[,xlab]
        if (!is.null(type[[xlab]])) {
            ## Check consistency and set default parameters
            type.wk <- type[[xlab]][[1]]
            if
            (!(type.wk%in%c("ordinal","nominal","cubic","linear","per",
                            "cubic.per","linear.per","tp","sphere","custom")))
                stop("gss error in .mkterm: unknown type")
            if (type.wk%in%c("ordinal","nominal")) {
                par.wk <- NULL
                if (!is.factor(x))
                    stop("gss error in .mkterm: wrong type")
            }
            if (type.wk%in%c("cubic","linear")) {
                if (length(type[[xlab]])==1) {
                    mn <- min(x)
                    mx <- max(x)
                    par.wk <- c(mn,mx)+c(-1,1)*.05*(mx-mn)
                }
                else par.wk <- type[[xlab]][[2]]
                if (is.factor(x)|!is.vector(x))
                    stop("gss error in .mkterm: wrong type")
            }
            if (type.wk%in%c("per","cubic.per","linear.per")) {
                if (type.wk=="per") type.wk <- "cubic.per"
                if (length(type[[xlab]])==1)
                    stop("gss error in .mkterm: missing domain of periodicity")
                else par.wk <- type[[xlab]][[2]]
                if (is.factor(x)|!is.vector(x))
                    stop("gss error in .mkterm: wrong type")
            }
            if (type.wk=="tp") {
                if (length(type[[xlab]])==1)
                    par.wk <- list(order=2,mesh=x,weight=1)
                else {
                    par.wk <- par.wk1 <- type[[xlab]][[2]]
                    if (length(par.wk1)==1)
                        par.wk <- list(order=par.wk1,mesh=x,weight=1)
                    if (is.null(par.wk$mesh)) par.wk$mesh <- x
                    if (is.null(par.wk$weight)) par.wk$weight <- 1
                }
                if (dim(as.matrix(x))[2]!=dim(as.matrix(par.wk$mesh))[2])
                    stop("gss error in .mkterm: wrong dimension in normalizing mesh")
            }
            if (type.wk=="sphere") {
                if (length(type[[xlab]])==1)
                    par.wk <- 2
                else par.wk <- type[[xlab]][[2]]
                if (!(par.wk%in%(2:4)))
                    stop("gss error in .mkterm: spherical order not implemented")
            }
            if (type.wk=="custom") par.wk <- type[[xlab]][[2]]
        }
        else {
            ## Set default types
            if (is.factor(x)) {
                ## categorical variable
                if (is.ordered(x)) type.wk <- "ordinal"
                else type.wk <- "nominal"
                par.wk <- NULL
            }
            else {
                ## numerical variable
                if (is.vector(x)) {
                    type.wk <- "cubic"
                    mn <- min(x)
                    mx <- max(x)
                    par.wk <- c(mn,mx)+c(-1,1)*.05*(mx-mn)
                }
                else {
                    type.wk <- "tp"
                    par.wk <- list(order=2,mesh=x,weight=1)
                }
            }
        }
        var.type[[xlab]] <- list(type.wk,par.wk)
    }
    ## Create the phi and rk functions
    term <- list(labels=term.labels)
    iphi.wk <- 1
    irk.wk <- 1
    for (label in term.labels) {
        iphi <- irk <- phi <- rk <- NULL
        if (label=="1") {
            ## the constant term
            iphi <- iphi.wk
            iphi.wk <- iphi.wk + 1
            term[[label]] <- list(iphi=iphi,nphi=1,nrk=0)
            next
        }
        vlist <- xvars[as.logical(xfacs[,label])]
        x <- mf[,vlist]
        dm <- length(vlist)
        if (dm==1) {
                type.wk <- var.type[[vlist]][[1]]
                if (type.wk%in%c("nominal","ordinal")) {
                ## factor variable
                if (type.wk=="nominal") fun.env <- .mkrk.nominal(levels(x))
                else fun.env <- .mkrk.ordinal(levels(x))
                if (nlevels(x)>2) {
                    ## phi
                    nphi <- 0
                    ## rk
                    rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                        env$fun(x,y,env$env,outer.prod)
                    }
                    nrk <- 1
                    irk <- irk.wk
                    irk.wk <- irk.wk + nrk
                    rk <- list(fun=rk.fun,env=fun.env)
                }
                else {
                    ## phi
                    phi.fun <- function(x,nu=1,env) {
                        wk <- as.factor(names(env$env$code)[1])
                        env$fun(x,wk,env$env)
                    }
                    nphi <- 1
                    iphi <- iphi.wk
                    iphi.wk <- iphi.wk + nphi
                    phi <- list(fun=phi.fun,env=fun.env)
                    ## rk
                    nrk <- 0
                }
            }
            if (type.wk=="cubic") {
                ## cubic splines
                range <- var.type[[vlist]][[2]]
                ## phi
                phi.env <- .mkphi.cubic(range)
                phi.fun <- function(x,nu=1,env) env$fun(x,nu,env$env)
                nphi <- 1
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
                ## rk
                rk.env <- .mkrk.cubic(range)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk%in%c("cubic.per","linear","linear.per","sphere")) {
                ## cubic periodic, linear, and linear periodic splines
                range <- var.type[[vlist]][[2]]
                ## phi
                nphi <- 0
                ## rk
                if (type.wk=="cubic.per") rk.env <- .mkrk.cubic.per(range)
                if (type.wk=="linear") rk.env <- .mkrk.linear(range)
                if (type.wk=="linear.per") rk.env <- .mkrk.linear.per(range)
                if (type.wk=="sphere") rk.env <- .mkrk.sphere(range)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk=="tp") {
                ## thin-plate splines
                par <- var.type[[vlist]][[2]]
                order <- par$order
                mesh <- par$mesh
                weight <- par$weight
                if (is.vector(x)) xdim <- 1
                else xdim <- dim(x)[2]
                ## phi
                phi.env <- .mkphi.tp(xdim,order,mesh,weight)
                phi.fun <- function(x,nu=1,env) {
                    env$fun(x,nu,env$env)
                }
                nphi <- choose(xdim+order-1,xdim)-1
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
                ## rk
                rk.env <- .mkrk.tp(xdim,order,mesh,weight)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk=="custom") {
                ## user-defined
                par <- var.type[[vlist]][[2]]
                nphi <- par$nphi
                if (nphi>0) {
                    phi.env <- par$mkphi(par$env)
                    phi.fun <- function(x,nu=1,env) {
                        env$fun(x,nu,env$env)
                    }
                    iphi <- iphi.wk
                    iphi.wk <- iphi.wk + nphi
                    phi <- list(fun=phi.fun,env=phi.env)
                }
                rk.env <- par$mkrk(par$env)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
        }
        else {
            bin.fac <- n.phi <- phi.list <- rk.list <- NULL
            for (i in 1:dm) {
                type.wk <- var.type[[vlist[i]]][[1]]
                if (type.wk%in%c("nominal","ordinal")) {
                    ## factor variable
                    if (type.wk=="nominal")
                        rk.wk <- .mkrk.nominal(levels(x[[i]]))
                    else rk.wk <- .mkrk.ordinal(levels(x[[i]]))
                    phi.wk <- rk.wk
                    n.phi <- c(n.phi,0)
                    bin.fac <- c(bin.fac,!(nlevels(x[[i]])>2))
                }
                if (type.wk=="cubic") {
                    ## cubic splines
                    range <- var.type[[vlist[i]]][[2]]
                    ## phi
                    phi.wk <- .mkphi.cubic(range)
                    n.phi <- c(n.phi,1)
                    ## rk
                    rk.wk <- .mkrk.cubic(range)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk%in%c("cubic.per","linear","linear.per","sphere")) {
                    ## cubic periodic, linear, or linear periodic splines
                    range <- var.type[[vlist[i]]][[2]]
                    n.phi <- c(n.phi,0)
                    phi.wk <- NULL
                    if (type.wk=="cubic.per") rk.wk <- .mkrk.cubic.per(range)
                    if (type.wk=="linear") rk.wk <- .mkrk.linear(range)
                    if (type.wk=="linear.per") rk.wk <- .mkrk.linear.per(range)
                    if (type.wk=="sphere") rk.wk <- .mkrk.sphere(range)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk=="tp") {
                    ## thin-plate splines
                    par <- var.type[[vlist[i]]][[2]]
                    order <- par$order
                    mesh <- par$mesh
                    weight <- par$weight
                    if (is.vector(x[[i]])) xdim <- 1
                    else xdim <- dim(x[[i]])[2]
                    phi.wk <- .mkphi.tp(xdim,order,mesh,weight)
                    n.phi <- c(n.phi,choose(xdim+order-1,xdim)-1)
                    rk.wk <- .mkrk.tp(xdim,order,mesh,weight)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk=="custom") {
                    ## user-defined
                    par <- var.type[[vlist[i]]][[2]]
                    n.phi <- c(n.phi,par$nphi)
                    if (par$nphi>0) phi.wk <- par$mkphi(par$env)
                    else phi.wk <- NULL
                    rk.wk <- par$mkrk(par$env)
                    bin.fac <- c(bin.fac,0)
                }
                phi.list <- c(phi.list,list(phi.wk))
                rk.list <- c(rk.list,list(rk.wk))
            }
            ## phi
            if (!all(as.logical(n.phi+bin.fac))) nphi <- 0
            else {
                phi.env <- list(dim=dm,phi=phi.list,n.phi=n.phi,bin.fac=bin.fac)
                phi.fun <- function(x,nu=1,env) {
                    ind <- nu - 1
                    z <- 1
                    for (i in 1:env$dim) {
                        if (env$bin.fac[i]) {
                            wk <- as.factor(names(env$phi[[i]]$env$code)[1])
                            z <- z * env$phi[[i]]$fun(x[[i]],wk,env$phi[[i]]$env)
                        }
                        else {
                            code <- ind%%env$n.phi[i] + 1
                            ind <- ind%/%env$n.phi[i]
                            z <- z * env$phi[[i]]$fun(x[[i]],code,env$phi[[i]]$env)
                        }
                    }
                    z
                }
                nphi <- prod(n.phi+bin.fac)
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
            }
            ## rk
            rk.env <- list(dim=dm,n.phi=n.phi,nphi=nphi,
                           phi=phi.list,rk=rk.list)
            rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                n.rk <- ifelse(env$n.phi,2,1)
                ind <- nu - !env$nphi
                z <- 1
                for (i in 1:env$dim) {
                    code <- ind%%n.rk[i] + 1
                    ind <- ind%/%n.rk[i]
                    if (code==n.rk[i]) {
                        z <- z * env$rk[[i]]$fun(x[[i]],y[[i]],
                                                 env$rk[[i]]$env,outer.prod)
                    }
                    else {
                        z.wk <- 0
                        for (j in 1:env$n.phi[i]) {
                            phix <- env$phi[[i]]$fun(x[[i]],j,env$phi[[i]]$env)
                            phiy <- env$phi[[i]]$fun(y[[i]],j,env$phi[[i]]$env)
                            if (outer.prod) z.wk <- z.wk + outer(phix,phiy)
                            else z.wk <- z.wk + phix * phiy
                        }
                        z <- z * z.wk
                    }
                }
                z
            }
            n.rk <- ifelse(n.phi,2,1)
            nrk <- prod(n.rk) - as.logical(nphi)
            irk <- irk.wk
            irk.wk <- irk.wk + nrk
            rk <- list(fun=rk.fun,env=rk.env)
        }
        term[[label]] <- list(vlist=vlist,
                              iphi=iphi,nphi=nphi,phi=phi,
                              irk=irk,nrk=nrk,rk=rk)
    }
    term
}


## -----------------------------------------------------------------------------

## Fit single smoothing parameter density
.sspdsty <- function(s,r,q,cnt,qd.s,qd.r,qd.wt,prec,maxiter,alpha)
{
    nxi <- dim(r)[1]
    nobs <- dim(r)[2]
    nqd <- length(qd.wt)
    if (!is.null(s)) nnull <- dim(s)[1]
    else nnull <- 0
    nxis <- nxi+nnull
    if (is.null(cnt)) cnt <- 0
    ## cv function
    cv <- function(lambda) {
        fit <- .Fortran("dnewton",
                        cd=as.double(cd), as.integer(nxis),
                        as.double(10^(lambda+theta)*q), as.integer(nxi),
                        as.double(rbind(10^theta*r,s)), as.integer(nobs),
                        as.integer(sum(cnt)), as.integer(cnt),
                        as.double(t(rbind(10^theta*qd.r,qd.s))), as.integer(nqd),
                        as.double(qd.wt),
                        as.double(prec), as.integer(maxiter),
                        as.double(.Machine$double.eps),
                        wk=double(2*(nqd+nobs)+nxis*(nxis+4)+max(nxis,3)),
                        info=integer(1),PACKAGE="gss")
        if (fit$info==1) stop("gss error in .ssden: Newton iteration diverges")
        if (fit$info==2) warning("gss warning in .ssden: Newton iteration fails to converge")
        assign("cd",fit$cd,inherit=TRUE)
        assign("int",fit$wk[3],inherit=TRUE)
        cv <- alpha*fit$wk[2]-fit$wk[1]
        alpha.wk <- max(0,log.la0-lambda-5)*(3-alpha) + alpha
        alpha.wk <- min(alpha.wk,3)
        adj <- ifelse (alpha.wk>alpha,(alpha.wk-alpha)*fit$wk[2],0)
        cv+adj
    }
    ## initialization
    mu.r <- apply(qd.wt*t(qd.r),2,sum)/sum(qd.wt)
    v.r <- apply(qd.wt*t(qd.r^2),2,sum)/sum(qd.wt)
    mu.s <- apply(qd.wt*t(qd.s),2,sum)/sum(qd.wt)
    v.s <- apply(qd.wt*t(qd.s^2),2,sum)/sum(qd.wt)
    if (is.null(s)) theta <- 0
    else theta <- log10(sum(v.s-mu.s^2)/nnull/sum(v.r-mu.r^2)*nxi) / 2
    log.la0 <- log10(sum(v.r-mu.r^2)/sum(diag(q))) + theta
    ## lambda search
    cd <- rep(0,nxi+nnull)
    int <- NULL
    la <- log.la0
    repeat {
        mn <- la-1
        mx <- la+1
        zz <- .nlm0(cv,c(mn,mx))
        if (min(zz$est-mn,mx-zz$est)>=1e-3) break
        else la <- zz$est
    }
    ## return
    jk1 <- cv(zz$est)
    c <- cd[1:nxi]
    if (nnull) d <- cd[nxi+(1:nnull)]
    else d <- NULL
    list(lambda=zz$est,theta=theta,c=c,d=d,int=int,cv=zz$min)
}

## Fit multiple smoothing parameter density
.mspdsty <- function(s,r,q,cnt,qd.s,qd.r,qd.wt,prec,maxiter,alpha)
{
    nxi <- dim(r)[1]
    nobs <- dim(r)[2]
    nqd <- length(qd.wt)
    nq <- dim(q)[3]
    if (!is.null(s)) nnull <- dim(s)[1]
    else nnull <- 0
    nxis <- nxi+nnull
    if (is.null(cnt)) cnt <- 0
    ## cv function
    cv <- function(theta) {
        r.wk <- q.wk <- qd.r.wk <- 0
        for (i in 1:nq) {
            r.wk <- r.wk + 10^theta[i]*r[,,i]
            q.wk <- q.wk + 10^theta[i]*q[,,i]
            qd.r.wk <- qd.r.wk + 10^theta[i]*qd.r[,,i]
        }
        fit <- .Fortran("dnewton",
                        cd=as.double(cd), as.integer(nxis),
                        as.double(10^lambda*q.wk), as.integer(nxi),
                        as.double(rbind(r.wk,s)), as.integer(nobs),
                        as.integer(sum(cnt)), as.integer(cnt),
                        as.double(t(rbind(qd.r.wk,qd.s))), as.integer(nqd),
                        as.double(qd.wt),
                        as.double(prec), as.integer(maxiter),
                        as.double(.Machine$double.eps),
                        wk=double(2*(nqd+nobs)+nxis*(nxis+4)+max(nxis,3)),
                        info=integer(1),PACKAGE="gss")
        if (fit$info==1) stop("gss error in .ssden: Newton iteration diverges")
        if (fit$info==2) warning("gss warning in .ssden: Newton iteration fails to converge")
        assign("cd",fit$cd,inherit=TRUE)
        assign("int",fit$wk[3],inherit=TRUE)
        cv <- alpha*fit$wk[2]-fit$wk[1]
        alpha.wk <- max(0,theta-log.th0-5)*(3-alpha) + alpha
        alpha.wk <- min(alpha.wk,3)
        adj <- ifelse (alpha.wk>alpha,(alpha.wk-alpha)*fit$wk[2],0)
        cv+adj
    }
    cv.wk <- function(theta) cv.scale*cv(theta)+cv.shift
    ## initialization
    theta <- -log10(apply(q,3,function(x)sum(diag(x))))
    r.wk <- q.wk <- qd.r.wk <- 0
    for (i in 1:nq) {
        r.wk <- r.wk + 10^theta[i]*r[,,i]
        q.wk <- q.wk + 10^theta[i]*q[,,i]
        qd.r.wk <- qd.r.wk + 10^theta[i]*qd.r[,,i]
    }
    ## theta adjustment
    z <- .sspdsty(s,r.wk,q.wk,cnt,qd.s,qd.r.wk,qd.wt,prec,maxiter,alpha)
    theta <- theta + z$theta
    for (i in 1:nq) {
        theta[i] <- 2*theta[i] + log10(t(z$c)%*%q[,,i]%*%z$c)
        r.wk <- r.wk + 10^theta[i]*r[,,i]
        q.wk <- q.wk + 10^theta[i]*q[,,i]
        qd.r.wk <- qd.r.wk + 10^theta[i]*qd.r[,,i]
    }
    mu <- apply(qd.wt*t(qd.r.wk),2,sum)/sum(qd.wt)
    v <- apply(qd.wt*t(qd.r.wk^2),2,sum)/sum(qd.wt)
    log.la0 <- log10(sum(v-mu^2)/sum(diag(q.wk)))
    log.th0 <- theta-log.la0
    ## lambda search
    z <- .sspdsty(s,r.wk,q.wk,cnt,qd.s,qd.r.wk,qd.wt,prec,maxiter,alpha)
    lambda <- z$lambda
    log.th0 <- log.th0 + z$lambda
    theta <- theta + z$theta
    cd <- c(z$c,z$d)
    int <- z$int
    ## theta search
    counter <- 0
    ## scale and shift cv
    tmp <- abs(cv(theta))
    cv.scale <- 1
    cv.shift <- 0
    if (tmp<1&tmp>10^(-4)) {
        cv.scale <- 10/tmp
        cv.shift <- 0
    }
    if (tmp<10^(-4)) {
        cv.scale <- 10^2
        cv.shift <- 10
    }
    repeat {
        zz <- nlm(cv.wk,theta,stepmax=1,ndigit=7)
        if (zz$code<=3)  break
        theta <- zz$est        
        counter <- counter + 1
        if (counter>=5) {
            warning("gss warning in .ssden: CV iteration fails to converge")
            break
        }
    }
    ## return
    jk1 <- cv(zz$est)
    c <- cd[1:nxi]
    if (nnull) d <- cd[nxi+(1:nnull)]
    else d <- NULL
    list(lambda=lambda,theta=zz$est,c=c,d=d,int=int,cv=zz$min)
}


################################################################################


## minimization of univariate function on finite intervals
## using 3-point quadratic fit with golden-section safe-guard
.nlm0 <- function(fun,range,prec=1e-7)
{
    ratio <- 2/(sqrt(5)+1)
    ll.x <- min(range)
    uu.x <- max(range)
    if (uu.x-ll.x<prec) {
        sol <- (ll.x+uu.x)/2
        fit <- fun(sol)
        return(list(estimate=sol,minimum=fit,evaluations=1))
    }
    ml.x <- uu.x - ratio*(uu.x-ll.x)
    mu.x <- ll.x + ratio*(uu.x-ll.x)
    ## Initialization
    uu.fit <- fun(uu.x)
    mu.fit <- fun(mu.x)
    ml.fit <- fun(ml.x)
    ll.fit <- fun(ll.x)
    neval <- 4
    ## Iteration
    repeat {
        ## Fit a parabola to the 3 best points and find its minimum
        if (ll.fit<uu.fit) {
            delta.l <- ml.x-ll.x
            sigma.l <- ml.x+ll.x
            d.l <- (ml.fit-ll.fit)/delta.l
            delta.u <- mu.x-ml.x
            d.u <- (mu.fit-ml.fit)/delta.u
        }
        else {
            delta.l <- mu.x-ml.x
            sigma.l <- mu.x+ml.x
            d.l <- (mu.fit-ml.fit)/delta.l
            delta.u <- uu.x-mu.x
            d.u <- (uu.fit-mu.fit)/delta.u
        }
        a <- (d.u-d.l)/(delta.l+delta.u)
        b <- d.l-a*sigma.l
        if (a<=0) nn.x <- max(range)
        else nn.x <- -b/2/a
        ## New bracket
        if (ml.fit<mu.fit) {
            uu.x <- mu.x
            uu.fit <- mu.fit
            mm.x <- ml.x
            mm.fit <- ml.fit
        }
        else {
            ll.x <- ml.x
            ll.fit <- ml.fit
            mm.x <- mu.x
            mm.fit <- mu.fit
        }
        range.l <- mm.x-ll.x
        range.u <- uu.x-mm.x
        delta <- min(abs(nn.x-c(ll.x,mm.x,uu.x)))
        ## Safeguard
        if ((nn.x<ll.x)|(nn.x>uu.x)|(delta<prec)) {
            if (range.u>range.l) nn.x <- uu.x - ratio*range.u
            else nn.x <- ll.x + ratio*range.l
        }
        ## Update middle points
        nn.fit <- fun(nn.x)
        neval <- neval + 1
        if (nn.x<mm.x) {
            ml.x <- nn.x
            ml.fit <- nn.fit
            mu.x <- mm.x
            mu.fit <- mm.fit
        }
        else {
            ml.x <- mm.x
            ml.fit <- mm.fit
            mu.x <- nn.x
            mu.fit <- nn.fit
        }
        ## Return results
        if ((range.l+range.u<.5)&(abs(mm.x-nn.x)<sqrt(prec))) {
            if (nn.fit<mm.fit) {
                solution <- nn.x
                fit <- nn.fit
            }
            else {
                solution <- mm.x
                fit <- mm.fit
            }
            break
        }
    }
    list(estimate=solution,minimum=fit,evaluations=neval)
}


################################################################################


## Make RK for thin-plate splines
.mkrk.tp <- function(dm,order,mesh,weight=1)
{
    ## Check inputs
    if (!((2*order>dm)&(dm>=1))) {
        stop("gss error: thin-plate spline undefined for the parameters")
    }
    if (xor(is.vector(mesh),dm==1)
        |xor(is.matrix(mesh),dm>=2)) {
        stop("gss error in mkrk.tp: mismatched inputs")
    }
    if ((min(weight)<0)|(max(weight)<=0)) {
        stop("gss error in mkrk.tp: negative weights")
    }
    ## Set weights
    if (is.vector(mesh)) N <- length(mesh)
    else N <- dim(mesh)[1]
    weight <- rep(weight,len=N)
    weight <- sqrt(weight/sum(weight))
    ## Obtain orthonormal basis
    phi.p <- .mkphi.tp.p(dm,order)
    nnull <- choose(dm+order-1,dm)
    s <- NULL
    for (nu in 1:nnull) s <- cbind(s,phi.p$fun(mesh,nu,phi.p$env))
    s <- qr(weight*s)
    if (s$rank<nnull) {
        stop("gss error in mkrk.tp: insufficient normalizing mesh for thin-plate spline")
    }
    q <- qr.Q(s)
    r <- qr.R(s)
    ## Set Q^{T}E(|u_{i}-u_{j}|)Q
    rk.p <- .mkrk.tp.p(dm,order)
    pep <- weight*t(weight*rk.p$fun(mesh,mesh,rk.p$env,out=TRUE))
    pep <- t(q)%*%pep%*%q
    ## Create the environment
    env <- list(dim=dm,order=order,weight=weight,
                phi.p=phi.p,rk.p=rk.p,q=q,r=r,mesh=mesh,pep=pep)
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ## Check inputs
        if (env$dim==1) {
            if (!(is.vector(x)&is.vector(y))) {
                stop("gss error in rk: inputs are of wrong types")
            }
            nx <- length(x)
            ny <- length(y)
        }
        else {
            if (is.vector(x)) x <- t(as.matrix(x))
            if (env$dim!=dim(x)[2]) {
                stop("gss error in rk: inputs are of wrong dimensions")
            }
            nx <- dim(x)[1]
            if (is.vector(y)) y <- t(as.matrix(y))
            if (env$dim!=dim(y)[2]) {
                stop("gss error in rk: inputs are of wrong dimensions")
            }
            ny <- dim(y)[1]
        }
        ## Return the results
        nnull <- choose(env$dim+env$order-1,env$dim)
        if (outer.prod) {
            phix <- phiy <- NULL
            for (nu in 1:nnull) {
                phix <- rbind(phix,env$phi.p$fun(x,nu,env$phi.p$env))
                phiy <- rbind(phiy,env$phi.p$fun(y,nu,env$phi.p$env))
            }
            phix <- backsolve(env$r,phix,tr=TRUE)
            phiy <- backsolve(env$r,phiy,tr=TRUE)
            ex <- env$rk.p$fun(env$mesh,x,env$rk.p$env,out=TRUE)
            ex <- env$weight*ex
            ex <- t(env$q)%*%ex
            ey <- env$rk.p$fun(env$mesh,y,env$rk.p$env,out=TRUE)
            ey <- env$weight*ey
            ey <- t(env$q)%*%ey
            env$rk.p$fun(x,y,env$rk.p$env,out=TRUE)-t(phix)%*%ey-
                t(ex)%*%phiy+t(phix)%*%env$pep%*%phiy
        }
        else {
            N <- max(nx,ny)
            phix <- phiy <- NULL
            for (nu in 1:nnull) {
                phix <- rbind(phix,env$phi.p$fun(x,nu,env$phi.p$env))
                phiy <- rbind(phiy,env$phi.p$fun(y,nu,env$phi.p$env))
            }
            phix <- backsolve(env$r,phix,tr=TRUE)
            phix <- matrix(phix,nnull,N)
            phiy <- backsolve(env$r,phiy,tr=TRUE)
            phiy <- matrix(phiy,nnull,N)
            ex <- env$rk.p$fun(env$mesh,x,env$rk.p$env,out=TRUE)
            ex <- env$weight*ex
            ex <- t(env$q)%*%ex
            ex <- matrix(ex,nnull,N)
            ey <- env$rk.p$fun(env$mesh,y,env$rk.p$env,out=TRUE)
            ey <- env$weight*ey
            ey <- t(env$q)%*%ey
            ey <- matrix(ey,nnull,N)
            fn1 <- function(x,n) x[1:n]%*%x[n+(1:n)]
            fn2 <- function(x,pep,n) t(x[1:n])%*%pep%*%x[n+(1:n)]
            env$rk.p$fun(x,y,env$rk.p$env)-apply(rbind(phix,ey),2,fn1,nnull)-
                apply(rbind(phiy,ex),2,fn1,nnull)+
                    apply(rbind(phix,phiy),2,fn2,env$pep,nnull)
        }
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make phi function for thin-plate splines
.mkphi.tp <-  function(dm,order,mesh,weight)
{
    ## Check inputs
    if (!((2*order>dm)&(dm>=1))) {
        stop("gss error: thin-plate spline undefined for the parameters")
    }
    if (xor(is.vector(mesh),dm==1)
        |xor(is.matrix(mesh),dm>=2)) {
        stop("gss error in mkphi.tp: mismatched inputs")
    }
    if ((min(weight)<0)|(max(weight)<=0)) {
        stop("gss error in mkphi.tp: negative weights")
    }
    ## Set weights
    if (is.vector(mesh)) N <- length(mesh)
    else N <- dim(mesh)[1]
    weight <- rep(weight,len=N)
    weight <- sqrt(weight/sum(weight))
    ## Create the environment
    phi.p <- .mkphi.tp.p(dm,order)
    nnull <- choose(dm+order-1,dm)
    s <- NULL
    for (nu in 1:nnull) s <- cbind(s,phi.p$fun(mesh,nu,phi.p$env))
    s <- qr(weight*s)
    if (s$rank<nnull) {
        stop("gss error in mkphi: insufficient normalizing mesh for thin-plate spline")
    }
    r <- qr.R(s)
    env <- list(dim=dm,order=order,phi.p=phi.p,r=r)
    ## Create the phi function
    fun <- function(x,nu,env) {
        nnull <- choose(env$dim+env$order-1,env$dim)
        phix <- NULL
        for(i in 1:nnull)
            phix <- rbind(phix,env$phi.p$fun(x,i,env$phi.p$env))
        t(backsolve(env$r,phix,tr=TRUE))[,nu+1]
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make pseudo RK for thin-plate splines
.mkrk.tp.p <- function(dm,order)
{
    ## Check inputs
    if (!((2*order>dm)&(dm>=1))) {
        stop("gss error: thin-plate spline undefined for the parameters")
    }
    ## Create the environment
    if (dm%%2) {                    
        theta <- gamma(dm/2-order)/2^(2*order)/pi^(dm/2)/gamma(order)
    }
    else {
        theta <- (-1)^(dm/2+order+1)/2^(2*order-1)/pi^(dm/2)/
            gamma(order)/gamma(order-dm/2+1)
    }
    env <- list(dim=dm,order=order,theta=theta)
    ## Create the rk.p function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ## Check inputs
        if (env$dim==1) {
            if (!(is.vector(x)&is.vector(y))) {
                stop("gss error in rk: inputs are of wrong types")
            }
        }
        else {
            if (is.vector(x)) x <- t(as.matrix(x))
            if (env$dim!=dim(x)[2]) {
                stop("gss error in rk: inputs are of wrong dimensions")
            }
            if (is.vector(y)) y <- t(as.matrix(y))
            if (env$dim!=dim(y)[2]) {
                stop("gss error in rk: inputs are of wrong dimensions")
            }
        }
        ## Return the results
        if (outer.prod) {               
            if (env$dim==1) {
                fn1 <- function(x,y) abs(x-y)
                d <- outer(x,y,fn1)
            }
            else {
                fn2 <- function(x,y) sqrt(sum((x-y)^2))
                d <- NULL
                for (i in 1:dim(y)[1]) d <- cbind(d,apply(x,1,fn2,y[i,]))
            }
        }
        else {
            if (env$dim==1) d <- abs(x-y)
            else {
                N <- max(dim(x)[1],dim(y)[1])
                x <- t(matrix(t(x),env$dim,N))
                y <- t(matrix(t(y),env$dim,N))
                fn <- function(x) sqrt(sum(x^2))
                d <- apply(x-y,1,fn)
            }
        }
        power <- 2*env$order-env$dim
        switch(1+env$dim%%2,
               env$theta*d^power*log(ifelse(d>0,d,1)),
               env$theta*d^power)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make pseudo phi function for thin-plate splines
.mkphi.tp.p <- function(dm,order)
{
    ## Check inputs
    if (!((2*order>dm)&(dm>=1))) {
        stop("gss error: thin-plate spline undefined for the parameters")
    }
    ## Create the environment
    pol.code <- NULL
    for (i in 0:(order^dm-1)) {
        ind <- i; code <- NULL
        for (j in 1:dm) {
            code <- c(code,ind%%order)
            ind <- ind%/%order
        }
        if (sum(code)<order) pol.code <- cbind(pol.code,code)
    }
    env <- list(dim=dm,pol.code=pol.code)
    ## Create the phi function  
    fun <- function(x,nu,env) {
        if (env$dim==1) x <- as.matrix(x)
        if (env$dim!=dim(x)[2]) {
            stop("gss error in phi: inputs are of wrong dimensions")
        }
        apply(t(x)^env$pol.code[,nu],2,prod)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make RK for spherical splines
.mkrk.sphere <- function(order)
{
    ## Create the environment
    env <- list(order=order)
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ##% Check the inputs
        if (is.vector(x)) x <- t(as.matrix(x))
        if (is.vector(y)) y <- t(as.matrix(y))
        if (!(is.matrix(x)&is.matrix(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        if ((dim(x)[2]!=2)|(dim(y)[2]!=2)) {
            stop("gss error in rk: inputs are of wrong dimensions")
        }
        if ((max(abs(x[,1]),abs(y[1,]))>90)|(max(abs(x[,2]),abs(y[,2]))>180)) {
            stop("gss error in rk: inputs are out of range")
        }
        ##% Convert to gradient
        lat.x <- x[,1]/180*pi; lon.x <- x[,2]/180*pi
        lat.y <- y[,1]/180*pi; lon.y <- y[,2]/180*pi
        ##% Return the result
        rk <- function(lat.x,lon.x,lat.y,lon.y,order) {
            z <- cos(lat.x)*cos(lat.y)*cos(lon.x-lon.y)+sin(lat.x)*sin(lat.y)
            W <- ifelse(z<1-10^(-10),(1-z)/2,0)
            A <- ifelse(W>0,log(1+1/sqrt(W)),0)
            C <- ifelse(W>0,2*sqrt(W),0)
            switch(order-1,
                   (A*4*W*(3*W-1)+6*W*(1-C)+1)/2,
                   (W*W*(A*((840*W-720)*W+72)+420*W*(1-C)+220*C-150)-4*W+3)/12,
                   (W*W*W*(A*(((27720*W-37800)*W+12600)*W-600)+
                           (13860*(1-C)*W+14280*C-11970)*W-2772*C+1470)+
                    15*W*W-3*W+5)/30) - 1/(2*order-1)
        }
        if (outer.prod) {
            zz <- NULL
            for (i in 1:length(lat.y))
                zz <- cbind(zz,rk(lat.x,lon.x,lat.y[i],lon.y[i],env$order))
        }
        else zz <- rk(lat.x,lon.x,lat.y,lon.y,env$order)
        zz
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}


################################################################################


## Make phi and rk for model terms
.mkterm <- function(mf,type)
{
    ## Obtain model terms
    mt <- attr(mf,"terms")
    xvars <- as.character(attr(mt,"variables"))[-1]
    xfacs <- attr(mt,"factors")
    term.labels <- labels(mt)
    if (attr(attr(mf,"terms"),"intercept"))
        term.labels <- c("1",term.labels)
    ## Backward compatibility
    vlist <- xvars[as.logical(apply(xfacs,1,sum))]
    if (!is.null(type)&&!is.list(type)&&(type%in%c("cubic","linear","tp"))) {
        type.wk <- type
        type <- NULL
        for (xlab in vlist) type[[xlab]] <- type.wk
    }
    ## Set types for marginals    
    var.type <- NULL
    for (xlab in vlist) {
        x <- mf[,xlab]
        if (!is.null(type[[xlab]])) {
            ## Check consistency and set default parameters
            type.wk <- type[[xlab]][[1]]
            if
            (!(type.wk%in%c("ordinal","nominal","cubic","linear","per",
                            "cubic.per","linear.per","tp","sphere","custom")))
                stop("gss error in mkterm: unknown type")
            if (type.wk%in%c("ordinal","nominal")) {
                par.wk <- NULL
                if (!is.factor(x))
                    stop("gss error in mkterm: wrong type")
            }
            if (type.wk%in%c("cubic","linear")) {
                if (length(type[[xlab]])==1) {
                    mn <- min(x)
                    mx <- max(x)
                    par.wk <- c(mn,mx)+c(-1,1)*.05*(mx-mn)
                }
                else par.wk <- type[[xlab]][[2]]
                if (is.factor(x)|!is.vector(x))
                    stop("gss error in mkterm: wrong type")
            }
            if (type.wk%in%c("per","cubic.per","linear.per")) {
                if (type.wk=="per") type.wk <- "cubic.per"
                if (length(type[[xlab]])==1)
                    stop("gss error in mkterm: missing domain of periodicity")
                else par.wk <- type[[xlab]][[2]]
                if (is.factor(x)|!is.vector(x))
                    stop("gss error in mkterm: wrong type")
            }
            if (type.wk=="tp") {
                if (length(type[[xlab]])==1)
                    par.wk <- list(order=2,mesh=x,weight=1)
                else {
                    par.wk <- par.wk1 <- type[[xlab]][[2]]
                    if (length(par.wk1)==1)
                        par.wk <- list(order=par.wk1,mesh=x,weight=1)
                    if (is.null(par.wk$mesh)) par.wk$mesh <- x
                    if (is.null(par.wk$weight)) par.wk$weight <- 1
                }
                if (dim(as.matrix(x))[2]!=dim(as.matrix(par.wk$mesh))[2])
                    stop("gss error in mkterm: wrong dimension in normalizing mesh")
            }
            if (type.wk=="sphere") {
                if (length(type[[xlab]])==1)
                    par.wk <- 2
                else par.wk <- type[[xlab]][[2]]
                if (!(par.wk%in%(2:4)))
                    stop("gss error in mkterm: spherical order not implemented")
            }
            if (type.wk=="custom") par.wk <- type[[xlab]][[2]]
        }
        else {
            ## Set default types
            if (is.factor(x)) {
                ## categorical variable
                if (is.ordered(x)) type.wk <- "ordinal"
                else type.wk <- "nominal"
                par.wk <- NULL
            }
            else {
                ## numerical variable
                if (is.vector(x)) {
                    type.wk <- "cubic"
                    mn <- min(x)
                    mx <- max(x)
                    par.wk <- c(mn,mx)+c(-1,1)*.05*(mx-mn)
                }
                else {
                    type.wk <- "tp"
                    par.wk <- list(order=2,mesh=x,weight=1)
                }
            }
        }
        var.type[[xlab]] <- list(type.wk,par.wk)
    }
    ## Create the phi and rk functions
    term <- list(labels=term.labels)
    iphi.wk <- 1
    irk.wk <- 1
    for (label in term.labels) {
        iphi <- irk <- phi <- rk <- NULL
        if (label=="1") {
            ## the constant term
            iphi <- iphi.wk
            iphi.wk <- iphi.wk + 1
            term[[label]] <- list(iphi=iphi,nphi=1,nrk=0)
            next
        }
        vlist <- xvars[as.logical(xfacs[,label])]
        x <- mf[,vlist]
        dm <- length(vlist)
        if (dm==1) {
                type.wk <- var.type[[vlist]][[1]]
                if (type.wk%in%c("nominal","ordinal")) {
                ## factor variable
                if (type.wk=="nominal") fun.env <- .mkrk.nominal(levels(x))
                else fun.env <- .mkrk.ordinal(levels(x))
                if (nlevels(x)>2) {
                    ## phi
                    nphi <- 0
                    ## rk
                    rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                        env$fun(x,y,env$env,outer.prod)
                    }
                    nrk <- 1
                    irk <- irk.wk
                    irk.wk <- irk.wk + nrk
                    rk <- list(fun=rk.fun,env=fun.env)
                }
                else {
                    ## phi
                    phi.fun <- function(x,nu=1,env) {
                        wk <- as.factor(names(env$env$code)[1])
                        env$fun(x,wk,env$env)
                    }
                    nphi <- 1
                    iphi <- iphi.wk
                    iphi.wk <- iphi.wk + nphi
                    phi <- list(fun=phi.fun,env=fun.env)
                    ## rk
                    nrk <- 0
                }
            }
            if (type.wk=="cubic") {
                ## cubic splines
                range <- var.type[[vlist]][[2]]
                ## phi
                phi.env <- .mkphi.cubic(range)
                phi.fun <- function(x,nu=1,env) env$fun(x,nu,env$env)
                nphi <- 1
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
                ## rk
                rk.env <- .mkrk.cubic(range)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk%in%c("cubic.per","linear","linear.per","sphere")) {
                ## cubic periodic, linear, and linear periodic splines
                range <- var.type[[vlist]][[2]]
                ## phi
                nphi <- 0
                ## rk
                if (type.wk=="cubic.per") rk.env <- .mkrk.cubic.per(range)
                if (type.wk=="linear") rk.env <- .mkrk.linear(range)
                if (type.wk=="linear.per") rk.env <- .mkrk.linear.per(range)
                if (type.wk=="sphere") rk.env <- .mkrk.sphere(range)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk=="tp") {
                ## thin-plate splines
                par <- var.type[[vlist]][[2]]
                order <- par$order
                mesh <- par$mesh
                weight <- par$weight
                if (is.vector(x)) xdim <- 1
                else xdim <- dim(x)[2]
                ## phi
                phi.env <- .mkphi.tp(xdim,order,mesh,weight)
                phi.fun <- function(x,nu=1,env) {
                    env$fun(x,nu,env$env)
                }
                nphi <- choose(xdim+order-1,xdim)-1
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
                ## rk
                rk.env <- .mkrk.tp(xdim,order,mesh,weight)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
            if (type.wk=="custom") {
                ## user-defined
                par <- var.type[[vlist]][[2]]
                nphi <- par$nphi
                if (nphi>0) {
                    phi.env <- par$mkphi(par$env)
                    phi.fun <- function(x,nu=1,env) {
                        env$fun(x,nu,env$env)
                    }
                    iphi <- iphi.wk
                    iphi.wk <- iphi.wk + nphi
                    phi <- list(fun=phi.fun,env=phi.env)
                }
                rk.env <- par$mkrk(par$env)
                rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                    env$fun(x,y,env$env,outer.prod)
                }
                nrk <- 1
                irk <- irk.wk
                irk.wk <- irk.wk + nrk
                rk <- list(fun=rk.fun,env=rk.env)
            }
        }
        else {
            bin.fac <- n.phi <- phi.list <- rk.list <- NULL
            for (i in 1:dm) {
                type.wk <- var.type[[vlist[i]]][[1]]
                if (type.wk%in%c("nominal","ordinal")) {
                    ## factor variable
                    if (type.wk=="nominal")
                        rk.wk <- .mkrk.nominal(levels(x[[i]]))
                    else rk.wk <- .mkrk.ordinal(levels(x[[i]]))
                    phi.wk <- rk.wk
                    n.phi <- c(n.phi,0)
                    bin.fac <- c(bin.fac,!(nlevels(x[[i]])>2))
                }
                if (type.wk=="cubic") {
                    ## cubic splines
                    range <- var.type[[vlist[i]]][[2]]
                    ## phi
                    phi.wk <- .mkphi.cubic(range)
                    n.phi <- c(n.phi,1)
                    ## rk
                    rk.wk <- .mkrk.cubic(range)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk%in%c("cubic.per","linear","linear.per","sphere")) {
                    ## cubic periodic, linear, or linear periodic splines
                    range <- var.type[[vlist[i]]][[2]]
                    n.phi <- c(n.phi,0)
                    phi.wk <- NULL
                    if (type.wk=="cubic.per") rk.wk <- .mkrk.cubic.per(range)
                    if (type.wk=="linear") rk.wk <- .mkrk.linear(range)
                    if (type.wk=="linear.per") rk.wk <- .mkrk.linear.per(range)
                    if (type.wk=="sphere") rk.wk <- .mkrk.sphere(range)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk=="tp") {
                    ## thin-plate splines
                    par <- var.type[[vlist[i]]][[2]]
                    order <- par$order
                    mesh <- par$mesh
                    weight <- par$weight
                    if (is.vector(x[[i]])) xdim <- 1
                    else xdim <- dim(x[[i]])[2]
                    phi.wk <- .mkphi.tp(xdim,order,mesh,weight)
                    n.phi <- c(n.phi,choose(xdim+order-1,xdim)-1)
                    rk.wk <- .mkrk.tp(xdim,order,mesh,weight)
                    bin.fac <- c(bin.fac,0)
                }
                if (type.wk=="custom") {
                    ## user-defined
                    par <- var.type[[vlist[i]]][[2]]
                    n.phi <- c(n.phi,par$nphi)
                    if (par$nphi>0) phi.wk <- par$mkphi(par$env)
                    else phi.wk <- NULL
                    rk.wk <- par$mkrk(par$env)
                    bin.fac <- c(bin.fac,0)
                }
                phi.list <- c(phi.list,list(phi.wk))
                rk.list <- c(rk.list,list(rk.wk))
            }
            ## phi
            if (!all(as.logical(n.phi+bin.fac))) nphi <- 0
            else {
                phi.env <- list(dim=dm,phi=phi.list,n.phi=n.phi,bin.fac=bin.fac)
                phi.fun <- function(x,nu=1,env) {
                    ind <- nu - 1
                    z <- 1
                    for (i in 1:env$dim) {
                        if (env$bin.fac[i]) {
                            wk <- as.factor(names(env$phi[[i]]$env$code)[1])
                            z <- z * env$phi[[i]]$fun(x[[i]],wk,env$phi[[i]]$env)
                        }
                        else {
                            code <- ind%%env$n.phi[i] + 1
                            ind <- ind%/%env$n.phi[i]
                            z <- z * env$phi[[i]]$fun(x[[i]],code,env$phi[[i]]$env)
                        }
                    }
                    z
                }
                nphi <- prod(n.phi+bin.fac)
                iphi <- iphi.wk
                iphi.wk <- iphi.wk + nphi
                phi <- list(fun=phi.fun,env=phi.env)
            }
            ## rk
            rk.env <- list(dim=dm,n.phi=n.phi,nphi=nphi,
                           phi=phi.list,rk=rk.list)
            rk.fun <- function(x,y,nu=1,env,outer.prod=FALSE) {
                n.rk <- ifelse(env$n.phi,2,1)
                ind <- nu - !env$nphi
                z <- 1
                for (i in 1:env$dim) {
                    code <- ind%%n.rk[i] + 1
                    ind <- ind%/%n.rk[i]
                    if (code==n.rk[i]) {
                        z <- z * env$rk[[i]]$fun(x[[i]],y[[i]],
                                                 env$rk[[i]]$env,outer.prod)
                    }
                    else {
                        z.wk <- 0
                        for (j in 1:env$n.phi[i]) {
                            phix <- env$phi[[i]]$fun(x[[i]],j,env$phi[[i]]$env)
                            phiy <- env$phi[[i]]$fun(y[[i]],j,env$phi[[i]]$env)
                            if (outer.prod) z.wk <- z.wk + outer(phix,phiy)
                            else z.wk <- z.wk + phix * phiy
                        }
                        z <- z * z.wk
                    }
                }
                z
            }
            n.rk <- ifelse(n.phi,2,1)
            nrk <- prod(n.rk) - as.logical(nphi)
            irk <- irk.wk
            irk.wk <- irk.wk + nrk
            rk <- list(fun=rk.fun,env=rk.env)
        }
        term[[label]] <- list(vlist=vlist,
                              iphi=iphi,nphi=nphi,phi=phi,
                              irk=irk,nrk=nrk,rk=rk)
    }
    term
}


################################################################################


## Make RK for nominal shrinkage
.mkrk.nominal <- function(levels)
{
    k <- length(levels)
    if (k<2) stop("gss error: factor should have at least two levels")
    code <- 1:k
    names(code) <- as.character(levels)
    ## Create the environment
    env <- list(code=code,table=diag(k)-1/k)
    ## Create the rk function
    fun <- function(x, y, env, outer.prod = FALSE) {
        if (!(is.factor(x)&is.factor(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        x <- as.numeric(env$code[as.character(x)])
        y <- as.numeric(env$code[as.character(y)])
        if (any(is.na(c(x,y)))) {
            stop("gss error in rk: unknown factor levels")
        }
        if (outer.prod) env$table[x, y]
        else env$table[cbind(x,y)]
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make RK for ordinal shrinkage
.mkrk.ordinal <- function(levels)
{
    k <- length(levels)
    if (k<2) stop("gss error: factor should have at least two levels")
    code <- 1:k
    names(code) <- as.character(levels)
    ## penalty matrix
    if (k==2) {
        B <- diag(.25,2)
        B[1,2] <- B[2,1] <- -.25
    }
    else {
        B <- diag(2,k)
        B[1,1] <- B[k,k] <- 1
        diag(B[-1,-k]) <- diag(B[-k,-1]) <- -1
        ## Moore-Penrose inverse
        B <- eigen(B)
        B <- B$vec[,-k] %*% diag(1/B$val[-k]) %*% t(B$vec[,-k])
        tol <- sqrt(.Machine$double.eps)
        B <- ifelse(abs(B)<tol,0,B)
    }
    ## Create the environment
    env <- list(code=code,table=B)
    ## Create the rk function
    fun <- function(x, y, env, outer.prod = FALSE) {
        if (!(is.factor(x)&is.factor(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        x <- as.numeric(env$code[as.character(x)])
        y <- as.numeric(env$code[as.character(y)])
        if (any(is.na(c(x,y)))) {
            stop("gss error in rk: unknown factor levels")
        }
        if (outer.prod) env$table[x, y]
        else env$table[cbind(x,y)]
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}


################################################################################


## Make RK for cubic splines
.mkrk.cubic <- function(range)
{
    ## Create the environment
    env <- list(min=min(range), max=max(range))
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ##% Check the inputs
        if (!(is.vector(x)&is.vector(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        if ((min(x,y)<env$min)|(max(x,y)>env$max)) {
            stop("gss error in rk: inputs are out of range")
        }
        ##% Scale the inputs
        x <- (x-env$min)/(env$max-env$min)
        y <- (y-env$min)/(env$max-env$min)
        ##% Return the result
        rk <- function(x,y) {
            k2 <- function(x) ((x-.5)^2-1/12)/2
            k4 <- function(x) ((x-.5)^4-(x-.5)^2/2+7/240)/24
            k2(x)*k2(y)-k4(abs(x-y))
        }
        if (outer.prod) outer(x,y,rk)
        else rk(x,y)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make phi function for cubic splines
.mkphi.cubic <- function(range)
{
    ## Create the environment
    env <- list(min=min(range), max=max(range))
    ## Create the phi function
    fun <- function(x,nu,env) {
        ##% Check the input
        if (!is.vector(x)) {
            stop("gss error in phi: inputs are of wrong types")
        }
        if ((min(x)<env$min)|(max(x)>env$max)) {
            stop("gss error in phi: inputs are out of range")
        }
        ##% Return the result
        (x-env$min)/(env$max-env$min)-.5
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make RK for periodic cubic splines
.mkrk.cubic.per <- function(range)
{
    ## Create the environment
    env <- list(min=min(range), max=max(range))
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ##% Check the inputs
        if (!(is.vector(x)&is.vector(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        if ((min(x,y)<env$min)|(max(x,y)>env$max)) {
            stop("gss error in rk: inputs are out of range")
        }
        ##% Scale the inputs
        x <- (x-env$min)/(env$max-env$min)
        y <- (y-env$min)/(env$max-env$min)
        ##% Return the result
        rk <- function(x,y) {
            k4 <- function(x) ((x-.5)^4-(x-.5)^2/2+7/240)/24
            -k4(abs(x-y))
        }
        if (outer.prod) outer(x,y,rk)
        else rk(x,y)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make RK for linear splines
.mkrk.linear <- function(range)
{
    ## Create the environment
    env <- list(min=min(range), max=max(range))
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ##% Check the inputs
        if (!(is.vector(x)&is.vector(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        if ((min(x,y)<env$min)|(max(x,y)>env$max)) {
            stop("gss error in rk: inputs are out of range")
        }
        ##% Scale the inputs
        x <- (x-env$min)/(env$max-env$min)
        y <- (y-env$min)/(env$max-env$min)
        ##% Return the result
        rk <- function(x,y) {
            k1 <- function(x) (x-.5)
            k2 <- function(x) ((x-.5)^2-1/12)/2
            k1(x)*k1(y)+k2(abs(x-y))
        }
        if (outer.prod) outer(x,y,rk)
        else rk(x,y)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}

## Make RK for periodic linear splines
.mkrk.linear.per <- function(range)
{
    ## Create the environment
    env <- list(min=min(range), max=max(range))
    ## Create the rk function
    fun <- function(x,y,env,outer.prod=FALSE) {
        ##% Check the inputs
        if (!(is.vector(x)&is.vector(y))) {
            stop("gss error in rk: inputs are of wrong types")
        }
        if ((min(x,y)<env$min)|(max(x,y)>env$max)) {
            stop("gss error in rk: inputs are out of range")
        }
        ##% Scale the inputs
        x <- (x-env$min)/(env$max-env$min)
        y <- (y-env$min)/(env$max-env$min)
        ##% Return the result
        rk <- function(x,y) {
            k2 <- function(x) ((x-.5)^2-1/12)/2
            k2(abs(x-y))
        }
        if (outer.prod) outer(x,y,rk)
        else rk(x,y)
    }
    ## Return the function and the environment
    list(fun=fun,env=env)
}


################################################################################

