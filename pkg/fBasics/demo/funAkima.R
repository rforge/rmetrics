

# Package: akima
# Version: 0.3-4
# Title: Interpolation of irregularly spaced data
# Author: Fortran code by H. Akima
#   R port by Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
# Maintainer: Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
# Description: Linear or cubic spline interpolation for irregular gridded data
# License: Fortran code: ACM, free for non-commercial use, R functions GPL


# ------------------------------------------------------------------------------


"interp"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                   yo = seq(min(y), max(y), length = 40), 
                   ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
 if (ncp==0)
   # use the old version for linear interpolation
   interp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
 else
   interp.new(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
}
"interp.new"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                       yo = seq(min(y), max(y), length = 40), linear=FALSE,
                       ncp = NULL, extrap = FALSE, duplicate = "error", 
                       dupfun = NULL)
{
  if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
    stop("missing values and Infs not allowed")
  if(!is.null(ncp)){
    if(ncp!=0){
      cat("ncp not supported, it is automatically choosen by Fortran code\n")
    }
    else {
      cat("linear interpolation not yet implemented with interp.new().\n")
      stop("use interp.old().")
    }
  }
  if(linear){
      cat("linear interpolation not yet implemented with interp.new().\n")
      stop("use interp.old().")
  }

  drx <- diff(range(x))
  dry <- diff(range(y))
  if(drx == 0 || dry == 0)
    stop("all data collinear")	# other cases caught in Fortran code
  if(drx/dry > 10000 || drx/dry < 0.0001)
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  nx <- length(xo)
  ny <- length(yo)
  if(length(y) != n || length(z) != n)
    stop("Lengths of x, y, and z do not match")
  xy <- paste(x, y, sep =",")
  i <- match(xy, xy)
  if(duplicate=="user" && !is.function(dupfun))
    stop("duplicate=\"user\" requires dupfun to be set to a function")
  if(duplicate!="error")
    {
      centre <- function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x))
      }
      if(duplicate!="strip"){
        z <- unlist(lapply(split(z,i), centre))
        ord <- !duplicated(xy)
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
      }
      else{
        ord <- (hist(i,plot=FALSE,freq=TRUE,breaks=seq(0.5,max(i)+0.5,1))$counts==1)
        x <- x[ord]
        y <- y[ord]
        z <- z[ord]
        n <- length(x)
      }
    }
  else
    if(any(duplicated(xy)))
      stop("duplicate data points")
  zo <- matrix(0, nx, ny)
  storage.mode(zo) <- "double"
  miss <- !extrap	#if not extrapolating use missing values
  extrap <- matrix(TRUE, nx, ny)
  if(!is.null(ncp)){
    if(extrap & ncp == 0)
      warning("Cannot extrapolate with linear option")
  }
  else {
    if(extrap & linear)
      warning("Cannot extrapolate with linear option")
  }
  ans <- .Fortran("sdsf3p",
                  as.integer(1),
                  as.integer(n),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(nx),
                  x = as.double(xo),
                  as.integer(ny),
                  y = as.double(yo),
                  z = zo,
                  ier = integer(1),
                  double(36 * n),
                  integer(25 * n),
                  extrap = as.logical(extrap),
                  near = integer(n),
                  nxt = integer(n),
                  dist = double(n),
                  PACKAGE = "akima")
  temp <- ans[c("x", "y", "z", "extrap")]
  if(miss)
    temp$z[temp$extrap]<-NA
  temp[c("x", "y", "z")]
}
"interp.old"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                   yo = seq(min(y), max(y), length = 40), 
                   ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
  if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
    stop("missing values and Infs not allowed")
  if(ncp>25){
    ncp <- 25
    cat("ncp too large, using ncp=25\n")
  }
  drx <- diff(range(x))
  dry <- diff(range(y))
  if(drx == 0 || dry == 0)
    stop("all data collinear")	# other cases caught in Fortran code
  if(drx/dry > 10000 || drx/dry < 0.0001)
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  nx <- length(xo)
  ny <- length(yo)
  if(length(y) != n || length(z) != n)
    stop("Lengths of x, y, and z do not match")
  xy <- paste(x, y, sep =",")
  i <- match(xy, xy)
  if(duplicate=="user" && !is.function(dupfun))
    stop("duplicate=\"user\" requires dupfun to be set to a function")
  if(duplicate!="error")
    {
      centre <- function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x))
      }
      if(duplicate!="strip"){
        z <- unlist(lapply(split(z,i), centre))
        ord <- !duplicated(xy)
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
      }
      else{
        ord <- (hist(i,plot=FALSE,freq=TRUE,breaks=seq(0.5,max(i)+0.5,1))$counts==1)
        x <- x[ord]
        y <- y[ord]
        z <- z[ord]
        n <- length(x)
      }
    }
  else
    if(any(duplicated(xy)))
      stop("duplicate data points")
  zo <- matrix(0, nx, ny)
  storage.mode(zo) <- "double"
  miss <- !extrap	#if not extrapolating use missing values
  misso <- matrix(miss, nx, ny)
  if(extrap & ncp == 0)
    warning("Cannot extrapolate with linear option")
  ans <- .Fortran("idsfft",
                  as.integer(1),
                  as.integer(ncp),
                  as.integer(n),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(nx),
                  as.integer(ny),
                  x = as.double(xo),
                  y = as.double(yo),
                  z = zo,
                  integer((31 + ncp) * n + nx * ny),
                  double(5 * n),
                  misso = as.logical(misso),
                  PACKAGE = "akima")
  temp <- ans[c("x", "y", "z", "misso")]
  temp$z[temp$misso]<-NA
  temp[c("x", "y", "z")]
}
"interpp"<-function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
  # interpp.new has some bugs at the moment (segfaults), so use
  # the old Akima code:
  interpp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
}
"interpp.new"<-function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
  if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
    stop("missing values and Infs not allowed")
  if(is.null(xo))
    stop("xo missing")
  if(is.null(yo))
    stop("yo missing")
  if(ncp>25){
    ncp <- 25
    cat("ncp too large, using ncp=25\n")
  }
  drx <- diff(range(x))
  dry <- diff(range(y))
  if(drx == 0 || dry == 0)
    stop("all data collinear")	# other cases caught in Fortran code
  if(drx/dry > 10000 || drx/dry < 0.0001)
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  np <- length(xo)
  if(length(yo)!=np)
    stop("length of xo and yo differ")
  if(length(y) != n || length(z) != n)
    stop("Lengths of x, y, and z do not match")
  xy <- paste(x, y, sep =",")
  i <- match(xy, xy)
  if(duplicate=="user" && !is.function(dupfun))
    stop("duplicate=\"user\" requires dupfun to be set to a function")
  if(duplicate!="error")
    {
      centre <- function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x)
               )
      }
      if(duplicate!="strip"){
        z <- unlist(lapply(split(z,i), centre))
        ord <- !duplicated(xy)
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
      }
      else{
        ord <- (hist(i,plot=FALSE,freq=TRUE,breaks=seq(0.5,max(i)+0.5,1))$counts==1)
        x <- x[ord]
        y <- y[ord]
        z <- z[ord]
        n <- length(x)
      }
    }
  else
    if(any(duplicated(xy)))
      stop("duplicate data points")
  zo <- rep(0, np)
  storage.mode(zo) <- "double"
  miss <- !extrap	#if not extrapolating use missing values
  extrap <- seq(TRUE, np)
  if(extrap & ncp == 0)
    warning("Cannot extrapolate with linear option")
  ans <- .Fortran("sdbi3p",
                  as.integer(1),
#                  as.integer(ncp),
                  as.integer(n),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(np),
                  x = as.double(xo),
                  y = as.double(yo),
                  z = zo,
                  double(17 * n),
                  integer(25 * n),
                  extrap = as.logical(extrap),
                  near = integer(n),
                  net = integer(n),
                  dist = double(n),
                  PACKAGE = "akima")
  temp <- ans[c("x", "y", "z", "extrap")]
  if(miss)
    temp$z[temp$extrap]<-NA
  temp[c("x", "y", "z")]
}
"interpp.old"<-function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
  if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
    stop("missing values and Infs not allowed")
  if(is.null(xo))
    stop("xo missing")
  if(is.null(yo))
    stop("yo missing")
  if(ncp>25){
    ncp <- 25
    cat("ncp too large, using ncp=25\n")
  }
  drx <- diff(range(x))
  dry <- diff(range(y))
  if(drx == 0 || dry == 0)
    stop("all data collinear")	# other cases caught in Fortran code
  if(drx/dry > 10000 || drx/dry < 0.0001)
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  np <- length(xo)
  if(length(yo)!=np)
    stop("length of xo and yo differ")
  if(length(y) != n || length(z) != n)
    stop("Lengths of x, y, and z do not match")
  xy <- paste(x, y, sep =",")
  i <- match(xy, xy)
  if(duplicate=="user" && !is.function(dupfun))
    stop("duplicate=\"user\" requires dupfun to be set to a function")
  if(duplicate!="error")
    {
      centre <- function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x)
               )
      }
      if(duplicate!="strip"){
        z <- unlist(lapply(split(z,i), centre))
        ord <- !duplicated(xy)
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
      }
      else{
        ord <- (hist(i,plot=FALSE,freq=TRUE,breaks=seq(0.5,max(i)+0.5,1))$counts==1)
        x <- x[ord]
        y <- y[ord]
        z <- z[ord]
        n <- length(x)
      }
    }
  else
    if(any(duplicated(xy)))
      stop("duplicate data points")
  zo <- rep(0, np)
  storage.mode(zo) <- "double"
  miss <- !extrap	#if not extrapolating use missing values
  misso <- seq(miss, np)
  if(extrap & ncp == 0)
    warning("Cannot extrapolate with linear option")
  ans <- .Fortran("idbvip",
                  as.integer(1),
                  as.integer(ncp),
                  as.integer(n),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(np),
                  x = as.double(xo),
                  y = as.double(yo),
                  z = zo,
                  integer((31 + ncp) * n + np),
                  double(8 * n),
                  misso = as.logical(misso),
                  PACKAGE = "akima")
  temp <- ans[c("x", "y", "z", "misso")]
  temp$z[temp$misso]<-NA
  temp[c("x", "y", "z")]
}
.First.lib <- function(lib, pkg) {
  if(version$major==0 && version$minor < 62)
    stop("This version for R 0.62 or later")
  library.dynam("akima", pkg, lib)
}

