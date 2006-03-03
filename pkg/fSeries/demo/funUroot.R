
# Function Builtin: uroot [No Gui]
#   This function Builtin will be used when the original package 'uroot'
#   is not available for your hardware configuration and/or operating 
#   system. 

# Package: uroot
# Version: 1.4
# Date: 2005/10/10
# Title: Unit Root Tests and Graphics for Seasonal Time Series
# Author: Javier López-de-Lacalle <javlacalle@yahoo.es> and
#   Ignacio Díaz-Emparanza <Ignacio.Diaz-Emparanza@ehu.es>
# Maintainer: Javier López-de-Lacalle <javlacalle@yahoo.es>
# Depends: R (>= 2.0.0), methods
# Imports: graphics
# Suggests: tcltk, xtable
# Collate: adf.R ch.R graphics.R hegy.R kpss.R makegui.R misc.R rec-test.R 
#   stepdate.R urootgui.R urt-cvtables.R xtable.R
# Description: This package contains several functions for analysing quarterly 
#   and monthly time series. Unit root test: ADF, KPSS, HEGY, and CH, as well 
#   as graphics: Buys-Ballot and seasonal cycles, among others, have been 
#   implemented to accomplish either an analytical or a graphical analysis. 
#   Combined use of both enables the user to characterize the seasonality as
#   deterministic, stochastic or a mixture of them. An easy to use graphical 
#   user interface is also provided to run some of the implemented functions.
# License: GPL version 2 or newer. The terms of this license are in a file 
#   called COPYING which is provided with R.
# Packaged: Tue Oct 18 12:56:27 2005; root


################################################################################


# adf.R
# ch.R
# graphics.R
# hegy.R
# kpss.R
# misc.R
# rec-test.R
# stepdate.R
# urt-cvtables.R
# xtable.R

##~ significance codes (*,**) en regvarcoefs...

setClassUnion("maybeRegvar", c("NULL", "numeric", "matrix", "data.frame")) # NULL y numeric para ltrunc en KPPS Y CH rect. 
setClassUnion("maybeLags", c("NULL", "list", "character", "numeric", "matrix")) ##~ "list"
setClass("adfstat", representation(wts="ts", itsd="numeric", regvar="maybeRegvar", selectlags="list",
  regvarcoefs="maybeRegvar", lagsorder="maybeLags", lagcoefs="maybeLags", res="numeric", lmadf="lm",
  stat="matrix"))

ADF.test <- function(wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
{
  s  <- frequency(wts); t0 <- start(wts); N  <- length(wts)
  if(s==1 && itsd[3] != 0){
     detreg$detcomp <- c(detreg$detcomp[1:2], 0)
     cat("  Seasonal dummies are not considered for annual data.\n")
  }

  # Dependent variable.
  Deltay <- matrix(c(NA, diff(wts, lag=1)), ncol=1)

  # Regressor variables.
  Intercept <- matrix(rep(1, N), ncol=1)[,itsd[1]]
  Trend     <- matrix(c(1:N), ncol=1)[,itsd[2]]
  SDummy <- data.frame(SeasDummy=SeasDummy(wts, "alg"))[,itsd[-c(1,2)]]

  if(!identical(regvar, 0) && length(names(regvar)) == 0)
    regvar <- data.frame(Regvar=regvar)

  if(identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- numeric(0)
  if(!identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy))
  if(!identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy, regvar))
  if(identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(regvar))
  regvarnames <- dimnames(Mdetreg)[[2]]

  # DF regression without lags.
  Madfreg <- as.matrix(data.frame(adf.reg=c(NA, wts[1:(N-1)])))
  ifelse(length(Mdetreg) == 0,
    lmdf <- lm(Deltay[,1] ~ 0+Madfreg),
    lmdf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg))

  # Lags selection.
  if(class(selectlags[[1]]) == "numeric"){
    selP <- selectlags[[1]]
  } else
      switch(selectlags[[1]],
        aic   = selP <- selPabic(lmdet=lmdf, type="aic", Pmax=selectlags[[2]]),
        bic   = selP <- selPabic(lmdet=lmdf, type="bic", Pmax=selectlags[[2]]),
        signf = selP <- selPsignf(lmdet=lmdf, cvref=NULL, Pmax=selectlags[[2]]),)

  # ADF regression.
  if(identical(selP, 0) || length(selP)==0){
    ifelse(length(Mdetreg)==0, lmadf <- lm(Deltay[,1] ~ 0+Madfreg),
                               lmadf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg))
  } else{
      Mlags <- ret(Deltay, max(selP)+2)[,-1]; aux <- dimnames(Mlags)[[2]]
      Mlags <- data.frame(Mlags[,selP]); lagnames <- aux[selP]
      Mlags <- as.matrix(Mlags)
      ifelse(length(Mdetreg)==0, lmadf <- lm(Deltay[,1] ~ 0+Madfreg + Mlags),
                                 lmadf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg + Mlags))
    }

  # lmadf estimates.
  coefs <- coef(summary(lmadf)); Ncoef <- length(coef(lmadf))
  colnames <- dimnames(coefs)[[2]]
  ifelse(Ncoef==1, ref<-1, ref <- which(dimnames(coefs)[[1]] == "Madfreg"))

  # determiistic components estimates.
  if(ref > 1){
    regvarcoefs <- coefs[1:(ref-1),1:4]
    dim(regvarcoefs) <- c((ref-1), 4)
    dimnames(regvarcoefs) <- list(regvarnames, colnames)
  } else
      regvarcoefs <- NULL

  # ADF statistic and p-value.
  adfreg <- t(as.data.frame(coefs[ref,1:4]))
  code <- paste("DF", paste(itsd[1:2], collapse=""), "0", sep="")
  adfreg[,4] <- interpolpval(code=code, stat=adfreg[,3], N=N)$pval
  dimnames(adfreg) <- list("adf.reg", colnames[1:4])

  # Lags estimates
  if(ref < Ncoef){
    lagcoefs <- coefs[(ref+1):Ncoef,1:4]
    dim(lagcoefs) <- c(length((ref+1):Ncoef), 4); lagcoefs <- data.frame(lagcoefs)
    dimnames(lagcoefs) <- list(lagnames, colnames); lagcoefs <- as.matrix(lagcoefs)
  } else
      lagcoefs <- NULL

  new("adfstat", wts=wts, itsd=itsd, regvar=regvar, selectlags=selectlags, regvarcoefs=regvarcoefs,
    lagsorder=selP, lagcoefs=lagcoefs, res=residuals(lmadf), lmadf=lmadf, stat=adfreg)
}

setMethod("show", "adfstat",
  function(object)
  {
    lmadf <- object@lmadf
    coefs <- coef(lmadf); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="Madfreg"))

    cat("  --------- ------ - ------ ----\n")
    cat("  Augmented Dickey & Fuller test\n")
    cat("  --------- ------ - ------ ----\n\n")

    cat("  Null hypothesis: Unit root.\n")
    cat("  Alternative hypothesis: Stationarity.\n")

    cat("\n----\n  ADF statistic:\n\n")
    print(round(object@stat, 3))
    cat("\n  Lag orders:", object@lagsorder)
    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setMethod("summary", "adfstat",
  function(object)
  {
    lmadf <- object@lmadf
    coefs <- coef(lmadf); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="Madfreg"))

    ##~ Mostrando p-values.
    cat("  --------- ------ - ------ ----\n")
    cat("  Augmented Dickey & Fuller test\n")
    cat("  --------- ------ - ------ ----\n\n")

    cat("----\n  Deterministic regressors estimates:\n\n")
    if(ref > 1){
      object@regvarcoefs[,1:4] <- round(object@regvarcoefs[,1:4], 3)
      print(object@regvarcoefs)
    } else
        cat("    None selected.\n")

    cat("\n----\n  ADF regressor estimate:\n\n")
    print(round(object@stat, 3))

    cat("\n----\n  Selected lags estimates:\n\n")
    if(ref < length(coefnames)){
      object@lagcoefs[,1:4] <- round(object@lagcoefs[,1:4], 3)
      print(object@lagcoefs)
    } else
        cat("    None selected.\n")

    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setClass("chstat", representation(wts="ts", frec="numeric", DetTr="logical", ltrunc="numeric",
  Fhat="matrix", Omfhat="matrix", MA="matrix", stat="numeric"))

CH.test <- function(wts, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL)
{
  s <- frequency(wts); t0 <- start(wts); N <- length(wts)
  if(class(frec)=="NULL") frec <- rep(1, s/2)
  if(class(ltrunc)=="NULL") ltrunc <- round(s*(N/100)^0.25)

  # Regressors for the auxiliar regression

  R1 <- SeasDummy(wts, "trg")
  VFEalg <- SeasDummy(wts, "alg")
  tiempo <- c(1:N)
 
  if(DetTr ==FALSE){
    if(f0 == 1){
      lmch  <- lm(wts[2:N] ~ wts[1:(N-1)] + R1[2:N,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
    if(f0 == 0){
      lmch  <- lm(wts ~ R1[,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
  }
  if(DetTr ==TRUE){
    if(f0 == 1){
      lmch  <- lm(wts[2:N] ~ wts[1:(N-1)] + tiempo[1:(N-1)] + R1[2:N,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
    if(f0 == 0){
      lmch  <- lm(wts ~ tiempo + R1[,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
  }

  # Fhat
  Fhat <- Fhataux <- matrix(nrow=length(ehat), ncol=(s-1))
 
  #ifelse(f0 == 1, op <- 1, op <- 0) # también cambia varnw[3:13...]
  for(i in 1:length(ehat))
    Fhataux[i,] <- R1[i+f0,]*ehat[i]   # No producto matricial %*%
 
  for(i in 1:nrow(Fhat)){
    for(n in 1:(s-1))
      Fhat[i,n] <- sum(Fhataux[1:i,n])
  }
 
  # Omega supra-f estimate  (Omfhat)
  
  wnw <- 1 - seq(1,ltrunc,1)/(ltrunc+1)
  Ne <- nrow(Fhataux)
  Omnw <- 0
  for(k in 1:ltrunc)
    Omnw <- Omnw + (t(Fhataux)[,(k+1):Ne] %*% Fhataux[1:(Ne-k),]) * wnw[k]
  Omfhat <- (crossprod(Fhataux) + Omnw + t(Omnw))/Ne
  
  # Matriz A
  # frec <- c(0,1)          # input for quarterly series
  # frec <- c(1,0,0,0,0,0)  # input for monthly series

  #sq     <- seq(1,11,2)
  sq <- seq(1,(s-1),2)
  frecob <- rep(0,(s-1))

  for(i in 1:length(frec)){
    if(frec[i] == 1 && i == s/2)
       frecob[sq[i]]     <- 1
    if(frec[i] == 1 && i < s/2)
       frecob[sq[i]] <- frecob[sq[i]+1] <- 1
  }
 
  a <- length(which(frecob == 1))
  A <- matrix(0, nrow=s-1, ncol=a)
 
  j <- 1
  for(i in 1:(s-1))
    if(frecob[i] == 1){
      A[i,j] <- 1
      ifelse(frecob[i] == 1, j <- j+1, j <- j)
    }

  # Statistic (equation (13) Canova & Hansen (1995))
 
  stL <- (1/N^2)*sum(diag( solve(t(A) %*% Omfhat %*% A)
                           %*%
                           t(A) %*% t(Fhat) %*% Fhat %*% A ))
  
  new("chstat", wts=wts, frec=frec, DetTr=DetTr, ltrunc=ltrunc,
    Fhat=Fhat, Omfhat=Omfhat, MA=A, stat=stL)
}

setMethod("show", "chstat",
  function(object)
  {
    s <- frequency(object@wts)
    if(s == 4)
      fnames <- c("pi/2", "pi")
    if(s == 12)
      fnames <- c("pi/6", "pi/3", "pi/2", "2pi/3", "5pi/6", "pi")
    ref <- which(object@frec==1)
    cvtable <- lookupCVtable(code=paste("CHp", sum(object@frec*c(rep(2, s/2-1),1)), sep=""))[1,]
    dimnames(cvtable)[[1]] <- ""

    cat("\n  ------ - ------ ----")
    cat("\n  Canova & Hansen test")
    cat("\n  ------ - ------ ----\n\n")

    cat("  Null hypothesis: Stationarity.\n")
    cat("  Alternative hypothesis: Unit root.\n")
    cat("  Frequency of the tested cycles:", paste(fnames[ref], collate=","), "\n\n")

    cat(c("  L-statistic:", round(object@stat, 3), " \n"))
    cat(c("  Lag truncation parameter:", object@ltrunc, "\n\n"))
    cat("  Critical values:\n\n")
    print(cvtable)
    cat("\n")
  }
)

rmg <- function(wts, krmg=NULL)
{
 # k es el número de periodos entre los que
   # se calcula el rango y la media

  N <- length(wts)   
  if(class(krmg) == "NULL")
    krmg <- integer(sqrt(N))
  media <- c(1:(N-krmg+1))
  rango <- c(1:(N-krmg+1))

  for(i in 1:length(media)){
    media[i]<- mean(wts[i:(i+(krmg-1))])
    rango[i]<- (max(wts[i:(i+(krmg-1))])-min(wts[i:(i+(krmg-1))]))
                          }
  cor.mr <- cor(media, rango, use = "complete")

  plot(media, rango, pch=20, main="Range-mean plot", xlab="Mean", ylab="Range")
  mtext(as.expression(substitute(cor(R,M)==cor.mr,
        list(cor.mr=round(cor.mr, 4)))), side = 3, line = 0.35, cex=0.7)

  cat("\n  Correlation range-mean: ", round(cor.mr, 4), "\n")
}

##~ Ver hacer como método de una clase sts, clase sts todo lo de ts más atributo Mseas. Ver class mts ???
##~ chequeado para s=4, s=12.
##~ probarlo con diferentes start(wts) end(wts)

# Build a matrix containing seasonal paths.

Msts <- function(wts)  ##~ hacer que trate a las columnas como ts, ts.union.
{
  s <- frequency(wts)
  seas.data <- split(wts, cycle(wts))
  Mseas <- matrix(NA, nrow=ceiling(length(wts)/s), ncol=s)
  ref1 <- c(rep(2, start(wts)[2]-1), rep(1, s-start(wts)[2]+1))
  ref2 <- c(rep(0, end(wts)[2]), rep(1, s-end(wts)[2]))
  for(i in 1:s)
    Mseas[ref1[i]:(nrow(Mseas)-ref2[i]),i] <- seas.data[[i]]  # unlist(seas.data[[i]])

  ynames <- as.integer(time(wts))
  if(s==4)
    snames <- c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
  else if(s==12)
    snames <- month.abb
  else
    snames <- paste("Seas", 1:s, sep="")

  Mseas <- matrix(Mseas, nrow=nrow(Mseas), ncol=ncol(Mseas),
           dimnames=list(as.character(ynames[1]:ynames[length(ynames)]), snames))

#  for(i in 1:ncol(Mseas))
#    Mseas[,i] <- ts(Mseas[,i], frequency=1, start=start(wts))
#  as.matrix(Mseas)
  Mseas
}

# Buys-Ballot plot.
##~ poner np-nl como opción.
bbplot <- function(wts, colour=c("SlateBlue","SeaGreen","red","magenta"))
{
  s <- frequency(wts)
  if(s==4){
    nl <- 4; np <- 1; fr <- 2
    snames <- c("Qrt1", "Qrt2", "Qrt3", "Qrt4")
  }
  else if(s==12){
    nl <- 3; np <- fr <- 4
    snames <- month.abb
  }
  else{
    if(identical(c(s/2 - as.integer(s/2)), 0)){
      nl <- 4; np <- s/nl
    }
    else{
      np <- floor((s+1)/4); nl <- c(rep(4, np-1), s/np+1)
    }
  }

  Mseas <- Msts(wts)

  aux <- Mseas[nrow(Mseas),]
  labaux <- c(na.omit(aux), Mseas[(nrow(Mseas)-1),which(is.na(aux))])

  xlim <- c(start(wts)[1], end(wts)[1]+1.5)
  ylim <- c(min(wts), max(wts))
  aux1 <- seq(1,s,nl); aux2 <- seq(nl,s,nl)

  if(s==4)
    opar <- par(mar=c(3,3.5,2,2), las=1)
  if(s==12)
    opar <- par(mfrow=c(fr/2, fr/2), mar=c(3,3.5,2,2), las=1)
  for(i in 1:np){
    ref <- aux1[i]:aux2[i]
    ts.plot(ts(Mseas[,ref], start=start(wts)[1]), xlab="", ylab="",
      xlim=xlim, ylim=ylim , col=colour)

    #text(xlim[2]-0.2, as.matrix(Mseas[,ref])[nrow(Mseas),], month.abb[ref])
    text(xlim[2]-0.2, labaux[ref], snames[ref], col=colour)
  }
  par(opar)
}

# Buys-Ballot annual plot.

bbaplot <- function(wts, years=NULL, colour=c("SlateBlue","SeaGreen","red","magenta"))
{
  if(class(years) == "NULL")
    years <- c(start(wts)[1], as.integer((start(wts)[1]+end(wts)[1])/2), end(wts)[1])

  s <- frequency(wts)
  c1 <- which(years < start(wts)[1])
  c2 <- which(years > end(wts)[1])
  if(length(c1) != 0 || length(c2) != 0)
    stop("  Years", years[c1], years[c2], "are out of the sample.")

  Mseas <- Msts(wts)
  ylabs <- numeric(0)
  ref <- numeric(0)
  for(i in 1:length(years))
    ref <- c(ref, which(as.numeric(dimnames(Mseas)[[1]]) == years[i]))

  opar <- par(las=1)
  ts.plot(t(Mseas[ref,]), xlab="Seasons", ylab="", xlim=c(1,s+0.7), col=colour)

  for(j in 1:length(years)){
    aux <- na.omit(Mseas[ref[j],])
    ylabs <- c(ylabs, aux[length(aux)])
  }

  text(s+0.6, ylabs, years, col=colour)
  par(opar)
}

# Buys-Ballot 3D plot.


bb3D <- function(wts, color=TRUE, x=30, y=30)
{
  require(tcltk)
  ttplot <- tktoplevel()
  tkconfigure(ttplot, cursor="fleur")
  tkwm.title(ttplot, "Rotate")

  bbpersp <- function(wts=wts, color=color, x=x, y=y)
  {
    if(color==TRUE)
       colores <- "lightgoldenrod"
    if(color==FALSE)
       colores <- grey((0:6)/6)

    Mseas <- Msts(wts)
  
    if(frequency(wts) == 12){ xlabel <- "Months"; ntic <- frequency(wts)/2}
    if(frequency(wts) == 4) { xlabel <- "Quarters"; ntic <- frequency(wts)}
    xx <- c(1:ncol(Mseas))
    yy <- c(start(wts)[1]:(start(wts)[1]+nrow(Mseas)-1))
  
    persp(xx, yy, t(Mseas), # main="Buys-Ballot 3D"
          theta = x, phi = y, expand = 0.5, xlab=xlabel, ylab="", zlab="", 
          shade=0.4, col = colores, ticktype="detailed", nticks=ntic)
  }

  bbpersp(wts, color=color, x=as.integer(x), y=as.integer(y))

  RightClick2 <- function(x,y)
  {
    rootx <- as.integer(tkwinfo("rootx",ttplot))
    rooty <- as.integer(tkwinfo("rooty",ttplot))
    xTxt <- as.integer(x)+rootx
    yTxt <- as.integer(y)+rooty
    bbpersp(wts, color=color, x=as.integer(x), y=as.integer(y))
  }
  tkbind(ttplot, "<Button-3>", RightClick2)
}

# Buys-Ballot contour plot.

bbcn <- function(wts, color=TRUE)  # MMp ó Mq de bbmp() y quarterg()
{
  if(color==TRUE)
     colores <- terrain.colors(200)
  if(color==FALSE)
     colores <- grey((0:32)/32)
  if(frequency(wts) == 12)
     xlabel <- month.abb
  if(frequency(wts) == 4){ xlabel <- c("Qrtr1", "Qrtr2", "Qrtr3", "Qrtr4")}

  Mseas <- Msts(wts)
  x <- c(1:ncol(Mseas))
  y <- c(start(wts)[1]:(start(wts)[1]+nrow(Mseas)-1))

  image(x, y, t(Mseas), # main="Buys-Ballot contour"
        las=1, xlab="", ylab="", xaxt="n", col = colores)
  mtext(xlabel[1:frequency(wts)], side=1, line=1, at=c(1:frequency(wts))) #, cex=0.7)
  contour(x, y, t(Mseas), add = TRUE, drawlabels = TRUE, col="blue")
}

##~ plotcycles: poner el polinomio en lugar de frecuencias (en doc tabla con frecuencias).

plotcycles <- function(wts)
{
  s <- frequency(wts)
  if(s==4)
   pdim <- c(2,2)
  if(s==12)
   pdim <- c(4,2)

  mains <- c("Frequency zero", "Frequency pi", "Frequencies (pi/2; 3pi/2)", "Frequencies (2pi/3; 4pi/3)",
             "Frequencies (pi/3; 5pi/3)", "Frequencies (5pi/6; 7pi/6)", "Frequencies (pi/6; 11pi/6)")

  Mfilt <- matrix(nrow=length(wts), ncol=(s/2+1))

  for(i in 1:ncol(Mfilt)){
    factors <- rep(1, (s/2+1))
    factors[i] <- 0
    Mfilt[,i] <- factorsdiff(wts, factors=factors)$Fil.wts
    ts(Mfilt, frequency=s, start=start(wts))
  }

  opar <- par(mfrow=pdim, las=1)
  plot(diff(wts, lag=s), xlab="", ylab="", main="Seasonal difference")
  for(i in 1:ncol(Mfilt)){
    fts <- ts(Mfilt[,i], frequency=s, start=start(wts))
    plot(fts, xlab="", ylab="", main=mains[i])
  }
  par(opar)
}

##~ F-st para VFE seleccionadas. y p.value para F.2:s F1:s, buscar tablas.
##~ significance codes (*,**) en regvarcoefs...

setClass("hegystat", representation(wts="ts", itsd="numeric", regvar="maybeRegvar", hegyreg="matrix",
  selectlags="list", regvarcoefs="maybeRegvar", hegycoefs="matrix", lagsorder="maybeLags",
  lagcoefs="maybeLags", res="numeric", lmhegy="lm", stats="matrix"))

HEGY.test <- function(wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
{
  s  <- frequency(wts); t0 <- start(wts); N <- length(wts)

  # Dependent variable.
  Deltay <- matrix(c(rep(NA, s), diff(wts, lag=s)), ncol=1)

  # Regressor variables.
  Intercept <- matrix(rep(1, N), ncol=1)[,itsd[1]]
  Trend     <- matrix(c(1:N), ncol=1)[,itsd[2]]
  SDummy <- data.frame(SeasDummy=SeasDummy(wts, "alg"))[,itsd[-c(1,2)]]

  if(!identical(regvar, 0) && length(names(regvar)) == 0)
    regvar <- data.frame(Regvar=regvar)

  if(identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- numeric(0)
  if(!identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy))
  if(!identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy, regvar))
  if(identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(regvar))
  regvarnames <- dimnames(Mdetreg)[[2]]

  # HEGY regression without lags.
  Mhegyreg <- hegy.reg(wts)
  ifelse(length(Mdetreg) == 0,
    lmdf <- lmhegyp <- lm(Deltay[,1] ~ 0+Mhegyreg),
    lmdf <- lmhegyp <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg))

  # Lags selection.
  if(class(selectlags[[1]]) == "numeric"){
    selP <- selectlags[[1]]
  } else
      switch(selectlags[[1]],
        aic   = selP <- selPabic(lmdet=lmhegyp, type="aic", Pmax=selectlags[[2]]),
        bic   = selP <- selPabic(lmdet=lmhegyp, type="bic", Pmax=selectlags[[2]]),
        signf = selP <- selPsignf(lmdet=lmhegyp, cvref=NULL, Pmax=selectlags[[2]]),)

  # HEGY regression.
  # lmdetlag: regression with deterministic components and lags (without the hegy regressors).
  # lmhegy: lmdetlag including the hegy regressors.
  if(identical(selP, 0) || length(selP)==0){
    if(length(Mdetreg)==0){
      lmdetlag <- lm(Deltay[,1] ~ 0)
      lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mhegyreg)
    } else{
        lmdetlag <- lm(Deltay[,1] ~ 0+Mdetreg)
        lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg)
      }
  } else{
      Mlags <- ret(Deltay, max(selP)+2)[,-1]; aux <- dimnames(Mlags)[[2]]
      Mlags <- data.frame(Mlags[,selP]); lagnames <- aux[selP]
      Mlags <- as.matrix(Mlags)
      if(length(Mdetreg)==0){
        lmdetlag <- lm(Deltay[,1] ~ 0+ Mlags)
        lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mhegyreg + Mlags)
      } else{
          lmdetlag <- lm(Deltay[,1] ~ 0+Mdetreg + Mlags)
          lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg + Mlags)
      }
    }

  # lmhegy estimates.
  coefs <- coef(summary(lmhegy)); Ncoef <- length(coef(lmhegy))
  colnames <- dimnames(coefs)[[2]]
  ifelse(Ncoef==s, ref<-1, ref <- which(dimnames(coefs)[[1]] == "MhegyregYpi1"))

  if(ref > 1){
    regvarcoefs <- coefs[1:(ref-1),1:4]
    dim(regvarcoefs) <- c((ref-1), 4)
    dimnames(regvarcoefs) <- list(regvarnames, colnames)
  } else
      regvarcoefs <- NULL

  # hegyreg
  ##~ Con bootshegy puede ocurrir que Mhegyreg tiene columnas casi 0 y estimación es NA, entonces coefs[ref:(ref+s-1),1:4] eliman esos NAs y ref ya no sirve como referencia.
  hegycoefs <- coefs[ref:(ref+s-1),1:4]  ##~ Añadir F_1:s, F_2:s
  dimnames(hegycoefs)[[1]] <- dimnames(Mhegyreg)[[2]]

  if((ref+s-1) < Ncoef){
    lagcoefs <- coefs[(ref+s):Ncoef,1:4]
    dim(lagcoefs) <- c(length((ref+s):Ncoef), 4); lagcoefs <- data.frame(lagcoefs)
    dimnames(lagcoefs) <- list(lagnames, colnames); lagcoefs <- as.matrix(lagcoefs)
  } else
      lagcoefs <- NULL

  # HEGY Statistics and p-values.
  # tpi
  if(s==4) c1 <- "HEGY";
  if(s==12) c1 <- "BM"
  c2 <- paste(itsd[1:2], sep="", collapse="")
  ifelse(itsd[3] != 0, c3 <-1, c3 <-0)

  for(i in 1:s){
    code <- paste(c(c1, c2, c3, "tpi", i), sep="", collapse="")
    hegycoefs[i,4] <- interpolpval(code=code, stat=hegycoefs[i,3], N=N)$pval
  }

  # tpi1-tpi2, Fpi_
  EtFst <- matrix(nrow=(s/2+3), ncol=2)
  EtFst[1:2,] <- hegycoefs[1:2,3:4]

  code <- paste(c(c1, c2, c3, "Foddeven"), sep="", collapse="")
  for(i in 1:(s/2-1)){
    lmpart <- update(lmdetlag, . ~ . + Mhegyreg[,c(1:s)[-c(2*i+1, 2*i+2)]])
    lmhegy <- update(lmpart, . ~ . + Mhegyreg[,c(2*i+1, 2*i+2)])
    EtFst[i+2,1] <- anova(lmpart, lmhegy, test="F")$F[2]
    EtFst[i+2,2] <- interpolpval(code=code, stat=EtFst[i+2,1], N=N)$pval
  }

  lmpart <- update(lmdetlag, . ~ . + Mhegyreg[,1])
  lmhegy <- update(lmpart, . ~ . + Mhegyreg[,2:s])
  EtFst[(s/2+2),1] <- anova(lmpart, lmhegy, test="F")$F[2]

  lmhegy <- update(lmdetlag, . ~ . + Mhegyreg[,1:s])
  EtFst[(s/2+3),1] <- anova(lmdetlag, lmhegy, test="F")$F[2]

  EtFsnames <- c("tpi_1", "tpi_2",
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))
  #dim(EtFst) <- c((s/2+3), 2);
  dimnames(EtFst) <- list(EtFsnames, c("Stat.", "p-value"))

  new("hegystat", wts=wts, itsd=itsd, regvar=regvar, hegyreg=Mhegyreg, selectlags=selectlags,
    regvarcoefs=regvarcoefs, hegycoefs=hegycoefs, lagsorder=selP, lagcoefs=lagcoefs,
    res=residuals(lmhegy), lmhegy=lmhegyout, stats=EtFst)
}

setMethod("show", "hegystat",
  function(object)
  {
    s <- frequency(object@wts); lmhegy <- object@lmhegy
    coefs <- coef(lmhegy); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="MhegyregYpi1"))

    ##~ Mostrando p-values.
    cat("  ---- ----\n")
    cat("  HEGY test\n")
    cat("  ---- ----\n\n")

    cat("  Null hypothesis: Unit root.\n")
    cat("  Alternative hypothesis: Stationarity.\n")

    cat("\n----\n  HEGY statistics:\n\n")
    print(round(object@stats, 3))
    cat("\n  Lag orders:", object@lagsorder)
    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setMethod("summary", "hegystat",
  function(object)
  {
    s <- frequency(object@wts); lmhegy <- object@lmhegy
    coefs <- coef(lmhegy); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="MhegyregYpi1"))

    ##~ Mostrando p-values.
    cat("  ---- ----\n")
    cat("  HEGY test\n")
    cat("  ---- ----\n\n")

    cat("----\n  Deterministic regressors estimates:\n\n")
    if(ref > 1){
      print(round(object@regvarcoefs, 3))
    } else
        cat("    None selected.\n")

    cat("\n----\n  HEGY regressors estimates:\n\n")
    print(round(object@hegycoefs, 3))

    cat("\n----\n  Selected lags estimates:\n\n")
    if((ref+s-1) < length(coefnames)){
      print(round(object@lagcoefs, 3))
    } else
        cat("    None selected.\n")

    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setClass("kpssstat", representation(wts="ts", lmkpss="lm", ltrunc="numeric", levelst="numeric",
  trendst="numeric"))

KPSS.test <- function(wts, ltrunc=NULL)
{
  tiempo <- c(1:length(wts))
  ML <- ret(wts, 2)

  ##~ ver
  if(class(ltrunc)=="NULL")
    ltrunc <- as.integer(3*sqrt(length(wts))/13)
  #ltrunc <- as.integer(3*sqrt(length(wts))/13)  # ver PP.test, artículo KPSS
  #ltrunc <- as.integer(10*sqrt(length(wts))/14)
  #ltrunc <- as.integer(4*(length(wts)/100)^(1/4))
  #ltrunc <- as.integer(12*(length(wts)/100)^(1/4))

  lmkpss <- lm(ML[,1] ~ tiempo)
  ehat <- residuals(lmkpss)
  Sa  <- cumsum(ehat)
  N <- length(ehat)

  # estimador consistente de la varianza de los residuos:
  if(ltrunc == 0)
    s.2a <- 1/N*sum(ehat^2)
  if(ltrunc > 0){
    auxa <- c(1:ltrunc)
    for(i in 1:ltrunc)
      auxa[i] <- (1-i/(ltrunc+1))*sum(ehat[(i+1):N]*ehat[1:(N-i)])
    s.2a <- (1/N)*sum(ehat^2)+(2/N)*sum(auxa)
  }

  Trend <- N^(-2)*sum(Sa^2/s.2a)

  # para la hipótesis nula de estacionariedad en tendencia los
  # residuos son ee=var-mean(var),
  # se analiza la serie extrayendo la media y la tendencia.

  wts2 <- wts - mean(na.omit(wts))
  Sb    <- cumsum(wts2)

  # estimador consistente de la varianza de los residuos:
  if(ltrunc==0)
    s.2b <- 1/N*sum(wts2[1:N]^2)
  if(ltrunc>0){
    auxb <- c(1:ltrunc)
    for(i in 1:ltrunc)
      auxb[i] <- (1-i/(ltrunc+1))*sum(wts2[(i+1):N]*wts2[1:(N-i)])
    s.2b <- (1/N)*sum(wts2[1:N]^2)+(2/N)*sum(auxb)
  }

  Level <- N^(-2)*sum(Sb[1:N]^2/s.2b)

  new("kpssstat", wts=wts, lmkpss=lmkpss, ltrunc=ltrunc, levelst=Level, trendst=Trend)
}

setMethod("show", "kpssstat",
  function(object){
    cvl <- data.frame(rbind(c(0.347, 0.463, 0.574, 0.739)))
    dimnames(cvl) <- list("", c("0.10", "0.05", "0.025", "0.01"))
    cvt <- data.frame(rbind(c(0.119, 0.146, 0.176, 0.216)))
    dimnames(cvt) <- list("", c("0.10", "0.05", "0.025", "0.01"))

    cat("\n  ---- ----")
    cat("\n  KPSS test")
    cat("\n  ---- ----\n\n")

    cat("  Null hypotheses: Level stationarity and stationarity around a linear trend.\n")
    cat("  Alternative hypothesis: Unit root.\n")

    cat(c("----\n  Statistic for the null hypothesis of \n   level stationarity:",
        round(object@levelst, 3), "\n"))
    cat("\n    Critical values:\n\n")
    print(cvl)
    cat(c("----\n  Statistic for the null hypothesis of \n   trend stationarity:",
        round(object@trendst, 3), "\n"))
    cat("\n    Critical values:\n\n")
    print(cvt)
    cat(c("----\n  Lag truncation parameter:", object@ltrunc, "\n\n"))
  }
)

## Auxiliar functions

ret <- function(wts, k)
{
  N <- length(wts)
  wts.r <- matrix(NA, nrow=N, ncol=k); wts.r[,1] <- wts
  for(i in 1:(k-1))
    wts.r[(i+1):N, (i+1)] <- wts[1:(N-i)]
  wts.r <- data.frame(wts.r); dimnames(wts.r)[[2]] <- paste("Lag", 0:(k-1), sep=".")
  as.matrix(wts.r)
}

SeasDummy <- function(wts, type)
{
  s <- frequency(wts); t0 <- start(wts); N <- length(wts)

  if(type == "alg"){        # Empleada en Barsky & Miron (1989)
   auxD <- matrix(0,nrow=N, ncol=s)
   sq   <- seq(1,N,s)
   k    <- 0

   for(j in 1:s){
     ifelse(sq[length(sq)] + k > N, n <- length(sq)-1, n <- N)
     for(i in 1:n)
       auxD[sq[i]+k,j] <- 1
     k <- k+1
   }
   VFE <- auxD
   if(t0[2] != 1){
      VFE <- matrix(nrow=N, ncol=s)
      VFE[,1:(t0[2]-1)] <- auxD[,(s-t0[2]+2):s]; VFE[,t0[2]:s] <- auxD[,1:(s-t0[2]+1)]
                  }
   if(t0[2] == 1){ VFE <- auxD }
  }

  if(type == "trg"){        # Empleada en Granger & Newbold (1986)
   qq  <- s/2
   VFE <- matrix(nrow=N, ncol=(s-1))

   sq1 <- seq(1,qq*2,2)
   sq2 <- seq(2,qq*2,2)
   j   <- c(1:(qq-1))

   for(i in 1:N){
     for(k in 1:(s-qq-1)){
         VFE[i,sq1[k]] <- cos((j[k]*pi/qq)*i)
         VFE[i,sq2[k]] <- sin((j[k]*pi/qq)*i)
     }
     VFE[i,(s-1)] <- (-1)^i
   }
  }
  VFE
}

contts <- function(lm, a)
{
  var.coef <- vcov(lm)[a,a]
  se.coef  <- sqrt(var.coef)
  et       <- lm$coef[a]/se.coef
  list(se.coef=se.coef, t.stat=et)
}

# HEGY regressors

hegy.reg <- function(wts)
{
  s <- frequency(wts); ML <- ret(ret(wts, 2)[,2], s+1)

  if(s==4){
    y1 <- ML[,1] + ML[,2] + ML[,3] + ML[,4]
    y2 <- -(ML[,1] - ML[,2] + ML[,3] - ML[,4])
    y4 <- -(ML[,1] - ML[,3])
    y3 <- ret(y4, 2)[,2]
    Mypi <- data.frame(y1, y2, y3, y4)
  }

  if(s==12){
    y1 <- ML[,1] + ML[,2] + ML[,3] + ML[,4] + ML[,5] + ML[,6] +
          ML[,7] + ML[,8] + ML[,9] + ML[,10] + ML[,11] + ML[,12]
    y2 <- -(ML[,1] - ML[,2] + ML[,3] - ML[,4] + ML[,5] - ML[,6] +
            ML[,7] - ML[,8] + ML[,9] - ML[,10] + ML[,11] - ML[,12])
    y3 <- -(ML[,2] - ML[,4] + ML[,6] - ML[,8] + ML[,10] - ML[,12])
    y4 <- -(ML[,1] - ML[,3] + ML[,5] - ML[,7] + ML[,9] - ML[,11])
    y5 <- -0.5*(ML[,1] + ML[,2] - 2*ML[,3] + ML[,4] + ML[,5] - 2*ML[,6] +
                ML[,7] + ML[,8] - 2*ML[,9] + ML[,10] + ML[,11] - 2*ML[,12])
    y6 <- (sqrt(3)/2)*(ML[,1] - ML[,2] + ML[,4] - ML[,5] + ML[,7] - ML[,8] + ML[,10] - ML[,11])
    y7 <- 0.5*(ML[,1] - ML[,2] - 2*ML[,3] - ML[,4] + ML[,5] + 2*ML[,6] +
               ML[,7] - ML[,8] - 2*ML[,9] - ML[,10] + ML[,11] + 2*ML[,12])
    y8 <- -(sqrt(3)/2)*(ML[,1] + ML[,2] - ML[,4] - ML[,5] + ML[,7] + ML[,8] - ML[,10] - ML[,11])
    y9 <- -0.5*(sqrt(3)*ML[,1] - ML[,2] + ML[,4] - sqrt(3)*ML[,5] + 2*ML[,6] - sqrt(3)*ML[,7] +
                ML[,8] - ML[,10] + sqrt(3)*ML[,11] - 2*ML[,12])
    y10 <- 0.5*(ML[,1] - sqrt(3)*ML[,2] + 2*ML[,3] - sqrt(3)*ML[,4] + ML[,5] - ML[,7] +
                sqrt(3)*ML[,8] - 2*ML[,9] + sqrt(3)*ML[,10] - ML[,11])
    y11 <- 0.5*(sqrt(3)*ML[,1] + ML[,2] - ML[,4] - sqrt(3)*ML[,5] - 2*ML[,6] - sqrt(3)*ML[,7] -
                ML[,8] + ML[,10] + sqrt(3)*ML[,11] + 2*ML[,12])
    y12 <- -0.5*(ML[,1] + sqrt(3)*ML[,2] + 2*ML[,3] + sqrt(3)*ML[,4] + ML[,5] - ML[,7] -
                 sqrt(3)*ML[,8] - 2*ML[,9] - sqrt(3)*ML[,10] - ML[,11])
    Mypi <- data.frame(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12)
  }

  dimnames(Mypi)[[2]] <- paste("Ypi", 1:s, sep="")
  Mypi <- as.matrix(Mypi)
  Mypi
}

crpp <- function(vcoef1, vrfil1, vcoef2, vrfil2)
{
  n1 <- length(vcoef1)
  n2 <- length(vcoef2)
  fcoef <- rep(NA, n1*n2)
  rfil  <- rep(NA, n1*n2)
  fcoef0 <- rep(0, n1*n2)
  rfil0  <- rep(0, n1*n2)
  k <- 1
  for(i in 1:n1){
    for(j in 1:n2){
       fcoef0[k] <- vcoef1[i]*vcoef2[j]
       rfil0[k]  <- vrfil1[i]+vrfil2[j]
       k <- k+1
                  }
                }
  # Simplificar
  for(i in 1:(n1*n2)){
     simpl <- which(rfil0 == rfil0[i])
     if(length(simpl)>0){
        fcoef[i] <- sum(fcoef0[simpl]); fcoef0[simpl] <- NA
        rfil[i]  <- rfil0[i]  ; rfil0[simpl]  <- NA
                        }
                   }
  fcoef[which(fcoef==0)] <- rfil[which(fcoef==0)] <- NA
  fcoef <- na.omit(fcoef)[1:length(na.omit(fcoef))]
  rfil  <- na.omit(rfil)[1:length(na.omit(rfil))]

  # Ordenar
  fcoef0 <- rfil0 <- c(1:length(rfil))
  for(j in 1:length(rfil)){
    wm        <- which.min(rfil)
    rfil0[j]  <- rfil[wm]
    fcoef0[j] <- fcoef[wm]
    rfil[wm]  <- fcoef[wm] <- Inf
                          }
  rfil <- rfil0; fcoef <- fcoef0
  list(fcoef, rfil)
}

factorsdiff <- function(wts, factors)
{
  N     <- length(wts)
  ML    <- ret(wts, frequency(wts)+1)
  VCOEF <- cbind(c(1,-1,0),c(1,1,0),c(1,1,0),c(1,1,1),c(1,-1,1), c(1,sqrt(3),1),c(1,-sqrt(3),1))
  VRFIL <- cbind(c(0,1,0),c(0,1,0),c(0,2,0),c(0,1,2), c(0,1,2),c(0,1,2),c(0,1,2))

  if(length(which(factors == 1)) == 1)
  {
    Rfil  <- c(1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
    Fcoef <- c(-1,1,1,1,1,-1,1,sqrt(3),1,-sqrt(3),1)

    frfil <- which(factors == 1)
    Fil.vari <-  ML[,1] + Fcoef[frfil]*ML[,Rfil[frfil]+1]

    auxt0 <- length(wts)-length(na.omit(Fil.vari))
    Fil.vari <- ts(Fil.vari, frequency=frequency(wts), start=start(wts))
    fcoef <- Fcoef[frfil]
    rfil <- Rfil[frfil]
  }

  if(length(which(factors == 1)) > 1)
  {
    frfil <- which(factors == 1)
    fcoefrfil <- crpp(VCOEF[,frfil[1]], VRFIL[,frfil[1]], VCOEF[,frfil[2]], VRFIL[,frfil[2]])
    fcoef <- fcoefrfil[[1]]
    rfil  <- fcoefrfil[[2]]
    if(length(which(factors == 1)) > 2){
      for(i in 3:length(frfil)){
        fcoefrfil <- crpp(fcoef, rfil, VCOEF[,frfil[i]], VRFIL[,frfil[i]])
        fcoef <- fcoefrfil[[1]]
        rfil  <- fcoefrfil[[2]]
                               }
                                      }
    Fil.vari.aux  <- matrix(nrow=N, ncol=length(rfil))
    Fil.vari      <- matrix(nrow=N, ncol=1)

    for(i in 1:length(rfil))
       Fil.vari.aux[,i] <- fcoef[i]*ML[,(rfil[i]+1)]
    for(i in 1:N)
       Fil.vari[i,1] <- sum(Fil.vari.aux[i,])
    Fil.vari <- ts(Fil.vari, frequency=frequency(wts), start=start(wts))
  }
  list(Fil.wts=Fil.vari, fcoef, rfil)
}

## selectP

# Basado en top-down AIC y BIC.  ##~ Probar bic en lm1 y aic en lm2.

selPabic <- function(lmdet, type, Pmax=NULL)
{
  if(mode(Pmax) != "numeric") Pmax <- round(10*log10(length(lmdet$model[,1])))
  switch(type, aic = k <- 2,
               bic = k <- log(length(lmdet$model[,1])),)

  ML <- ret(lmdet$model[,1], Pmax+1)[,-1]
  drop <- NULL
  for(i in Pmax:1){
    ifelse(length(drop) == 0,
      lm1 <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)]),
      lm1 <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)[-drop]]))
    aic1 <- AIC(lm1, k=k)
    #ifelse(length(drop) == (Pmax-1),
    ifelse(identical(drop, as.numeric(c(2:Pmax))),
      aic2 <- AIC(lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1])), k=k),
      aic2 <- AIC(lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)[-c(drop,i)]]), k=k))

    if(aic1 >= aic2)
      drop <- c(drop, i)
  }

  ifelse(length(drop) == 0, lags <- c(1:Pmax), lags <- c(1:Pmax)[-drop])
  lags
}

# Basado en retardos significativos, al 10% por defecto.
## ** en bsadf puede ocurrit que matriz de retardos tiene columas distintas pero todas aproximadamente cero, entonces el coeficiente estimado es NA, hay que quitarlo. En selPabic no se considera esto.

selPsignf <- function(lmdet, cvref=1.65, Pmax=NULL)
{
  if(mode(cvref) != "numeric") cvref <- 1.65  # 10% approx.
  if(mode(Pmax) != "numeric") Pmax <- round(10*log10(length(lmdet$model[,1])))

  ref <- ncol(model.matrix(lmdet))
  ML <- ret(lmdet$model[,1], Pmax+1)[,-1]
  drop <- NULL; cond <- TRUE

  while(cond == TRUE){
    lmref <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML)
    #Nreg <- length(coef(lmref))
    #tstats <- coef(summary(lmref))[(ref+1):Nreg,3]; drop <- which(abs(tstats) < cvref)  **
    rcoefs <- na.omit(coef(summary(lmref))[,3]); Nreg <- length(coef(lmref))
    aux1 <- which(is.na(coef(lmref)[(ref+1):Nreg]))
    tstats <- rcoefs[(ref+1):length(rcoefs)]; drop <- c(aux1, which(abs(tstats) < cvref))

    if(length(aux1) == 0){
      cond <- FALSE
      aux <- names(data.frame(ML))[-drop]
      lags <- as.numeric(substr(aux, 5, nchar(aux)))
    }
    if(length(aux1) == ncol(ML)){
      cond <- FALSE
      lags <- numeric(0)
    }
    if(length(aux1) > 0 && length(drop) < ncol(ML)){
      aux <- dimnames(ML)[[2]][-drop]
      ML <- as.matrix(data.frame(ML[,-drop]))
      dimnames(ML)[[2]] <- aux
    }
  }
  lags
}

#polymake <- function(roots)
#{
#  coefs <- rep(NA, length(roots))
#  coefs[1] <- 1
#
#  coefs[2] <- -roots[1]
#  if(length(roots) > 1){
#    for(i in 2:length(roots)){
#      coefs[i+1] <- -coefs[i]*roots[i]
#      for(j in i:2)
#        coefs[j] <- coefs[j] - coefs[j-1]*roots[i]
#    }
#  }
#
#  #coefs <- c(1, coefs)
#  coefs <- coefs/coefs[length(coefs)]
#  #coefs <- round(coefs[c(length(coefs):1)], 2)  ##~ ver. al redondear una raíz 0.999 pasa a ser 1.
#  coefs <- Re(coefs[c(length(coefs):1)])  ## para posibles problemas de redondeo.
#  coefs
#}

elapsedtime <- function(ptm1, ptm2)
{
  elaps <- (ptm2 - ptm1)[1]  # en segundos

  if(elaps < 60)
         units <- "seconds"
  else if(elaps < 3600){
         elaps <- elaps/60
         units <- "minutes" }
  else if(elaps < 86400){
         elaps <- elaps/3600
         units <- "hours" }
  else { elaps <- elaps/86400
         units <- "days" }

  list(elaps=as.numeric(elaps), units=units)
}

#elapsedtime <- function(time1, time2)
#{
#  t1 <- as.numeric(unlist(strsplit(substring(time1, 9,19), ":")))
#  t2 <- as.numeric(unlist(strsplit(substring(time2, 9,19), ":")))
#
#  d1<- as.numeric(unlist(strsplit(substring(time1, 1,8), "/")))
#  d2<- as.numeric(unlist(strsplit(substring(time2, 1,8), "/")))
#
#  elaps <- ISOdatetime(year=d2[3],month=d2[1],day=d2[2], hour=t2[1], min=t2[2], sec=t2[3]) -
#           ISOdatetime(year=d1[3],month=d1[1],day=d1[2], hour=t1[1], min=t1[2], sec=t1[3])
#
#  if(length(which(d1 != d2)) == 0){
#    units <- c("hours", "minutes", "seconds")[which(t1 != t2)[1]]
#  } else
#      units <- c("years", "days", "months")[which(d1 != d2)[1]]
#
#  list(elaps=as.numeric(elaps), units=units)
#}

# Función para redondear las tabla que se exportan (usando cat...) permitiendo que el último
# decimal sea cero.
  # tabla es una coulumna de matriz de datos
  # colum es una columna de la tabla
  # digits es el número de decimales (se permite que el último sea cero)
# format indicando un valor de digits vale si es una matriz con todo números, no otos caracteres, **,...

Tround <- function(tabla, column, digits)
{
  catround <- function(x, digits)
  {
    rx <- as.character(round(x, digits=digits))

    rxchar <- rep(NA, nchar(rx))
    for(i in 1:nchar(rx))
      rxchar[i] <- substring(rx, i, i)

    if(length(which(rxchar==".")) == 0)
    {
       aux1 <- rep("0", digits); aux2 <- "0"
       for(i in 1:(digits-1))
          aux2 <-  paste(aux2, aux1[i], sep="")
       rx <- paste(rx, aux2, sep=".")
    }
    if(digits > 0 && length(which(rxchar==".")) > 0)
    {
      logic <- FALSE; i <- 1
      if(substr(rx, 2, 2) != "")
      {
        while(logic == FALSE){
          logic <- substr(rx, i, i) == "."
          i <- i+1
        }
        if(nchar(substr(rx, i, i+digits)) < digits){
           n0 <- digits - nchar(substr(rx, i, i+digits))
           aux1 <- rep("0", n0)
           for(i in 1:(n0-1))
              aux2 <-  paste(aux1[i], aux1[i+1], sep="")
           rx <- paste(rx, aux2, sep="")
        }
      }
      if(nchar(rx) == 1){
        aux1 <- rep("0", digits)
        for(i in 1:(digits-1))
          aux2 <-  paste(aux1[i], aux1[i+1], sep="")
        rx <- paste(rx, aux2, sep=".")
      }
    }
    rx
  }

  for(i in 1:nrow(tabla))
    tabla[i,column] <- catround(as.numeric(tabla[i,column]), digits)
  tabla
}

##~ Poner en help: regvar es cero, no se ha implementado este argumento aquí.

# ADF recursive test.

setClass("adfrecst", representation(wts="ts", type="character", nsub="numeric", itsd="numeric",
  regvar="numeric", selectlags="list", recstats="matrix", elaps="list"))

ADF.rectest <- function(wts, type="moving", nsub=48, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=2)
  code <- paste("DF", paste(itsd[1:2], collapse=""), "0", sep="")

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    Mst[i+1,1] <- ADF.test(wts=wtsss, itsd=itsd, regvar=0, selectlags=selectlags)@stat[,3]
    Mst[i+1,2] <- interpolpval(code=code, stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1)], xlim=c(1,iter+1), ylab="ADF statistic", xlab="Iteration", main="ADF statistics")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    Mst[nrow(Mst),1] <- ADF.test(wts=wts, itsd=itsd, regvar=0, selectlags=selectlags)@stat[,3]
    Mst[nrow(Mst),2] <- interpolpval(code=code, stat=Mst[nrow(Mst),1], N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=2, dimnames=list(subsdates, c("adf.stat", " ")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("adfrecst", wts=wts, type=type, nsub=nsub, itsd=itsd, regvar=0, selectlags=selectlags,
    recstats=Mst, elaps=elaps)
}

setMethod("show", "adfrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    sts <- Tround(matrix(as.numeric(object@recstats[,1])), column=1, digits=2)
    sts <- format(sts, justify="right")
    pvls <- format(object@recstats[,2], justify="left")

    smpls <- dimnames(object@recstats)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  --- --------- ----")
    cat("\n  ADF recursive test")
    cat("\n  --- --------- ----\n\n")
    cat("  ADF statistics in each subsample:\n\n")

    for(i in 1:iter)
      cat(" ", smpls[i], " ", sts[i], pvls[i], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="adfrecst", y="missing"),
  function(x,...)
  {
    plot(as.numeric(x@recstats[,1]), xlim=c(1,nrow(x@recstats)), ylab="ADF statistic",
      xlab="Iteration", main="ADF statistics", las=1)
  }
)

# KPSS recursive test.

setClass("kpssrecst", representation(wts="ts", type="character", nsub="numeric", ltrunc="maybeRegvar",
  recstats="matrix", elaps="list"))

KPSS.rectest <- function(wts, type="moving", nsub=48, ltrunc=NULL, trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=4)

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    out <- KPSS.test(wts=wtsss, ltrunc=ltrunc)
    Mst[i+1,1] <- out@levelst
    Mst[i+1,2] <- interpolpval(code="KPSSlevel", stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    Mst[i+1,3] <- out@trendst
    Mst[i+1,4] <- interpolpval(code="KPSStrend", stat=Mst[(i+1),3], N=length(wtsss), swarn=FALSE)$pvlab

    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(3,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1),1], xlim=c(1,iter+1),
        ylab="KPSS statistic", xlab="Iteration", main="KPSS Level.stat")
      plot(Mst[1:(i+1),2], xlim=c(1,iter+1),
        ylab="KPSS statistic", xlab="Iteration", main="KPSS Trend.stat")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    out <- KPSS.test(wts=wts, ltrunc=ltrunc)
    Mst[nrow(Mst),1] <- out@levelst
    Mst[nrow(Mst),2] <- interpolpval(code="KPSSlevel", stat=Mst[nrow(Mst),1],
                                     N=length(wts), swarn=FALSE)$pvlab
    Mst[nrow(Mst),3] <- out@trendst
    Mst[nrow(Mst),4] <- interpolpval(code="KPSStrend", stat=Mst[nrow(Mst),3],
                                     N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=4,
           dimnames=list(subsdates, c("level.stat", "pvl", "trend.stat", "pvl")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("kpssrecst", wts=wts, type=type, nsub=nsub, ltrunc=ltrunc, recstats=Mst, elaps=elaps)
}

setMethod("show", "kpssrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    stsl <- Tround(matrix(as.numeric(object@recstats[,1])), column=1, digits=2)
    stsl <- format(stsl, justify="right")
    pvll <- format(object@recstats[,2], justify="left")
    stst <- Tround(matrix(as.numeric(object@recstats[,3])), column=1, digits=2)
    stst <- format(stst, justify="right")
    pvlt <- format(object@recstats[,4], justify="left")

    smpls <- dimnames(object@recstats)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  ---- --------- ----")
    cat("\n  KPSS recursive test")
    cat("\n  ---- --------- ----\n\n")
    cat("  KPSS statistics in each subsample:\n\n")

    cat(rep(" ", nchar(smpls[1])/2), "    Level", "      Trend", "\n")
    for(i in 1:iter)
      cat(" ", smpls[i], " ", stsl[i], pvll[i], "  ", stst[i], pvlt[i], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="kpssrecst", y="missing"),
  function(x,...)
  {
    Mst <- x@recstats
    opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
    plot(as.numeric(Mst[,1]), xlim=c(1,nrow(Mst)),
      ylab="KPSS statistic", xlab="Iteration", main="KPSS Level.stat")
    plot(as.numeric(Mst[,3]), xlim=c(1,nrow(Mst)),
      ylab="KPSS statistic", xlab="Iteration", main="KPSS Trend.stat")
    par(opar)
  }
)

# HEGY recursive test.

setClass("hegyrecst", representation(wts="ts", type="character", nsub="numeric", itsd="numeric",
  regvar="numeric", selectlags="list", recstats="matrix", elaps="list"))

HEGY.rectest <- function(wts, type="moving", nsub=72, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)
  if(s==4)  pdim <- c(3,2)
  if(s==12) pdim <- c(5,2)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  if(s==4)  c1 <- "HEGY"
  if(s==12) c1 <- "BM"
  c2 <- paste(itsd[1:2], sep="", collapse="")
  ifelse(itsd[3] != 0, c3 <-1, c3 <-0)
  codeaux <- paste(c1, c2, c3, sep="")

  etfsnames <- c(paste("tpi_", 1:2, sep=""),
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

  out <- matrix(nrow=iter+lss, ncol=((s/2+3)*2))  #ncol=(s+s/2+1))  ##~ no poner tpi_>2
  subsdates <- rep(NA, nrow(out))

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    hegy.out <- HEGY.test(wts=wtsss, itsd=itsd, regvar=0, selectlags=selectlags)
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    out[i+1,c(1,3)] <- hegy.out@hegycoefs[1:2,3]  # hegy.out@hegycoefs[,3]
    out[i+1,2] <- interpolpval(code=paste(codeaux, "tpi1", sep=""), stat=out[i+1,1],
                               N=length(wtsss), swarn=FALSE)$pvlab
    out[i+1,4] <- interpolpval(code=paste(codeaux, "tpi2", sep=""), stat=out[i+1,3],
                               N=length(wtsss), swarn=FALSE)$pvlab

    out[i+1,seq(5,ncol(out),2)] <- hegy.out@stats[3:(s/2+3),1]
    codeF <- paste(codeaux, "Foddeven", sep="")
    for(j in seq(6,5+(s/2-1)*2,2))
      out[i+1,j] <- interpolpval(code=codeF, stat=out[i+1,j-1], N=length(wtsss), swarn=FALSE)$pvlab

    if(trace[2] == 1){
      opar <- par(mfrow=pdim, mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      k<-1
      for(j in seq(1,ncol(out),2)){
        plot(out[1:(i+1),j], xlim=c(1,iter+1), ylab="", xlab="Iteration",
          main=paste(etfsnames[k], "statistics", sep=" "))
        k<-k+1
      }
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    hegy.out <- HEGY.test(wts=wts, itsd=itsd, regvar=0, selectlags=selectlags)
    subsdates[iter+lss] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")

    out[nrow(out),c(1,3)] <- hegy.out@hegycoefs[1:2,3]  # hegy.out@hegycoefs[,3]
    out[nrow(out),2] <- interpolpval(code=paste(codeaux, "tpi1", sep=""),
                          stat=out[nrow(out),1], N=length(wts), swarn=FALSE)$pvlab
    out[nrow(out),4] <- interpolpval(code=paste(codeaux, "tpi2", sep=""),
                          stat=out[nrow(out),3], N=length(wts), swarn=FALSE)$pvlab

    out[nrow(out),seq(5,ncol(out),2)] <- hegy.out@stats[3:(s/2+3),1]
    codeF <- paste(codeaux, "Foddeven", sep="")
    for(j in seq(6,5+(s/2-1)*2,2))
      out[nrow(out),j] <- interpolpval(code=codeF, stat=out[nrow(out),j-1],
                                       N=length(wts), swarn=FALSE)$pvlab
  }

  etfsn <- rep(NA, ncol(out))
  etfsn[seq(1,ncol(out),2)] <- etfsnames
  etfsn[seq(2,ncol(out),2)] <- " "
  out <- matrix(out, nrow=nrow(out), ncol=ncol(out), dimnames=list(subsdates, etfsn))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("hegyrecst", wts=wts, type=type, nsub=nsub, itsd=itsd, regvar=0, selectlags=selectlags,
    recstats=out, elaps=elaps)
}

setMethod("show", "hegyrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps; s <- frequency(object@wts)
    out <- object@recstats[,1:((s/2+1)*2)]      # Sin tomar F_2:s, F_1:s
    aux1 <- seq(1,ncol(out),2); aux2 <- aux1+1  # aux2 <- seq(2,ncol(out),2)
    auxout <- matrix(as.numeric(out[,aux1]), ncol=ncol(out)/2)
    for(i in 1:ncol(auxout))
      auxout <- Tround(auxout, column=i, digits=2)
    out[,aux1] <- format(auxout, justify="right")
    out[,aux2] <- format(out[,aux2], justify="left")

    smpls <- dimnames(out)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    out2 <- matrix(ncol=ncol(out), nrow=nrow(out)+1)
    out2[1,aux1] <- dimnames(out)[[2]][aux1]
    out2[1,aux2] <- "   "
    out2[2:nrow(out2),] <- out
    out2[,aux1] <- format(out2[,aux1], justify="right")

    cat("\n  ---- --------- ----")
    cat("\n  HEGY recursive test")
    cat("\n  ---- --------- ----\n\n")
    cat("  HEGY statistics in each subsample:\n\n")

    cat(rep(" ", nchar(smpls[1])/2+1), out2[1,1:4], "\n")
    for(i in 2:(iter+1))
      cat(" ", smpls[i-1], " ", out2[i,1:4], "\n")
    if(ncol(out2) > 4){
      cat("\n", rep(" ", nchar(smpls[1])/2+1), out2[1,5:ncol(out2)], "\n")
      for(i in 2:(iter+1))
        cat(" ", smpls[i-1], " ", out2[i,5:ncol(out2)], "\n")
    }

    cat("\n\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="hegyrecst", y="missing"),
  function(x,...)
  {
    out <- x@recstats; s <- frequency(x@wts)
    if(frequency(x@wts)==4)  pdim <- c(3,2)
    if(frequency(x@wts)==12) pdim <- c(5,2)
    etfsnames <- c(paste("tpi_", 1:2, sep=""),
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

    opar <- par(mfrow=pdim, mar=c(3,4.2,2.5,2), las=1)
    k<-1
    for(j in seq(1,ncol(x@recstats),2)){
      plot(out[,j], xlim=c(1,nrow(out)), ylab="", xlab="Iteration",
        main=paste(etfsnames[k], "statistics", sep=" "))
      k<-k+1
    }
    par(opar)
  }
)


setClass("chrecst", representation(wts="ts", type="character", nsub="numeric", frec="numeric", f0="numeric",
  DetTr="logical", ltrunc="maybeRegvar", recstats="matrix", elaps="list"))

CH.rectest <- function(wts, type="moving", nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)
  if(class(frec)=="NULL") frec <- rep(1, s/2)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  code <- paste("CHp", sum(frec*c(rep(2, s/2-1),1)), sep="")
  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=2)

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    Mst[i+1,1] <- CH.test(wts=wtsss, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc)@stat
    Mst[i+1,2] <- interpolpval(code=code, stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1),1], xlim=c(1,iter+1), ylab="CH statistic", xlab="Iteration", main="CH statistics")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    Mst[nrow(Mst),1] <- CH.test(wts=wts, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc)@stat
    Mst[nrow(Mst),2] <- interpolpval(code=code, stat=Mst[nrow(Mst),1], N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=2, dimnames=list(subsdates, c("ch.stat", " ")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("chrecst", wts=wts, type=type, nsub=nsub, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc,
    recstats=Mst, elaps=elaps)
}

setMethod("show", "chrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    out <- matrix(c(as.numeric(object@recstats[,1]), object@recstats[,2]),
             ncol=2, dimnames=dimnames(object@recstats))
    out <- Tround(out, column=1, digits=2)
    out[,1] <- format(out[,1], justify="right")
    out[,2] <- format(out[,2], justify="left")

    smpls <- dimnames(out)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  -- --------- ----")
    cat("\n  CH recursive test")
    cat("\n  -- --------- ----\n\n")
    cat("  CH statistics in each subsample:\n\n")

    for(i in 1:iter)
      cat(" ", smpls[i], " ", out[i,], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="chrecst", y="missing"),
  function(x,...)
  {
    out <- x@recstats
    plot(out[,1], xlim=c(1,nrow(out)),
      ylab="CH statistic", xlab="Iteration", main="CH statistics", las=1)
  }
)

setClass("vdate", representation(input="numeric", output="matrix", wts="ts",
  ys="matrix", obs="matrix", Myso="matrix"))

Mdates <- function(wts, yso)
{
  ys <- as.integer(time(wts))
  seas <- cycle(wts)@.Data

  Myso <- matrix(nrow=length(seas), ncol=4,
          dimnames=list(c(1:length(seas)), c("Year", "Season", "Observation", "ys")))
  Myso[,1] <- ys
  Myso[,2] <- seas
  Myso[,3] <- c(1:nrow(Myso))
  Myso[,4] <- paste(ys, seas, sep=":")

  if(length(yso) == 2){
    ys <- matrix(t(yso), ncol=2, dimnames=list("", c("Year", "Season")))
    obs <- out <- which(Myso[,4] == paste(yso[1], yso[2], sep=":"))
    if(length(out) != 0){
      obs <- matrix(obs, dimnames=list("", "Observation")) }
  }
  if(length(yso) == 1){
    obs <- matrix(yso, dimnames=list("", "Observation"))
    ys <- out <- Myso[which(Myso[,3] == yso),1:2]
    #if(nrow(out) != 0)
    #  dimnames(ys)[[1]] <- ""
  }

  new("vdate", input=as.numeric(yso), output=as.matrix(t(out)), wts=wts,
    ys=matrix(ys), obs=matrix(obs), Myso=Myso)
}

setMethod("show", "vdate",
  function(object)
  {
    input <- object@input
    output <- object@output

    if(length(input) == 1){
      if(length(output) == 0){
        cat("  Observation", as.numeric(object@obs), "is out of sample.\n")
      } else{
          refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
          cat("  Observation", as.numeric(object@obs), "corresponds to date", refys, ".\n")
        }
    }
    if(length(input) == 2){
      refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
      if(length(output) == 0){
        cat("  Date", refys, "is out of sample.\n")
      } else{
          refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
          cat("  Date", refys, "corresponds to observation", as.numeric(object@obs), ".\n")
        }
    }
  }
)

setMethod("as.numeric", "vdate",
  function(x,...)
  {
    out <- x@output
    out
  }
)

as.vdate <- function(object, yso=1){
}
setMethod("as.vdate", "ts",
  function(object, yso)
  {
    #Mdates(object, yso=start(object))
    Mdates(object, yso=yso)
  }
)

stepdate <- function(object, step=1){
}
setMethod("stepdate", "vdate",
  function(object, step)
  {
    obs0 <- object@obs
    if(obs0+step > length(object@wts))
      stop("Observation ", obs0+step, " is out of sample.\n")
    ys0 <- object@ys
    Myso <- object@Myso

    obsout <- matrix(obs0+step, dimnames=list("", "Observation"))

    ref <- which(Myso[,4] == paste(ys0[1], ys0[2], sep=":"))
    ysout <- Myso[ref+step,4]
    ysout <- matrix(unlist(strsplit(ysout, ":")), nrow=1,
             dimnames=list("", c("Year", "Season")))

    new("vdate", input=object@input, output=object@output, wts=object@wts,
      ys=ysout, obs=obsout, Myso=object@Myso)
  }
)

interpolpval <- function(code, stat, N, swarn=TRUE)
{
  table <- lookupCVtable(code)
  alphas <- dimnames(table)[[2]]
  Nref <- as.numeric(dimnames(table)[[1]]) # table[1:nrow(table),1]
  pvlref <- 0

  selrow <- rep(NA, ncol(table))
  for (i in 1:ncol(table))
    selrow[i] <- approx(Nref, table[,i], N, rule = 2)$y

  pval <- approx(selrow, alphas, stat, rule = 2)$y
  if(is.na(approx(selrow, alphas, stat, rule = 1)$y))
  {
    if(substr(code, 1,2) == "DF" || substr(code, 6,8) == "tpi" || substr(code, 8,10) == "tpi")
    {
      if(pval == min(as.numeric(alphas))){
        if(swarn==TRUE)
          warning("p-value is smaller than printed p-value")
        pvalw <- paste("<", format.pval(pval), sep=" ")
      } else{
          if(swarn==TRUE)
            warning("p-value is greater than printed p-value")
          pvalw <- paste(">", format.pval(pval), sep=" ")
      }
    }
    if(substr(code, 1,2) == "KP" || substr(code, 1,2) == "CH" ||
       substr(code, 6,9) == "Fodd" || substr(code, 8,11) == "Fodd")
    {
      if(pval == min(as.numeric(alphas))){
        if(swarn==TRUE)
          warning("p-value is smaller than printed p-value")
        pvalw <- paste("<", format.pval(pval), sep=" ")
      } else{
          if(swarn==TRUE)
            warning("p-value is greater than printed p-value")
          pvalw <- paste(">", format.pval(pval), sep=" "); pvlref <- -1
      }
    }
  } else
    pvalw <- format.pval(pval)

  ref1 <- c(1, 0.1, 0.05, 0.025, 0.01, 0.001)
  ref2 <- c(" ", ".", "*", "**", "***")
  #ref2 <- c("   ", ".  ", "*  ", "** ", "***")
  pvl <- ref2[length(which(pval <= ref1))+pvlref]

  list(pval=pval, pvalw=as.character(pvalw), pvlab=pvl)
}

lookupCVtable <- function(code)
{
  if(code=="DF000")
  {
    table <- data.frame(CV1 = -c(2.66, 2.62, 2.60, 2.58, 2.58, 2.58),
                        CV2 = -c(2.26, 2.25, 2.24, 2.24, 2.23, 2.23),
                        CV3 = -c(1.95, 1.95, 1.95, 1.95, 1.95, 1.95),
                        CV4 = -c(1.60, 1.61, 1.61, 1.62, 1.62, 1.62))
    dimnames(table) <- list(c(25, 50, 100, 250, 500, 100000),
                            c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="DF100")
  {
    table <- data.frame(CV1 = -c(3.75, 3.58, 3.51, 3.46, 3.44, 3.43),
                        CV2 = -c(3.33, 3.22, 3.17, 3.14, 3.13, 3.12),
                        CV3 = -c(3.00, 2.93, 2.89, 2.88, 2.87, 2.86),
                        CV4 = -c(2.63, 2.60, 2.58, 2.57, 2.57, 2.57))
    dimnames(table) <- list(c(25, 50, 100, 250, 500, 100000),
                            c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="DF110")
  {
    table <- data.frame(CV1 = -c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96),
                        CV2 = -c(3.95, 3.80, 3.73, 3.69, 3.68, 3.66),
                        CV3 = -c(3.60, 3.50, 3.45, 3.43, 3.42, 3.41),
                        CV4 = -c(3.24, 3.18, 3.15, 3.13, 3.13, 3.12))
    dimnames(table) <- list(c(25, 50, 100, 250, 500, 100000),
                            c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code == "KPSSlevel")
  {
    table <- data.frame(rbind(c(0.347, 0.463, 0.574, 0.739),
                              c(0.347, 0.463, 0.574, 0.739)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }

  if(code == "KPSStrend")
  {
    table <- data.frame(rbind(c(0.119, 0.146, 0.176, 0.216),
                              c(0.119, 0.146, 0.176, 0.216)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }

  if(code=="BM000tpi1")
  {
    table <- data.frame(CV1 = c(-2.51, -2.52, -2.57),
                        CV2 = c(-2.18, -2.21, -2.24),
                        CV3 = c(-1.89, -1.91, -1.95),
                        CV4 = c(-1.58, -1.59, -1.62))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM100tpi1")
  {
    table <- data.frame(CV1 = c(-3.35, -3.40, -3.41),
                        CV2 = c(-3.06, -3.11, -3.12),
                        CV3 = c(-2.80, -2.85, -2.86),
                        CV4 = c(-2.51, -2.55, -2.57))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM101tpi1")
  {
    table <- data.frame(CV1 = c(-3.32, -3.37, -3.41),
                        CV2 = c(-3.02, -3.06, -3.12),
                        CV3 = c(-2.76, -2.81, -2.86),
                        CV4 = c(-2.47, -2.53, -2.57))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM110tpi1")
  {
    table <- data.frame(CV1 = c(-3.87, -3.92, -3.97),
                        CV2 = c(-3.58, -3.63, -3.67),
                        CV3 = c(-3.32, -3.37, -3.40),
                        CV4 = c(-3.06, -3.09, -3.12))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM111tpi1")
  {
    table <- data.frame(CV1 = c(-3.83, -3.85, -3.97),
                        CV2 = c(-3.54, -3.57, -3.67),
                        CV3 = c(-3.28, -3.32, -3.40),
                        CV4 = c(-2.99, -3.04, -3.12))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="BM000tpi2")
  {
    table <- data.frame(CV1 = c(-2.53, -2.52, -2.57),
                        CV2 = c(-2.16, -2.20, -2.24),
                        CV3 = c(-1.87, -1.91, -1.95),
                        CV4 = c(-1.57, -1.59, -1.62))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM100tpi2")
  {
    table <- data.frame(CV1 = c(-2.48, -2.54, -2.57),
                        CV2 = c(-2.15, -2.20, -2.24),
                        CV3 = c(-1.89, -1.91, -1.95),
                        CV4 = c(-1.57, -1.59, -1.62))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM101tpi2")
  {
    table <- data.frame(CV1 = c(-3.28, -3.37, -3.41),
                        CV2 = c(-3.01, -3.07, -3.12),
                        CV3 = c(-2.76, -2.81, -2.86),
                        CV4 = c(-2.48, -2.52, -2.57))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM110tpi2")
  {
    table <- data.frame(CV1 = c(-2.52, -2.55, -2.57),
                        CV2 = c(-2.18, -2.20, -2.24),
                        CV3 = c(-1.88, -1.93, -1.95),
                        CV4 = c(-1.55, -1.60, -1.62))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM111tpi2")
  {
    table <- data.frame(CV1 = c(-3.31, -3.40, -3.41),
                        CV2 = c(-3.02, -3.08, -3.12),
                        CV3 = c(-2.75, -2.84, -2.86),
                        CV4 = c(-2.47, -2.54, -2.57))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="BM000tpi3" || code=="BM000tpi5" || code=="BM000tpi7" || code=="BM000tpi9" || code=="BM000tpi11")
  {
    table <- data.frame(CV1 = -c(2.50, 2.52, 2.56),
                        CV2 = -c(2.16, 2.18, 2.23),
                        CV3 = -c(1.88, 1.90, 1.95),
                        CV4 = -c(1.55, 1.57, 1.59))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM100tpi3" || code=="BM100tpi5" || code=="BM100tpi7" || code=="BM100tpi9" || code=="BM100tpi11")
  {
    table <- data.frame(CV1 = -c(2.51, 2.56, 2.56),
                        CV2 = -c(2.16, 2.20, 2.23),
                        CV3 = -c(1.87, 1.90, 1.95),
                        CV4 = -c(1.54, 1.57, 1.59))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM101tpi3" || code=="BM101tpi5" || code=="BM101tpi7" || code=="BM101tpi9" || code=="BM101tpi11")
  {
    table <- data.frame(CV1 = -c(3.83, 3.86, 3.91),
                        CV2 = -c(3.51, 3.55, 3.61),
                        CV3 = -c(3.25, 3.29, 3.35),
                        CV4 = -c(2.95, 2.99, 3.05))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM110tpi3" || code=="BM110tpi5" || code=="BM110tpi7" || code=="BM110tpi9" || code=="BM110tpi11")
  {
    table <- data.frame(CV1 = -c(2.49, 2.53, 2.56),
                        CV2 = -c(2.16, 2.20, 2.23),
                        CV3 = -c(1.88, 1.91, 1.95),
                        CV4 = -c(1.54, 1.57, 1.59))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="BM111tpi3" || code=="BM111tpi5" || code=="BM111tpi7" || code=="BM111tpi9" || code=="BM111tpi11")
  {
    table <- data.frame(CV1 = -c(3.79, 3.85, 3.91),
                        CV2 = -c(3.50, 3.55, 3.61),
                        CV3 = -c(3.24, 3.29, 3.35),
                        CV4 = -c(2.95, 3.00, 3.05))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="BM000tpi4" || code=="BM000tpi6" || code=="BM000tpi8" || code=="BM000tpi10" || code=="BM000tpi12")
  {
    table <- data.frame(CV1 = c(-2.31, -2.33, -2.30),
                        CV2 = c(-1.95, -1.96, -1.94),
                        CV3 = c(-1.63, -1.65, -1.63),
                        CV4 = c(-1.27, -1.28, -1.28),
                        CV5 = c(1.25, 1.27, 1.27),
                        CV6 = c(1.61, 1.63, 1.63),
                        CV7 = c(1.93, 1.94, 1.94),
                        CV8 = c(2.29, 2.32, 2.32))
    dimnames(table) <- list(c(240, 480, 10000),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="BM100tpi4" || code=="BM100tpi6" || code=="BM100tpi8" || code=="BM100tpi10" || code=="BM100tpi12")
  {
    table <- data.frame(CV1 = c(-2.30, -2.32, -2.30),
                        CV2 = c(-1.93, -1.95, -1.94),
                        CV3 = c(-1.62, -1.63, -1.63),
                        CV4 = c(-1.27, -1.27, -1.28),
                        CV5 = c(1.24, 1.27, 1.27),
                        CV6 = c(1.60, 1.62, 1.63),
                        CV7 = c(1.91, 1.93, 1.94),
                        CV8 = c(2.28, 2.30, 2.32))
    dimnames(table) <- list(c(240, 480, 10000),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="BM101tpi4" || code=="BM101tpi6" || code=="BM101tpi8" || code=="BM101tpi10" || code=="BM101tpi12")
  {
    table <- data.frame(CV1 = c(-2.61, -2.65, -2.72),
                        CV2 = c(-2.21, -2.25, -2.31),
                        CV3 = c(-1.85, -1.90, -1.95),
                        CV4 = c(-1.45, -1.49, -1.54),
                        CV5 = c(1.46, 1.49, 1.53),
                        CV6 = c(1.86, 1.91, 1.95),
                        CV7 = c(2.20, 2.25, 2.30),
                        CV8 = c(2.60, 2.63, 2.72))
    dimnames(table) <- list(c(240, 480, 10000),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="BM110tpi4" || code=="BM110tpi6" || code=="BM110tpi8" || code=="BM110tpi10" || code=="BM110tpi12")
  {
    table <- data.frame(CV1 = c(-2.28, -2.30, -2.30),
                        CV2 = c(-1.93, -1.94, -1.94),
                        CV3 = c(-1.61, -1.63, -1.63),
                        CV4 = c(-1.25, -1.27, -1.28),
                        CV5 = c(1.24, 1.25, 1.27),
                        CV6 = c(1.59, 1.61, 1.63),
                        CV7 = c(1.90, 1.92, 1.94),
                        CV8 = c(2.26, 2.28, 2.32))
    dimnames(table) <- list(c(240, 480, 10000),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="BM111tpi4" || code=="BM111tpi6" || code=="BM111tpi8" || code=="BM111tpi10" || code=="BM111tpi12")
  {
    table <- data.frame(CV1 = c(-2.57, -2.66, -2.72),
                        CV2 = c(-2.18, -2.27, -2.31),
                        CV3 = c(-1.85, -1.91, -1.95),
                        CV4 = c(-1.45, -1.49, -1.54),
                        CV5 = c(1.45, 1.49, 1.53),
                        CV6 = c(1.86, 1.90, 1.95),
                        CV7 = c(2.19, 2.25, 2.30),
                        CV8 = c(2.60, 2.64, 2.72))
    dimnames(table) <- list(c(240, 480, 10000),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }

  if(code=="BM000Foddeven")
  {
    table <- data.frame(CV1 = c(2.34, 2.38, 2.40),
                        CV2 = c(3.03, 3.08, 3.10),
                        CV3 = c(3.71, 3.78, 3.79),
                        CV4 = c(4.60, 4.70, 4.68))
    dimnames(table) <- list(c(240, 480, 10000), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="BM100Foddeven")
  {
    table <- data.frame(CV1 = c(2.32, 2.36, 2.40),
                        CV2 = c(3.01, 3.06, 3.10),
                        CV3 = c(3.68, 3.76, 3.79),
                        CV4 = c(4.60, 4.66, 4.68))
    dimnames(table) <- list(c(240, 480, 10000), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="BM101Foddeven")
  {
    table <- data.frame(CV1 = c(5.27, 5.42, 5.64),
                        CV2 = c(6.26, 6.42, 6.67),
                        CV3 = c(7.19, 7.38, 7.63),
                        CV4 = c(8.35, 8.60, 8.79))
    dimnames(table) <- list(c(240, 480, 10000), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="BM110Foddeven")
  {
    table <- data.frame(CV1 = c(2.30, 2.36, 2.40),
                        CV2 = c(2.97, 3.05, 3.10),
                        CV3 = c(3.64, 3.72, 3.79),
                        CV4 = c(4.53, 4.62, 4.68))
    dimnames(table) <- list(c(240, 480, 10000), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="BM111Foddeven")
  {
    table <- data.frame(CV1 = c(5.25, 5.44, 5.64),
                        CV2 = c(6.23, 6.43, 6.67),
                        CV3 = c(7.14, 7.35, 7.63),
                        CV4 = c(8.33, 8.52, 8.79))
    dimnames(table) <- list(c(240, 480, 10000), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="HEGY000tpi1")
  {
    table <- data.frame(CV1 = c(-2.72, -2.60, -2.62, -2.62),
                        CV2 = c(-2.29, -2.26, -2.25, -2.23),
                        CV3 = c(-1.95, -1.97, -1.93, -1.94),
                        CV4 = c(-1.59, -1.61, -1.59, -1.62))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY100tpi1")
  {
    table <- data.frame(CV1 = c(-3.66, -3.47, -3.51, -3.48),
                        CV2 = c(-3.25, -3.14, -3.17, -3.13),
                        CV3 = c(-2.96, -2.88, -2.89, -2.87),
                        CV4 = c(-2.62, -2.58, -2.58, -2.57))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY101tpi1")
  {
    table <- data.frame(CV1 = c(-3.77, -3.55, -3.56, -3.51),
                        CV2 = c(-3.39, -3.22, -3.23, -3.18),
                        CV3 = c(-3.08, -2.95, -2.94, -2.91),
                        CV4 = c(-2.72, -2.63, -2.62, -2.59))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY110tpi1")
  {
    table <- data.frame(CV1 = c(-4.23, -4.07, -4.09, -4.05),
                        CV2 = c(-3.85, -3.73, -3.75, -3.70),
                        CV3 = c(-3.56, -3.47, -3.46, -3.44),
                        CV4 = c(-3.21, -3.16, -3.16, -3.15))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY111tpi1")
  {
    table <- data.frame(CV1 = c(-4.46, -4.09, -4.15, -4.05),
                        CV2 = c(-4.04, -3.80, -3.80, -3.74),
                        CV3 = c(-3.71, -3.53, -3.52, -3.49),
                        CV4 = c(-3.37, -3.22, -3.21, -3.18))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="HEGY000tpi2")
  {
    table <- data.frame(CV1 = c(-2.67, -2.61, -2.60, -2.60),
                        CV2 = c(-2.27, -222, -2.23, -2.24),
                        CV3 = c(-1.95, -1.92, -1.94, -1.95),
                        CV4 = c(-1.60, -1.57, -1.61, -1.61))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY100tpi2")
  {
    table <- data.frame(CV1 = c(-2.68, -2.61, -2.60, -2.58),
                        CV2 = c(-2.27, -2.24, -2.21, -2.22),
                        CV3 = c(-1.95, -1.95, -1.91, -1.92),
                        CV4 = c(-1.60, -1.60, -1.58, -1.59))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY101tpi2")
  {
    table <- data.frame(CV1 = c(-3.75, -3.60, -3.49, -3.50),
                        CV2 = c(-3.37, -3.22, -3.15, -3.16),
                        CV3 = c(-3.04, -2.94, -2.90, -2.89),
                        CV4 = c(-2.69, -2.63, -2.59, -2.60))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY110tpi2")
  {
    table <- data.frame(CV1 = c(-2.65, -2.58, -2.65, -2.59),
                        CV2 = c(-2.24, -2.24, -2.25, -2.25),
                        CV3 = c(-1.91, -1.94, -1.96, -1.95),
                        CV4 = c(-1.57, -1.60, -1.63, -1.62))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY111tpi2")
  {
    table <- data.frame(CV1 = c(-3.80, -3.60, -3.57, -3.52),
                        CV2 = c(-3.41, -3.22, -3.18, -3.18),
                        CV3 = c(-3.08, -2.94, -2.93, -2.91),
                        CV4 = c(-2.73, -2.63, -2.61, -2.60))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="HEGY000tpi3")
  {
    table <- data.frame(CV1 = -c(2.66, 2.55, 2.58, 2.58),
                        CV2 = -c(2.23, 2.18, 2.21, 2.24),
                        CV3 = -c(1.93, 1.90, 1.92, 1.92),
                        CV4 = -c(1.52, 1.53, 1.56, 1.55))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY100tpi3")
  {
    table <- data.frame(CV1 = -c(2.64, 2.23, 1.90, 1.52),
                        CV2 = -c(2.61, 2.23, 1.90, 1.54),
                        CV3 = -c(2.53, 2.18, 1.88, 1.53),
                        CV4 = -c(2.57, 2.21, 1.90, 1.53))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY101tpi3")
  {
    table <- data.frame(CV1 = -c(4.31, 4.06, 4.06, 4.00),
                        CV2 = -c(3.92, 3.72, 3.72, 3.67),
                        CV3 = -c(3.61, 3.44, 3.44, 3.38),
                        CV4 = -c(3.24, 3.14, 3.11, 3.07))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY110tpi3")
  {
    table <- data.frame(CV1 = -c(2.68, 2.56, 2.56, 2.58),
                        CV2 = -c(2.27, 2.19, 2.20, 2.21),
                        CV3 = -c(1.92, 1.89, 1.90, 1.92),
                        CV4 = -c(1.52, 1.54, 1.52, 1.56))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }
  if(code=="HEGY111tpi3")
  {
    table <- data.frame(CV1 = -c(4.46, 4.12, 4.05, 4.04),
                        CV2 = -c(4.02, 3.76, 3.72, 3.69),
                        CV3 = -c(3.66, 3.48, 3.44, 3.41),
                        CV4 = -c(3.28, 3.14, 3.12, 3.10))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.01", "0.025", "0.05", "0.10"))
  }

  if(code=="HEGY000tpi4")
  {
    table <- data.frame(CV1 = c(-2.51, -2.43, -2.44, -2.43),
                        CV2 = c(-2.11, -2.01, -1.99, -1.98),
                        CV3 = c(-1.76, -1.68, -1.68, -1.65),
                        CV4 = c(-1.35, -1.32, -1.31, -1.30),
                        CV5 = c(1.33, 1.31, 1.30, 1.29),
                        CV6 = c(1.72, 1.67, 1.66, 1.67),
                        CV7 = c(2.05, 2.00, 1.99, 1.97),
                        CV8 = c(2.49, 2.40, 2.38, 2.36))
    dimnames(table) <- list(c(48, 100, 136, 200),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="HEGY100tpi4")
  {
    table <- data.frame(CV1 = c(-2.44, -2.38, -2.36, -2.36),
                        CV2 = c(-2.06, -1.99, -1.98, -1.98),
                        CV3 = c(-1.72, -1.68, -1.68, -1.66),
                        CV4 = c(-1.33, -1.30, -1.31, -1.29),
                        CV5 = c(1.30, 1.28, 1.27, 1.28),
                        CV6 = c(1.68, 1.65, 1.65, 1.65),
                        CV7 = c(2.04, 1.97, 1.97, 1.96),
                        CV8 = c(2.41, 2.32, 2.31, 2.30))
    dimnames(table) <- list(c(48, 100, 136, 200),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="HEGY101tpi4")
  {
    table <- data.frame(CV1 = c(-2.86, -2.78, -2.72, -2.74),
                        CV2 = c(-2.37, -2.32, -2.31, -2.33),
                        CV3 = c(-1.98, -1.96, -1.96, -1.96),
                        CV4 = c(-1.53, -1.53, -1.52, -1.54),
                        CV5 = c(1.54, 1.52, 1.51, 1.53),
                        CV6 = c(1.96, 1.93, 1.92, 1.95),
                        CV7 = c(2.35, 2.29, 2.28, 2.32),
                        CV8 = c(2.81, 2.73, 2.71, 2.78))
    dimnames(table) <- list(c(48, 100, 136, 200),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="HEGY110tpi4")
  {
    table <- data.frame(CV1 = c(-2.41, -2.38, -2.36, -2.35),
                        CV2 = c(-2.05, -1.97, -1.97, -1.97),
                        CV3 = c(-1.70, -1.65, -1.64, -1.66),
                        CV4 = c(-1.33, -1.28, -1.29, -1.29),
                        CV5 = c(1.26, 1.28, 1.26, 1.26),
                        CV6 = c(1.64, 1.65, 1.62, 1.64),
                        CV7 = c(1.96, 1.98, 1.92, 1.96),
                        CV8 = c(2.37, 2.32, 2.31, 2.30))
    dimnames(table) <- list(c(48, 100, 136, 200),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }
  if(code=="HEGY111tpi4")
  {
    table <- data.frame(CV1 = c(-2.75, -2.76, -2.71, -2.65),
                        CV2 = c(-2.26, -2.32, -2.78, -2.27),
                        CV3 = c(-1.91, -1.94, -1.94, -1.92),
                        CV4 = c(-1.48, -1.51, -1.51, -1.48),
                        CV5 = c(1.51, 1.51, 1.53, 1.55),
                        CV6 = c(1.97, 1.92, 1.96, 1.97),
                        CV7 = c(2.34, 2.28, 2.31, 2.31),
                        CV8 = c(2.78, 2.69, 2.78, 2.71))
    dimnames(table) <- list(c(48, 100, 136, 200),
                            c("0.01", "0.025", "0.05", "0.10", "0.90", "0.95", "0.975", "0.99"))
  }

  if(code=="HEGY000Foddeven")
  {
    table <- data.frame(CV1 = c(2.45, 2.39, 2.41, 2.42),
                        CV2 = c(3.26, 3.12, 3.14, 3.16),
                        CV3 = c(4.04, 3.89, 3.86, 3.92),
                        CV4 = c(5.02, 4.89, 4.81, 4.81))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="HEGY100Foddeven")
  {
    table <- data.frame(CV1 = c(2.32, 2.35, 2.36, 2.37),
                        CV2 = c(3.04, 3.08, 3.00, 3.12),
                        CV3 = c(3.78, 3.81, 3.70, 3.86),
                        CV4 = c(4.78, 4.77, 4.73, 4.76))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="HEGY101Foddeven")
  {
    table <- data.frame(CV1 = c(5.50, 5.56, 5.56, 5.56),
                        CV2 = c(6.60, 6.57, 6.63, 6.61),
                        CV3 = c(7.68, 7.72, 7.66, 7.53),
                        CV4 = c(9.22, 8.74, 8.92, 8.93))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="HEGY110Foddeven")
  {
    table <- data.frame(CV1 = c(2.23, 2.31, 2.33, 2.34),
                        CV2 = c(2.95, 2.98, 3.04, 3.07),
                        CV3 = c(3.70, 3.71, 3.69, 3.76),
                        CV4 = c(4.64, 4.70, 4.57, 4.66))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="HEGY111Foddeven")
  {
    table <- data.frame(CV1 = c(5.37, 5.52, 5.55, 5.56),
                        CV2 = c(6.55, 6.60, 6.62, 6.57),
                        CV3 = c(7.70, 7.52, 7.59, 7.56),
                        CV4 = c(9.27, 8.79, 8.77, 8.96))
    dimnames(table) <- list(c(48, 100, 136, 200), c("0.10", "0.05", "0.025", "0.01"))
  }

  if(code=="CHp1")
  {
    table <- data.frame(rbind(c(0.353, 0.470, 0.593, 0.748),
                              c(0.353, 0.470, 0.593, 0.748)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp2")
  {
    table <- data.frame(rbind(c(0.610, 0.749, 0.898, 1.070),
                              c(0.610, 0.749, 0.898, 1.070)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp3")
  {
    table <- data.frame(rbind(c(0.846, 1.010, 1.160, 1.350),
                              c(0.846, 1.010, 1.160, 1.350)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp4")
  {
    table <- data.frame(rbind(c(1.07, 1.24, 1.39, 1.60),
                              c(1.07, 1.24, 1.39, 1.60)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp5")
  {
    table <- data.frame(rbind(c(1.28, 1.47, 1.63, 1.88),
                              c(1.28, 1.47, 1.63, 1.88)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp6")
  {
    table <- data.frame(rbind(c(1.49, 1.68, 1.89, 2.12),
                              c(1.49, 1.68, 1.89, 2.12)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp7")
  {
    table <- data.frame(rbind(c(1.69, 1.90, 2.10, 2.35),
                              c(1.69, 1.90, 2.10, 2.35)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp8")
  {
    table <- data.frame(rbind(c(1.89, 2.11, 2.33, 2.59),
                              c(1.89, 2.11, 2.33, 2.59)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp9")
  {
    table <- data.frame(rbind(c(2.10, 2.32, 2.55, 2.82),
                              c(2.10, 2.32, 2.55, 2.82)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp10")
  {
    table <- data.frame(rbind(c(2.29, 2.54, 2.76, 3.05),
                              c(2.29, 2.54, 2.76, 3.05)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp11")
  {
    table <- data.frame(rbind(c(2.49, 2.75, 2.99, 3.27),
                              c(2.49, 2.75, 2.99, 3.27)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }
  if(code=="CHp12")
  {
    table <- data.frame(rbind(c(2.69, 2.96, 3.18, 2.51),
                              c(2.69, 2.96, 3.18, 2.51)))
    dimnames(table) <- list(c("100000","100001"), c("0.10", "0.05", "0.025", "0.01"))
  }

  table
}

urt.xtable <- function(x, caption=NULL, label=NULL, align=NULL, vsep=NULL, digits=NULL, display=NULL,...){
}
save.xtable <- function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...){
}

## ADF

setMethod("urt.xtable", "adfstat",
  function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(x@stat, ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(x@stat))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@stat), t(x@lagcoefs))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(x@stat), t(x@lagcoefs))), ncol=4, byrow=TRUE)

    regnames <- c(dimnames(x@regvarcoefs)[[1]], "adf.stat", dimnames(x@lagcoefs)[[1]])
    cnames <- c("Estimate" , "Std.Error", "t-stat", "p-value")
    Mout <- matrix(Mout, ncol=4, dimnames=list(regnames, cnames))

    xtable(x=Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "adfstat",
  function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tadf.tex"
    }

    regnames <- c(dimnames(x@regvarcoefs)[[1]], "adf.stat", dimnames(x@lagcoefs)[[1]])
    Tout <- urt.xtable(x, caption=caption, label=label, align=align,
                       vsep=vsep, digits=digits, display=display)

    hline.after <- hlref0 <- which(regnames=="adf.stat")
    if(hlref0 > 1)
      hline.after <- c(hline.after-1, hline.after)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

## HEGY

setMethod("urt.xtable", "hegystat",
  function(x, caption="HEGY test", label="Thegy", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    s <- frequency(x@wts); refrv <- length(x@regvarcoefs[,1])
    hegyout <- matrix(nrow=(s+s/2+1), ncol=4)
    hegyout[1:s,] <- x@hegycoefs
    hegyout[(s+1):(s+s/2+1),3:4] <- x@stats[3:(s/2+3),]

    etfsnames <- c(paste("tpi_", 1:s, sep=""),
                   paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                   paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "NULL")
      Mout <- hegyout
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(hegyout))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(hegyout), t(x@lagcoefs))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(hegyout), t(x@lagcoefs))), ncol=4, byrow=TRUE)

    regnames <- c(dimnames(x@regvarcoefs)[[1]], etfsnames, dimnames(x@lagcoefs)[[1]])
    cnames <- c("Estimate" , "Std.Error", "Statistics", "p-value")
    Mout <- matrix(Mout, ncol=4, dimnames=list(regnames, cnames))

    #xtable(x=Mout[1:(s+refrv),],
    #  caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
    xtable(x=Mout[,3:4], caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "hegystat",
  function(x, caption="HEGY test", label="Thegy", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Thegy.tex"
    }

    etfsnames <- c(paste("tpi_", 1:s, sep=""),
                   paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                   paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))
    regnames <- c(dimnames(x@regvarcoefs)[[1]], etfsnames, dimnames(x@lagcoefs)[[1]])

    Tout <- urt.xtable(x, caption=caption, label=label, align=align, vsep=vsep,
                       digits=digits, display=display)

    hline.after <- hlref0 <- which(regnames=="tpi_1")
    if(hlref0 > 1)
      hline.after <- c(hline.after-1, hline.after)
    hline.after <- c(hline.after, frequency(x@wts)+s+s/2+1)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

# KPSS

setMethod("urt.xtable", "kpssstat",
  function(x, caption="KPSS test", label="Tkpss", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    #Mout <- matrix(NA, nrow=5, ncol=3)
    #Mout[,1] <- c("0","1","2","3","4")

    ifelse(x@ltrunc-3 < 0, lt0<-0, lt0<-x@ltrunc-3)
    aux <- length(lt0:(x@ltrunc+3))
    Mout <- matrix(NA, nrow=aux, ncol=3)
    j <- 1
    for(i in lt0:(x@ltrunc+3)){
      out <- KPSS.test(wts=x@wts, ltrunc=i)
      Mout[j,1] <- i
      Mout[j,2] <- round(out@levelst, 2)
      Mout[j,3] <- round(out@trendst, 2)
      j <- j+1
    }
    Mout <- matrix(Mout, ncol=3, dimnames=list(rep("",aux), c("Trunc", "Level.st", "Trend.st")))
    xtable(Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "kpssstat",
  function(x, caption="KPSS test", label="Tkpss", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tkpss.tex"
    }

    Tout <- urt.xtable(x)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

# CH

setMethod("urt.xtable", "chstat",
  function(x, caption="CH test", label="Tch", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    s <- frequency(x@wts)
    if(s==4)
      fnames <- c("$L_{\\pi/2}$", "$L_{\\pi$}", "$L_f$")
    if(s==12)
      fnames <- c("$L_{\\pi/6}$", "$L_{\\pi/3}$", "$L_{\\pi/2}$",
                  "$L_{2\\pi/3}$", "$L_{5\\pi/6}$", "$L_{\\pi}$", "$L_f$")

    Mout <- matrix(nrow=(s/2+1), ncol=2)
    for(i in 1:(s/2)){
      frec <- rep(0, s/2); frec[i] <- 1
      Mout[i,1] <- CH.test(wts=x@wts, frec=frec, f0=0, DetTr=x@DetTr, ltrunc=NULL)@stat
      Mout[i,2] <- CH.test(wts=x@wts, frec=frec, f0=1, DetTr=x@DetTr, ltrunc=NULL)@stat
    }
    Mout[(s/2+1),1] <- CH.test(wts=x@wts, frec=rep(1,s/2), f0=0, DetTr=x@DetTr, ltrunc=NULL)@stat
    Mout[(s/2+1),2] <- CH.test(wts=x@wts, frec=rep(1,s/2), f0=1, DetTr=x@DetTr, ltrunc=NULL)@stat

    Mout <- matrix(Mout, ncol=2, dimnames=list(fnames, c("$y_t$", "$\\Delta\\,y_t$")))

    xtable(Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "chstat",
  function(x, caption="CH test", label="Tch", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tch.tex"
    }

    Tout <- urt.xtable(x)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

## Output in recursive testing.


recadf.save.xtable <- function(wts, nsub=72, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1),
  caption="ADF recursive test", label="Tadfrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    rec.bw <- ADF.rectest(wts=wts, type="backw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)
    rec.fw <- ADF.rectest(wts=wts, type="forw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,1])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,2], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,1])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,2], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

rechegy.save.xtable <- function(wts, nsub=72, itsd, refstat, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1),
  caption="HEGY recursive test", label="Thegyrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    refstat2 <- which(c("tpi1","","tpi2","","Fpi3:4","","Fpi5:6","",
                        "Fpi7:8","","Fpi9:10","","Fpi11:12") == refstat)
    if(length(refstat2)==0)
      stop(call="Select a correct statistic's name.")
    rec.bw <- HEGY.rectest(wts=wts, type="backw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)
    rec.fw <- HEGY.rectest(wts=wts, type="forw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,refstat2])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,refstat2+1], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,refstat2])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,refstat2+1], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat(paste("Statistic ", refstat, "\n"))
    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

reckpss.save.xtable <- function(wts, nsub=48, testtype="level", ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1),
  caption="KPSS recursive test", label="Tkpssrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    if(testtype=="level")
      ref <- 1
    if(testtype=="trend")
      ref <- 3
    rec.bw <- KPSS.rectest(wts=wts, type="backw", nsub=nsub,ltrunc=ltrunc, trace=trace)
    rec.fw <- KPSS.rectest(wts=wts, type="forw", nsub=nsub, ltrunc=ltrunc, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,ref])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,ref+1], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,ref])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,ref+1], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

recch.save.xtable <- function(wts, nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1),
  caption="CH recursive test", label="Tchrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }
    chrec.bw <- CH.rectest(wts=wts, type="backw", nsub=nsub, frec=frec, f0=f0, DetTr=DetTr,
                           ltrunc=ltrunc, trace=trace)
    chrec.fw <- CH.rectest(wts=wts, type="forw", nsub=nsub, frec=frec, f0=f0, DetTr=DetTr,
                           ltrunc=ltrunc, trace=trace)

    fwsmpls <- dimnames(chrec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(chrec.fw@recstats[,1])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(chrec.fw@recstats[,2], justify="left")

    bwsmpls <- dimnames(chrec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(chrec.bw@recstats[,1])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(chrec.bw@recstats[,2], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}
