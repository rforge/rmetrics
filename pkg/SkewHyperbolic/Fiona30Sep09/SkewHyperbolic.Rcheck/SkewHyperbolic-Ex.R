### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx",
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") -
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## at least one package changes these via ps.options(), so do this
## before loading the package.
## Use postscript as incomplete files may be viewable, unlike PDF.
## Choose a size that is close to on-screen devices, fix paper
grDevices::ps.options(width = 7, height = 7, paper = "a4", reset = TRUE)
grDevices::postscript("SkewHyperbolic-Ex.ps")

assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)
options(pager = "console")
library('SkewHyperbolic')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("dskewhyp")
### * dskewhyp

flush(stderr()); flush(stdout())

### Name: SkewHyperbolicDistribution
### Title: Skewed Hyperbolic Student's t-Distribution
### Aliases: dskewhyp pskewhyp qskewhyp rskewhyp
### Keywords: distribution

### ** Examples

param <- c(0,1,40,10)
par(mfrow=c(1,2))
range <- skewhypCalcRange(param=param, tol=10^(-2))

#curves of density and distribution
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the \n Skew Hyperbolic Distribution")
curve(pskewhyp, range[1], range[2], n=500, param=param)
title("Distribution Function of the \n Skew Hyperbolic Distribution")

#curves of density and log density
par(mfrow=c(1,2))
data <- rskewhyp(1000, param=param)
curve(dskewhyp(x, param=param), range(data)[1], range(data)[2],
      n=1000, col=2)
hist(data, freq=FALSE, add=TRUE)
title("Density and Histogram of the\n Skew Hyperbolic Distribution")
logHist(data, main="Log-Density and Log-Histogram of\n the Skew
      Hyperbolic Distribution")
curve(dskewhyp(x, param=param, log=TRUE), range(data)[1], range(data)[2],
      n=500, add=TRUE, col=2)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("lrdji")
### * lrdji

flush(stderr()); flush(stdout())

### Name: lrdji
### Title: Dow Jones Log Return Data
### Aliases: lrdji
### Keywords: datasets

### ** Examples

data(lrdji)
##fit a skew hyperbolic students t-distribution to the data
fit<-skewhypFit(lrdji, plot=TRUE, print=TRUE)



cleanEx(); nameEx("lrnokeur")
### * lrnokeur

flush(stderr()); flush(stdout())

### Name: lrnokeur
### Title: Log Returns of the NOK/EUR Exchange Rate
### Aliases: lrnokeur
### Keywords: datasets

### ** Examples

##Fit the skew hyperbolic students-t distribution to the data
data(lrnokeur)
fit <- skewhypFit(lrnokeur, method="nlm", plot=TRUE, print=TRUE)



cleanEx(); nameEx("qqskewhyp")
### * qqskewhyp

flush(stderr()); flush(stdout())

### Name: qqskewhyp
### Title: Skew Hyperbolic Student's t-Distribution Quantile-Quantile and
###   Percent-Percent Plots
### Aliases: qqskewhyp ppskewhyp
### Keywords: distribution hplot

### ** Examples

par(mfrow = c(1,2))
param <- c(0,1,20,10)
y <- rskewhyp(500, param=param)
qqskewhyp(y, param=param, main="Skew Hyperbolic\n Q-Q Plot")
ppskewhyp(y, param=param, main="Skew Hyperbolic\n P-P Plot")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("skewhypBreaks")
### * skewhypBreaks

flush(stderr()); flush(stdout())

### Name: skewhypBreaks
### Title: Break points for the Skew Hyperbolic Student's t-Distribuiton
### Aliases: skewhypBreaks skewhypCalcRange ddskewhyp
### Keywords: distribution

### ** Examples

param <- c(0,1,10,10)
range <- skewhypCalcRange(param=param, tol=10^(-3))

#plots of density and derivative
par(mfrow=c(2,1))
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the Skew\n Hyperbolic Distribution")
curve(ddskewhyp(x, param=param), range[1], range[2], n=1000)
title("Derivative of the Density\n of the Skew Hyperbolic Distribution")

#plot of the density marking the break points
par(mfrow=c(1,1))
range <- skewhypCalcRange(param=param, tol=10^(-6))
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the Skew Hyperbolic Distribution\n with Breakpoints")
breaks <- skewhypBreaks(param=param)
abline(v=breaks)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("skewhypFit")
### * skewhypFit

flush(stderr()); flush(stdout())

### Name: skewhypFit
### Title: Fit the Skew Hyperbolic Student's t-Distribution to Data
### Aliases: skewhypFit plot.skewhypFit print.skewhypFit summary.skewhypFit
### Keywords: distribution

### ** Examples

## See how well skewhypFit works
param <- c(0,1,4,10)
data <- rskewhyp(500, param=param)
fit <- skewhypFit(data)
## Use data set NOK/EUR as per Aas&Haff
data(lrnokeur)
nkfit <- skewhypFit(lrnokeur, method = "nlm")
## Use data set DJI
data(lrdji)
djfit <- skewhypFit(lrdji)



cleanEx(); nameEx("skewhypFitStart")
### * skewhypFitStart

flush(stderr()); flush(stdout())

### Name: skewhypFitStart
### Title: Find Starting Values for Fittting a Skew Hyperbolic Student's
###   t-Distribution
### Aliases: skewhypFitStart skewhypFitStartLA
### Keywords: distribution

### ** Examples

#find starting values to feed to skewhypFit
data(lrnokeur)
skewhypFitStart(lrnokeur, startValues="LA")$paramStart
#user supplied values
skewhypFitStart(lrnokeur, startValues="US",
paramStart=c(0,0.01,0,5))$paramStart



cleanEx(); nameEx("skewhypMeanVarMode")
### * skewhypMeanVarMode

flush(stderr()); flush(stdout())

### Name: skewhypMeanVarMode
### Title: Moments and Mode of the Skew Hyperbolic Student's
###   t-Distribution.
### Aliases: skewhypMeanVarMode skewhypMean skewhypVar skewhypSkew
###   skewhypKurt skewhypMode
### Keywords: distribution

### ** Examples

param <- c(10,1,5,9)
skewhypMean(param=param)
skewhypVar(param=param)
skewhypSkew(param=param)
skewhypKurt(param=param)
skewhypMode(param=param)
range <- skewhypCalcRange(param=param)
curve(dskewhyp(x, param=param), range[1], range[2])
abline(v=skewhypMode(param=param), col="red")
abline(v=skewhypMean(param=param), col="blue")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
