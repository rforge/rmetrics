
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


# fEcofin::0-AEcofinUtilities.R
################################################################################
# FUNCTION:                 INTERNAL USED PLOT FUNCTIONS:
#  .residualsPlot            Returns a residual series plot
#  .acfPlot                  Returns a autocorrelation function plot
#  .pacfPlot                 Returns a partial ACF plot
#  .mrlPlot                  Returns a mean residual life plot
# FUNCTION:                 INTERNAL USED BIVARIATE PLOT FUNCTIONS:
#  .responsesPlot            Returns a response series plot
#  .firePlot                 Returns a fitted values vs.residuals plot
# FUNCTION:                 INTERNAL THREE-DIMENSIONAL PLOT UTILITIES:
#  .circlesPlot              Returns a circles plot indexing 3rd variable
#  .perspPlot                Returns a perspective plot in 2 dimensions
#  .contourPlot              Returns a contour plot in 2 dimensions
#  .histStack                Returns a stacked histogram plot
# FUNCTION:                 SLIDER MENU:
#  .sliderMenu               Opens a teching demo slider menu
#  .tdSliderMenu             Opens a teching demo slider and button menu
# FUNCTION:                 SOME UTILITIES:
#  .description              Sets default description string
#  .unirootNA                Computes zero without error exit
#  .datax                    Loads timeSeries objects from demo files
# FUNCTION:                 DATE FUNCTIONS:
#  .fjulian                  Transform formatted dates to julian day numbers
#  .julian                   Implements SPlus like 'julian'
#  .isISO8601                Checks if the date/time is ISO8601 formatted
#  .isPOSIX                  Checks for an object of class POSIX
#  .by2seconds               Converts 'by' string into numeric value of seconds
# FUNCTION:                 SOME METHODS:
#  .print                    Internal print method
#  .plot                     Internal plot method
#  .summary                  Internal summary method
#  .predict                  Internal predict method
# FUNCTION:                 DESCRIPTION:
#  .distCheck                Checks consistency of distributions
################################################################################


################################################################################
#  .residualsPlot            Returns a residual series plot
#  .acfPlot                  Returns a autocorrelation function plot
#  .pacfPlot                 Returns a partial ACF plot
#  .mrlPlot                  Returns a mean residual life plot


.residualsPlot =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns time series graph of residuals

    # Arguments:
    #   x - an univariate time series of residuals

    # FUNCTION:

    # Get Data:
    x = as.vector(x)

    # Plot:
    plot(x, type = "l", ylab = "Residuals",
        main = "Residual Series", col = "steelblue", ...)
    rug(x, ticksize = 0.01, side = 4)
    grid()
    abline(h = 0, col = "grey")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.acfPlot =
function(x, ...)
{
    # FUNCTION:

    # Convert Type:
    x = as.vector(x)

    # ACF:
    acf(x, ...)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.pacfPlot =
function(x, ...)
{
    # FUNCTION:

    # Convert Type:
    x = as.vector(x)

    # ACF:
    pacf(x, ...)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.mrlPlot =
function(x, ci = 0.95, umin = mean(x), umax = max(x), nint = 100,
doplot = TRUE, plottype = c("autoscale", ""), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Create a mean residual life plot with
    #   confidence intervals.

    # References:
    #   A function originally written by S. Coles

    # FUNCTION:

    # Convert Type:
    x = as.vector(x)

    # Settings:
    plottype = plottype[1]

    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[,1]
    sx = xu = xl = rep(NA, nint)
    u = seq(umin, umax, length = nint)
    for (i in 1:nint) {
        x = x[x >= u[i]]
        sx[i] = mean(x - u[i])
        sdev = sqrt(var(x))
        n = length(x)
        xu[i] = sx[i] + (qnorm((1 + ci)/2) * sdev) / sqrt(n)
        xl[i] = sx[i] - (qnorm((1 + ci)/2) * sdev) / sqrt(n)
    }

    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Threshold: u"
            ylab = "Mean Excess: e"
            main = "Mean Residual Live Plot"
        } else {
            main = xlab = ylab = ""
        }
        if (plottype == "autoscale") {
            ylim = c(min(xl[!is.na(xl)]), max(xu[!is.na(xu)]))
            plot(u, sx, type = "o", pch = 19, col = "steelblue",
                xlab = xlab, ylab = ylab, ylim = ylim, main = main, ...)
        } else {
            plot(u[!is.na(xl)], sx[!is.na(xl)], type = "o",
                pch = 19, col = "steelblue",
                xlab = xlab, ylab = ylab, main = main, ...)
        }
        lines(u[!is.na(xl)], xl[!is.na(xl)], col = "brown")
        lines(u[!is.na(xu)], xu[!is.na(xu)], col = "brown")
        if (labels) {
            grid()
            text = paste("ci =", as.character(round(ci, 3)))
            mtext(text, side = 4, adj = 0, cex = 0.7)
        }
    }

    # Result
    result = data.frame(threshold = u, mrl = sx)

    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


################################################################################
#  .responsesPlot            Returns a response series plot
#  .firePlot                 Returns a fitted values vs.residuals plot


.responsesPlot =
function(x, y = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns time series graph of responses and fitted values

    # Arguments:
    #   x - an univariate time series of responses
    #   y - an univariate time series of fitted values

    # FUNCTION:

    # Get Data:
    x = as.vector(x)
    y = as.vector(y)

    # Responses Plot:
    plot(x, type = "l", ylab = "Responses",
        main = "Responses & Fitted Values", col = "steelblue", ...)
    rug(x, ticksize = 0.01, side = 4)
    grid()
    abline(h = 0, col = "grey")

    # Add fitted values:
    if (!is.null(y)) points(y, pch = 19, col = "red")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.firePlot =
function(x, y, method = c("scatter", "hist"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns fitted values vs. residuals plots

    # Arguments:
    #   x - univariate time series (residuals)
    #   y - univariate time series (fitted)

    # FUNCTION:

    # Check Arguments:
    method = match.arg(method)

    # Get Data:
    x = as.vector(x)
    y = as.vector(y)


    if (method == "scatter") {

        # Scatter Plot:
        plot(x, y,
            xlab = "Fitted Values", ylab = "Residuals",
            main = "Residuals vs Fitted",
            pch = 19, col = "steelblue")
        panel.smooth(x, y)
        abline(h = 0, lty = 3, col = "grey")
        rug(x, ticksize = 0.01, side = 3)
        rug(y, ticksize = 0.01, side = 4)

    } else if (method == "hist") {

        # Histogram Plot:

        # Save default, for resetting ...
        def.par = par(no.readonly = TRUE)

        # Layout:
        nf = layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1),
            c(1, 3), TRUE)

        # Scatterplot:
        par(mar = c(3 ,3, 1, 1))
        plot(x, y, xlim = range(x), ylim = range(y),
            xlab = "", ylab = "", pch = 19, col = "steelblue")
        panel.smooth(x, y)
        abline(h = 0, lty = 3, col = "grey")
        rug(x, side = 3)
        rug(y, side = 4)

        # Histogram:
        xhist = hist(x, nclass = 15, plot = FALSE)
        yhist = hist(y, nclass = 15, plot = FALSE)
        top = max(c(xhist$counts, yhist$counts))

        # X-Side:
        par(mar = c(0, 3, 1, 1))
        Main = "\n                            Fitted"
        barplot(xhist$counts, axes = FALSE, ylim = c(0, top),
            space = 0, col = "steelblue", border = "white",
            main = Main)
        abline(h = 0, lwd = 2, col = "grey")

        # Y-Side:
        par(mar = c(3, 0, 1, 1))
        barplot(yhist$counts, axes = FALSE, xlim = c(0, top),
            space = 0, col = "steelblue", , border = "white",
            horiz = TRUE, main = "Residuals")
        abline(v = 0, lwd = 2, col = "grey")

        # Reset:
        par(def.par)

    }

    # Return Value:
    invisible()
}


################################################################################
#  .circlesPlot           Returns a scatterplot of circles indexing 3rd variable
#  .perspPlot             Returns a perspective plot in 2 dimensions
#  .contourPlot           Returns a contour plot in 2 dimensions
#  .histStack             Returns a stacked histogram plot


.circlesPlot =
function(x, y = NULL, z = NULL, scale = 1, points = TRUE,
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a scatterplot with circle size as third variable

    # Example:
    #   circlesPlot(x=rnorm(50), y=rnorm(50), z=rnorm(50))
    #   circlesPlot(x=rnorm(50), y=rnorm(50), z=rnorm(50), labels= FALSE)

    # FUNCTION:

    # Transfor Input:
    if (is.list(x)) x = matrix(unlist(x), ncol = 3)
    if (is.data.frame(x)) x = as.matrix.data.frame(x)
    if (is.matrix(x)) {
        z = x[, 3]
        y = x[, 2]
        x = x[, 1]
    }
    nX = length(x)
    nY = length(y)
    # nZ = length(z)
    stopifnot(nX == nY)
    # stopifnot(nX == nZ || nX*nY == nZ)

    # Create Circle Plot:
    if (labels) {
        plot(x, y, type = "n")
    } else {
        plot(x, y, xlab = "", ylab = "", type = "n")
    }
    symbols(x, y, add = TRUE, circles = abs(z)^scale, inches = 0.25,
        fg = "black", bg = "steelblue", ...)
    X = x[z < 0]
    Y = y[z < 0]
    Z = z[z < 0]
    symbols(X, Y, add = TRUE, circles = abs(Z)^scale, inches = 0.25,
        fg = "black", bg = "orange", ...)
    if (points) points(x, y, pch = 19)
    grid()

    # Return Value:
    invisible(NULL)
}


# ------------------------------------------------------------------------------


.perspPlot =
function(x, y, z, theta = -40, phi = 30, col = "steelblue", ps = 9, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a perspecvtive plot

    # Notes:
    #   A synonyme call for function 'persp'

    # FUNCTION:

    # Perspective Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = persp(x = x, y = y, z = z, ...)
    } else {
        # R:
        par(ps = ps)
        if (!exists("ticktype")) ticktype = "detailed"
        if (!exists("expand")) expand = 0.6
        if (!exists("r")) r = 500
        ans = persp(x = x, y = y, z = z, theta = theta, phi = phi,
            col = col, ticktype = ticktype, expand = expand, ...)
    }

    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


.contourPlot =
function(x, y, z, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a contour plot

    # Notes:
    #   A synonyme call for function 'contour'

    # FUNCTION:

    # Contour Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = contour(x = x, y = y, z = z, ...)
    } else {
        # R:
        ans = contour(x = x, y = y, z = z, ...)
    }

    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


.histStack =
function(x, y = NULL, space = 0, ylab = "frequency", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a stacked histogram Plot

    # Example:
    #   .histStack(rnorm(1000, -1), rnorm(1000, 1))

    # FUNCTION:

    # Compute Histograms:
    breaks = hist(c(x, y))$breaks
    bars = rbind(
        hist(x, breaks = breaks, plot = FALSE)$counts,
        hist(y, breaks = breaks, plot = FALSE)$counts)

    # Plot:
    barplot(bars, space = space, ylab = ylab, ...)
    at = seq(along = breaks) - 1
    modulo = ceiling(length(at)/10)
    sel = (at%%modulo == 0)
    axis(side = 1, at = at[sel], labels = paste(breaks)[sel])

    # Return Value:
    invisible()
}


################################################################################
#  .sliderMenu           Starts a slider menu
#  .tdSlider             Opens a teching demo slider menu


.sliderMenu =
function(refresh.code, names, minima, maxima, resolutions, starts,
title = "Slider", no = 0, set.no.value = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Starts a slider menu

    # Source:
    #   Built on code written by Peter Wolf

    # FUNCTION:

    # Requirement:
    require(tcltk)

    # Environment:
    if (!exists("slider.env")) {
        slider.env <<- new.env()
    }
    if (no != 0) {
        options(show.error.messages = FALSE)
        ans = as.numeric(tclvalue(get(paste("slider", no, sep = ""),
            env = slider.env)))
        options(show.error.messages = TRUE)
        return(ans)
    }
    if (set.no.value[1] != 0) {
        try(eval(parse(text = paste("tclvalue(slider", set.no.value[1],
            ")<-", set.no.value[2], sep = "")), env = slider.env),
            silent = TRUE)
        return(set.no.value[2])
    }

    # Toplevel:
    nt = tktoplevel()
    tkwm.title(nt, title)


    # Slider:
    for (i in seq(names)) {
        eval(parse(text = paste("assign(\"slider", i, "\",
            tclVar(starts[i]), env = slider.env)", sep = "")))
        tkpack(fr<-tkframe(nt), anchor = "sw")
        lab = tklabel(fr, text = names[i], anchor = "sw")
        sc = tkscale(fr, command = refresh.code, from = minima[i],
            to = maxima[i], showvalue = TRUE, resolution =
            resolutions[i], orient = "horiz")
        assign("sc", sc, env = slider.env)
        tkgrid(sc, lab)
        eval(parse(text = paste("tkconfigure(sc, variable = slider", i, ")",
            sep = "")), env = slider.env)
    }
    tkpack(fr<-tkframe(nt), anchor = "sw")

    # Quit:
    quitButton = tkbutton(fr, text = "   Quit   ",
        command = function() {
            tkdestroy(nt)
        } )

    # Reset:
    resetButton = tkbutton(fr, text = "   Start | Reset   ",
        command = function() {
            for (i in seq(starts)) eval(parse(text =
                paste("tclvalue(slider", i, ")<-", starts[i], sep = "")),
                env = slider.env)
            refresh.code()
        }  )

    # Compose:
    tkgrid(resetButton, quitButton, sticky = "sew")
}


# ------------------------------------------------------------------------------


.tdSliderMenu = 
function(sl.functions, names, minima, maxima, resolutions, starts,
but.functions, but.names, no, set.no.value, obj.name, obj.value, 
reset.function, title)
{   # A function implemented by Diethelm Wuertz

    # Description 
    #   Opens a teching demo slider menu
    
    # Notes:
    #   Build on ideas and code from:
    #   R Package: TeachingDemos
    #   Title: Demonstrations for teaching and learning
    #   Version: 1.5
    #   Author: Greg Snow
    #   Description: This package is a set of demonstration functions 
    #       that can be used in a classroom to demonstrate statistical 
    #       concepts, or on your own to better understand the concepts 
    #       or the programming.
    #   Maintainer: Greg Snow <greg.snow@intermountainmail.org>
    #   License: Artistic
    
    # FUNCTION:
    
    # Setup:
    if(!missing(no)) {
        return(as.numeric(tclvalue(get(paste(".tdSlider", no, sep=""),
            env = slider.env))))
    }
    if(!missing(set.no.value)){ 
        try(eval(parse(text=paste("tclvalue(.tdSlider", set.no.value[1],")<-",
            set.no.value[2], sep = "")), env = slider.env))
        return(set.no.value[2]) 
    }
    if(!exists("slider.env")) {
        slider.env <<- new.env()
    }
    if(!missing(obj.name)){
        if(!missing(obj.value)) {
            assign(obj.name, obj.value, env = slider.env) 
        } else {
            obj.value <- get(obj.name, env = slider.env)
        }
        return(obj.value)
    }
    if(missing(title)) {
        title = "Control Widget"
    }
    
    # GUI Settings:
    require(tcltk)
    nt <- tktoplevel() 
    tkwm.title(nt, title)
    tkwm.geometry(nt, "+0+0")

    # Buttons:
    tkpack(
        f.but <- tkframe(nt), fill = "x")
        
    # Quit Button:
    quitCMD = function() {
        tkdestroy(nt)
    } 
    tkpack(
        tkbutton(f.but, text = "Quit", command = quitCMD, anchor = "sw"), 
        side = "right",
        fill = "y")    
        
    # Reset Button:    
    if(missing(reset.function)) {
        reset.function <- function(...) print("relax")
    }
    if(!is.function(reset.function)) {
        reset.function<-eval(parse(text = 
            paste("function(...){",reset.function,"}")))
    }
    resetCMD = function() 
    {
        for(i in seq(names))
            eval(parse(text = paste("tclvalue(.tdSlider",i,")<-",
                starts[i], sep = "")),
            env = slider.env)
        reset.function() 
    }
    tkpack(
        tkbutton(f.but, text = "Reset", command = resetCMD, anchor = "sw"),
        side = "right",
        fill = "y")   
    if (missing(but.names)) {
        but.names <- NULL
    }    
    for (i in seq(but.names)) {
        but.fun <- 
            if (length(but.functions) > 1) 
                but.functions[[i]]
            else 
                but.functions  
        if (!is.function(but.fun)) {
            but.fun <- 
                eval(parse(text = paste("function(...){", but.fun, "}")))
        }    
        tkpack(
            tkbutton(f.but, text = but.names[i], command = but.fun, 
                anchor = "nw"), 
            # side = "right",
            fill = "x"
            )
    }
    
    # Sliders: 
    if(missing(names)) {
        names <- NULL
    }        
    if(missing(sl.functions)) {
        sl.functions <- function(...){}
    }
    for(i in seq(names)){
        eval(parse(text = paste("assign('.tdSlider",i,"', 
            tclVar(starts[i]), env = slider.env)", sep = "")))
        tkpack(fr <- tkframe(nt)) 
        lab <- tklabel(fr, 
            text = names[i], 
            anchor = "sw",
            width = "35")
        sc <- tkscale(fr, 
            from = minima[i], 
            to = maxima[i], 
            showvalue = TRUE,
            resolution = resolutions[i], 
            orient = "horiz")
        tkpack(lab, 
            sc, 
            anchor = "sw",
            side = "right"); 
        assign("sc", sc, env = slider.env)
        
        eval(parse(text=paste("tkconfigure(sc,variable=.tdSlider",i,")", 
            sep="")), env = slider.env)
        sl.fun <- 
            if(length(sl.functions)>1) 
                sl.functions[[i]] 
            else 
                sl.functions  
        if(!is.function(sl.fun)) 
            sl.fun<-eval(parse(text=paste("function(...){", sl.fun,"}")))
            
        tkconfigure(sc, command = sl.fun)
    }
    assign("slider.values.old", starts, env = slider.env)

    # Return Value:
    invisible(nt)
}


################################################################################


.description =
function()
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Sets default description string:

    # FUNCTION:

    # Description String:
    ans = paste(as.character(date()), "by user:", Sys.getenv("USERNAME"))

    # Return Value:
    ans
}


################################################################################


.unirootNA =
function(f, interval, lower = min(interval), upper = max(interval),
tol = .Machine$double.eps^0.25, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Searches the interval from lower to upper for a
    #   root (i.e., zero) of the function f with respect
    #   to its first argument.

    # Arguments:
    #   see 'uniroot'

    # Value:
    #   Returns the x value of f where the root is located. If
    #   now root exists NA will be returned. In the last case
    #   the function doesn't terminate with an error like in the
    #   case of the standard function uniroot.

    # Details:
    #   R:
    #   uniroot(f, interval, lower = min(interval), upper = max(interval),
    #       tol = .Machine$double.eps^0.25,
    #       maxiter = 1000, ...)
    #   uniroot(f, interval, lower = min(interval), upper = max(interval),
    #       tol = .Machine$double.eps^.25,
    #       keep.xy = F, f.lower = NA,  f.upper = NA, ...)

    # Example:
    #   .unirootNA(sin, c(1, 2)); .unirootNA(sin, c(-1, 1))

    # FUNCTION:

    # There is no Root:
    if (is.null(args(f))) {
        if (f(lower) * f(upper) >=0) return(NA)
    } else {
        if (f(lower, ...) * f(upper, ...) >= 0) return(NA)
    }

    # There is a Root:
    ans = uniroot(f = f, interval = interval, lower = lower,
        upper = upper, tol = tol, ...)

    # Return Value:
    ans$root
}


################################################################################
#  .datax                    Loads timeSeries objects from demo files


.datax =
function(..., list = character(0), package = NULL, lib.loc = NULL,
verbose = getOption("verbose"), envir = .GlobalEnv)
{   # An extended copy of the var() function from R's base package

    # FUNCTION:

    # data:
    fileExt = function(x) sub(".*\\.", "", x)
    names = c(as.character(substitute(list(...))[-1]), list)
    ## Find the directories of the given packages and maybe the working
    ## directory.
    if (!is.null(package)) {
        if (!is.character(package))
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) warning(
            "datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) warning(
            "datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] = "datasets"
    }
    paths = .find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc))
        paths = c(.path.package(package, TRUE),
            if(is.null(package)) getwd(), paths)
    paths = unique(paths[file.exists(paths)])
    ## Find the directories with a 'data' subdirectory.
    paths = paths[tools::file_test("-d", file.path(paths, "data"))]
    dataExts = tools:::.make_file_exts("data")
    if(length(names) == 0) {
        ## List all possible data sets.
        ## Build the data db.
        db = matrix(character(0), nr = 0, nc = 4)
        for(path in paths) {
            entries = NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(tools::file_test("-f", file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds'
            if(tools::file_test("-f", INDEX <-
                file.path(path, "Meta", "data.rds"))) {
                entries = .readRDS(INDEX)
            } else {
                ## No index: should only be true for ./data >= 2.0.0
                dataDir = file.path(path, "data")
                entries = tools::list_files_with_type(dataDir, "data")
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries = cbind(entries, "")
                }
            }
            if(NROW(entries) > 0) {
                if(is.matrix(entries) && ncol(entries) == 2)
                    db = rbind(db, cbind(packageName, dirname(path), entries))
                else
                    warning(gettextf("data index for package '%s' is invalid and will be ignored", packageName), domain=NA, call.=FALSE)
            }
        }
        colnames(db) = c("Package", "LibPath", "Item", "Title")

        footer = if(missing(package))
            paste("Use ", sQuote(paste("data(package =",
                ".packages(all.available = TRUE))")), "\n",
                "to list the data sets in all *available* packages.", sep = "")
            else
                NULL
        y = list(title = "Data sets", header = NULL, results = db,
            footer = footer)
        class(y) = "packageIQR"
        return(y)
    }
    paths = file.path(paths, "data")
    for(name in names) {
        found = FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if (tools::file_test("-f", file.path(p, "Rdata.rds"))) {
                rds = .readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found = TRUE
                    if(verbose)
                        cat("name=", name, ":\t found in Rdata.rdb\n")
                    thispkg = sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg = sub("_.*$", "", thispkg) # versioned installs.
                    thispkg = paste("package:", thispkg, sep="")
                    objs = rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = envir,
                             filter = function(x) x %in% objs)
                    break
                }
            }
            ## check for zipped data dir
            if(tools::file_test("-f", file.path(p, "Rdata.zip"))) {
                if(tools::file_test("-f",fp = file.path(p, "filelist")))
                    files = file.path(p, scan(fp, what="", quiet = TRUE))
                else {
                    warning(gettextf("file 'filelist' is missing for directory '%s'", p), domain = NA)
                    next
                }
            } else {
                files = list.files(p, full = TRUE)
            }
            files = files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1) {
                ## more than one candidate
                o = match(fileExt(files), dataExts, nomatch = 100)
                paths0 = dirname(files)
                paths0 = factor(paths0, levels=paths0)
                files = files[order(paths0, o)]
            }
            if (length(files) > 0) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if(verbose)
                        cat("name=", name, ":\t file= ...",
                            .Platform$file.sep, basename(file), "::\t", sep = "")
                    ext = fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if(basename(file) != paste(name, ".", ext, sep = ""))
                        found = FALSE
                    else {
                        found = TRUE
                        zfile = zip.file.extract(file, "Rdata.zip")
                        if(zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                            R = , r =sys.source(zfile, chdir = TRUE,
                            envir = envir), RData = , rdata = , rda =
                            load(zfile, envir = envir), TXT = , txt = ,
                            tab = assign(name,
                            read.table(zfile, header = TRUE), envir = envir),
                            CSV = , csv = assign(name,
                            read.table(zfile, header = TRUE, sep = ";"),
                            envir = envir),
                            found = FALSE)
                    }
                    if (found) break # from files
                }
                if (verbose) cat(if(!found) "*NOT* ", "found\n")
            }
            if (found) break # from paths
        }
        if(!found)
            warning(gettextf("data set '%s' not found", name), domain = NA)
    }

    # REQUIRES FCALENDAR !! -> move it to fCalendar
    # DW added:
    for (name in names) {
        # If the data set can be transformed in a timeSeriesobject
        # then do it ...
        z = eval(parse(text = paste("x = ", name)))
        # print(c(class = class(z)))
        if (class(z) == "data.frame") {

            rtry <- try(as(eval(parse(text = paste("x = ", name))),
                           "timeSeries"), silent = TRUE)
            tS <- as.character(!is.character(rtry))
            # print(c(tS = as.logical(tS)))
            if (as.logical(tS)) {
                z = as(name, "timeSeries")
                eval(parse(text = paste(name, "<<- z")))
            }
        }
    }
    # DW

    invisible(names)
}


################################################################################
#  .fjulian            Transform formatted dates to julian day numbers
#  .julian             Implements SPlus like 'julian'


.fjulian =
function(fdates, origin = 19600101, order = 'mdy', cc = NULL, swap = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Transforms formatted dates (fdates) from several formats
    #   as 8/11/73 11Aug1973, ... into ISO-8601 Gregorian dates
    #   ... makes use of C-Program char_date.c implemented by
    #   Terry Therneau

    # Notes:
    #   cc - Century, becoming obsolete with the introduction of swap.

    # Example:
    #   require(date)
    #   fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    #   .fjulian(fdates)
    #   fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    #   .fjulian(fdates, order = 'dmy')

    # Note:
    #   Requires R-package "date"

    # FUNCTION:

    # Requires
    # require(date)

    # Formats:
    order.vec = switch(order,
        'ymd'= c(1,2,3),
        'ydm'= c(1,3,2),
        'mdy'= c(2,3,1),
        'myd'= c(2,1,3),
        'dym'= c(3,1,2),
        'dmy'= c(3,2,1),
        stop("Invalid value for 'order' option"),
        PACKAGE = "date")
    nn = length(fdates)
    temp = .C("char_date",
        as.integer(nn),
        as.integer(order.vec),
        as.character(fdates),
        month = integer(nn),
        day = integer(nn),
        year = integer(nn),
        PACKAGE = "date")
    month = temp[[4]]
    day = temp[[5]]
    year = temp[[6]]
    yy = year - 100 * floor (year/100)

    # Swap:
    cc = 19 + trunc(sign(swap-yy)+1)/2
    year = cc*100 + yy

    # Origin:
    cc0 = origin %/% 1000000
    yymmdd0 = origin - cc0*1000000
    yy0 = yymmdd0 %/% 10000
    mm0 = yymmdd0 %/% 100 - yy0*100
    dd0 = yymmdd0 - yy0*10000 - mm0*100

    # Result:
    ans = .julian(month, day, year, origin = c(mm0, dd0, cc0*100+yy0))

    # Return Value:
    ans
}


# ******************************************************************************


.julian =
function(m, d, y, origin = c(month = 1, day = 1, year = 1960))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   This function is a synonyme for Splus' "julian()" with the
    #   same list of arguments.

    # Note:
    #   SPlus like function.

    # FUNCTION:

    # Selection:
    .R = TRUE
    .S = FALSE

    # Implementation under R:
    if(.R) {
        only.origin = all(missing(m), missing(d), missing(y))
        if(only.origin) m = d = y = NULL    # return days since origin
        nms = names(d)
        max.len = max(length(m), length(d), length(y))
        # prepend new origin value and rep out to common max. length:
        m = c(origin[1], rep(m, length = max.len))
        d = c(origin[2], rep(d, length = max.len))
        y = c(origin[3], rep(y, length = max.len))
        # code from julian date in the S book (p.269)
        y = y + ifelse(m > 2, 0, -1)
        m = m + ifelse(m > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        out = (146097 * c) %/% 4 + (1461 * ya) %/% 4 +
            (153 * m + 2) %/% 5 + d + 1721119
        # now subtract the new origin from all dates
        if(!only.origin) {
            if(all(origin == 0)) out = out[-1] else out = out[-1] - out[1] }
        names(out) = nms
        result = out }

    # Synonyme for S:
    if(.S) {
        result = julian(m = m, d = d, y = y, origin. = origin)}

    # Return Value:
    result
}


################################################################################
# FUNCTION:              DESCRIPTION
#  .isISO8601             Checks if the date/time is ISO8601 formatted


.isISO8601 =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks if the date/time string is ISO8601 formatted

    # Example:
    #   .isISO8601(c("2007-01-01", "2007-12-31" ))
    #   .isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    #   .isISO8601("2007-Jan-01")[[1]]
    #   .isISO8601("2007-01-01 15:00:000")[[1]]

    # FUNCTION:

    # Check:
    stopifnot(class(x) == "character")

    # Test:
    options(warn = -1)
    format = .whichFormat(x)
    ans = FALSE
    if (format == "%Y-%m-%d %H:%M:%S") ans = TRUE
    if (format == "%Y-%m-%d") ans = TRUE
    if (format == "%Y%m%d%H%M%S") ans = TRUE
    if (format == "%Y%m%d") ans = TRUE
    attr(ans, "control")<- format

    # Return Value:
    ans
}


################################################################################
# FUNCTION ADDON:      DESCRIPTION:
#  .isPOSIX             Checks for an object of class POSIX


.isPOSIX =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks for an object of class POSIX

    # FUNCTION:

    # Check:
    ans = inherits(x, "POSIXt")

    # Return Value:
    ans
}


################################################################################
# GENERAL UTILITIES:          DESCRIPTION:
#  .by2seconds                 Convert 'by' string into numeric value of seconds
#  .print                      Internal print method
#  .plot                       Internal plot method
#  .summary                    Internal summary method


.by2seconds =
function(by = "1 h")
{   # A function written by Diethelm Wuertz

    # Description:
    #   Convert 'by' string into numeric value of seconds

    # FUNCTION:

    # Convert:
    by = strsplit(by, " ")[[1]]
    byTime = as.integer(by[1])
    byUnits = substr(by[2], 1, 1)
    timeUnits = c(1, 60, 3600)
    names(timeUnits) = c("s", "m", "h")
    bySeconds = byTime * timeUnits[byUnits]
    names(bySeconds) = "secs"

    # Return Value:
    bySeconds
}


# ------------------------------------------------------------------------------


.print =
function(x, ...)
{
    UseMethod(".print")
}


# ------------------------------------------------------------------------------


.plot =
function(x, ...)
{
    UseMethod(".plot")
}


# ------------------------------------------------------------------------------


.summary =
function(object, ...)
{
    UseMethod(".summary")
}


# ------------------------------------------------------------------------------


.predict =
function(object, ...)
{
    UseMethod(".predict")
}


################################################################################
# FUNCTION:               DESCRIPTION:
#  .distCheck              Checks consistency of distributions


.distCheck = 
function(fun = "norm", n = 1000, robust = TRUE, subdivisions = 100, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks consistency of distributions
    
    # Arguments:
    #   fun - name of distribution
    #   ... - distributional parameters
    
    # Examples:
    #   .distCheck("norm", mean = 1, sd = 1)
    #   .distCheck("t", df = 4)
    #   .distCheck("exp", rate = 2)
    #   .distCheck("weibull", shape = 1)

    # FUNCTION:
    
    # Distribution Functions:
    cat("\nDistribution Check for:", fun, "\n ")
    CALL = match.call()
    cat("Call: ")
    cat(paste(deparse(CALL), sep = "\n", collapse = "\n"), "\n", sep = "") 
    dfun = match.fun(paste("d", fun, sep = ""))
    pfun = match.fun(paste("p", fun, sep = ""))
    qfun = match.fun(paste("q", fun, sep = ""))
    rfun = match.fun(paste("r", fun, sep = ""))
    
    # Range:
    xmin = qfun(p = 0.01, ...)
    xmax = qfun(p = 0.99, ...)
    
    # Check 1 - Normalization:
    NORM = integrate(dfun, lower = -Inf, upper = Inf, 
        subdivisions = subdivisions, stop.on.error = FALSE, ...)
    cat("\n1. Normalization Check:\n NORM ")
    print(NORM)
    normCheck = (abs(NORM[[1]]-1) < 0.01)
    
    # Check 2:
    cat("\n2. [p-pfun(qfun(p))]^2 Check:\n ")
    p = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    P = pfun(qfun(p, ...), ...)
    pP = round(rbind(p, P), 3)
    print(pP)
    RMSE = sd(p-P)
    print(c(RMSE = RMSE))
    rmseCheck = (abs(RMSE) < 0.0001)
    
    # Check 3:
    cat("\n3. r(", n, ") Check:\n", sep = "")
    r = rfun(n = n, ...)
    if (!robust) {
        SAMPLE.MEAN = mean(r)
        SAMPLE.VAR = var(r)
    } else {
        robustSample = cov.mcd(r, quantile.used = floor(0.95*n))
        SAMPLE.MEAN = robustSample$center
        SAMPLE.VAR = robustSample$cov[1,1]
    }
    SAMPLE = data.frame(t(c(MEAN = SAMPLE.MEAN, "VAR" = SAMPLE.VAR)), 
        row.names = "SAMPLE")
    print(signif(SAMPLE, 3))
    fun1 = function(x, ...) { x * dfun(x, ...) }
    fun2 = function(x, M, ...) { x^2 * dfun(x, ...) }   
    MEAN = integrate(fun1, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE,...)
    cat("   X   ")
    print(MEAN)
    VAR = integrate(fun2, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE, ...)  
    cat("   X^2 ")
    print(VAR)
    EXACT = data.frame(t(c(MEAN = MEAN[[1]], "VAR" = VAR[[1]] - MEAN[[1]]^2)),
        row.names = "EXACT ")
    print(signif(EXACT, 3))
    meanvarCheck = (abs(SAMPLE.VAR-EXACT$VAR)/EXACT$VAR < 0.1)
    cat("\n")
    
    # Done:
    ans = list(
        normCheck = normCheck, 
        rmseCheck = rmseCheck, 
        meanvarCheck = meanvarCheck)
    unlist(ans)
}


################################################################################

