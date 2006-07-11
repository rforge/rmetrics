
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
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  xjulian               Compute Julian minute numbers from ISO-8601 dates
#  xdate                 Compute ISO-8601 date/times from Julian minute numbers
#  xday.of.week          Compute day of the week for ISO-8601 dates
#  xleap.year            Return T/F if date/times belong to leap years or not
# FUNCTION:             DESCRIPTION:
#  fxdata.contributors   Creates a table with contributor names    
#  fxdata.parser         Parses FX contributors and delay times
#  fxdata.filter         Filters price and spread values from FX data records
#  fxdata.varmin         Aggregates data records to variable minutes data format
# FUNCTION:             DESCRIPTION:
#  xts.log               Calculates logarithms for xts time series values
#  xts.diff              Differentiates xts time series values with lag=1
#  xts.cut               Cuts a piece out of a xts time series
#  xts.interp            Interpolates for equidistant time steps
#  xts.map               Creates a volatility adjusted time-mapping 
#  xts.upsilon           Interpolates a time series in upsilon time
#  xts.dvs               Creates a de-volatilizised time series
#  xts.dwh               Plots intra-daily/weekly histograms 
################################################################################


xjulian = 
function (xdates, origin = 19600101)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates Julian minute counts from ISO-8601 Gregorian
    #   dates/times.
    
    # Arguments: 
    #   XDATES: CCYYMMDDhhmm format expected
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Date/Time:
    cc = xdates %/% 10000000000 
    yymmdd = xdates %/% 10000 - cc * 1000000
    yy = yymmdd %/% 10000
    mm = yymmdd %/% 100 - yy * 100
    dd = yymmdd - yy * 10000 - mm * 100
    hhmm = xdates - 10000000000 * cc - 10000 * yymmdd
    hh = hhmm %/% 100
    ms = hhmm-100 * hh
    
    # Origin: 
    cc0 = origin %/% 1000000
    yymmdd0 = origin-cc0*1000000
    yy0 = yymmdd0 %/% 10000
    mm0 = yymmdd0 %/% 100 - yy0 * 100
    dd0 = yymmdd0 - yy0 * 10000 - mm0 * 100
    
    # Return Value:
    1440 * .julian(mm, dd,100*cc + yy, 
        origin=c(mm0, dd0, 100*cc0+yy0)) + 60*hh + ms
}


# ------------------------------------------------------------------------------


xdate =  
function (xjulians, origin = 19600101)
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Calculates ISO-8601 Gregorian dates/times from Julian
    #   minute counts.
    
    # Arguments: 
    #   ORIGIN: CCYYMMDD expected
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Julians:
    cc0 = origin%/%1000000
    yymmdd0 = origin-cc0*1000000
    yy0 = yymmdd0%/%10000
    mm0 = yymmdd0%/%100-yy0*100
    dd0 = yymmdd0-yy0*10000-mm0*100
    
    # EXTENDED JULIAN COUNTING: expected in minutes
    # TO XDATE:
    julians = xjulians%/%1440
    xgreg = month.day.year(julians,origin=c(mm0,dd0,cc0*100+yy0))
    ccyymmdd = xgreg$year*10000+xgreg$month*100+xgreg$day
    minutes = xjulians-1440*julians
    hh = minutes%/%60
    ms = minutes-60*hh
    hhmm = 100*hh+ms
    
    # Return Value:
    ccyymmdd*10000+hhmm
}


# ------------------------------------------------------------------------------


xday.of.week =  
function(xdates)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the day of the week from ISO-8601 Gregorian
    #   dates/times
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    .sday.of.week(xdates %/% 10000)
}


# ------------------------------------------------------------------------------


xleap.year =  
function(xdates)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Decides whether ISO 8601 Gregorian dates/times are leap
    #   years or not.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    .sleap.year(xdates %/% 10000)
}


################################################################################



fxdata.contributors = 
function(x, include = 10)
{   # A function implemented by Diethelm Wuertz
    #
    # Description:
    #   Create a table with contributor names 
    #
    # Note:
    #   This function expects an ASCII data file with intradaily records
    #   from the FX market and creates a standardized data.frame. 
    #   The variables "datetime", "timestamp", "contributor", "bid", "ask",
    #   and "tag" describe in which column of the original file the
    #   corresponding information is positioned. Note, "datetime" is
    #   expected to be in ISO-8601 date/time format as "CCYYMMDDhhmm".
    #   If the file has a header its length in number of lines has to 
    #   be specified by the variable "skip".
    #   The data fields are comma separated, otherwise the field separator 
    #   has to be given explixitely through the variable "sep".
    #   File Columns:
    #   The function reads from 5-column input file, with arbitrary
    #   order of the columns. By default in 
    #   column 1 datetime (ISO-8601) "CCYYMMDDhhmm" is expected, in 
    #   column 2 timestamp (record delay in min) "mm" is expected, in
    #   column 3 contributor (4-digit code) "XXXX" is expected, in
    #   column 4 bid price, in
    #   column 5 ask price, and in
    #   column 6 tag are expected.
    
    # Values:
    #   The created output is a data.frame with 5 comma separated fields: 
    #   date/time in ISO-8601 format, an additional timestamp, the 
    #   contributors name, and the bid and ask price. The header is
    #   of the form "CCYYMMDDhhmm,TIMESTAMP,CONTRIBUTOR,BID,ASK".
    
    # Arguments:
    #   x       Standard FX data.frame with columns "datetime, 
    #           timestamp, contributor, bid, ask, tag", in the same 
    #           format as specified in "xts.fxread".
    #   include How many contributors should be included? By default
    #           the first 10 market makers
    
    # Changes:
    #
    
    # FUNCTION:
        
    # Parser Table:
    sorted = sort(x[,3])
    contributor = as.vector(unique(sorted))
    counts = diff(grep("FALSE",
        c(as.character(duplicated(sorted)), "FALSE")))
    percent = round(100*counts/sum(counts), digits=4)
    select = substring(as.character(percent > include), 1, 1)
    contributor.table = data.frame(cbind(contributor, 
        counts, percent, select))
    names(contributor.table) = c("CONTRIBUTOR", "COUNTS",
        "PERCENT", "SELECT")    
    # Return Value:
        contributor.table
}


# ------------------------------------------------------------------------------


fxdata.parser = 
function(x, parser.table)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Create or read a table with contributor names and parse the
    #   data. Return the data according to this parser table. I.e., 
    #       only those contributors are parsed who are listed in the 
    #   parser table. 
    
    # Arguments:
    #   x   Standard FX data.frame with columns "datetime, 
    #       timestamp, contributor, bid, ask, tag", in the same 
    #       format as specified in "xts.fxread".
    
    # Changes:
    #
    
    # FUNCTIOM:
    
    # Parse Contributor List:   
    index = NA
    contributors <<- levels(parser.table[,1])[parser.table[,4] == "T"]
    for (contributor in contributors)  { 
        new.index = grep(contributor, as.character(x[, 3]))
        index = c(index, new.index)
        }
    index = sort(index[2:length(index)])
    
    # Return Values:
    x[index, ]   
}


# ------------------------------------------------------------------------------


fxdata.filter = 
function(x, parameter = "strong", doprint = TRUE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Filter a FX time series
    
    # Note:
    #   Function Calls:
    #   Fortran: SUBROUTINE FXFILTER()
    
    # Changes:
    #
    
    # FUNCTION:
        
    # Parameter Settings:
    # Select one from the two defaults:
    if(is.character(parameter))  { 
        if(parameter == "strong")  
            fxparam = c(0.5, 2.0, 0.18, 0.25, 1.3, 45.0, 
                4.0, 0.00010,0.025)  
        if(parameter == "weak")
            fxparam = c(0.5, 2.2, 0.27, 0.40, 1.5, 75.0,
                5.5, 0.00008,0.004) }
    # Tailored Parameter Selection:
    else {
        fxparam = parameter }
        
    # Now filter the data.
    # FORTRAN subroutine 
    # fxfilter(xjulian, bid, ask, fparm, index, nd, nacc)
    #   if index is positive, the price is accepted, otherwise
    #   if index is negative, the prise is filtered out
    #   the value itself serves as a counter
    if(doprint) { 
        cat("\nFiltering Data Set ...")
        cat("\n  Filter Parameter:")
        cat("\n    D  = ", fxparam[1])
        cat("\n    S  = ", fxparam[2]) 
        cat("\n    A  = ", fxparam[3])
        cat("\n    Q  = ", fxparam[4]) 
        cat("\n    C  = ", fxparam[5])
        cat("\n    T  = ", fxparam[6]) 
        cat("\n    U  = ", fxparam[7])
        cat("\n    V  = ", fxparam[8]) 
        cat("\n    W  = ", fxparam[9], "\n")}   
    julians = xjulian(as.numeric(x[,1]), 
        origin=floor(as.numeric(x[1,1])/10000))
    nd = length(julians)
    result = .Fortran("fxfilter",
        as.double(julians),
        as.double(as.vector(x[,4])),
        as.double(as.vector(x[,5])),
        as.double(fxparam),
        as.integer(1:nd),
        as.integer(nd),
        PACKAGE = "fCalendar")
    index = result[[5]]
    
    # Return Result:
    list(
        accepted = x[ index[index > 0],], 
        rejected = x[-index[index < 0],])
}


# ------------------------------------------------------------------------------


fxdata.varmin = 
function(x, digits = 4)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Creates from a standardized FX high frequency data file
    #   a variable minutes formatted data file, i.e. all data
    #   records within the same minute are averaged.
    
    # Note:
    #   Function Calls:
    #   Fortran: SUBROUTINE FXVARMIN()
    #   Functions:
    #   xjulian, 
    #   Fortran: fxvarmin

    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    julian = xjulian(x[, 1], origin=x[1, 1]%/%10000)    
    index = unique(sort(julian))    
    
    # Subroutine(bid, ask, vbid, vask, julian, icount, nx, nv)
    result = .Fortran("fxvarmin",
        as.double(x[, 4]),
        as.double(x[, 5]),
        as.double(rep(0, times = length(index))),
        as.double(rep(0, times = length(index))),
        as.integer(julian),
        as.integer(rep(0, times = length(index))),
        as.integer(length(julian)),
        as.integer(length(index)),
        PACKAGE = "fCalendar")    
    # Bind columns return as data.frame:
    result = data.frame(cbind(
        unique(x[, 1]), 
        rep(0, times=length(index)),
        rep("MEAN",times=length(index)),
        round(result[[3]], digits=digits),
        round(result[[4]], digits=digits),
        result[[6]] 
        ))
    names(result) = c("XDATE", "DELAY", "CONTRIBUTOR", 
        "BID", "ASK", "FLAG")   
    
    # Return Result:
    result
}


################################################################################



xts.log = 
function(xts)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Calculate logarithms for xts time series values;
     
    # Details:
    #   This function is mainly used for calculating 
    #   log-prices from prices.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Values:
    list(t = xts$t, x = log(xts$x))
}


# ------------------------------------------------------------------------------


xts.diff = 
function(xts)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Differnetiate xts time series values with lag=1
    #   and add a zero at the beginning so that the
    #   differentiated time series has the same length
    #   as the original time series; mainly used to 
    #   calculate log-returns from log-prices.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Values:
    list(t = xts$t, x = c(0, diff(xts$x)))
}


# ------------------------------------------------------------------------------


xts.cut =  
function(xts, from.date, to.date)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Cut a piece out of a xts time series:
    #   Simply forward/back-extrapolate if 'from' 
    #   and/or 'to' is out of the data range.
    
    # Notes:
    #   This kind of extrapolation to the start and 
    #   end date may be not a good and final solution.
    #   Perhaps one should also have the option to
    #   provide a first and last xts record; e.g. for
    #   the extraction of monthly data, the last xts
    #   record from the previous month and the first
    #   record from the following month.) 

    # Changes:
    #
    
    # FUNCTION: 
        
    # Date/time:
    from = from.date*10000+0000
    to = to.date*10000+2359 
    
    # Extrapolate:
    time = xts$t
    x = xts$x
    if (from < time[1]){
        time = c(from, time) 
        x = c(x[1], x)}
    if (to > time[length(time)]){
        time = c(time, to)
        x = c(x, x[length(x)])} 

    # Return Values:
    list(t=time[time >= from & time <= to], x=x[time >= from & time <= to])
}


# ------------------------------------------------------------------------------
 

xts.interp = 
function(xts, deltat = 1, method = "constant")
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Create time-steps "equidistant in physical time"
    #   for the data records for the time range starting
    #   at "from.date" (CCYYMMDD) and ending at "to.date".
    #   The time intervals are of length "deltat" measured
    #   in minutes. 
    #   The method to be used in approximating the interpolation 
    #   is described by "method". This must be either "linear" or 
    #   "constant". "linear" results in a linear interpolation,
    #   whereas "constant" keeps the previous value (left value)
    #   within the interpolation region.    
     
    # Note:
    #   ceiling(), floor(), approx(), xjulian(), xdate()

    # Changes:
    #
    
    # FUNCTION:
    
    # Date/time - Fill the current day:
    from = floor(xts$t[1]/10000)*10000+0000
    to = floor(xts$t[length(xts$t)]/10000)*10000+2359
        
    # Out-of-Range Extrapolation:
    time = xts$t; x = xts$x
    if (from < time[1]) {
        time = c(from, time)
        x = c(x[1], x) }
    if (to > time[length(time)]){ 
        time = c(time, to)
        x = c(x, x[length(x)]) }    
            
    # Convert to Julian Calender times:
    time = xjulian(time)    
        
    # Interpolate:
    tapprox1 = ceiling(xjulian(from)/deltat)
    tapprox2 = floor(xjulian(to)/deltat)
    tapprox = deltat*(tapprox1:tapprox2)    
    
    # Interpolation:
    # Collapsing to unique x values in "approx" may be likely.
    # This results in warning messages, which we will suppress.
    currentWarningFlag = .Options$warn
    options(warn=-1)
    x.prox = approx(time, x, tapprox, method = method)$y
    options(warn=currentWarningFlag)
        
    # Return Value:
    list(t = xdate(tapprox), x = x.prox)
}


# ------------------------------------------------------------------------------


xts.map = 
function(xts, mean.deltat, alpha) 
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Create a volatility adjusted time-mapping for a later 
    #   extraction of records in weekly periodic upsilon time.
     
    # Notes:
    #   "from" must start on a Monday, 
    #   "to" must end on a Sunday!
    #   The proper starting and ending day is not yet 
    #   automatically be checked. See als the ToDo in
    #   the function xts.cut().
    #   Function Calls:
    #   matrix(), apply(), approx()
     
    # Changes:
    #
    
    # FUNCTION:
 
    # Interpolate prices:
    xts = xts.cut(xts=xts, 
    from.date = floor(xts$t[1]/10000), 
    to.date = floor(xts$t[length(xts$t)])/10000)
    epts = xts.interp(xts, deltat=1)    
        
    # Volatilities on 1 minute time intervals:
    volas = matrix(abs(xts.diff(xts.log(epts))$x)^alpha,
        ncol = 10080, byrow = TRUE)     
            
    # Weekly mean volatilities:
    vmean = apply(volas, 2, mean)   
        
    # During the week normalized cumulated volatilies:
    cumvmean = cumsum(vmean)
    cumvmean = 10080*cumvmean/cumvmean[length(cumvmean)]    
        
    # Calculate time map on predefined mean.dt
    timesteps = 10080/mean.deltat
    tvals = mean.deltat*(1:timesteps)
    tmap = approx(cumvmean, 1:10080, tvals, method="linear")$y  
        
    # Calculate all timepoints t for the whole time period 
    # from/to:
    time = apply(matrix(10080*(0:(nrow(volas)-1)),
        ncol = 1, byrow = TRUE), 1, rep, timesteps)
    time = matrix(time, ncol=1, byrow = TRUE) +
        matrix(rep(tmap, nrow(volas)), ncol = 1, byrow = TRUE)  
            
    # Interpolate time series for those points:
    # xmap physical time, ymap risk-adjusted time
    tapprox = approx(1:length(epts$x), epts$x,
        time, method = "linear")    
            
    # Return Value:
    list(xmap = tvals, ymap = round(tmap))
}


# ------------------------------------------------------------------------------


xts.upsilon = 
function(xts, weekly.map = seq(from = 59, by = 60, length = 168), 
method = "constant", doplot = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Interpolate data (prices, returns, etc. on a weekly.
    #   time schedule. The time schedule "weekly.map" is a 
    #   vector counting its elements in minutes from the 
    #   beginning to the end of the week. For hourly entries
    #   the length of the vector is 168. 
    #   "seq(from=59, by=60, length=168)
    #   creates such a map.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolate:
    xts = xts.interp(xts=xts, deltat=1, method=method)
    xts.t = matrix(xts$t, byrow=TRUE, ncol=7*24*60)
    xts.x = matrix(xts$x, byrow=TRUE, ncol=7*24*60)
    zt = xts.t[, weekly.map[1]]
    zx = xts.x[, weekly.map[1]]
    for (i in 2:length(weekly.map)) {
        zt = cbind(zt, xts.t[, weekly.map[i]])
        zx = cbind(zx, xts.x[, weekly.map[i]])  }
    if(doplot) plot(as.vector(t(zx)), type = "l", ylab = "Prices", ...) 
        
    # Return Value:
    list(t = as.vector(t(zt)), x = as.vector(t(zx))) 
}
 

# ------------------------------------------------------------------------------
        

xts.dvs = 
function(xts, k, volatility, doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Create a de-volatilizised time series according
    #   to Bin Zhous dv-series algorithm. "xts" are 
    #   prices.
    
    # Arguments:
    #   Fortran: SUBROUTINE DVSERIES()
    #   subroutine dv(vmax, s, ms, nt, ns, k)
    #   vmax    volatility threshold 
    #   s(nt)   original time series of log prices
    #   ms(nt)  index
    #   nt      their lengths
    #   ns
    #   k       length of averaging interval
    
    # Changes:
    #
    
    # FUNCTION: 
    
    # Settings:
    xts = xts.log(xts)
    nt = length(xts$x)
    ns = 0
        
    # Execute Fortran Program:
    result = .Fortran("dv",
        as.double(volatility),
        as.double(xts$x),
        as.integer(rep(0, nt)),
        as.integer(nt),
        as.integer(ns),
        as.integer(k),
        PACKAGE = "fCalendar")
        
        
    # Select dv-Series:
    test = sum(result[[3]][result[[3]]>0])
    
    # Plot:
    if(doplot && test > 0) 
        plot(exp(xts$x[result[[3]]>0]), type="l", ylab="Prices", ...)
            
    # Return Value:
    list(t=xts$t[result[[3]]>0], x=exp(xts$x[result[[3]]>0]))
}


# ------------------------------------------------------------------------------


xts.dwh = 
function(xts, deltat = 60, period = "weekly", dolog = TRUE, dodiff = TRUE, 
doplot = TRUE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Plot intra-daily/weekly histogram of volatility. 
    #   "xts" is the data list(t,x) input , may be either a price,
    #   a logprice, or return; choose properly "dolog" and 
    #   "dodiff" flags:
    #   xts prices - dolog=TRUE dodiff=TRUE
    #   xts logprices - dolog=FALSE dodiff=TRUE
    #   xts (log)returns - dolog=FALSE dodiff=FALSE
    #   "from.date" (CCYYMMDD) and "to.date" cut out a proper
    #   part of the time series. Start on Monday, so the weekly
    #   plot also starts on Monday.
    #   "deltat" is the width of the bins in minutes. 
    #   "period" may be one of "daily", "weekly" or "both"
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolate:
    xts = xts.interp(xts, deltat=deltat)
    if(dolog) xts = xts.log(xts)
    if(dodiff) xts = xts.diff(xts)
    mult = 60/deltat
    nd = 1440/deltat
    nw = 7*nd
    if(period == "daily" || period == "both") { 
    xd = apply(matrix(abs(xts$x), byrow = TRUE, ncol = nd), 2, mean)
        xd = as.vector(matrix(c(xd, xd, rep(0, length(xd))), 
            byrow = TRUE, ncol = nd))
        td = as.vector(matrix(c(1:nd, 1:nd, 1:nd), byrow = TRUE, 
            ncol = nd))/mult
        if(doplot) plot(x = c(0,0,td,0), y = c(0,xd,0,0), type = "l", 
            xlab = "hours", ylab = "mean", main = "Daily", 
            xlim = c(0,24)) }
    if(period == "weekly" || period == "both") { 
    xw = apply(matrix(abs(xts$x), byrow = TRUE, ncol = nw), 2, mean)
        xw = as.vector(matrix(c(xw, xw, rep(0, length(xw))), 
            byrow = TRUE, ncol = nw))
        tw = as.vector(matrix(c(1:nw, 1:nw, 1:nw), byrow = TRUE, 
            ncol = nw))/mult
        if(doplot) plot(x = c(0,0,tw,0), y = c(0,xw,0,0), type = "l",
            xlab = "hours", ylab = "mean", main = "Weekly", 
            xlim = c(0,168)) }

    # Result:
    if (period == "daily") result = list(td = td, xd = xd)
    if (period == "weekly") result = list(tw = tw, xw = xw)
    if (period == "both") result = list(td = td, xd = xd, tw = tw, xw = xw)
    
    # Return Value: 
    result
}


################################################################################



