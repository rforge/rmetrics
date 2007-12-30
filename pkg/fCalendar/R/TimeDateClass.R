
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

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 SETTINGS:
#  currentYear               Sets date of the current year
#  .currentYear              Returns the the current year
#  myUnits                   Sets date units
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'                S4 Class representation for timeDate objects
#  timeDate                  Creates a 'timeDate' object from given dates
#  .whichFormat              Returns format string called by 'timeDate'
#  .midnightStandard         Corrects midnight standard called by 'timeDate'
#  .formatFinCenter          Internal called by timeDate
#  timeCalendar              Creates a 'timeDate' object from calendar atoms
#  timeSequence              Creates a regularly spaced 'timeDate' object
#  seq                       A synonyme function for timeSequence
#  Sys.timeDate              Returns system time as an 'timeDate' object 
#  is.timeDate               Tests if the object is of class 'timeDate' 
# METHODS:                  REPRESENTATION OF TIMEDATE OBJECTS:
#  show.timeDate             Prints 'timeDate' object
#  plot.timeDate             Plots 'timeDate' object
#  points.timeDate           Adds points to a 'timeDate' plot
#  lines.timeDate            Adds lines to a 'timeDate' plot
#  summary.timeDate          Summarizes details of a 'timeDate' object
#  format.timeDate           Formats 'timeDate' as ISO conform string
################################################################################


################################################################################
# FUNCTION:              SETTINGS:
#  currentYear               Sets date of the current year
#  .currentYear              Returns the the current year
#  myUnits                   Sets date units


.currentYear <- 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets date of the current year
    
    # FUNCTION:
    
    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Return current year:
    as.POSIXlt(Sys.time())$year + 1900
}


# ------------------------------------------------------------------------------


currentYear = .currentYear()


# ------------------------------------------------------------------------------


myUnits = "days"


################################################################################
# FUNCTION:              GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'             S4 Class representation for timeDate objects
#  timeDate               Creates a 'timeDate' object from given dates
#  .whichFormat            Returns format string called by timeDate
#  .midnightStandard       Corrects for midnight standard called by timeDate
#  .formatFinCenter        Internal called by timeDate
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#  seq                    A synonyme generic function for timeSequence
#  Sys.timeDate           Returns system time as an object of class 'timeDate'
#  is.timeDate            Tests if the object is of class 'timeDate'


## DW: Do we need this ?
require(methods)


# ------------------------------------------------------------------------------


setClass("timeDate",
    # A class implemented by Diethelm Wuertz

    # Description:
    #   Class representatation for 'timeDate' Objects.

    # CLASS:

    representation(
        Data = "POSIXct",
        Dim = "numeric",
        format = "character",
        FinCenter = "character"
    )
)


# ------------------------------------------------------------------------------


timeDate <-
function(charvec = Sys.timeDate(), format = NULL, zone = myFinCenter,
FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector

    # Arguments:
    #   charvec - a character vector of dates and times. Alternatively
    #       it may be a 'timeDate', a 'Date', or a 'POSIXt' object. In
    #       these cases the argument will be coerced into a character
    #       string or character vector.
    #   format - the format specification of the input character
    #       vector. If set to NULL autodetection will be tried.
    #   zone - the time zone or financial center where the data
    #       were recorded.
    #   FinCenter - a character string with the the location of
    #       the financial center named as "continent/city" where the
    #       data will be used.

    # Value:
    #   Returns a S4 object of class 'timeDate'.

    # Note:
    #   Changeover DST not yet fully implemented!

    # Examples:
    #   timeDate("2004-01-01")
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")
    #   timeDate("200401011600")
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")
    #   timeDate("20040101", FinCenter = "GMT")
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)

    # FUNCTION:

    # Settings and Checks:
    trace = FALSE
    if (FinCenter == "" || is.null(FinCenter)) FinCenter = "GMT"
    if (is.null(zone)) zone = "GMT"

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # ISO Date/Time Format:
    isoDate   <- "%Y-%m-%d"
    isoFormat <- "%Y-%m-%d %H:%M:%S"

    if (inherits(charvec, "character")) { # Autodetect Format:
        if (is.null(format))
            format <- .whichFormat(charvec)
    } else { ## convert from known classes to ISO :
        format <- isoFormat
        charvec <-
            if (is(charvec, "timeDate")) {
                format(charvec@Data, format)
            } else if (inherits(charvec, "Date")) {
                zone <- FinCenter
                format <- isoDate
                format(charvec, format)
            } else if (inherits(charvec, "POSIXt")) {
                format(charvec, format)
            }
    }

    # Midnight Standard & conversion to isoFormat:
    charvec <- .midnightStandard(charvec, format)

    # Financial Centers:
    recFinCenter = zone      # Time zone where the data were recorded
    useFinCenter = FinCenter # Time zone where the data will be used

    # Trace Input:
    if (trace) {
        cat("\nInput: ")
        print(recFinCenter)
        print(charvec)
    }

    ## Convert:
    if (recFinCenter == "GMT" && useFinCenter == "GMT") {   
        ## GMT -> GMT:
        ## nothing to do
    } else if (recFinCenter == "GMT" && useFinCenter != "GMT") {  
        ## GMT -> nonGMT
        charvec = .formatFinCenter(charvec, useFinCenter, type = "gmt2any")
    } else if (recFinCenter != "GMT" && useFinCenter == "GMT") {  
        ## nonGMT -> GMT
        charvec = .formatFinCenter(charvec, recFinCenter, type = "any2gmt")
    } else if (recFinCenter == useFinCenter) {        
        ## nonGMT -> equal nonGMT
        ## nothing to do
    } else if (recFinCenter != useFinCenter) {        
        ## nonGMT -> other nonGMT
        charvec = .formatFinCenter(charvec, recFinCenter, type = "any2gmt")
        charvec = .formatFinCenter(charvec, useFinCenter, type = "gmt2any")
    } else { 
        ## impossible
        ## when *not* returning a timeDate() object, we should warn
        message("returning NULL instead of \"timeDate\"")
        return(invisible())
    }
    
    ## In all good cases :
    if (trace) {
        cat("\nOutput: ")
        print(useFinCenter)
        print(charvec)
        cat("\n")
    }
    lt <- strptime(charvec, isoFormat)
    noTime <- lt$sec == 0 & lt$min == 0 & lt$hour == 0
    noTime <- all(noTime | is.na(noTime))

    new("timeDate",
        Data = as.POSIXct(lt),
        Dim = as.integer(length(charvec)),
        format = if(noTime) isoDate else isoFormat,
        FinCenter = useFinCenter)
}


.timeDate.OLD = 
function(charvec = Sys.timeDate(), format = NULL, zone = myFinCenter, 
FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector
    
    # Arguments:
    #   charvec - a character vector of dates and times. Alternatively
    #       it may be a 'timeDate', a 'Date', or a 'POSIXt' object. In
    #       these cases the argument will be coerced into a character
    #       string or character vector.
    #   format - the format specification of the input character 
    #       vector. If set to NULL autodetection will be tried.
    #   zone - the time zone or financial center where the data  
    #       were recorded.
    #   FinCenter - a character string with the the location of   
    #       the financial center named as "continent/city" where the
    #       data will be used.
    
    # Value:
    #   Returns a S4 object of class 'timeDate'.
    
    # Note:
    #   Changeover DST not yet fully implemented!
    
    # Examples:
    #   timeDate("2004-01-01") 
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")    
    #   timeDate("200401011600") 
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")   
    #   timeDate("20040101", FinCenter = "GMT") 
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)
    
    # FUNCTION:

    # Settings and Checks:
    trace = FALSE
    if (FinCenter == "" || is.null(FinCenter)) FinCenter = "GMT"
    if (is.null(zone)) zone = "GMT"
         
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # ISO Date/Time Format:
    isoFormat = "%Y-%m-%d %H:%M:%S"
    
    # Autodetect Format:
    if (inherits(charvec, "character")) {
        if (is.null(format)) format = .whichFormat(charvec)
    }
    
    # Crae for Other Formats:
    if (inherits(charvec, "timeDate")) {
        posix = charvec@Data 
        charvec = format(posix, isoFormat) 
        format = isoFormat
    }
    if (inherits(charvec, "Date")) {
        charvec = format(charvec) 
        zone = FinCenter
    }
    if (inherits(charvec, "POSIXt")) {
        charvec = format(charvec, isoFormat) 
        format = isoFormat
    }
    
    # Get Dimension:
    Dim = length(charvec)
 
    # Midnight Standard:
    charvec = .midnightStandard(charvec, format)
    charvec = format(strptime(charvec, .whichFormat(charvec)), isoFormat)
    format = isoFormat
    
    # Financial Centers:
    recFinCenter = zone      # Time zone where the data were recorded
    useFinCenter = FinCenter # Time zone where the data will be used
    
    # Trace Input:
    if (trace) { 
        cat("\nInput: ")
        print(recFinCenter)
        print(charvec) 
    }

    # Convert:    
    DEBUG = FALSE
    
    # GMT -> GMT:
    if (recFinCenter == "GMT" && useFinCenter == "GMT") {       
        if (DEBUG) print("if - 1:")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, format)
        if (sum(lt$sec+lt$min+lt$hour) == 0) isoFormat = "%Y-%m-%d"
        # Return Value:
        ans = new("timeDate", 
            Data = as.POSIXct(lt), 
            Dim = as.integer(Dim),
            format = isoFormat,
            FinCenter = useFinCenter)
        Sys.setenv(TZ = myTZ)
        return(ans)
    }  
        
    # GMT -> nonGMT     
    if (recFinCenter == "GMT" && useFinCenter != "GMT") {
        if (DEBUG) print("if - 2:") 
        charvec = .formatFinCenter(charvec, useFinCenter, type = "gmt2any")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, format)
        if (sum(lt$sec+lt$min+lt$hour) == 0) isoFormat = "%Y-%m-%d"
        # Return Value:
        ans = new("timeDate", 
            Data = as.POSIXct(lt), 
            Dim = as.integer(Dim),
            format = isoFormat,
            FinCenter = useFinCenter)
        Sys.setenv(TZ = myTZ)
        return(ans)
    }    
         
    # nonGMT -> GMT       
    if (recFinCenter != "GMT" && useFinCenter == "GMT") {
        if (DEBUG) print("if - 3:")
        charvec = .formatFinCenter(charvec, recFinCenter, type = "any2gmt")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, format)
        if (sum(lt$sec+lt$min+lt$hour) == 0) isoFormat = "%Y-%m-%d"
        # Return Value:
        ans = new("timeDate", 
            Data = as.POSIXct(lt), 
            Dim = as.integer(Dim),
            format = isoFormat,
            FinCenter = useFinCenter)
        Sys.setenv(TZ = myTZ)
        return(ans)
    }      
          
    # nonGMT -> equal nonGMT   
    if (recFinCenter == useFinCenter) {     
        if (DEBUG) print("if - 4:")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, format)
        if (sum(lt$sec+lt$min+lt$hour) == 0) isoFormat = "%Y-%m-%d"
        # Return Value:
        ans = new("timeDate", 
            Data = as.POSIXct(lt),
            Dim = as.integer(Dim),
            format = isoFormat ,
            FinCenter = useFinCenter)
        Sys.setenv(TZ = myTZ)
        return(ans)
    }    
            
    # nonGMT -> other nonGMT 
    if (recFinCenter != useFinCenter) {
        if (DEBUG) print("if - 5:")
        charvec = .formatFinCenter(charvec, recFinCenter, type = "any2gmt")
        charvec = .formatFinCenter(charvec, useFinCenter, type = "gmt2any")
        if (trace) { 
            cat("\nOutput: ") 
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, format)
        if (sum(lt$sec+lt$min+lt$hour) == 0) isoFormat = "%Y-%m-%d"
        # Return Value:
        ans = new("timeDate", 
            Data = as.POSIXct(lt), 
            Dim = as.integer(Dim),
            format = isoFormat,
            FinCenter = useFinCenter)
        Sys.setenv(TZ = myTZ)
        return(ans)
    }    
            
    # Return Value:
    invisible()         
}


# ------------------------------------------------------------------------------


.whichFormat =
function(charvec, silent = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Charvec String:
    charvec = as.character(charvec)

    # Specifications:
    NCHAR = mean(nchar(charvec))
    NCHAR = nchar(charvec[1])
    SUBSTR = (substring(charvec[1], 5, 5) == "-")

    # American Format:
    if (regexpr("/....", charvec[1])[[1]] > 0) return("%m/%d/%Y")
    if (regexpr("-...-....", charvec[1])[[1]] > 0) return("%d-%b-%Y")

    # Human readable ISO:
    if (NCHAR ==  4 & !SUBSTR) return("%Y")
    if (NCHAR ==  7 &  SUBSTR) return("%Y-%m")
    if (NCHAR == 10 &  SUBSTR) return("%Y-%m-%d")
    if (NCHAR == 13 &  SUBSTR) return("%Y-%m-%d %H")
    if (NCHAR == 16 &  SUBSTR) return("%Y-%m-%d %H:%M")
    if (NCHAR == 19 &  SUBSTR) return("%Y-%m-%d %H:%M:%S")

    # Short ISO:
    if (NCHAR ==  6 & !SUBSTR) return("%Y%m")
    if (NCHAR ==  8 & !SUBSTR) return("%Y%m%d")
    if (NCHAR == 10 & !SUBSTR) return("%Y%m%d%H")
    if (NCHAR == 12 & !SUBSTR) return("%Y%m%d%H%M")
    if (NCHAR == 14 & !SUBSTR) return("%Y%m%d%H%M%S")

    # Otherwise:
    if (!silent)
    warning("Could not determine time(date) format")
    
    # Return Value:
    "unknown"
}


# ------------------------------------------------------------------------------


.midnightStandard <- 
function(charvec, format)
{   # A function written by Diethelm Wuertz   
    # and entirely rewritten by Martin Maechler
    
    # Description:
    
    # FUNCTION:
    
    ## Midnight Standard & conversion to isoFormat:

    ## Motivation: strptime() {et al}  cannot deal with "24:00:00"
    ##         In that case, subtract 1 seconds convert and re-add it

    paste0 <- function(...) paste(..., sep = '')
    
    # Missing Format:
    if (missing(format)) format = .whichFormat(charvec)
    
    # Format:
    rng.nch <- range(nchar(charvec[!is.na(charvec)]))
    if(rng.nch[1] != rng.nch[2])
    stop("'charvec' has non-NA entries of different number of characters")
    nch <- rng.nch[1]

    n <- length(charvec)
    s <- rep(0, n)

    ## Do two common formats *fast* (for large n), and then use 
    ## flexible approach:
    
    # ISO-8601 Midnight Standard:
    if (length(grep("%H:%M:%S", format, fixed = TRUE)) == 1) {
        if(length(ii <- grep("24:00:00", charvec, fixed=TRUE)) > 0) {
            s[ii] <- 1
            charvec[ii] <- gsub("24:00:00", "23:59:59", charvec[ii], fixed=TRUE)
        }
    } else if (length(grep("%H%M%S$", format)) == 1) {
        ## format *ends* in  %H%M%S, i.e. last 6 chars are time
        ch.time <- substr(charvec, nch-6+1, nch)
        if(length(ii <- grep("240000$", ch.time)) > 0) {
            s[ii] <- 1
            charvec[ii] <- paste(substr(charvec[ii], 1, nch-6),
                gsub("240000$", "235959", ch.time[ii]), sep = "")
        }
    } else {
        ## Very general approach, to work for any valid format:
        forms <- c("%Y", "%m", "%d",  "%H","%M","%S")
        nums  <- c("2003","01","31",  "23","59","58") # pairwise different
        fDate <- format
        for(i in seq_along(forms)) {
            ## make sure, we don't have nums[i] already :
            if(length(grep(nums[i], fDate, fixed=TRUE)))
            fDate <- gsub(nums[i], paste(rep("x", nchar(nums[i])), collapse=""),
                      fDate, fixed=TRUE)
            fDate <- sub(forms[i], nums[i], fDate, fixed=TRUE)
        }
        ## in the ISO case, now have  fDate == "2001-01-31 23:59:58"
        names(nums) <- forms
        ## at which character positions in charvec do I need to look for %H, ... :
        iHMS <- sapply(nums[c("%H","%M","%S")], regexpr, text=fDate, fixed=TRUE)
        if(iHMS["%H"] >= 1) { 
            ## have "%H" -- otherwise, nothing to do!
            has.S <- iHMS["%S"] >= 1
            has.M <- iHMS["%M"] >= 1
            if(has.S && !has.M) stop("invalid format: has '%S' but no '%M'")
            ## 3 remaining cases:  (H,M,S), (H,M), (H)
            m. <- 1 + has.M + has.S # in {1,2,3}
            HMStab <- matrix(unlist(lapply(iHMS[1:m.],
                function(ic) substr(charvec, start=ic, stop=ic+1))), n, m.)
            twenty4 <- paste0("24", if(has.M)"00", if(has.S)"00")
            isMidN <- twenty4 == apply(HMStab, 1, paste, collapse='')
            if(any(isMidN)) { 
                ## need midnight correction
                s[isMidN] <- 1
                ## now *need* seconds, so we can subtract and add 1 sec :
                if(!has.S) {
                    if(!has.M) {
                    iHMS["%M"] <- nchar(fDate) + 1
                    format <-  paste0(format,  "%M")
                    fDate  <-  paste0(fDate,   "00")
                    charvec <- paste0(charvec, "00")
                    }
                    iHMS["%S"] <- nchar(fDate) + 1
                    format <-  paste0(format,  "%S")
                    charvec <- paste0(charvec, "00")
                }
                substr(charvec[isMidN], iHMS["%H"], iHMS["%H"]+1) <- "23"
                substr(charvec[isMidN], iHMS["%M"], iHMS["%M"]+1) <- "59"
                substr(charvec[isMidN], iHMS["%S"], iHMS["%S"]+1) <- "59"
            }
        }
    }
    
    ## Convert "charvec" to standard ISO format:
    ans = format(s + strptime(charvec, format), "%Y-%m-%d %H:%M:%S")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.midnightStandard.OLD = 
function(charvec, format)
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # Format:
    nchar.iso = mean(nchar(charvec))
    isoFormat = "%Y-%m-%d %H:%M:%S"
    
    # ISO-8601 Midnight Standard:
    s = rep(0, length(charvec))
    if (nchar.iso == 19) {
        s[grep("24:00:00", charvec)] = 1
        charvec = gsub("24:00:00", "23:59:59", charvec) 
        # Convert "charvec" to standard ISO format:
        charvec = format(strptime(charvec, format)+s, isoFormat)
    }
    if (nchar.iso == 14) {
        # Fixed DW 2006-03-13
        charvec.date = substr(charvec, 1, 8)
        charvec.time = substr(charvec, 9, 14)
        s[grep("240000", charvec.time)] = 1
        sub.charvec = substr(charvec, 9, 14)
        # charvec = gsub("240000", "235959", charvec) 
        charvec.time = gsub("240000", "235959", charvec.time) 
        charvec = paste(charvec.date, charvec.time, sep = "")
        # Convert "charvec" to standard ISO format:
        charvec = format(strptime(charvec, format)+s, isoFormat)
    }   
    
    # Return Value:
    charvec 
}


# ------------------------------------------------------------------------------


.formatFinCenter <- 
function(charvec, FinCenter, type = c("gmt2any", "any2gmt"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal function used by function timeDate()

    if (FinCenter == "GMT")
        return(charvec)

    ## else start working:

    type <- match.arg(type)
    signum <- switch(type,
                     "gmt2any" = +1,
                     "any2gmt" = -1)
    ##  otherwise give error


        # Get the DST list from the database:
        dst.list = rulesFinCenter(FinCenter)
        # Update list with last entry:
        z = as.matrix(dst.list)
        z[dim(z)[1], ]
        vec1 = as.vector(c(z[, 1], "2099-01-01 00:00:00"))
        vec2 = as.vector(c(z[, 2], rev(z[, 2])[1]))
        dst.list = data.frame(ruleChanges = as.character(vec1),
            offSet = as.integer(vec2))
        # Extract the dates when DST was changed:
        dst.dates = as.character(dst.list[, 1])
        # Extract the Offsets to GMT
        dst.offsets = as.character(dst.list[, 2])
        # The new dates ar the charvec's:
        new.dates = charvec
        # The new offsets are still unknown:
        new.offsets = rep(NA, length(charvec))
        # Combine all Dates and Offsets:
        dates = c(dst.dates, new.dates)
        offsets = c(dst.offsets, new.offsets)
        # The number of Offsets:
        n = length(dates)
        # Order the Dates:
        o = order(dates)
        # Dates and Offsets in the right order:
        o.dates = dates[o]
        o.offsets = offsets[o]
        # The points at which we have to determine the offsets
        xout = (1:n)[is.na(o.offsets)]
        # The date indexes:
        x = (1:n)[-xout]
        # The corresponding offsets
        y = o.offsets[x]
        # The new offsets:
        yout = approx(x, y , xout, method = "constant")$y
        # All dates:
        m = length(dst.dates)
        # Put them in the right order:
        # Added DW: 2005-05-27
        idx = order(o[which(o>m)])
        offSets = yout[idx]
        dt = strptime(charvec, "%Y-%m-%d %H:%M:%S")

    ## Return Value:
    format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S")
}


# ------------------------------------------------------------------------------


timeCalendar <-
function(y = currentYear, m = 1:12, d = 1, h = 0, min = 0, s = 0,
zone = myFinCenter, FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a 'timeDate' object from calendar atoms

    # Arguments:
    #   y - calendar years (e.g. 1997), defaults are 1960.
    #   m - calendar months (1-12), defaults are 1.
    #   d - calendar days (1-31), defaults are 1.
    #   h - hours of the days (0-23), defaults are 0.
    #   min - minutes of the days (0-59), defaults are 0.
    #   s - seconds of the days (0-59), defaults are 0.
    #   FinCenter - a character sting with the the location of the
    #       financial center named as "continent/city"

    # Value:
    #   Returns a 'timeDate' object corresponding to the "atomic"
    #   inputs. For the default arguments the first day in each
    #   month of the current year will be returned.

    # Details:
    #   Creates a 'timeDate' object from date as month, day, year and
    #   time of day as hours, and minutes [seconds, milliseconds]

    # Note:
    #   The 'zone' where the data were recorded is fixed to myFincenter!
    #   The argument list has ISO-8601 ordering!
    #   ms - Milliseconds is not supported.

    # Example:
    #   x = timeCalendar(y = 2000, h = rep(16,12))
    #   x = timeCalendar(m = c(3,4,5), d = c(12,15,7), y = c(1998,1997,2004))
    #   x = timeCalendar(h = c(9,14), min = c(15,23))

    # FUNCTION:

    # Settings and Check:
    trace = FALSE
    if (FinCenter == "") FinCenter = "GMT"
    if (is.null(h) & is.null(min) & is.null(s)) zone = FinCenter

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Check Input:
    len = c(length(m), length(d), length(y), length(h), length(min), length(s))
    data.len = max(len)
    if (data.len < 1)
        stop("No arguments defined!")
    if (any((data.len %% len[len > 0]) != 0))
        stop("Arguments have incompatible lengths")

    # Make All Arguments the Same Length:
    if (len[1] == 0) m = 1
    if (len[2] == 0) d = 1
    if (len[3] == 0) y = 1960
    if (len[4] == 0) h = 0
    if (len[5] == 0) min = 0
    if (len[6] == 0) s = 0

    # Presettings:
    # m = rep(m, length = data.len)
    # d = rep(d, length = data.len)
    # y = rep(y, length = data.len)
    # h = rep(h, length = data.len)
    # min = rep(min, length = data.len)
    # s = rep(s, length = data.len)
    # DW 2006-03-13
    if (length(m) < data.len) m = rep(m, length = data.len)
    if (length(d) < data.len) d = rep(d, length = data.len)
    if (length(y) < data.len) y = rep(y, length = data.len)
    if (length(h) < data.len) h = rep(h, length = data.len)
    if (length(min) < data.len) min = rep(min, length = data.len)
    if (length(s) < data.len) s = rep(s, length = data.len)

    # Date-Time Strings:
    # Note Format is always of type  "%Y%m%d%H%M%S"  !
    CCYYMMDD = as.integer(y*10000 + m*100 + d)
    chardate = as.character(CCYYMMDD)
    xhhmmss = as.integer(1000000 + h*10000 + min*100 + s)

    # Date and Date/Time Checks:
    if (mean(xhhmmss) == 1000000) {
        chartime = substr(as.character(xhhmmss), 2, 7)
        charvec = as.vector(chardate)
        format = "%Y%m%d"
    } else {
        chartime = substr(as.character(xhhmmss), 2, 7)
        charvec = paste(as.vector(chardate), as.vector(chartime), sep = "")
        format = "%Y%m%d%H%M%S"
    }

    # Return Value:
    timeDate(charvec = charvec, format = format,
             zone = zone, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeSequence <- 
function(from, to = format(Sys.time(), "%Y-%m-%d"),
by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
length.out = NULL, format = NULL,
zone = myFinCenter, FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a regularly spaced 'timeDate' object

    # Arguments:
    #   from - starting date. Required.
    #   to - end date. Optional. If supplied must be after from.
    #   by - a character string, containing one of "sec", "min",
    #       "hour", "day", "week", "month" or "year".
    #       This can optionally be preceded by an integer and a
    #       space, or followed by "s".
    #   length.out - length.out integer, optional. Desired length
    #       of the sequence, if specified "to" will be ignored.
    #   format - the format specification of the input character
    #       vector.
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city".

    # Value:
    #   Returns a 'timeDate' object corresponding to the "sequence"
    #   specification.

    # Note:
    #   The 'zone' where the data were recorded is fixed to myFincenter!

    # Example:
    #   x = timeSequence("2004-01-28", "2004-02-04", by = "day")
    #   x = timeSequence("2004-01-01", "2005-01-31", by = "month")
    #   x = timeSequence("2004-01-28", by = "day", length.out = 10)
    #   x = timeSequence("2004-01-01", by = "month", length.out = 12)
    #   x = timeSequence("2004-01-28 18:00:00", "2004-01-29 05:00:00", by = "hour")
    #   x = timeSequence("2004-01-28 18:00:00", by = "hour", length.out = 12)

    # FUNCTION:

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Settings and Checks:
    if (!is.null(length.out)) to = from
    if (FinCenter == "") FinCenter = "GMT"
    by = match.arg(by)
    if (by == "quarter") by = "3 months"

    # Auto-detect Input Format:
    format.from = format.to = format
    if (is.null(format)) {
        format.from = .whichFormat(as.character(from))
        format.to = .whichFormat(as.character(to))
    }
    if (format.from == format.to) {
        format = format.from
    } else {
        stop ("Args from and to must have the same format specification.")
    }

    # Create Charvector:
    from = strptime(as.character(from), format = format)
    if (is.null(length.out)) {
        # The start "from" and end date "to" must be specified!
        to = strptime(as.character(to), format = format)
        charvec = format(seq.POSIXt(from = from,
            to = to, by = by), format)
    } else  {
        # The end date is missing and has to be specified
        charvec = format(seq.POSIXt(from = from, by = by,
            length.out = length.out))
    }

    # Return timeDate Object:
    timeDate(charvec = charvec, format = .whichFormat(charvec[1]),
             zone = zone, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


seq.timeDate =
function(from, to,
by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
length.out = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A synonyme function for timeSequence.

    # Arguments:
    #   from, to - two 'timeDate' objects

    # Changes:
    #

    # FUNCTION:

    # Check:
    stopifnot(class(from) == "timeDate")
    stopifnot(class(to) == "timeDate")

    # Sequence:
    by = match.arg(by)
    to = timeDate(to, zone = to@FinCenter, FinCenter = from@FinCenter)
    ans = timeSequence(from = from, to = to, by = by, length.out = length.out,
        format = NULL, zone = from@Fincenter, FinCenter = from@FinCenter)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


Sys.timeDate =
function(FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns system time as an object of class 'timeDate'

    # Arguments:
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city"

    # Value:
    #   Returns the system time as an object of class 'timeDate'.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    if (FinCenter == "") FinCenter = "GMT"

    # Return System Time:
    timeDate(as.character(Sys.time()), zone = "GMT", FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


is.timeDate =
function(object)
{   # A function implemented by Diethelm Wuertz

    ## MM: should be deprecated ---  use   is(object, "timeDate") !!!
    .Deprecated("is( . , \"timeDate\")")

    # Description:
    #   Checks if object is of class 'timeDate'

    # Arguments:
    #   object - a 'timeDate' object to be checked.

    # Value:
    #   Returns 'TRUE' or 'FALSE' depending on whether its
    #   argument is of 'timeDate' type or not.

    # Changes:
    #

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # Check Object:
    ans = inherits(object, "timeDate")

    # Return Value:
    Sys.setenv(TZ = myTZ)
    ans
}


################################################################################
# S3 METHODS:            REPRESENTATION OF TIMEDATE OBJECTS:
#  show.timeDate          Prints 'timeDate' object
#  plot.timeDate          Plots 'timeDate' object
#  points.timeDate        Adds points to a 'timeDate' plot
#  lines.timeDate         Adds lines to a 'timeDate' plot
#  summary.timeDate       Summarizes details of a 'timeDate' object
#  format.timeDate        Formats 'timeDate' as ISO conform character string


show.timeDate = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for an S4 object of class "timeDate"
 
    # FUNCTION:
       
    # Unlike print the argument for show is 'object'.
    x = object
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Print:
    cat(x@FinCenter, "\n", sep = "")
    layout = paste("[", as.character(x@Data), "]", sep = "")
    
    # timeDate:
    Sys.setenv(TZ = myTZ)
    print(layout, quote = FALSE)
    
    # Control:
    control = attr(x, "control")
    if (!is.null(control)) print(control)
    
    # Return Value:
    invisible(x)
}


setMethod("show", "timeDate", show.timeDate)


# ------------------------------------------------------------------------------


plot.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
 
    # Plot:
    plot(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


points.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
    
    # Add Points:
    points(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


lines.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
    
    # Add Lines:
    lines(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


summary.timeDate =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summarizes details of a 'timeDate' object

    # Arguments:
    #   x - a 'timeDate' object to be summarized.

    # Effect:
    #   Produce a summary report of the details of a 'timeDate'
    #   object.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Print:
    x = object
    cat(  "Object:       ", as.character(match.call())[2])
    cat("\nStart Record: ", as.character(start(x)))
    cat("\nEnd Record:   ", as.character(end(x)))
    cat("\nObservations: ", length(as.character(x)))
    cat("\nFormat:       ", x@format)
    cat("\nFinCenter:    ", x@FinCenter)
    cat("\n")

    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


format.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Formats 'timeDate' as ISO conform character string

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns an ISO conform formatted character string.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Format - format.POSIXlt(x, format = "", usetz = FALSE, ...)
    format.POSIXct(x@Data, ...)
}


################################################################################

