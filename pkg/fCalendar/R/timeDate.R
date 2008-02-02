
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
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'                S4 Class representation for timeDate objects
#  timeDate                  Creates a 'timeDate' object from given dates
################################################################################


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


################################################################################

