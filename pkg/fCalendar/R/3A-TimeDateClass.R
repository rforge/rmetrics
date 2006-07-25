
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:              FINANCIAL CENTERS:
#  myFinCenter            Sets my financial center
#  rulesFinCenter         Returns DST rules for a financial center
#  listFinCenter          Lists all supported financial centers
# FUNCTION:              GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'             S4 Class representation for timeDate objects
#  timeDate               Creates a 'timeDate' object from given dates
#  .whichFormat            Returns format string called by timeDate
#  .midnightStandard       Corrects for midnight standard called by timeDate
#  .formatFinCenter        Internal called by timeDate
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#   seq.timeDate           A synonyme function for timeSequence
#  Sys.timeDate           Returns system time as an object of class 'timeDate' 
#  is.timeDate            Tests if the object is of class 'timeDate' 
# METHODS:               REPRESENTATION OF TIMEDATE OBJECTS:
#  print.timeDate         Prints 'timeDate' object
#  plot.timeDate          Plots 'timeDate' object
#  points.timeDate        Adds points to a 'timeDate' plot
#  lines.timeDate         Adds lines to a 'timeDate' plot
#  summary.timeDate       Summarizes details of a 'timeDate' object
#  format.timeDate        Formats 'timeDate' as ISO conform character string
################################################################################


################################################################################
# FUNCTION:              FINANCIAL CENTERS:
#  myFinCenter            Sets my financial center
#  rulesFinCenter         Returns DST rules for a financial center
#  listFinCenter          Lists all supported financial centers 


myFinCenter = "Zurich"


# ------------------------------------------------------------------------------

  
rulesFinCenter =
function(FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Show the day light saving rules for a financial center
    
    # Arguments:
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Internal Function for Conversion from Ical Tables:
    if (FALSE) {
    rulesFinCenter2 = 
    function(FinCenter = myFinCenter) {   
        # A function implemented by Diethelm Wuertz 
        # Description:
        #   Show the day light saving rules for a financial center      
        # Arguments:
        #   FinCenter - a character string with the the location of the  
        #       financial center named as "continent/city".         
        # Value:
        #   Returns a printed list of DST rules.        
        # Example:
        #   > rulesFinCenter("Zurich")
        #               ruleChanges offSet
        #   1   1894-05-31 23:30:16   3600
        #   2   1940-11-01 23:00:00   7200
        #   3   1940-12-30 22:00:00   3600
        #   5   1941-10-04 22:00:00   3600
        #   6   1942-05-03 01:00:00   7200
        #   7   1942-10-03 22:00:00   3600
        #   8   1980-12-31 23:00:00   3600
        #   9   1981-03-29 01:00:00   7200
        #   ...     
        # Note:
        #   Important, the "TZ" environment variable must set 
        #   to "GMT" in your Windows Environment!       
        # Set Timezone to GMT:
        myTZ = Sys.getenv("TZ")  
        Sys.putenv(TZ = "GMT")
        if (FinCenter == "") FinCenter = "GMT"      
        # Read the Rules:
        # Get IcalPath from .FirstLib
        file = paste(IcalPath, FinCenter, sep = "")
        zfile <- zip.file.extract(file, "Rdata.zip")
        ical = read.table(zfile, skip = 2)              
        # GMT Offsets:
        hm = as.integer(ical[,6])
        sg = sign(hm)
        hm = abs(hm)
        h = floor(hm/100)
        hms.off = sg * ( floor(hm/100)*3600 + (hm - 100*h)*60 + 0 )
        hms.off  
        # When have the rules changed?
        months.num = 1:12
        names(months.num) = c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        Y = as.integer(ical[,4])
        m = as.integer(months.num[as.character(ical[,3])])
        d = as.integer(ical[,2])
        CCYYMMDD = as.character(Y*10000+100*m+d)
        hms = unlist(strsplit(as.character(ical[,5]), ":"))
        hms = matrix(as.integer(hms), byrow=TRUE, ncol=3)
        hms = 1000000 + 10000*hms[,1] + 100*hms[,2] + hms[,3]
        hhmmss = substr(as.character(hms), 2, 7)
        ruleChangesGMT = strptime(paste(CCYYMMDD, hhmmss), "%Y%m%d %H%M%S")
        attr(ruleChangesGMT, "tzone") <- "GMT"      
        # Return Value:
        Sys.putenv(TZ = myTZ)
        data.frame(ruleChanges = as.character(ruleChangesGMT), 
            offSet = hms.off) } 
    }
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
       
    # Match City:
    City = strsplit(FinCenter, "/")[[1]][length(strsplit(FinCenter, "/")[[1]])]
    fun = match.fun(City)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    fun()
}  


# ------------------------------------------------------------------------------


listFinCenter = 
function(pattern = "*")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   List available Time Zones in Database
    
    # Arguments:
    #   pattern - a pattern character string which can be recognized
    #       by the 'grep' functs. Wild cards are allowed.
    
    # Value:
    #   Returns a printed list of financia centers. 
    
    # Example:
    #   > listFinCenter("Europe/*")
    #    [1] "Europe/Amsterdam"   "Europe/Andorra"     "Europe/Athens"     
    #    [4] "Europe/Belfast"     "Europe/Belgrade"    "Europe/Berlin"     
    #    [7] "Europe/Bratislava"  "Europe/Brussels"    "Europe/Bucharest"  
    #   [10] "Europe/Budapest"    "Europe/Chisinau"    "Europe/Copenhagen" 
    #   [13] "Europe/Dublin"      "Europe/Gibraltar"   "Europe/Helsinki"   
    #   [16] "Europe/Istanbul"    ...   
    
    # Note:
    #   The timezone database is required.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Load Database:
    # require(fBasics)
    data(timezones.db)
    tz = as.character(unclass(timezones.db)$TIMEZONES)
    
    # Financial Centers:
    if (pattern == "*") pattern = "\\\\*"
    result = as.character(tz[grep(pattern = pattern, x = tz)])
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    result
}
    

################################################################################
# FUNCTION:              GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'             S4 Class representation for timeDate objects
#  timeDate               Creates a 'timeDate' object from given dates
#  .whichFormat            Returns format string called by timeDate
#  .midnightStandard       Corrects for midnight standard called by timeDate
#  .formatFinCenter        Internal called by timeDate
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#   seq.timeDate           A synonyme function for timeSequence           
#  Sys.timeDate           Returns system time as an object of class 'timeDate'
#  is.timeDate            Tests if the object is of class 'timeDate'


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
   

timeDate = 
function(charvec = Sys.timeDate(), format = NULL, zone = myFinCenter, 
FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   format - the format specification of the input character 
    #       vector.
    #   zone - the time zone or financial center where the data  
    #       were recorded.
    #   FinCenter - a character string with the the location of   
    #       the financial center named as "continent/city". 
    
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
     
    # Changes:
    #
    
    # FUNCTION:

    # Settings and Checks:
    trace = FALSE
    if (FinCenter == "" || is.null(FinCenter)) FinCenter = "GMT"
    if (is.null(zone)) zone = "GMT"
         
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
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
        Sys.putenv(TZ = myTZ)
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
        Sys.putenv(TZ = myTZ)
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
        Sys.putenv(TZ = myTZ)
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
        Sys.putenv(TZ = myTZ)
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
        Sys.putenv(TZ = myTZ)
        return(ans)
    }    
            
    # Return Value:
    invisible()         
}


# ------------------------------------------------------------------------------


.whichFormat =
function(charvec)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Format:
    format = "unknown"
    
    # Specifications:
    NCHAR = mean(nchar(charvec[1]))
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
    
    # STOP:
    if (format == "unknown") warning("Unknown Format Specification")
     
    # Return Value:
    format
}


# ------------------------------------------------------------------------------


.midnightStandard = 
function(charvec, format)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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


.formatFinCenter = 
function(charvec, FinCenter, type = c("gmt2any", "any2gmt")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internasl function used by function timeDate()
    
    # Changes:
    #
    
    # FUNCTION:
    
    type = match.arg(type)
    signum = 0
    if (type == "gmt2any") 
        signum = +1
    if (type == "any2gmt") 
        signum = -1
    if (FinCenter != "GMT") {
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
        ans = format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S") 
    } else {
        ans = charvec
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


timeCalendar = 
function(y = currentYear, m = 1:12, d = 1, h = NULL, min = NULL, 
s = NULL, zone = myFinCenter, FinCenter = myFinCenter)
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
  
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings and Check:
    trace = FALSE
    if (FinCenter == "") FinCenter = "GMT"
    if (is.null(h) & is.null(min) & is.null(s)) zone = FinCenter
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
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
    
    # Reset TimeZone:  
    Sys.putenv(TZ = myTZ)
    
    # Return Value:
    timeDate(charvec = charvec, format = NULL,  
        zone = zone, FinCenter = FinCenter) 
}


# ------------------------------------------------------------------------------


timeSequence = 
function(from = "2004-01-01", to = format(Sys.time(), "%Y-%m-%d"), 
by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"), 
length.out = NULL, format = NULL, zone = myFinCenter, FinCenter = myFinCenter)
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
        
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
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
            
    # Create timeDate Object:  
    ans = timeDate(charvec = charvec, format = .whichFormat(charvec[1]), 
        zone = zone, FinCenter = FinCenter) 
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Get System Time:
    ans = timeDate(as.character(Sys.time()), zone = "GMT", 
        FinCenter = FinCenter)
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
    
}


# ------------------------------------------------------------------------------


is.timeDate = 
function(object) 
{   # A function implemented by Diethelm Wuertz

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
    Sys.putenv(TZ = "GMT")
    
    # Check Object:
    ans = inherits(object, "timeDate")
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}
    

################################################################################
# S3 METHODS:            REPRESENTATION OF TIMEDATE OBJECTS:
#  print.timeDate         Prints 'timeDate' object
#  plot.timeDate          Plots 'timeDate' object
#  points.timeDate        Adds points to a 'timeDate' plot
#  lines.timeDate         Adds lines to a 'timeDate' plot
#  summary.timeDate       Summarizes details of a 'timeDate' object
#  format.timeDate        Formats 'timeDate' as ISO conform character string


print.timeDate = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints FinCenter and timeDate for a 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object to be printed.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns a printed report on 'timeDate' objects.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Print:
    cat(x@FinCenter, "\n", sep = "")
    layout = paste("[", as.character(x@Data), "]", sep = "")
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    print(layout, quote = FALSE, ...)
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.timeDate = 
function(x, y, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Plot:
    plot(as.POSIXct(x), y, ...)
    
    # return Value:
    invisible()
}


# ------------------------------------------------------------------------------


points.timeDate = 
function(x, y, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Add Points:
    points(as.POSIXct(x), y, ...)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


lines.timeDate = 
function(x, y, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Add Lines:
    lines(as.POSIXct(x), y, ...)
    
    # Return Value:
    invisible()
}
    

# ------------------------------------------------------------------------------


summary.timeDate = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summarizes details of a 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object to be summarized.
    
    # Value:
    #   Returns a summary report of the details of a 'timeDate'
    #   object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
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
    Sys.putenv(TZ = myTZ)
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Format - format.POSIXlt(x, format = "", usetz = FALSE, ...) 
    ans = format.POSIXct(x@Data, ...)
    # print(x@FinCenter)    
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}


################################################################################

