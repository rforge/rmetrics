
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
#  rulesFinCenter         Returns DST rules for a financial center
#  listFinCenter          Lists all supported financial centers
#
# FUNCTION:              GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'             S4 Class representation for timeDate objects
#  timeDate               Creates a 'timeDate' object from given dates
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#  Sys.timeDate           Returns system time as an object of class 'timeDate'  
#
# FUNCTION:              SPECIAL TIMEDATE OPERATIONS:
#  timeLastDayInMonth     Computes the last day in a given month and year
#  timeFirstDayInMonth    Computes the first day in a given month and year
#  timeLastDayInQuarter   Computes the last day in a given quarter and year
#  timeFirstDayInQuarter  Computes the first day in a given quarter and year
#  timeNdayOnOrAfter      Computes date in month that is a n-day ON OR AFTER  
#  timeNdayOnOrBefore     Computes date in month that is a n-day ON OR BEFORE  
#  timeNthNdayInMonth     Computes n-th ocurrance of a n-day in year/month
#  timeLastNdayInMonth    Computes the last n-day in year/month
#
# S3 METHODS:            REPRESENTATION OF TIMEDATE OBJECTS:
#  print.timeDate         Prints 'timeDate' Object
#  summary.timeDate       Summarizes details of a 'timeDate' object
#  format.timeDate        Formats 'timeDate' as ISO conform character string
#
# S3 METHODS:            TEST AND REPRESENTATION OF OBJECTS:
#  is.timeDate            Tests if the object is of class 'timeDate'
#  isWeekday              Tests if a date is a weekday or not
#  isWeekend              Tests if a date falls on a weekend or not
#  isBizday               Tests if a date is a business day or not
#  isHoliday              Tests if a date is a non-business day or not
#  getDayOfWeek           Returns the day of the week to a 'timeDate' object
#  getDayOfYear           Returns the day of the year to a 'timeDate' object
#
# S3 MEHOD:              SUBSETTING TIMEDATE OBJECTS:
#  [.timeDate             Extracts or replaces subsets from 'timeDate' Objects
#  cut.timeDate
#  start.timeDate         Extracts the first object of a 'timeDate' vector
#  end.timeDate           Extracts the last object of a 'timeDate' vector
#  blockStart             Creates start dates for equally sized blocks
#  blockEnd               Creates end dates for equally sized blocks
#
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  round.tinmeDate        Rounds objects of class 'timeDate'
#  trunc.timeDate         Truncates objects of class 'timeDate' 
#
# S3 MEHOD:              CONCATENATION, ORDERING AND SORTING:
#  c.timeDate             Concatenates objects of class 'timeDate'
#  rep.timeDate           Replicates objects of class 'timeDate'
#  sort.timeDate          Sorts, Rounds or truncates a 'timeDate' vector
#  sample.timeDate        Resamples objects of class 'timeDate'
#  unique.timeDate        Remove duplicates from a 'timeDate' vector
#  rev.timeDate           Reverts  a 'timeDate' vector object
#
# S3 MEHOD:              OBJECT TRANSFORMATIONS:
#  as.character.timeDate  Returns a 'timeDate' object as character string
#  as.data.frame.timeDate Returns a 'timeDate' object as data frame
#  as.POSIXct.timeDate    Returns a 'timeDate' object as POSIXct object
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extract months atom from a 'timeDate' object
################################################################################


################################################################################
# FUNCTION:              FINANCIAL CENTERS:
#  rulesFinCenter         Returns DST rules for a financial center
#  listFinCenter          Lists all supported financial centers 

  
rulesFinCenter =
function(FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Show the day light saving rules for a financial center
    
    # Arguments:
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
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
#  .whichFormat
#  .midnightStandard
#  .formatFinCenter
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#  Sys.timeDate           Returns system time as an object of class 'timeDate'


require(methods)


setClass("timeDate", 
    # A class implemented by Diethelm Wuertz
    
    # Description:
    #   Class representatation for 'timeDate' Objects.
    
    # CLASS:
    
    representation(
        Data = "POSIXlt",
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
     
    # FUNCTION:

    # Settings and Checks:
    trace = FALSE
    if (FinCenter == "") FinCenter = "GMT"
         
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
  
    # If charvec is not a chracter string, then try:
    if (inherits(charvec, "timeDate")) {
        charvec = charvec@Data   
    }
    if (inherits(charvec, "POSIXt")) {
        charvec = format(charvec, "%Y-%m-%d %H:%M:%S") 
    }
    if (inherits(charvec, "Date")) {
        charvec = as.character(charvec) 
    }
    stopifnot(is.character(charvec))
    
    # Get Dimension:
    Dim = length(charvec)
        
    # ISO Format - Automatic Format Detection:
    CHARVEC = is.character(charvec)
    if (CHARVEC & is.null(format)) format = .whichFormat(charvec) 
    
    if (format == "%Y-%m-%d" | format == "%Y%m%d") {
        zone = FinCenter
        lt = strptime(charvec, format)
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = format,
            FinCenter = FinCenter)) 
    }
    
    if (format == "%m/%d/%Y") {
        zone = FinCenter
        lt = strptime(charvec, format)
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = format,
            FinCenter = FinCenter)) 
    }
    
    # Midnight Standard:
    charvec = .midnightStandard(charvec, format)
    
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
    
    # ISO Format:
    iso.format = "%Y-%m-%d %H:%M:%S"
    
    # GMT -> GMT:
    if (recFinCenter == "GMT" && useFinCenter == "GMT") {       
        if (DEBUG) print("if - 1:")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec)
            cat("\n") 
        }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) 
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
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) 
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
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) 
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
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt,
            Dim = as.integer(Dim),
            format = iso.format ,
            FinCenter = useFinCenter)) 
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
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        # Return Value:
        Sys.putenv(TZ = myTZ)
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) 
    }    
            
    # Return Value:
    Sys.putenv(TZ = myTZ)
    invisible()         
}


# ------------------------------------------------------------------------------


.whichFormat =
function(charvec)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Specifications:
    NCHAR = mean(nchar(charvec[1]))
    SUBSTR = (substring(charvec[1], 5, 5) == "-")
    
    # Human readable ISO:
    if (NCHAR ==  4 & !SUBSTR) format = "%Y" 
    if (NCHAR ==  7 &  SUBSTR) format = "%Y-%m"
    if (NCHAR == 10 &  SUBSTR) format = "%Y-%m-%d" 
    if (NCHAR == 13 &  SUBSTR) format = "%Y-%m-%d %H"
    if (NCHAR == 16 &  SUBSTR) format = "%Y-%m-%d %H:%M" 
    if (NCHAR == 19 &  SUBSTR) format = "%Y-%m-%d %H:%M:%S"
    
    # Short ISO:
    if (NCHAR ==  6 & !SUBSTR) format = "%Y%m"
    if (NCHAR ==  8 & !SUBSTR) format = "%Y%m%d"
    if (NCHAR == 10 & !SUBSTR) format = "%Y%m%d%H"
    if (NCHAR == 12 & !SUBSTR) format = "%Y%m%d%H%M"
    if (NCHAR == 14 & !SUBSTR) format = "%Y%m%d%H%M%S"
    
    # American Format:
    if (regexpr("/", charvec[1])[[1]] > 0) format = "%m/%d/%Y" 
    
    # Return Value:
    format
}


# ------------------------------------------------------------------------------


.midnightStandard = 
function(charvec, format)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Format:
    nchar.iso = mean(nchar(charvec))
    iso.format = "%Y-%m-%d %H:%M:%S"
    
    # ISO-8601 Midnight Standard:
    s = rep(0, length(charvec))
    if (nchar.iso == 19) {
        s[grep("24:00:00", charvec)] = 1
        charvec = gsub("24:00:00", "23:59:59", charvec) 
        # Convert "charvec" to standard ISO format:
        charvec = format(strptime(charvec, format)+s, iso.format)
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
        charvec = format(strptime(charvec, format)+s, iso.format)
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
        zone = FinCenter
    } else {
        chartime = substr(as.character(xhhmmss), 2, 7)
        charvec = paste(as.vector(chardate), as.vector(chartime), sep = "")
        format = "%Y%m%d%H%M%S"
        zone = zone
    }
    
    # Reset TimeZone:  
    Sys.putenv(TZ = myTZ)
    
    # return Value:
    timeDate(charvec = charvec, format = format,  zone = zone, 
        FinCenter = FinCenter) 
}


# ------------------------------------------------------------------------------


timeSequence = 
function(from = "2004-01-01", to = format(Sys.time(), "%Y-%m-%d"), 
by = c("day", "year", "quarters", "month", "week", "hour", "min", "sec"), 
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
    #   x = timeSequence("2004-01-31", "2005-01-31", by = "month")
    #   x = timeSequence("2004-01-28", by = "day", length.out = 10)
    #   x = timeSequence("2004-01-31", by = "month", length.out = 12))   
    #   x = timeSequence("2004-01-28 18:00:00", "2004-01-29 06:00:00", 
    #       format = "%Y-%m-%d %H:%M:%S", by = "hour")
    #   x = timeSequence("2004-01-28 18:00:00", 
    #       format = "%Y-%m-%d %H:%M:%S", by = "hour", length.out = 10)
        
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Settings and Checks:
    if (!is.null(length.out)) to = from 
    if (FinCenter == "") FinCenter = "GMT"
    by = match.arg(by)
    if (by == "quarters") by = "3 months"
    
    # Auto-detect Input Format:
    format.from = format.to = format
    if (is.character(from) & is.null(format)) {
        nchar.iso = mean(nchar(from))
        if (nchar.iso == 10) format.from = "%Y-%m-%d" 
        if (nchar.iso == 19) format.from = "%Y-%m-%d %H:%M:%S"
        if (nchar.iso ==  8) format.from = "%Y%m%d"
        if (nchar.iso == 14) format.from = "%Y%m%d%H%M%S"
        if (regexpr("/", from[1])[[1]] > 0) format.from = "%m/%d/%Y" }
    if (is.character(to) & is.null(format)) {
        nchar.iso = mean(nchar(to))
        if (nchar.iso == 10) format.to = "%Y-%m-%d" 
        if (nchar.iso == 19) format.to = "%Y-%m-%d %H:%M:%S"
        if (nchar.iso ==  8) format.to = "%Y%m%d"
        if (nchar.iso == 14) format.to = "%Y%m%d%H%M%S"
        if (regexpr("/", to[1])[[1]] > 0) format.to = "%m/%d/%Y" }
    format = format.from
    if (format != format.to)
        stop ("Args from and to must have the same format specification.")
    
    # Create Charvector:  
    from = strptime(as.character(from), format = format) 
    iso.format = "%Y-%m-%d %H:%M:%S"
    if (is.null(length.out)) {
        # The start "from" and end date "to" must be specified!
        to = strptime(as.character(to), format = format)
        charvec = format(seq.POSIXt(from = from, 
            to = to, by = by), iso.format) 
    } else  {
        # The end date is missing and has to be specified
        charvec = format(seq.POSIXt(from = from, 
            by = by, length.out = length.out), iso.format) 
    }
            
    # Create timeDate Object:  
    ans = timeDate(charvec = charvec, format = NULL, 
        zone = zone, FinCenter = FinCenter) 
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
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

 
################################################################################
# FUNCTION:              SPECIAL TIMEDATE OPERATIONS:
#  timeLastDayInMonth     Computes the last day in a given month and year
#  timeFirstDayInMonth    Computes the first day in a given month and year
#  timeLastDayInQuarter   Computes the last day in a given quarter and year
#  timeFirstDayInQuarter  Computes the first day in a given quarter and year
#  timeNdayOnOrAfter      Computes date in month that is a n-day ON OR AFTER  
#  timeNdayOnOrBefore     Computes date in month that is a n-day ON OR BEFORE  
#  timeNthNdayInMonth     Computes n-th ocurrance of a n-day in year/month
#  timeLastNdayInMonth    Computes the last n-day in year/month


timeLastDayInMonth = 
function(charvec, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last day in a given month and year
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   format - the format specification of the input character vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the last day in a given month and year as a
    #   'timeDate' object.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Last day of month:
    last.day = c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
    lt = strptime(charvec, format)
    y = 1900 + lt$year
    leap.year = (y%%4 == 0 & (y%%100 != 0 | y%%400 == 0))
    leap.day = as.integer(leap.year)*as.integer(lt$mon == 1)
    lt$mday = last.day[1 + lt$mon] + leap.day
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(lt, format = "%Y-%m-%d", zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------

    
timeFirstDayInMonth = 
function(charvec, format = "%Y-%m-%d", FinCenter = "GMT") 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last day in a given month and year
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # First Day In Month:
    lt = strptime(charvec, format)
    lt$mday = 1

    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(lt, format = "%Y-%m-%d", zone = FinCenter, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeLastDayInQuarter = 
function(charvec, format = "%Y-%m-%d", FinCenter = "GMT") 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last day in a given month and year
    
    # FUNCTION:
    
    # First Day in Month:
    charvec = timeFirstDayInMonth(charvec = charvec, format = format, 
        FinCenter = FinCenter)
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Last Day in Quarter:
    lt = strptime(charvec, format)
    last.quarter = rep(c(3,6,9,12), each = 3) - 1
    lt$mon = last.quarter[1 + lt$mon] 
    Sys.putenv(TZ = myTZ)
    charvec = timeDate(lt, format = "%Y-%m-%d", zone = FinCenter, 
        FinCenter = FinCenter)
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeLastDayInMonth(charvec = charvec, format = format, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------
    
    
timeFirstDayInQuarter = 
function(charvec, format = "%Y-%m-%d", FinCenter = "GMT") 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last day in a given month and year
    
    # FUNCTION:
    
    # First Day in Month:
    charvec = timeFirstDayInMonth(charvec =charvec, format = format, 
        FinCenter = FinCenter)
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # First Day in Quarter:
    lt = strptime(charvec, format)
    first.quarter = rep(c(1,4,7,10), each = 3) - 1
    lt$mon = first.quarter[1 + lt$mon] 

    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(lt, format = "%Y-%m-%d", zone = FinCenter, FinCenter = FinCenter)
}

    
# ------------------------------------------------------------------------------


timeNdayOnOrAfter = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes date in month that is a n-day ON OR AFTER 
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the date in month that is a n-day ON OR AFTER as
    #   a 'timeDate' object.
    
    # Details:
    #   nday = 1 is a Monday
    
    # Example: 
    #   What date has the first Monday on or after March 15, 1986?
    #   timeNdayOnOrAfter("1986-03-15", 1)
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) + (nday-lt$wday)%%7)
    class(ct) = "POSIXct"
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeNdayOnOrBefore = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes date in month that is a n-day ON OR BEFORE 

    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the date in month that is a n-day ON OR BEFORE
    #   as a 'timeDate' object.
    
    # Example: 
    #   What date has Friday on or before April 22, 1977?
    
    # FUNCTION: 
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) - (-(nday-lt$wday))%%7)
    class(ct) = "POSIXct"
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeNthNdayInMonth = 
function(charvec, nday = 1, nth = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes "nth" ocurrance of a "nday" (nth = 1,...,5) 
    #   in "year,month"
 
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   nth - an integer vector numbering the n-th occurence.
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the "nth" ocurrance of a "nday" (nth = 1,...,5) 
    #   in "year,month" as a 'timeDate' object.
    
    # Example: 
    #   What date is the second Monday in April 2004?
    #   timeNthNdayInMonth("2004-04-01", 1, 2)
    
    # FUNCTION: 
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    lt1 = lt
    lt1$mday = 1
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) + (nth-1)*7 + 
        (nday-lt1$wday)%%7)
    class(ct) = "POSIXct"

    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeLastNdayInMonth = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last "nday" in "year/month"
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the last "nday" in "year/month" as a 'timeDate' 
    #   object.
    
    # Example: 
    #   What date has the last Tuesday in May, 1996?
    #   timeLastNdayInMonth("1996-05-01", 2)
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Last Day:
    last.day = c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
    lt = strptime(charvec, format)
    y = 1900 + lt$year
    leap.year = (y%%4 == 0 & (y%%100 != 0 | y%%400 == 0))
    leap.day = as.integer(leap.year)*as.integer(lt$mon == 1)
    lt$mday = last.day[1 + lt$mon] + leap.day
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) - (-(nday-lt$wday))%%7)
    class(ct) = "POSIXct"

    # Return Value:
    Sys.putenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = FinCenter,
        FinCenter = FinCenter)
}


################################################################################
# S3 METHODS:            REPRESENTATION OF TIMEDATE OBJECTS:
#  print.timeDate         Prints 'timeDate' Object
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
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Format - format.POSIXlt(x, format = "", usetz = FALSE, ...) 
    ans = format.POSIXlt(x@Data, ...)
    # print(x@FinCenter)    
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}


################################################################################
# S3 METHODS:            TEST AND REPRESENTATION OF OBJECTS:
#  is.timeDate            Tests if the object is of class 'timeDate'
#  isWeekday              Tests if a date is a weekday or not
#  isWeekend              Tests if a date falls on a weekend or not
#  isBizday               Tests if a date is a business day or not
#  isHoliday              Tests if a date is a non-business day or not
#  getDayOfWeek           Returns the day of the week to a 'timeDate' object
#  getDayOfYear           Returns the day of the year to a 'timeDate' object


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
    

# ------------------------------------------------------------------------------


isWeekday = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Test if a date is a weekday day or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    
    # Value:
    #   returns a logical or a vector of logicals
    
    # Example:
    #   isWeekday(timeDate("2004-07-01"))
    #   isWeekday(Sys.timeDate())
    
    # FUNCTION:
    
    # Test for Weekdays:
    wday = (x@Data)$wday
    ans = (!(wday == 0 | wday == 6)) 
    names(ans) = x@Data
    
    ans
}


# ------------------------------------------------------------------------------

    
isWeekend = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a date is a weekend day or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    
    # Value:
    #   Returns a logical or a vector of logicals
    
    # Example:
    #   isWeekend(timeDate("2004-07-01"))
    #   isWeekend(Sys.timeDate())
    
    # FUNCTION:
    
    # Return Value:
    return(!isWeekday(x)) 
}   


# ------------------------------------------------------------------------------

    
isBizday = 
function(x, holidays = holiday.NYSE()) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Test if a date is a business day or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    #   holidays - a holiday calendar
    
    # Value:
    #   Returns a logical or a vector of logicals
    
    # Example:
    #   x = timeSequence(from = "2005-05-15", to = "2005-07-15")
    #   h = holiday.NYSE(2005)
    #   cbind(as.character(x), is.bizday(x, h))
    
    # FUNCTION:
    
    # Test:
    char.x = substr(as.character(x), 1, 10)
    char.h = substr(as.character(holidays), 1, 10)
    Weekday = as.integer(isWeekday(x))
    nonHoliday = as.integer(!(char.x %in% char.h))
    
    # Business Days:
    bizdays = as.logical(Weekday*nonHoliday)
    names(bizdays) = x@Data
    
    # Return Value:
    bizdays
} 


# ------------------------------------------------------------------------------


isHoliday = 
function(x, holidays = holiday.NYSE()) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Test if a date is a holiday or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    #   holidays - a holiday calendar
    
    # Value:
    #   Returns a logical or a vector of logicals

    # FUNCTION:
    
    # Return Value:
    return(!isBizday(x, holidays)) 
}   


# ------------------------------------------------------------------------------


getDayOfWeek =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns day of week for time date objects
    
    # Example:
    #   weekDay(Sys.timeDate())
    #   weekDay(timeSequence("2005-05-15", "2005-07-15"))
    
    wd = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    n = as.POSIXlt(x@Data)$wday + 1
    wdays = wd[n]
    names(wdays) = as.character(x@Data)
    
    # Return Value:
    wdays
}    


# ------------------------------------------------------------------------------


getDayOfYear =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns day of week for time date objects
    
    # FUNCTION:
    
    # Assign:
    yd = 1:366
    n = as.POSIXlt(x@Data)$yday + 1
    ydays = yd[n]
    names(ydays) = as.character(x@Data)
    
    # Return Value:
    ydays
}   


################################################################################
# S3 MEHOD:              SUBSETTING TIMEDATE OBJECTS:
#  [.timeDate             Extracts or replaces subsets from 'timeDate' Objects
#  cut.timeDate
#  start.timeDate         Extracts the first object of a 'timeDate' vector
#  end.timeDate           Extracts the last object of a 'timeDate' vector
#  blockStart             Creates start dates for equally sized blocks
#  blockEnd               Creates end dates for equally sized blocks


"[.timeDate" =
function(x, ..., drop = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts or replaces subsets from 'timeDate' objects
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns a subset from a 'timeDate' object.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Subsets:
    val <- lapply(x@Data, "[", ..., drop = drop)
    attributes(val) <- attributes(x@Data) 
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    new("timeDate", 
        Data = val, 
        Dim = length(as.character(val)),
        format = x@format,
        FinCenter = x@FinCenter)      
}   


# ------------------------------------------------------------------------------


cut.timeDate = 
function(x, from , to, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    X = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    FROM = timeDate(from, zone = x@FinCenter, FinCenter = "GMT")
    TO = timeDate(to, zone = x@FinCenter, FinCenter = "GMT")
    test = (X >= FROM & X <= TO)
    ans = timeDate(X[test], zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


start.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns from 'x' the earliest entry as an object of class 
    #   'timeDate'.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # First element:
    # print(x@FinCenter)
    xGMT = timeDate(x, zone=x@FinCenter, FinCenter="GMT")@Data
    z = as.numeric(as.POSIXct(xGMT))
    order(z)[1]
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    x[1]
}


# ------------------------------------------------------------------------------


end.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns an object of class 'timeDate'.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Last element:
    # print(x@FinCenter)
    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")@Data
    z = as.numeric(as.POSIXct(xGMT))
    n = order(z)[length(z)]
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    x[n]
}


# ------------------------------------------------------------------------------


blockStart =
function(x, block = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes start dates for numeric blocks of dates
    
    # Example:
    #   blockEnd(timeSequence(), block = 30)
    
    # Start Dates of Blocks:
    nx = length(as.character(x))
    fromIdx = seq(1, nx, by = block)
    from = x[fromIdx]
    
    # Return Value:
    from
}

# ------------------------------------------------------------------------------


blockEnd =
function(x, block = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes start dates for numeric blocks of dates
    
    # Example:
    #   blockEnd(timeSequence(), block = 30)
    
    # End Dates of Blocks:
    nx = length(as.character(x))
    fromIdx = seq(1, nx, by = block)
    toIdx = c(fromIdx[-1]-1, nx)
    to = x[toIdx]
    
    # Return Value:
    to
}


################################################################################
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  round.tinmeDate        Rounds objects of class 'timeDate'
#  trunc.timeDate         Truncates objects of class 'timeDate'


Ops.timeDate = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Uses group 'Ops' generic functions for 'timeDate' objects

    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' 
    
    # Value:
    #   Returns the 'Ops' grouped object.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Logical Operators:
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for timeDate objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
        "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) 
        stop(paste(.Generic, "not defined for timeDate XXX objects"))   
        
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    e2GMT = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
    
    # Convert to Julian:
    if (inherits(e1GMT, "POSIXlt")) e1 <- as.POSIXct(e1GMT)
    if (inherits(e2GMT, "POSIXlt")) e2 <- as.POSIXct(e2GMT)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    NextMethod(.Generic)
}


# ------------------------------------------------------------------------------


"+.timeDate" =
function(e1, e2)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs arithmetic "+" operation on 'timeDate' objects.
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'numeric'
    
    # Value:
    #   Returns a 'timeDate' object "e2" seconds later than the
    #   'timeDate' object "e1". 
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    
    # Add and Convert back to FinCenter:
    ans = timeDate(e1GMT+e2, zone = "GMT", FinCenter = e1@FinCenter)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}


# ------------------------------------------------------------------------------


"-.timeDate" = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs arithmetic "-" operation on 'timeDate' objects
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' or of class 'numeric'
    
    # Value:
    #   Returns a 'difftime' object if both "e1" and "e2" are
    #   'timeDate' objects, or returns a 'timeDate' object "e2"
    #   seconds earlier than "e1".
    
    # Example:
    #   charvec = c("2004-01-01 16:00:00", "2004-01-01 18:00:00")
    #   x = timeDate(charvec, zone = "GMT", FinCenter = "Europe/Zurich")
    
    # FUNCTION:
     
    ## Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "timeDate")) test2 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # First Object:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    if (inherits(e2, "timeDate")) {
        e2 = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
        # Returns difftime:
        return(e1GMT-e2) }
    if (inherits(e2, "numeric")) {
        # Returns timeDate:
        return(timeDate(e1GMT-e2, zone = "GMT", FinCenter = e1@FinCenter)) }
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
    invisible()         
}


# ------------------------------------------------------------------------------


diff.timeDate =
function (x, lag = 1, differences = 1, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns suitably lagged and iterated differences
    
    # Arguments:
    #   x - a 'timeDate' object.
    #   lag - an integer indicating which lag to use, by 
    #       default 1.
    #   differences - an integer indicating the order of the 
    #       difference, by default 1.
    #   ... - further arguments to be passed to or from methods.
  
    # Value:
    #   If 'x' is a vector of length 'n' and 'differences=1', then 
    #   the computed result is equal to the successive differences
    #   'x[(1+lag):n] - x[1:(n-lag)]'.
    #   If 'difference' is larger than one this algorithm is applied
    #   recursively to 'x'. Note that the returned value is a vector 
    #   which is shorter than 'x'.

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Convert to GMT:
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
    ans = diff.POSIXt(as.POSIXct(GMT@Data), 
        lag = lag, differences = differences, ...) 
        
    # Reset Timezone:
    Sys.putenv(TZ = myTZ)

    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


difftimeDate = 
function(time1, time2, 
units = c("auto", "secs", "mins", "hours", "days", "weeks")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Takes a difference of two 'timeDate' objects
    
    # Arguments:
    #   time1, time2 - 'timeDate' objects.
    #   units - a character string. The units in which the results 
    #       are desired, one of the following: "auto", "secs", 
    #       "mins", "hours", "days", "weeks"
    
    # Value:
    #   'difftimeDate' takes a difference of two 'timeDate' 
    #   objects and returns an object of class 'difftime' with
    #   an attribute indicating the units. 

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Convert to GMT:
    time1GMT = timeDate(time1, zone = time1@FinCenter, 
        FinCenter = "GMT") 
    time2GMT = timeDate(time2, zone = time2@FinCenter, 
        FinCenter = "GMT") 

    # Return Value:
    Sys.putenv(TZ = myTZ)
    difftime(time1GMT@Data, time2GMT@Data, tz = "GMT", units = units[1]) 
}


# ------------------------------------------------------------------------------


round.timeDate =
function(x, units = c("days", "hours", "mins"), ...)     
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Get Units:
    units = match.arg(units)
    
    # Sorting under GMT is not what we want!
    # GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    # lt = round.POSIXt(GMT@Data, units = units, ...)
    # ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)
    
    # Use:
    lt = round.POSIXt(x@Data, units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


trunc.timeDate =
function(x, units = c("days", "hours", "mins"), ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Get Units:
    units = match.arg(units)
    
    # Sorting under GMT is not what we want!
    # GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    # lt = trunc.POSIXt(GMT@Data, units = units)
    # ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)
    
    # Use:
    lt = trunc.POSIXt(x@Data, units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


################################################################################
# S3 MEHOD:              CONCATENATION, ORDERING AND SORTING:
#  c.timeDate             Concatenates objects of class 'timeDate'
#  rep.timeDate           Replicates objects of class 'timeDate'
#  sample.timeDate        Resamples objects of class 'timeDate'
#  sort.timeDate          Sorts, Rounds or truncates a 'timeDate' vector
#  unique.timeDate        Remove duplicates from a 'timeDate' vector
#  rev.timeDate           Reverts  a 'timeDate' vector object


c.timeDate = 
function(..., recursive = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Concatenates objects of class 'timeDate'
    
    # Arguments:
    #   ... - objects to be concatenated.
    #   recursive - a logical. If 'recursive=TRUE', the function 
    #       recursively descends through lists combining all their 
    #       elements into a vector.
    
    # Value:
    #   Returns all arguments to be coerced to a 'timeDate' object  
    #   which is the type of the returned value.

    # Details:
    #   This is a generic function which combines its arguments.

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
        
    # List all:
    z = list(...)
    
    # Convert to GMT character vectors:
    all = NULL
    for (i in 1:length(z)) {
        new = format(timeDate(z[[i]], zone = z[[i]]@FinCenter, 
            FinCenter = "GMT")@Data, "%Y-%m-%d %H:%M:%S")
        all = c(all, new) 
    }
    
    # Convert to Financial Center of the first element:
    ans = timeDate(all, zone = "GMT", FinCenter = z[[1]]@FinCenter)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}
  
    
# ------------------------------------------------------------------------------


rep.timeDate =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Replicates objects of class 'timeDate'
    
    # Arguments:
    #   x - a 'timeDate' object
    #   times - a non-negative integer.  A vector giving the number 
    #       of times to repeat each element if of length 'length(x)', 
    #       or to repeat the whole vector if of length 1.
    
    # Value:
    #   Returns a vector of repeated elements belonging to the same 
    #   class as 'x'.
    
    # FUNCTION:

    # Replicate: 
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = rep(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sample.timeDate =
function(x, ...)
{
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = sample(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sort.timeDate =
function(x, ...)
{
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = sort(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


unique.timeDate =
function(x, ...) 
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Returns a timeDate object with duplicate entries removed
    
    # Arguments:
    #   x - an object of class timeDate
    #   incomparables - not used
    
    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = unique(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rev.timeDate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Reverts  a 'timeDate' vector object.
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object in reversed order.
    
    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = rev(as.character(GMT@Data))
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    ans
    
    # Return Value:
    x
}

    
################################################################################
# S3 MEHOD:              OBJECT TRANSFORMATIONS:
#  as.character.timeDate  Returns a 'timeDate' object as character string
#  as.data.frame.timeDate Returns a 'timeDate' object as data frame
#  as.POSIXct.timeDate    Returns a 'timeDate' object as POSIXct object
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extract months atom from a 'timeDate' object


as.character.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as character string
    
    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns 'x' as a character vector. 

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Format:
    ans = format.POSIXlt(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.data.frame.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as data frame
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a data frame.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    stopifnot(inherits(x, "timeDate"))
    
    # Data Frame:
    ans = as.data.frame.POSIXlt(x@Data, ...)
    colnames(ans) = paste(x@FinCenter, ":", substitute(x), sep = "")
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.POSIXct.timeDate =
function(x, tz = "")
{# A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as POSIXct object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)
    
    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # POSIXlt:
    ans = as.POSIXct.POSIXlt(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------

 
julian.timeDate = 
function(x, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts Julian time in days since 1970-01-01
    
    # Arguments:
    #   x - a 'timeDate' object
    #   units - a character string, one of the units listed, 
    #       by default "secs".
    
    # Value:
    #   Returns the number of days (possibly fractional) since
    #   the origin.
    
    # Details:
    #   The origin is "1970-01-01 00:00:00 GMT"

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Fixed Units:
    if (!exists("myUnits")) units = "secs" else units = myUnits
    
    # POSIX:
    lt = timeDate(x, zone = x@FinCenter, FinCenter = FinCenter)@Data

    # Difftime:  
    origin = as.POSIXlt("1970-01-02", tz = "GMT") - 24 * 3600
    res = difftime(as.POSIXct(lt), origin, units = units[1])
    ans = structure(res, origin = origin)
        
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

# ------------------------------------------------------------------------------


atoms.timeDate = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts atoms from a 'timeDate' object.
    
    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       calendar "atoms".
    
    # Value:
    #   Returns a data.frame with the following calendar atoms:
    #   Y(ear), m(onth), d(ay), H(our), M(inutes), S(econds).
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # mdy:
    X = x@Data
    Y = X$year + 1900
    m = X$mon + 1
    d = X$mday
    H = X$hour
    M = X$min
    S = X$sec
    
    # Data Frame:
    ans = data.frame(Y = Y, m = m, d = d, H = H, M = M, S = S)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


months.timeDate =
function(x, abbreviate = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts months atom from a timeDate object

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       month "atom".
    
    # Value:
    #   Returns the month from a 'timeDate' object as an integer
    #   value or vector with elements ranging between 1 and 12,
    #   numbering the months from January to December.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Month:
    ans = x@Data$mon+1
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

################################################################################

