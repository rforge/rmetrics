#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.2
#   POSIX Based Implementation in R
#
# List of Examples:                
#   
#   Example: 1 Introduction
#   Example: 2 Generation of Objects
#   Example: 3 Representation of Objects
#   Example: 4 Mathematical Operations
#   Example: 5 Object Transformations
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Learn to Manage POSIXt Objects from R's "base" Package

    # Description:
    #   There are two basic classes of date/times. Class "POSIXct" 
    #   represents the (signed) number of seconds since the beginning 
    #   of 1970 as a numeric vector. Class "POSIXlt" is a named list 
    #   of vectors representing date/time atoms. The classes correspond 
    #   to the ANSI C constructs of “calendar time” (the time_t data 
    #   type) and “local time” (or broken-down time, the struct tm data 
    #   type), from which they also inherit their names. "POSIXct" is 
    #   more convenient for including in data frames, and "POSIXlt" is 
    #   closer to human-readable forms. A virtual class "POSIXt" inherits 
    #   from both of the classes: it is used to allow operations which
    #   mix the two classes. In detail we will cover the following topics:
    #
    #       1 Introduction
    #       2 Generation of Objects
    #       3 Representation of Objects
    #       4 Mathematical Operations
    #       5 Object Transformations
    #
    # Author:
    #   These examples were written by Diethelm Wuertz
    #
    # References:
    #   R's base package.
    ###
    

# ------------------------------------------------------------------------------


### Load Packages:

    # require(fCalendar)
    ###
    

# ------------------------------------------------------------------------------


### Example: 1 Introduction
  
    # Note, POSIX is part of the base package!
    Sys.time()
    # In the following we use only "GMT" time zone settings. 
    Sys.putenv(TZ = "GMT")
    Sys.time()
    # For other time zone settings use the new 'timeDate'
    # objects which also work properly under the Windows OS.
    ###
    

# ------------------------------------------------------------------------------

  
### Example: 2 Generation of Objects
  
    # Create Character Vectors and Format Specification:
    dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
    tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23") 
    ISO = "%Y-%m-%d %H:%M:%S"
    ###  
    
    # "strptime" - Create 'list' Based 'POSIXlt' date Objects:
    lt = strptime(dts, "%Y-%m-%d")
    attr(lt, "tzone") <- "GMT"
    print(lt)
    # [1] "1989-09-28 GMT" "2001-01-15 GMT" ...
    ###
      
    # "strptime" - Create 'list' Based 'POSIXlt' date/time Objects:
    paste(dts, tms); 
    lt = strptime(paste(dts, tms), "%Y-%m-%d %H:%M:%S")
    attr(lt, "tzone") <- "GMT"
    print(lt)
    # [1] "1989-09-28 23:12:55 GMT" "2001-01-15 10:34:02 GMT" ...
    ###
      
    # "POSIXlt" - Display the Internal Representation:
    class(lt)
    # [1] "POSIXt"  "POSIXlt"
    unclass(lt)
    # [1] "POSIXt"  "POSIXlt"
    # $sec
    # [1] 55  2  0 23
    # $min
    # [1] 12 34 30 18
    # $hour
    # [1] 23 10  8 11
    # $mday
    # [1] 28 15 30  9
    # $mon
    # [1] 8 0 7 1
    # $year
    # [1]  89 101 104  90
    # $wday
    # [1] 4 1 1 5
    # $yday
    # [1] 270 14 242  39
    # $isdst
    # [1] 1 1 1 0  
    # attr(,"tzone")
    # [1] "GMT"
    ###
     
    # "as.POSIXct" - Create Integer Based POSIXct date/time Objects:
    ct = as.POSIXct(lt)
    ct
    # [1] "1989-09-29 01:12:55 GMT" "2001-01-15 11:34:02 GMT"
    # [3] "2004-08-30 10:30:00 GMT" "1990-02-09 12:18:23 GMT"
    ###
     
    # "POSIXct" - Display the Internal Representation:
    class(ct)
    # [1] "POSIXt"  "POSIXct"
    unclass(ct)
    # [1]  623027575  979554842 1093854600  634562303
    ###
    
    # "seq" - Create POSIXt Sequences:
    # USE: seq.POSIXt(from, to, by, length.out, along.with, ...)
    # FIRST:
    #   by - A number, taken to be in seconds, use one day:
    seq(min(ct), by = 24*3600, length = 4)
    # [1] "1989-09-28 23:12:55 GMT" "1989-09-29 23:12:55 GMT" ...
    # NEXT: 
    #   by - A object of class 'difftime':
    seq(min(ct), by = ct[2]-ct[1], length = 4)
    # [1] "1989-09-28 23:12:55 GMT" "2001-01-15 10:34:02 GMT" ...
    # NEXT:
    #   by - A character string, containing one of 'sec', 'min',
    #   'hour', 'day', 'DSTday', 'week', 'month' or 'year'.  
    #   This can optionally be preceded by an integer and
    #   a space, or followed by 's'.
    seq(min(ct), by = "months", length = 4)
    # [1] "1989-09-28 23:12:55 GMT" "1989-10-28 23:12:55 GMT" ...
    # NEXT:
    #   Use it for 'quarters':
    posix = seq(min(ct), by = "3 months", length = 4)
    posix
    # [1] "1989-09-28 23:12:55 GMT" "1989-12-28 23:12:55 GMT" ...
    class(posix)
    # [1] "POSIXt"  "POSIXct"
    ###
  
   
# ------------------------------------------------------------------------------

         
### Example: 3 Representation of Objects
 
    # "print" - Prints Objects:
    print(lt)
    # [1] "1989-09-28 23:12:55 GMT" "2001-01-15 10:34:02 GMT" ...
    print(ct)
    # [1] "1989-09-28 23:12:55 GMT" "2001-01-15 10:34:02 GMT" ...
    ###
     
    # "format" - Formats Objects as Character Vectors:
    format(lt)
    # [1] "1989-09-28 23:12:55" "2001-09-15 10:34:02" ...
    format(ct)
    # [1] "1989-09-28 23:12:55" "2001-09-15 10:34:02" ... 

   
# ------------------------------------------------------------------------------


### Example: 4 Mathematical Operations

    # Create POSIXt Objects:
    paste(dts, tms) 
    lt = strptime(paste(dts, tms), "%Y-%m-%d %H:%M:%S")
    attr(lt, "tzone") <- "GMT"
    ct = as.POSIXct(lt)
    ###
        
    # "[" - Extract Subsets from POSIXt Objects:
    lt[2:3]
    # [1] "2001-01-15 10:34:02 GMT" "2004-08-30 08:30:00 GMT" ...
    lt[-(1:2)]
    # [1] "2004-08-30 08:30:00 GMT" "1990-02-09 11:18:23 GMT" ...
    ct[2:3]
    # [1] "2001-01-15 10:34:02 GMT" "2004-08-30 08:30:00 GMT" ...
    ct[-(1:2)]
    # [1] "2004-08-30 08:30:00 GMT" "1990-02-09 11:18:23 GMT" ...
    ###
    
    # "+" - Add a Number to a POSIXt Object: 
    # Add 5h30m, and 5 1/2 days ...
    lt1 = lt + 5.5*3600;    ct1 = ct + 5.5*3600
    lt2 = lt + 5.5*24*3600; ct2 = ct + 5.5*24*3600
    lt1
    # [1] "2000-01-01 21:30:00 GMT" ...
    lt2
    # [1] "2000-01-07 04:00:00 GMT" ...
    ct1
    # [1] "2000-01-01 21:30:00 GMT" ...
    ct2
    # [1] "2000-01-07 04:00:00 GMT" ...
    ###
    
    # "-", "diff", "difftime" - Compute Differences:
    # USE: diff(x, lag=1, differences=1, ...) 
    lt[2] - lt[1]
    # Time difference of 4369.473 days
    diff(as.POSIXct(lt))
    # next
    ct[2] - ct[1]
    # Time difference of 4369.473 days
    diff(ct)
    ###
      
    # "=="  "!="  "<"  "<="  ">"  ">=" - Perform Logical Operations: 
    lt[lt > strptime("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")]
    lt
    # [1] "2001-09-15 10:34:02 GMT" "2004-08-30 08:30:00 GMT"
    # More Logical Operations ...
    lt[1] < lt[2]
    # [1]  TRUE FALSE
    ct[1:2] > ct[3:4]
    # [1] FALSE  TRUE
    ct[1:2] == lt
    # [1] TRUE TRUE 
    ###
     
    # "c", "rep" - Concatenate and Replicate POSIXt Objects:
    c(lt[1], ct[2:3])
    # [1] "1989-09-28 23:12:55 GMT" "2001-09-15 10:34:02 GMT"
    rep(c(lt[1], ct[2]), times = 2)
    # [1] "1989-09-28 23:12:55 GMT" "2001-09-15 10:34:02 GMT"
    ###
     
    # "trunc", "round" - Truncate and Round POSIXt Objects:
    # Truncate:  
    data.frame(lt, truncated=trunc(lt, "days"))
    data.frame(lt, truncated=trunc(lt, "mins"))
    data.frame(ct, truncated=trunc(ct, "hours"))
    # Round:  
    data.frame(lt, rounded=round(lt, "days"))
    data.frame(lt, rounded=round(lt, "mins"))
    data.frame(ct, rounded=round(ct, "hours"))
    ###
     
    # "min", "max" - Compute Minimum and/or Maximum of POSIXt Objects:
    range = c(min(lt), max(lt))
    range
    # [1] "1989-09-28 23:12:55 GMT" "2004-08-30 08:30:00 GMT"
    ###
     
    # "sort" - Sort and Revert POSIXt Objects:
    # POSIXlt objects cannot be sorted or reverted
    sort(as.POSIXct(lt))
    # [1] "1989-09-28 23:12:55 GMT" "1990-02-09 11:18:23 GMT" ...
    sort(ct[4:1])
    # [1] "1989-09-28 23:12:55 GMT" "1990-02-09 11:18:23 GMT" ...
    reverted = ct[4:1]; reverted
    # [1] "1990-02-09 11:18:23 GMT" "2004-08-30 08:30:00 GMT" ...
    # Note but also "rev()" works ...
    ###
   
   
# ------------------------------------------------------------------------------


### Example: 5 Object Transformations
   
    # Create 'POSIXt' Objects:
    lt = strptime(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S")
    attr(lt, "tzone") <- "GMT"
    ct = as.POSIXct(lt)
    ### 
     
    # Transform 'POSIXct' to 'integer'/'numeric' Objects:
    as.integer(ct)
    # [1]  623027575 1000550042 1093854600  634562303
    as.numeric(ct)
    # [1]  623027575 1000550042 1093854600  634562303
    ###
     
    # Transform 'POSIXt' to 'character' Objects:
    as.character(lt)
    # [1] "1989-09-28 23:12:55" "2001-09-15 10:34:02" ...
    as.character(ct)
    # [1] "1989-09-28 23:12:55" "2001-09-15 10:34:02" ...
    ###
     
    # Transform 'POSIXt' to 'data.frame' Objects:
    as.data.frame(lt)
    #                    lt
    # 1 1989-09-28 23:12:55
    # 2 2001-09-15 10:34:02 ...
    as.data.frame(ct)
    #                    ct
    # 1 1989-09-28 23:12:55
    # 2 2001-09-15 10:34:02 ...
    ###
      
    # Transform between 'POSIXlt' and 'POSIXct' Objects:
    ct = as.POSIXct(lt)
    ct
    # [1] "1989-09-28 23:12:55 GMT" "2001-09-15 10:34:02 GMT" ...
    lt = as.POSIXlt(ct)
    lt
    # [1] "1989-09-28 23:12:55 GMT" "2001-09-15 10:34:02 GMT" ...
    ###
    
    # julian.POSIXt - The Julian origin is buggy, we use the patch ...
    jul = julian(lt)
    jul
    # Time differences of  7210.967, 11580.440, 12660.354,  7344.471 days
    class(jul)
    # [1] "difftime"
    as.numeric(jul)
    # [1]  7210.967 11580.440 12660.354  7344.471
    unclass(jul)
    # [1]  7210.967 11580.440 12660.354  7344.471
    # attr(,"units")
    # [1] "days"
    # attr(,"origin")
    # [1] "1970-01-01 GMT"
    ###
    
 
################################################################################

 