#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.3
#   Rmetrics 'timeDate' Objects
#
# List of Examples:                
#   
#	Example: 1 Generation of Objects
#	Example: 2 Representation of Objects
#   Example: 3 Mathematical Operations
#	Example: 4 Object Transformations
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Functions and methods for 'timeDate' objects


	# Description:
	#   These are test cases for 'timeDate' objects. We used these
	#   examples to check if the functions and methods included in
	#   the two files '*timeDateClass.R' and 'timeDatemethods' work
	#   properly. The following five topics are considered:
	#
	#   1. Generation of Objects
	#   2. Representation of Objects
	#   3. Mathematical Operations
	#   4. Object Conversions
	#   5. Object Transformations
	#
	# References:
	#   POSIXt from R's "base" package.
	###
 

# ------------------------------------------------------------------------------  


### Load Packages:

	require(fBasics)
	require(fCalendar)
	###
	

# ------------------------------------------------------------------------------


### Example: 1 Generation of Objects
  
	# Character Format:
	# Create Character Vectors and Format Specification:
	dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
	tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
	dts
	tms
	###
   
	# timeDate - Construct Object from a Character Vector:      
	# USE: timeDate(charvec, format = "%Y-%m-%d %H:%M:%S", zone = "GMT", 
	#        FinCenter = "Europe/Zurich", trace = FALSE)   
	# Dates:
	timeDate(dts, format = "%Y-%m-%d", FinCenter = "GMT" )
	# [1] "GMT"
	# [1] [1989-09-28] [2001-01-15] [2004-08-30] [1990-02-09]
	timeDate(dts, format = "%Y-%m-%d", FinCenter = "Europe/Zurich" ) 
	# GMT created from GMT charvectors:
	GMT = timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S", 
	 	zone = "GMT", FinCenter = "GMT")
	GMT
	# Zurich created from local charvectors
	timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S", 
	 zone = "Europe/Zurich", FinCenter = "Europe/Zurich")
	# Zurich created from GMT charvectors
	ZURICH = timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S", 
	 zone = "GMT", FinCenter = "Europe/Zurich") 
	ZURICH
	# [1] "Europe/Zurich"
	# [1] [1989-09-29 00:12:55] [2001-01-15 11:34:02]
	# [3] [2004-08-30 10:30:00] [1990-02-09 12:18:23]
	###
   
  
	# timeDate - Display the Internal Representation:
	# What class?
	class(ZURICH)
	# [1] "timeDate"
	# attr(,"package")
	# [1] ".GlobalEnv"
	unclass(ZURICH)  
	# list()
	# attr(,"Data")
	# [1] "1989-09-29 00:12:55" "2001-01-15 11:34:02"
	# [3] "2004-08-30 10:30:00" "1990-02-09 12:18:23"
	# attr(,"Dim")
	# [1] 4
	# attr(,"format")
	# [1] "%Y-%m-%d %H:%M:%S"
	# attr(,"FinCenter")
	# [1] "Europe/Zurich"
	###
  
    # timeDate - Display the Internal Representation for the @Data Slot:  
	# What class?
	class(GMT@Data)
	# [1] "POSIXt"  "POSIXlt"
	unclass(GMT@Data)  
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
	# [1] 270  14 242  39
	# $isdst
	# [1] 1 0 1 0
	# attr(,"tzone")
	# [1] "GMT"
	###
   
    # timeCalendar - Create a 'timeDate' Object From Calendar Atoms: 
	# USE: function(m = NULL, d = NULL, y = NULL, h = NULL, min = NULL, 
	#        s = NULL, format = options("time.in.format"), zone = "GMT")     
	# Argument List:
	args(timeCalendar)
	###  
	# Format Date:
	CCYYMMDD = timeCalendar(
	 m = c(9, 1, 8, 2), 
	 d = c(28, 15, 30, 9), 
	 y = c(1989, 2001, 2004, 1990), FinCenter = "GMT") 
	CCYYMMDD
	# [1] "GMT"
	# [1] [1989-09-28] [2001-01-15] [2004-08-30] [1990-02-09]
	###
     
	# timeCalender - Continued ...
	CCYYMMDD = timeCalendar(
	 	m = c(9, 1, 8, 2), 
	 	d = c(28, 15, 30, 9), 
	 	y = c(1989, 2001, 2004, 1990), FinCenter = "Europe/Zurich")
	CCYYMMDD
	# [1] "Europe/Zurich"
	# [1] [1989-09-28] [2001-01-15] [2004-08-30] ...
	# Class:
	class(CCYYMMDD)
	# [1] "timeDate"
	# attr(,"package")
	# [1] "fCalendar"
	###
              
	# timeCalender - Continued ...
	CCYYMMDDhhmmss = timeCalendar(
	 	h = c(9, 14), 
	 	min = c(15, 23)) 
	CCYYMMDDhhmmss
	# [1] "Zurich"
	#  [1] [2005-01-01 09:15:00] [2005-02-01 14:23:00]
	#  [3] [2005-03-01 09:15:00] [2005-04-01 14:23:00]
	#  [5] [2005-05-01 09:15:00] [2005-06-01 14:23:00]
	#  [7] [2005-07-01 09:15:00] [2005-08-01 14:23:00]
	#  [9] [2005-09-01 09:15:00] [2005-10-01 14:23:00]
	# [11] [2005-11-01 09:15:00] [2005-12-01 14:23:00]	
	class(CCYYMMDDhhmmss) 
	# [1] "timeDate"
	# attr(,"package")
	# [1] "fCalendar"
	###  

	# timeSequence - Create a Regularly Spaced 'timeDate' Object:   
	# USE: timeSequence(from = "2004-01-01", to = format(Sys.time(), "%Y-%m-%d"), 
	#        by = "day", length.out = NULL, format = "%Y-%m-%d",
	#        FinCenter = "Europe/Zurich", trace = FALSE)
	#   by - a character string, containing one of 
	#        "sec", "min", "hour", "day", "DSTday", "week", "month" or "year".
	# Argument List:
	args(timeSequence)
	###
	# GMT/Zurich Daily:
	timeSequence(
	    from = "2004-03-12", to = "2004-04-11", 
	    format = "%Y-%m-%d", FinCenter = "GMT") 
	timeSequence(
	    from = "2004-03-12", to = "2004-04-11", 
	    format = "%Y-%m-%d", FinCenter = "Europe/Zurich")
	###
	      
	# timeSequence - Continued ...
	# GMT/Zurich Daily 16:00:
	timeSequence(
	    from = "2004-03-12 16:00:00", to = "2004-04-11 16:00:00", 
	    format = "%Y-%m-%d %H:%M:%S", FinCenter = "GMT")
	timeSequence(
	    from = "2004-03-12 16:00:00", to = "2004-04-11 16:00:00", 
	    format = "%Y-%m-%d %H:%M:%S", FinCenter = "Europe/Zurich")    
	timeSequence(
	    from = "2003-03-12 16:00:00", to = "2004-02-12 16:00:00", 
	    by = "month", format = "%Y-%m-%d %H:%M:%S", FinCenter = "GMT")
	timeSequence(
	    from = "2003-03-12 16:00:00", to = "2004-02-12 16:00:00", 
	    by = "month", format = "%Y-%m-%d %H:%M:%S", FinCenter = "Europe/Zurich")
	### 
   
	# American Day Format Type "m/d/y":
	# Day formats can be detected automatically the format argument
	# has not be specified explicitely.
	timeDate("2004-03-11", FinCenter = "GMT")    
	timeDate("11/5/2004", FinCenter = "GMT") 
	###
  
  
# ------------------------------------------------------------------------------        


### Example: 2 Representation of Objects

	# format.timeDate - Formats 'timeDate' as ISO Conform Character String:
	# Format as character vector:
	charvec = format(ZURICH)  
	charvec
	# [1] "1989-09-29 00:12:55" "2001-01-15 11:34:02" ...
	class(charvec)
	# [1] "character"
	###
   
    # print - Print 'timeDate' object:
   	print(ZURICH)
   	###

   
# ------------------------------------------------------------------------------


### Example: 3 Mathematical Operations

	# "[" - Extracts or Replaces Subsets From 'timeDate' Objects:
	# Zurich 'timeDate':
	ZURICH
	n = length(format(ZURICH)); n
	# Revert:
	ZURICH[n:1]
	# Skip the first three:
	ZURICH[-(1:3)]
	# Get the second and the fourth:
	ZURICH[c(2,4)]
	###
    
	# "+.timeDate" - Performs Arithmetic + Operation on 'timeDate' Objects,
	# "-.timeDate" - Performs Arithmetic - Operation on 'timeDate' Objects:
	# Add One Day:
	ZURICH
	ZURICH + 24*3600
	# Subtract one Day:
	ZURICH - 24*3600
	####
	 
   	# Difftime Object:
   	n = length(format(ZURICH)); n
   	ZURICH[2:n] - ZURICH[1:(n-1)]
   	###
 
	# Ops.timeDate - Group 'Ops' Generic Functions for 'timeDate' Objects:
	# Logical Operations:
   	ZURICH[ZURICH < ZURICH[3]]
   	# [1] "Europe/Zurich"
   	# [1] [1989-09-29 00:12:55] [2001-01-15 11:34:02] [1990-02-09 13:18:23]
   	ZURICH[ZURICH < ZURICH[3]] == ZURICH[1:3]
   	# [1]  TRUE  TRUE FALSE
   	###

	# diff.timeDate  - Returns Suitably Lagged and Iterated Differences:
	# Print:
	ZURICH
	# Compute Differences:
	diff(ZURICH)
	# Time differences of  4126.473,  1322.914, -5315.883 days
	# Lag 3:
	diff(ZURICH, lag = 3)
	# Time difference of 133.5038 days
	# Second order differences:
	diff(ZURICH, lag = 1, differences = 2)
	# Time differences of -2803.559, -6638.797 days
	###

	# difftimeDate - Computes Time Differences:
	# Take difference:
	DIFF = difftimeDate(ZURICH[1:2], ZURICH[-(1:2)])
	DIFF
	# Time differences of -5449.387,  3992.969 days
	## > as.integer(round(DIFF))
	# [1] -5449  3993
	###
	
	# c.timeDate - Concatenates Objects of Class 'timeDate':
   	# Print GMT and LONDON 'timeDate' object
	GMT[1:2]
	ZURICH[1:2]  
	# Concatenate:
	c(GMT[1:2], ZURICH[1:2])
	c(ZURICH[1:2], GMT[1:2])
    ###

	# rep.timeDate - Replicates Objects of Class 'timeDate':
    # Replicate:
    rep(ZURICH[2], times = 3)
    rep(ZURICH[2:3], times = 2)
    ###
   
	# round.timeDate - Rounds Objects of Class 'timeDate':
	# Round:
	modify(GMT, "round", "days")
	modify(ZURICH, "round", "days")
	###
   
	# trunc.timeDate - Truncates Objects of Class 'timeDate':
	# Truncate:
	modify(GMT, "trunc", "days")
	modify(ZURICH, "trunc", "days")
	###

	# start | end - Extracts the first/last Object of a 'timeDate' Vector:
	# Print:
	ZURICH
	# Extract first and last:
	start(ZURICH)
	end(ZURICH)
	###
   
	# sort.timeDate - Sorts the Objects of a 'timeDate' Vector:
	# Print:
	ZURICH
	# Sort:
	modify(ZURICH, "sort")
	# Sort reverted:
	n = length(format(ZURICH))
	modify(ZURICH, "sort")[n:1] 
	###


# ------------------------------------------------------------------------------


### Example: 4 Object Transformations

	# as.character | as.data.frame |
	# as.POSIXct | as.POSIXlt - Convert "timeDate" Objects:  
	# Character:
	as.character(ZURICH)
	# Data Frame:
	as.data.frame(ZURICH)
	# POSIX:
	as.POSIXct(ZURICH)
	###
		
	# julian.timeDate - Extracts Julian Time in Days Since 1970-01-01:
	# Transform to Julian:
	julian(ZURICH)   
	as.integer(julian(ZURICH))
	# Day Units:
	julian(ZURICH, "days")   
	as.integer(julian(ZURICH, "days"))
	## > as.integer(round(julian(ZURICH, "days")))
	###
		
	## atoms.timeDate - Returns "Atoms" From a 'timeDate' Object:
	
	# Extract Calendar Atoms:
	ZURICH
	atoms(ZURICH)
	# Days:
	atoms(ZURICH)[,3]
	# Columnames are: "Y", "m", "d", "H", "M", "S"
	atoms(ZURICH)[, "d"]
	###
   
   
################################################################################
    
