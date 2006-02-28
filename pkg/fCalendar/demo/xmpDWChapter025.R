#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.5
#   Calendarical Calculations
#
# List of Examples:                
#   
#	Example: Formatted Gregorian Dates to Julian Date Numbers
#	Example: Dates in Standard Date Format
#   Example: Date/Time in Extended Date/Time Format
#   Example: Manage Holiday Calendars
#   Example: Time Zones
#	Example: Calculate Date of Easter
#   Example: Calculate Holiday Dates
#   Example: Time Zones and Day Light Saving Times
#   Example: Convert to/from Local Time:
#   Example: Convert Within the Same Time Zone
#	Example: Investigate Changeover to DST:
#	Example: Some Additional Time Zone Abbreviations
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Calendarical Calculations


### Example: Formatted Gregorian Dates to Julian Date Numbers

    
	# Convert Formatted Gregorian Dates
	fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
	fdates
	fjulian(fdates)
	###
	
	# Convert with dmy order:
	fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
	fjulian(fdates, order = 'dmy')
	###
   
   
# ------------------------------------------------------------------------------


### Load Packages:

	require(fBasics)
	require(fCalendar)
	###
	

# ------------------------------------------------------------------------------


### Example: Dates in Standard Date Format

   # Handling a Single Date:
   # Manage Dates in Standard Date Formats
   # Date Standard: ISO-8601
   # Handling a Single Date:
   sjulian (19990101) 
   sdate(sjulian(19990101))
   sday.of.week(19990101)
   day.of.week(1, 1, 1999)
   sleap.year(19990101)
   ###

   # Handling Date Vectors:
   date = c(19730101, 19950131, 20000101)
   sjulian(date)
   sdate(sjulian(date))
   sday.of.week(date)
   sleap.year(date)
   ###
   
   
# ------------------------------------------------------------------------------


### Example: Date/Time in Extended Date/Time Format

	# ISO-8601 ISO DATE/TIME FORMAT:
	# Manage Date/Time in Extended Date/Time Format
	# Date Standard: ISO-8601
	# Date: 1973-01-01 15:30
	xjulian(197301011530)
	print(xdate(xjulian(197301011530)), digits=9)
	xday.of.week(197301011530)
	xleap.year(197301011530)
	###
	

# ------------------------------------------------------------------------------


### Example: Manage Holiday Calendars

	# What date has Monday (nday=1) on.or.after March 15, 1986? 
	on.or.after(year=1986, month=3, day=15, nday=1)	
	# What date has the Friday (nday=5) on.or.before April 22, 1977? 
	on.or.before(year=1977, month=4, day=22, nday=5)	
	# What date is the second Sunday in October 1980?
	nth.of.nday(year=1980, month=10, nday=0, nth=2)	
	# What date has the last Monday in May, 1996?
	last.of.nday(year=1996, month=5, lastday=31, nday=1)
	###
	
	# Calculate the date for Easter and Pentecost from 2000 until 2010:
	Easter(2000:2010)
	Pentecost(2000:2010)
	###
	
	# Create a holiday Calendar for the New York Stock Exchange for 2002:
	NYSECalendar = sort(c(NewYearsDay(2002), USMLKingsBirthday(2002), 
	 USWashingtonsBirthday(2002), GoodFriday(2002), USMemorialDay(2002), 
	 USIndependenceDay(2002), USLaborDay(2002), USThanksgivingDay(2002), 
	 ChristmasDay(2002)))
	NYSECalendar
	sday.of.week(NYSECalendar)
	###
	
	
# ------------------------------------------------------------------------------


### Example: Time Zones

	# Note we need "GMT" as time zone
	# Test:
	Sys.timezone()
	###
	
	# How to change Time Zone to GMT ?
	# Start -> Control Panel -> System
	#    Advanced -> Environment Variablees
	#      User Variables -> New
	# Add:
	#      Variable Name   TZ
	#      Variable Value  GMT
	# Ok -> Ok -> Ok
   	###
   	

# ------------------------------------------------------------------------------


### Example: Calculate Date of Easter

	currentYear         # prints current year as integer
	easter()            # date of easter this year
	easter(2000:2009)   # easter for the 2k decade  
	###
	
	class(easter())     # what class?
	timeDate(easter())  # Convert to timeDate
	###
    
    
# ------------------------------------------------------------------------------


### Example: Calculate Holiday Dates

	holiday()
	holiday(2000:2009, "USLaborDay")
	class(holiday())
	print(USLaborDay(2000:2009))
	###
    
    
# ------------------------------------------------------------------------------


### Example: Time Zones and Day Light Saving Times
   
	Sys.getenv("TZ")
	Sys.timezone()
	if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
	###
    
   
# ------------------------------------------------------------------------------


### Example: Convert to/from Local Time:

	# Start with character string:
	charvec = "2004-08-01 00:00:00"
	# Greenwich Mean Time:
	GMT = timeDate(charvec, zone = "GMT", FinCenter = "GMT")
	GMT
	# From GMT to local Zurich time:
	ZUR = timeDate(GMT, zone = "GMT", FinCenter = "Europe/Zurich")
	ZUR
	# From Zurich local time to New-York local time:
	NYC = timeDate(ZUR, zone = "Europe/Zurich", FinCenter = "America/NewYork")
	NYC
	# Or, directly from GMT to New-York local time:
	NYC = timeDate(GMT, zone = "GMT", FinCenter = "America/NewYork")
	NYC
	###

 
# ------------------------------------------------------------------------------


### Example: Convert Within the Same Time Zone

	# What time was it in Berlin at April 5th and 6th, 1980, 
	# at 4:00 PM Zurich time?
	ZURICH = c("1980-04-05 16:00:00", "1980-04-06 16:00:00")
	zone = "Europe/Zurich"
	FinCenter = "Europe/Berlin"
	BERLIN = timeDate(ZURICH, zone = zone, FinCenter = FinCenter)
	ZURICH; BERLIN
	# [1] "1980-04-05 16:00:00" "1980-04-06 16:00:00"
	# [1] "Europe/Berlin"
	# [1] [1980-04-05 16:00:00] [1980-04-06 17:00:00]
	# Note, in 1980 Switzerland had no Daylight Savings Time in 
	# contrast, to Germany!


# ------------------------------------------------------------------------------


### Example: Investigate Changeover to DST
	
	TIME = c("27 23:00:00", "27 23:15:00", "27 23:30:00", "27 23:45:00",
	         "28 00:00:00", "28 00:15:00", "28 00:30:00", "28 00:45:00",
	         "28 01:00:00", "28 01:15:00", "28 01:30:00", "28 01:45:00",
	         "28 02:00:00", "28 02:15:00", "28 02:30:00", "28 02:45:00",
	         "28 03:00:00", "28 03:15:00", "28 03:30:00", "28 03:45:00",
	         "28 04:00:00", "28 04:15:00", "28 04:30:00", "28 04:45:00"  )
	GMT = paste("2004-03-", TIME, sep = "")
	LONDON = timeDate(GMT, zone = "GMT", FinCenter = "Europe/London")
	GMT2 = timeDate(LONDON, zone = "Europe/London", FinCenter = "GMT")
	# Print:
	cbind(GMT, LONDON=as.character(LONDON@Data), GMT2=as.character(GMT2@Data))
	###
	
	# Check the other way around in Autumn!
	# Has still to be implemented ..
	###
   

# ------------------------------------------------------------------------------


### Example: Some Additional Time Zone Abbreviations

	# -------------------------------------------------------------------------
	# Time Zone Name     Other abbreviations  Time Zone    Use
	# -------------------------------------------------------------------------
	# GMT   Greenwich Mean Time          UTC  UTC          "GMT"
	# BST   British Summer  Time              UTC+1 hour   "Europe/London"
	# WET   Western European Time             UTC      
	# WEST  Western European Summer Time      UTC+1 hour   "Europe/Zurich"
	# CET   Central European Time        MEZ  UTC+1 hour 
	# CEST  Central European Summer Time MESZ UTC+2 hours 
	# EET   Eastern European Time             UTC+2 hours 
	# EEST  Eastern European Summer Time      UTC+3 hours  
	# ------------------------------------------------------------------------- 
	# AST   Atlantic Standard Time       HNA  UTC-4 hours  
	# ADT   Atlantic Daylight Time       HAA  UTC-3 hours 
	# EST   Eastern Standard Time        HNE  UTC-5 hours  "Europe/NewYork"
	# EDT   Eastern Daylight Time        HAE  UTC-4 hours 
	# CST   Central Standard Time        HNC  UTC-6 hours 
	# CDT   Central Daylight Time        HAC  UTC-5 hours   
	# MST   Mountain Standard Time       HNR  UTC-7 hours 
	# MDT   Mountain Daylight Time       HAR  UTC-6 hours 
	# PST   Pacific Standard Time        HNP  UTC-8 hours 
	# PDT   Pacific Daylight Time        HAP  UTC-7 hours 
	# -------------------------------------------------------------------------
    
  
################################################################################

  