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
#   Example: Formatted Gregorian Dates to Julian Date Numbers
#   Example: Dates in Standard Date Format
#   Example: Date/Time in Extended Date/Time Format
#   Example: Manage Holiday Calendars
#   Example: Time Zones
#   Example: Calculate Date of Easter
#   Example: Calculate Holiday Dates
#   Example: Time Zones and Day Light Saving Times
#   Example: Convert to/from Local Time:
#   Example: Convert Within the Same Time Zone
#   Example: Investigate Changeover to DST:
#   Example: Some Additional Time Zone Abbreviations
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Calendarical Calculations


### Load Packages:

    require(fCalendar)
    ###
    

# ------------------------------------------------------------------------------


### Example: Manage Holiday Calendars
    
    # Calculate the date for Easter and Pentecost from 2000 until 2010:
    Easter(2000:2010)
    Pentecost(2000:2010)
    ###
    
    # Create a 2002 holiday Calendar for New York:
    holidaysNY = c("NewYearsDay", "USMLKingsBirthday", 
        "USWashingtonsBirthday", "GoodFriday", "USMemorialDay", 
        "USIndependenceDay", "USLaborDay", "USThanksgivingDay", 
        "ChristmasDay")
    holidaysNY2002 = sort(holiday(2002, holidaysNY))
    holidaysNY2002
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Time Zones

    # Note we need "GMT" as time zone
    # Test:
    Sys.putenv(TZ = "GMT")
    Sys.time()
    ###
    

# ------------------------------------------------------------------------------


### Example: Calculate Date of Easter

    currentYear         # prints current year as integer
    Easter()            # date of easter this year
    Easter(2000:2009)   # easter for the 2k decade  
    ###
    
    class(Easter())     # what class?
    timeDate(Easter())  # Convert to timeDate
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

  