#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.1
#   Date and Time Conventions and Standards
#
# List of Examples:  
#
#	Exercise: Convert ISO-8601 Dates to Julian Counts  
#	Exercise: Convert Julian Counts to ISO-8601 Dates  
# 	Exercise: Test the functions .DateToJulian and .JulianToDate  
# 	Exercise: Compute the Day-of-Week 
#  	Exercise: Decide if a Year is a Leap Year or not        
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Packages:

	require(fBasics)
	require(fCalendar)
	###
	

# ------------------------------------------------------------------------------


### Exercise: Convert ISO-8601 Dates to Julian Counts

    # Write your own function:
    .DateToJulian = function(YYYYMMDD)
    {
        # To Julian Date:
        year = YYYYMMDD %/% 10000
        month = (YYYYMMDD - year*10000) %/% 100
        day = YYYYMMDD - year*10000 - month*100
        y = year + ifelse(month > 2, 0, -1)
        m = month + ifelse(month > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        JD = (146097 * c) %/% 4 + (1461 * ya) %/% 4 +
            (153 * m + 2) %/% 5 + day + 1721119
            
        # Return Value:
        JD
    }

    # Try:
    .DateToJulian(c(19600101, 19700101, 20050101))
    # [1] 2436936 2440588 2453372
 	###
 	
  
# ------------------------------------------------------------------------------


### Exercise: Convert Julian Counts to ISO-8601 Dates
 
	# Write your own function:
	.JulianToDate = function(JD)
    {
        # To ISO Date:
        y <- (4 * (JD - 1721119) - 1) %/% 146097
        j <- 4 * j - 1 - 146097 * y
        d <- j %/% 4
        j <- (4 * d + 3) %/% 1461
        d <- 4 * d + 3 - 1461 * j
        d <- (d + 4)%/%4
        m <- (5 * d - 3)%/%153
        d <- 5 * d - 3 - 153 * m
        d <- (d + 5)%/%5
        y <- 100 * y + j
        y <- y + ifelse(m < 10, 0, 1)
        m <- m + ifelse(m < 10, 3, -9)
        YYYYMMDD = y*10000 + m*100 + d
        
        # Return Value:
        YYYYMMDD
    }

    # Try:
    .JulianToDate(c(2436935, 2440588, .DateToJulian(20050101)))
    # [1] 19600101 19700101 20050101
    ###
    
    
# ------------------------------------------------------------------------------
   

### Exercise: Test the functions .DateToJulian and .JulianToDate  
    
	# Generate 20 random Dates:
	n = 20
	YYYY = floor(c(1970, 1960, runif(n, 1620, 2100)))
	MM = floor(c(1, 1, runif(n, 1, 12)))
	DD = floor(c(1, 1, runif(n, 1, 28)))
	cbind(YYYY, MM, DD)
	###
	
		
	# Test - Returns a Vector of zeros:
	YYYY*10000+MM*100+DD - julian2date(date2julian(YYYY, MM, DD))
	###
	
	
# ------------------------------------------------------------------------------


### Exercise: Compute the Day-of-Week
       
	# Write your own function:
	.DayOfWeek = function(YYYYMMDD)
    {
        # Compute Day of Week:
        year = YYYYMMDD %/% 10000
        month = (YYYYMMDD - year*10000) %/% 100
        day = YYYYMMDD - year*10000 - month*100
        
       	# Method based on Julian Counts:
       	a = (14 - month) %/% 12
	    y = year - a
	    m = month + 12*a - 2
	    DOW.JD = (day + y + y%/%4 - y%/%100 + y%/%400 + (31*m)%/%12) %% 7
	   
        # Method used in Chron Package;
        ix = year + trunc((month - 14)/12)
	    jx = (trunc((13 * (month + 10 - (month + 10)%/%13 * 12) - 1)/5) + 
	    	day + 77 + (5 * (ix - (ix%/%100) * 100))%/%4 + ix%/%400 - 
	    	(ix%/%100) * 2)
	    DOW.CHRON = jx %% 7

	    # Method of Uspensky and Heaslet:
	    c = year %/% 100
	    y = year - c*100
	    m = ifelse(month > 2, month-2, month+10)
	    y = y - ifelse(month < 3, 1, 0)
	    DOW.UH = (day + floor(2.6*m-0.2) + y + y%/%4 + c%/%4 - 2*c ) %% 7
	        
	    # Return Value:
	    cbind(DOW.JD, DOW.CHRON, DOW.UH)
    }
    ###
    
    # Test it with Random Dates:
	n = 20
	YYYY = floor(c(1970, 1960, runif(n, 1620, 2100)))
	MM = floor(c(1, 1, runif(n, 1, 12)))
	DD = floor(c(1, 1, runif(n, 1, 28)))
	YYYYMMDD = YYYY*10000 + MM*100 + DD
	.DayOfWeek(YYYYMMDD)
    ###
    
    
# ------------------------------------------------------------------------------


### Exercise: Decide if a Year is a Leap Year or Not
       
	# Write your own function:
	.LeapYear = function(YYYY)
    {
        # Return TRUE or FALSE:
        leap.year =
            YYYY %% 4 == 0 & (YYYY %% 100 != 0 | YYYY %% 400 == 0)
            
        # Return Value:
        leap.year
    }

    # Try:
    .LeapYear(c(2000, 2002, 2004, 2006))
    # [1]  TRUE FALSE  TRUE FALSE
	###
	
	
################################################################################  
	    
	   