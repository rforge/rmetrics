
#
# fCalendar Functions Addon:
#
#   1 Chron Package Addon
#   2 Zurich Holiday Calendar
#	3 Time Series Summary
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
# 1 Chron Package Addon


print.dates =
function (x, digits = NULL, quote = FALSE, prefix = "", simplify, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	yyyy-m-d Fix for chron package and thus allows for ISO8601 
	#	"CCYY-MM-DD" standard format.	
	
	# Details:
	# 	Default input format: "m/d/y"
	# 	Dates yields the wrong print out - try ...
	# 	  dates("5/15/03",   out.format="yyyy-m-d")   # yields: 2003-May-15
	# 	  dates("5/15/2003", out.format="yyyy-m-d")   # yields: 2003-May-15
	#   Note, that the correct result should be: 2003-05-15
	#   This function patches the bug.		
	
	# FUNCTION:    
	
	# Requirements:
	require(chron)
	
	# Print Dates:
	if (!as.logical(length(x))) {
        cat("dates(0)\n")
        return(invisible(x)) }
    if (missing(simplify) && is.null(simplify <- getOption("chron.simplify"))) 
        simplify <- FALSE 	    
    # Patch - A quick and dirty hack ...
    save.month.abb <<- month.abb
    m = regexpr("mon", attributes(x)$format) < 0
    if (m) month.abb <<- c(paste("0", 1:9, sep=""), "10", "11", "12") 
    formatted = as.character(format.dates(x, simplify = simplify))
	print(formatted, quote = quote) 
    month.abb <<- save.month.abb 	    
    
    # Return Value:
    invisible(x) 
}
	

# ------------------------------------------------------------------------------

    
print.chron = 
function (x, digits = NULL, quote = FALSE, prefix = "", sep = " ", 
enclosed = c("(", ")"), simplify, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	yyyy-m-d Fix for chron package and thus allows for ISO8601 
	#	"CCYY-MM-DD hh:mm:ss" standard format.	
	
	# Details:
	# 	Dates yields the wrong print out - try ...
	# 	  chron("5/15/03", "15:30:00", out.format=c("yyyy-m-d", "h:m:s")) 
	#     chron("5/15/2003", "15:30:00", out.format=c("yyyy-m-d", "h:m:s"))
	#   This yields: (2003-May-15 15:30:00)
	#   Note, that the result should be: (2003-05-15 15:30:00)
	#   This function patches the bug.	
	
	# FUNCTION:	    
	
	# Requirements:
	require(chron)
	
	# Print Chron:
	if (!as.logical(length(x))) {
        cat("chron(0)\n")
        return(invisible(x))}
    if (missing(simplify) && is.null(simplify <- getOption("chron.simplify"))) 
        simplify <- FALSE	    
    # Patch - A quick and dirty hack ...
    save.month.abb <<- month.abb
    m = regexpr("mon", attributes(x)$format) < 0
    if (m[1]) month.abb <<- c(paste("0", 1:9, sep=""), "10", "11", "12") 
    formatted = as.character(format.chron(x, sep = sep, enclosed = enclosed, 
    	simplify = simplify))
	print(formatted, quote = quote) 
    month.abb <<- save.month.abb 	    
    # Return Value:
    invisible(x) }

    
# ------------------------------------------------------------------------------


seq.chron = 
function(from, to, by = "days", length.out = NULL, k.by = 1, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	SPlus like "seq.dates" function	
	
	# Details:
	#	Like "seq.dates", adding by = "quarters" arguments and
	#	SPlus like argument list. For Splus compatibility.
	
	# Notes:
	# 	Very preliminary status.
	
	# FUNCTION:
	
	# Requirements:
	require(chron)
	
	# Add-Ons to Chron:	
	options(warn = -1) # Don't warn if the last column of the matrix cycles!
	if (is.numeric(by)) {
		ans = seq.dates(from = from, to = to, by = "days") 
		index = matrix(1:length(ans), byrow = TRUE, ncol = by)[,1]
		ans = ans[index] }
	else {
		if (by == "quarters") {
			ans = seq.dates(from = from, to = to, by = "months") 
			index = matrix(1:length(ans), byrow = TRUE, ncol = 3)[,1]
			ans = ans[index] }
		else {
			ans = seq.dates(from = from, to = to, by = by) }}       
    if (k.by > 1) {
    	index = matrix(1:length(ans), byrow = TRUE, ncol = k.by)[,1]
		ans = ans[index] }	
	# Return Value:
	ans 
}


################################################################################
# 2 Zurich Holiday Calendar
      
    
holiday.ZURICH = 
function(y = currentYear) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Returns a holiday Calendar for Zurich in Switzerland

	# Details:
    # 	Inspect the holiday database in "data/holiday.db.R"
    # 	... You can add there additional holidays!
    #   	NewYearsDay         Jan, 1st
    #   	GoodFriday          2 days before Easter
    #   	EasterMonday        1 day after Easter
    #   	LaborDay            May, 1st  
    #   	PentecostMonday     50 days after Easter
    #   	ChristmasDay        Dec, 25 
    #   	BoxingDay           Dec, 26  
    #   	CHBerchtoldsDay     Jan, 2nd
    #   	CHSechselaeuten     3rd Monday in April 
    #                       	1 week later if it coincides with Easter Monday
    #   	CHAscension         39 days after Easter
    #   	CHConfederationDay  Aug, 1st
    #   	CHKnabenschiessen   2nd Saturday to Monday in Sep
    
    # FUNCTION:
    
    # Calendar:
	years = y
	holidays = NULL
	# Iterate Years:
	for (y in years ) { 
		holidays = c(holidays, NewYearsDay(y))
	    holidays = c(holidays, GoodFriday(y))   
	    holidays = c(holidays, EasterMonday(y)) 
	    holidays = c(holidays, LaborDay(y))
	    holidays = c(holidays, PentecostMonday(y))  
	    holidays = c(holidays, ChristmasDay(y)) 
	    holidays = c(holidays, BoxingDay(y)) 
	    holidays = c(holidays, CHBerchtoldsDay(y))
	    holidays = c(holidays, CHSechselaeuten(y))
	    holidays = c(holidays, CHAscension(y))
	    holidays = c(holidays, CHConfederationDay(y))
	    holidays = c(holidays, CHKnabenschiessen(y)) }
	
	# Sort and Convert to 'timeDate':
	holidays = as.character(sort(holidays))
	ans = timeDate(holidays, format = "%Y%m%d", FinCenter = "GMT")

	# Remove Remaining Weekend Dates:
	ans = ans[!( (ans@Data)$wday == 0 | (ans@Data)$wday == 6 )]

	# Set Financial Center:
	ans@FinCenter = "Europe/Zurich"

	# Return Value:
	ans 
}


################################################################################
# 3 'timeSeries' Summary

    
summary.timeSeries = 
function(x) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	S3 Summary method for objects of class "timeDate"
	
	# Arguments
	#	x - an object of class "timeDate"
	
	# FUNCTION: 

    # Series Name:
    cat("\nTime Series:        ")
    cat("\n Name:              ", substitute(x))    
    # Data Matrix:
    Dim = dim(x@Data)
    cat("\nData Matrix:        ")
    cat("\n Dimension:         ", Dim)
    cat("\n Column Names:      ", colnames(x@Data) )
    firstName = rownames(x@Data)[1]
    lastName = rownames(x@Data)[Dim[1]]
    cat("\n Row Names:         ", firstName, " ... ", lastName)
    # Date/Time Positions:
    positions = seriesPositions(x)
    cat("\nPositions:          ")
    cat("\n Start:             ", as.character(start(positions)))
    cat("\n End:               ", as.character(end(positions)))
    # Other Attributes:
    cat("\nAttributes:         ")
    cat("\n Format:            ", x@format)
    cat("\n FinCenter:         ", x@FinCenter)
    cat("\n Units:             ", x@units)
    cat("\n Title:             ", x@title)
    cat("\n Documentation:     ", x@documentation)
    cat("\n") 
}  


################################################################################

