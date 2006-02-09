#
# Examples from the Monograph:
# 	"Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 1.2
#   Financial Time Series Data
#
# List of Examples, Exercises and Code Snippets:
#
#	1.2.1 Example: Representations by 'vector' and 'matrix' Objects
#   1.2.2 Example: Representations by 'ts' Objects
#	1.2.3 Example: Representations by 'timeSeries' Objects
# 	1.2.4 Example: Downloading from the Federal Reserve Database
#   1.2.5 Example: Downloading from Yahoo's Web Site
#	1.2.6 Example: Downloading Fundamental Data from Yahoo
#       * Example: Downloading from Economagics Web Site
#   1.2.7 Example: Loading and Executing an Example File
#   1.2.8 Example: Loading Example Data Files
#   1.2.9 Example: Loading Non-ISO 8601 Example Data Files
#       * Exercise: Function to load CSV files with arbitrary delimiter
#
# Author:
#	(C) 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


### Load Packages:

	require(fBasics)
	require(fCalendar)
	###
	
	
# ------------------------------------------------------------------------------
	

### 1.2.1 Example: Representation by 'vector' and 'matrix' Objects

	# This example demonstrates the representation of data records 
	# as a vector, matrix or a data frame.

	# What possibilities do we have to represent regular monthly, quarterly,
	# or annual data? We assume that we know the date when the data was
	# recorded. For example and of month data can bedelivered at the last
	# calendar date, or business day of a month. Here we consider the 
	# foreign exchange rates between the USD and EUR in the year 2003.
		
	# We write the values as a simple numeric vector x:
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)
  	###
  	 	
  	# We attribute the last day in the month to these values:
  	dates = c( 
  		"2003-01-31", "2003-02-28", "2003-03-31", 
  		"2003-04-30", "2003-05-31", "2003-06-30",
		"2003-07-31", "2003-08-31", "2003-09-30", 
		"2003-10-31", "2003-11-30", "2003-12-31")
	attr(x, "dates") <- dates
	###
		
	# And we attribute an appropriate unit for the instrument:
	attr(x, "units") <- "USDEUR"
	###
	
	# So this vector completely determines the time series:
	print(x)
	class(x)
	plot(x, type = "l")
	###
		
	# Using a matrix representation we can use this concept for 
	# multivariate time series, e.g. add the USDCAD rate:
	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	X = matrix(c(x, y), ncol = 2)
	attr(X, "dates") <- dates
	attr(X, "units") <- c("USDEUR", "USDCAD")
	print(X)
	class(X)
	plot(x, type = "l", ylim = c(min(c(x, y)), max(c(x, y)) ))
	lines(y, col = "red")
	###
	
	# Instead of using attributes we can attribute the units and
	# dates as column and rownames to the matrix:
	X = matrix(c(x, y), ncol = 2)
	colnames(X) = c("USDEUR", "USDCAD")
	rownames(X) = dates
	print(X)
	###
	
		
# ------------------------------------------------------------------------------


### 1.2.2 Example: Representation by 'ts' Objects:  	

	# This example demonstrates the representation of data records
	# as a regular time series object of class 'ts':
	
	# R offers objects of classes 'ts' and 'mts' to represent
	# regular time series.
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)
	x.ts = ts(x, start = c(2003, 1), frequency = 12)
	###
		
	# Unfortunately, the full date gets lost, but in most cases
	# this would be irrelevant.
	# Optionally 'units' information and if needed full dates
	# can be added as attributes:
	attr(x.ts, "dates") <- dates
	attr(x.ts, "units") <- "USDEUR"
	print(x.ts)
	plot(x.ts)
	class(x.ts)
	unclass(x.ts)
	###
		
	# Now let us consider a multivariate time series:
	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	Y.ts = ts(cbind(USDEUR = x, USDCAD = y))	
	print(Y.ts)
	###
		
	# Optionally full dates have to be attached as an attribute:
	attr(Y.ts, "dates") = dates
	print(Y.ts)
	###
	
	# Plotting a multivariate 'ts' in one graph can be done as:
	ts.plot(Y.ts)
	# Or in multiple graphs using:
	plot(Y.ts)
	###
	
	
# ------------------------------------------------------------------------------


### 1.2.3 Example: Representations by 'timeSeries' Objects
	
	# This example demonstrates the representation of data records
	# as a regular time series object of class 'timeSeries':
	
	# USDEUR:
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)	
  	# USDCAD:
  	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	# Create Dates:
	oneDay.inSeconds = 3600*24
	dates = timeSequence(from = "2003-02-01", to = "2004-01-01", by = "month",
		format = "%Y-%m-%d", FinCenter = "GMT") - oneDay.inSeconds
	dates
	class(dates)
	###
	
	# Create Series:
	units = c("USDEUR", "USDCAD")
	FinCenter = "GMT"
	series = timeSeries(cbind(x, y), dates, units, FinCenter = "GMT")	
	series
	class(series)
	###
	
	# Time Series Slots
	slotNames(series)
	###
		
	# Print some of the slots ...
	series@Data
	series@positions
	series@units	
	series@format
	series@FinCenter
	###
		
	# Plot:
	plot(series)
	###
	
	
# ------------------------------------------------------------------------------


### 1.2.4 Example: Downloading from the Federal Reserve Database

	# Show the Arguments of the Function:
	args(fredImport)
	###
	
	# Download DPRIME Data:
    X.web = fredImport("DPRIME")
    # trying URL `http://research.stlouisfed.org/fred2/series/...
    # Content type `application/octet-stream' length 245752 ...
    # URL downloaded 239Kb
	# Read 12923 items
	###
	
    # Show the Download Report:
    print(X.web)
    ###
   
  	# Extract the time series from the @data Slot: 
	DPRIME = as.timeSeries(X.web@data)
    head(DPRIME, 3)
    #            DPRIME
    # 1955-08-04   3.25
    # 1955-08-05   3.25
    # 1955-08-08   3.25
    ###
       
    
# ------------------------------------------------------------------------------


### 1.2.5 Example: Downloading from Yahoo's Web Site

	# Settings:
	symbol = "^DJI"
	file = "DJI.CSV"
	source = "http://chart.yahoo.com/table.csv?"
	# 1.1.1999 - 31.12.2000
	# Note months count from 0 to 11 !
	query = paste("s=", symbol,"&a=0&b=1&c=1999&d=11&e=31&f=2000&x=.csv", 
		sep = "")
	###
	
	# Download:	
	DJI = yahooImport(query, file, source, try = TRUE)
	head(DJI@data)
	###
		
	# Direct data access as.timeSeries Object:
	DJI = as.timeSeries(yahooImport(query, file, source, try = TRUE)@data)
	class(DJI)
	head(DJI)
	tail(DJI)
	###
	
	
# ------------------------------------------------------------------------------


### 12.6 Example: Downloading Fundamental Data from Yahoo
	
	# Note - When the *.html formating from Yahoo changes this 
	# function has to be modified!
	
	# Import:
	keystatsImport("IBM")
    # trying URL 'http://finance.yahoo.com/q/ks?s=IBM'
	#
    # Key Statistics for IBM
    #                     Keyname  Statistic
    # 1                      Date 2005-07-20
    # 2                Market Cap    135.03B
    # 3          Enterprise Value    149.80B
    # 4              Trailing P/E      16.74
    # 5               Forward P/E      15.59
    # 6                 PEG Ratio       1.63
    # 7               Price/Sales       1.36
    # 8                Price/Book       4.41
    # 9  Enterprise Value/Revenue       1.54
    # 10  Enterprise Value/EBITDA      8.977
    # 11          Annual Dividend       0.74
    # 12           Dividend Yield       0.90
    # 13                     Beta      1.653
    # 14           52-Week Change     -3.08%
    # 15             52-Week High      7.66%
    # 16              52-Week Low      99.10
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Downloading from Economagic's Web Site

	# Settings:
	file = "fedfunds2.csv"
	source = "http://www.economagic.com/em-cgi/data.exe/"
	query = "fedstl/fedfunds+2"
	###
	
	# Download:
	FEDFUNDS = economagicImport(query, file, source)@data
	class(FEDFUNDS)
	head(FEDFUNDS)
	###

	
# ------------------------------------------------------------------------------


### 1.2.7 Example: Loading and Executing an Example File

    # Select from the Menu:
    xmpfBasics()
    ###
    
    
# ------------------------------------------------------------------------------


### 1.2.8 Example: Loading Example Data Files
	
    # Show example files:
    require(fCalendar)
    data(package = "fBasics")
    ###
    
    # Load Dow Jones 30:
    data(DowJones30)
    class(DowJones30)
    DowJones30 = as.timeSeries(DowJones30)
    class(DowJones30)
    head(DowJones30, 5)
    ###
    
    # Subsetting:
    IBM = DowJones30[, "IBM"]
    head(IBM)
    #              IBM
    # 1990-12-31 27.86
    # 1991-01-02 27.86
    # 1991-01-03 27.95
    # 1991-01-04 27.86
    # 1991-01-07 27.39
    # 1991-01-08 27.08
    ###
   
    # Start and End Date:
    c(start(IBM), end(IBM))
    # [1] "Zurich"
    # [1] [1990-12-31] [2001-01-02]
    ###


# ------------------------------------------------------------------------------
    

### 1.2.9 Example: Loading Non-ISO 8601 Example Data Files

    # Load IP.dat:
    data(IP.dat)
    class(IP.dat)
    ###

    # Print Head of Data:
    head(IP.dat)
    #     X.d..b..Y    IP
    # 1 28-Jan-1919 7.628
    # 2 28-Feb-1919 7.291
    # 3 28-Mar-1919 7.080
    # 4 28-Apr-1919 7.206
    # 5 28-May-1919 7.249
    # 6 28-Jun-1919 7.712
    ###

    # Convert to ISO-8601 formatted timeSeries Object:
    IP = as.timeSeries(IP.dat, format = "%d-%b-%Y")
    class(IP)
    head(IP)
    #               IP
    # 1919-01-28 7.628
    # 1919-02-28 7.291
    # 1919-03-28 7.080
    # 1919-04-28 7.206
    # 1919-05-28 7.249
    # 1919-06-28 7.712
    ###
    
    
# ------------------------------------------------------------------------------


### Exercise: Function to load CSV files with arbitrary delimiter

	#	Developers can put functions in the data directory of their
	#	packages. For Windows it is quite natural to store data
	#	as "comma separated value" files, sometimes called CSV
	#	spreadsheets. Unfortunately, the R function "data" uses the
	#	'semicolon' and not the 'comma' as field separator. Modify R's
	#	data function in such a way, that it can handle files with an
	#   arbitrary delimeter through the argument 'sep':

	# Import:
	csvImport =
	function (..., package = .packages(), sep = ",")
	{	
		# Description:
		#	Imports data from comma delimited CSV files
		
		# Arguments:
		#	package - a name or character vector giving the packages to 
		#	  look into for data sets.  By default, all packages in the 
		#	  search path are used, then the `data' directory (if present) 
		#	  of the current working directory.
		#	sep - the field separator character. Values on each line of 
		#	  the file are separated by this delimeter, by default a comma 
		#	 for "fLibraries" files.
		#	... - the filename (excluding the .csv extension), a character 
		#	string.
		
		# Value:
		#	A dataframe for a multi-column data file, a vector for a 
		#	single-column data file.
		
		# Notes:
		#	This function is a synonyme for R's data() reading *.csv files
		#	  with an arbitrary file separator. The only difference to the
		#	  original code of data() ist, that we have added an additional
		#	  argument sep.csv with default value ",".
		#	Be aware , multi-columns tables are returned as dataframes, 
		#	  single-column tables are returned as vector.
		
		# FUNCTION:
		
		# Settings:
		list = character(0)
		lib.loc = NULL 
		verbose = getOption("verbose")
		sep.csv = ","
		row.names=NULL 
		
		# Internal Function:
		sQuote <- function(s) paste("`", s, "'", sep = "")
		names <- c(as.character(substitute(list(...))[-1]), list)
		if (!missing(package)) 
			if (is.name(y <- substitute(package))) 
				package <- as.character(y)
		found <- FALSE
		fsep <- .Platform$file.sep
		paths <- .find.package(package, lib.loc, verbose = verbose)
		if (is.null(lib.loc)) 
			paths <- c(.path.package(package, TRUE), getwd(), paths)
		paths <- unique(paths[file.exists(paths)])
		nodata <- !file.exists(file.path(paths, "data"))
		if (any(nodata)) {
			if (!missing(package) && (length(package) > 0)) {
				packagesWithNoData <- package[package %in% sapply(paths[nodata], 
				basename)]
			if (length(packagesWithNoData) > 1) {
				warning(paste("packages", paste(sQuote(packagesWithNoData), 
				collapse = ", "), "contain no datasets")) }
			else if (length(packagesWithNoData) == 1) {
				warning(paste("package", sQuote(packagesWithNoData), 
					"contains no datasets")) } }
			paths <- paths[!nodata] }
		if (length(names) == 0) {
			db <- matrix(character(0), nr = 0, nc = 4)
			noindex <- character(0)
			for (path in paths) {
				INDEX <- file.path(path, "data", "00Index")
			if (file.exists(INDEX)) {
				entries <- read.00Index(INDEX)
				if (NROW(entries) > 0) {
					db <- rbind(db, cbind(basename(path), dirname(path), 
				entries)) } }
			else {
				if (length(list.files(file.path(path, "data"))) > 0) 
				noindex <- c(noindex, basename(path)) } }
			colnames(db) <- c("Package", "LibPath", "Item", "Title")
			if (length(noindex) > 0) {
				if (!missing(package) && (length(package) > 0)) {
					packagesWithNoIndex <- package[package %in% noindex]
				if (length(packagesWithNoIndex) > 1) {
					warning(paste("packages", paste(sQuote(packagesWithNoIndex), 
					collapse = ", "), "contain datasets but no index")) }
				else if (length(packagesWithNoIndex) == 1) 
					warning(paste("package", sQuote(packagesWithNoIndex), 
					"contains datasets but no index")) } }
			footer <- if (missing(package)) 
			paste("Use `data(package = ", ".packages(all.available = TRUE))'\n", 
				"to list the data sets in all ", "*available* packages.", 
				sep = "")
			else NULL
			y <- list(type = "data", header = NULL, results = db, 
				footer = footer)
				class(y) <- "packageIQR"
			return(y) }
		paths <- file.path(paths, "data")
		for (name in names) {
			files <- NULL
			for (p in paths) {
				if (file.exists(file.path(p, "Rdata.zip"))) {
					if (file.exists(fp <- file.path(p, "filelist"))) 
					files <- c(files, file.path(p, scan(fp, what = "", 
						quiet = TRUE)))
				else warning(paste("`filelist' is missing for dir", p)) }
				else {
				files <- c(files, list.files(p, full = TRUE)) } }
			files <- files[grep(name, files)]
			found <- FALSE
			if (length(files) > 0) {
				subpre <- paste(".*", fsep, sep = "")
				for (file in files) {
				if (verbose) 
				cat("name=", name, ":  file= ...", fsep, sub(subpre, 
					"", file), "::      ", sep = "")
				if (found) 
				break
				found <- TRUE
				ext <- sub(".*\\.", "", file)
				if (sub(subpre, "", file) != paste(name, ".", 
				ext, sep = "")) 
				found <- FALSE
				else {
				zfile <- zip.file.extract(file, "Rdata.zip")
				# next four lines added:
				if (ext == "csv" || ext == "CSV") {
					# handling data.frames:
					result <- read.table(zfile, header = TRUE, 
						sep = sep.csv,  row.names=row.names)
					# handling if there is only one column: 
					if(length(names) == 1) result <- result[,]}
				switch(ext, 
					R = , 
						r = source(zfile, chdir = TRUE), 
					RData = , 
					rdata = , 
						rda = load(zfile, envir = .GlobalEnv), 
					TXT = , 
					txt = , 
						tab = assign(name, read.table(zfile, header = TRUE), 
							env = .GlobalEnv), 
					CSV = , 
						csv = assign(name, result, env = .GlobalEnv), 
					found <- FALSE)			
				if (zfile != file) 
					unlink(zfile) }
				if (verbose) 
				cat(if (!found) 
				 "*NOT* ", "found\n") } }
			if (!found) 
			warning(paste("Data set", sQuote(name), "not found")) }
		
		# Return Value:
		invisible(name)
	}


################################################################################

