
#
# fBasics Functions Addon:
#
#   2 Computes Bootstrapped Mean
#   3 Imports data from comma delimited CSV files
#
# Author:
#   Diethelm Wuertz, GPL
#


################################################################################
# 2 Computes Bootstrapped Mean
#   A very fast implementation of the basic nonparametric bootstrap for 
#   obtaining confidence limits for the population mean without assuming 
#   normality.       


bootMean =
function(x, B = 1000, ci = 0.95, na.rm = TRUE, reps = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   A very fast implementation of the basic nonparametric 
    #   bootstrap for obtaining confidence limits for the population 
    #   mean without assuming normality.       
    
    # Arguments:
    #   B - number of bootstrap resamples, by default 1000.
    #   ci - specifies the confidence level (0-1) for interval 
    #       estimation of the population mean. 
    #   na.rm - a logical flag, should NAs be removed?
    #   reps - set to TRUE to have bootMean return the vector 
    #       of bootstrapped means as the reps attribute of 
    #       the returned object .
    
    # Notes:
    #   The function calls "smean.cl.boot" from the "HMisc" package
    #   Requirements: require(Hmisc)       
    
    # FUNCTION:       
    
    # Requirements:
    sink("@sink@") # Skip Loading Comments ...
    library(Design, warn.conflicts = FALSE)
    library(Hmisc, warn.conflicts = FALSE)
    sink()
    unlink("@sink@") 
           
    # Return Value:
    smean.cl.boot(x = x, conf.int = ci, B = B, na.rm = na.rm, reps = reps)
}


################################################################################
# 3 Imports data from comma delimited CSV files


data.csv = csvImport =
function (..., package = .packages(), sep = ",")
{   
    # Description:
    #   Imports data from comma delimited CSV files
    
    # Arguments:
    #   package - a name or character vector giving the packages to 
    #     look into for data sets.  By default, all packages in the 
    #     search path are used, then the `data' directory (if present) 
    #     of the current working directory.
    #   sep - the field separator character. Values on each line of 
    #     the file are separated by this delimeter, by default a comma 
    #    for "fLibraries" files.
    #   ... - the filename (excluding the .csv extension), a character 
    #   string.
    
    # Value:
    #   A dataframe for a multi-column data file, a vector for a 
    #   single-column data file.
    
    # Notes:
    #   This function is a synonyme for R's data() reading *.csv files
    #     with an arbitrary file separator. The only difference to the
    #     original code of data() ist, that we have added an additional
    #     argument sep.csv with default value ",".
    #   Be aware , multi-columns tables are returned as dataframes, 
    #     single-column tables are returned as vector.
    
    # FUNCTION:
    
    # Settings:
    list = character(0)
    lib.loc = NULL 
    verbose = getOption("verbose")
    sep.csv = ","
    row.names=NULL 
    
    # Internal Function:
    sQuote = function(s) paste("`", s, "'", sep = "")
    names = c(as.character(substitute(list(...))[-1]), list)
    if (!missing(package)) 
        if (is.name(y = substitute(package))) 
            package = as.character(y)
    found = FALSE
    fsep = .Platform$file.sep
    paths = .find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc)) 
        paths = c(.path.package(package, TRUE), getwd(), paths)
    paths = unique(paths[file.exists(paths)])
    nodata = !file.exists(file.path(paths, "data"))
    if (any(nodata)) {
        if (!missing(package) && (length(package) > 0)) {
            packagesWithNoData = package[package %in% sapply(paths[nodata], 
            basename)]
        if (length(packagesWithNoData) > 1) {
            warning(paste("packages", paste(sQuote(packagesWithNoData), 
            collapse = ", "), "contain no datasets")) }
        else if (length(packagesWithNoData) == 1) {
            warning(paste("package", sQuote(packagesWithNoData), 
                "contains no datasets")) } }
        paths = paths[!nodata] }
    if (length(names) == 0) {
        db = matrix(character(0), nr = 0, nc = 4)
        noindex = character(0)
        for (path in paths) {
            INDEX = file.path(path, "data", "00Index")
        if (file.exists(INDEX)) {
            entries = read.00Index(INDEX)
            if (NROW(entries) > 0) {
                db = rbind(db, cbind(basename(path), dirname(path), 
            entries)) } }
        else {
            if (length(list.files(file.path(path, "data"))) > 0) 
            noindex = c(noindex, basename(path)) } }
        colnames(db) = c("Package", "LibPath", "Item", "Title")
        if (length(noindex) > 0) {
            if (!missing(package) && (length(package) > 0)) {
                packagesWithNoIndex = package[package %in% noindex]
            if (length(packagesWithNoIndex) > 1) {
                warning(paste("packages", paste(sQuote(packagesWithNoIndex), 
                collapse = ", "), "contain datasets but no index")) }
            else if (length(packagesWithNoIndex) == 1) 
                warning(paste("package", sQuote(packagesWithNoIndex), 
                "contains datasets but no index")) } }
        footer = if (missing(package)) 
        paste("Use `data(package = ", ".packages(all.available = TRUE))'\n", 
            "to list the data sets in all ", "*available* packages.", 
            sep = "")
        else NULL
        y = list(type = "data", header = NULL, results = db, 
            footer = footer)
            class(y) = "packageIQR"
        return(y) }
    paths = file.path(paths, "data")
    for (name in names) {
        files = NULL
        for (p in paths) {
            if (file.exists(file.path(p, "Rdata.zip"))) {
                if (file.exists(fp = file.path(p, "filelist"))) 
                files = c(files, file.path(p, scan(fp, what = "", 
                    quiet = TRUE)))
            else warning(paste("`filelist' is missing for dir", p)) }
            else {
            files = c(files, list.files(p, full = TRUE)) } }
        files = files[grep(name, files)]
        found = FALSE
        if (length(files) > 0) {
            subpre = paste(".*", fsep, sep = "")
            for (file in files) {
            if (verbose) 
            cat("name=", name, ":  file= ...", fsep, sub(subpre, 
                "", file), "::      ", sep = "")
            if (found) 
            break
            found = TRUE
            ext = sub(".*\\.", "", file)
            if (sub(subpre, "", file) != paste(name, ".", 
            ext, sep = "")) 
            found = FALSE
            else {
            zfile = zip.file.extract(file, "Rdata.zip")
            # next four lines added:
            if (ext == "csv" || ext == "CSV") {
                # handling data.frames:
                result = read.table(zfile, header = TRUE, 
                    sep = sep.csv,  row.names=row.names)
                # handling if there is only one column: 
                if(length(names) == 1) result = result[,]}
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
                found = FALSE)          
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

