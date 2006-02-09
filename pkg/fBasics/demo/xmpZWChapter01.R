
#
# Example:
# 	A Compendium for R and Rmetrics users to the book 
#     "Modeling Financial Time Series with S-Plus" 
#     written by E. Zivot and J. Wang
#   ISBN 0-387-95549-6
#
# Details:
#   Chapter 1
#
# Notes:
#   This is not a COPY of the S-Plus "example.ssc" files accompanying the
#     book of Zivot and Wang. It is worth to note that this file contents a 
#     new implementation of the examples tailored to Rmetrics based on R.
# 	Diethelm Wuertz
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#
# Author:
#	(C) 2002-2004, Diethelm Wuertz, GPL
#


################################################################################


# ------------------------------------------------------------------------------
# Section 1.2.1 - Assignment

    
	# Integer:
    a = 3
    a
    ###
       
    # Matrix:
    # matrix(a = 10, 5, 5) dosn't work
    # requires "<-" !
    matrix(a <- 10, 5, 5)
    ###
    
    
# ------------------------------------------------------------------------------
# Section 1.2.2 - Class
    

    # Numeric:
    class(a)
    b = 100000.
    class(b)
    ###
       
    # Integer Multiplication:
    # Next Works in R - in Splus you will get NA
    100000 * 100000
    2^31 - 1
    ###
        
    # largest Integer:
    .Machine$integer.max
    b*b
    ###
        
    # Matrix Object:
    abMat = matrix(c(a, b), nrow = 2)
    class(abMat)
    abMat
    class(matrix)
    matrix
    ###
      
    # List Object:
    abList = list(aComponent = a, bComponent = b)
    class(abList)
    abList
    length(abList)
    names(abList)
    abList$aComponent
    abList[[2]]
    ###   
    
    # timeDate Object:
    # Note R has another default Format - 
    # so you net the format specification
    timeStamp = timeDate(c("1/1/2001", "1/2/2001", "1/3/2001"), "%m/%d/%Y")
    timeStamp
    class(timeStamp)
    # NOTE: 
    # R's timeDate returns an S3 object of class POSIXt/POSIXlt 
    # @-slots are only available for S4 objects
    # slotNames("timeStamp")
    # timeStamp@.Data
    ###
       
    # List Object, continued ...
    abList$anotherComponent = "a string component"
    abList
    abList$aComponent = NULL
    abList
    ###
        
    # timeDate Object, continued ...
    # NOTE: 
    # R's timeDate returns an S3 object of class POSIXt/POSIXlt,
    # @-slots are only available for S4 objects 
    # timeStamp@time.zone
    ###
    
    
# ------------------------------------------------------------------------------
# Section 1.2.3 - Method
    
    
    # Summary method:
    summary(abMat)
    summary(abList)
    ###
       
    # NOTE: 
    # R has no print.list function, 
    # I think we don't really need it! 
    # print.list(matrix)
    ###
    

# ------------------------------------------------------------------------------
# Section 1.3 - Modeling Functions in S+FinMetrics
    
        
    # Add Data:
    stack.loss = 
        c(42,37,37,28,18,18,19,20,15,14,14,13,11,12,8,7,8,8,9,15,15)
    stack.x = matrix(c(
        80,27,89, 80,27,88, 75,25,90, 62,24,87, 62,22,87, 62,23,87,
        62,24,93, 62,24,93, 58,23,87, 58,18,80, 58,18,89, 58,17,88,
        58,18,82, 58,19,93, 50,18,89, 50,18,86, 50,19,72, 50,19,79,
        50,20,80, 56,20,82, 70,20,91), byrow = TRUE, ncol = 3, 
        dimnames = list(NULL, c("Air.Flow", "Water.Temp", "Acid.Conc.")))
    ###
    
    
# ------------------------------------------------------------------------------
# Section 1.3.1 - Formula Specification

    
    # Arguments:
    args(lm)
    ###
          
    # Data Frame:
    stack.df = data.frame(Loss = stack.loss, stack.x)
    stack.df
    class(stack.df)
    # For colIds use colnames ...
    colnames(stack.df)
    ###
           
    # Regression:
    test.mod = lm(Loss ~ Air.Flow + Water.Temp, data = stack.df)
    test.mod
    class(test.mod)
    oldClass(test.mod)
    ###
                
    # timeDate Object, continued ...
    # Splus would yield "NULL" see what R delivers
    oldClass(timeStamp)
    ###
        
    # As "timeSeries" Object:
    # stack.ts = timeSeries(stack.df)
    # For a dummy 'timeSeries' use:
    stack.ts = as.timeSeries(stack.df)
    class(stack.ts)
    # Wouldn't work neither in R nor in Splus:
    test = try(lm(Loss ~ Air.Flow + Water.Temp, data = stack.ts),
        silent = TRUE)
    test
    ###
       
    # Try convert to "matrix" Object:
    stack.mat = stack.ts@Data
    class(stack.mat) 
    # Wouldn't work neither in R nor in Splus:
    colnames(stack.mat) = colnames(stack.df)
    # Wouldn't work neither in R nor in Splus:
    test = try(lm(Loss ~ Air.Flow + Water.Temp, data = stack.mat), 
        silent = TRUE)
    test
    ### 
        
    # Try with missing data object - it's on the stack:
    lm(stack.loss ~ stack.x)
    ###
      
    # Try, wouldn't work neither in R nor in Splus:
    stack.x.df = as.data.frame(stack.x)
    test = try(lm(stack.loss ~ stack.x.df), silent = TRUE)
    test
    ### 
     
    # Try, wouldn't work neither in R nor in Splus:
    test = try(timeSeries(stack.loss), silent = TRUE)
    # > lm(stack.loss.ts ~ stack.x)
    ###
    
    
# ------------------------------------------------------------------------------
# Section 1.3.2 - Method
    

    # Coefficients Method:
    coef(test.mod)
    ###
       
    # Predict Method:
    # > predict(test.mod, matrix(1, 5, 3))
    # Wouldn't work in R, R requires a data frame with dimnames!
    # Convert to data frame and add dimnames: 
    dimnames = list(paste(1:5), dimnames(stack.x)[[2]])
    predict(test.mod, as.data.frame(matrix(1, 5, 3, dimnames = dimnames)))
    ###
       
    # Plot Method:
    # Splus shows 7 plots, R the following four:
    # - Residual vs. Fitted
    # - Normal Q-Q Plot
    # - Scale Location Plot
    # - Cook's Distance Plot
    par(mfrow = c(2, 2), cex = 0.75)
    plot(test.mod, ask = FALSE)
    ###
    
    
# ------------------------------------------------------------------------------
# Section 1.4 - S-Plus Resources


    # For the Insightful web page ...
    #     http://www.sinsightful.com
    # For SPlus Books ...
    #     http://www.sinsightful.com/support/splusbooks/.asp
    # For the R web page
    #     http://www.r-project.org
	# For the Rmetrics web page
    #     http://www.rmetrics.org
    ###
    

################################################################################

