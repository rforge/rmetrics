test.vgMean <- function(testParam = smallShape, n = 10000, accuracy = 0.01) {
    for (i in 1:nrow(testParam)) {
      param <- testParam[i,]  
      # random number generation:  
        x <- rvg(n,param = param)
        
      # Compute mean of the sample data: 
        sampleMean <- mean(x)
      
      # Get mean value from vgMean function:
        funMean <- vgMean(param = param)
      
      # Precision within the accuracy value?
        difference <- abs(sampleMean - funMean) 
        cat("param", sep="\n")
        print(param)
        cat("sample mean", sep="\n")
        print(sampleMean)
        cat("funvtion mean", sep="\n")
        print(funMean)
        cat("difference", sep="\n")
        print(difference)
        cat("checkTrue", sep="\n")
        print(checkTrue(difference < accuracy))
  } 
  
}   
 

test.vgVar <- function(testParam = smallShape, n = 10000, accuracy = 0.01) {
 for (i in 1:nrow(testParam)) {
      param <- testParam[i,]  
      # random number generation:  
        x <- rvg(n,param = param)
        
      # Compute variance of the sample data: 
        sampleVar <- var(x)
      
      # Get mean value from vgVar function:
        funVar <- vgVar(param = param)
      
      # Precision within the accuracy value?
        difference <- abs(sampleVar - funVar) 
        cat("param", sep="\n")
        print(param)
        cat("sample variance", sep="\n")
        print(sampleVar)
        cat("function variance", sep="\n")
        print(funVar)
        cat("difference", sep="\n")
        print(difference)
        cat("checkTrue", sep="\n")
        print(checkTrue(difference < accuracy))
  } 

}

test.vgSkew <- function(testParam = smallShape, n = 10000, accuracy = 0.01) {
 for (i in 1:nrow(testParam)) {
      param <- testParam[i,]  
      # random number generation:  
        x <- rvg(n,param = param)
        
      # Compute skewness of the sample data: 
        sampleSkew <- skewness(x)
      
      # Get skewness value from vgSkew function:
        funSkew <- vgSkew(param = param)
      
      # Precision within the accuracy value?
        difference <- abs(sampleSkew - funSkew) 
        cat("param", sep="\n")
        print(param)
        cat("sample skewness", sep="\n")
        print(sampleSkew)
        cat("function skewness", sep="\n")
        print(funSkew)
        cat("difference", sep="\n")
        print(difference)
        cat("checkTrue", sep="\n")
        print(checkTrue(difference < accuracy))
  } 
}

test.vgKurt <- function(testParam = smallShape, n = 10000, accuracy = 0.01) {
 for (i in 1:nrow(testParam)) {
      param <- testParam[i,]  
      # random number generation:  
        x <- rvg(n,param = param)
        
      # Compute kurtosis of the sample data: 
        sampleKurt <- kurtosis(x)
      
      # Get kurtosis value from vgKurt function:
        funKurt <- vgKurt(param = param)
      
      # Precision within the accuracy value?
        difference <- abs(sampleKurt - funKurt) 
        cat("param", sep="\n")
        print(param)
        cat("sample kurtosis", sep="\n")
        print(sampleKurt)
        cat("function kurtosis", sep="\n")
        print(funKurt)
        cat("difference", sep="\n")
        print(difference)
        cat("checkTrue", sep="\n")
        print(checkTrue(difference < accuracy))
  } 
}