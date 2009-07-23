## Transfer moments about different locations for any distributions
momChangeAbout <- function(order, oldMom, oldAbout, newAbout) {
     if (!is.vector(oldMom)){
        stop("A vector of moments must be supplied")
     }
     if (length(oldMom) < order) {
        stop("The length of of the vector oldMom must not be less than the
          value of order")
     }
     ## Compute moment about location new
     oldMom <- c(1,oldMom)
     oldMom <- oldMom[1:(order+1)]
     binomCoeff <- choose(order, 0:order)
     diffPower <- (oldAbout - newAbout)^(order:0)
     mom <- sum(binomCoeff*diffPower*oldMom)
     ## Return moment
     return(mom)
}

