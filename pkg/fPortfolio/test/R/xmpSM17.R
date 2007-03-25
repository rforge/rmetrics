
# ------------------------------------------------------------------------------
# Creating datalist, specifications and constraints 
Data = sm132PortfolioData()

 
# ------------------------------------------------------------------------------
# Specification:      
Spec = portfolioSpec()
Constraints = NULL

ans1 = portfolioFrontier(data = Data, spec = Spec)


# ------------------------------------------------------------------------------
# Long only portfolio
Constraints = c(
     "minW[c(1:8)] = 0.0"
     )

ans2 = portfolioFrontier(data = Data, spec = Spec, constraintsString = Constraints)   


# ------------------------------------------------------------------------------
# constrained portfolio
Constraints = c(
     "minW[c(3, 4, 6, 7, 8)] = 0.0", 
     "maxW[1] = 0.39",
     "maxW[2] = 0.33",
     "maxW[3] = 0.27",
     "maxW[4] = 0.21",
     "maxW[5] = 0.31",
     "maxW[6] = 0.27",
     "maxW[7] = 0.23",
     "minsumW[1:4] = 0.4",
     "maxsumW[1:4] = 0.8", 
     "minsumW[5:8] = 0.1", 
     "maxsumW[5:8] = 0.5" )

ans3 = portfolioFrontier(data = Data, spec = Spec, 
    constraintsString = Constraints)  
     
# ------------------------------------------------------------------------------
par(mfrow = c(2, 2))
frontierPlot(ans1, cex = .5)
frontierPlot(ans2, add = TRUE, col = "red", pch = 12, cex = .5)
frontierPlot(ans3, add = TRUE, col = "green", pch = 14, cex = .5)

weightsPlot(ans1, legend = TRUE)
weightsPlot(ans2, legend = TRUE)
weightsPlot(ans3, legend = TRUE)
# ------------------------------------------------------------------------------
par(mfrow = c(2, 2))
frontierPlot(ans1, cex = .5)
frontierPlot(ans2, add = TRUE, col = "red", pch = 12, cex = .5)
frontierPlot(ans3, add = TRUE, col = "green", pch = 14, cex = .5)

attributesPlot(ans1, legend = TRUE)
attributesPlot(ans2, legend = TRUE)
attributesPlot(ans3, legend = TRUE)
