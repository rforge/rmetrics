

portfolioTest <-  
function(
    type = "MV", 
    optimize = "minRisk",
    solver = "solveRquadprog", 
    constraints = c("LongOnly", "Short", "BoxGroup"),
    NFrontierPoints = 25)
{
    # Example:
    
    #   portfolioTest("MV",   "minRisk",   "solveRquadprog",   "LongOnly")
    #   portfolioTest("MV",   "minRisk",   "solveRquadprog",   "BoxGroup")
    
    #   portfolioTest("MV",   "minRisk",   "solveRshortExact", "Short")
    
    #   portfolioTest("MV",   "minRisk",   "solveRipop",       "LongOnly")
    #   portfolioTest("MV",   "minRisk",   "solveRipop",       "BoxGroup")
    
    #   portfolioTest("MV",   "minRisk",   "solveRdonlp2",     "LongOnly")
    #   portfolioTest("MV",   "minRisk",   "solveRdonlp2",     "BoxGroup")
    
    #   portfolioTest("MV",   "maxReturn", "solveRsocp",       "LongOnly")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolve",    "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolve",    "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolveAPI", "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolveAPI", "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRsymphony",   "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRsymphony",   "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRglpk",       "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRglpk",       "BoxGroup")
    
    #   type="VaR"; optimize="minRisk"; solver="solveRlpSolveAPI";constraints="LongOnly";NFrontierPoints=25
    
    # FUNCTION:
    
    Start =  Sys.time()
    
    # Data - Take care of proper scaling:
    data = 100 * as.timeSeries(data(LPP2005REC))[, 1:6]
    Data = portfolioData(data) 
    
    # Constraints:
    if(constraints == "BoxGroup") {
        constraints = c(
            "minW[3:4]=0.1",    "maxW[5:6]=0.9", 
            "minsumW[1:3]=0.2", "maxsumW[c(2,4)]=0.8") 
    }
    
    # Settings:
    spec <- portfolioSpec()
    setType(spec) = type
    setOptimize(spec) = optimize
    setSolver(spec) = solver
    setTargetReturn(spec) = mean(data)
    setNFrontierPoints(spec) = NFrontierPoints
    
    if (optimize == "maxReturn") {
        setTargetRisk(spec) = mean(colSds(data))
        print(getTargetRisk(spec))
        fp = efficientPortfolio(data, spec, constraints)
        out = capture.output(fp)
        cat("\n")
        print(out[c(3:7, 11, 15, 19)], quote = FALSE)
        return(invisible())
    }
   
    # Equal Weights Feasible Portfolio:
    fp = feasiblePortfolio(data, spec, constraints)
    out = capture.output(fp)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
       
    # Minimum Risk Efficient Portfolio I:
    solveRfun = match.fun(solver)
    sol = solveRfun(data, spec, constraints)
    sol$weights
    sol$objective
    
    # Efficient Portfolio II:
    ep = efficientPortfolio(data, spec, constraints)
    out = capture.output(ep)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
    
    # Max Reward/Risk Ratio Portfolio:
    tg = maxratioPortfolio(data, spec, constraints)
    out = capture.output(tg)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
       
    # Minimum Risk Portfolio:
    mv = minriskPortfolio(data, spec, constraints)
    out = capture.output(mv)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
    
    # Efficient Frontier:
    pf = portfolioFrontier(data, spec, constraints)
    cat("\n")
    print(pf) 
    
    
    # Efficient Frontier Plot: 
    print("Plot ...")
    if (type == "MV") x.max = 0.08
    if (type == "CVaR") x.max = 0.0175
    frontierPlot(pf) #, xlim = c(0, x.max))
    minvariancePoints(pf, pch = 19, col = "red")
    tangencyPoints(pf, pch = 19)
    tangencyLines(pf, col = "blue")
    points(0, 0, pch = 19, col = "blue")
    equalWeightsPoints(pf, col = "blue")
    singleAssetPoints(pf, col = rainbow(6), pch = 15)
    # twoAssetsLines(pf, col = "grey")
    sharpeRatioLines(pf)
    # monteCarloPoints(pf, 10000, cex = 0.25)
    
    # LongOnly Interactive Plot:
    # plot(pf)
    
    cat("\n")
    print(Sys.time()-Start)
    cat("\n")
   
    invisible()
} 



