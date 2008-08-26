
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA


################################################################################
# FUNCTION:                     DESCRIPTION:
#  portfolioTest                
################################################################################


portfolioTest <-  
function(
    type = "MV", 
    optimize = "minRisk",
    solver = "solveRquadprog", 
    constraints = c("LongOnly", "Short", "BoxGroup", "CovBudget", "130/30"),
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
    #   portfolioTest("MV",   "minRisk",   "solveRdonlp2",     "CovBudget")
    
    #   portfolioTest("MV",   "maxReturn", "solveRsocp",       "LongOnly")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolve",    "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolve",    "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolveAPI", "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRlpSolveAPI", "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRsymphony",   "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRsymphony",   "BoxGroup")
    
    #   portfolioTest("CVaR", "minRisk",   "solveRglpk",       "LongOnly")
    #   portfolioTest("CVaR", "minRisk",   "solveRglpk",       "BoxGroup")
    
    #   type="MV"; optimize="minRisk"; solver="solveRdonlp2";constraints="BoxGroup";NFrontierPoints=25
    
    # FUNCTION:
    
    Start = Sys.time()
    Constraints = match.arg(constraints)
    if(Constraints == "BoxGroup") constraints = .BoxGroup
    if(Constraints == "CovBudget") constraints = .CovBudget
    Subtitle = paste(type, "|", optimize, "|", Constraints)
    
    # Data - Take care of proper scaling:
    data = 100 * as.timeSeries(data(LPP2005REC))[, 1:6]
    Data = portfolioData(data)     
    
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
    fp.Risk = getTargetRisk(fp)[,c(1,3)]
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
    ep.Risk = getTargetRisk(ep)[,c(1,3)]
    if (type == "MV" | type == "MAD") epPoint = c(ep.Risk[1], mean(data)) 
    if (type == "CVaR") epPoint = c(ep.Risk[2], mean(data)) 
       
    # Max Reward/Risk Ratio Portfolio:
    tg = maxratioPortfolio(data, spec, constraints)
    out = capture.output(tg)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
    tg.Risk = getTargetRisk(tg)[,c(1,3)]
       
    # Minimum Risk Portfolio:
    mv = minriskPortfolio(data, spec, constraints)
    out = capture.output(mv)
    cat("\n")
    print(out[c(3:7, 11, 15, 19)], quote = FALSE)
    mv.Risk = getTargetRisk(mv)[,c(1,3)]
    
    Risk = round(rbind(fp.Risk, ep.Risk, tg.Risk, mv.Risk), 4)
    rownames(Risk) = c("Equal Wts", "Grand Mean", "R/R Ratio", "Min Risk")
    riskText = capture.output(Risk)
    
    # Efficient Frontier:
    pf = portfolioFrontier(data, spec, constraints)
    cat("\n")
    print(pf) 
      
    # Efficient Frontier Plot: 
    print("Plot ...")
    if (type == "MV" | type == "MAD") {
        xlim = c(0, 0.8)
        offset = 0.03
    }
    if (type == "CVaR") {
        xlim = c(0, 2)
        offset = 0.075
    }
    frontierPlot(pf, ylim = c(-0.01, 0.09), xlim = xlim)
    mtext(Subtitle, 3, 0.5, font = 2, cex = 0.8)
    grid()
    minvariancePoints(pf, pch = 19, col = "red")
    tangencyPoints(pf, pch = 19)
    tangencyLines(pf, col = "blue")
    points(0, 0, pch = 19, col = "blue")
    coord = equalWeightsPoints(pf, pch = 22, col = "darkgrey")
    text(coord[,1]+offset, coord[,2], "EW", font = 2, cex = 0.7)
    points(epPoint[1], epPoint[2], pch = 19, col = "magenta", cex = 0.8)
    coord = singleAssetPoints(pf, col = rainbow(6), pch = 15)
    text(coord[,1]+offset, coord[,2], rownames(coord), font = 2, cex = 0.7)
    # twoAssetsLines(pf, col = "grey")
    sharpeRatioLines(pf, col = "brown")
    # monteCarloPoints(pf, 10000, cex = 0.25)
    
    # Risk Text:
    text(offset, seq(0.088, 0.072, length=5), riskText, adj = 0, 
        cex = 0.7, font = 11)
    
    # LongOnly Interactive Plot:
    # plot(pf)
    
    cat("\n")
    Elapsed = capture.output(Sys.time()-Start)
    print(Elapsed)
    mtext(Elapsed, 1, 4, adj = 1, cex = 0.7)
    cat("\n")
   
    ans = list(fp = fp, ep = ep, tg = tg, mv = mv, pf = pf)
    invisible(ans)
} 


################################################################################

