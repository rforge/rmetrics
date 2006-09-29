
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#  1999 - 2004, Diethelm Wuertz, GPL
#  Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#  info@rmetrics.org
#  www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#  see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#  see Rmetrics's copyright file


################################################################################
# REQUIRED PACKAGE:     
# systemfit
# FUNCTION:             SYSTEMFIT WRAPPER:
# 'fSYSTEM'             S4 Class Representation
# systemFit             Wrapper Function for "systemfit" and "sem" Models:
# * OLS                  Ordinary Least Squares
# * WLS                  Weighted Least Squares
# * SUR                  Seemingly Unrelated Regressions
# * 2SLS                 Two-Stage Least Squares
# * W2SLS                Weighted Two-Stage Least Squares
# * 3SLS                 Three-Stage Least Squares
# * W3SLS                Weighted Three-Stage Least Squares
# S3-METHODS:           DESCRIPTION:
# print.fSYSTEM         S3: Print method for an object of class fSYSTEM  
# ? plot 
# summary.fSYSTEM       S3: Summary method for an object of class fSYSTEM
# S3-METHODS:           DESCRIPTION:
# coef.fSYSTEM          S3: Method for coefficients
# coefficients.fSYSTEM  S3: Synonyme for coef.fSYSTEM
# fitted.fSYSTEM        S3: Method for fitted values
# residuals.fSYSTEM     S3: Method for residual values
# S3-METHODS:           DESCRIPTION:
# predict.fSYSTEM       S3: Prediction method for an object of class fSYSTEM
# FINMETRICS LIKE:      WRAPPER:
# SUR                   SUR Wrapper
################################################################################


### Uncomplete - Under Development ###


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(EquationsModelling); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.systemfitSUR =
function()
{
    require(systemfit)
    data(Kmenta)
    
    demand = consump ~ price + income
    supply = consump ~ price + farmPrice + trend
    labels = list("demand", "supply")
    system = list(demand, supply)
    restrm = matrix(0,1,7)   # restriction matrix "R"
    restrm[1,3] =  1
    restrm[1,7] = -1
    restr2m = matrix(0,2,7)  # restriction matrix "R" 2
    restr2q = matrix(0,2,1)  # restriction vector "q" 2
    restr2m[1,3] =  1
    restr2m[1,7] = -1
    restr2m[2,2] = -1
    restr2m[2,5] =  1
    restr2q[2,1] =  0.5
    tc = matrix(0, 7, 6)
    tc[1,1] = 1
    tc[2,2] = 1
    tc[3,3] = 1
    tc[4,4] = 1
    tc[5,5] = 1
    tc[6,6] = 1
    tc[7,3] = 1
    restr3m = matrix(0,1,6)  # restriction matrix "R" 2
    restr3q = matrix(0,1,1)  # restriction vector "q" 2
    restr3m[1,2] = -1
    restr3m[1,5] =  1
    restr3q[1,1] =  0.5
    
    # the standard equations do not converge and lead to a singular 
    # weighting matrix both in R and in EViews, since both equations 
    # have the same endogenous variable
    supply2 = price ~ income + farmPrice + trend
    system2 = list(demand, supply2)
    
    
    # SUR estimation*********
    fitsur1 = systemfit("SUR", system, labels, data = Kmenta)
    print(summary(fitsur1))
    print(round(fitsur1$bcov, digits = 6))
    
    # SUR (EViews-like) 
    fitsur1e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 0, probdfsys = TRUE)
    print(summary(fitsur1e))
    print(round(fitsur1e$bcov, digits = 6))
    
    # SUR (rcovformula = 2) 
    fitsur1c = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 2)
    print(summary(fitsur1c))
    print(round(fitsur1c$bcov, digits = 6))
    
    # SUR (rcovformula = 2, probdfsys = TRUE)
    fitsur1cp = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 2, probdfsys = TRUE)
    print(summary(fitsur1cp))
    print(round(fitsur1cp$bcov, digits = 6))
    
    # SUR (rcovformula = 3) 
    fitsur1c = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 3)
    print(summary(fitsur1c))
    print(round(fitsur1c$bcov, digits = 6))
    
    # SUR with cross-equation restriction 
    fitsur2 = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restrm)
    print(summary(fitsur2))
    print(round(fitsur2$bcov, digits = 6))
    
    # SUR with cross-equation restriction (EViews-like) 
    fitsur2e = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restrm, rcovformula = 0)
    print(summary(fitsur2e))
    print(round(fitsur2e$bcov, digits = 6))
    
    # SUR with restriction via TX 
    fitsur3 = systemfit("SUR", system, labels, data = Kmenta, 
        TX = tc)
    print(summary(fitsur3))
    print(round(fitsur3$bcov, digits = 6))
    
    # SUR with restriction via TX (EViews-like) 
    fitsur3e = systemfit("SUR", system, labels, data = Kmenta, 
        TX = tc, rcovformula = 0)
    print(summary(fitsur3e))
    print(round(fitsur3e$bcov, digits = 6))
    
    # SUR with 2 restrictions 
    fitsur4 = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restr2m, q.restr = restr2q)
    print(summary(fitsur4))
    print(round(fitsur4$bcov, digits = 6))
    
    # SUR with 2 restrictions (EViews-like) 
    fitsur4e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 0, R.restr = restr2m, q.restr = restr2q)
    print(summary(fitsur4e))
    print(round(fitsur4e$bcov, digits = 6))
    
    # SUR with 2 restrictions (rcovformula = 2) 
    fitsur4e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 2, R.restr = restr2m, q.restr = restr2q)
    print(summary(fitsur4e))
    print(round(fitsur4e$bcov, digits = 6))
    
    # SUR with 2 restrictions (rcovformula = 3) 
    fitsur4e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 3, R.restr = restr2m, q.restr = restr2q)
    print(summary(fitsur4e))
    print(round(fitsur4e$bcov, digits = 6))
    
    # SUR with 2 restrictions via R and TX*
    fitsur5 = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restr3m, q.restr = restr3q, TX = tc)
    print(summary(fitsur5))
    print(round(fitsur5$bcov, digits = 6))
    
    # SUR with 2 restrictions via R and TX (EViews-like) 
    fitsur5e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 0, R.restr = restr3m, q.restr = restr3q, 
        TX = tc)
    print(summary(fitsur5e))
    print(round(fitsur5e$bcov, digits = 6))
    
    # Iterated SUR 
    fitsuri1 = systemfit("SUR", system2, labels, data = Kmenta, 
        maxit = 100)
    print(summary(fitsuri1))
    print(round(fitsuri1$bcov, digits = 6))
    
    # Iterated SUR (EViews-like) 
    fitsuri1e = systemfit("SUR", system2, labels, data = Kmenta, 
        rcovformula = 0, probdfsys = TRUE, maxit = 100)
    print(summary(fitsuri1e))
    print(round(fitsuri1e$bcov, digits = 6))
    
    # Iterated SUR (rcovformula = 2) 
    fitsuri1c = systemfit("SUR", system2, labels, data = Kmenta, 
        maxit = 100, rcovformula = 2)
    print(summary(fitsuri1c))
    print(round(fitsuri1c$bcov, digits = 6))
    
    # Iterated SUR (rcovformula = 2, probdfsys = TRUE) 
    fitsuri1cp = systemfit("SUR", system2, labels, data = Kmenta, 
        rcovformula = 2, probdfsys = TRUE, maxit = 100)
    print(summary(fitsuri1cp))
    print(round(fitsuri1cp$bcov, digits = 6))
    
    # Iterated SUR (rcovformula = 3) 
    fitsuri1c = systemfit("SUR", system2, labels, data = Kmenta, 
        maxit = 100, rcovformula = 3)
    print(summary(fitsuri1c))
    print(round(fitsuri1c$bcov, digits = 6))
    
    # Iterated SUR with restriction 
    fitsuri2 = systemfit("SUR", system2, labels, data = Kmenta, 
        R.restr = restrm, maxit = 100)
    print(summary(fitsuri2))
    print(round(fitsuri2$bcov, digits = 6))
    
    # Iterated SUR with restriction (EViews-like)
    fitsuri2e = systemfit("SUR", system2, labels, data = Kmenta, 
        R.restr = restrm, rcovformula = 0, maxit = 100)
    print(summary(fitsuri2e))
    print(round(fitsuri2e$bcov, digits = 6))
    
    # Iterated SUR with restriction via TX 
    fitsuri3 = systemfit("SUR", system2, labels, data = Kmenta, 
        TX = tc, maxit = 100)
    print(summary(fitsuri3))
    print(round(fitsuri3$bcov, digits = 6))
    
    # Iterated SUR with restriction via TX (EViews-like)
    fitsuri3e = systemfit("SUR", system2, labels, data = Kmenta, 
        TX = tc, rcovformula = 0, maxit = 100)
    print(summary(fitsuri3e))
    print(round(fitsuri3e$bcov, digits = 6))
    
    # Iterated SUR with 2 restrictions 
    fitsuri4 = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restr2m, q.restr = restr2q, maxit = 100)
    print(summary(fitsuri4))
    print(round(fitsuri4$bcov, digits = 6))
    
    # iterated SUR with 2 restrictions (EViews-like) 
    fitsuri4e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 0, R.restr = restr2m, q.restr = restr2q, 
        maxit = 100)
    print(summary(fitsuri4e))
    print(round(fitsuri4e$bcov, digits = 6))
    
    # Iterated SUR with 2 restrictions via R and TX*
    fitsuri5 = systemfit("SUR", system, labels, data = Kmenta, 
        R.restr = restr3m, q.restr = restr3q, TX = tc, maxit = 100)
    print(summary(fitsuri5))
    print(round(fitsuri5$bcov, digits = 6))
    
    # Iterated SUR with 2 restrictions via R and TX (EViews-like) 
    fitsuri5e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 0, R.restr = restr3m, q.restr = restr3q, TX = tc, 
        maxit = 100)
    print(summary(fitsuri5e))
    print(round(fitsuri5e$bcov, digits = 6))
    
    # Iterated SUR with 2 restrictions via R and TX (rcovformula = 2) 
    fitsuri5e = systemfit("SUR", system, labels, data = Kmenta, 
        rcovformula = 2, R.restr = restr3m, q.restr = restr3q, TX = tc, 
        maxit = 100)
    print(summary(fitsuri5e))
    print(round(fitsuri5e$bcov, digits = 6))
    
    # Iterated SUR with 2 restrictions via R and TX (rcovformula=3) 
    # fitsuri5e = systemfit("SUR", system, labels, data = Kmenta, 
    #   rcovformula = 3, R.restr = restr3m, q.restr = restr3q, 
    #   TX = tc, maxit = 100)
    # print(summary(fitsuri5e))
    # print(round(fitsuri5e$bcov, digits = 6))
    # disabled, because the estimation does not converge

    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.systemfit.ols = 
function()
{    
    demand = consump ~ price + income
    supply = consump ~ price + farmPrice + trend
    
    ## OLS Estimations:   
    formula = list(demand = demand, supply = supply)
    fitOls = systemFit(formula, data = Kmenta)
    print(fitOls)
    summary(fitOls)
    
    ## OLS Estimation with 2 Restrictions:
    Rrestr = matrix(0, 2, 7)
    qrestr = matrix(0, 2, 1)
    Rrestr[1,3] =  1
    Rrestr[1,7] = -1
    Rrestr[2,2] = -1
    Rrestr[2,5] =  1
    qrestr[2,1] =  0.5
    FITOLS2 = systemFit(formula, data = Kmenta, R.restr = Rrestr, 
     q.restr = qrestr)
    FITOLS2
    
    ## Iterated SUR Estimation:
    FITSUR = systemFit(formula, data = Kmenta, method = "SUR", maxit = 100)
    FITSUR
    # Coefficients, Fitted Values, Residuals and Variance-Covariance Matrix:
    # Call by Method:
    coef(FITSUR)
    fitted(FITSUR)
    residuals(FITSUR)
    
    ## 2SLS Estimation:
    inst = ~ price + farmPrice + trend
    FIT2SLS = systemFit(formula, data = Kmenta, method = "2SLS", inst = inst)
    FIT2SLS
    # Coefficients, Fitted Values, Residuals and Variance-Covariance Matrix:
    # Call by Slot:
    FIT2SLS@fit$coef
    FIT2SLS@fitted.values
    FIT2SLS@residuals
    
    ## 2SLS Estimation with Different Instruments in Each Equation:
    insts = list(~ price + farmPrice, ~ price + farmPrice + trend)
    FIT2SLS2 = systemFit(formula, data = Kmenta, method = "2SLS", inst = insts)
    print(FIT2SLS2)
    
    ## 3SLS Estimation with GMM-3SLS Formula:
    instruments = ~ price + farmPrice + trend
    FIT3SLS = systemFit(formula, data = Kmenta, method = "3SLS", 
     inst = instruments, formula3sls = "GMM")
    print(FIT3SLS)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.nlsystemFit = 
function()
{  
   
    data(ppine)

    # OLS:
    hg.formula = hg ~ exp(h0 + h1*log(tht) + h2*tht^2 + h3*elev + h4*cr)
    dg.formula = dg ~ exp(d0 + d1*log(dbh) + d2*hg + d3*cr + d4*ba )
    formula = list(hg.formula, dg.formula)
    labels = list("height.growth", "diameter.growth")
    inst = ~ tht + dbh + elev + cr + ba
    start = c(
        h0 = -0.5, h1 = 0.500, h2 = -0.001, h3 = 0.0001, h4 =  0.08,
        d0 = -0.5, d1 = 0.009, d2 =  0.250, d3 = 0.0050, d4 = -0.02)
    fit = nlsystemFit(formula, data = ppine, method = "OLS",  
        start = start, eqnlabels = labels)
    
    print(fit)
    
    summary(fit)
    
    coef(fit)
    fitted(fit)    # <-
    residuals(fit)
    
    # From Slots:
    fit@fit$coef
    fit@fitted.values
    fit@residuals
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit3A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
