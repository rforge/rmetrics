
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
    
   
frontierResampled = 
function(x = as.matrix(read.csv("nova-updated.csv", header = FALSE)),
expectedReturns = read.csv("expected-returns.csv"), frequency = 52,
control = list(nFrontier = 200, returnChoice = 2, covarianceChoice = 2,
targetChoice = 6, correlationChoice = 1), confidenceLevel = TRUE, 
trace = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - any rectangular multivariate time series object which can be
    #       transformed into a matrix throught the function 'as.matrix'.  
    #   expectedReturns - expected returns
    #   frequency - frequency of time series records. An integer value, by 
    #       default 52 which denotes weekly data records.
    #   control - a control list with the following entries: ...
    #       nFrontier
    #       returnChoice
    #       covarianceChoice
    #       targetChoice
    #       correlationChoice
    #   confidenceLevel - a logical flag, if TRUE then 95% confidence 
    #       levels are returned, otherwise not. By default TRUE.
    #   trace - a logical if TRUE then the portfolio resampling process
    #       will be ttraced, otherwise not. By default TRUE.
    
    # Value:
    
    # Note:
    #   solve.QP is needed !
    
    # Author:
    #   This code was written by Lorenzo Isella (2005), Copyright GPL
    
    # FUNCTION:
    
    # Default Settings:
    #   niter = by default 200
    #   rtnchoice = by default 2 
    #   choicecov = by default 2
    #   nupper = by default 0
    #   nlower = by default 0
    #   limupper = by default c(1,20)
    #   maxweight = by default c(0.00001, 1)
    #   target = by default 6 
    #   correlation = by default 1
    #   confidence = by default 0
    #   alpha = by default 0.9
    
    # Plot:
    doplot = TRUE
    
    # I read the matrix containing the historical returns the historical 
    # returns of the i-th asset are the i-th column of the matrix
    rtnmat = x
    vecdim = dim(rtnmat)
    
    # Extract the length of the sequence of random numbers I need to use
    # to simulate the life of the historical ptfs
    nlength = vecdim[1] 
   
    # Now we start dealing with a part in which I have to make some choices
    # Number of efficient frontiers I am going to Monte-Carlo simulate
    niter = control$nFrontier # 200
    
    # Now I save the historical covariance matrix
    histcovmat = cov(rtnmat)
    
    # Now I load a library I will need to run the portfolio.optim package 
    # (needed only in order to generate the efficient frontier for the 
    # random ptfs)
    # require(tseries)
    # I fix the number of assets my ptf is made up of
    nassets = vecdim[2]
    
    # Now I choose whether I want to use the historical rtns of something else
    # rtnchoice = 0 -> historical rtns
    # rtnchoice = 1 -> expected rtns to be read from a file 
    # rtnchoice = 2 -> shrinkage of the historical expected rtns
    # (James-Stein estimator)
    rtnchoice = control$returnChoice # 2
                  
    # Now I introduce some options about the covariance matrix I am going 
    # to use the choices are:
    # 0 for the historical cov matrix
    # 1 for the constant correlation model (the non-diagonal elements are
    #   assumed to be constant; empirically it works in many cases)
    # 2 for the shrinkage of the historical covariance matrix with 
    #   different targets (bayesian priors are used to shrink
    #   the historical covariance matrix; see paper by Oliver Ledoit)
    choicecov = control$covarianceChoice # 2
    
    # Now I specify the number of upper and lower constrains (different 
    # from the no SS). I want to use on the asset weights, 
    # if set to zero, like in this case, then there is not anything
    # like a maximum weight for a specific asset
    nupper = 0
    
    # basically the lower constrains I have are only to forbid the SS 
    nlower = 0 
    
    # later on we will use it when setting up the optimization problem
    bvectemp = seq(1:(nassets+1+nupper))
    
    # the following part between two lines of #s is effective only if 
    # nupper is different from zero (otherwise it does not affect the 
    # final results)
  
    # Now I enter as a column vector the assets I want to limit
    # it contains the assets (in this case the 1st and 20th ones 
    # of my portfolio) whose weights I want to limit I need at least 
    # two weights to limit [technicality of the code]; in case I want 
    # to limit only one, I can choose a maxweight = 1 for the second
    limupper = c(1, 20)
    
    # like I did above, I now enter the maximum allowed weight for the 
    # limited assets the weights have to match the assets given above
    # vector containing the limits on the portfolio weights
    # NB:the order here as to match the one in limupper!!!!!!    
    maxweight = c(0.00001, 1)
    
    # the previous choice of maxweight means I am simply taking away 
    # the 1st asset in my portfolio 
    # NB:for numerical reasons, choose a very small value for an asset 
    # you want to take away from your portfolio, but never set it its 
    # weight exactly equal to zero.

    # Now I need to choose which target I am using for the shrinkage of 
    # the covariance matrix
    #  1: diagonal, unit variance
    #  2: diagonal, common variance
    #  3: diagonal, common (co)variance
    #  4: diagonal, unequal variance
    #  5: perfect positive correlation
    #  6: constant correlation
    target = control$targetChoice # 6 
    
    # In the following I use some brutal approximation:the assets are 
    # distributed according to a Gaussian multivariate (this can be 
    # improved in some later version by using some copula #techniques; 
    # though some working papers by Michaud's entourage suggest that 
    # only extreme deviations from a Gaussian multivariate are relevant 
    # for realistical portfolios).
    # NB:in the following I will talk about SIMULATED PORTFOLIOS to mean 
    # that I Monte-Carlo simulate the returns of the assets and 
    # I then optimize the portfolio according to the simulated data
    # I will then Monte-Carlo simulate some sequences of returns for the 
    # assets of my portfolio according to a multivariate statistics.
    # correlation = 1 -> Gaussian correlated multivariate 
    # correlation = 0 -> Gaussian uncorrelated multivariate   
    correlation = control$correlationChoice # 1
    
    # The following two lines implement the confidence level of the portfolio.
    # as intended by Michaud in his book. It is the same idea as in Jobson-
    # Korkie experiment:many of the statistically equivalent portfolios to 
    # the optimized portfolios are quite far from Markowitz efficient frontier. 
    # I work  a piecewise linear function
    # containing a fraction alpha (i.e. 90% in this case) of the simulated 
    # portfolios countinG them from the efficient frontier of course.
    # This is quite time-consuming, so the default is NOT to work all this out. 
    # At this point I have the collected rtns of the simulated ptfs
    # confidence = 1 -> I compute the confidence level
    # confidence = 0 -> I do not compute the confidence level 
    confidence = confidenceLevel # 1 #0
    alpha = 0.9
    

    # END OF PART DEALING WITH THE OPTIONS TO SET UP THE OPTIMIZATION PROBLEM
    
    # NEW SECTION::
    # The vector "assetmeans" will be containing in general the expected 
    # rather than the historical rtns of the assets.
    assetmeans  = seq(1:nassets)
    assetmeans1 = seq(1:nassets)
    assetmeans0 = seq(1:nassets)
    assetmeans2 = seq(1:nassets)
    # The vector assettemp is now used simply as a box to read the file 
    # containing the expected rtns of the ptf. Careful about how the file 
    # is written!
    # I removed a column for the expected rtns in the file which is read here
    assettemp = expectedReturns
    for ( i in 1:nassets) {
        # Vector containing the expected rtns provided by the wealth manager 
        assetmeans1[i] = assettemp[i, 1]
    }                                                         
    # If the previous line does not work, then try this:
    # for ( i in 1:nassets) {assetmeans[i] = assettemp[i, 2]}
    
    # Now I save the vector of the historical rtns
    for (i in 1:nassets) {
        assetmeans0[i] = mean(rtnmat[, i])
    }
    
    # Now I perform the shrinking of the historical expected rtns
    musub0 = seq(1:nassets)
    # vector made up of the historical expected rtns
    musub0[ ] = mean(assetmeans0)
    newrtnvec = assetmeans0-musub0
    shrinkmean = ((nassets-2)/nlength) /
        (newrtnvec%*%solve(histcovmat,newrtnvec))
    # This is the shrinkage constant for the historical returns.
    shrinkmean = min(1, shrinkmean)
    if (trace) {
        print("the shrinkage factor for the mean is")
        print(shrinkmean)
    }
    # Now I create the vector of the shrunk rtns
    assetmeans2 = shrinkmean*musub0+(1-shrinkmean)*assetmeans0
     
    # In the following part I choose whether I want to use the expected 
    # rtns or the historical returns or the shrunk rtns
    if (rtnchoice == 0) {
        assetmeans = assetmeans0
    }
    # OK! this is all consistent with the value
    if (rtnchoice == 1) {
        assetmeans = assetmeans1
    }
    # of rtnchoice
    if (rtnchoice == 2) {
        assetmeans = assetmeans2
    }

    # Now I introduce the vectors which will contain the expected asset 
    # returns and the portfolio return at each iteration
    # Number of points I use to create a grid of returns ranging 
    # from the minimum variance (MV) portfolio to the one with the
    # maximum expected rtn
    Npoints = 400
    
    # Now I redefine the number of pts I am going to use
    # number of points along the grid counted from the MV portfolio
    # In the simulation, I can choose not to work out the efficient 
    # frontier entirely (i.e. up to the maximum return portfolio) 
    Npointseff = 200                  
    # This array will contain the optimized returns along each 
    # efficient frontier.
    stochaexprtn = seq(1:(Npointseff+1))

    # Now I also introduce the arrays I will use to collect the std and 
    # rtns of the simulated ptfs
    if (confidence == 1) {
        JKrtn = seq(1:niter*(Npointseff+1))
        JKstd = seq(1:niter*(Npointseff+1))
    }
   

    # NEW SECTION:
    # after reading the historical data, I start to perform some operations 
    # on them. I need to get the the historical correlation matrix first.
    # Now I have the historical correlations
    histcormat = cor(rtnmat)
    # The next step in order to implement the constant correlation model is 
    # to replace the off-diagonal elements of the correlation matrix with 
    # their average value. 
    # First I initialize it
    constcor = 0.0
    numberelement = 0
    for (i in 1:nassets) {
        for (j in 1:nassets) {
            if (i != j){
                constcor = histcormat[i, j]+constcor}
            }
    }
    constcor = constcor / (nassets*(nassets-1.0))
    # Now I have the constant off-diagonal element of the correlation matrix.
    # At this point I need to create the constant covariance matrix.
    constcovmat = matrix(nrow = nassets, ncol = nassets)
    # I create the whole covariance matrix using the const I worked out above 
    for (i in 1:nassets) {
        for (j in 1:nassets) { 
            constcovmat[i, j] = sqrt(histcovmat[i, i] *
                histcovmat[j, j]) * constcor
        }
    }
    # Now I redefine the diagonal elements [I want them to be equal to 
    # those of the historical covariance matrix]
    for (i in 1:nassets) {
        constcovmat[i, i] = histcovmat[i, i]
    }
    # Now I have the constant covariance matrix

    
    # NEW SECTION:
    # Now I implement the formula of the shrinkage of the covariance matrix
    # Now I need to define a few parameters
    # A = sum(var(sqrt(nlength)*histcovmat))
    # B = sum(cov(sqrt(nlength)*histcovmat,sqrt(nlength)*constcovmat))
    # C = sum((histcovmat-constcovmat)*(histcovmat-constcovmat))
    # Now I can express the shrinkage constant
    # shrinkdelta = 1/nlength*(A-B)/C
    # if (trace) print("the shrinkage constant for the covariance matrix is")
    # if (trace) print(shrinkdelta)
    # First I'll define wsubkij
    wsubkij = array(dim=c(nlength,nassets,nassets))
    if (trace) print("ok wsubkij")
    # Now I need to calculate this array
    for (k in 1:nlength) {
        for (i in 1:nassets) {
            for (j in 1:nassets) {
                wsubkij[k, i, j] = (rtnmat[k, i]-mean(rtnmat[, i])) *
                    (rtnmat[k, j]-mean(rtnmat[, j]))
            }        
        }
    }
    wbarsubij = matrix(ncol=nassets,nrow=nassets)
    # Now I need to calculate this array
    columntemp = seq(1:nlength)
    for (i in 1:nassets) {
        for (j in 1:nassets) {
            columntemp = wsubkij[, i, j]          
            wbarsubij[i, j] = sum(columntemp)
        }
    }
    # Now I have the other array I possibly need
    wbarsubij = wbarsubij/nlength
    # Now a test
    covsubij = matrix(ncol=nassets,nrow=nassets)
    for (i in 1:nassets) {
        for (j in 1:nassets) {
            covsubij[i, j] = wbarsubij[i, j]*(nlength/(nlength-1))
        }
    }
    # alright, I get again histcovmat=cov(rtnmat)
    # there are other two important arrays which I need to work out
    vars = matrix(nrow=nassets,ncol=nassets)
    
    for (i in 1:nassets) {
        for ( j in 1:nassets) {
            columntemp = (wsubkij[, i, j]-wbarsubij[i, j]) *
                (wsubkij[, i, j]-wbarsubij[i, j])
            vars[i, j] = sum(columntemp)
        }
    }
    statfactor = nlength/(nlength-1)^3
    vars = vars*statfactor
    # Now a test
    columntemp = (wsubkij[, 1, 1]-wbarsubij[1, 1]) * 
        (wsubkij[, 1, 1] - wbarsubij[1, 1])
    vars11 = statfactor*sum(columntemp)
    numbertest = 0
    for (k in 1:nlength) {
        numbertest = numbertest+(wsubkij[k, 1, 1]-wbarsubij[1, 1])^2
    }
    numbertest = numbertest*statfactor
    # ok! both vars11 and numbertest are equal to vars[1, 1]
    #
    # Now I have to introduce another array
    covsubijlm = array(dim=c(nassets,nassets,nassets,nassets))
    columntemp2 = seq(1:nlength)
    for (i in 1:nassets) {
        for (j in 1:nassets) {
            for (l in 1:nassets) {
                for (m in 1:nassets) {
                    columntemp = wsubkij[, i, j]
                    columntemp2 = wsubkij[,l,m]
                    covsubijlm[i, j,l,m] = sum((columntemp-wbarsubij[i, j]) *
                        (columntemp2-wbarsubij[l,m]))
                }
            }
        }
    }
    # its diagonal elements are the same as the diagonal 
    # elements of vars, which is a simple test to perform
    covsubijlm = statfactor*covsubijlm
    # Now I can estimate the shrinkage constant lambdastar
    targetcov = matrix(ncol=nassets,nrow=nassets)   
    # CASE WHEN THE TARGET IS 1   
    if (target == 1) {
        targetcov = diag(nassets)
        lambdanum = sum(vars)
        lambdaden1 = 0
        lambdaden2 = 0
        for(i in 1:nassets){
            for (j in 1:nassets){
                if (j != i) {
                    lambdaden1 = lambdaden1 + (histcovmat[i, j])^2
                }
                if (j == i) {
                    lambdaden2 = lambdaden2 + (histcovmat[i, j]-1)^2
                }
            }
        }
        lambdastar = lambdanum/(lambdaden1+lambdaden2)
        if (trace) {
            print("lambdastar is")
            print(lambdastar)
        }
    } 
    # CASE WHEN THE TARGET IS 2 
    if (target ==2 ) {
        nu = mean(diag(histcovmat))
        targetcov = diag(nassets)*nu
        lambdanum = sum(vars)
        lambdaden1 = 0
        lambdaden2 = 0
        for(i in 1:nassets){
            for (j in 1:nassets){
                if (j != i) {
                    lambdaden1 = lambdaden1 + (histcovmat[i, j])^2
                }
                if (j == i) {
                    lambdaden2 = lambdaden2 + (histcovmat[i, j]-nu)^2
                }
            }
        }
        lambdastar = lambdanum/(lambdaden1+lambdaden2)
        if (trace) {
            print("lambdastar is")
            print(lambdastar)
        }
    }    
    # CASE WHEN THE TARGET IS 3 
    if (target == 3) {
        nu = mean(diag(histcovmat))
        targetcov = diag(nassets)*nu
        c = sum(histcovmat-diag(histcovmat))
        # Now c provides me with the average of the off-diagonal elements
        # of he historical covariance matrix.
        c = c/(nassets^2-nassets) 
        for (i in 1:nassets) {
            for (j in 1:nassets) {    
                if (j != i) {
                    targetcov[i, j] = c    
                }
            }
        }
        lambdanum = sum(vars)
        lambdaden1 = 0
        lambdaden2 = 0
        for(i in 1:nassets){
            for (j in 1:nassets){
                if (j != i) {
                    lambdaden1 = lambdaden1 + (histcovmat[i, j]-c)^2
                }
                if (j == i) {
                    lambdaden2 = lambdaden2 + (histcovmat[i, j]-nu)^2
                }
            }
        }
        lambdastar = lambdanum / (lambdaden1+lambdaden2)
        if (trace)  {
            print("lambdastar is")
            print(lambdastar)
        }
    }    
    # CASE WHEN THE TARGET IS 4
    if (target == 4) {
        targetcov = diag(nassets)
        for (i in 1:nassets) {
            targetcov[i, i] = histcovmat[i, i]
        }
        lambdanum = 0
        lambdaden = 0
        for (i in 1:nassets) {
            for (j in 1:nassets)  {
                if ( j != i)    {
                    lambdanum = lambdanum + vars[i, j]
                    lambdaden = lambdaden + histcovmat[i, j]^2
                }       
            }    
        }    
        lambdastar = lambdanum/lambdaden
        if (trace) {
            print("lambdastar is")
            print(lambdastar)
        }
    }  
    # CASE WHEN THE TARGET IS 5
    if (target == 5) {
        targetcov = diag(nassets)
        for ( i in 1:nassets) {
            for (j in 1:nassets) {
                if (j == i) {
                    targetcov[i, j] =  histcovmat[i, j]
                }    
                if (j !=i) {
                    targetcov[i, j] = sqrt(histcovmat[i, i]*histcovmat[j, j])
                }
            }
        }
        fsubij = diag(nassets)
        for ( i in 1:nassets) {
            for (j in 1:nassets)  {
                fsubij[i, j] = 0.5*(sqrt(histcovmat[j, j]/histcovmat[i, i]) *
                    covsubijlm[i, i, i, j] + sqrt(histcovmat[i, i] /
                    histcovmat[j, j])*covsubijlm[j, j, i, j])
            }
        }
        print("ok so far") 
        lambdanum = 0
        lambdaden = 0
        for (i in 1:nassets) {
            for (j in 1:nassets) {
                if ( j != i) {
                    lambdanum =  lambdanum + vars[i, j] - fsubij[i, j]
                    lambdaden = lambdaden + (histcovmat[i, j] -
                        sqrt(histcovmat[i, i]*histcovmat[j, j]))^2
                }
            }
        }            
        lambdastar  =  lambdanum / lambdaden 
        if (trace) {
            print("lambdastar is")
            print(lambdastar)   
        } 
    }    
    # CASE WHEN THE TARGET IS 6  
    if (target == 6) {   
        targetcov = constcovmat   
        fsubij = diag(nassets)
        for ( i in 1:nassets) {
            for (j in 1:nassets)  {
                fsubij[i, j] = 0.5*(sqrt(histcovmat[j, j]/histcovmat[i, i]) *
                    covsubijlm[i, i, i, j] + sqrt(histcovmat[i, i] /
                    histcovmat[j, j])*covsubijlm[j, j, i, j])  
            }
        }
        lambdanum = 0
        lambdaden = 0   
        for ( i in 1:nassets) {
            for (j in 1:nassets) {
                if (j != i) {
                    lambdanum = lambdanum + vars[i, j] - constcor*fsubij[i, j]
                    lambdaden =  lambdaden+ (histcovmat[i, j]-targetcov[i, j])^2
                }
            }
        }   
        lambdastar  =  lambdanum / lambdaden
        if (trace) {
            print("lambdastar is")
            print(lambdastar)
        }
    }
  
     
    # NEW SECTION:   
    # Now I have all the data for the optimization problem.
    # I determine the min var ptf without any stochastic sampling
    # (pure mean variance problem)
    # First, I need to know the rtn of the min var ptf. I obtain it 
    # using the solve.QP routine directly. See its documentation 
    # [help(solve.QP)]. 
    # Now I choose the kind of covariance matrix I want to use
    if (choicecov == 1) {
        Dmat = constcovmat
    }
    if (choicecov == 0) { 
        Dmat = histcovmat
    }
    if (choicecov == 2) {
        # shrinkage of the historical covariance matrix.
        Dmat =  lambdastar*targetcov + (1-lambdastar)*histcovmat
    }
    dvec2 = seq(1:nassets)
    # I do not have any linear extra term in the function I want to minimize
    # which is simply the ptf variance
    dvec2[ ] = 0.0 
    # it is better to express dvec as a column vector, but solve.QP
    # is smart enough to understand the quadratic problem even if
    # dvec is a row    
    dvec = t(dvec2)
    # this is the matrix containing the linear constrains 
    Atrasp=matrix(ncol = nassets, nrow = (nassets+1)) 
    # the identity matrix will be used to impose that
    # the vector weights are >=0
    tempmat = diag(nassets)
    # this 1st row is needed to have \sum wsubi=1 
    Atrasp[1, ] = 1.0 
    Atrasp[2:(nassets+1), ] = tempmat
    # Now I create another matrix which will be used to deal with the 
    # extra constrain I impose on the ptf weights
    # Now I deal with the case in which I have some extra constraints 
    # on the asset weights
    if (nupper != 0) {
        constrainmat = matrix(ncol = nassets, nrow = nupper)
        constrainmat[, ] = 0.0
        for (h in 1:nupper) { 
            constrainmat[h, limupper[h]] = -1.0  
        }
        # I include the extra constrains in Amat
        Amat = t(rbind(Atrasp,constrainmat))  
        bvec2 = seq(1:(nassets+1+nupper))
        # used to impose that wsubi >=0 for all i 
        bvec2[ ] = 0.0
        # used to impose that \sumsubi wsubi=1
        bvec2[1] = 1.0
        for (h in 1:nupper) {
            bvec2[nassets+1+h] = -maxweight[h]
        }
    }
    # Now the case in which the portfolio weights are not constrained
    if (nupper == 0) {
        Amat = t(Atrasp)
        # since solve.QP actually wants the transpose of the matrix Atrasp 
        # this vector is also needed to implement the constrains
        # I need to get the min var ptf 
        bvec2 = seq(1:(nassets+1))
        # used to impose that wsubi >=0 for all i
        bvec2[ ] = 0.0
        # used to impose that \sumsubi wsubi=1 
        bvec2[1] = 1.0
        # I need to use a column vector here, but solve.QP understands 
        # it anyway
        dim(bvec2) = c((nassets+1), 1) 
    }                             
    # to impose the fact that the only equality constrain is the 1st 
    # one i.e. the one on the sum of the ptf weights
    meq = 1
    if (trace) print("ok so far")
    # finally! this calls to the solve.QP routine works out the 
    # min var ptf
    ### We use here the Rmetrics builtin .solve.QP --
    resminvar = .solve.QP(Dmat, dvec, Amat, bvec2, meq = 1)
    if (trace) print("ok so far2")
    # composition of the min var ptf
    minvarweights = resminvar$solution
    # Now I choose the min and max rtns I will be using to delimit the 
    # efficient frontieR I want to calculate.      
    # rtn of the min var ptf
    minval = minvarweights%*%assetmeans
    # highest expected rtn in my assets
    maxval = max(assetmeans)
    # actually the classical efficient frontier is calculated much later 
    # on in the code
    
  
    # NEW SECTION: 
    # I define the step in the rtns along the efficient frontier
    delta = (maxval-minval)/Npoints   
    # I slightly correct the max value along the efficient frontier to be 
    # sure not to end up with some problems with the finite precision
    maxval = max(assetmeans)-delta/40 
    # Now I introduce the idea of statistical equivalence for all the 
    # stochastically generated ptfs (the MV, the average rtn and the 
    # maximum rtn portfolios are worked out by Michaud in his book).
    mineq = matrix(nrow=niter,ncol=nassets)
    maxeq = matrix(nrow=niter,ncol=nassets)
    middleeq = matrix(nrow=niter,ncol=nassets)
    # Now I initialize them in a convenient way
    mineq[, ] = 2.0
    maxeq[, ] = 2.0
    middleeq[, ] = 2.0
    # matrix representing a statistical realization of the historical ptf.
    # i.e. this matrix will contain the Monte-Carlo simulated "historical" 
    # returns of the assets in my portfolio
    samplemat = matrix(ncol=nassets,nrow=nlength)                                            
    # each simulated ptf will have its own expected returns
    # which I need to calculate to know beforehand if the 
    # portfolio can be optimized along the whole interval 
    # [minvalue,maxvalue] or not.
    rtnsamplemat = seq(1:nassets) 
    # vector of the expected rtns along the efficient frontier.  
    exprtn = seq(1,Npointseff+1)   
    # vector containing the returns of the simulated portfolios
    # along the efficient frontier
    for (i in 1:(Npointseff+1)) {
        exprtn[i]=minval+(i-1)*delta  
    } 
    if (Npoints == Npointseff) {                                                     
        # to avoid numerical problems
        # at the end of the computation.
        exprtn[(Npointseff+1)] = exprtn[(Npointseff+1)]-delta/40.0
    
    } 
    # matrix which will contain the
    # ptf weights averaged on many stochastic
    # trajectories
    ptfweight = matrix(ncol=nassets,nrow=(Npointseff+1))                                                     
    # Now I initialize the matrix which will contain a list of the ptf weight
    for (i in 1:(Npointseff+1)) {
        for (j in 1:nassets) {
            ptfweight[i, j] = 0.0
        }
    }
    # I also introduce the vectors which will contain the expected means 
    # and stds of the simulated ptfs calculated using the EXPECTED ptf 
    # rtns (not the simulated ones) and the covariance matrix I ASSUME TO 
    # BE THE TRUE ONE.
    ptfmean = seq(1:(Npointseff+1))
    ptfstd = seq(1:(Npointseff+1))
    # Now I initialize the vectors containing the average means and the 
    # final stds of the resampled ptfs
    for (i in 1:(Npointseff+1)) {
        ptfmean[i] = 0.0
        # I initialize the previous arrays
        ptfstd[i] = 0.0
    }
   
    
    # NEW SECTION:
    # Now I start performing the iterations (i.e. the optimization of many 
    # Gaussian-distributed random portfolios)
    # Right now I generate the matrix containing the multivariate Gaussian 
    # distribution for each asset, i.e. I generate a sequence of Gaussian-
    # distributed random numbers with mean equal to the required (historical 
    # or expected) return of each asset and with std equal to the expected 
    # std of each asset  
    if (correlation == 0) {
        # Length of the Gaussian univariate random numbers which I will 
        # turn into a Gaussian multivariate later on
        randomlength = nlength*niter*nassets     
        noisemat = rnorm(randomlength)
        # Now the matrix has the right dimension
        # but it is an univariate with zero mean
        dim(noisemat) = c((niter*nlength), nassets)
        for (i in 1:nassets) {
            noisemat[, i] = noisemat[, i] * sqrt(Dmat[i, i])+assetmeans[i]
        }  
    }
    if (correlation == 1) {  
        # First I use Cholesky factorization of the covariance matrix
        # (in order to introduce the correlations in the multivariate 
        # distribution)
        R = chol(Dmat)     
        randomlength = nlength*niter*nassets
        noisemat = rnorm(randomlength)
        # Now the matrix has the right dimension
        # but it is an univariate with zero mean
        dim(noisemat) = c((niter*nlength), nassets) 
        # Now the matrix has the right correlations, 
        # but the wrong mean
        noisemat = noisemat %*% R  
        for (i in 1:nassets) {
            # finally I have my correlated Gaussian 
            # multivariate distribution.
            noisemat[, i] = noisemat[, i]+assetmeans[i] 
        }   
    }
    for (i in 1:niter) {
        if (i%%100 == 0) {
            # just to get an idea of how fast the script is running
            if (trace) print("iteration is") 
            if (trace) print(i)
        }           
        # I now read sequentially the matrix containing the simulated 
        # stories of the ptfs
        samplemat[1:nlength, ] = noisemat[(1+(nlength*(i-1))):(nlength*i), ]    
        for (m in 1:nassets) {
            rtnsamplemat[m] = mean(samplemat[, m])
        }
        # Now I start to work out the composition of the min-var ptf 
        # this is the covariance matrix (coming from the sampling of the
        # multivariate distribution).
        # I will be using at each  iteration
        Dmat3 = cov(samplemat)                     
        # the other objects I need have already been defined before for 
        # the case of the Markowitz mean variance optimization
        # this gives the MV portfolio 
        # which is the portfolio with
        # the lowest return 
        resminvar3 = .solve.QP(Dmat3,dvec,Amat,bvec2,meq=1)
        stochaminvarweights = resminvar3$solution
        ptfweight[1, ] = (resminvar3$solution+ptfweight[1, ])
        # this is the lowest expected return of each
        # portfolio along the simulated efficient #frontier
        stochaexprtn[1] = stochaminvarweights%*%rtnsamplemat       
        # In the following part between the #s I carry out Jobson & Korkie 
        # experiment
        # Now I collect the expected rtns
        # This is the idea of Jobson and Korkie experiment:estimate the 
        # variance and expected rtn of each of the previously calculated 
        # portfolios using the "true" expected returns and the "true" 
        # covariance matrix (not the ones estimated from the simulated data)    
        if (confidence ==1) {
            # at this point I have collected the rtns
            # of the "simulated" portfolios 
            JKrtn[(Npointseff+1)*(i-1)+1] =  stochaminvarweights%*%assetmeans                                        
            # and the expected std
            JKstd[(Npointseff+1)*(i-1)+1] = 
                sqrt(stochaminvarweights%*%Dmat%*%stochaminvarweights)
        } 
        # I carry on to calculate the efficient frontier        
        # I now need to fix a grid in the portfolio returns 
        delta3 = (max(rtnsamplemat)-stochaexprtn[1])/Npoints
        for (l in 1:(Npointseff+1)) {
            # new vector of asset returns
            stochaexprtn[l] = stochaexprtn[1]+(l-1)*delta3
        }
        # Now I introduce a correction to be sure not to have troubles 
        # at the extreme
        if (Npoints == Npointseff) {
            stochaexprtn[(Npointseff+1)] = 
                stochaexprtn[(Npointseff+1)]-delta/40.0
        }
        for (j in 2:(Npointseff+1)) {  
            # Now I have to start from 2
            number=stochaexprtn[j]  
            # Now I need to set up the optimization problem again
            # for the Monte-Carlo simulated portfolios
            if (nupper == 0) {
                Atrasp3 = rbind(t(rtnsamplemat), Atrasp)
                #case when the only constrain is not to allow the short-selling
                bvec3 = rbind(number,bvec2)     
                Amat4 = t(Atrasp3)
            } else {
                Atrasp3 = rbind(t(rtnsamplemat), Atrasp,constrainmat)
                bvectemp[1] = number
                bvectemp[2:(nassets+2+nupper)] = bvec2
                bvec3 = bvectemp
                Amat4 = t(Atrasp3)
            }           
            # Now I re-express the optimization problem
            res = .solve.QP(Dmat3,dvec,Amat4,bvec3,meq=2)
            # if (trace) print("ok optim")
            # I am mainly interested in the ptf composition which is saved 
            # and finally averaged in the ptf weight array.         
            ptfweight[j, ] = (res$solution+ptfweight[j, ])     
            if (confidence == 1) {    
                JKstd[j+(i-1)*(Npointseff+1)] = 
                    sqrt(res$solution%*%Dmat%*%res$solution)
                JKrtn[j+(i-1)*(Npointseff+1)] = res$solution%*%assetmeans
            }
            # Now I add an extra condition
            if (j == 1)  {
                mineq[i, ] = res$solution
            }
            if (j == (Npointseff+1)) {
                maxeq[i, ] = res$solution
            }
            if (j == (Npointseff/2.0)) {
                middleeq[i, ] = res$solution}
        }
    }
    # I do need the weights averaged on many stochastic trajectories
    for (i in 1:(Npointseff+1)) {
        for (j in 1:nassets) {
            ptfweight[i, j] = ptfweight[i, j]/niter
        }
    }
   
    
    # NEW SECTION:
    # Now I work out the efficient frontier using simply Markowitz without 
    # any sampling since in general I am not using the historical covariance 
    # matrix, I cannot use portfolio.optim but I need to resort to solve.QP 
    # directly
    # NB:much earlier on in the code I only worked out the MV portfolio 
    # using classical Markowitz optimization.
    # I define the vector which will contain the ptf std
    mystd = seq(1:(Npointseff+1))
    # matrix with the ptf weights
    myweight = matrix(nrow = (Npointseff+1), ncol = nassets)
    if (nupper == 0) {
        # I need to add an extra constrain on the expected
        # return of the ptf, so I have to add a line 
        # with the expected rtns of the assets
        Atraspnew = rbind(t(assetmeans),Atrasp)
        # this line can be misleading:here adding 1 does not mean 
        # anything apart from the fact that the I am stretching the
        # bvec vector. The extra element will contain the expected
        # ptf return at each iteration
        bvecnew = rbind(1.0,bvec2)
        # again, I need to transpose the matrix to use it inside solve.QP
        Amatnew = t(Atraspnew)
        # Now I can perform a Markowitz optimization of the ptf  
    }
    if (trace) {
        print("OK so far!")
    }
    if (nupper != 0) {
        Atraspnew = rbind(t(assetmeans),Atrasp,constrainmat)
        Amatnew = t(Atraspnew)    
        bvectemp[2:(nassets+2+nupper)] = bvec2
        bvecnew = as.matrix(bvectemp)
    }                    
    for (i in 1:(Npointseff+1)) {
        number=exprtn[i]
        # the new element of the vector contains the constrain of the
        # total ptf return. Again, even it bvecnew is a row, solve.QP
        # is smart enough to understand the problem                     
        bvecnew[1, 1] = number
        # Now I have 2 equality constrains
        # the ptf has to be totally invested
        # and I fix its expected rtn
        nosample = .solve.QP(Dmat, dvec, Amatnew, bvecnew, meq = 2)
        # ptf std
        mystd[i] = sqrt(nosample$solution %*% Dmat %*% nosample$solution)
        # ptf weights
        myweight[i, ] = nosample$solution 
    }
 
        
    # NEW SECTION:
    # Now I work out the std and the expected rtn of the ptf by using 
    # the average weights obtained from the sampling with the "true" 
    # covariance matrix and the "true" expected rtns this is the idea 
    # introduced by Michaud and it now leads to the resampled efficient
    # frontier   
    mcrtn = seq(1:(Npointseff+1))
    tempvec = seq(1:nassets)
    for (i in 1:(Npointseff+1)) {
        # I get the expected rtn of the ptf
        for (j in 1:nassets) {
            tempvec[j] = ptfweight[i, j]
        }
        mcrtn[i] = assetmeans %*% tempvec
    }
    mcstd = seq(1:(Npointseff+1))
    for (i in 1:(Npointseff+1)) {
        # I get the expected std of the ptf
        for (j in 1:nassets) {
            tempvec[j] = ptfweight[i, j]
        }
        mcstd[i] = sqrt(tempvec %*% Dmat %*% tempvec)  
    }
   
    
    # NEW SECTION:
    # Now I work out the set of ptfs statistically equivalent to the minvar, 
    # max rtn and middlE rtn ptfs first I need to count how many ptfs of 
    # each kind I have
    # number of ptfs with the highest rtn
    countmax = niter 
    # number of ptfs with the lowest rtn
    countmin = niter 
    # number of ptfs with middle rtn
    countmiddle = niter 
    # Now I define the vectors which will contain the mean/var coordinates 
    # of these portfolios
    stdmin = seq(1:countmin)
    stdmiddle = seq(1:countmiddle)
    stdmax = seq(1:countmax)  
    # and now the same for the statistically equivalent means
    meanmin = seq(1:countmin)
    meanmiddle = seq(1:countmiddle)
    meanmax = seq(1:countmax)
    # Now I actually work out those objects
    # I will perform three different loops 1st one for the min rtn ptf 
    # NB:I initialized to 2 the arrays containing the weights of the 
    # three remarkable statistically equivalent ptfs, so I need to read 
    # only the weights which are different from 2 (i.e. those worked out
    # by the Monte Carlo technique).
    for (i in  1:countmin) {
        if (mineq[i, 1] != 2) {
            meanmin[i] = assetmeans%*%mineq[i, ]
            stdmin[i] = sqrt(mineq[i, ]%*%Dmat%*%mineq[i, ])
        }
    }
    # 2nd one for the max rtn ptf
    for (i in  1:countmax) {
        if (maxeq[i, 1] != 2) {
            meanmax[i] = assetmeans%*%maxeq[i, ]
            stdmax[i] = sqrt(maxeq[i, ]%*%Dmat%*%maxeq[i, ])
        }
    }
    # 3rd one for the mean rtn ptf
    for (i in  1:countmiddle) {
        if (middleeq[i, 1] != 2) {
            meanmiddle[i] = assetmeans%*%middleeq[i, ]
            stdmiddle[i] = sqrt(middleeq[i, ] %*% Dmat %*% middleeq[i, ])
        }
    }
    # Now I use Michaud's definition of the efficient portfolio as the 
    # averaged portfolio given by the averaged middle ptf
    Michptfeff = seq(1:nassets)
    Michptfeff = ptfweight[(Npointseff/2), ]
    write.csv(Michptfeff,"composition-Michaud-ptf.csv")
    # Now a save a vector with the parameters used in this simulation
    param = c(rtnchoice,choicecov,target,confidence,correlation)
    write.csv(param,"parameters.csv")
    annualizedrtn = frequency*assetmeans
    # just a diagnostic
    write.csv(annualizedrtn,"exprtn.csv")
    # Now I create an array containing the numbers of the constrained 
    # assets and their maximum allowed weight.
    saveconstr = cbind(limupper,maxweight)
    write.csv(saveconstr,"extra-constrains.csv")
    # Now I just redefine and annualized a few objects needed to plot 
    # the efficient frontiers
    mystd = mystd*sqrt(frequency)
    exprtn = exprtn*frequency
    mcrtn = mcrtn*frequency
    mcstd = mcstd*sqrt(frequency)
   
    
    # NEW SECTION: 
    if (confidence == 1) {
        # Now I annualize the collected stds and the collected rtns along 
        # the stochastic trajectories
        # collectstd = collectstd*sqrt(frequency)
        # collectrtn = collectrtn*frequency
        if (confidence == 1) {
            JKstd = JKstd*sqrt(frequency)
            JKrtn = JKrtn*frequency
        }              
        # Now I need to start defining a grid which will contain first 
        # 100% of the random ptfs and then 90% of them.
        # If I choose zero as the basis (i.e. the minimum rtns) of each
        # element of the the grid of rectangles, then that is going to 
        # contain all the generated ptfs first I care about the max and 
        # min of the std of the generated ptf.
        # JKstd = collectstd
        # JKrtn = collectrtn
        # dim(JKstd) = c(niter*(Npoints+1))
        # plotting these together with the mean variance
        # frontier is the famous Jobson-Korkie
        # experiment.
        #  dim(JKrtn) = c(niter*(Npoints+1))  
        # Now I think I about how to define the 90% confidence area.
        # minstd = min(collectstd)
        # maxstd = max(collectstd)
        minstd = min(JKstd)
        maxstd = max(JKstd)   
        nrect = 20
        deltastd = (maxstd-minstd)/nrect
        rectgrid = seq(1:(nrect+1))
        for (i in 1:(nrect+1)) {
            rectgrid[i] = minstd+(i-1)*deltastd
        }
        # Now I try to define a 90% confidence region
        # First of all I need to know how many ptfs fall in each region of std
        countptf = seq(1:nrect)
        countptf[ ]  = 0
        numbertot = niter*(Npointseff+1)
        # orderedrtn = seq(1:numbertot)
        # orderedrtn[ ] = 0
        # orderedstd = orderedrtn
        # I somehow need to make the code more performing in here.
        # first of all, I can make an array of ordered couples of coordinated
        # in the mean/var plane
        # this is a matrix made up of 2 very long lines and JKstd is on top
        ptfs = rbind(JKstd, JKrtn)
        # First I order the std of the various ptfs
        # Now the stds are in increasing order
        # orderstd = sort(JKstd,decreasing=FALSE, method=c("quick"))
        # for (i in 1:numbertot) {
        #   for (j in 1:numbertot) {
        #       if (orderstd[i] == JKstd[j]) {
        #           orderedrtn[i] = JKrtn[j]
        #       }
        #   }
        # }
        # orderedptfs = rbind(orderedstd,orderedrtn)   
        # Now I quickly sort the couple of numbers in increasing order
        orderedptfs = ptfs[,order(ptfs[1,],decreasing=FALSE)]
        for (i in 1:numbertot) {
            for (j in 1:nrect) {
                if (JKstd[i] > rectgrid[j] & JKstd[i]< rectgrid[j+1]) {
                    countptf[j] = countptf[j]+1
                }
            }
        }         
        rtnline = seq(1:nrect)       
        countptf[1] = countptf[1]+1
        countptf[nrect] = countptf[nrect]+1
        for (i in 1:nrect){
            if (i == 1) {
                number = 0
            }
            if( i != 1) {
                number = sum(countptf[1:(i-1)])
            }    
            # slot = orderedrtn[(number+1):(number+countptf[i])]
            slot = orderedptfs[2,(number+1):(number+countptf[i])]
            slotordered = sort(slot, decreasing = TRUE, method = c("quick"))
            position = alpha*countptf[i]-(alpha*countptf[i])%%1
            rtnline[i] = slotordered[position]
        }     
        # as a horizontal coord, I simply pick up the 
        # median of each interval on the std grid I created above
        stdmedian = seq(1:nrect)   
        for ( i in 1:nrect) {
            stdmedian[i] = (rectgrid[i]+rectgrid[i+1])/2
        }
    }
    

    # NEW SECTION::
    # Now I also save the portfolios weights along the efficient frontier 
    write.csv(ptfweight, "ptf-weights-along-frontier-resampled.csv")
    
    
    # NEW SECTION::
    # Finally, some plots
    if (doplot) {
        # Efficient Frontier Plot:
        plot(mystd, exprtn, type = "l", col = 10, 
            main = "Markowitz and Resampled Frontiers", 
            xlab = "Standard Deviation", ylab = "Expected Return",lwd=2)
        lines(mcstd, mcrtn, col = 300, lwd = 2)
        legend(0.01, 0.11, 
            c("Markowitz Efficient Frontier", "Resampled Frontier"), 
            c(col = 10, col = 300) ) 
        # Confidence Level Plot:
        if (confidence == 1) {       
            plot(mystd, exprtn, type = "l", col = 10, 
                main = "Confidence Level", 
                xlab = "Standard Deviation", 
                ylab = "Expected Return", lwd = 2)
            lines(stdmedian, rtnline, col = 300, lwd = 2)
            legend(0.01, 0.11, c("Markowitz Efficient Frontier", 
                "90% confidence level"), c(col = 10, col = 300) ) 
        }
    }
    
    
    # NEW SECTION::
    if (trace) print("The End")
    
    # Return Value:
    pfolio = list(
        mystd = mystd, exprtn = exprtn, mcrtn = mcrtn, rtnline = rtnline,
        ptfweight = ptfweight)
        
    # Return Value:
    pfolio
}


################################################################################

