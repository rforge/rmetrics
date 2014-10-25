\name{garchFitControl}


\alias{garchFitControl}


\title{GARCH Fitting Algorithms and Control}


\description{

    Estimates the parameters of an univariate GARCH process.

}


\usage{    
garchFitControl(
    llh = c("filter", "internal", "testing"),
    nlminb.eval.max = 2000, 
    nlminb.iter.max = 1500,
    nlminb.abs.tol = 1.0e-20, 
    nlminb.rel.tol = 1.0e-14, 
    nlminb.x.tol = 1.0e-14, 
    nlminb.step.min = 2.2e-14,
    nlminb.scale = 1, 
    nlminb.fscale = FALSE,
    nlminb.xscale = FALSE,      
    sqp.mit = 200,       
    sqp.mfv = 500,       
    sqp.met = 2,                                 
    sqp.mec = 2,                                
    sqp.mer = 1,                                  
    sqp.mes = 4,                                 
    sqp.xmax = 1.0e3,    
    sqp.tolx = 1.0e-16,    
    sqp.tolc = 1.0e-6,   
    sqp.tolg = 1.0e-6,  
    sqp.told = 1.0e-6,  
    sqp.tols = 1.0e-4,  
    sqp.rpf = 1.0e-4,
    lbfgsb.REPORT = 10,
    lbfgsb.lmm = 20, 
    lbfgsb.pgtol = 1e-14, 
    lbfgsb.factr = 1, 
    lbfgsb.fnscale = FALSE,
    lbfgsb.parscale = FALSE,   
    nm.ndeps = 1e-14,
    nm.maxit = 10000, 
    nm.abstol = 1e-14,
    nm.reltol = 1e-14, 
    nm.alpha = 1.0, 
    nm.beta = 0.5, 
    nm.gamma = 2.0,
    nm.fnscale = FALSE, 
    nm.parscale = FALSE)
}


\arguments{ 
        
    % In general:
        
    \item{llh}{
        llh = c("filter", "internal", "testing")[1],
        defaults to "filter".
        }
                   
    % nlminb:
    
    \item{nlminb.eval.max}{
        Maximum number of evaluations of the objective function 
        allowed, defaults to 200.
        }
    \item{nlminb.iter.max}{
        Maximum number of iterations allowed, defaults to 150.
        } 
    %\item{nlminb.trace}{
    %    The value of the objective function and the parameters is 
    %    printed every trace'th iteration. Defaults to 0 which 
    %    indicates no trace information is to be printed.
    %    } 
    \item{nlminb.abs.tol}{
        Absolute tolerance, defaults to 1e-20.
        }
    \item{nlminb.rel.tol}{
        Relative tolerance, defaults to 1e-10. 
        }
    \item{nlminb.x.tol}{
        X tolerance, defaults to 1.5e-8. 
        }
    \item{nlminb.fscale}{
        defaults to FALSE.
        }
    \item{nlminb.xscale}{ 
        defaulkts to FALSE.
        }
    \item{nlminb.step.min}{
        Minimum step size, defaults to 2.2e-14. 
        }
    \item{nlminb.scale}{
        defaults to 1.
        }    

    % sqp:

    %\item{sqp.iprnt}{
    %    as.integer(trace). Default to 1.
    %    } 
    \item{sqp.mit}{
        maximum number of iterations, defaults to 200.
        }
    \item{sqp.mfv}{
        maximum number of function evaluations, defaults to 500.
        }
    \item{sqp.met}{
        specifies scaling strategy:\cr
        sqp.met=1 - no scaling\cr 
        sqp.met=2 - preliminary scaling in 1st iteration (default)\cr
        sqp.met=3 - controlled scaling\cr
        sqp.met=4 - interval scaling \cr
        sqp.met=5 - permanent scaling in all iterations 
        }
    \item{sqp.mec}{
        correction for negative curvature:\cr
        sqp.mec=1 - no correction\cr
        sqp.mec=2 - Powell correction (default)
        }
    \item{sqp.mer}{
        restarts after unsuccessful variable metric updates:\cr
        sqp.mer=0 - no restarts\cr
        sqp.mer=1 - standard restart 
        }
    \item{sqp.mes}{
        interpolation method selection in a line search:\cr
        sqp.mes=1 - bisection\cr
        sqp.mes=2 - two point quadratic interpolation\cr
        sqp.mes=3 - three point quadratic interpolation\cr
        sqp.mes=4 - three point cubic interpolation (default) 
        }           
    \item{sqp.xmax}{
        maximum stepsize, defaults to 1.0e+3.
        } 
    \item{sqp.tolx}{
        tolerance for the change of the coordinate vector,
        defaults to 1.0e-16.
        } 
    \item{sqp.tolc}{
        tolerance for the constraint violation,
        defaults to 1.0e-6.
        } 
    \item{sqp.tolg}{
        tolerance for the Lagrangian function gradient,
        defaults to 1.0e-6. 
        }  
    \item{sqp.told}{
        defaults to 1.0e-6.
        }  
    \item{sqp.tols}{
        defaults to 1.0e-4.
        }
    \item{sqp.rpf}{
        value of the penalty coefficient,
        default to1.0D-4.
        The default velue may be relatively small. Therefore, larger
        value, say one, can sometimes be more suitable.
        } 
        
    % optim[lbfgsb]:
    
    \item{lbfgsb.REPORT}{
        The frequency of reports for the "BFGS" and "L-BFGS-B" methods if 
        control\$trace is positive. Defaults to every 10 iterations.
        } 
    \item{lbfgsb.lmm}{
        is an integer giving the number of BFGS updates retained in 
        the "L-BFGS-B" method, It defaults to 5. 
        }
    \item{lbfgsb.factr}{
        controls the convergence of the "L-BFGS-B" method. Convergence 
        occurs when the reduction in the objective is within this factor 
        of the machine tolerance. Default is 1e7, that is a tolerance 
        of about 1.0e-8. 
        }
    \item{lbfgsb.pgtol}{
        helps control the convergence of the "L-BFGS-B" method. It is a 
        tolerance on the projected gradient in the current search 
        direction. This defaults to zero, when the check is suppressed.
        } 
    \item{lbfgsb.fnscale}{
        defaults to FALSE.
        }
    \item{lbfgsb.parscale}{ 
        defaults to FALSE. 
        }  
        
    % optim[nm]:
    
    %\item{nm.trace}{
    %    Non-negative integer. If positive, tracing information on the 
    %    progress of the optimization is produced. Higher values may 
    %    produce more tracing information: for method "L-BFGS-B" there 
    %   are six levels of tracing. (To understand exactly what these 
    %   do see the source code: higher levels give more detail.)
    %   } 
    \item{nm.ndeps}{
        A vector of step sizes for the finite-difference approximation 
        to the gradient, on par/parscale scale. Defaults to 1e-3.
        } 
    \item{nm.maxit}{
        The maximum number of iterations. Defaults to 100 for the 
        derivative-based methods, and 500 for "Nelder-Mead". For "SANN" 
        maxit gives the total number of function evaluations. There is 
        no other stopping criterion. Defaults to 10000.
        } 
    \item{nm.abstol}{
        The absolute convergence tolerance. Only useful for non-negative 
        functions, as a tolerance for reaching zero.
        } 
    \item{nm.reltol}{
        Relative convergence tolerance. The algorithm stops if it is 
        unable to reduce the value by a factor of 
        reltol * (abs(val) + reltol) at a step. Defaults to 
        sqrt(.Machine\$double.eps), typically about 1e-8. 
        }
    \item{nm.alpha, nm.beta, nm.gamma}{
        Scaling parameters for the "Nelder-Mead" method. 
        alpha is the reflection factor (default 1.0), 
        beta the contraction factor (0.5), and 
        gamma the expansion factor (2.0). 
        }
    \item{nm.fnscale}{
        An overall scaling to be applied to the value of fn and gr 
        during optimization. If negative, turns the problem into a 
        maximization problem. Optimization is performed on 
        fn(par)/fnscale. 
        }
    \item{nm.parscale}{
        A vector of scaling values for the parameters. Optimization is 
        performed on par/parscale and these should be comparable in the 
        sense that a unit change in any element produces about a unit 
        change in the scaled value. 
        }
    
}


\value{
    
    returns a list.
      
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port,\cr
    R Core Team for the 'optim' \R-port,\cr
    Douglas Bates and Deepayan Sarkar for the 'nlminb' \R-port,\cr
    Bell-Labs for the underlying PORT Library,\cr
    Ladislav Luksan for the underlying Fortran SQP Routine, \cr
    Zhu, Byrd, Lu-Chen and Nocedal for the underlying L-BFGS-B Routine.
    
}


\examples{  
## 
}


\keyword{models}

