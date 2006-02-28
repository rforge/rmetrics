

# Package: lmomco
# Title: L-moments, L-comoments, and Distributions
# Version: 0.3
# Date: 2006-01-31
# Author: William H. Asquith
# Description: The package implements the statistical theory of L-moments including
#  L-moment estimation, probability-weighted moment estimation, parameter estimation
#  for numerous familiar and not-so-familiar distributions, and L-moment estimation
#  for the same distributions from the parameters. L-moments are derived from the
#  expectations of order statistics and are linear with respect to the probability-
#  weighted moments. L-moments are directly analogous to the well-known product
#  moments; however, L-moments have many advantages including unbiasedness,
#  robustness, and consistency with respect to the product moments. This package
#  is oriented around the FORTRAN algorithms of J.R.M. Hosking, and the
#  nomenclature for many of the functions parallels that of the Hosking library.
#  Further features are added to aid in extension of the breadth of L-moment
#  application. Additionally, recent developments by Robert Serfling and Peng Xiao
#  have extended L-moments into multivariate space; the so-called sample L-comoments
#  are implemented here.
# Maintainer: William H. Asquith <wasquith@austin.rr.com>
# License: GPL
# Packaged: Tue Jan 31 14:18:07 2006; wasquith


################################################################################


"INT.check.fs" <-
function(fs) {
  if(any(fs < 0) | any(fs > 1)) {
     print("invalid nonexceedance probability")
     return(FALSE)
  }
  return(TRUE)
}

"INT.kapicase1" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    # OFL SHOULD BE CHOSEN SO THAT EXP(OFL) JUST DOES NOT CAUSE OVERFLOW
    # Hosking's code based used 170
    OFL <- log(.Machine$double.xmax)
    DLGAM <- lgamma(1+G)
      #
      #  - CASE H<0, G NONZERO
    for(R in seq(1,5)) {
      ARG <- DLGAM+lgamma(-R/H-G)-lgamma(-R/H)-G*log(-H)
      if(abs(ARG) > OFL) {
        warning("Calculations of L-moments have broken down")
        return()
      }
      BETA[R] <- exp(ARG)
    }
    return(BETA)
}

"INT.kapicase2" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    DLGAM <- lgamma(1+G)
    #
    #         - CASE H SMALL, G NONZERO
    #
    for(R in seq(1,5)) {
      BETA[R] <- exp(DLGAM-G*log(R))*(1-0.5*H*G*(1+G)/R)
    }
    return(BETA)
}

"INT.kapicase3" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    # OFL SHOULD BE CHOSEN SO THAT EXP(OFL) JUST DOES NOT CAUSE OVERFLOW
    OFL <- log(.Machine$double.xmax)
    DLGAM <- lgamma(1+G)
    #
    #         - CASE H>0, G NONZERO
    #
    for(R in seq(1,5)) {
      ARG <- DLGAM+lgamma(1+R/H)-lgamma(1+G+R/H)-G*log(H)
      if(abs(ARG) > OFL) {
        warning("Calculations of L-moments have broken down")
        return()
      }
      BETA[R] <- exp(ARG)
    }
    return(BETA)
}

"INT.kapicase4" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    #
    #         - CASE H<0, G <- 0
    #
    #   EU  IS EULER'S CONSTANT
    EU <- 0.577215664901532861
    for(R in seq(1,5)) {
      BETA[R] <- EU + log(-H)+digamma(-R/H)
    }
    return(BETA)
}

"INT.kapicase5" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    #
    #         - CASE H SMALL, G <- 0
    #
    #   EU  IS EULER'S CONSTANT
    EU <- 0.577215664901532861
    for(R in seq(1,5)) {
      BETA[R] <- EU+log(R)
    }
    return(BETA)
}

"INT.kapicase6" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    #
    #         - CASE H>0, G <- 0
    #
    #   EU  IS EULER'S CONSTANT
    EU <- 0.577215664901532861
    for(R in seq(1,5)) {
      BETA[R] <- EU+log(H)+digamma(1+R/H)
    }
    return(BETA)
}

"Lcomoment.Lk12" <-
function(X1, X2, k=1) {
  # Following notation of Serfling and Xiao (2006)
  #  compute the unbiased L-statistic estimator
  
  # Compute the concomitant of X2
  #    First sort X2 in ascending order, but need the indices
  #    Second rearrange X1 in the order of X2
  I   <- sort(X2, decreasing=FALSE, index.return=TRUE)
  X12 <- X1[I$ix]
  
  sum <- 0                     # a summation
  n   <- length(X1)            # sample size
  for(r in seq(1,n)) {         # for each value in the sample
    Wk  <- Lcomoment.Wk(k,r,n) # compute the weight factor
    sum <- sum + Wk*X12[r]     # sum them up
  }                            # end of loop
  Lk12 <- sum/n                # compute the expected value
  return(Lk12)                 # return the L-comoment
}
"Lcomoment.Wk" <-
function(k, r, n) {
  # Following notation of Serfling and Xiao (2006)
  #   compute the Wk weight factor for kth L-moment
  Wk <- 0
  jn <- min(c(r-1,k-1))  # find the minimum for the loop end
  for(j in seq(0,jn)) {
    t1 <- (-1)**(k-1-j)
    t2 <- choose(k-1,j)
    t3 <- choose(k-1+j,j)
    t4 <- choose(n-1,j)
    t5 <- choose(r-1,j)
    Wk <- Wk + t1*t2*t3*t5/t4
  }
  return(Wk)
}
"Lcomoment.coefficients" <-
function(Lk,L2) {
  # Following notation of Serfling and Xiao (2006)
  #  compute the L-comoment coefficients
  #  The univariate L-moment ratios are on the diagonal of Lk
  if(is.null(Lk$type) || Lk$type != "Lcomoment.matrix") {
    warning("First argument does not appear to be an L-comoment matrix")
    return()
  }
  if(is.null(L2$type) || L2$type != "Lcomoment.matrix") {
    warning("Second argument does not appear to be an L-comoment matrix")
    return()
  }
  if(Lk$order >= 2 && L2$order != 2) {
    warning("Frist L-comoment matrix is order 2 or greater, but second matrix is not of order 2")
    return()
  }
  if(Lk$order == 1 && L2Rorder != 1) { # In L-CV calculations L2/L1, but in others Lk/L2
    warning("First L-comoment matrix is order 1, but second matrix is not 2nd order.")
    return()
  }
  LC      <- Lk$matrix        # to get the structure of Lk
  Lscales <- diag(L2$matrix)  # get univariate L-scale values
  n       <- length(Lscales)  # how many are there (how many columns)
  for(i in seq(1,n)) {        # loop through each column
    Lscale <- Lscales[i]      # extract single L-scale value
    LC[i,] <- Lk$matrix[i,]/Lscale   # divide the column by L-scale
                                     # to form coefficients
  }                           # end of loop
  z <- list(type="Lcomoment.coefficients", order = Lk$order, matrix = LC)
  return(z)                   # return the matrix
}

"Lcomoment.correlation" <-
function(L2) {
  if(L2$order != 2) {
    warning("L-comoment matrix argument is not of order 2")
    return()
  }

  # Following Serfling and Xiao (2006)
  #  L-correlations are the L-comoment coefficents of L-scale
  #  The diagonal of LC are the coefficients of L-variation
  LC <- Lcomoment.coefficients(L2,L2)
  return(LC)
}
"Lcomoment.matrix" <-
function(DATAFRAME, k=1) {
  # DATAFRAME is data.frame of rectangular dimension
  # k is the kth order of L-comoments
  
  f <- length(DATAFRAME)        # how many fields or "random variables"
  M <- matrix(nrow=f, ncol=f)   # generate square matrix
  n <- length(DATAFRAME[,1])    # sample size 

  for(x1 in seq(1,f)) {         # BEGIN LOOP 1
    X1 <- DATAFRAME[,x1]        # extract array "1"
    for(x2 in seq(1,f)) {       # BEGIN LOOP 2
      X2 <- DATAFRAME[,x2]      # extract array "2"
      M[x1,x2] <- Lcomoment.Lk12(X1,X2,k) # compute the L-comoments
                                # for 1 and 2 and order k
    }                           # END LOOP 2
  }                             # END LOOP 1
  z <- list(type="Lcomoment.matrix", order=k, matrix=M)
  return(z)                     # return the matrix
}

"are.lmom.valid" <-
function(lmom) {
   # The early return trues are for situations in which the higher moments
   # are simply not available--say from computing the l-moments of a distribution
   if(is.null(lmom$L2))    return(TRUE)
   if(lmom$L2 <= 0)        return(FALSE)
   if(is.null(lmom$TAU3))  return(TRUE)
   if(abs(lmom$TAU3) > 1)  return(FALSE)
   if(is.null(lmom$TAU4))  return(TRUE)
   if(lmom$TAU4 < (0.25*(5*lmom$TAU3^2 - 1)) | lmom$TAU4 > 1) return(FALSE)
   if(is.null(lmom$TAU5))  return(TRUE)
   if(lmom$TAU5 > 1)       return(FALSE)
   return(TRUE)
}

"are.par.valid" <-
function(para) {
    if(is.null(para$para)) {
      warning("The parameter object is missing a para attribute.")
      return()
    }
    if(is.null(para$type)) {
      warning("The parameter object is missing a type attribute.")
      return()
    }
    type <- para$type
    if(type == 'cau') {
      return(are.parcau.valid(para))
    }
    else if(type == 'exp') {
      return(are.parexp.valid(para))
    }
    else if(type == 'gam') {
      return(are.pargam.valid(para))
    }
    else if(type == 'gev') {
      return(are.pargev.valid(para))
    }
    else if(type == 'gld') {
      return(are.pargld.valid(para))
    }    
    else if(type == 'glo') {
      return(are.parglo.valid(para))
    }
    else if(type == 'gno') {
      return(are.pargno.valid(para))
    }
    else if(type == 'gpa') {
      return(are.pargpa.valid(para))
    }
    else if(type == 'gum') {
      return(are.pargum.valid(para))
    }
    else if(type == 'kap') {
      return(are.parkap.valid(para))
    }
    else if(type == 'nor') {
      return(are.parnor.valid(para))
    }
    else if(type == 'pe3') {
      return(are.parpe3.valid(para))
    }
    else if(type == 'wak') {
      return(are.pareak.valid(para))
    }
    else {
      stop("Did not find a valid distribution type.")
    }
}
"are.parcau.valid" <-
function(para) {
    if(! is.cau(para)) return(FALSE)
    U <- para$para[1] 
    A  <- para$para[2] 
    #if() {
    #  warning("Parameters are invalid.")
    #  return(FALSE)
    #}
    return(TRUE)
}

"are.parexp.valid" <-
function(para) {
    if(! is.exp(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.pargam.valid" <-
function(para) {
    if(! is.gam(para)) return(FALSE)
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    if(ALPHA <= 0 | BETA <= 1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.pargev.valid" <-
function(para) {
    if(! is.gev(para)) return(FALSE)
    A <- para$para[2]
    G <- para$para[3]
    if(A <= 0 | G <= -1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.pargld.valid" <-
function(para) {
    if(! is.gld(para)) return(FALSE)

    La2 <- para$para[2]
    La3 <- para$para[3]
    La4 <- para$para[4]

    if(La3 <= -1 && La4 >=  1) { # REGION 1
       return(TRUE)
    }
    if(La3 >=  1 && La4 <= -1) { # REGION 2
       return(TRUE)
    }
    if(La3 < 0 && La4 > 0 && La4 < 1) { # REGION V1
       warning("Parameters are invalid (region V1).")
       return(FALSE)
    }
    if(La3 > 0 && La3 < 1 && La4 < 0) { # REGION V2
       warning("Parameters are invalid (region V2).")
       return(FALSE)
    }
    if(La3 > -1 && La3 < 0 && La4 > 1) { # REGION V3
       tmp1 <- (1-La3)**(1-La3)
       tmp2 <- (La4-La3)**(La4-La3)
       tmp3 <- (La4-1)**(La4-1)
       rhs  <- -La3/La4
       if(tmp3*(tmp1/tmp2) < rhs) {
         return(TRUE)
       }
       else {
         warning("Parameters are invalid (region V3).")
         return(FALSE)
       }
    }
    if(La3 > 1 && La4 > -1 && La4 < 0) { # REGION V4
       # Unclear in Karian and Dudewicz (2000) that
       # the following same condition on V3 applies
       # in V4.  See top of page 16. However, this basic
       # test is also stated on page 17 to be an if and only if
       tmp1 <- (1-La3)**(1-La3)
       tmp2 <- (La4-La3)**(La4-La3)
       tmp3 <- (La4-1)**(La4-1)
       rhs  <- -La3/La4
       if(tmp3*(tmp1/tmp2) < rhs) {
         return(TRUE)
       }
       else {
         warning("Parameters are invalid (region V4).")
         return(FALSE)
       }
    }
    return(TRUE)
}

"are.parglo.valid" <-
function(para) {
    if(! is.glo(para)) return(FALSE)
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(A <= 0 | abs(K) >= 1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.pargno.valid" <-
function(para) {
    if(! is.gno(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
       warning("Parameters are invalid.")
       return(FALSE)
    } 
    return(TRUE)
}

"are.pargpa.valid" <-
function(para) {
    if(! is.gpa(para)) return(FALSE)
    A <- para$para[2]
    K <- para$para[3]
    if(A <= 0 | K < -1) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    return(TRUE)
}

"are.pargum.valid" <-
function(para) {
    if(! is.gum(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.parkap.valid" <-
function(para) {
    if(! is.kap(para)) return(FALSE)
    if(para$ifail == 2) return(FALSE)
    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    if(G <= -1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    if(H < 0 & G*H <= -1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.parnor.valid" <-
function(para) {
    if(! is.nor(para)) return(FALSE)
    sd <- para$para[2]
    if(sd <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.parpe3.valid" <-
function(para) {
    if(! is.pe3(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

"are.parwak.valid" <-
function(para) {
    if(! is.wak(para)) return(FALSE)

    A <- para$para[2]
    B <- para$para[3]
    C <- para$para[4]
    D <- para$para[5]
    if(B+D <= 0 & (B != 0 | C != 0 | D != 0)) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    if(A == 0 & B != 0) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    if(C == 0  & D != 0) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    if(C < 0 | A+C < 0) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    if(A == 0 & C == 0) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    if(D >= 1) {
       warning("Parameters are invalid.")
       return(FALSE)
    }
    return(TRUE)
}

"cdfcau" <-
function(x,para) {
    if(! are.parcau.valid(para)) return()
    U <- para$para[1] 
    A <- para$para[2] 
    tmp <- (x - U)/A
    tmp <- (atan(tmp)/pi)+0.5
    return(tmp)
}

"cdfexp" <-
function(x,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]
    Y <- (x-U)/A
    if(Y <= 0) return(0)
    return(1-exp(-Y))
}

"cdfgam" <-
function(x,para) {
    if(! are.pargam.valid(para)) return()
    if(x <= 0) return(0)
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    return(pgamma(x,ALPHA,scale=BETA))
}

"cdfgev" <-
function(x,para) {
    if(! are.pargev.valid(para)) return()
    # SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT 
    # THE ENDPOINT OF THE DISTRIBUTION 
    SMALL <- 1e-15 

    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y  <- (x - XI)/A
    if(K == 0) return(exp(-exp(-Y)))
    ARG <- 1-K*Y 
    if(ARG > SMALL) {
      Y <- -log(ARG)/K
      return(exp(-exp(-Y)))
    } 
    if(K < 0) return(0)
    # K must be greater than zero to return other end 
    return(1)
}

"cdfglo" <-
function(x,para) {
    if(! are.parglo.valid(para)) return()
    SMALL <- 1e-15 
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y  <- (x-XI)/A 
    if(K == 0) {
      return(1/(1+exp(-Y)))
    }
    ARG <- 1-K*Y 
    if(ARG > SMALL) {
      Y <- -log(ARG)/K
      return(1/(1+exp(-Y)))
    }
    if(K < 0) return(0)
    if(K > 0) return(1)
}

"cdfgno" <-
function(x,para) {
    # Error function from R documentation
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    RTHALF <- 0.707106781186547524
    #  SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT
    #  THE ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15

    if(! are.pargno.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]
    K  <- para$para[3]
    Y <- (x-XI)/A
    if(K != 0) {
      ARG <- 1-K*Y
      if(ARG > SMALL) {
        Y <- -log(ARG)/K
      }
      else {
        if(K < 0) return(0)
        # K must be greater than zero--other end of distribution
        return(1)
      }
    }
    return(0.5+0.5*erf(Y*RTHALF))
}

"cdfgpa" <-
function(x,para) {
    if(! are.pargpa.valid(para)) return()
    #  SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT
    #  THE ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y <- (x-XI)/A
    if(Y <= 0) return(0)
    if(K == 0) {
      return(1-exp(-Y))
    }
    else {
      ARG <- 1-K*Y
      if(ARG > SMALL) {
        Y <- -log(ARG)/K
        return(1-exp(-Y))
      }
      return(1)
    }
}

"cdfgum" <-
function(x,para) { 
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 
   Y <- (x-U)/A 
   return(exp(-exp(-Y))) 
}

"cdfkap" <-
function(x,para) {
    if(! are.parkap.valid(para)) return()

    #  SMALL IS A SMALL NUMBER, USED TO TEST WHETHER X IS
    #  EFFECTIVELY AT AN ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    Y <- (x-U)/A
    if(G == 0) {
      Y <- exp(-Y)
    }
    else {
      ARG <- 1-G*Y
      if(ARG > SMALL) { 
        Y <- -log(ARG)/G
        Y <- exp(-Y)
      }
      else {
        if(G < 0) return(0)
        if(G > 0) return(1)
        stop("should not be here in execution")
      }
   }
   if(H == 0) {
     return(exp(-Y))
   }
   else {
     ARG <- 1-H*Y
     if(ARG > SMALL) {
       Y <- -log(ARG)/H
       return(exp(-Y))
     }
     else {
      return(0)
     }
   }
}

"cdfnor" <-
function(x,para) {
    if(! are.parnor.valid(para)) return()
    return(pnorm(x,mean = para$para[1], sd = para$para[2]))
}

"cdfpe3" <-
function(x,para) {
    if(! are.parpe3.valid(para)) return()

    ROOT0p5 <- sqrt(1/2)

    # Error function as defined by R documentation
    #   and is used for zero skew condition
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    U <- para$para[1]
    A <- para$para[2]
    GAMMA <- para$para[3]

    if(abs(GAMMA) <= SMALL) { 
      # ZERO SKEWNESS
      Z <- (x-U)/A
      return(0.5+0.5*erf(Z*ROOT0p5))
    }
    ALPHA <- 4/GAMMA^2
    Z <- 2*(x-U)/(A*GAMMA)+ALPHA
    CDFPE3 <- 0
    if(Z     > 0)  CDFPE3 <- pgamma(Z,ALPHA)
    if(GAMMA < 0 ) CDFPE3 <- 1-CDFPE3
    return(CDFPE3)
}

"cdfwak" <-
function(x,wakpara) {

    # CONVERT Z TO PROBABILITY
    z2f <- function(Z,UFL) {
      if(-Z < UFL) return(1)
      return(1-exp(-Z))
    }


    #  METHOD: THE EQUATION X=G(Z), WHERE G(Z) IS THE WAKEBY QUANTILE
    #  EXPRESSED AS A FUNCTION OF Z=-LOG(1-F), IS SOLVED USING HALLEY'S
    #  METHOD (THE 2ND-ORDER ANALOGUE OF NEWTON-RAPHSON ITERATION).
    #

    #
    #         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF THE ITERATION
    #         ZINCMX IS THE LARGEST PERMITTED ITERATIVE STEP
    #         ZMULT CONTROLS WHAT HAPPENS WHEN THE ITERATION STEPS BELOW ZERO
    #         UFL SHOULD BE CHOSEN SO THAT DEXP(UFL) JUST DOES NOT CAUSE
    #           UNDERFLOW

    EPS    <- 1e-8;
    MAXIT  <- 20;
    ZINCMX <- 3;
    ZMULT  <- 0.2;
    UFL    <- log(.Machine$double.xmin);

    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A  <- wakpara$para[2]
    B  <- wakpara$para[3]
    C  <- wakpara$para[4]
    D  <- wakpara$para[5]

    if(x <= XI) return(0)

    #
    #         TEST FOR SPECIAL CASES
    #
    if(B == 0 & C == 0 & D == 0) {
      #  SPECIAL CASE B=C=D=0: WAKEBY IS EXPONENTIAL
      Z <- (x-XI)/A
      return(z2f(Z,UFL))
    }
    if(C == 0) {
      #  SPECIAL CASE C=0: WAKEBY IS GENERALIZED PARETO, BOUNDED ABOVE
      CDFWAK <- 1
      if(x >= XI+A/B) return(1)
      Z <- -log(1-(x-XI)*B/A)/B
      return(z2f(Z,UFL))
    }
    if(A == 0) {
      #  SPECIAL CASE A=0: WAKEBY IS GENERALIZED PARETO, NO UPPER BOUND
      Z <- log(1+(x-XI)*D/C)/D
      return(z2f(Z,UFL))
    }


    #         GENERAL CASE
    #
    if(D < 0 & x >= XI+A/B-C/D) return(1)

    # INITIAL VALUES FOR ITERATION:
    #   IF X IS IN THE LOWEST DECILE OF THE DISTRIBUTION,
    #     START AT Z = 0 (F = 0);
    #   IF X IS IN THE HIGHEST PERCENTILE OF THE DISTRIBUTION,
    #   STARTING VALUE IS OBTAINED FROM ASYMPTOTIC FORM OF THE
    #   DISTRIBUTION FOR LARGE Z (F NEAR 1);
    #   OTHERWISE START AT Z <- 0.7 (CLOSE TO F <- 0.5).
    #
    Z <- 0.7
    if(x < quawak(0.1,wakpara)) Z <- 0
    if(x >= quawak(0.99,wakpara)) {
      if(D <  0) Z <- log((x-XI-A/B)*D/C+1)/D
      if(D == 0) Z <- (x-XI-A/B)/C
      if(D >  0) Z <- log((x-XI)*D/C+1)/D
    }
    #
    #  HALLEY'S METHOD, WITH MODIFICATIONS:
    #  IF HALLEY ITERATION WOULD MOVE IN WRONG DIRECTION
    #   (TEMP <= ZERO), USE ORDINARY NEWTON-RAPHSON INSTEAD;
    #   IF STEP GOES TOO FAR (ZINC > ZINCMX | ZNEW <= 0),
    #   LIMIT ITS LENGTH.
    #

    LOOPEND <- FALSE

    for(IT in seq(1,MAXIT)) {
      EB <- 0
      BZ <- -B*Z
      if(BZ >= UFL) EB <- exp(BZ)
      GB <- Z
      if(abs(B) > EPS) GB <- (1-EB)/B
      ED <- exp(D*Z)
      GD <- -Z
      if(abs(D) > EPS) GD <- (1-ED)/D
      XEST <- XI+A*GB-C*GD
      FUNC <- x-XEST
      DERIV1 <- A*EB+C*ED
      DERIV2 <- -A*B*EB+C*D*ED
      TEMP <- DERIV1+0.5*FUNC*DERIV2/DERIV1
      if(TEMP <= 0) TEMP <- DERIV1
      ZINC <- FUNC/TEMP
      if(ZINC > ZINCMX) ZINC <- ZINCMX
      ZNEW <- Z+ZINC
      if(ZNEW <= 0) { 
        Z <- Z*ZMULT
        next
      }
      Z <- ZNEW
      if(abs(ZINC) <= EPS) break
      if(IT == MAXIT) LOOPEND <- TRUE
    }
    if(LOOPEND == TRUE) {
      warning("Iteration has not converged. Result might be unreliable.")
    }

    # CONVERT Z VALUE TO PROBABILITY
    return(z2f(Z,UFL))
}

"freq.curve.all" <-
function(lmom) {
    F <- nonexceeds()
    print("Exponential distribution")
    EXP <- freq.curve.exp(F,parexp(lmom))
    print("Gamma distribution")
    GAM <- freq.curve.gam(F,pargam(lmom))
    print("Generalized Extreme Value distribution")
    GEV <- freq.curve.gev(F,pargev(lmom))
    print("Generalized Logistic distribution")
    GLO <- freq.curve.glo(F,parglo(lmom))
    print("Generalized Normal distribution")
    GNO <- freq.curve.gno(F,pargno(lmom))
    print("Generalized Pareto distribution")
    GPA <- freq.curve.gpa(F,pargpa(lmom))
    print("Generalized Gumbel distribution")
    GUM <- freq.curve.gum(F,pargum(lmom))
    print("Kappa distribution")
    KAP <- freq.curve.kap(F,parkap(lmom))
    print("Normal distribution")
    NOR <- freq.curve.nor(F,parnor(lmom))
    print("Pearson Type III distribution")
    PE3 <- freq.curve.pe3(F,parpe3(lmom))
    print("Wakeby distribution")
    WAK <- freq.curve.wak(F,parwak(lmom))
    return(list(exp = EXP, gam = GAM, gev = GEV, glo = GLO,
                gno = GNO, gpa = GPA, gum = GUM, kap = KAP,
                nor = NOR, pe3 = PE3, wak = WAK))
}

"freq.curve.cau" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.cau(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quacau(fs[i],para)
  }
  return(Q)
}

"freq.curve.exp" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.exp(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quaexp(fs[i],para)
  }
  return(Q)
}

"freq.curve.gam" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gam(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagam(fs[i],para)
  }
  return(Q)
}

"freq.curve.gev" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gev(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagev(fs[i],para)
  }
  return(Q)
}

"freq.curve.gld" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gld(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagld(fs[i],para)
  }
  return(Q)
}

"freq.curve.glo" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.glo(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quaglo(fs[i],para)
  }
  return(Q)
}

"freq.curve.gno" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gno(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagno(fs[i],para)
  }
  return(Q)
}

"freq.curve.gpa" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gpa(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagpa(fs[i],para)
  }
  return(Q)
}

"freq.curve.gum" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gum(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagum(fs[i],para)
  }
  return(Q)
}

"freq.curve.kap" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.kap(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quakap(fs[i],para)
  }
  return(Q)
}

"freq.curve.nor" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.nor(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quanor(fs[i],para)
  }
  return(Q)
}

"freq.curve.pe3" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.pe3(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quape3(fs[i],para)
  }
  return(Q)
}

"freq.curve.wak" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.wak(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quawak(fs[i],para)
  }
  return(Q)
}

"is.cau" <-
function(para) {
    if(para$type != "cau") {
      warning("Parameters are not Cauchy parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.exp" <-
function(para) {
    if(para$type != "exp") {
      warning("Parameters are not exponential parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gam" <-
function(para) {
    if(para$type != "gam") {
      warning("Parameters are not gamma parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gev" <-
function(para) {
    if(para$type != "gev") {
      warning("Parameters are not Generalized Extreme Value parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gld" <-
function(para) {
    if(para$type != "gld") {
      warning("Parameters are not Generalized Lambda parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.glo" <-
function(para) {
    if(para$type != "glo") {
      warning("Parameters are not Generalized Logistic parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gno" <-
function(para) {
    if(para$type != "gno") {
      warning("Parameters are not Generalized Normal parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gpa" <-
function(para) {
    if(para$type != "gpa") {
      warning("Parameters are not Generalized Pareto parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.gum" <-
function(para) {
    if(para$type != "gum") {
      warning("Parameters are not Gumbel parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.kap" <-
function(para) {
    if(para$type != "kap") {
      warning("Parameters are not Kappa parameters.")
      return(FALSE)
    }
    return(TRUE)
}

"is.nor" <-
function(para) {
    if(para$type != "nor") {
      warning("Parameters are not Normal parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.pe3" <-
function(para) {
    if(para$type != "pe3") {
      warning("Parameters are not Pearson Type III parameters")
      return(FALSE)
    }
    return(TRUE)
}

"is.wak" <-
function(para) {
    if(para$type != "wak") {
      warning("Parameters are not Wakeby parameters")
      return(FALSE)
    }
    return(TRUE)
}

"lmom.diff" <-
function(lmomparm, lmomdata) {
   print("THE FIVE DIFFERENCES BETWEEN L-MOMENTS OF DISTRIBUTION AND DATA")
   print("Mean  L2   TAU3   TAU4   TAU5")
   L1diff <- lmomparm$L1 - lmomdata$L1
   L2diff <- lmomparm$L2 - lmomdata$L2
   T3diff <- lmomparm$TAU3 - lmomdata$TAU3
   T4diff <- lmomparm$TAU4 - lmomdata$TAU4
   T5diff <- lmomparm$TAU5 - lmomdata$TAU5
   print(c(L1diff,L2diff,T3diff,T4diff,T5diff))
   return(list(L1diff = L1diff, L2diff = L2diff, T3diff = T3diff,
               T4diff = T4diff, T5diff = T5diff))
}

"lmom.test.all" <-
function(data) {
    lmom.test.exp(data)
    lmom.test.gam(data)
    lmom.test.gev(data)
    lmom.test.glo(data)
    lmom.test.gno(data)
    lmom.test.gpa(data)
    lmom.test.gum(data)
    lmom.test.kap(data)
    lmom.test.nor(data)
    lmom.test.pe3(data)
    lmom.test.wak(data)
}

"lmom.test.exp" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parexp(lmom)
   print("EXPONENTIAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomexp(para)
   Q50 <- quaexp(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfexp(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.gam" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargam(lmom)
   print("GAMMA DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgam(para)
   Q50 <- quagam(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgam(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.gev" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargev(lmom)
   print("GENERALIZED EXTREME VALUE DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgev(para)
   Q50 <- quagev(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgev(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.glo" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parglo(lmom)
   print("GENERALIZED LOGISTIC DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomglo(para)
   Q50 <- quaglo(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfglo(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.gno" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargno(lmom)
   print("GENERALIZED NORMAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgno(para)
   Q50 <- quagno(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgno(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.gpa" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargpa(lmom)
   print("GENERALIZED PARETO DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgpa(para)
   Q50 <- quagpa(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgpa(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.gum" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargum(lmom)
   print("GUMBEL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgum(para)
   Q50 <- quagum(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgum(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.kap" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parkap(lmom)
   print("GENERALIZED KAPPA DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomkap(para)
   Q50 <- quakap(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfkap(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.nor" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parnor(lmom)
   print("NORMAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomnor(para)
   Q50 <- quanor(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfnor(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.pe3" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parpe3(lmom)
   print("PEARSON TYPE III DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmompe3(para)
   Q50 <- quape3(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfpe3(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.test.wak" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parwak(lmom)
   print("WAKEBY DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomwak(para)
   Q50 <- quawak(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfwak(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

"lmom.ub" <-
function(x) {
  n <- length(x)
  if(n == 1) stop("use mean() for data with one value")
  if(n < 5)  stop("a minimum of 5 data values are required
                   because 5 lmoments are to be computed")
  if(length(unique(x))  == 1) stop("all values are equal--lmoments can not be computed")
  x <- sort(x)
  L1 = 0; L2 = 0; L3 = 0; L4 = 0; L5 = 0
  for(i in seq(1,n)) {
    CL1 <- i-1
    CL2 <- CL1 * (i-1-1) / 2
    CL3 <- CL2 * (i-1-2) / 3
    CL4 <- CL3 * (i-1-3) / 4
    CR1 <- n-i
    CR2 <- CR1 * (n-i-1) / 2
    CR3 <- CR2 * (n-i-2) / 3
    CR4 <- CR3 * (n-i-3) / 4     
    L1  <- L1 + x[i]
    L2  <- L2 + x[i] * (CL1 - CR1)
    L3  <- L3 + x[i] * (CL2 - 2*CL1*CR1 + CR2)
    L4  <- L4 + x[i] * (CL3 - 3*CL2*CR1 + 3*CL1*CR2 - CR3)
    L5  <- L5 + x[i] * (CL4 - 4*CL3*CR1 + 6*CL2*CR2 - 4*CL1*CR3 + CR4)    
  }
  
  C1 <- n
  C2 <- C1 * (n-1) / 2
  C3 <- C2 * (n-2) / 3
  C4 <- C3 * (n-3) / 4
  C5 <- C4 * (n-4) / 5
  L1 <- L1 / C1
  L2 <- L2 / C2 / 2
  L3 <- L3 / C3 / 3
  L4 <- L4 / C4 / 4
  L5 <- L5 / C5 / 5
  z <- list(L1 = L1, L2 = L2, TAU3 = L3/L2, TAU4 = L4/L2, TAU5 = L5/L2,
            LCV = L2/L1, L3 = L3, L4 = L4, L5=L5,
            )
  return(z)
}

"lmom2par" <-
function(lmom,type) {
    if(type == 'exp') {
      return(parexp(lmom))
    }
    else if(type == 'gam') {
      return(pargam(lmom))
    }
    else if(type == 'gev') {
      return(pargev(lmom))
    }
    else if(type == 'glo') {
      return(parglo(lmom))
    }
    else if(type == 'gno') {
      return(pargno(lmom))
    }
    else if(type == 'gpa') {
      return(pargpa(lmom))
    }
    else if(type == 'gum') {
      return(pargum(lmom))
    }
    else if(type == 'kap') {
      return(parkap(lmom))
    }
    else if(type == 'nor') {
      return(parnor(lmom))
    }
    else if(type == 'pe3') {
      return(parpe3(lmom))
    }
    else if(type == 'wak') {
      return(parwak(lmom))
    }
    else {
      stop("Do not find a valid distribution type.")
    }
}

"lmom2pwm" <-
function(lmom) {
   p0 = lmom$L1
   p1 = 0.5*(lmom$L2+p0)
   p2 = (1/6)*(lmom$L2*lmom$TAU3+6*p1-p0)
   p3 = (1/20)*(lmom$L2*lmom$TAU4+30*p2-12*p1+p0)
   p4 = (1/70)*(lmom$L2*lmom$TAU5+140*p3-90*p2+20*p1-p0)
   z <- list(BETA0 = p0, BETA1 = p1, BETA2 = p2, BETA3 = p3, BETA4 = p4)
   return(z)
}

"lmomexp" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    if(! are.parexp.valid(para)) return()
    A <- para$para[2]
    z$L1 <- para$para[1]+A
    z$L2 <- 0.5*A
    z$TAU3 <- 2/(3*(2))
    z$TAU4 <- 2/(4*(3))
    z$TAU5 <- 2/(5*(4))
    z$L3 <- z$TAU3*z$L2
    z$L4 <- z$TAU4*z$L2
    z$L5 <- z$TAU5*z$L2
    return(z)
}

"lmomgam" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    # Note that TAU5 and L5 are not available from Hosking's FORTRAN base.

    # CONST IS 1/sqrt(pi) 
    CONST <- 0.564189583547756287
     
    # COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS 
    #   A0 IS 1/sqrt(3*pi) 
    #   C0 IS TAU-4 FOR THE NORMAL DISTRIBUTION 
    A0 <- 0.32573501 
    A1 <- 0.16869150;     A2 <- 0.78327243e-01; A3 <- -0.29120539e-02 
    B1 <- 0.46697102;     B2 <- 0.24255406 
    C0 <- 0.12260172; 
    C1 <- 0.53730130e-01; C2 <- 0.43384378e-01; C3 <- 0.11101277e-01 
    D1 <- 0.18324466;     D2 <- 0.20166036e+00 
    E1 <- 0.23807576e+01; E2 <- 0.15931792e+01; E3 <- 0.11618371e+00
    F1 <- 0.51533299e+01; F2 <- 0.71425260e+01; F3 <- 0.19745056e+01
    G1 <- 0.21235833e+01; G2 <- 0.41670213e+01; G3 <- 0.31925299e+01
    H1 <- 0.90551443e+01; H2 <- 0.26649995e+02; H3 <- 0.26193668e+02
 
    if(! are.pargam.valid(para)) return()

    ALPHA <- para$para[1] 
    BETA  <- para$para[2]
    z$L1 <- ALPHA*BETA 
    z$L2 <- BETA*CONST*exp(lgamma(ALPHA + 0.5) - lgamma(ALPHA)) 

    if(ALPHA < 1) {
      Z <- ALPHA 
      z$TAU3 <- (((E3*Z+E2)*Z+E1)*Z+1)/(((F3*Z+F2)*Z+F1)*Z+1) 
      z$TAU4 <- (((G3*Z+G2)*Z+G1)*Z+1)/(((H3*Z+H2)*Z+H1)*Z+1) 
    }
    else {
      Z <- 1/ALPHA 
      z$TAU3 <- sqrt(Z)*(((A3*Z+A2)*Z+A1)*Z+A0)/((B2*Z+B1)*Z+1) 
      z$TAU4 <- (((C3*Z+C2)*Z+C1)*Z+C0)/((D2*Z+D1)*Z+1)
    }
    z$LCV <- z$L2/z$L1
    z$L3  <- z$TAU3*z$L2
    z$L4  <- z$TAU4*z$L2
    return(z)
}

"lmomgev" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    lmom <- matrix(nrow = 5, ncol = 1)

    # ARRAY ZMOM CONTAINS THE L-MOMENT RATIOS OF THE STANDARD
    #  GUMBEL DISTRIBUTION (XI=0, ALPHA=1).
    #  ZMOM(1) IS EULER'S CONSTANT, ZMOM(2) IS LOG(2).
    ZMOM <- c(0.577215664901532861,
              0.693147180559945309,
              0.169925001442312363,
              0.150374992788438185,
              0.558683500577583138e-1)

    #  SMALL IS USED TO TEST WHETHER K IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    if(! are.pargev.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]

    if(abs(G) <= SMALL) {
      z$L1   <- U
      z$L2   <- A*ZMOM[2]
      z$LCV  <- z$L2/z$L1
      z$TAU3 <- ZMOM[3]
      z$TAU4 <- ZMOM[4]
      z$TAU5 <- ZMOM[5]
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
      return(z)
    }
    else {
      GAM  <- exp(lgamma(1+G))
      z$L1 <- U+A*(1-GAM)/G
      XX2  <- 1-2^(-G)
      z$L2 <- A*XX2*GAM/G
      Z0   <- 1
      for(J in seq(3,5)) {
        BETA <- (1-J^(-G))/XX2
        Z0 <- Z0*(4*J-6)/J
        Z <- Z0*3*(J-1)/(J+1)
        SUM <- Z0*BETA-Z
        if(J > 3) {
          for(I in seq(2,J-2)) {
            Z <- Z*(I+I+1)*(J-I)/((I+I-1)*(J+I))
            SUM <- SUM-Z*lmom[I+1]
          }
        }
        lmom[J] = SUM
      }
    }
    z$LCV  <- z$L2/z$L1
    z$TAU3 <- lmom[3]
    z$TAU4 <- lmom[4]
    z$TAU5 <- lmom[5]
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmomglo" <-
function(para) {
    # function derived partially from Hosking and Wallis (1997) for K != 0
    # and from Hosking's FORTRAN library for K near or equal to zero.
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    # fifth L-moment is not given by Hosking and Wallis (1997)
    #  SMALL IS USED TO DECIDE WHETHER TO APPROXIMATE THE FIRST 2 
    #  L-MOMENTS BY A POWER-SERIES EXPANSION WHEN G IS NEAR ZERO. 
    #  C1,C2 ARE COEFFICIENTS OF THIS POWER-SERIES EXPANSION. 
    #  C1 IS pi^2/6, C2 IS 7*pi^4/360. 
    SMALL <- 1e-4
    C1 <- 0.164493406684822644e1; C2 <- 0.189406565899449184e1 

    if(! are.parglo.valid(para)) return()

    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    KK <- K*K 
    ALAM1 <- -K*(C1+KK*C2) 
    ALAM2 <- 1+KK*(C1+KK*C2) 
    if(abs(K) > SMALL) ALAM2 <- K*pi/sin(K*pi) 
    if(abs(K) > SMALL) ALAM1 <- (1-ALAM2)/K 
    z$L1   <- XI+A*ALAM1 
    z$L2   <- A*ALAM2 
    z$TAU3 <- -K
    z$TAU4 <- (1+5*K^2)/6
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    return(z)
}

"lmomgno" <-
function(para) {
    # function derived partially from Hosking and Wallis (1997) for K != 0
    # and from Hosking's FORTRAN library for K near or equal to zero.
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

    SUM <- matrix(nrow = 5, ncol = 1)
    for(i in seq(1,5)) SUM[i] <- 0
    EST <- matrix(nrow = 5, ncol = 1)
    for(i in seq(1,5)) EST[i] <- 0
    ESTX <- matrix(nrow = 5, ncol = 1)
    for(i in seq(1,5)) ESTX[i] <- 0


    #  ARRAY ZMOM CONTAINS L-MOMENTS OF THE STANDARD NORMAL DIST.
    ZMOM <- c( 0, 0.564189583547756287,
               0, 0.122601719540890947,
               0)

    #  RRT2 IS 1/SQRT(2), RRTPI IS 1/SQRT(PI)
    RRT2  <- 1/sqrt(2)
    RRTPI <- 1/sqrt(pi)

    #  RANGE,EPS,MAXIT CONTROL THE ITERATIVE PROCEDURE FOR NUMERICAL INTEGRATION
    RANGE <- 5
    EPS   <- 1e-8
    MAXIT <- 10

    if(! are.pargno.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]

    # TEST FOR K=0
    if(abs(G) <= EPS) {
      # K is zero
      z$L1   <- U
      z$L2   <- A*ZMOM[2]
      z$TAU3 <- ZMOM[3]
      z$TAU4 <- ZMOM[4]
      z$TAU5 <- ZMOM[5]
      z$LCV  <- z$L2/z$L1
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
      return(z)
    }

    # LAMBDA-1
    EGG   <- exp(0.5*G^2)
    ALAM1 <- (1-EGG)/G
    z$L1  <- U+A*ALAM1

    # LAMBDA-2
    ALAM2 <- EGG*erf(0.5*G)/G
    z$L2 <- A*ALAM2

    #     HIGHER MOMENTS. THE INTEGRAL DEFINING LAMBDA-R IS EVALUATED
    #         BY ITERATIVE APPLICATION OF THE TRAPEZIUM RULE.
    #
    #         - INITIAL ESTIMATE, USING 16 ORDINATES  (THE 'DO 20' LOOP
    #           CALCULATES LEGENDRE POLYNOMIALS RECURSIVELY)
    CC   <- -G*RRT2
    XMIN <- CC-RANGE
    XMAX <- CC+RANGE
    
    N <- 16
    XINC <- (XMAX-XMIN)/N
    for(i in seq(1,N-1)) {         
      X  <- XMIN+i*XINC
      E  <- exp(-((X-CC)^2))
      D  <- erf(X)
      P1 <- 1
      P  <- D
      for(m in seq(3,5)) {  
        C1 <- m+m-3
        C2 <- m-2
        C3 <- m-1
        P2 <- P1
        P1 <- P
        P  <- (C1*D*P1-C2*P2)/C3
        SUM[m] <- SUM[m]+E*P
      }
    }
    EST[3] <- SUM[3]*XINC
    EST[4] <- SUM[4]*XINC
    EST[5] <- SUM[5]*XINC

    #  -  DOUBLE THE NUMBER OF ORDINATES UNTIL CONVERGED
    for(it in seq(1,MAXIT)) {

      ESTX[3] <- EST[3]
      ESTX[4] <- EST[4]
      ESTX[5] <- EST[5]

      N <- N*2
      XINC <- (XMAX - XMIN)/N
      for(i in seq(1,N-1,2)) {
        X  <- XMIN+i*XINC
        E  <- exp(-((X-CC)^2))
        D  <- erf(X)
        P1 <- 1
        P  <- D
        for(m in seq(3,5)) {
          C1 <- m+m-3
          C2 <- m-2
          C3 <- m-1
          P2 <- P1
          P1 <- P
          P  <- (C1*D*P1-C2*P2)/C3
          SUM[m] <- SUM[m]+E*P
        }
      }
   
      #  --- TEST FOR CONVERGENCE
      NOTCGD <- 0
      for(m in seq(5,3,-1)) {
        EST[m] <- SUM[m]*XINC
        if(abs(EST[m]-ESTX[m]) > EPS*abs(EST[m])) NOTCGD <- m
      }
      if(NOTCGD == 0) break
    }
    if(NOTCGD != 0) {
       warning(c("ITERATION HAS NOT CONVERGED. ONLY THE FIRST ",NOTCGD-1,
                 " L-MOMENTS ARE RELIABLE"))
    }    
    CONST  <- -exp(CC*CC)*RRTPI/(ALAM2*G)  
    z$TAU3 <- CONST*EST[3]
    z$TAU4 <- CONST*EST[4]
    z$TAU5 <- CONST*EST[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmomgpa" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )

    if(! are.pargpa.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 

    #  LAMBDA-1
    Y <- 1/(1+K)
    z$L1 <- XI+A*Y

    #  LAMBDA-2
    Y <- Y/(2+K)
    z$L2 <- A*Y

    #  HIGHER MOMENTS
    x <- matrix(nrow = 5, ncol = 1)
    Y <- 1
    for(m in seq(3,5)) {
      AM   <- m-2
      Y    <- Y*(AM-K)/(m+K)
      x[m] <- Y
    }
    z$TAU3 <- x[3]
    z$TAU4 <- x[4]
    z$TAU5 <- x[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmomgum" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    #  ARRAY ZMOM CONTAINS THE L-MOMENT RATIOS OF THE STANDARD 
    #  GUMBEL DISTRIBUTION (XI=0, ALPHA=1). 
    #  ZMOM(1) IS EULER'S CONSTANT, ZMOM(2) IS LOG(2). 
    #
    ZMOM <- c(0.577215664901532861,
              0.693147180559945309, 
              0.169925001442312363,
              0.150374992788438185, 
              0.558683500577583138e-1)
 
    A <- para$para[2] 
    z$L1 <- para$para[1] + A*ZMOM[1] 
    z$L2 <- A*ZMOM[2] 
    z$TAU3 <- ZMOM[3]
    z$TAU4 <- ZMOM[4]
    z$TAU5 <- ZMOM[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmomkap" <-
function(para) {
    if(! are.parkap.valid(para)) return()
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    BETA <- matrix(nrow = 5, ncol = 1)

    # SMALL IS USED TO TEST WHETHER H IS EFFECTIVELY ZERO
    SMALL <- 1e-8

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]


    #   CALCULATE FUNCTIONS OCCURRING IN THE PWM'S BETA-SUB-R
    ICASE <- 1
    if(H > 0)          ICASE <- 3
    if(abs(H) < SMALL) ICASE <- 2
    if(G == 0)         ICASE <- ICASE+3
    if(ICASE == 1) BETA <- INT.kapicase1(U,A,G,H)
    if(ICASE == 2) BETA <- INT.kapicase2(U,A,G,H)
    if(ICASE == 3) BETA <- INT.kapicase3(U,A,G,H)
    if(ICASE == 4) BETA <- INT.kapicase4(U,A,G,H)
    if(ICASE == 5) BETA <- INT.kapicase5(U,A,G,H)
    if(ICASE == 6) BETA <- INT.kapicase6(U,A,G,H)

    #         LAMBDA-1
    if(G == 0) {
      z$L1 <- U+A*BETA[1]
    }
    else {
      z$L1 <- U+A*(1-BETA[1])/G
    }

    #         LAMBDA-2
    ALAM2 <- BETA[2]-BETA[1]
    if(G == 0) {
      z$L2 <- A*ALAM2
    }
    else {
      z$L2 <- A*ALAM2/(-G)
    }
    z$LCV <- z$L2 / z$L1
    #         HIGHER MOMENTS
    Z0 <- 1
    x <- matrix(nrow = 5, ncol = 1)
    for(J in seq(3,5)) {
      Z0 <- Z0*(4*J-6)/J
      Z <- Z0*3*(J-1)/(J+1)
      SUM <- Z0*(BETA[J]-BETA[1])/ALAM2-Z
      if(J == 3) {
        x[J] <- SUM
      }
      else {
        for(I in seq(2,J-2)) {
          Z <- Z*(I+I+1)*(J-I)/((I+I-1)*(J+I))
          SUM <- SUM-Z*x[I+1]
        }
        x[J] <- SUM
      }
    }
    z$TAU3 <- x[3]
    z$TAU4 <- x[4]
    z$TAU5 <- x[5]
    z$L3   <- z$TAU3*z$LCV
    z$L4   <- z$TAU4*z$LCV
    z$L5   <- z$TAU5*z$LCV
    return(z)
}

"lmomnor" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )

    if(! are.parnor.valid(para)) return()

    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

    #  ARRAY ZMOM CONTAINS L-MOMENTS OF THE STANDARD NORMAL DIST.
    ZMOM <- c( 0, 0.564189583547756287,
               0, 0.122601719540890947,
               0)

    #  RRT2 IS 1/SQRT(2), RRTPI IS 1/SQRT(PI)
    RRT2  <- 1/sqrt(2)
    RRTPI <- 1/sqrt(pi)

    z$L1   <- para$para[1]
    z$L2   <- para$para[2]*ZMOM[2]
    z$TAU3 <- ZMOM[3]
    z$TAU4 <- ZMOM[4]
    z$TAU5 <- ZMOM[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmompe3" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )

    if(! are.parpe3.valid(para)) return()

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    # CONST IS 1/SQRT(PI)
    CONST <- 1/sqrt(pi)

    #  COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS
    #    A0 IS 1/SQRT(3*PI)
    #    C0 IS TAU-4 FOR THE NORMAL DISTRIBUTION
    A0 <- 1/sqrt(3*pi)
    A1 <-  0.16869150
    A2 <-  0.78327243e-1
    A3 <- -0.29120539e-2
    B1 <-  0.46697102
    B2 <-  0.24255406
    C0 <-  0.12260172
    C1 <-  0.53730130e-1
    C2 <-  0.43384378e-1
    C3 <-  0.11101277e-1
    D1 <-  0.18324466
    D2 <-  0.20166036
    E1 <-  0.23807576e1
    E2 <-  0.15931792e1
    E3 <-  0.11618371
    F1 <-  0.51533299e1
    F2 <-  0.71425260e1
    F3 <-  0.19745056e1
    G1 <-  0.21235833e1
    G2 <-  0.41670213e1
    G3 <-  0.31925299e1
    H1 <-  0.90551443e1
    H2 <-  0.26649995e2
    H3 <-  0.26193668e2

    SD <- para$para[2]

    # LAMBDA-1
    z$L1 <- para$para[1]

    # LAMBDA-2
    GAMMA <- para$para[3]
    if(abs(GAMMA) < SMALL) {
      # CASE OF ZERO SKEWNESS
      z$L2 <- CONST*para[2]
      z$TAU3 <- 0
      z$TAU4 <- C0
      z$L3 <- z$L2*z$TAU3
      z$L4 <- z$L2*z$TAU4
      # NO TAU5 AVAILABLE
    }
    else { 
      ALPHA <- 4/(GAMMA*GAMMA)
      BETA <- abs(0.5*SD*GAMMA)
      ALAM2 <- CONST*exp(lgamma(ALPHA+0.5)-lgamma(ALPHA))
      z$L2 <- ALAM2*BETA

      #  HIGHER MOMENTS
      if(ALPHA < 1) {
        Z <- ALPHA
        z$TAU3 <- (((E3*Z+E2)*Z+E1)*Z+1)/(((F3*Z+F2)*Z+F1)*Z+1)
        if(GAMMA < 0) z$TAU3 <- -z$TAU3
        z$TAU4 <- (((G3*Z+G2)*Z+G1)*Z+1)/(((H3*Z+H2)*Z+H1)*Z+1)
        z$L3 <- z$L2*z$TAU3
        z$L4 <- z$L2*z$TAU4
      }
      else {
        Z <- 1/ALPHA
        z$TAU3 <- sqrt(Z)*(((A3*Z+A2)*Z+A1)*Z+A0)/((B2*Z+B1)*Z+1)
        if(GAMMA < 0) z$TAU3 <- -z$TAU3
        z$TAU4 <- (((C3*Z+C2)*Z+C1)*Z+C0)/((D2*Z+D1)*Z+1)
        z$L3 <- z$L2*z$TAU3
        z$L4 <- z$L2*z$TAU4
      }
    }
    return(z)
}

"lmomwak" <-
function(wakpara) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )

    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A  <- wakpara$para[2]
    B  <- wakpara$para[3]
    C  <- wakpara$para[4]
    D  <- wakpara$para[5]

    #  LAMBDA-1
    #
    Y <- A/(1+B)
    Z <- C/(1-D)
    z$L1 <- XI+Y+Z

    #  LAMBDA-2
    #
    Y <- Y/(2+B)
    Z <- Z/(2-D)
    ALAM2 <- Y+Z
    z$L2 <- ALAM2

    #  HIGHER MOMENTS
    #
    x <- matrix(nrow = 5, ncol = 1)
    for(M in seq(3,5)) {
      Y <- Y*(M-2-B)/(M+B)
      Z <- Z*(M-2+D)/(M-D)
      x[M] <- (Y+Z)/ALAM2
    }
    z$TAU3 <- x[3]
    z$TAU4 <- x[4]
    z$TAU5 <- x[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

"lmrdia" <-
function() {
   step = 0.005
   n = 1
   lim <- matrix(nrow = 401, ncol = 2)
   gpa <- matrix(nrow = 401, ncol = 2)
   for(t3 in seq(-1,1,step)) {
     lim[n,1] = t3
     lim[n,2] = 0.25*(5*t3^2 - 1)
     gpa[n,1] = t3
     gpa[n,2] = (t3*(1+5*t3))/(5+t3)
     n = n + 1
   }
   n = 1
   gev <- matrix(nrow = 582, ncol = 2)
   for(k in seq(-1,1,step)) {
     h = -k
     gev[n,1] = 2*(1-3^h)/(1-2^h) - 3
     gev[n,2] = (5*(1-4^h)-10*(1-3^h)+6*(1-2^h))/(1-2^h)
     n = n + 1
   }
   for(k in seq(1-step,10,0.05)) {
     h = -k
     gev[n,1] = 2*(1-3^h)/(1-2^h) - 3
     gev[n,2] = (5*(1-4^h)-10*(1-3^h)+6*(1-2^h))/(1-2^h)
     n = n + 1
   }
   n = 1
   glo <- matrix(nrow = 401, ncol = 2)
   for(k in seq(-1,1,step)) {
     glo[n,1] = -k
     glo[n,2] = (1+5*k^2)/6
     n = n + 1
   }

   n = 1
   pIII <- matrix(nrow = 361, ncol = 2)
   for(t3 in seq(-.9,.9,step)) {
     pIII[n,1] = t3
     pIII[n,2] = 0.1224+0.30115*t3^2+0.95812*t3^4-0.57488*t3^6+0.19383*t3^8
     n = n + 1
   }

   n = 1
   ln <- matrix(nrow = 361, ncol = 2)
   for(t3 in seq(-.9,.9,step)) {
     ln[n,1] = t3
     ln[n,2] = 0.12282+0.77518*t3^2+0.12279*t3^4-0.13638*t3^6+0.11368*t3^8
     n = n + 1
   }


   exp <- matrix(nrow = 1, ncol = 2)
   exp[1,] <- c(1/3,1/6)
   gum <- matrix(nrow = 1, ncol = 2)
   gum[1,] <- c(log(9/8)/log(2),(16*log(2)-10*log(3))/log(2))
   nor <- matrix(nrow = 1, ncol = 2)
   nor[1,] <- c(0,30*pi^-1*atan(sqrt(2))-9)
   uni <- matrix(nrow = 1, ncol = 2)
   uni[1,] <- c(0,0)
   z <- list(limits = lim, exp=exp, gev = gev, glo = glo,
             gpa=gpa, gum=gum, gno=ln, nor=nor,
             pe3=pIII, uniform=uni)
   return(z)
}

"nonexceeds" <-
function() {
 F <- c(0.01,0.02,0.04,0.05,0.10,0.15,0.20,0.25,0.3,0.4,0.5,
         0.6,0.7,0.8,0.85,,0.9,0.95,0.96,0.98,0.99,0.996,0.998)
}

"par2cdf" <-
function(x,para) {
    type <- para$type
    if(type == 'cau') {
      return(cdfcau(x,para))
    }
    else if(type == 'exp') {
      return(cdfexp(x,para))
    }
    else if(type == 'gam') {
      return(cdfgam(x,para))
    }
    else if(type == 'gev') {
      return(cdfgev(x,para))
    }
    else if(type == 'glo') {
      return(cdfglo(x,para))
    }
    else if(type == 'gno') {
      return(cdfgno(x,para))
    }
    else if(type == 'gpa') {
      return(cdfgpa(x,para))
    }
    else if(type == 'gum') {
      return(cdfgum(x,para))
    }
    else if(type == 'nor') {
      return(cdfnor(x,para))
    }
    else if(type == 'kap') {
      return(cdfkap(x,para))
    }
    else if(type == 'pe3') {
      return(cdfpe3(x,para))
    }
    else if(type == 'wak') {
      return(cdfwak(x,para))
    }
    else {
      stop("Do not find a valid distribution type.")
    }
}

"par2lmom" <-
function(para) {
    type <- para$type
    if(type == 'exp') {
      return(lmomexp(para))
    }
    else if(type == 'gam') {
      return(lmomgam(para))
    }
    else if(type == 'gev') {
      return(lmomgev(para))
    }
    else if(type == 'glo') {
      return(lmomglo(para))
    }
    else if(type == 'gno') {
      return(lmomgno(para))
    }
    else if(type == 'gpa') {
      return(lmomgpa(para))
    }
    else if(type == 'gum') {
      return(lmomgum(para))
    }
    else if(type == 'nor') {
      return(lmomnor(para))
    }
    else if(type == 'kap') {
      return(lmomkap(para))
    }
    else if(type == 'pe3') {
      return(lmompe3(para))
    }
    else if(type == 'wak') {
      return(lmomwak(para))
    }
    else {
      stop("Do not find a valid distribution type.")
    }
}

"par2qua" <-
function(f,para) {
    type <- para$type
    if(type == 'cau') {
      return(quacau(f,para))
    }
    else if(type == 'exp') {
      return(quaexp(f,para))
    }
    else if(type == 'gam') {
      return(quagam(f,para))
    }
    else if(type == 'gev') {
      return(quagev(f,para))
    }
    else if(type == 'glo') {
      return(quaglo(f,para))
    }
    else if(type == 'gno') {
      return(quagno(f,para))
    }
    else if(type == 'gpa') {
      return(quagpa(f,para))
    }
    else if(type == 'gum') {
      return(quagum(f,para))
    }
    else if(type == 'nor') {
      return(quanor(f,para))
    }
    else if(type == 'kap') {
      return(quakap(f,para))
    }
    else if(type == 'pe3') {
      return(quape3(f,para))
    }
    else if(type == 'wak') {
      return(quawak(f,para))
    }
    else {
      stop("Do not find a valid distribution type.")
    }
}

"parexp" <-
function(lmom) {
    para <- matrix(nrow = 2, ncol = 1)
    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }
    para[2] <- 2*lmom$L2
    para[1] <- lmom$L1 - para[2]
    return(list(type = 'exp', para = para))
}

"pargam" <-
function(lmom) {
    para <- matrix(nrow = 2, ncol = 1)
    # METHOD: RATIONAL APPROXIMATION IS USED TO EXPRESS ALPHA AS A FUNCTION
    # OF L-CV. RELATIVE ACCURACY OF THE  APPROXIMATION IS BETTER THAN 5E-5.
    #
    #  CONSTANTS USED IN MINIMAX APPROXIMATIONS
    #
    A1 <- -0.3080; A2 <- -0.05812; A3 <-  0.01765
    B1 <-  0.7213; B2 <- -0.5947;  B3 <- -2.1817; B4 <- 1.2113
    
    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }

    if(lmom$LCV >= 0.5) { 
      T <- 1-lmom$LCV
      ALPHA <- T*(B1+T*B2)/(1+T*(B3+T*B4))
    }
    else {
      T <- pi*lmom$LCV^2
      ALPHA <- (1+A1*T)/(T*(1+T*(A2+T*A3)))
    }  
    para[1] <- ALPHA
    para[2] <- lmom$L1/ALPHA
    return(list(type = 'gam', para = para))
}

"pargev" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)
    #  METHOD: FOR  -0.8 LE TAU3 LT 1,  K IS APPROXIMATED BY RATIONAL
    #  FUNCTIONS AS IN DONALDSON (1996, COMMUN. STATIST. SIMUL. COMPUT.).
    #  IF TAU3 IS OUTSIDE THIS RANGE, NEWTON-RAPHSON ITERATION IS USED.
    #   SMALL IS USED TO TEST WHETHER K IS EFFECTIVELY ZERO
    #   EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF N-R ITERATION

    SMALL <- 1e-5; EPS <- 1e-6; MAXIT <- 20;

    #  EU IS EULER'S CONSTANT
    #    DL2 IS LOG(2), DL3 IS LOG(3)
    EU <- 0.57721566; DL2 <- 0.69314718; DL3 <- 1.0986123

    # COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS FOR K
    A0 <- 0.28377530;  A1 <- -1.21096399; A2 <- -2.50728214
    A3 <- -1.13455566; A4 <- -0.07138022
    B1 <- 2.06189696;  B2 <-  1.31912239; B3 <-  0.25077104
    C1 <- 1.59921491;  C2 <- -0.48832213; C3 <-  0.01573152
    D1 <- -0.64363929; D2 <-  0.08985247

    T3 <- lmom$TAU3

    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    } 

    if(T3 > 0) {
      #  RATIONAL-FUNCTION APPROXIMATION FOR TAU3 BETWEEN 0 AND 1
      #
      Z <- 1-T3
      G <- (-1+Z*(C1+Z*(C2+Z*C3)))/(1+Z*(D1+Z*D2))
      if(abs(G) < SMALL) {
        #  ESTIMATED K EFFECTIVELY ZERO
        para[3] <- 0
        para[2] <- lmom$L2/DL2
        para[1] <- lmom$L1-EU*para[2]
        return(list(type = 'gev', para = para))
      }
    }
    else { # T3 is <= to zero
      if(T3 < 0 & T3 >= -0.80) {
        #   RATIONAL-FUNCTION APPROXIMATION FOR TAU3 BETWEEN -0.8 AND 0
        G <- (A0+T3*(A1+T3*(A2+T3*(A3+T3*A4))))/(1+T3*(B1+T3*(B2+T3*B3)))
      }
      else {
        #  NEWTON-RAPHSON ITERATION FOR TAU3 LESS THAN -0.8
        #
        if(T3 <= -0.97) {
          G <- 1-log(1+T3)/DL2
        }
        else {
          T0 <- (T3+3)*0.5
          CONVERGE = FALSE
          for(it in seq(1,MAXIT)) {
            X2  <- 2^-G
            X3  <- 3^-G
            XX2 <- 1-X2
            XX3 <- 1-X3
            T   <- XX3/XX2
            DERIV <- (XX2*X3*DL3-XX3*X2*DL2)/(XX2*XX2)
            GOLD <- G
            G <- G-(T-T0)/DERIV
            if(abs(G-GOLD) <= EPS*G) {
              CONVERGE = TRUE
            }
          }
          if(CONVERGE == FALSE) 
          	warning("Iteration has not converged. Results might be unreliable.")
        }
      }
    }

    #  ESTIMATE ALPHA,XI
    para[3] <- G
    GAM <- exp(lgamma(1+G))
    para[2] <- lmom$L2*G/(GAM*(1-2**(-G)))
    para[1] <- lmom$L1 - para[2]*(1-GAM)/G
    return(list(type = 'gev', para = para))
}

"parglo" <-
function(lmom) {
    SMALL <- 1e-6 
    # Estimate kappa of distribution
    para <- matrix(nrow = 3, ncol = 1)
    K <- -lmom$TAU3
    if(! are.lmom.valid(lmom)) {
       warning("L-moments are invalid.")
       return()
    } 
    if(abs(K) <= SMALL) {
      # kappa is effectively zero
      para[3] = 0
      para[2] = lmom$L2
      para[1] = lmom$L1
      return(list(type = 'glo', para = para))
    } 
    # Estimate alpha and xi of distribution
    KK <- K*pi/sin(K*pi) 
    A  <- lmom$L2/KK 
    para[1] <- lmom$L1 - A*(1-KK)/K
    para[2] <- A 
    para[3] <- K 
    return(list(type = 'glo', para = para)) 
}

"pargno" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

    # METHOD: RATIONAL-FUNCTION APPROXIMATION OF K IN TERMS OF TAU-3
    #  COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATION
    #  A0 IS 0.5*sqrt(3/pi)
    A0 <-  0.20466534e1;   A1 <- -0.36544371e+1;
    A2 <-  0.18396733e+1;  A3 <- -0.20360244;
    B1 <- -0.20182173e+1;  B2 <-  0.12420401e+1;  B3 <- -0.21741801

    #  SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-8

    T3 <- lmom$TAU3
    if(! are.lmom.valid(lmom)) {
       warning("L-moments are invalid.")
       return()
    } 
    if(abs(T3) >= 0.95) {
      warning("L-SKEW IS TOO LARGE FOR ROUTINE")
      para[1] = 0
      para[2] = -1
      para[3] = 0
      return(list(type = 'gno', para = para))
    }
    if(abs(T3) <= SMALL) {
      para[1] = lmom$L1
      para[2] = lmom$L2*sqrt(pi)
      para[3] = 0
      return(list(type = 'gno', para = para))
    }
    TT <- T3*T3
    K <- -T3*(A0+TT*(A1+TT*(A2+TT*A3)))/(1+TT*(B1+TT*(B2+TT*B3)))
    E <- exp(0.5*K*K)
    A <- lmom$L2*K/(E*erf(0.5*K))
    XI <- lmom$L1+A*(E-1)/K
    para[1] <- XI
    para[2] <- A 
    para[3] <- K 
    return(list(type = 'gno', para = para)) 
}

"pargpa" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)
    L1 <- lmom$L1
    L2 <- lmom$L2
    T3 <- lmom$TAU3
    if(! are.lmom.valid(lmom)) {
       warning("L-moments are invalid.")
       return()
    } 
    K <- (1-3*T3)/(1+T3)
    para[3] <- K
    para[2] <- (1+K)*(2+K)*L2
    para[1] <- L1 - para[2]/(1+K)
    return(list(type = 'gpa', para=para)) 
}

"pargum" <-
function(lmom) {
   euler <- 0.577215664901532861
   para <- matrix(nrow = 2, ncol = 1);
   if(! are.lmom.valid(lmom)) {
     warning("L-moments are invalid.")
     return()
   } 
   para[2] <- lmom$L2/log(2) 
   para[1] <- lmom$L1-euler*para[2] 
   return(list(type = 'gum', para=para))
}

"parkap" <-
function(lmom) {
    para <- matrix(nrow = 4, ncol = 1)
    #  IFAIL  *OUTPUT* FAIL FLAG. ON EXIT, IT IS SET AS FOLLOWS.
    #                  0  SUCCESSFUL EXIT
    #                  1  L-MOMENTS INVALID
    #                  2  (TAU-3, TAU-4) LIES ABOVE THE GENERALIZED-LOGISTIC
    #                     LINE (SUGGESTS THAT L-MOMENTS ARE NOT CONSISTENT
    #                     WITH ANY KAPPA DISTRIBUTION WITH H.GT.-1)
    #                  3  ITERATION FAILED TO CONVERGE
    #                  4  UNABLE TO MAKE PROGRESS FROM CURRENT POINT IN
    #                     ITERATION
    #                  5  ITERATION ENCOUNTERED NUMERICAL DIFFICULTIES -
    #                     OVERFLOW WOULD HAVE BEEN LIKELY TO OCCUR
    #                  6  ITERATION FOR H AND K CONVERGED, BUT OVERFLOW
    #                     WOULD HAVE OCCURRED WHEN CALCULATING XI AND ALPHA
    #
    #  N.B.  PARAMETERS ARE SOMETIMES NOT UNIQUELY DEFINED BY THE FIRST 4
    #  L-MOMENTS. IN SUCH CASES THE ROUTINE RETURNS THE SOLUTION FOR WHICH
    #  THE H PARAMETER IS LARGEST.

    #  THE SHAPE PARAMETERS K AND H ARE ESTIMATED USING NEWTON-RAPHSON
    #  ITERATION ON THE RELATIONSHIP BETWEEN (TAU-3,TAU-4) AND (K,H).
    #  THE CONVERGENCE CRITERION IS THAT TAU-3 AND TAU-4 CALCULATED FROM
    #  THE ESTIMATED VALUES OF K AND H SHOULD DIFFER BY LESS THAN 'EPS'
    #  FROM THE VALUES SUPPLIED IN ARRAY XMOM.

    #
    #         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF N-R ITERATION 
    #         MAXSR IS THE MAX. NO. OF STEPLENGTH REDUCTIONS PER ITERATION
    #         HSTART IS THE STARTING VALUE FOR H
    #         BIG IS USED TO INITIALIZE THE CRITERION FUNCTION
    #         OFLEXP IS SUCH THAT exp(OFLEXP) JUST DOES NOT CAUSE OVERFLOW
    #         OFLGAM IS SUCH THAT exp(lgamma(OFLGAM)) JUST DOES NOT CAUSE
    #           OVERFLOW
    #
    EPS    <- 1e-6;
    MAXIT  <- 20;
    MAXSR  <- 10;
    HSTART <- 1.001;
    BIG    <- 10;
    OFLEXP <- log(.Machine$double.xmax);
    OFLGAM <- uniroot(function(x) lgamma(x)-OFLEXP,c(1,OFLEXP))$root;

    T3 <- lmom$TAU3
    T4 <- lmom$TAU4

    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      IFAIL <- 1
      return()
    }
    if(T4 >= (5*T3*T3+1)/6 ) {
      IFAIL <- 2
      return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "TAU3 and TAU4 are above Generalized Logistic line."))
    }
    #
    #  SET STARTING VALUES FOR N-R ITERATION: 
    #   G IS CHOSEN TO GIVE THE CORRECT VALUE OF TAU-3 ON THE
    #   ASSUMPTION THAT H=1 (I.E. A GENERALIZED PARETO FIT) -
    #   BUT H IS ACTUALLY SET TO 1.001 TO AVOID NUMERICAL
    #   DIFFICULTIES WHICH CAN SOMETIMES ARISE WHEN H=1 EXACTLY
    #
    G <- (1-3*T3)/(1+T3)
    H <- HSTART
    Z <- G+H*0.725
    XDIST <- BIG
    #
    # START OF NEWTON-RAPHSON ITERATION
    #
    MAXITLOOPEND <- FALSE
    for(IT in seq(1,MAXIT)) {
      #
      #  REDUCE STEPLENGTH UNTIL WE ARE NEARER TO THE REQUIRED
      #   VALUES OF TAU-3 AND TAU-4 THAN WE WERE AT THE PREVIOUS STEP
      #
      MAXSRLOOPEND <- FALSE
      for(I in seq(1,MAXSR)) {
        #        
        # CALCULATE CURRENT TAU-3 AND TAU-4
        #       
        #   NOTATION:
        #    U.    - RATIOS OF GAMMA FUNCTIONS WHICH OCCUR IN THE PWM'S
        #            BETA-SUB-R
        #    ALAM. - L-MOMENTS (APART FROM A LOCATION AND SCALE SHIFT)
        #    TAU.  - L-MOMENT RATIOS
        #
        if(G > OFLGAM) {
          IFAIL <- 5
          return(list(type = 'kap', para = para, ifail = IFAIL,,
                  ifailtext = "H/K iteration encountered numerical difficulties."))
        }
        if(H > 0) {
          U1 <- exp(lgamma(1/H)-lgamma(1/H+1+G))
          U2 <- exp(lgamma(2/H)-lgamma(2/H+1+G))
          U3 <- exp(lgamma(3/H)-lgamma(3/H+1+G))
          U4 <- exp(lgamma(4/H)-lgamma(4/H+1+G))
        }
        else {
          U1 <- exp(lgamma(-1/H-G)-lgamma(-1/H+1))
          U2 <- exp(lgamma(-2/H-G)-lgamma(-2/H+1))
          U3 <- exp(lgamma(-3/H-G)-lgamma(-3/H+1))
          U4 <- exp(lgamma(-4/H-G)-lgamma(-4/H+1))
        }
        ALAM2 <-  U1- 2*U2
        ALAM3 <- -U1+ 6*U2 -6*U3
        ALAM4 <-  U1-12*U2+30*U3-20*U4
        if(ALAM2 == 0) {
          IFAIL <- 5
          return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "H/K iteration encountered numerical difficulties."))
        }
        TAU3 <- ALAM3/ALAM2
        TAU4 <- ALAM4/ALAM2
        E1 <- TAU3-T3
        E2 <- TAU4-T4
        DIST <- max(abs(E1),abs(E2))
        if(DIST >= XDIST) {
          #
          # HALVE THE STEPLENGTH AND TRY AGAIN
          #
          DEL1 <- 0.5*DEL1
          DEL2 <- 0.5*DEL2
          G    <- XG-DEL1
          H    <- XH-DEL2
        }
        else {
           # IF NEARER THAN BEFORE, EXIT MAXSR LOOP
           break
        }
        if(I == MAXSR) MAXSRLOOPEND <- TRUE 
      }   # END OF MAXSR LOOP
      if(MAXSRLOOPEND == TRUE) {
        #
        # TOO MANY STEPLENGTH REDUCTIONS
        #
        IFAIL <- 4
        return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "Unable to make progress from current point in H/K iteration."))
      }
      #
      # TEST FOR CONVERGENCE
      #
      if(DIST >= EPS) {
        #
        # NOT CONVERGED: CALCULATE NEXT STEP
        #
        #         NOTATION:
        #         U1G  - DERIVATIVE OF U1 W.R.T. G
        #         DL2G - DERIVATIVE OF ALAM2 W.R.T. G
        #         D..  - MATRIX OF DERIVATIVES OF TAU-3 AND TAU-4 W.R.T. G AND H
        #         H..  - INVERSE OF DERIVATIVE MATRIX
        #         DEL. - STEPLENGTH
        #
        XG <- G
        XH <- H
        XZ <- Z
        XDIST <- DIST
        RHH <- 1/(H*H)
        if(H > 0) {
          U1G <- -U1*digamma(1/H+1+G)
          U2G <- -U2*digamma(2/H+1+G)
          U3G <- -U3*digamma(3/H+1+G)
          U4G <- -U4*digamma(4/H+1+G)
          U1H <-   RHH*(-U1G-U1*digamma(1/H))
          U2H <- 2*RHH*(-U2G-U2*digamma(2/H))
          U3H <- 3*RHH*(-U3G-U3*digamma(3/H))
          U4H <- 4*RHH*(-U4G-U4*digamma(4/H))
        }
        else {
          U1G <- -U1*digamma(-1/H-G)
          U2G <- -U2*digamma(-2/H-G)
          U3G <- -U3*digamma(-3/H-G)
          U4G <- -U4*digamma(-4/H-G)
          U1H <-   RHH*(-U1G-U1*digamma(-1/H+1))
          U2H <- 2*RHH*(-U2G-U2*digamma(-2/H+1))
          U3H <- 3*RHH*(-U3G-U3*digamma(-3/H+1))
          U4H <- 4*RHH*(-U4G-U4*digamma(-4/H+1))
        }
        DL2G <- U1G-2*U2G
        DL2H <- U1H-2*U2H
        DL3G <- -U1G+6*U2G-6*U3G
        DL3H <- -U1H+6*U2H-6*U3H
        DL4G <- U1G-12*U2G+30*U3G-20*U4G
        DL4H <- U1H-12*U2H+30*U3H-20*U4H
        D11  <- (DL3G-TAU3*DL2G)/ALAM2
        D12  <- (DL3H-TAU3*DL2H)/ALAM2
        D21  <- (DL4G-TAU4*DL2G)/ALAM2
        D22  <- (DL4H-TAU4*DL2H)/ALAM2
        DET  <- D11*D22-D12*D21
        H11  <-  D22/DET
        H12  <- -D12/DET
        H21  <- -D21/DET
        H22  <-  D11/DET
        DEL1 <- E1*H11+E2*H12
        DEL2 <- E1*H21+E2*H22
        # 
        # TAKE NEXT N-R STEP
        #
        G <- XG-DEL1
        H <- XH-DEL2
        Z <- G+H*0.725
        #
        # REDUCE STEP IF G AND H ARE OUTSIDE THE PARAMETER SPACE
        #
        FACTOR <- 1
        if(G <= -1) FACTOR <- 0.8*(XG+1)/DEL1
        if(H <= -1) FACTOR <- min(FACTOR,0.8*(XH+1)/DEL2)
        if(Z <= -1) FACTOR <- min(FACTOR,0.8*(XZ+1)/(XZ-Z))
        if(H <= 0 & G*H <= -1) {
          FACTOR <- min(FACTOR,0.8*(XG*XH+1)/(XG*XH-G*H))
        }
        if(FACTOR != 1) { 
          DEL1 <- DEL1*FACTOR
          DEL2 <- DEL2*FACTOR
          G <- XG-DEL1
          H <- XH-DEL2
          Z <- G+H*0.725
        }
        if(IT == MAXIT) MAXITLOOPEND <- TRUE 
        next;
      }
      break;
      #
      # END OF NEWTON-RAPHSON ITERATION
      #
    }
    #
    # NOT CONVERGED
    #
    if(MAXITLOOPEND == TRUE) {
      IFAIL <- 3
      return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "Iteration on H and K failed to converge."))
    }
    #
    #  CONVERGED
    #
    IFAIL <- 0
    para[4] <- H
    para[3] <- G
    TEMP <- lgamma(1+G)
    if(TEMP > OFLEXP) {
      IFAIL <- 6
      return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "H and K converged, but overflow on XI and ALPHA."))
    }
    GAM  <- exp(TEMP)
    TEMP <- (1+G)*log(abs(H))
    if(TEMP > OFLEXP) {
      IFAIL <- 6
      return(list(type = 'kap', para = para, ifail = IFAIL,
                  ifailtext = "H and K converged, but overflow on XI and ALPHA."))
    }
    HH <- exp(TEMP)
    para[2] <- lmom$L2*G*HH/(ALAM2*GAM)
    para[1] <- lmom$L1-para[2]/G*(1-GAM*U1/HH)
    return(list(type = 'kap', para = para, ifail = IFAIL,
                ifailtext = "Successful parameter estimation."))
}

"parnor" <-
function(lmom) {
    para <- matrix(nrow = 2, ncol = 1)
    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }
    para[1] <- lmom$L1
    para[2] <- lmom$L2*sqrt(pi)
    return(list(type = 'nor', para=para))
}

"parpe3" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)

    # METHOD: RATIONAL APPROXIMATION IS USED TO EXPRESS ALPHA, THE SHAPE
    # PARAMETER OF THE GAMMA DISTRIBUTION, AS A FUNCTION OF TAU-3.
    # RELATIVE ACCURACY OF THE APPROXIMATION IS BETTER THAN 3E-5.

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    # CONSTANTS USED IN MINIMAX APPROXIMATIONS
    C1 <-  0.2906
    C2 <-  0.1882
    C3 <-  0.0442
    D1 <-  0.36067
    D2 <- -0.59567
    D3 <-  0.25361
    D4 <- -2.78861
    D5 <-  2.56096
    D6 <- -0.77045
    PI3 <- 3*pi
    ROOTPI <- sqrt(pi)

    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }
    
    L1 <- lmom$L1
    L2 <- lmom$L2
    T3 <- abs(lmom$TAU3)
    if(T3 <= SMALL) {
      # ZERO SKEWNESS
      para[1] <- L1
      para[2] <- L2*ROOTPI
      para[3] <- 0
      return(list(type = 'pe3', para = para))
    }
    if(T3 >= 1/3) {
      T <- 1-T3
      ALPHA <- T*(D1+T*(D2+T*D3))/(1+T*(D4+T*(D5+T*D6)))
    }
    else {
      T <- PI3*T3^2
      ALPHA=(1+C1*T)/(T*(1+T*(C2+T*C3)))
    }
    RTALPH <- sqrt(ALPHA)
    BETA <- ROOTPI*L2*exp(lgamma(ALPHA)-lgamma(ALPHA+0.5))
    para[1] <- L1
    para[2] <- BETA*RTALPH
    para[3] <- 2/RTALPH
    if(T3 < 0) para[3] <- -para[3]
    return(list(type = 'pe3', para = para))
}

"parwak" <-
function(lmom) {
    #  PARA   *OUTPUT* ARRAY OF LENGTH 5. ON EXIT, CONTAINS THE PARAMETERS
    #                  IN THE ORDER XI, ALPHA, BETA, GAMMA, DELTA.
    #  IFAIL  *OUTPUT* FAIL FLAG. ON EXIT, IT IS SET AS FOLLOWS.
    #                  0 SUCCESSFUL EXIT
    #                  1 ESTIMATES COULD ONLY BE OBTAINED BY SETTING XI=0
    #                  2 ESTIMATES COULD ONLY BE OBTAINED BY FITTING A
    #                    GENERALIZED PARETO DISTRIBUTION
    #                  3 L-MOMENTS INVALID
    #
    #  PROCEDURE:
    #  1. LOOK FOR A SOLUTION WITH XI UNCONSTRAINED;
    #  2. IF NONE FOUND, LOOK FOR A SOLUTION WITH XI=0;
    #  3. IF NONE FOUND, FIT A GENERALIZED PARETO DISTRIBUTION TO THE
    #     FIRST 3 L-MOMENTS.
    #  ESTIMATES ARE CALCULATED USING THE FORMULAS GIVEN BY GREENWOOD ET AL.
    #  (1979, WATER RESOUR. RES., TABLE 5), BUT EXPRESSED IN TERMS OF
    #  L-MOMENTS RATHER THAN PROBABILITY WEIGHTED MOMENTS.



# Hosking's GOTO 20 in the Wakeby Parameter Estimation
wak.gpa_instead <- function(ALAM1,ALAM2,T3) {
    para <- matrix(nrow = 5, ncol = 1)
    #
    #         CAN'T FIND VALID ESTIMATES EVEN WITH XI=0 -
    #         FIT GENERALIZED PARETO DISTRIBUTION INSTEAD
    #
    IFAIL <- 2
    D <- -(1-3*T3)/(1+T3)
    C <- (1-D)*(2-D)*ALAM2
    B <- 0
    A <- 0
    XI <- ALAM1-C/(1-D)
    para[1] <- XI
    if(D > 0) {
      para[2] <- A
      para[3] <- B
      para[4] <- C
      para[5] <- D
    }
    else {
      A <- C
      B <- -D
      C <- 0
      D <- 0
      para[2] <- A
      para[3] <- B
      para[4] <- C
      para[5] <- D
    }
    return(list(type = 'wak', para = para, ifail = 2,
		ifailtext = "Solution possible by fitting Generalized Pareto instead."))
}



    para <- matrix(nrow = 5, ncol = 1)

    ALAM1 <- lmom$L1
    ALAM2 <- lmom$L2
    ALAM3 <- lmom$L3
    ALAM4 <- lmom$L4
    ALAM5 <- lmom$L5

    T3    <- lmom$TAU3
    T4    <- lmom$TAU4
    T5    <- lmom$TAU5

    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      IFAIL <- 3
      return()
    }

    IFAIL <- 0
    #
    # ESTIMATE N1,N2,N3,C1,C2,C3 WHEN XI.NE.0
    #
    N1 <-  3*ALAM2 - 25*ALAM3 +  32*ALAM4
    N2 <- -3*ALAM2 +  5*ALAM3 +   8*ALAM4
    N3 <-  3*ALAM2 +  5*ALAM3 +   2*ALAM4
    C1 <-  7*ALAM2 - 85*ALAM3 + 203*ALAM4 -125 * ALAM5
    C2 <- -7*ALAM2 + 25*ALAM3 +   7*ALAM4  -25 * ALAM5
    C3 <-  7*ALAM2 +  5*ALAM3 -   7*ALAM4   -5 * ALAM5
    #
    # ESTIMATE B AND D
    #
    A    <- N2*C3 - C2*N3
    B    <- N1*C3 - C1*N3
    C    <- N1*C2 - C1*N2
    DISC <- B*B  - 4*A*C
    if(DISC >= 0) { # if DISC is greater then we can root it
      #warning("X=unknown, looking for dual roots.")
      DISC <- sqrt(DISC)
      ROOT1 <- 0.5*(-B+DISC)/A   # the two roots to the quadratic
      ROOT2 <- 0.5*(-B-DISC)/A
      B <-  max(ROOT1,ROOT2)
      D <- -min(ROOT1,ROOT2)
      if(D < 1) {
        #warning("X=unknown, D is Wakeby consistent")
        #
        # ESTIMATE A, C AND XI
        #
        A <-  (1+B)*(2+B)*(3+B) / (4*(B+D))*((1+D)*ALAM2-(3-D)*ALAM3)
        C <- -(1-D)*(2-D)*(3-D) / (4*(B+D))*((1-B)*ALAM2-(3+B)*ALAM3)
        XI <- ALAM1 - A/(1+B) - C/(1-D)
        if(C >= 0 & A+C >= 0) {
          #warning("X=unknown, other parameters are Wakeby consistent.")
          para[1] <- XI
          para[2] <- A
          para[3] <- B
          para[4] <- C
          para[5] <- D
          return(list(type = 'wak', para = para, ifail = IFAIL,
                      ifailtext = "Successful parameter estimation."))
        }
      }
    }
    #
    #  CAN'T FIND VALID ESTIMATES FOR XI UNRESTRICTED, SO TRY XI=0
    #
    #  ESTIMATE B AND D FOR XI=0
    #
    IFAIL <- 1
    XI  <- 0
    N1  <-  4*ALAM1 - 11*ALAM2 +  9*ALAM3
    N2  <-   -ALAM2 +  3*ALAM3
    N3  <-    ALAM2 +    ALAM3
    C1  <- 10*ALAM1 - 29*ALAM2 + 35*ALAM3 - 16*ALAM4
    C2  <-   -ALAM2 +  5*ALAM3 -  4*ALAM4
    C3  <-    ALAM2 -    ALAM4
    A   <- N2*C3 - C2*N3
    B   <- N1*C3 - C1*N3
    C   <- N1*C2 - C1*N2
    DISC <- B*B - 4*A*C

    if(DISC >= 0 ) {
      #warning("X=0, looking for dual roots.")
      DISC <- sqrt(DISC)
      ROOT1 <- 0.5*(-B+DISC)/A
      ROOT2 <- 0.5*(-B-DISC)/A
      B <-  max(ROOT1,ROOT2)
      D <- -min(ROOT1,ROOT2)
      if(D < 1) {
        #warning("X=0, D is Wakeby consistent.")
        A <-  (1+B)*(2+B) / (B+D)*(ALAM1 - (2-D)*ALAM2)
        C <- -(1-D)*(2-D) / (B+D)*(ALAM1 - (2+B)*ALAM2)
        if(C >= 0 & A+C >= 0) {
          #warning("X=0, other parameters are Wakeby consistent.")
          para[1] <- XI
          para[2] <- A
          para[3] <- B
          para[4] <- C
          para[5] <- D
          return(list(type = 'wak', para = para, ifail = IFAIL,
                      ifailtext = "Solution possible only with XI=0."))
        }
      }
    }
    # give up and return generalized pareto instead
    return(wak.gpa_instead(ALAM1,ALAM2,T3))
}

"plotlmrdia" <-
function(lmr, 
         nopoints=FALSE,
	 nolines=FALSE,
	 nolimits=FALSE,
	 nogev=FALSE,
	 noglo=FALSE,
	 nogpa=FALSE,
	 nope3=FALSE,
	 nogno=FALSE,
	 noexp=FALSE,
	 nonor=FALSE,
	 nogum=FALSE,
	 nouni=FALSE, ...) {
   plot(lmr$limits, xlab = "L-SKEW", ylab = "L-KURTOSIS", type = "n",
        ...)
   if(nolimits == FALSE) {
     lines(lmr$limits,lwd=2,col=8)
   }
   if(nolines == FALSE) {
     if(nogev == FALSE) lines(lmr$gev, col=2,lty=2)
     if(noglo == FALSE) lines(lmr$glo, col=3)
     if(nogno == FALSE) lines(lmr$gno, col=4, lty=2)
     if(nogpa == FALSE) lines(lmr$gpa, col=4)
     if(nope3 == FALSE) lines(lmr$pe3, col=6)
   }
   if(nopoints == FALSE) {
     if(noexp == FALSE) points(lmr$exp,pch=16,col=2)
     if(nonor == FALSE) points(lmr$nor,pch=15,col=2)
     if(nogum == FALSE) points(lmr$gum,pch=17,col=2)
     if(nouni == FALSE) points(lmr$uniform,pch=18,cex=1.5,col=2)
   }
}

"pwm.gev" <-
function(x) { return(pwm.pp(x,A=-0.35,B=0)) }

"pwm.pp" <-
function(x,A,B) {
    N <- length(x)
    #  FOR UNBIASED ESTIMATES SET A AND B EQUAL TO ZERO. OTHERWISE,
    #  PLOTTING-POSITION ESTIMATORS ARE USED, BASED ON THE PLOTTING POSITION
    #  (J+A)/(N+B)  FOR THE J'TH SMALLEST OF N OBSERVATIONS. FOR EXAMPLE,
    #  A=-0.35D0 AND B=0.0D0 YIELDS THE ESTIMATORS RECOMMENDED BY
    #  HOSKING ET AL. (1985, TECHNOMETRICS) FOR THE GEV DISTRIBUTION.
    #
    PWM <- matrix(nrow = 5, ncol = 1)
    for(j in seq(1,5)) PWM[j] <- 0

    if(A == 0 & B == 0) {
      # UNBIASED ESTIMATES OF PWM'S
      for(i in seq(1,N)) {
        WEIGHT <- 1/N
        PWM[1] <- PWM[1] + WEIGHT*x[i]
        for(j in seq(2,5)) { 
          jm <- j-1
          WEIGHT <- WEIGHT*(i-jm)/(N-jm)
          PWM[j] <- PWM[j]+WEIGHT*x[i]
        }
      }
      z <- list(BETA0 = PWM[1], BETA1 = PWM[2], BETA2 = PWM[3],
                BETA3 = PWM[4], BETA4 = PWM[5])
      return(z)
    }
    if(A <= -1 | A >= B) {
      warning("Plotting position parameters are invalid.")
      return()
    }
    #
    # PLOTTING-POSITION ESTIMATES OF PWM'S
    #
    for(i in seq(1,N)) {
      PPOS <- (i+A)/(N+B)
      TERM <- x[i]
      PWM[1] <- PWM[1]+TERM
      for(j in seq(2,5)) {
        TERM <- TERM*PPOS
        PWM[j] <- PWM[j]+TERM
      }
    }
    for(j in seq(1,5)) PWM[j] <- PWM[j]/N
    z <- list(BETA0 = PWM[1], BETA1 = PWM[2], BETA2 = PWM[3],
              BETA3 = PWM[4], BETA4 = PWM[5])
    return(z)
}

"pwm.ub" <-
function(x) { return(pwm.pp(x,A=0,B=0)) }

"pwm2lmom" <-
function(pwm) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    z$L1 <- pwm$BETA0
    z$L2 <- 2*pwm$BETA1 - pwm$BETA0
    z$L3 <- 6*pwm$BETA2 - 6*pwm$BETA1 + pwm$BETA0
    z$L4 <- 20*pwm$BETA3 - 30*pwm$BETA2 + 12*pwm$BETA1 - pwm$BETA0
    z$L5 <- 70*pwm$BETA4 - 140*pwm$BETA3 + 90*pwm$BETA2 - 20*pwm$BETA1 + 
    	pwm$BETA0
    z$LCV <- z$L2/z$L1
    z$TAU3 <- z$L3/z$L2
    z$TAU4 <- z$L4/z$L2
    z$TAU5 <- z$L5/z$L2
    return(z) 
}

"quacau" <-
function(f,para) {

    if(! are.parcau.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]

    if(f == 1) return(Inf)
    if(f == 0) return(-Inf)

    if(f == 0.5) return(U)
    return(U + A*tan(pi*(f-0.5)))
}

"quaexp" <-
function(f,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]
    if(f <= 0 || f >= 1) {
      warning("Nonexceedance probability is invalid")
      return()
    }
    return(U-A*log(1-f))
}

"quagam" <-
function(f,para) { 
    if(! are.pargam.valid(para)) return()
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    if(f <= 0 || f >= 1) {
      warning("argument of function is invalid")
      return()
    }
    if(f == 0) return(0)
    return(qgamma(f,ALPHA,scale=BETA))
}

"quagev" <-
function(f,para) { 
    if(! are.pargev.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(f <= 0 || f >= 1) {
      if(f == 0 & K < 0) return(XI+A/K)
      if(f == 1 & K > 0) return(XI+A/K)
      warning("argument of function is invalid")
      return()
    }
    Y <- -log(-log(f)) 
    if(K != 0) Y <- (1-exp(-K*Y))/K 
    return(XI+A*Y)
}

"quagld" <-
function(f,gldpara) {

    if(! are.pargld.valid(gldpara)) return()

    La1 <- gldpara$para[1]
    La2 <- gldpara$para[2]
    La3 <- gldpara$para[3]
    La4 <- gldpara$para[4]
    tmp <- 1/La2

    if(f <= 0 || f >= 1) {
      if(f == 0) return(La1-tmp)
      if(f == 1) return(La1+tmp)
    }
    return(La1 + tmp*(f**La3 - (1-f)**La4))
}

"quaglo" <-
function(f,para) { 
    if(! are.parglo.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(f <= 0 || f >= 1) {
      if(f == 0 & K < 0) return(XI+A/K)
      if(f == 1 & K > 0) return(XI+A/K)
      warning("argument of function is invalid")
      return()
    }
    Y <- log(f/(1-f)) 
    if(K != 0) Y <- (1-exp(-K*Y))/K
    return(XI+A*Y)
}

"quagno" <-
function(f,para) {
    if(! are.pargno.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(f <= 0 || f >= 1) {
      if(f == 0 & K < 0) return(XI+A/K)
      if(f == 1 & K > 0) return(XI+A/K)
      warning("argument of function is invalid")
      return()
    }
    Y <- qnorm(f)
    if(K != 0) Y <- (1-exp(-K*Y))/K
    return(XI+A*Y)
}

"quagpa" <-
function(f,para) { 
    if(! are.pargpa.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(f <= 0 || f >= 1) {
      if(f == 0) return(XI)
      if(f == 1 & K > 0) return(XI+A/K)
      warning("argument of function is invalid")
      return()
    }
    Y <- -log(1-f)
    if(K != 0) {
      Y=(1-exp(-K*Y))/K
      return(XI+A*Y)
    }
}

"quagum" <-
function(f,para) {
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 
   if(f <= 0 || f >= 1) {
     warning("nonexceedance probability value invalid")
     return()
   }
   return(U-A*log(-log(f)))
}

"quakap" <-
function(f,para) {
    if(! are.parkap.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]   
    if(f <= 0 || f >= 1) {
      if(f == 0) {
        if(H <= 0 & G  < 0) return(U+A/G)
        if(H >  0 & G != 0) return(U+A/G*(1-H^-G))
        if(H >  0 & G == 0) return(U+A*log(H))
        if(H <= 0 & G >= 0) {
          warning("argument to function invalid.")
          return()
        }
        stop("f is fine: should not be here in code execution.")
      }
      if(f == 1) {
        if(G <= 0) {
          warning("argument of function is invalid")
          return()
        }
        else {
          return(U+A/G)
        }
        stop("f=1: should not be here in code execution.")
      }
    }
    else {
      Y <- -log(f)
      if(H != 0) Y <- (1-exp(-H*Y))/H
      Y <- -log(Y)
      if(G != 0) Y <- (1-exp(-G*Y))/G
      return(U+A*Y)
    }
}

"quanor" <-
function(f,para) {
    if(! are.parnor.valid(para)) return()
    return(qnorm(f,mean = para$para[1], sd = para$para[2]))
}

"quape3" <-
function(f,para) { 
    if(! are.parpe3.valid(para)) return()

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    U <- para$para[1]
    A <- para$para[2]
    GAMMA <- para$para[3]
    if(f <= 0 || f >= 1) {
      if((f == 0 & GAMMA > 0) |
         (f == 1 & GAMMA < 0)) {
        U-2*A/GAMMA
      }
      else {
        warning("Argument to function invalid")
        return()
      }
    }
    if(abs(GAMMA) < SMALL) {
      # ZERO SKEWNESS, qnorm() is the standard normal distribution
      return(U+A*qnorm(f))
    }
    else {
       ALPHA <- 4/GAMMA^2
       BETA <- abs(0.5*A*GAMMA)
       if(GAMMA > 0) {
         return(U-ALPHA*BETA+qgamma(f,ALPHA,scale=BETA))
       }
       else {
         return(U+ALPHA*BETA-qgamma(1-f,ALPHA,scale=BETA))
       }
    }
}

"quawak" <-
function(f,wakpara) {
    #
    #   UFL SHOULD BE CHOSEN SO THAT EXP(UFL) JUST DOES NOT CAUSE
    #    UNDERFLOW 
    #
    UFL <- log(.Machine$double.xmin);

    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A <- wakpara$para[2]
    B <- wakpara$para[3]
    C <- wakpara$para[4]
    D <- wakpara$para[5]

    if(f <= 0 || f >= 1) {
      if(f == 0) return(XI)
      if(f == 1) {
        if(D < 0) return(XI+A/B-C/D)
        if(D == 0 & C == 0 & B > 0) return(XI+A/B)
        warning("argument of function is invalid")
        return()
      }
    }
    Z <- -log(1-f)
    Y1 <- Z
    if(B == 0) {
      Y2 <- Z
      if(D != 0) Y2 <- (1-exp(D*Y2))/(-D)
      return(XI+A*Y1+C*Y2)
    }
    else {
      TEMP <- -B*Z
      if(TEMP <  UFL) Y1 <- 1/B
      if(TEMP >= UFL) Y1 <- (1-exp(TEMP))/B
      Y2 <- Z
      if(D != 0) Y2 <- (1-exp(D*Y2))/(-D)
      return(XI+A*Y1+C*Y2)
    }
}

"vec2lmom" <-
function(vec,lscale=TRUE) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )
    z$L1 <- vec[1]
    if(lscale == TRUE) {
      z$L2   <- vec[2]
      z$TAU3 <- vec[3]
      z$TAU4 <- vec[4]
      z$TAU5 <- vec[5]

      z$LCV  <- z$L2/z$L1
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
    }
    else {
      z$LCV  <- vec[2]
      z$TAU3 <- vec[3]
      z$TAU4 <- vec[4]
      z$TAU5 <- vec[5]

      z$L2   <- z$LCV*z$L1
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
    }
    return(z) 
}

"vec2par" <-
function(vec,type) {
    if(type == 'cau') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'cau', para = para)
    }
    else if(type == 'exp') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'exp', para = para)
    }
    else if(type == 'gam') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gam', para = para)
    }
    else if(type == 'gev') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gev', para = para)
    }
    else if(type == 'gld') {
      para <- matrix(nrow = 4, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gld', para = para)
    }
    else if(type == 'glo') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'glo', para = para)
    }
    else if(type == 'gno') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gno', para = para)
    }
    else if(type == 'gpa') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gpa', para = para)
    }
    else if(type == 'gum') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gum', para = para)
    }
    else if(type == 'kap') {
      para <- matrix(nrow = 4, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'kap', para = para)
    }
    else if(type == 'nor') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'nor', para = para)
    }
    else if(type == 'pe3') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'pe3', para = para)
    }
    else if(type == 'wak') {
      para <- matrix(nrow = 5, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'wak', para = para)
    }
    else {
      stop("Did not find a valid distribution type.")
    }
    if(! are.par.valid(z)) {
      warning("The parameters are invalid for the distribution.")
      return()
    }
    return(z)
}

"vec2pwm" <-
function(vec) {
    z <- list(BETA0 = vec[1], BETA1 = vec[2], BETA2 = vec[3], 
    	BETA3 = vec[4], BETA4 = vec[5])
    return(z)
}

