#
# Examples from the forthcoming Monograph:
#   Rmetrics - Financial Engineering and Computational Finance
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 4.6
#   Matrix and Linear Algebra Addon:
#
# List of Examples, Exercises and Code Snippets:
#       
#   Example: 1 Overview of Acvailable Functions
#   Example: 2 Generation of Matrices
#   Example: 3 Matrices and Linear Algebra
#   Example: 4 Basic Statistics
#   Example: 5 Column and Row Vectors
#   Example: 6 Stack Operations and Matrix Products
#   Example: 7 Missing Values in a Data Matrix 
#
#   *** This list is not yet complete ***
#
#
# Author:
#   (C) 2002-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Example: 1 Overview of Available Functions

    # GENERATION:           DESCRIPTION:
    #  matrix               R  creates a matrix from the given set of values
    #   diag                R  creates a diagonal matrix or extracts diagonals
    #   triang              S  extracs the lower tridiagonal part from a matrix
    #   Triang              S  extracs the upper tridiagonal part from a matrix
    #   pascal              S  creates a pascal matrix
    #   colVec              S  creates a column vector from a vector
    #   rowVec              S  creates a row vector from a vector
    #  as.matrix            R  attempts to turn its argument into a matrix     
    #  is.matrix            R  tests if its argument is a (strict) matrix
    #  dimnames             R  retrieves or sets the dimnames of an object
    #  colnames|rownames    R  retrieves or sets the row or column names 
    #  colIds|rowIds        S  ... use alternatively
    ###
    
    # SUBSETS:              DESCRIPTION:
    #  dim                  R  returns the dimension of a matrix object
    #  ncol|nrow            R  counts columns|rows of a matrix object
    #  colNums|rowNums      S  ... use alternatively
    #  length               R  counts elements of a matrix object
    #   "["|"[["            R  subsets a matrix object
    #   (Arith)             R  Elementwise Arithmetic: + - * /
    #   (Lops)              R  Elementwise logical Ops: > < >= <= == !=
    #  cbind|rbind          R  augments a matrix object by columns|rows
    #  na.omit              R  removes NA from a matrix object
    ###
    
    # BASIC STATISTICS:     DESCRIPTION:
    #  var                     returns the variance matrix
    #  cov                     returns the covariance matrix
    #  col|rowStats *          calculates column|row statistics 
    #   col|rowMeans           calculates column|row means
    #   col|rowAvgs *          calculates column|row averages
    #   col|rowVars *          calculates column|row variances
    #   col|rowStdevs *        calculates column|row standard deviations
    #   col|rowSkewness *      calculates column|row skewness 
    #   col|rowKurtosis *      calculates column|row kurtosis 
    #   col|rowCumsums *       calculates column|row cumulated sums 
    ###
    
    # LINEAR ALGEBRA:       DESCRIPTION:
    #  det                     returns the determinante of a matrix
    #  norm                    returns the norm of a matrix
    #  rk *                    returns the rank of a matrix
    #  tr *                    returns trace of a matrix
    #  %*%                  R  returns the product of two matrices
    #  %x%|kron           R|S  returns the Kronecker product
    #  t                    R  returns the transposed matrix
    #  inv|chol2inv *          returns the inverse of a matrix
    ###
    
    # MORE LINEAR ALGEBRA:  DESCRIPTION:
    #  chol                 R  returns the Cholesky factor matrix
    #  eigen                R  returns eigenvalues and eigenvectors
    #  svd                  R  returns the singular value decomposition
    #  qr                   R  returns the QR decomposition of a matrix
    #  ginv                 R  returns Moore-Penrose generalized inverse
    #  solve                R  solves a system of linear equations
    #  backsolve            R  ... use when the matrix is upper triangular
    #  forwardsolve         R  ... use when the matrix is lower triangular
    ###
    

# ------------------------------------------------------------------------------


### Example: 2 Generation of Matrices

    # matrix -
    # Create Matrixes from Scratch:
    Q = matrix(1:16, 4); Q
    G = matrix(1:16, 8, 2); G
    R = matrix(rnorm(20), byrow = TRUE, ncol = 2); R
    ###
  
    # Dimension Names: 
    R 
    colnames(R) = c("C1", "C2"); R
    rownames(R) = as.character(1:nrow(R)); R
    dimnames(R) = NULL; R
    dimnames(R) = list(as.character(1:nrow(R)), c("C1", "C2")); R
    colIds(R)
    ###

    # Create Lower/Upper Triangle Matrices:
    Triang(Q)                      # returns upper triangle as matrix
    triang(Q)                      # returns lower triangle as matrix
    Q[upper.tri(Q)]                # returns upper triangle as vector
    Q[lower.tri(Q)]                # returns lower triangle as vector  
    ###
    
    # Create Diagonal Matrixes:
    diag(3)                        # 3 x 3 Identity matrix
    diag(1, 3)                     # ... another way to create it
    diag(1:3, 3)                   # A diagonal matrix with elements 1,2,3
    D = diag(c(1,2,6), 3); D       # ... another diagonal atrix
    ###
     
    # Create Pascal Matrixes:
    P = pascal(3); P  
    ###
    
   
# ------------------------------------------------------------------------------


### Example: 3 Matrices and Linear Algebra

    # Add/Subtract/Multiply/Divide a Constant or Matrix:  
    X + 3                          # adds a constant to a matrix
    3 - X                          # subtracts from a constant
    X * 0.5                        # Multiplies with a constant
    X / 6                          # devides by constant
    6 / X                          # devides a constant by a matrix
    X + D                          # adds two matrices element by element
    D - X                          # subtracts two matrices element by element
    X * D                          # subtracts two matrices element by element
    D / X                          # devides two matrices element by element
    X %*% diag(1:3)                # multiplies rows of a matrix by a vector
    diag(v) %*% X                  # multiplies columns of a matrix by a vector
    ###
    
    # Operate on Subsets of a Matrix:
    n = 3; i = 2; j = 3
    dim(X)                         # returns the dimension of a matrix
    ncol(X)                        # counts columns of a matrix
    nrow(X)                        # counts rows of a matrix
    length(X)                      # counts elements of a matrix
    X[, ncol(X)]                   # the last colum of a matrix
    X[nrow(X), ]                   # the last row of a matrix
    X[, -i]                        # deletes a column of a matrix
    X[-i, ]                        # deletes a row of a matrix
    X[, i] + X[, j]                # adds two columns of a matrix
    X[i, ] - X[j, ]                # subtracts two rows of a matrix
    X[c(3, 1, 2), ]                # permutes the rows
    X[, c(3, 1, 2)]                # permutes the coliumns
    cbind(X, 1:3)                  # augments horizontally 
    rbind(X, D)                    # augments vertically 
    X[, c(1,1,1)]                  # repeats first column 3 times
    X[rep(1:3, rep(2, 3)), ]       # replicates every other line
    diag(X)                        # extracs diagonal as vector
    ###
       
    # Apply a function to all Elements of a Matrix: 
    sqrt(X); X^1/2                 # returns square root for each element
    abs(-X); sign(R)               # returns abs|sign root for each element
    min(X); max(X)                 # returns min|max root for each element
    cos(X); sin(X); tan(X)         # returns trigonometric function elementwise
    acos(X); asin(X); atan(X)      # returns inverse trigonometric functions
    exp(X); 10^X                   # returns the exponential|power functions
    log(X); log10(X)               # returns logarithmic function elementwise
    apply(x, 2, "min")             # minimum value of each column
    apply(x, 1, "max")             # maximum value of each row    
    all( R > 0 )                   # tests on all elements of a matrix
    any( R > 0 )                   # tests on any element in a matrix
    ###
       
    # More Matrix Operations:
    X %*% D                        # returns the product of two matrices
    X %x% D                        # returns the Kronecker Product
    t(X)                           # returns the transposed matrix
    inv(X)                         # returns the inverse of a matrix
    norm(X)                        # returns the norm of a matrix
    det(X)                         # returns the determinante of a matrix
    rk(X)                          # returns the rank of a matrix
    tr(X)                          # returns trace of a matrix
    var(X)                         # returns the variance matrix
    cov(X)                         # returns the covariance matrix
    ###
   
    # More Linear Algebra:
    b = c(1, 2, 3)
    chol(X)                        # returnsns the Cholesky factor matrix
    eigen(X)                       # returns eigenvalues and eigenvectors
    svd(X)                         # returns the singular value decomposition
    kappa(X)                       # returns the condition number
    qr(X)                          # returns the QR decomposition of a matrix
    ginv(X)                        # returns Moore-Penrose generalized inverse
    solve(X, b)                    # solves a system of linear equations
    backsolve(R)                   # ... use when the matrix is upper triangular
    forwardsolve(L)                # ... use when the matrix is upper triangular
    ###
   
# ------------------------------------------------------------------------------


### Example: 4 Basic Statistics

    # Create matrix:
    ts = matrix(rnorm(100), 50)
    var(ts)
    cov(ts)
    ###
   
    # col* -
    # Column Operations:
    colMeans(ts)
    ###
 

# ------------------------------------------------------------------------------


### Example: 5 Column and Row Vectors
   
    # Define and use vectors:
    u = c(3, 1, 4)
    v = c(2, 0, -1)   
    u * v; v * u
    u %*% v; v %*% u
    # Convert to column and row vectors:
    U = colVec(u); U
    V = rowVec(v); V
    # Dot Product or Inner Product: 
    # Row x Col == Scalar
    V %*% U
    # Outer Product:
    # Col x Row == Matrix
    U %*% V
    # Matrix * Column Vector and Row Vector * Matrix
    X = matrix(rep(1, 9), 3)
    X %*% U
    V %*% X
    ###
    
    
# ------------------------------------------------------------------------------


### Example: 6 Stack Operations and Matrix Products

    # Show, that the following relation holds:
    # vec ( A %*% B %*% C ) = ( t(C) %x% A ) %*% vec(B)
    ###
    
    # Three random Matrixes:
    A = matrix(rnorm(9), 3)
    B = matrix(rnorm(9), 3)
    C = matrix(rnorm(9), 3)
    ###
    
    # Print vech:
    A
    vech(A)
    ###
    
    # Print vec:
    B
    vec(B)
    ###
    
    # Vector Product:
    A %*% B
    ###
    
    # Kronecker Product:
    A %x% B
    # Left Hand Side:
    LHS = vec ( A %*% B %*% C )
    # Right Hand Side:
    # %x# denotes the Kronecker Product
    RHS = ( t(C) %x% A ) %*% vec(B)
    # Test the Difference:
    data.frame(LHS, RHS, LHS-RHS)
    ###

    

# ------------------------------------------------------------------------------


### Example: 7 Missing Values in a Data Matrix 

    # Write R functions which remove, substitute, interpolate and
    # impute missing values in a matrix object:
    #   1. removeNA        removes NAs from a matrix 
    #                      object
    #   2. subtituteNA     substitutes NAs by zeroes, the column 
    #                      mean or column median
    #   3. interpNA        interpolate NAs using R's "approx" 
    #                      function
    #   4. knnNA           imputes NAs by the knn-Algorithm using R's
    #                      contributed function "knn" from the "EMV" Package
    # Notes:
    #   We didn't take care, that NAs at the border of a matrix are
    #   properly considered. That has still to be done!
    #   The R functions can be found in "funSeries.R"
    ###

    # Generate a matrix with missing values:
    M = matrix(round(rnorm(40), 2), ncol = 5)
    M[2:3, 1] = M[4, 3:4] = c(NA, NA)
    M[7, 1] =  NA
    colnames(M) = c("a", "b", "c", "d", "e")
    rownames(M) = paste("R", as.character(1:8), sep = "")
    M
    ###
       
    # Remove rows with missing values from M:
    M
    removeNA(M)
    # Remove Columns with NAs from M:
    t(removeNA(t(M)))
    ###  
    
    # Substitute missing values with Zeros, the Median and Mean:
    M
    substituteNA(M)
    substituteNA(M, "median")
    round(substituteNA(M, "mean"), 2)
    ###
   
    # Interpolate by Columns:
    M
    interpNA(M, "before")
    interpNA(M, "after")
    interpNA(M, "linear")
    
    # By Rows:
    M
    t(interpNA(t(M), "before"))
    t(interpNA(t(M), "after"))
    t(interpNA(t(M), "linear"))
    ###
    
    # Use "knn" Algorithm:
    knnNA(M, k=2)
    round(knnNA(M, k = 2, correlation = TRUE), 2)
    # Apply it to the transposed matrix:
    t(round(knnNA(t(M), k = 2), 2))
    t(round(knnNA(t(M), k = 2, correlation = TRUE), 2))
    ###
    

################################################################################

   