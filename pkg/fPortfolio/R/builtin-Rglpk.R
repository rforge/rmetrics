




# ------------------------------------------------------------------------------


.Rglpk_solve_LP <-
function(obj, mat, dir, rhs, types = NULL, max = FALSE,
    bounds = NULL, verbose = FALSE)
{
    # the R-ported GNU Linear Programming kit
    # solve function --- C Interface

    # validate direction of optimization
    if(!identical( max, TRUE ) && !identical( max, FALSE ))
        stop("'Argument 'max' must be either TRUE or FALSE.")
    direction_of_optimization <- as.integer(max)
    
    # validate verbosity flag
    if(!identical(verbose, TRUE) && !identical(verbose, FALSE))
        stop("'Argument 'verbose' must be either TRUE or FALSE.")
    verb <- as.integer(verbose)
    
    # match direction of constraints
    n_of_constraints <- length(dir)
    # match relational operators to requested input
    direction_of_constraints <- match(dir, c("<", "<=", ">", ">=", "=="))
    if(any(is.na(direction_of_constraints)))
        stop("Argument 'dir' must be either '<', '<=', '>', '>=' or '=='.")
    
    n_of_objective_vars <- length(obj)
    
    constraint_matrix <- as.simple_triplet_matrix(mat)
    
    # types of objective coefficients
    # Default: "C"
    if(is.null(types))
        types <- "C"
    # check if valid types
    if(any(is.na(match(types, c("I", "B", "C"), nomatch = NA))))
        stop("'types' must be either 'B', 'C' or 'I'.")
    # replicate types to fit number of columns
    types <- rep(types, length.out = n_of_objective_vars)
    # need a TRUE/FALSE integer/binary representation
    integers <- types == "I"
    binaries <- types == "B"
    
    # do we have a mixed integer linear program?
    is_integer <- any( binaries | integers )
    
    # bounds of objective coefficients
    bounds <- .as.glp_bounds( as.list( bounds ), n_of_objective_vars )
    
    # call the C interface - this actually runs the solver
    x <- .glp_call_interface(
        obj, n_of_objective_vars, constraint_matrix$i,
        constraint_matrix$j, constraint_matrix$v,
        length(constraint_matrix$v),
        rhs, direction_of_constraints, n_of_constraints,
        is_integer,
        integers, binaries,
        direction_of_optimization, bounds[, 1L],
        bounds[, 2L], bounds[, 3L], verb)
    
    solution <- x$lp_objective_vars_values
    # are integer variables really integers? better round values
    solution[integers | binaries] <-
        round( solution[integers | binaries])
    # match status of solution
    # 0 -> optimal solution (5 in GLPK) else 1
    status <- as.integer(x$lp_status != 5L)
    list(optimum = sum(solution * obj), solution = solution, status = status)
}


# ------------------------------------------------------------------------------


.glp_call_interface <-
function(lp_objective_coefficients, lp_n_of_objective_vars,
    lp_constraint_matrix_i, lp_constraint_matrix_j, lp_constraint_matrix_v,
    lp_n_of_values_in_constraint_matrix, lp_right_hand_side,
    lp_direction_of_constraints, lp_n_of_constraints, lp_is_integer,
    lp_objective_var_is_integer, lp_objective_var_is_binary,
    lp_direction_of_optimization,
    lp_bounds_type, lp_bounds_lower, lp_bounds_upper,
    verbose)
{
    # this function calls the C interface
    
    out <- .C("R_glp_solve",
        lp_direction_of_optimization= as.integer(lp_direction_of_optimization),
        lp_n_of_constraints         = as.integer(lp_n_of_constraints),
        lp_direction_of_constraints = as.integer(lp_direction_of_constraints),
        lp_right_hand_side          = as.double(lp_right_hand_side),
        lp_n_of_objective_vars      = as.integer(lp_n_of_objective_vars),
        lp_objective_coefficients   = as.double(lp_objective_coefficients),
        lp_objective_var_is_integer = as.integer(lp_objective_var_is_integer),
        lp_objective_var_is_binary  = as.integer(lp_objective_var_is_binary),
        lp_is_integer               = as.integer(lp_is_integer),
        lp_n_of_values_in_constraint_matrix = as.integer(lp_n_of_values_in_constraint_matrix),
        lp_constraint_matrix_i      = as.integer(lp_constraint_matrix_i),
        lp_constraint_matrix_j      = as.integer(lp_constraint_matrix_j),
        lp_constraint_matrix_values = as.double(lp_constraint_matrix_v),
        lp_bounds_type              = as.integer(lp_bounds_type),
        lp_bounds_lower             = as.double(lp_bounds_lower),
        # lp_n_of_bounds_l         = as.integer(length(lp_lower_bounds_i)),
        lp_bounds_upper             = as.double(lp_bounds_upper), 
        # lp_n_of_bounds_u         = as.integer(length(lp_upper_bounds_i)),
        lp_optimum                  = double(1),
        lp_objective_vars_values    = double(lp_n_of_objective_vars),
        lp_verbosity                = as.integer(verbose),
        lp_status                   = integer(1),
        NAOK = TRUE,
        PACKAGE = "fPortfolio") 
        # PACKAGE = "Rglpk")
    out
}


# ------------------------------------------------------------------------------
# bounds.R 


.glp_fix_bound_type <-
function(x)
{
    # bounds of objective coefficients
    
    # fixes the GLPK bound types given a data.frame with bounds
    # GLP_FR 1 free variable
    # GLP_LO 2 variable with lower bound
    # GLP_UP 3 variable with upper bound
    # GLP_DB 4 double-bounded variable
    # GLP_FX 5 fixed variable

    if(!inherits(x,"bound_table"))
        stop("'x' is not of class 'bound_table'")
    x$type <- ifelse(is.finite(x$lower),
                   ifelse(is.finite(x$upper), 4L, 3L),
                   ifelse(is.finite(x$upper), 2L, 1L))
    x$type[x$upper == x$lower] <- 5L
    x
}

.as.glp_bounds <- 
function(x, ...) {
    # A generic function which allows to take different dense and sparse
    # representations of bounds.
    UseMethod(".as.glp_bounds")
}
  

.as.glp_bounds.default <- 
function(x) {
    # No default representation
    stop("There is no default method for bounds representations.")
}

 
.as.glp_bounds.bound_table <- 
function(x, n) {
    # returns identity
    x
}

  
.as.glp_bounds.list <- 
function(x, n) {
    # list -> GLPK bounds representation
    .as.glp_bounds(x, n)
}
  
  
.as.glp_bounds <- 
function(x, n)
{
    # General input validation
    ##if(!is.list(x))
    #  stop("Bounds have to be of type list")
    
    # Initialize default matrix
    bound_table <-
        expand.grid(type = rep.int(2L, n), upper = 0.0, lower = Inf)
    class(bound_table) <- c("bound_table", class(bound_table))
    
    # Lower bounds
    lower <- x$lower
    if(!is.null(lower)){
      # input validation
      .as.glp_bounds_check_sanity(lower, n)
      if(any(lower[[1L]] == Inf))
        stop("Lower bound cannot be 'Inf'")
      # if everything is OK set new lower bounds
      bound_table[lower[[1L]], 2L] <- lower[[2L]]
    }
    
    # Upper bounds
    upper <- x$upper
    if(!is.null(upper)){
      # input validation
      .as.glp_bounds_check_sanity(upper, n)
      if(any(upper[[1L]] == -Inf))
        stop("Upper bound cannot be '-Inf'")
      # so far, the same as with lower bounds but in addition we have to be
      # sure that upper bounds are greater than or equal to lower bounds
      if(any(bound_table[upper[[1L]], 2L] > upper[[2L]]))
        stop("Upper bounds have to be greater than or equal to lower bounds")
      bound_table[upper[[1L]], 3L] <- upper[[2L]]
    }
    
    # Fix bound types
    out <- .glp_fix_bound_type(bound_table)
    out
}
  

.as.glp_bounds_check_sanity <-
function(x, n)
{
    if(!is.numeric(x[[1L]]))
        warning("Bound indices not numeric. Coercing to integers ...")
    x[[1L]] <- as.integer(x[[1L]])
    if(length(x[[1L]]) != length(x[[2L]]))
        stop("Length of bound indices must be equal to the length of the corresponding bound values.")
    if(any(duplicated(x[[1L]])))
        stop("Duplicated entries in bound indices found.")
    if((max(x[[1L]]) > n))
        stop("Bound indices must not exceed number of objective coefficients.")
}

# ------------------------------------------------------------------------------
# sparse.R


## This file is borrowed from Kurt Hornik's relations package

## A simple class for sparse (triplet) matrices.

## Mostly intended for being able to take advantage of LP solvers which
## allow for sparse specifictions of (possible rather large) constraint
## matrices.


simple_triplet_matrix <-
function(i, j, v, nrow = max(i), ncol = max(j))
{
    structure(list(i = i, j = j, v = v, nrow = nrow, ncol = ncol),
              class = "simple_triplet_matrix")
}


as.simple_triplet_matrix <-
function(x)
    UseMethod("as.simple_triplet_matrix")

    
as.simple_triplet_matrix.simple_triplet_matrix <- 
identity


as.simple_triplet_matrix.matrix <-
function(x)
{
    if(prod(dim(x)) == 0L)
        return(simple_triplet_matrix(integer(), integer(), c(x),
                                     nrow = nrow(x), ncol = ncol(x)))
    ind <- which(x != vector(typeof(x), 1L), arr.ind = TRUE)
    simple_triplet_matrix(ind[, 1L], ind[, 2L], x[ind],
                          nrow = nrow(x), ncol = ncol(x))
}


as.matrix.simple_triplet_matrix <-
function(x, ...)
{
    nr <- x$nrow
    nc <- x$ncol
    y <- matrix(vector(typeof(x$v), nr * nc), nr, nc)
    y[cbind(x$i, x$j)] <- x$v
    y
}


## We could also simply write a method to coerce to a dgTMatrix, based
## on something like
##  new("dgTMatrix",
##       i = as.integer(i - 1),
##       j = as.integer(j - 1),
##       x = v,
##       Dim = c(nrow, ncol))
## (Note that these have C-style indices starting at zero.)


dim.simple_triplet_matrix <-
function(x)
    c(x$nrow, x$ncol)

    
`[.simple_triplet_matrix` <-
function(x, i, j, ...)
{
    ## (Well, we certainly don't drop ...)
    mi <- missing(i)
    mj <- missing(j)
    na <- nargs()

    if(mi && mj) {
        out <- vector(mode = typeof(x$v), length = x$nrow * x$ncol)
        out[(x$j - 1) * x$nrow + x$i] <- x$v
    }
    else if(na == 2L) {
        ## Single index subscripting.
        if(is.logical(i))
            stop("Logical subscripting currently not implemented.")
        else if(is.character(i))
            stop("Character subscripting currently not implemented.")
        else if(!is.matrix(i)) {
            ## Let's hope we have a vector.
            ## What if we have both negatives and positives?
            if(all(i >= 0)) {
                i <- i[i > 0]
                out <- vector(mode = typeof(x$v), length = length(i))
                pos <- match(i, (x$j - 1) * x$nrow + x$i, 0)
                out[i[pos > 0]] <- x$v[pos]
                out
            } else if(all(i <= 0)) {
                out <- vector(mode = typeof(x$v),
                              length = x$nrow * x$ncol)
                out[(x$j - 1) * x$nrow + x$i] <- x$v
                out <- out[i]
            }
            else stop("Cannot mix positive and negative subscripts.")
        }
        else {
            ## Note that negative values are not allowed in a matrix
            ## subscript.
            if((ncol(i) != 2L) || (any(i < 0)))
                stop("Invalid subscript.")
            i <- i[!apply(i == 0, 1L, any), , drop = FALSE]
            out <- vector(mode = typeof(x$v), length = nrow(i))
            pi <- match(i[, 1L], x$i)
            pj <- match(i[, 2L], x$j)
            ind <- which(pi == pj)
            out[ind] <- x$v[pi[ind]]
        }
    }
    else {
        nr <- x$nrow
        nc <- x$ncol
        ## Two index subscripting is rather tricky, as it can also be
        ## used for rearranging and "recycling" rows and columns.  Let
        ## us not support the latter for now, so that selected rows and
        ## columns must be unique.
        if(mi) {
            pos <- rep.int(TRUE, length(x$v))
            nr <- x$nrow
            pi <- seq_len(nr)
        }
        else if(!is.numeric(i))
            stop("Only numeric two-index subscripting is implemented.")
        else {
            pi <- seq_len(x$nrow)
            if(all(i >= 0)) {
                i <- i[i > 0]
                if(any(duplicated(i)))
                    stop("Repeated indices currently not allowed.")
            } else if(all(i <= 0))
                i <- pi[i]
            else
                stop("Cannot mix positive and negative subscripts.")
            nr <- length(i)
            pos <- match(x$i, i, 0) > 0
            pi[i] <- seq_len(nr)
        }
        if(mj) {
            nc <- x$ncol
            pj <- seq_len(nc)
        }
        else if(!is.numeric(j))
            stop("Only numeric 2-index subscripting is implemented.")
        else {
            pj <- seq_len(x$ncol)
            if(all(j >= 0)) {
                j <- j[j > 0]
                if(any(duplicated(j)))
                    stop("Repeated indices currently not allowed.")
            } else if(all(j <= 0))
                j <- pj[j]
            else
                stop("Cannot mix positive and negative subscripts.")
            nc <- length(j)
            pos <- pos & (match(x$j, j, 0) > 0)            
            pj[j] <- seq_len(nc)
        }
        out <- simple_triplet_matrix(pi[x$i[pos]],
                                     pj[x$j[pos]],
                                     x$v[pos], nr, nc)
    }
            
    out
}


rbind.simple_triplet_matrix <-
function(..., deparse.level = 1L)
{
    ## Ignore 'deparse.level' ...
    Reduce(function(x, y) {
        if((nc <- ncol(x)) != ncol(y))
            stop("Numbers of columns of matrices must match.")
        nr <- nrow(x)
        simple_triplet_matrix(c(x$i, y$i + nr),
                              c(x$j, y$j),
                              c(x$v, y$v),
                              nrow = nr + nrow(y), ncol = nc)
    },
           lapply(Filter(Negate(is.null), list(...)),
                  as.simple_triplet_matrix))
}


cbind.simple_triplet_matrix <-
function(..., deparse.level = 1L)
{
    ## Ignore 'deparse.level' ...
    Reduce(function(x, y) {
        if((nr <- nrow(x)) != nrow(y))
            stop("Numbers of rows of matrices must match.")
        nc <- ncol(x)
        simple_triplet_matrix(c(x$i, y$i),
                              c(x$j, y$j + nc),
                              c(x$v, y$v),
                              nrow = nr, ncol = nc + ncol(y))
    },
           lapply(Filter(Negate(is.null), list(...)),
                  as.simple_triplet_matrix))
}


t.simple_triplet_matrix <-
function(x)
    simple_triplet_matrix(x$j, x$i, x$v, x$ncol, x$nrow)

## Utitilies for creating special simple triplet matrices:


.simple_triplet_zero_matrix <-
function(nrow, ncol = nrow, mode = "double")
    simple_triplet_matrix(integer(), integer(), vector(mode, 0L),
                          nrow, ncol)

                          
.simple_triplet_diag_matrix <-
function(x, nrow)
{
    x <- rep(x, length.out = nrow)
    i <- seq_len(nrow)
    simple_triplet_matrix(i, i, x, nrow, nrow)
}


################################################################################

