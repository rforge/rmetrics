### Calculate summary for hyperbFit object
### Only applies when hessian is asked for
###
### DJS 11/08/06
summary.hyperbFit <- function(object, ...)
{
  if (!class(object)=="hyperbFit"){
    stop("Object must belong to class hyperbFit")
  }
  if (!is.null(object$hessian)){
     sds <- sqrt(diag(solve(object$hessian)))
     sds[2] <- Theta[2]*sds[2]
     sds[3] <- Theta[3]*sds[3]
     names(sds) <- c("pi","zeta","delta","mu")
     object$sds <- sds
  }
  class(object) <- "summary.hyperbFit"
  object
} ## End of summary.hyperbFit

### Print summary
print.summary.hyperbFit <- function(object,
                              digits = max(3, getOption("digits") - 3), ...)
{
  if (!class(object)=="summary.hyperbFit"){
    stop("Object must belong to class summary.hyperbFit")
  }
  cat("\nData:     ", object$xName, "\n")
  cat("Parameter estimates:\n")
  if (is.null(object$sds)){
    print.default(format(object$Theta, digits = digits),
                  print.gap = 2, quote = FALSE)
  }else{
    ans <- format(rbind(object$Theta, object$sds), digits = digits)
    ans[1, ] <- sapply(ans[1, ], function(x) paste("", x))
    ans[2, ] <- sapply(ans[2, ], function(x) paste("(", x, ")", sep = ""))
    dn <- dimnames(ans)
    dn[[1]] <- rep("", 2)
    dn[[2]] <- paste(substring("      ", 1, (nchar(ans[2, ]) -
        nchar(dn[[2]]))%/%2), dn[[2]])
    dn[[2]] <- paste(dn[[2]], substring("      ", 1, (nchar(ans[2,
        ]) - nchar(dn[[2]]))%/%2))
    dimnames(ans) <- dn
    print.default(ans, print.gap = 2, quote = FALSE)
  }
  cat("Likelihood:        ",object$maxLik,"\n")
  cat("Method:            ",object$method,"\n")
  cat("Convergence code:  ",object$conv,"\n")
  cat("Iterations:        ",object$iter,"\n")
  invisible(object)
}
