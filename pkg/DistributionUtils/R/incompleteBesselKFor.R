incompleteBesselKFor <- function(x, y, nu, tol = .Machine$double.eps,
                              nmax = 90) {

  IBFOut <- .Fortran("incompleteBesselK",
                     as.double(x),
                     as.double(y),
                     as.double(nu),
                     as.double(tol),
                     as.integer(nmax),
                     IBF = double(1),
                     status = integer(1)
                     )
  str(IBFOut)
  status <- IBFOut$status
  IBF <- IBFOut$IBF
  if(status == 1) warning("Maximum order exceeded\nResult may be unreliable")
  return(IBF)
}

