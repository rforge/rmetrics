### Unit tests of functions  in momIntegrated

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make

test.momIntegrated <- function()
{
  ## Purpose: Level 1 test of momIntegrated
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date: 08 Feb 2010, 13:28

  ## Raw moments of gamma
  rawMom <- numeric(8)
  gammaMom <- function(order, shape, scale){
    gMom <- (scale^order)*gamma(shape + order)/gamma(shape)
    return(gMom)
  }

  ## Calculate moments for particular gamma
  shape <- 2
  rate <- 3
  rawMom <- sapply(1:8, gammaMom, shape = shape, scale = scale)
  ## Central moments, gamma
  centralMom <- momChangeAbout("all", rawMom, 0, rawMom[1])
  ## Moments about new value
  new <- 1 
  newMom <- momChangeAbout("all", rawMom, 0, new)
  
  ## Check integrated moments from gamma
  ## Raw moments
  m1 <- momIntegrated("gamma", order = 1, param = c(shape,scale), about = 0)
  m8 <- momIntegrated("gamma", order = 8, param = c(shape,scale), about = 0)
  checkEquals(rawMom[1], m1)
  checkEquals(rawMom[8], m8)

  ## Central moments
  cm1 <- momIntegrated("gamma", order = 1, param = c(shape,scale), about = m1)
  cm8 <- momIntegrated("gamma", order = 8, param = c(shape,scale), about = m1)
  checkEquals(centralMom[1], cm1)
  checkEquals(centralMom[8], cm8)

  ## Moments about new
  nm1 <- momIntegrated("gamma", order = 1, param = c(shape,scale),
                       about = new)
  nm8 <- momIntegrated("gamma", order = 8, param = c(shape,scale),
                       about = new)
  checkEquals(newMom[1], nm1)
  checkEquals(newMom[8], nm8)
  
  return()
}


