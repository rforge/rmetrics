### Unit tests of functions  in inversionTests.R

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make
### Functions with name graphicstest.* are run by make if
### LEVEL=graphics in call to make

test.inversionTestpq <- function()
{
  ## Purpose: Level 1 test of inversionTestpq
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date: 09 Feb 2010, 15:29

  ## Normal (default)
  diffs <- inversionTestpq()$diffs
  checkEquals(diffs, rep(0, length(diffs)))
  diffs <- inversionTestpq(mean = 1, sd = 2)$diffs
  checkEquals(diffs, rep(0, length(diffs)))


  ## Gamma
  diffs <- inversionTestpq("gamma", shape = 1)$diffs
  checkEquals(diffs, rep(0, length(diffs)))

  return()
}

test.inversionTestqp <- function()
{
  ## Purpose: Level 1 test of inversionTestqp
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date: 09 Feb 2010, 15:29

  ## Normal (default)
  diffs <- inversionTestqp()$diffs
  checkEquals(diffs, rep(0, length(diffs)))
  diffs <- inversionTestqp(mean = 1, sd = 2)$diffs
  checkEquals(diffs, rep(0, length(diffs)))


  ## Gamma
  diffs <- inversionTestqp("gamma", shape = 1)$diffs
  checkEquals(diffs, rep(0, length(diffs)))

  return()
}
