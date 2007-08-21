#### doRUnit.R --- Run RUnit tests
####------------------------------------------------------------------------

### This really follows Gregor Gojanc's example in CRAN package  'gdata'
### and the corresponding section in the R Wiki:
###  http://wiki.r-project.org/rwiki/doku.php?id=developers:runit

if(require("RUnit", quietly=TRUE)) {

  ## --- Setup ---

  wd <- getwd()
  pkg <- basename(sub(pattern="tests$", replacement="", wd))
  ## Path for standalone i.e. not by R CMD check testing
  if(Sys.getenv("RCMDCHECK") == "FALSE") {
    path <- file.path("..", "inst")
  } else {
    pkg <- sub(pattern="\.Rcheck$", replacement="", pkg)
    path <- file.path("..", pkg)
  }
  path <- file.path(wd, path, "unitTests")
  pathReport <- file.path(path, "report")

  library(package=pkg, character.only=TRUE)

  ## --- Testing ---

  ## Define tests
  testSuite <- defineTestSuite(name=paste(pkg, "unit testing"), dirs=path)
  ## Run
  tests <- runTestSuite(testSuite)

  ## Print results
  printTextProtocol(tests)
  printTextProtocol(tests, fileName=paste(pathReport, ".txt", sep=""))

  ## Print HTML version to a file
  printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep=""))

  ## Return stop() if there are any failures i.e. FALSE to unit test.
  ## This will cause R CMD check to return error and stop
  if(getErrors(tests)$nFail > 0) {
    stop("one of unit tests failed")
  }
}

###------------------------------------------------------------------------
### doRUnit.R ends here
