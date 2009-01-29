#.onLoad <- function(libname, pkgname)
#{
#}

# 080417 The R manual for .onLoad says to use .onAttach for startup
# messages.

.onAttach <- function(libname, pkgname)
{
  # This is executed when the package becomes visible to the user.
  
  # TODO: How to not do this if quietly=TRUE? Otherwise it will be
  # annoying, just like fBasics. randomForest seems to do it
  # correctly? Maybe not....
  
  cat(sprintf(paste("portfolioGui, Graphical interface for fPortfolio \n",
                    "Version %s. ", COPYRIGHT, "\n", sep=""), VERSION))
  if ("portfolioGui" %in% getOption("defaultPackages"))
    portfolioGui()
  else
    cat(paste("Type \"portfolioGui()\" to start.\n", sep=""))
}

