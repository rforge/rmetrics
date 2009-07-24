### Read all files into directory
fileList <- list.files("../../HyperbolicDist/R/")
fileList <- fileList[fileList!="zzz.R"]
fileNames <- paste("../../HyperbolicDist/R/", fileList, sep = "")
lapply(fileNames, source)

### Test gigCheckPars
Theta <- c(-0.5,5,2.5)
gigCheckPars(Theta)
gigCheckPars(c(0.5,-5,2.5))
gigCheckPars(c(0.5,5,-2.5))
gigCheckPars(c(0.5,-5,-2.5))
gigCheckPars(c(0.5,0,2.5))
gigCheckPars(c(-0.5,0,2.5))
gigCheckPars(c(0.5,0,0))
gigCheckPars(c(-0.5,0,0))
gigCheckPars(c(0.5,5,0))
gigCheckPars(c(-0.5,5,0))
