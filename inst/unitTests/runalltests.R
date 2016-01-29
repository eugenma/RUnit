
if (FALSE) {
  rm(list=ls(all.names=TRUE))
  try(detach(package:RUnit, unload=TRUE), silent=TRUE)
  try(remove.packages("RUnit"), silent=TRUE)
  install.packages("D:/projects/OpenSource/RUnit_0.5.0.tar.gz", repos=NULL)
}


# Returns the file path to the starting script.
# Source \url{http://stackoverflow.com/a/15373917/3142459}
.getPathToMainScript <- function(normalize=TRUE) {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  
  if (length(match) > 0) {
    # Rscript
    pathToCaller <- sub(needle, "", cmdArgs[match])
  } else {
    # 'source'd via R console
    pathToCaller <- sys.frames()[[1]]$ofile
  }
  
  if(normalize)
    return(normalizePath(pathToCaller))
  else
    return(pathToCaller)
}

setwd(dirname(.getPathToMainScript()))

library("RUnit")

options(warn=1)

testSuite <- defineTestSuite(name="RUnit",
                            dirs=".",
                            testFileRegexp="runit.*\\.r$",
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(testSuite, verbose=1L)
printTextProtocol(testData, showDetails=FALSE)

