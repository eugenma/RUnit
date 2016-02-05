options(keep.source=TRUE)

if (FALSE) {
  rm(list=ls(all.names=TRUE))
  try(detach(package:RUnit, unload=TRUE), silent=TRUE)
  try(remove.packages("RUnit"), silent=TRUE)
  install.packages("D:/projects/OpenSource/RUnit_0.5.0.tar.gz", repos=NULL)
}


# Returns the file path to the starting script.
.getPathToMainScript <- function(normalize=TRUE) {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  
  if (length(match) > 0) {
    # Rscript
    pathToCaller <- sub(needle, "", cmdArgs[match])
  } else {
    # 'source'd via R interactive mode
    # We need to distinguish here if source was run with debugMode or not.
    env <- sys.frames()[[1]]
    paths <- mget(c("ofile", "fileName"), envir=env, ifnotfound=list(NULL, NULL))
    validPaths <- sapply(paths, function(v)!is.null(v))
    firstValidPath <- min(which(validPaths))
    pathToCaller <- paths[[firstValidPath]]
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
                            # testFileRegexp="runit.*\\.[rR]$",
                            testFileRegexp="runitS4.R$",
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(testSuite, useOwnErrorHandler=TRUE, verbose=1L)
printTextProtocol(testData, showDetails=FALSE)

