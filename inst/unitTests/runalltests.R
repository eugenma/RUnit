
if (FALSE) {
  try(detach(package:RUnit, unload=TRUE), silent=TRUE)
  try(remove.packages("RUnit"), silent=TRUE)
  install.packages("D:/projects/OpenSource/RUnit_0.5.0.tar.gz", repos=NULL)
  
}

setwd("D:/projects/OpenSource/RUnit/inst/unitTests/")

library("RUnit")

options(warn=1)

testSuite <- defineTestSuite(name="RUnit",
                            dirs=".",
                           # testFileRegexp="runit.*\\.r$",
                            testFileRegexp="runitS4.r$",
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(testSuite, verbose=1L)
printTextProtocol(testData, showDetails=FALSE)

