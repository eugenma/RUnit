emptyFun <- function() {}

expected.withFailureCall <- "checkEquals\\(\"first\", \"second\"\\)"
withFailureCall <- function() {
  checkTrue(TRUE)
  
  # We will find this one
  checkEquals("first", "second")
  
  checkEquals("first", "thrid")
}


withNotRegisteredCheckFunction <- function() {
  # we will never find this but an exception.
  checkTrue(FALSE)
}


before <- function() {
  RUnitEnv$.testLogger$cleanup()
}


isValidResult <- function(expected) {
  errorContext <- RUnitEnv$.testLogger$getErrorContext()
  message <- .getErrorMessage(errorContext)
  
  grepResult <- grep(expected, message)
  
  return(length(grepResult) > 0)
}


test.ContextMsg.Failure <- function() {
  before()
  
  result <- try(withFailureCall(), silent=TRUE)
  
  checkTrue(RUnitEnv$.testLogger$isFailure())
  checkTrue(isValidResult(expected.withFailureCall))
}


test.ContextMsg.CheckFuncNotRegistered <- function() {
  before()
  
  opts <- getOption("RUnit")
  opts$checkFunctions <- c("checkEquals")
  options("RUnit"=opts)
  
  # NOTE: We cannot call it with checkException(), an error would occur otherwise.
  errResult <- try(withNotRegisteredCheckFunction(), silent=TRUE)
  
  errCondition <- attr(errResult, "condition")
  
  checkEquals(".getCallingCheckFun()", deparse(errCondition$call))
  checkEquals(TRUE, length(grep("checkFunction is not in", errCondition$message)) > 0)
}
