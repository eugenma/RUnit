##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id: checkFuncs.r,v 1.24 2009/11/05 18:57:56 burgerm Exp $

checkFuncRunner <- function(expr, testFailedChecker, errMsg, envir=parent.frame()) {  
  ##@bdescr
  ## runner for custom check functions. 
  ##@edescr
  ##@in expr                : [function] function call which should do the check and return a value which is passed to 'testFailedChecker' and 'errMsg'
  ##@in testFailedChecker   : [function] which takes the outcome of 'expr' and returns 'TRUE' in case of error and 'FALSE' otherwise
  ##@in errMsg              : [function] takes the return value of 'expr' and creates the check failed message
  ##@in envir               : [environment] an environment where 'expr' is evaluated. It should not be changed by default
  ##
  ##@ret                    : [logical] TRUE, if the test is passed, and error is raised (by 'stop()') otherwise
  ##
  ##@codestatus : testing
  if(.existsTestLogger()) {
    RUnitEnv$.testLogger$incrementCheckNum()
  }
  
  result <- eval(expr, envir=envir)
  
  if (testFailedChecker(result)) {
    if(.existsTestLogger()) {
      RUnitEnv$.testLogger$setFailure()
    }
    RUnitEnv$.testLogger$setErrorContext(getCallingCheckFun())
    stop(errMsg(result))
  } else {
    return(TRUE)
  }
}


getCheckFunctionRegexList <- function() {
  opts <- getOption("RUnit")
  funNames <- opts$checkFunctions
  
  regexFunNames <- paste("\\b", funNames, "\\b", sep="")
  
  return(regexFunNames)
}


getCallingCheckFun <- function() {
  calls <- sys.calls()
  calls <- Map(function(x)deparse(x, control="useSource"), calls)
  
  grepCall <- function(pattern)grep(pattern, calls, perl=TRUE, ignore.case=TRUE)
  
  callPos <- Map(grepCall, getCheckFunctionRegexList())
  callPos <- unlist(callPos, recursive=TRUE, use.names=FALSE)
  
  if(is.null(callPos) || (length(callPos) == 0)){
    stop("The checkFunction is not in RUnit$checkFunctions option list.")
  }
  
  frame <- sys.frames()[[callPos-1]]
  callEntry <- calls[[callPos]]
  
  return(list(call=callEntry, frame=frame))
}


checkEquals <- function(target, current, msg="",
                        tolerance = .Machine$double.eps^0.5, checkNames=TRUE, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal'
  ## with tolerance one can adjust to and allow for numerical imprecision

  ##@edescr
  ##@in  target    : [ANY] one thing to be compared
  ##@in  current   : [ANY] second object to be compared
  ##@in  msg       : [character] an optional message to further identify and document the call
  ##@in  tolerance : [numeric] directly passed to 'all.equal', see there for further documentation
  ##@in  checkNames: [logical] iff TRUE do not strip names attributes from current and target prior to the comparison
  ##@ret           : [logical] TRUE iff check was correct
  ##
  ##@codestatus : testing
  
  if (missing(current)) {
     stop("argument 'current' is missing")
  }
  if(!is.numeric(tolerance)) {
    stop("'tolerance' has to be a numeric value")
  }
  if (length(tolerance) != 1) {
    stop("'tolerance' has to be a scalar")
  }
  if(!is.logical(checkNames)) {
    stop("'checkNames' has to be a logical value")
  }
  if (length(checkNames) != 1) {
    stop("'checkNames' has to be a scalar")
  }

  checkExpr <- function(){
    if (!identical(TRUE, checkNames)) {
      names(target)  <- NULL
      names(current) <- NULL
    }
    result <- all.equal(target, current, tolerance=tolerance, ...)
    return(result)
  }

  testFailed <- function(result)!identical(result, TRUE)

  errorMsg <- function(result)paste(paste(result, collapse="\n"), "\n", msg)

  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


checkEqualsNumeric <- function(target, current, msg="", tolerance = .Machine$double.eps^0.5, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal.numeric'
  ## with tolerance one can adjust to and allow for numerical imprecision.
  ## current and target are converted via as.vector() thereby stripping all attributes.
  ##@edescr
  ##@in target    : [ANY] one thing to be compared
  ##@in current   : [ANY] second object to be compared
  ##@in tolerance : [numeric] directly passed to 'all.equal.numeric', see there for further documentation
  ##@in msg       : [character] an optional message to further identify and document the call
  ##
  ##@ret          : [logical] TRUE, if objects 'target' and 'current' are equal w.r.t. specified numerical tolerance, else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(current)) {
    stop("argument 'current' is missing")
  }
  if(!is.numeric(tolerance)) {
    stop("'tolerance' has to be a numeric value")
  }
  if (length(tolerance) != 1) {
    stop("'tolerance' has to be a scalar")
  }

  checkExpr <- function(){
  ##  R 2.3.0: changed behaviour of all.equal
  ##  strip attributes before comparing current and target
    result <- all.equal.numeric(as.vector(target), as.vector(current), tolerance=tolerance, ...)
    return(result)
  }
  testFailed <- function(result)!identical(result, TRUE)
  errorMsg <- function(result)paste(paste(result, collapse="\n"), "\n", msg)

  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


checkIdentical <- function(target, current, msg="")
{
  ##@bdescr
  ## checks if two objects are exactly identical, thin convenience wrapper around 'identical'
  ##
  ##@edescr
  ##@in target   : [ANY] one object to be compared
  ##@in current  : [ANY] second object to be compared
  ##@in msg      : [character] an optional message to further identify and document the call
  ##
  ##@ret         : [logical] TRUE, if objects 'target' and 'current' are identical
  ##
  ##@codestatus : testing

  if (missing(current)) {
    stop("argument 'current' is missing")
  }

  checkExpr <- function(){
    result <- identical(target, current)
    return(result)
  }
  
  testFailed <- function(result)!identical(result, TRUE)
  errorMsg <- function(result)paste(paste(result, collapse="\n"), "\n", msg)

  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


checkTrue <- function(expr, msg="")
{
  ##@bdescr
  ## checks whether or not something is true
  ##@edescr
  ##@in expr : [expression] the logical expression to be checked to be TRUE
  ##@in msg  : [character] optional message to further identify and document the call
  ##
  ##@ret     : [logical] TRUE, if the expression in a evaluates to TRUE, else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(expr)) {
    stop("'expr' is missing")
  }

  checkExpr <- function(){
    ##  allow named logical argument 'expr'
    result <- eval(expr)
    names(result) <- NULL
    return(result)
  }

  testFailed <- function(result)!identical(result, TRUE)
  errorMsg <- function(result)paste("Test not TRUE", "\n", msg)

  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


checkException <- function(expr, msg="", silent=getOption("RUnit")$silent)
{
  ##@bdescr
  ## checks if a function call creates an error. The passed function must be parameterless.
  ## If you want to check a function with arguments, call it like this:
  ## 'checkException(function() func(args...))'
  ##
  ##  adding argument silent was suggested by Seth Falcon <sfalcon@fhcrc.org>
  ##  who provided a patch.
  ##@edescr
  ##@in  expr   : [parameterless function] the function to be checked
  ##@in  msg    : [character] an optional message to further identify and document the call
  ##@in  silent : [logical] passed on to try, iff TRUE error messages will be suppressed 
  ##
  ##@ret        : [logical] TRUE, if evaluation of the expression results in a 'try-error', else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(expr)) {
    stop("'expr' is missing")
  }
  if(is.null(silent)) {
    silent <- FALSE
    warning("'silent' has to be of type 'logical'. Was NULL. Set to FALSE.")
  }

  checkExpr <- function(){
    tryResult <- try(eval(expr, envir=parent.frame()), silent=silent)
    return(tryResult)
  }

  testFailed <- function(tryResult)!inherits(tryResult, "try-error")
  errorMsg <- function(result)paste("Error not generated as expected\n", msg)
  
  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


checkWarning <- function(expr, msg="", silent=getOption("RUnit")$silent)
{
  ##@bdescr
  ## checks if a function call creates a warning. The passed function must be parameterless.
  ## If you want to check a function with arguments, call it like this:
  ## 'checkWarning(function() func(args...))'
  ##
  ##  adding argument silent was suggested by Seth Falcon <sfalcon@fhcrc.org>
  ##  who provided a patch.
  ##@edescr
  ##@in  expr   : [parameterless function] the function to be checked
  ##@in  msg    : [character] an optional message to further identify and document the call
  ##@in  silent : [logical] passed on to try, iff TRUE error messages will be suppressed 
  ##
  ##@ret        : [logical] TRUE, if evaluation of the expression causes a warning, else a stop signal is issued 
  ##
  ##@codestatus : testing
  if (missing(expr)) {
    stop("'expr' is missing")
  }
  if(is.null(silent)) {
    silent <- FALSE
    warning("'silent' has to be of type 'logical'. Was NULL. Set to FALSE.")
  }
  
  checkExpr <- function(){
    warningFun <- function(w){
      if(!silent)
        warning(w)
      return(w)
    }
    
    tryResult <- tryCatch(eval(expr, envir=parent.frame()), warning=warningFun)
    return(tryResult)
  }
  
  testFailed <- function(tryResult)!inherits(tryResult, "warning")
  errorMsg <- function(result)paste("Warning not generated as expected\n", msg)
  
  return(checkFuncRunner(checkExpr(), testFailed, errorMsg, envir=parent.frame()))
}


fail <- function(msg="") {
  ##@bdescr
  ## Always failing check.
  ##
  ##@edescr
  ##@in  msg    : [character] an optional message to further identify and document the call
  ##
  ##@ret        : always a stop signal is issued 
  ##
  ##@codestatus : testing
  
  errorMsg <- function(x)paste("Test failed as expected\n", msg)
  
  return(checkFuncRunner(stop(), function(x)TRUE, errorMsg, envir=parent.frame()))
}


DEACTIVATED <- function(msg="")
{
  ##@bdescr
  ##  Convenience function, for maintaining test suites.
  ##  If placed in an existing test case call
  ##  the test will be executed normally until occurrence of the call
  ##  after which execution will leave the test case (so all code will
  ##  be checked and errors or failures reported as usual).
  ##  An entry for a separate table in the log will be added
  ##  for this test case.
  ##
  ##@edescr
  ##@in msg : [character] optional message to further identify and document the call
  ##
  ##@codestatus : testing

  if(.existsTestLogger()) {
    RUnitEnv$.testLogger$setDeactivated(paste(msg, "\n", sep=""))
  }
  stop(msg)
}
