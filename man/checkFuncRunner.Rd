%%  RUnit : A unit test framework for the R programming language
%%  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; version 2 of the License.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

\encoding{latin1}
\name{checkFuncRunner}
\alias{checkFuncRunner}
\title{Runner for (customized) check functions}

\usage{
checkFuncRunner(expr, testFailedChecker, errMsg, envir=parent.frame())
}

\arguments{
  \item{expr}{function call which should do the check and return a value which is passed to \code{testFailedChecker} and \code{errMsg}.}
  \item{testFailedChecker}{function which takes the outcome of \code{expr} and returns \code{TRUE} in case of error and \code{FALSE} otherwise.}
  \item{errMsg}{function which takes the return value of \code{expr} and creates the check failed message.}
  \item{envir}{an environment where \code{expr} is evaluated. It should not be changed by default.}
}

\description{Runner for custom check functions.
    Each custom check function should be added to the RUnit options \code{checkFunctions} (see \link{RUnit-options}) .
}

\value{
Usually return value can be ignored, since an error is raised by \code{\link{stop}} if the test is failing.

If the test is passing the return value \code{TRUE}.
}


\author{
    Eugen Massini
}

\seealso{
    \code{\link{checkEquals}} for the default check functions.
    \link{RUnit-options} check the \code{checkFunctions} option.
}


\examples{
## a custom checkFalse() check function as a counterpart to the checkTrue() function.
checkFalse <- function(expr, msg="") {
  ## here we perform some check of the parameters passed by caller.
  if (missing(expr)) {
    stop("'expr' is missing")
  }

  ## The implementation of the actual check function
  check <- function(){
    result <- eval(expr)
    names(result) <- NULL
    return(result)
  }
  
  ## Here we check agaist the expected outcome. It should be FALSE.
  isFailed <- function(result)!identical(result, FALSE)

  ## The error message in case the check test is failing.
  ## We ignore the result and just print the msg argument of checkFalse()
  getErrorMsg <- function(result)paste("Test not FALSE", "\n", msg)

  ## The calling of the runner. 
  ## Since check() is a function, we need pass the call.
  return(checkFuncRunner(check(), isFailed, getErrorMsg))
}

## ... 

## Now at some test case:
sut <- function()TRUE 

test.testIt <- function() {
    checkFalse(sut())
}

} % end of \examples


\keyword{programming}
\concept{RUnit}

 % vim: fenc=latin1:spell:spelllang=en_us
