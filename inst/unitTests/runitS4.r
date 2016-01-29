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
##
##  $Id: runitS4.r,v 1.2 2009/04/16 09:18:52 burgerm Exp $


cat("\n\nRUnit test cases for S4 class inheritance\n\n")


testRUnit.S4classInheritance <- function()
{
  ##@bdescr
  ##  test case for noneof class: none
  ##  test if S4 classes can be instantiated inside test code
  ##  given that no where argument is specified
  ##@edescr


  myCls <- setClass("testVirtualClass",
                    contains="VIRTUAL",
                    slots=c(x = "numeric",
                            y = "numeric",
                            xlab = "character",
                            ylab = "character")
                    )
  
  print(class(myCls))
  checkEquals(myCls, "testVirtualClass")
  checkException( new(myCls), silent=TRUE)
  
  derivedClassName <- setClass("testDerivedClass",
                               contains="testVirtualClass",
                               slots=c(scale = "numeric",
                                       title = "character")
                               )
  ##  Attention:
  ##  invert inheritance order!
  on.exit(removeClass(derivedClassName))
  on.exit(removeClass(myCls), add=TRUE)
  
  checkEquals(derivedClassName, "testDerivedClass")
  objD <- new(derivedClassName)
  checkTrue( is(objD, derivedClassName))
  checkTrue( isS4(objD))

  checkTrue(require(stats4))

  ##  instantiate S4 class from stats
  ##  be sure to use a unique unused variable name here
  ##  i.e. NOT myCls as this has been used before
  ##  and the on.exit call will look up the name just *before*
  ##  the test function exists
  classNameMLE <- "mle"
  obj <- new(classNameMLE)
  checkTrue( is(obj, classNameMLE))
  checkTrue( isS4(obj))
  
  derivedStats4ClassName <- setClass("mleChild",
                                     contains=classNameMLE,
                                     slots=c(scale = "numeric",
                                             title = "character")
                                     )
  on.exit(removeClass(derivedStats4ClassName), add=TRUE)
  
  checkEquals(derivedStats4ClassName, "mleChild")
  obj <- new("mleChild")
  checkTrue( is(obj, "mleChild"))
  checkTrue( isS4(obj))
  
}
