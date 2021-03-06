% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/LifeTable-methods.R
\name{lifeExpectancy}
\alias{lifeExpectancy}
\alias{lifeExpectancy,LifeTable-method}
\title{Calculate life expectancy.}
\usage{
lifeExpectancy(object, age = 0)

\S4method{lifeExpectancy}{LifeTable}(object, age = 0)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{LifeTable}}.}

\item{age}{The age from which life expectancy is calculated. Defaults to 0.}
}
\value{
An object of class \code{\link[dembase]{Values}}, or, if,
there is only one dimension, a numeric vector.
}
\description{
Obtain life expectancies from an object of class
\code{\linkS4class{LifeTable}}.  By default, \code{lifeExpectancy}
returns life expectancy at age 0, but other ages can be used instead.
}
\examples{
al <- demdata::afghan.life
al <- dembase::Values(al)
mx <- dembase::subarray(al,
               subarray = (fun == "mx") & (time == "2001-2005"))
lt <- LifeTable(mx)
lt
lifeExpectancy(lt)
lifeExpectancy(lt, age = 65)
}
\seealso{
Values for life expectancy can also be extracted from a
\code{\linkS4class{LifeTable}} object using \code{\link{lifeTableFun}},
though in this case all ages are shown.
}
