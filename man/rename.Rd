\name{rename}
\alias{rename}
\title{Change Names of a Named Object}
\description{
  \code{rename} changes the names of a named object.
}
\usage{
rename(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
}
\arguments{
  \item{x}{Any named object}
  \item{\dots}{A sequence of named arguments, all of type character}
  \item{gsub}{a logical value; if TRUE, \code{\link{gsub}} is used to change the
      row and column labels of the resulting table.
      That is, instead of substituting whole names, substrings of the
      names of the object can changed.
      }
  \item{fixed}{a logical value, passed to \code{\link{gsub}}. If TRUE,
    substitutions are by fixed strings and not by regular expressions.}
  \item{warn}{a logical value; should a warning be issued if
    those names to change are not found?}
}
\details{
  This function changes the names of \code{x} according to the
  remaining arguments.
  If \code{gsub} is FALSE, argument tags are the \emph{old}
  names, the values are the new names.
  If \code{gsub} is TRUE, arguments are substrings of the names
  that are substituted by the argument values.
}

\value{
  The object \code{x} with new names defined by the \dots arguments.
}

\examples{
  x <- c(a=1, b=2)
  rename(x,a="A",b="B")
  # Since version 0.99.22 - the following also works:
  rename(x,a=A,b=B)
  
  str(rename(iris,
                  Sepal.Length="Sepal_Length",
                  Sepal.Width ="Sepal_Width",
                  Petal.Length="Petal_Length",
                  Petal.Width ="Petal_Width"
                  ))
  str(rename(iris,
                  .="_"
                  ,gsub=TRUE))

  # Since version 0.99.22 - the following also works:
  str(rename(iris,
                  Sepal.Length=Sepal_Length,
                  Sepal.Width =Sepal_Width,
                  Petal.Length=Petal_Length,
                  Petal.Width =Petal_Width
                  ))
}
\keyword{manip}
