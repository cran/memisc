\name{rename}
\alias{rename}
\title{Change Names of a Named Object}
\description{
  \code{rename} changes the names of a named object.
}
\usage{
rename(x, ...)
}
\arguments{
  \item{x}{Any named object}
  \item{\dots}{A sequence of named arguments, all of type character}
}
\details{
  This function changes the names of \code{x} according to the
  remaining arguments. The argument names are the \emph{old}
  names, the values are the new names.
}

\value{
  The object \code{x} with new names defined by the \dots arguments.
}

\examples{
  x <- c(a=1, b=2)
  rename(x,a="A",b="B")
  
  str(rename(iris,
                  Sepal.Length="SepalLength",
                  Sepal.Width ="SepalWidth",
                  Petal.Length="PetalLenght",
                  Petal.Width ="PetalWidth"
                  ))
}
\keyword{manip}
