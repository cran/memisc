\name{dimrename}
\alias{dimrename}
\alias{colrename}
\alias{rowrename}
\title{Change dimnames, rownames, or colnames}
\description{
  These functions provide an easy way to change the \code{dimnames}, \code{rownames} or \code{colnames} of
  an array.
}
\usage{
dimrename(x, dim = 1, ...)
rowrename(x, ...)
colrename(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An array with dimnames}
  \item{dim}{A vector that indicates the dimensions}
  \item{\dots}{A sequence of named arguments}
}
\details{
  \code{dimrename} changes the dimnames of \code{x} along dimension(s) \code{dim} according to the
  remaining arguments. The argument names are the \emph{old}
  names, the values are the new names.
  \code{rowrename} is a shorthand for changing the rownames,
  \code{colrename} is a shorthand for changing the colnames of a matrix
  or matrix-like object.
}
\value{
  Object \code{x} with changed dimnames.
}

\examples{
m <- matrix(1,2,2)
rownames(m) <- letters[1:2]
colnames(m) <- LETTERS[1:2]
m
dimrename(m,1,a="first",b="second")
dimrename(m,1,A="first",B="second")
dimrename(m,2,"A"="first",B="second")

rowrename(m,a="first",b="second")
colrename(m,"A"="first",B="second")
}

\keyword{manip}
