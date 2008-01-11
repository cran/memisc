\name{relabel}
\alias{relabel}
\alias{relabel.default}
\alias{relabel.factor}
\title{Change Labels of factors or of any labeled object}
\description{
  This function changes the labels of a factor of any object
  that has a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute.
}
\usage{
\method{relabel}{default}(x,...)
\method{relabel}{factor}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object with a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute}
  \item{\dots}{A sequence of named arguments, all of type character}
}
\details{
  This function changes the names or labels of \code{x} according to the
  remaining arguments. The argument names are the \emph{old}
  names, the values are the new names.
}
\value{
  The object \code{x} with new labels defined by the \dots arguments.
}
\examples{
  f <- as.factor(rep(letters[1:4],5))
  print(f)
  relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
