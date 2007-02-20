\name{Transform}
\alias{Transform}
\alias{Transform.data.frame}
\title{Extended Data Frame Transformation}
\description{Transform is a variant of \code{\link[base]{transform}} that preserves
the attributes of the transformed data frame 
and allows for some preliminary computations, which can be used in
variable transformations.}
\usage{
\method{Transform}{data.frame}(.data.,.expr.,\dots)
}
\arguments{
    \item{.data.}{The object to be transformed}
    \item{.expr.}{an expression or closure (a series of expression
    inside curly braces).}
    \item{\dots}{Further arguments of the form \code{tag=value}}
}
\details{
    \code{Transform} is a slightly extended version of \code{\link{transform}}.
    Thus it only does useful things with data frames.
    
    If an expression is given as second argument (\code{.expr.}) then it
    is evaluated with \code{.data.} as environment (that is, the
    variables inside this data frame can be accessed directly in the
    expression). If this expression returns a list with named elements,
    these elements can be accessed in subsequent tag-value assignments.
}
\value{
  The modified value of \code{.data.} with all the attributes of \code{.data.}
  (only the \code{names} attribute is updated, of course).
}
\examples{
berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)

Transform(berkeley,
  list(total=sum(Admitted) + sum(Rejected)),
  Admitted.p = Admitted/total,
  Rejected.p = Rejected/total)

Transform(berkeley,
  { total <- sum(Admitted) + sum(Rejected)
    list(total=total)
  },
  Admitted.p = Admitted/total,
  Rejected.p = Rejected/total)
}
\keyword{manip}
