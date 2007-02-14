\name{wtable}
\alias{wtable}
\title{One-Dimensional Table of Frequencies with Weights}
\description{
  'wtable' is a helper function for \link{aggregate.formula} and \link{genTable}.
  It is useful to recast a contingency table into a format suitable for
  binomial logit analysis.
  }
\usage{
  wtable(x,w=NULL)
}
\arguments{
  \item{x}{a factor}
  \item{w}{a numeric vector of weights of the same length as \code{x}.}
}
\value{
  A (one-dimensional) table of counts.
}
\examples{
  aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
}
\keyword{univar}
