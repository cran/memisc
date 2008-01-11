\name{wtable}
\alias{wtable}
\title{One-dimensional table of (weighted) frequencies}
\description{
  'wtable' is a helper function for \link{aggregate.formula} and \link{genTable}.
  It can be used to recast a contingency table into a format needed for
  binomial logit analyses.
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
