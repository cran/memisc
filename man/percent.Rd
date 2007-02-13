\name{percent}
\alias{percent}
\title{Table of Percentages with Percentage Base}
\description{
  \code{percent} returns a table of percentages along with
  the percentage base. It will be useful
  in conjunction with \link{aggregate.formula}.
}
\usage{
  percent(x,weights=NULL)
}
\arguments{
  \item{x}{a numeric vector or factor.}
  \item{weights}{a numeric vector of weights of the same length as \code{x}.}
}

\value{
  A character vector of formatted numbers.
}

\examples{
  ex.data <- expand.grid(mean=c(0,25,50),sd=c(1,10,100))[rep(1:9,rep(250,9)),]
  ex.data <- transform(ex.data,x=rnorm(n=nrow(ex.data),mean=ex.data$mean,sd=ex.data$sd))
  ex.data <- transform(ex.data,x.grp = cases( x < 0,
                                              x >= 0 & x < 50,
                                              x >= 50 & x < 100,
                                              x >= 100
                                            ))
  genTable(percent(x.grp)~mean+sd,data=ex.data)

  aggregate(percent(Admit,weight=Freq)~Gender+Dept,data=UCBAdmissions)
}
\keyword{univar}
