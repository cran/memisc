\name{cases}
\alias{cases}
\title{Code Several Logical Conditions into a Factor}
\description{
  \code{cases} takes a sequence of logical expressions and
  codes them into levels of a factor
  }
\usage{
cases(...)
}
\arguments{
  \item{\dots}{A sequence of logical expressions}
}
\details{
  The logical expressions are tested sequentially.
  If the first expression is true then the resulting
  factor attains its first level, otherwise if the
  second expression is true then teh resulting
  factor attains its second level, etc.
  Note that if none of the expressions is true,
  the resulting factor gets \code{NA}. One can, however
  specify a default condition by \code{TRUE}
  as the last argument to this function.

  This should be useful for some more complex recodings,
  for example for index construction.
}
\value{
  A factor with a number of levels equal to the number of
  logical expressions given as arguments and labels
  equal to the (deparsed) logical expressions or names given to the
  arguments.
}

\examples{
df <- data.frame(x = rnorm(n=40), y = rnorm(n=40))
df <- df[do.call(order,df),]
(df <- transform(df,
  x.2=cases(x>0,x<=0),
  y.2=cases(y>0,y<=0),
  z1=cases(
    "Condition 1"=x<0,
    "Condition 2"=(x>=0 & y <0),
    "Condition 3"=TRUE
    ),
  z2=cases(x<0,(x>=0 & y <0), (x>=0 & y >=0))
  ))
xtabs(~x.2+y.2,data=df)
}
\keyword{manip}
