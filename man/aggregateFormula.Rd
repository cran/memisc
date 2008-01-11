\name{aggregate.formula}
\alias{aggregate.formula}
\alias{genTable}
\title{Convenient and flexible aggregation of data frames}
\description{
  \code{aggregate.formula} constructs a data frame of summaries conditional on
  given values of independent variables given by a formula. It is a method
  of the generic function \code{aggregate} applied to \code{formula} objects.
  
  \code{genTable} does the same, but produces a \code{table}.
  }
\usage{
\method{aggregate}{formula}(x, data=parent.frame(), subset=NULL, na.action, exclude = c(NA, NaN),
      drop.unused.levels = FALSE, names=NULL, addFreq=TRUE,...)

genTable(formula, data=parent.frame(), subset=NULL, na.action, exclude = c(NA, NaN),
      drop.unused.levels = FALSE, names=NULL, addFreq=TRUE)
}
\arguments{
  \item{x, formula}{a formula object with
    an expression yielding a numeric result on the left hand side and
    the conditioning variables,
    separated by \code{+}, on the right hand side. Interactions are
    ignored. The left hand side of the formula is optional, see details below.}
  \item{data}{an environment or data frame or an object coercable into a data frame.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels of the classifying factors.}
  \item{drop.unused.levels}{a logical indicating whether to drop unused
    levels in the classifying factors.}
  \item{names}{an optional character vector giving names to the
    result(s) yielded by the expression on the left hand side of \code{formula}.
    This argument may be redundant if the left hand results in is a named vector.
    (See the example below.)}
  \item{addFreq}{a logical value. If TRUE and
      \code{data} is a table or a data frame with a variable
      named "Freq", a call to
      \code{table}, \code{\link{wtable}}, or \code{\link{percent}}
      is supplied by an additional argument \code{Freq}
      and a call to \code{table} is translated into
      a call to \code{wtable}.
    }
  \item{...}{further arguments, ignored.}
}
\details{
    If an expression is given as left hand side of the formula, its
    value is computed for any combination of values of the values on the
    right hand side. If the right hand side is a dot, then all
    variables in \code{data} are added to the right hand side of the
    formula.

    If no expression is given as left hand side,
    then the frequency counts for the respective
    value combinations of the right hand variables are computed.

    If a single factor is on the left hand side, then the left hand side is
    translated into an appropriate
    call to \code{table()}. Note that also in this case \code{addFreq} takes effect.
    
    If a single numeric variable is on the left hand side, frequency
    counts weighted by this variable are computed. In these cases,
    \code{aggregate.formula} is equivalent to \code{as.data.frame(xtabs(...))},
    but far less memory intensive if the data set or
    the number of different combinations of the conditioning variables
    is very large. Another difference is that conditioning variables are not
    coerced into factors.
}
\value{
    \code{aggregate.formula}
    results in a data frame with conditional summaries and unique value combinations
    of conditioning variables.
    
    \code{genTable} returns a \link{table}, that is, an array with class \code{"table"}.
}
\seealso{\link{aggregate.data.frame}, \link{xtabs}}
\examples{
ex.data <- expand.grid(mean=c(0,100),sd=c(1,10))[rep(1:4,rep(100,4)),]
ex.data <- transform(ex.data,x=rnorm(n=nrow(ex.data),mean=ex.data$mean,sd=ex.data$sd))

aggregate(~mean+sd,data=ex.data)
aggregate(mean(x)~mean+sd,data=ex.data)
aggregate(mean(x)~mean+sd,data=ex.data,name="Average")
aggregate(c(mean(x),sd(x))~mean+sd,data=ex.data)
aggregate(c(Mean=mean(x),StDev=sd(x),N=length(x))~mean+sd,data=ex.data)
genTable(c(Mean=mean(x),StDev=sd(x),N=length(x))~mean+sd,data=ex.data)
attach(ex.data)
aggregate(c(Mean=mean(x),StDev=sd(x))~mean+sd)
genTable(c(Mean=mean(x),StDev=sd(x))~mean+sd)
detach(ex.data)
aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
aggregate(Admit~.,data=UCBAdmissions)
aggregate(percent(Admit)~.,data=UCBAdmissions)
aggregate(percent(Admit)~Gender,data=UCBAdmissions)
aggregate(percent(Admit)~Dept,data=UCBAdmissions)
aggregate(percent(Gender)~Dept,data=UCBAdmissions)
aggregate(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
genTable(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
}
\keyword{misc}
