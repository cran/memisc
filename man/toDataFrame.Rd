\name{to.data.frame}
\alias{to.data.frame}
\title{Flexible conversion of arrays into data frames}
\description{
  \code{to.data.frame} converts an array into a data frame, in such a way
  that a chosen dimensional extent forms the variables in the data frame.
  The elements of the array must be either atomic, data frames
  with matching variables, or coercable into such data frames.
}
\usage{
to.data.frame(X,as.vars=1)
}
\arguments{
  \item{X}{an array.}
  \item{as.vars}{a numeric value; indicates the dimensional extend
    which defines the variables. Takes effect only if \code{X} is 
    an atomic array.}
}

\value{
  A data frame.
}

\examples{
berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
berktest1 <- By(~Dept+Gender,
                glm(cbind(Admitted,Rejected)~1,family="binomial"),
                data=berkeley)
berktest2 <- By(~Dept,
                glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                data=berkeley)
Stest1 <- Lapply(berktest2,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
Stest2 <- Sapply(berktest2,function(x)coef(summary(x)))
Stest2.1 <- Lapply(berktest1,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
to.data.frame(Stest1)
to.data.frame(Stest2,as.vars=2)
to.data.frame(Stest2.1)
}
\keyword{manip}
