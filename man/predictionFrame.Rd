\name{prediction.frame}
\alias{prediction.frame}
\alias{prediction.frame.default}
\title{Data frame of model predictions and independent variables}
\description{
  \code{prediction.frame} is a convenience function that produces a data frame 
  of independent variables and predictions of a fitted model.
  }
\usage{
  \method{prediction.frame}{default}(object,newdata=NULL,...,residuals=NULL)
}
\arguments{
\item{object}{a model object from which predictions are generated.}
\item{newdata}{an optional data frame for out-of-sample predictions.}
\item{...}{further arguments passed to \code{\link{predict}}.}
\item{residuals}{a character vector that specifies residuals
that are added to the resulting data frame.}
}
\value{
  A data frame.
}
\examples{
lm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi + pop15*dpi, data = LifeCycleSavings)

str(prediction.frame(lm1))
str(prediction.frame(lm1, se.fit=TRUE))
str(prediction.frame(lm1, interval="prediction"))
str(prediction.frame(lm1, type="terms"))
str(prediction.frame(lm1,
                  se.fit=TRUE,
                  type="terms"))

str(prediction.frame(lm1,
                  se.fit=TRUE,
                  type="terms",
                  residuals="working"))

str(prediction.frame(lm1,
                  se.fit=TRUE,
                  type="terms",
                  residuals="partial"))

str(prediction.frame(lm2,
                  se.fit=TRUE,
                  type="terms",
                  residuals="partial"))

berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

str(prediction.frame(berk2,
                  se.fit=TRUE,
                  type="terms",
                  residuals="partial"))
}
\keyword{datagen}
