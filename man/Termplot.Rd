\name{Termplot}
\alias{Termplot}
\alias{Termplot.default}
\alias{Termplot.lmList}
\title{Produce a Term Plot Lattice}
\description{
  \code{Termplot} produces a lattice plot of termplots.
  Terms are not plotted individually, rather the terms
  in which a variable appears are summed and plotted.
  }
\usage{
  
\method{Termplot}{default}(object, ..., variables=NULL,  col.term = 2,
        lty.term = 1, lwd.term = 1.5, col.se = "orange", lty.se = 2,
        lwd.se = 1, col.res = "gray", cex = 1, pch = par("pch"),
        col.smth = "darkred", lty.smth = 2, lwd.smth = 1,
        span.smth = 2/3, aspect="fill", xlab=NULL, ylab=NULL,
        main=paste(deparse(object$call),collapse="\n"),
        models = c("rows", "columns"), layout = NULL)
}
\arguments{
  \item{object}{an model fit object, or a list of model objects}
  \item{...}{further model objects.}
  \item{variables}{a character vector giving the names of 
    independent variables; note that
    the combined effect of all terms containing the respective
    values will be plotted; if empty, the effect of each independent
    variable will be plotted. Currently, higher-order terms
    will be ignored.}
  \item{col.term}{same meaning as in \code{\link{termplot}}.}
  \item{lty.term}{same meaning as in \code{\link{termplot}}.}
  \item{lwd.term}{same meaning as in \code{\link{termplot}}.}
  \item{col.se}{same meaning as in \code{\link{termplot}}.}
  \item{lty.se}{same meaning as in \code{\link{termplot}}.}
  \item{lwd.se}{same meaning as in \code{\link{termplot}}.}
  \item{col.res}{same meaning as in \code{\link{termplot}}.}
  \item{cex}{same meaning as in \code{\link{termplot}}.}
  \item{pch}{same meaning as in \code{\link{termplot}}.}
  \item{col.smth}{same meaning as in \code{\link{termplot}}.}
  \item{lty.smth}{same meaning as in \code{\link{termplot}}.}
  \item{lwd.smth}{same meaning as in \code{\link{termplot}}.}
  \item{span.smth}{same meaning as in \code{\link{termplot}}.}
  \item{aspect}{aspect ratio of the plot(s), see \code{\link{xyplot}}.}
  \item{xlab}{label of the x axis, see \code{\link{xyplot}}.}
  \item{ylab}{label of the y axis, see \code{\link{xyplot}}.}
  \item{main}{main heading, see \code{\link{xyplot}}.}
  \item{models}{character; should models arranged in rows or columns?}
  \item{layout}{layout specification, see \code{\link[lattice]{xyplot}.}}
}
\value{
  A trellis object.
}
\examples{

library(lattice)
library(grid)
\dontshow{
if(interactive())
  old.prompt <- grid::grid.prompt(TRUE)
}

lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

Termplot(lm2)
Termplot(berk2)
Termplot(lm0,lm1,lm2)
Termplot(berk0,berk1,berk2)

Termplot(By(~Gender,glm(cbind(Admitted,Rejected)~Dept,family="binomial"),data=berkeley))
Termplot(By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial"),data=berkeley))

require(splines)
xyz <- data.frame(
  x = 1:100,
  z = factor(rep(LETTERS[1:4],25))
)
xyz <- transform(xyz,
  y = rnorm(100,sin(x/10)+x/50+as.numeric(z))
)
yxz.lin <- glm(y ~ x + z, data=xyz)
yxz.bs <- glm(y ~ bs(x,6) + z, data=xyz)
yxz.ns <- glm(y ~ ns(x,6) + z, data=xyz)
yxz.poly <- glm(y ~ poly(x,6) + z, data=xyz)
yxz.sincos <- glm(y ~ sin(x/10) + cos(x/10) + x + z, data=xyz)
#
# This shows off a crucial feature: terms containing
# the same variable are not plotted
# individually but their combined effect is plotted
#
Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,models="columns",
  span.smth=1/3)

Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,variables="x",
  span.smth=1/3)


\dontshow{
if(interactive())
  grid::grid.prompt(old.prompt)
}
  
}
\keyword{hplot}
