\name{Simulate}
\alias{Simulate}
\title{Simulate Values from a Function or Expression}
\description{
  \code{Simulate} is a function to simplify simulation studies.
   It evaluates or calls its first argument within the
    settings given by its remaining arguments
    for a predetermined number of replications.
  }
\usage{
Simulate(object,conditions,...,replications=1,names=NULL,trace=0)
}
\arguments{
  \item{object}{a function or an expression that produces simulation results for each replication.}
  \item{conditions}{a data frame with various simulation conditions.
    Each combination of values in this
    list is given \code{replications} times to \code{object} as arguments}
  \item{...}{other arguments to \code{objects}, held fixed in the
    simulation experiment}
  \item{replications}{number of replication in each experimental setting}
  \item{names}{names given to the data frame that results from \code{simulation}}
  \item{trace}{an integer value determining the amount of information
    output during the simulation process. If \code{trace} equals zero
    nothing is reported during the simulation run.
    Otherwise, the replication number is output for each
    multiple of \code{trace}.
    }  
}
\value{
    A data frame that contains experimental conditions and simulation results.
}
\examples{
Normal.example <- function(mean=0,sd=1,n=10){
  x <- rnorm(n=n,mean=mean,sd=sd)
  c(
    Mean=mean(x),
    Median=median(x),
    Var=var(x)
  )
}

Normal.simres <- Simulate(
    Normal.example,
    expand.grid(
          mean=0,
          sd=c(1,10),
          n=c(10,100)
          ),
    replications=200,
    trace=50)

genTable(sd(Median)~sd+n,data=Normal.simres)


expr.simres <- Simulate(median(rnorm(n,mean,sd)),
      expand.grid(
          n=c(10,100),
          mean=c(0,1),
          sd=c(1,10)
      ),
    replications=200
    )

genTable(sd(result)~sd+n,data=expr.simres)

\dontrun{
## This takes a little bit longer
lm.example <- function(a=0,b=1,n=101,xrange=c(-1,1),serr=1){
  x <- seq(from=xrange[1],to=xrange[2],length=n)
  y <- a + b*x + rnorm(n,sd=serr)
  lm.res <- lm(y~x)
  coef <- lm.res$coef
  names(coef) <- c("a","b")
  coef
}

lm.simres <- Simulate(
      lm.example,
      expand.grid(
      serr=c(0.1,1,10),
      n=c(11,101,501)
      ),
      replications=200,
      trace=50
    )
genTable(c(sd(a),sd(b))~serr+n,data=lm.simres)
}
}
\keyword{misc}
