\name{getSummary}
\alias{getSummary}
\alias{getSummary.lm}
\alias{getSummary.glm}
\title{Get Model Summaries for Use with "mtable"}
\description{A generic function and methods to collect coefficients
  and summary statistics from a model object.
  }
\usage{
  \method{getSummary}{lm}(obj, alpha=.05)
  \method{getSummary}{glm}(obj, alpha=.05)
}
\arguments{
  \item{obj}{a model object, e.g. of class \code{lm} or \code{glm}}
  \item{alpha}{level of the confidence intervals; their coverage should
    be 1-alpha/2 }
}
\details{
  The generic function \code{getSummary} is called by \code{\link{mtable}}
  in order to obtain the coefficients and summaries of model objects.
  In order to adapt \code{\link{mtable}} to models of classes other
  than \code{\link{lm}} or \code{\link{glm}} one needs to
  define \code{getSummary} methods for these classes and
  to set a summary template via \code{\link{setSummaryTemplate}}
}
\value{
  Any method of \code{getSummary} must return a list with the following
  components:

  \item{coef}{an array with coefficient estimates;
    the lowest dimension \emph{must} have the following
    names and meanings:
    \tabular{rcl}{
    \code{est} \tab  \tab the coefficient estimates,\cr
    \code{se}  \tab  \tab the estimated standard errors,\cr
    \code{stat}\tab  \tab t- or Wald-z statistics,\cr
    \code{p}   \tab  \tab significance levels of the statistics,\cr
    \code{lwr} \tab  \tab lower confidence limits, \cr
    \code{upr} \tab  \tab upper confidence limits. \cr 
    }
    The higher dimensions of the array correspond to
    the individual coefficients and, in multi-equation models,
    to the model equations.
    }
  \item{sumstat}{a vector containing the model summary statistics;
    the components may have arbitrary names.}
}
\keyword{misc}
\keyword{utilities}