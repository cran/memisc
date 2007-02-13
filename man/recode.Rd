\name{recode}
\alias{recode}
\alias{recode.default}
\alias{recode.factor}
\alias{recode.numeric}
\title{Recode Factors and Numeric Vectors}
\description{
  \code{recode} substitutes old values of a factor or a numeric
  vector by new ones, just like the recode facilities in some
  commercial statistical packages.
}
\usage{
  recode(x,...)
}
\arguments{
  \item{x}{A factor or numerical vector}
  \item{\dots}{One or more assignment expressions, the value side specifies
    the codes to change, the target side gives the new code.
    Vectors may appear on the value side.
    If the \code{x} is numeric a call to \code{range}
    may also appear. In that case, values inside this range are exchanged by
    new values. If one of the arguments to \code{range} is \code{min},
    it is substituted by the minimum of \code{x}.
    If one of the arguments to \code{range} is \code{max},
    it is substituted by the maximum of \code{x}.
    }
}

\value{
  The object \code{x} with new codes specified by the \code{...} arguments.
}

\examples{
df <- data.frame(a = 1:7,
                   f = factor(letters[1:7]))

transform(df,
  r = recode(a,   1  -> 5,
                  2:3-> 2,
                  7  -> 6))
  
transform(df,
  r = recode(a,   1  -> 5,
                  2:3-> 2,
                  7  -> 6,
                  otherwise="copy"))

transform(df,
  r = recode(a,   1  -> 5,
                  2:3-> 2,
                  7  -> 6,
                  otherwise=100))

transform(df,
  r = recode(a,   1   -> 5,
                  2:3 -> 2,
                  7   -> 6,
                  to.factor=TRUE))

transform(df,
  r = recode(a,   1   -> "a",
                  2:3 -> "b",
                  7   -> "c"))

transform(df,
  r = recode(a,   1   -> "a",
                  2:3 -> "b",
                  7   -> "c",
                  otherwise="z"))

transform(df,
  r = recode(a,   1   -> "a",
                  2:3 -> "b",
                  7   -> "c",
                  otherwise="copy"))

transform(df,
  r = recode(a,   range(min,3) -> "a",
                             5 -> "b",
                             7 -> "d",
                  otherwise="copy"))

transform(df,
  r = recode(a,   range(max,3) -> "a",
                             1 -> "b",
                             2 -> "d",
                  otherwise="copy"))

transform(df,
  r = recode(f, letters[1:3] -> "a",
                letters[4:5] -> "b",
                "f"          -> "c"))

transform(df,
  r = recode(f, letters[1:3] -> "a",
                letters[4:5] -> "b",
                "f"          -> "c",
                otherwise="copy"))

transform(df,
  r = recode(f, letters[1:3] -> "a",
                letters[4:5] -> "b",
                "f"          -> "c",
                otherwise="z"))

}
\keyword{manip}
