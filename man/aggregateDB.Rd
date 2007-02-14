\name{aggregateDB}
\title{Aggregate Data Base Tables According to a Formula}
\alias{aggregateDB}
\alias{mergeSQL}
\description{
  \code{aggregateDB} constructs a data frame of summaries 
  from one or several data base tables conditional on
  given values of independent variables given by a formula.
  }
\usage{
aggregateDB(formula,
            table,
            connection,
            catalog="catalog",
            subset=NULL,
            names=NULL,
            showSQL=TRUE,
            allow.sql.eval=FALSE)

mergeSQL(x,y,by=NULL,by.x=by,by.y=by,all=FALSE,all.x=all,all.y=all)
}
\arguments{
  \item{formula}{a formula object with
    an expression yielding a numeric result on the left hand side and
    the conditioning variables,
    separated by \code{+}, on the right hand side. Interactions are
    ignored. The left hand side of the formula is optional, see details below.}
  \item{table}{a character vector giving the name of a data base table.}
  \item{connection}{a data base connection.}
  \item{catalog}{an optional character vector giving the name of a data base table
    that contains value labels.}
  \item{subset}{an optional expression specifying a subset of observations
    to be used.}
  \item{names}{an optional character vector giving new names to the 
    variables in the resulting data frame; names of this character
    vector are the old names, values are the new names.}
  \item{showSQL}{logical; should the generated SQL statements be shown?}
  \item{allow.sql.eval}{logical; should, where possible, the data base
    server compute aggregate functions?}
  \item{x,y}{data base table names.}
  \item{by,by.x,by.y}{names of data base table columns by which
  table rows are joined}
  \item{all,all.x,all.y}{logical; should rows be used that contain NULL?}
}
\details{
    If an expression is given as left hand side of the formula, its
    value is computed for any combination of values of the values on the
    right hand side. 

    If no expression is given as left hand side,
    then the frequency counts for the respective
    value combinations of the right hand variables are computed.

    If a single factor is on the left hand side, then the left hand side is
    translated into an appropriate
    call to \code{table()}.
    
    If a single numeric variable is on the left hand side, frequency
    counts weighted by this variable are computed. 

    \code{mergeSQL} is a little helper to conveniently construct
    \code{JOIN}s of data base tables without the need to qode SQL
    statements.
}
\value{
    A data frame with conditional summaries and unique value combinations
    of conditioning variables.
}
\seealso{\code{\link{aggregate.formula}}, packages \code{DBI} and \code{RSQLite}}
\examples{
\dontrun{
# This is a real life data management example from the author's work.
# It is the very reason for the existence of aggregatedDB
#
# First we got to 'plug in' the data base ...
library(RSQLite)
if(!exists("drv"))
  drv <- dbDriver("SQLite")
if(!exists("con"))
  con <- dbConnect(drv, dbname = "~/forschung/pieb.db3")
# This is a 400 MB(!) SQLite data base that contains
# several tables contructed from cumulated eurobarometer
# survey data files, spanning the period from 1973 to 2002.


poldisc.data <- aggregateDB(table(poldisc)~nation+eb+educrec+sex+age+year,
            subset = educrec < 900 & sex < 900 & age < 900 & poldisc < 900,
            connection=con,
            allow.sql.eval=TRUE,
            table="ebtrend",
            names=c(educrec="education",sex="gender")
            )

polint.poldisc.data <- aggregateDB(percent(polint)~poldisc,
            subset=polint < 900 & poldisc < 900,
            connection=con,
            table=mergeSQL("political_interest","ebtrend",by=c("nation","eb","id")))

}
}
\keyword{misc}
