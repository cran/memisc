\name{mtable}
\alias{mtable}
\alias{format.mtable}
\alias{print.mtable}
\alias{toLatex.mtable}
\alias{write.mtable}
\alias{relabel.mtable}
\title{Comparative Table of Model Estimates}
\description{
  \code{mtable} produces a table of estimates for several models.
  }
\usage{
mtable(...,coef.style=getOption("coef.style"),
           summary.stats=TRUE,
           factor.style=getOption("factor.style"),
           getSummary=function(obj,...)UseMethod("getSummary"),
           float.style=getOption("float.style"),
           digits=min(3,getOption("digits")))

\method{format}{mtable}(x,
          coef.title="Coefficients",
          summary.title="Summaries",
          colsep="\t",
          rowsep="\n",
          trim=TRUE,
          trimleft=trim,
          trimright=trim,
          center.at=NULL,
          align.integers=c("dot","right","left"),
          topsep="",
          bottomsep="",
          sectionsep="",
          forLaTeX=FALSE,
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits="-1",
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = if(forLaTeX) " $\\times$ " else " x ",
          ...
          )

\method{print}{mtable}(x,trim=FALSE,center.at=getOption("OutDec"),
      colsep=" ",
      topsep="=",bottomsep="=",sectionsep="-",...)

\method{toLatex}{mtable}(object,...)

write.mtable(object,file="",...)

\method{relabel}{mtable}(x,...)
}
\arguments{
     \item{...}{as argument to \code{mtable}: several model objects, e.g. of class \code{\link{lm}}; as argument to \code{print.mtable},
     \code{toLatex.mtable}, \code{write.mtable}: further arguments
     passed to \code{format.mtable}; as argument to \code{format.mtable}:
     further arguments passed to \code{format.default};
     as argument to \code{relabel.mtable}: further arguments
     passed to \code{\link{dimrename}}.
     }
     \item{coef.style}{a character string which specifies the style of
        coefficient values, whether standard errors, Wald/t-statistics,
         or significance levels are reported, etc. See \code{\link{coef.style}}.}
     \item{summary.stats}{if \code{FALSE}, no summary statistics
        are repored. If \code{TRUE}, all summary statistics
        produced by \code{getSummary} are reported.
        This argument may also contain a character vector with
        the names of the summary statistics to report}
     \item{factor.style}{a character string that specifies the style in
        which factor contrasts are labled. See \link{factor.style}.}
     \item{getSummary}{a function that computes model-related statistics that
        appear in the table. See \code{\link{getSummary}}.}
     \item{float.style}{default format for floating point numbers if
        no format is specified by \code{coef.style}; see \code{{float.style}}.
        }
     \item{digits}{number of significant digits if not specified by
        the template returned from \code{\link{getCoefTemplate}}
        \code{\link{getSummaryTemplate}}
        }
     \item{x, object}{an object of class \code{mtable}}
     \item{coef.title}{a character vector, the title for the reported coefficients.}
     \item{summary.title}{a character vector, the title for the reported model summaries.}
     \item{colsep}{a character string which seperates the columns in the output.}
     \item{rowsep}{a character string which seperates the rows in the output.}
     \item{trim}{should leading and trailing spaces be trimmed?}
     \item{trimleft}{should leading spaces be trimmed?}
     \item{trimright}{should trailing spaces be trimmed?}
     \item{center.at}{a character string on which resulting values are centered.
        Typically equal to ".". This is the default when \code{forLaTeX==TRUE}.
        If \code{NULL}, reported values are not centered.}
     \item{align.integers}{how to align integer values.}
     \item{topsep}{a character string that is recycled to a top rule.}
     \item{bottomsep}{a character string that is recycled to a bottom rule.}
     \item{sectionsep}{a character string that is recycled to seperate coefficients
        from summary statistics.}
     \item{forLaTeX}{should LaTeX code be produced?}
     \item{useDcolumn}{should the \code{dcolumn} LaTeX package be used?}
     \item{colspec}{LaTeX table column format specifyer(s).}
     \item{LaTeXdec}{the decimal point in the final LaTeX output.}
     \item{ddigits}{alignment specification or digits after the decimal point.}
     \item{useBooktabs}{should the \code{booktabs} LaTeX package be used?}
     \item{toprule}{appearance of the top border of the LaTeX \code{tabular} environment.}
     \item{midrule}{how are coefficients and summary statistics
        separated in the LaTeX \code{tabular} environment.}
     \item{bottomrule}{appearance of the bottom border of the LaTeX \code{tabular} environment.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
     \item{file}{a file where to write to; defaults to console output.}
}
\details{
  \code{mtable} constructs a table of estimates for regression-type models.
  \code{format.mtable} formats suitable for use with output or conversion functions
  such as \code{print.mtable}, \code{toLatex.mtable}, or \code{write.mtable}.
}
\value{
    A call to \code{mtable} results in an object that inherits from \code{mtable}
    with the following components:
    
    \item{coefficients}{an array that contains the model coefficients.}
    \item{summaries}{a matrix that contains the model summaries.}

    In addition the \code{mtable} object has attributes \code{as.row},
    \code{as.col}, \code{kill.col}, \code{kill.header},
    and \code{coef.dim} necessary for formatting.
}
\examples{
lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

(mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2))

#
# This should result in the following output:
#
# Calls:
# Model 1: lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
# Model 2: lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
# Model 3: lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
#
# ==================================================
#                  Model 1     Model 2     Model 3
# --------------------------------------------------
# Coefficients
# (Intercept)      30.628***    6.360***   28.566***
#                  (7.409)     (1.252)     (7.355)
# pop15            -0.471**                -0.461**
#                  (0.147)                 (0.145)
# pop75            -1.934                  -1.691
#                  (1.041)                 (1.084)
# dpi                           0.001      -0.000
#                              (0.001)     (0.001)
# ddpi                          0.529*      0.410*
#                              (0.210)     (0.196)
# --------------------------------------------------
# Summaries
# R-squared         0.262       0.162       0.338
# adj. R-squared    0.230       0.126       0.280
# sigma             3.931       4.189       3.803
# F                 8.3         4.5         5.8
# p                 0.001       0.016       0.001
# Log-likelihood -137.8      -141.0      -135.1
# Deviance        726.2       824.7       650.7
# AIC             283.7       290.0       282.2
# BIC             291.3       297.7       293.7
# N                50          50          50
# ==================================================
#
#

# Some other output formats

write.mtable(mtable123)
\dontrun{file123 <- "mtable123.txt"
write.mtable(mtable123,file=file123)
file.show(file123)}
\dontshow{toLatex(mtable123)}
\dontrun{texfile123 <- "mtable123.tex"
cat(toLatex(mtable123),sep="\\n",file=texfile123)
file.show(texfile123)}
#
# You should see:
# \%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%
# \%
# \% Calls:
# \% Model 1:   lm(formula = sr ~ pop15 + pop75, data = LifeCycleSavings)
# \% Model 2:   lm(formula = sr ~ dpi + ddpi, data = LifeCycleSavings)
# \% Model 3:   lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
# \%
# \%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%
# \begin{tabular}{lD{.}{.}{-1}D{.}{.}{-1}D{.}{.}{-1}}
# \toprule
#  & \multicolumn{1}{c}{Model 1}
#  & \multicolumn{1}{c}{Model 2}
#  & \multicolumn{1}{c}{Model 3}
# \\\\
# \midrule
# Coefficients    &                 &                 &                \\\\
# (Intercept)     &   30.628^{***}  &    6.360^{***}  &   28.566^{***} \\\\
#                 &   (7.409)       &   (1.252)       &   (7.355)      \\\\
# pop15           &   -0.471^{**}   &                 &   -0.461^{**}  \\\\
#                 &   (0.147)       &                 &   (0.145)      \\\\
# pop75           &   -1.934        &                 &   -1.691       \\\\
#                 &   (1.041)       &                 &   (1.084)      \\\\
# dpi             &                 &    0.001        &   -0.000       \\\\
#                 &                 &   (0.001)       &   (0.001)      \\\\
# ddpi            &                 &    0.529^{*}    &    0.410^{*}   \\\\
#                 &                 &   (0.210)       &   (0.196)      \\\\
# \midrule
# Summaries       &                 &                 &                \\\\
# R-squared       &    0.262        &    0.162        &    0.338       \\\\
# adj. R-squared  &    0.230        &    0.126        &    0.280       \\\\
# sigma           &    3.931        &    4.189        &    3.803       \\\\
# F               &    8.3          &    4.5          &    5.8         \\\\
# p               &    0.001        &    0.016        &    0.001       \\\\
# Log-likelihood  & -137.8          & -141.0          & -135.1         \\\\
# Deviance        &  726.2          &  824.7          &  650.7         \\\\
# AIC             &  283.7          &  290.0          &  282.2         \\\\
# BIC             &  291.3          &  297.7          &  293.7         \\\\
# N               &   50            &   50            &   50           \\\\
# \bottomrule
# \end{tabular}
#

berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)

berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

mtable(berk0,berk1,berk2)

#
# This should result in the following output:
# 
# Calls:
# berk0: glm(formula = cbind(Admitted, Rejected) ~ 1, family = "binomial",
#     data = berkeley)
# berk1: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = berkeley)
# berk2: glm(formula = cbind(Admitted, Rejected) ~ Gender + Dept, family = "binomial",
#     data = berkeley)
#
# =======================================================
#                        berk0       berk1       berk2
# -------------------------------------------------------
# Coefficients
# (Intercept)           -0.457***   -0.220***    0.582***
#                     (-14.972)    (-5.675)     (8.436)
# Gender: Female-Male               -0.610***    0.100
#                                  (-9.553)     (1.235)
# Dept: B-A                                     -0.043
#                                              (-0.395)
# Dept: C-A                                     -1.263***
#                                             (-11.841)
# Dept: D-A                                     -1.295***
#                                             (-12.234)
# Dept: E-A                                     -1.739***
#                                             (-13.792)
# Dept: F-A                                     -3.306***
#                                             (-19.452)
# -------------------------------------------------------
# Summaries
# McFadden R-sq.                     0.107       0.977
# Cox-Snell R-sq.                    0.020       0.172
# Nagelkerke R-sq.                   0.116       0.979
# phi                    1.000       1.000       1.000
# Likelihood-ratio                  93.4       856.9
# p                                  0.000       0.000
# Log-likelihood      -473.0      -426.3       -44.6
# Deviance             877.1       783.6        20.2
# AIC                  948.0       856.5       103.1
# BIC                  954.4       869.4       148.1
# N                   4526        4526        4526
# =======================================================

mtable(berk0,berk1,berk2,coef.style="stat")
mtable(berk0,berk1,berk2,coef.style="ci")
mtable(berk0,berk1,berk2,coef.style="ci.vertical")
mtable(berk0,berk1,berk2,coef.style="ci.horizontal")
mtable(berk0,berk1,berk2,coef.style="all")
mtable(berk0,berk1,berk2,coef.style="all.nostar")

mtable(by(berkeley,berkeley$Dept,function(x)glm(cbind(Admitted,Rejected)~Gender,data=x,family="binomial")))
#
# This should result in the following output:
#
# Calls:
# A: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# B: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# C: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# D: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# E: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# F: glm(formula = cbind(Admitted, Rejected) ~ Gender, family = "binomial",
#     data = x)
# 
# =====================================================================================
#                         A          B          C          D          E          F
# -------------------------------------------------------------------------------------
# Coefficients
# (Intercept)           0.492***   0.534***  -0.536***  -0.704***  -0.957***  -2.770***
#                      (0.072)    (0.088)    (0.115)    (0.104)    (0.162)    (0.220)
# Gender: Female-Male   1.052***   0.220     -0.125      0.082     -0.200      0.189
#                      (0.263)    (0.438)    (0.144)    (0.150)    (0.200)    (0.305)
# -------------------------------------------------------------------------------------
# Summaries
# McFadden R-sq.        1.000      1.000      1.000      1.000      1.000      1.000
# Cox-Snell R-sq.       0.020      0.000      0.001      0.000      0.002      0.001
# Nagelkerke R-sq.      1.000      1.000      1.000      1.000      1.000      1.000
# phi                   1.000      1.000      1.000      1.000      1.000      1.000
# Likelihood-ratio     19.1        0.3        0.8        0.3        1.0        0.4
# p                     0.000      0.611      0.386      0.585      0.320      0.536
# Log-likelihood       -5.9       -5.1       -6.4       -6.3       -5.8       -4.9
# Deviance              0.0        0.0       -0.0        0.0        0.0       -0.0
# AIC                  15.7       14.3       16.9       16.6       15.6       13.8
# BIC                  25.4       23.0       26.5       26.0       24.3       23.0
# N                   933        585        918        792        584        714
# =====================================================================================



mtable(By(~Gender,glm(cbind(Admitted,Rejected)~Dept,family="binomial"),data=berkeley))
#
# This should result in the following output:
#
# Male: glm(formula = cbind(Admitted, Rejected) ~ Dept, family = "binomial")
# Female: glm(formula = cbind(Admitted, Rejected) ~ Dept, family = "binomial")
# 
# ========================================
#                     Male       Female
# ----------------------------------------
# Coefficients
# (Intercept)         0.492***    1.544***
#                    (0.072)     (0.253)
# Dept: B-A           0.042      -0.790
#                    (0.113)     (0.498)
# Dept: C-A          -1.028***   -2.205***
#                    (0.135)     (0.267)
# Dept: D-A          -1.196***   -2.166***
#                    (0.126)     (0.275)
# Dept: E-A          -1.449***   -2.701***
#                    (0.177)     (0.279)
# Dept: F-A          -3.262***   -4.125***
#                    (0.231)     (0.330)
# ----------------------------------------
# Summaries
# McFadden R-sq.      1.000       1.000
# Cox-Snell R-sq.     0.174       0.136
# Nagelkerke R-sq.    1.000       1.000
# phi                 1.000       1.000
# Likelihood-ratio  514.8       268.9
# p                   0.000       0.000
# Log-likelihood    -18.4       -16.1
# Deviance           -0.0         0.0
# AIC                48.7        44.2
# BIC                84.1        77.3
# N                2691        1835
# ========================================

}
\keyword{misc}
