toLatex.matrix <- function(object,
          show.titles=TRUE,
          digits=0,
          format="f",
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=digits,
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          ...){
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  digits <- integer(m) 
  digits[] <- d
  fo <- format
  format <- integer(m)
  format[] <- fo
  #print(digits)
  body <- array("",dim=dim(object))
  for(i in seq(along=digits)) {
    #print(digits[i])
    body[,i] <- formatC(object[,i],digits=digits[i],format=format[i])
    body[is.na(object)] <- ""
    }
  ans <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  if(show.titles){
    if(length(colnames(object))){
      header <- sapply(colnames(object),function(x)paste("\\multicolumn{1}{c}{",x,"}",sep=""))
      ans <- rbind(
          header,
          ans
          )
    }
    if(length(rownames(object)))
      ans <- cbind(c("",rownames(object)),ans)
    }
  ans <- apply(ans,1,paste,collapse=" & ")
  ans <- paste(ans,"\\\\")
  if(show.titles && length(colnames(object)))
    ans <- c(
        toprule,
        ans[1],
        midrule,
        ans[-1],
        bottomrule
        )
  body.spec <- character(ncol(object))
  body.spec[] <- colspec
  if(show.titles && length(rownames(object)))
    tabspec <- c("l",body.spec)
  else
    tabspec <- body.spec
  tabspec <- paste(tabspec,collapse="")
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  structure(ans,class="Latex")
}

toLatex.default <- function(object,...) toLatex.matrix(as.matrix(object),...)