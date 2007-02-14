By <- function(formula,expr,data=parent.frame()){
  m <- match.call()
  expr <- m$expr
  formula <- m$formula
  if(is.environment(data)){
    allVars <- union(all.vars(formula),all.vars(expr))
    data <- lapply(allVars,function(v)get(v,envir=data))
    names(data) <- allVars
  } 
  data <- as.data.frame(data)
  fun <- function(x) eval(expr,data[x,])
  factors <- all.vars(formula)
  factors <- data[factors]
  nd <- nrow(data)
  ans <- eval(substitute(tapply(1:nd, factors, fun)), data)
  attr(ans, "call") <- match.call()
  class(ans) <- "by"
  ans
}
