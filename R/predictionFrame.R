add.residuals <- function(Y,object,variables){
  r <- residuals(object,type="working")
  sapply(variables,function(v){
      Y[,paste(v,"fit",sep=".")] + r
      })
}

prediction.frame <- function(object,newdata=NULL,...,residuals=NULL)
  UseMethod("prediction.frame")

prediction.frame.default <- function(object,newdata=NULL,...,residuals=NULL){
  if(missing(newdata)){
    envir <- attr(formula(object),".Environment")
    vars <- all.vars(formula(object))
    X <- eval(object$call$data, envir)
    if(is.null(X)){
      X <- as.data.frame(lapply(vars,function(v)
              get(v,envir=envir)
            ))
      names(X) <- vars
    }
    else
      X <- X[vars]
  }
  #  X <- model.frame(object)
  else
    X <- newdata
  termLabels <- attr(terms(object),"term.labels")
  variables <- all.vars(delete.response(terms(object)))
  dots.arg <- list(...)
  type.is.variables <- as.logical(length(dots.arg$type)) && dots.arg$type=="variables"
  type.is.terms <- as.logical(length(dots.arg$type)) && dots.arg$type=="terms"
  tvLabels <- if(type.is.variables) variables else termLabels
  
  Y <- predict(object,newdata=newdata,...)
  if(length(Y))
  Ynames <- names(Y)
  if(is.atomic(Y)){
    if(!length(dim(Y))){
      Y <- data.frame(prediction=Y)
    } else {
      Y <- as.data.frame(Y)
    }
    i <- names(Y) %in% tvLabels
    names(Y)[i] <- paste(names(Y)[i],"fit",sep=".")
  }
  else {
    Y <- lapply(seq(along=Y),function(i){
        if(!is.array(Y[[i]])){
          if(length(Y[[i]]) == nrow(X)){
          res <- data.frame(Y[[i]])
          return(res)
          }
          else
            return(NULL)
        }
        else {
          namesY.i <- names(Y)[i]
          res <- as.data.frame(Y[[i]])
          if(nrow(res)==nrow(X)){
            i <- names(res) %in% tvLabels
            names(res)[i] <- paste(names(res)[i],namesY.i,sep=".")
            return(res)
            }
          else
            return(NULL)
        }
      })
    getit <- !sapply(Y,is.null)
    Y <- Y[getit]
    #Ynames <- Ynames[getit]
    #names(Y) <- Ynames
    #browser()
    Y <- do.call("cbind",Y)
  }
  if(missing(residuals))
    return(cbind(X,Y))
  else {
    if(!missing(newdata)) stop("residuals not possible for new data")
    if(is.atomic(residuals)) residuals <- list(type=residuals)

    if(type.is.variables && residuals$type == "partial")
      R <- add.residuals(Y,object,variables)
    else
      R <- do.call("residuals",c(list(object),residuals))
    if(!is.array(R))
      R <- data.frame(resid=R)
    else{
      R <- as.data.frame(R)
      i <- names(R) %in% tvLabels
      names(R)[i] <- paste(names(R)[i],"resid",sep=".")
      }
    if(length(Y) && ncol(R) && nrow(R))
        return(cbind(X,Y,R))
    else if (length(Y))
        return(cbind(X,Y))
    else if (ncol(R) && nrow(R))
        return(cbind(X,R))
    else return(X)
  }
}
# debug(prediction.frame)

# 
# source("aggregateFormula.R")
# 
#   berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
#   berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
#   berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
#   berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")
# 
# require(splines)
#   x <- 1:100
#   z <- factor(rep(LETTERS[1:4],25))
#   y <- rnorm(100,sin(x/10)+as.numeric(z))
#   yxz.ns <- glm(y ~ ns(x,6) + z)
#   yxz.poly <- glm(y ~ poly(x,6) + z)
#   yxz.sincos <- glm(y ~ sin(x/10) + cos(x/10) + z)
#   
# 
# prediction.frame(berk2,type="variables")
# 
# prediction.frame(yxz.ns,type="terms")
# prediction.frame(yxz.ns,type="variables")
# 
# prediction.frame(yxz.ns,type="variables",residuals="partial",se.fit=TRUE,intervals="confidence")
# prediction.frame(yxz.ns,type="terms",residuals="partial",se.fit=TRUE,intervals="confidence")
# 
# #predict(yxz.ns,type="variables",residuals="partial",se.fit=TRUE,intervals="confidence")
# #predict(yxz.ns,type="terms",residuals="partial",se.fit=TRUE,intervals="confidence")
# 
# prediction.frame(yxz.sincos,type="terms",residuals="partial",se.fit=TRUE,intervals="confidence")
