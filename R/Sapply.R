SapplyOLD <- function (X, FUN, ..., test.dim=FALSE, simplify = TRUE, USE.NAMES = TRUE){
  sapply.call <- match.call()
  sapply.call$test.dim <- NULL
  sapply.call[[1]] <- as.name("sapply")
  Y <- eval(sapply.call,parent.frame())
  if(!test.dim && !is.array(X)) return(Y)
  dimX <- dim(X)
  dimnamesX <- dimnames(X)
  dimY <- dim(Y)
  if(!length(dimY)){
    return(structure(unname(Y),dim=dimX,dimnames=dimnamesX))
  }
  else {
    if(test.dim && length(dim(Y1 <- FUN(X[[1]])))) {
      dimY <- c(dim(Y1),dimX)
      if(length(dimnames(Y1)))
        dimnamesY <- c(dimnames(Y1),dimnamesX)
      else
        dimnamesY <- c(vector(mode="list",length=length(dim(Y1))),dimnamesX)
    }
    else {
      dimY <- c(dim(Y)[1],dimX)
      if(length(rownames(Y)))
        dimnamesY <- c(dimnames(Y)[1],dimnamesX)
      else
        dimnamesY <- c(list(NULL),dimnamesX)
    }
    return(structure(unname(Y),dim=dimY,dimnames=dimnamesY))
  }
}




numericIfPossible <- function(x){
    if(is.atomic(x)) return(.numericIfPossible(x))
    else {
        res <- lapply(x,.numericIfPossible)
        attributes(res) <- attributes(x)
        return(res)
    }
}

.numericIfPossible <- function(x){
    if(is.numeric(x)) return(x) 
    else if(is.character(x)) return(.Call("numeric_if_possible", as.character(x)))
    else if(is.factor(x)) {
        levels <- .Call("numeric_if_possible",levels(x),PACKAGE="memisc")
        if(is.numeric(levels)){
            return(levels[as.numeric(x)])
        } else return(x)
    }
    else return(x)
}







Sapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE){
    FUN <- match.fun(FUN)
    if(length(dim(X))){
        d.ans <- dim(X)
        dn.ans <- if(length(dimnames(X))) dimnames(X) else NULL
    } else {
        d.ans <- length(X)
        dn.ans <- if(USE.NAMES) NULL else names(X)
    }
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    answer <- .Internal(lapply(X,FUN))
    if (USE.NAMES && is.character(X) && length(d.ans) == 1 && is.null(names(answer))) 
            dn.ans <- X
    if(simplify){
        dd.ans <- NULL
        ddn.ans <- list(NULL)
        DIMS <- .Internal(lapply(answer,dim))
        ulDIMS <- unique(unlist(.Internal(lapply(DIMS,length))))
        if(length(ulDIMS)==1 && ulDIMS > 0){
            DIMS <- array(unlist(DIMS),dim=c(ulDIMS,length(X)))
            common.dims <- rep(NA,ulDIMS)
            for(i in seq(nrow(DIMS))){
                uDIMS.i <- unique(DIMS[i,])
                if(length(uDIMS.i) == 1){
                    common.dims[i] <- uDIMS.i
                }
            }
            if(!any(is.na(common.dims))){
            dd.ans <- common.dims
            ddn.ans <- dimnames(answer[[1]])
            }
        }
        else {
            LEN <- unique(unlist(.Internal(lapply(answer,length))))
            if(length(LEN)==1){
                dd.ans <- LEN
                ddn.ans <- list(names(answer[[1]])) 
                }
        }
        if(!is.null(dd.ans))
            return(array(unlist(answer,recursive=FALSE),dim=c(dd.ans,d.ans),dimnames=c(ddn.ans,dn.ans)))
    }
    return(array(answer,dim=d.ans,dimnames=dn.ans))
}


# Lapply <- function(X, FUN, ...)
#     Sapply(X, FUN, ..., test.dim=FALSE, simplify = FALSE, USE.NAMES = FALSE)

Lapply <- function(X,FUN,...){
    FUN <- match.fun(FUN)
    if(length(dim(X))){
        d.ans <- dim(X)
        dn.ans <- if(length(dimnames(X))) dimnames(X) else NULL
        if (!is.vector(X) || is.object(X))
        X <- as.list(X)
        return(array(
            .Internal(lapply(X,FUN)),
            dim=d.ans,dimnames=dn.ans))
    }
    else {
        if (!is.vector(X) || is.object(X))
        X <- as.list(X)
        return(.Internal(lapply(X,FUN)))
    }
}

to.data.frame <- function(X,as.vars=1){
  if(is.atomic(X)){
    ncols <- dim(X)[as.vars]
    nrows <- prod(dim(X)[-as.vars])
    coln <- dimnames(X)[[as.vars]]
    Z <- dimnames(X)[-as.vars]
    Z <- numericIfPossible(expand.grid(Z))
    ii <- seq(length(dim(X)))
    X <- aperm(X,c(ii[-as.vars],ii[as.vars]))
    dim(X) <- c(nrows,ncols)
    X <- as.data.frame.matrix(X)
    rownames(X) <- rownames(Z) <- 1:nrows
    names(X) <- coln
   }
  else {
    nrows <- prod(dim(X))
    Z <- dimnames(X)
    X <- lapply(X,as.data.frame)
    Z <- expand.grid(Z)
    lnrows <- sapply(X,nrow)
    lncols <- sapply(X,ncol)
    if(!allequal(lncols)) stop("array elements do not match")
    ncols <- lncols[1]
    X <- do.call("rbind",X)
    i <- rep(1:nrows,lnrows)
    Z <- Z[i,,drop=FALSE]
    rownames(X) <- rownames(Z) <- 1:nrow(X)
    }
  cbind(Z,X)
}