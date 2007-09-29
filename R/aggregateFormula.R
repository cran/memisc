quickInteraction <- function(by){
  if(is.list(by)){
    n.arg <- length(by)
    f <- 0.
    uf <- 0.
    for(i in 1:n.arg){
      y <- by[[i]]
      y <- as.numeric(y)
      uy <- unique(y)
      y <- match(y,uy,NA)
      l <- length(uy)
      f <- f*l + y - 1
      uf <- unique(f)
      f <- as.double(match(f,uf,NA))
      uf <- seq(length(uf))
    }
  }
  else {
    by <- as.numeric(by)
    uf <- unique(by)
    f <- match(by,uf,NA)
    uf <- seq(length(uf))
  }
  return(structure(f,unique=uf))
}
# debug(quickSplit)

aggregate.formula <- function (x,
                        data=parent.frame(),
                        subset=NULL,
                        na.action,
                        exclude = c(NA, NaN),
                        drop.unused.levels = FALSE,
                        names=NULL,
                        addFreq=TRUE,
                        ...)
{
    formula <- x
    #if (any(attr(terms(formula, data = data), "order") > 1))
    #    stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    
    if(attr(terms(formula,data=data),"response")){
      fcall <- formula[[2]]
      formula <- formula[-2]
      }
    else
      fcall <- NULL
    
    names(m)[2] <- "formula"
    m$formula <- formula
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$names <- NULL
    m[[1]] <- as.name("model.frame")
    by <- eval(m, parent.frame())
    omitted <- attr(by,"na.action")
    if(as.character(formula[[2]])[1]==".")
      by <- by[setdiff(names(by),all.vars(fcall))]

    if(length(fcall)){
      if(length(fcall)==1){
        makeTableCall <- FALSE
        fcall.c <- as.character(fcall)
        if(is.table(data)
            && fcall.c  %in% names(dimnames(data)))
            makeTableCall <- TRUE
        if(is.data.frame(data)
            && is.factor(data[[fcall.c]]))
            makeTableCall <- TRUE
        if(is.environment(data)
            && exists(fcall.c,envir=data)
            && is.factor(get(fcall.c,envir=data))) 
            makeTableCall <- TRUE
        if(makeTableCall)
          fcall <- as.call(c(as.symbol("table"),fcall))
        else
          fcall <- as.call(c(as.symbol("sum"),fcall))
      }
      if(addFreq){
        if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","wtable","percent")){
          if(is.table(data) || (is.data.frame(data) && "Freq" %in% names(data))){
            fcall[[3]] <- as.symbol("Freq")
            if(as.character(fcall[[1]])=="table")
              fcall[[1]] <- as.symbol("wtable")
            by <- by[setdiff(names(by),all.vars(fcall))]
            }
        }
      }
      resp.var.formula <- parse(text=paste("~",paste(all.vars(fcall),collapse="+")))[[1]]
      m$formula <- resp.var.formula
      m$na.action <- na.pass
      data <- eval(m, parent.frame())
      if(length(omitted))
        data <- data[-omitted,,drop=FALSE]
      rows <- seq(nrow(data))
    }
    else rows <- seq(nrow(by))
    #
    # this is FAR less memory intensive than interaction split(rows,by)
    BY <- quickInteraction(by)
    rows <- split.default(rows,BY)
    if(length(fcall)>1){
      workhorse <- function(i){}
      unnfcall <- as.call(unname(as.list(fcall)))
      unnfcall <- deparse(unnfcall)
      newargs <- paste("data$",all.vars(fcall),"[i]",sep="")
      oldargs <- all.vars(fcall)
      for(i in seq(oldargs)) unnfcall <- gsub(oldargs[i],newargs[i],unnfcall,fixed=TRUE)
      newcall <- parse(text=unnfcall)[[1]]
      body(workhorse) <- newcall
      res <- sapply(rows,workhorse)
      # that's MUCH faster than:
      #res <- sapply(rows,function(i)eval(fcall,data[i,]))
    } else
    if(length(fcall)==1){
      res <- c(rowsum(x=data[[all.vars(fcall)]],group=BY,reorder=FALSE,na.rm=FALSE))
      if(missing(names)) names <- "Freq"
    }
    else {
      res <- tabulate(BY,nbins=length(attr(BY,"unique")))
      if(missing(names)) names <- "Freq"
    }
    if(!is.array(res)){
      resp.names <- deparse(fcall)
      if(!missing(names)){
        if(length(names)!=length(resp.names))
          warning("Mismatching names argument ignored")
        else {
          resp.names <- names
          }
      }
        res <- data.frame(res)
        names(res) <- resp.names
    }
    else {
      if(length(rownames(res))){
        resp.names <- rownames(res)
        names(resp.names) <- deparse(fcall)
        }
      else{
        resp.names <- sapply(fcall,deparse)[-1]
        if(length((resp.names))==nrow(res)){
          if(length(names(resp.names))) resp.names <- names(resp.names)
          }
        else {
          resp.names <- as.character(seq(nrow(res)))
          }
        }
      res <- as.data.frame(t(res))
      resp.names <- unlist(resp.names)
      if(!missing(names)){
        if(length(names)!=length(resp.names))
          warning("Mismatching names argument ignored")
        else {
            resp.names <- names
          }
        }
      names(res) <- resp.names
    }
    urows <- sapply(rows,function(ix)ix[1])
    res <- cbind(by[urows,,drop=FALSE],res)
    rownames(res) <- 1:nrow(res)
    structure(res,
      call = match.call(),
      by.vars = names(by)
      )
}

genTable <- function (formula,
                        data=parent.frame(),
                        subset=NULL,
                        na.action,
                        exclude = c(NA, NaN),
                        drop.unused.levels = FALSE,
                        names=NULL,
                        addFreq=TRUE){
   m <- match.call()
   formula <- try(as.formula(formula),silent=TRUE)
   if(inherits(formula,"try-error")){
      if(is.na(strsplit(formula,": ")[[1]][2]))
        stop(formula)
      else
        stop(strsplit(formula,": ")[[1]][2])
      }
      
    
   m[[1]] <- as.name("aggregate")
   names(m)[2] <- "x"
   X <- eval(m, parent.frame())
   by.names <- X@by.vars
   Xcall <- X@call
   response.names <- setdiff(names(X),by.names)
   by <- X[by.names]
   X <- X[response.names]
   I <- length(response.names)
   dimnam <- lapply(by,function(x)as.character(unique(x)))
   if(I>1){
      rhs.label <- deparse(Xcall$x[[2]])
      lresponse.names <- list(response.names)
      names(lresponse.names) <- rhs.label
      dimnam <- c(lresponse.names,dimnam)
      dims <- sapply(dimnam,length)
      res <- array(NA,dim=dims,dimnames=dimnam)
      by <- sapply(by,function(x)match(x,unique(x)))
      i <- 1:I
      i <- rep(i,nrow(by))
      j <- rep(1:nrow(by),rep(I,nrow(by)))
      by <- cbind(i,by[j,])
      X <- t(as.matrix(X))
      res[by] <- X[]
      return(structure(as.table(res),
        call = match.call(),
        by.vars = by.names
        ))
   }
   else {
      dims <- sapply(dimnam,length)
      res <- array(NA,dim=dims,dimnames=dimnam)
      by <- sapply(by,function(x)match(x,unique(x)))
      res[by] <- X[[1]]
      return(structure(as.table(res),
        call = match.call(),
        by.vars = by.names
        ))
   }
}




percent <- function(x,weights=NULL){
  if(!is.factor(x)) stop("percent works only for factors")
  if(missing(weights)){
    tab <- table(x)
  } else {
    tmp <- rowsum(weights,x)
    tab <- structure(rep(0,nlevels(x)),names=levels(x))
    tab[rownames(tmp)] <- tmp[]
  }
  tabsum <- sum(tab)
  perc <- c(100*tab/tabsum)
  c(perc,Total=tabsum)
}


wtable <- function(x,w=NULL){
  if(missing(w)) return(table(x))
  tapply(w,x,sum)
}
