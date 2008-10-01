
unarray <- function(x,fill=FALSE){
  x <- drop(x)
  if(!length(dim(x))) return(x)
  if(length(dim(x))==1 || !length(dimnames(x))) return(c(x))
  dims <- dim(x)
  dimnames <- dimnames(x)
  for(i in seq_along(dimnames)){
    if(!length(dimnames[[i]]))
      if(fill)
        dimnames[[i]] <- format(seq_len(dims[i]))
      else
        dimnames[[i]] <- character(dims[i])
    }
  names <- c(reduce(dimnames,outer,dotpaste))
  structure(c(x),names=names)
}

dotpaste <- function(x,y)ifelse(nchar(x)&nchar(y),paste(x,y,sep="."),paste(x,y,sep=""))



has.response <- function(formula,data=NULL){
  #if(length(dim(data))) data <- data[1,]
  as.logical(attr(terms(formula,data=data),"response"))
}


mk.ixes <- function(dims){
 ijk <- as.matrix(seq(dims[1]))
 if(length(dims)>1){
  for(i in 2:length(dims)){
    tmp.nrow <- nrow(ijk)
    ijk <- ijk[rep(seq(nrow(ijk)),dims[i]),]
    ijk <- cbind(ijk,
              rep(
                  seq(dims[i]),
                  rep(tmp.nrow,dims[i])
                  )
                )
  }
 }
 ijk
}


genTable <- function (formula,
                        data=parent.frame(),
                        subset=NULL,
                        na.action=getOption("na.action"),
                        exclude = c(NA, NaN),
                        drop.unused.levels = FALSE,
                        names=NULL,
                        addFreq=TRUE,
                        ...){
   m <- match.call()
   mis.data <- missing(data)
   formula <- try(as.formula(formula),silent=TRUE)
   if(inherits(formula,"try-error")){
      if(is.na(strsplit(formula,": ")[[1]][2]))
        stop(formula)
      else
        stop(strsplit(formula,": ")[[1]][2])
      }
   #if(!is.environment(data) && !is.data.frame(data)) data <- as.data.frame(data)
   if(!mis.data && is.table(data)) data <- as.data.frame(data)
   
   m[[1]] <- as.name("fapply")
   names(m)[2] <- "formula"
   if(mis.data){
     m$data <- parent.frame()
     data <- parent.frame()
   }
#   res <- eval(m, parent.frame())
   if(!missing(subset))
    subset <- eval(substitute(subset),data,parent.frame())

   res <- fapply(formula=formula,data=data,
                  subset=subset,
                  na.action=na.action,
                  exclude=exclude,
                  drop.unused.levels=drop.unused.levels,
                  names=names,
                  addFreq=addFreq,
                  ...)
   by <- attr(res,"by")

#    ii <- do.call("order",by)
#    by <- by[ii,,drop=FALSE]
#    res <- res[ii]

   
   if(is.list(res)){
    isArr <- sapply(res,is.array)
    if(all(isArr))
      res <- clct.arrays(res)
    else
      res <- clct.vectors(res)
    }
   if(has.response(formula,data)){
      fcall <- formula[[2]]
      formula <- formula[-2]
      }
   else
      fcall <- NULL
   if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
        res[is.na(res)] <- 0
   if(length(dim(res)) == 2){
    if(is.null(rownames(res))){
      if(nrow(res)==1) rownames(res) <- deparse(fcall)
      else if( as.character(fcall[[1]]) %in% c("c","cbind","rbind")
              && length(fcall[-1]) == nrow(res))
            rownames(res) <- paste(fcall[-1])
      else if( as.character(fcall[[1]]) %in% c("range"))
        rownames(res) <- c("Min","Max")
      else
        rownames(res) <- seq_len(nrow(res))
    }
   }
   by.dimnames <- lapply(by,function(x)as.character(sort(unique(x))))
   by.dims <- sapply(by.dimnames,length)

   res.dims <- dim(res)
   res.dimnames <- dimnames(res)
   res.dims <- res.dims[-length(res.dims)]
   res.dimnames <- res.dimnames[-length(res.dimnames)]


   ijk.res <- mk.ixes(res.dims)
   ijk.res <- ijk.res[rep(seq(nrow(ijk.res)),nrow(by)),,drop=FALSE]

   ijk.by <- sapply(by,function(bby)
              rep(match(bby,sort(unique(bby))),
                rep(prod(res.dims), nrow(by))
                )
              )
   ijk <- cbind(ijk.res,ijk.by)
   
   tmp <- res
   res <- array(NA,c(res.dims,by.dims))
   res[ijk] <- tmp
   
   if(!length(fcall)) res[is.na(res)] <- 0
   
   dimnames(res) <- c(res.dimnames,by.dimnames)

   as.table(drop(res))
}

aggregate.formula <- function (x,
                        data = parent.frame(),
                        subset = NULL,
                        na.action=getOption("na.action"),
                        sort=FALSE,
                        exclude = c(NA, NaN),
                        drop.unused.levels = FALSE,
                        names=NULL,
                        addFreq=TRUE,
                        as.vars=1,
                        drop.constants=TRUE,
                        ...)
{
    formula <- x
    #if(missing(data)) data <- environment(formula)
    #if (any(attr(terms(formula, data = data), "order") > 1))
    #    stop("interactions are not allowed")
    #m <- match.call(expand.dots = FALSE)
    
   m <- match.call()
   mis.data <- missing(data)
   formula <- try(as.formula(formula),silent=TRUE)
   if(inherits(formula,"try-error")){
      if(is.na(strsplit(formula,": ")[[1]][2]))
        stop(formula)
      else
        stop(strsplit(formula,": ")[[1]][2])
      }
   #if(!missing(data) && !is.environment(data) && !is.data.frame(data)) data <- as.data.frame(data)
   if(!mis.data && is.table(data)) data <- as.data.frame(data)

   #cp <- code.plan(data)
   m[[1]] <- as.name("fapply")
   names(m)[2] <- "formula"
   if(mis.data){
     m$data <- parent.frame()
     data <- parent.frame()
   }
   #if(!length(m$data)) m$data <- parent.frame()
   #m$enclos <- parent.frame()
#   res <- eval(m, parent.frame())
   if(!missing(subset))
    subset <- eval(substitute(subset),data,parent.frame())
   res <- fapply(formula=formula,
                  data=data,
                  subset=subset,
                  na.action=na.action,
                  exclude=exclude,
                  drop.unused.levels=drop.unused.levels,
                  names=names,
                  addFreq=addFreq,
                  ...)
  
   by <- attr(res,"by")
   attr(res,"by") <- NULL
   isArr <- sapply(res,is.array)
   if(!all(isArr)) as.vars <- NULL
   else if(is.na(as.vars)){
    diml <- sapply(res,function(x)length(dim(x)))
    as.vars <- max(diml)
   }
   
   if(has.response(formula,data)){
        fcall <- formula[[2]]
        formula <- formula[-2]
        }
   else {
        ii <- order(attr(by,"row.names"))
        res <- cbind(as.data.frame(by),data.frame(Freq=res))[ii,,drop=FALSE]
        #cp <- cp[names(res)]
        #if(length(cp)) code.plan(res) <- cp
        return(res)
   }
   if(!length(as.vars)){
    res <- t(clct.vectors(lapply(res,unarray)))
    if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
          res[is.na(res)] <- 0
    if(!length(colnames(res))){
      if(ncol(res)==1) colnames(res) <- deparse(fcall)
      else if( as.character(fcall[[1]]) %in% c("c","cbind","rbind")
              && length(fcall[-1]) == ncol(res))
        colnames(res) <- paste(fcall[-1])
      else if( as.character(fcall[[1]]) %in% c("range"))
        colnames(res) <- c("Min","Max")
      else
        colnames(res) <- seq_len(nrow(res))
    }
    if(sort)
      ii <- do.call("order",rev(by))
    else 
      ii <- order(attr(by,"row.names"))
    if(drop.constants){
        keep <- sapply(by,function(x)length(unique(x))>1)
        by <- by[,keep,drop=FALSE]
      }
    res <- cbind(as.data.frame(by),as.data.frame(res,optional=TRUE))[ii,,drop=FALSE]
   }
   else {
    if(sort)
      ii <- do.call("order",by)
    else 
      ii <- order(attr(by,"row.names"))
    res <- lapply(res[ii],to.data.frame,as.vars=as.vars)
    by <- by[ii,,drop=FALSE]
    rp <- sapply(res,nrow)
    ii <- seq_len(nrow(by))
    ii <- rep(ii,rp)
    by <- data.frame(lapply(by,function(x)x[ii]))
    res <- do.call(rbind,res)
    if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
          res[is.na(res)] <- 0
    if(drop.constants){
        keep <- sapply(by,function(x)length(unique(x))>1)
        by <- by[,keep,drop=FALSE]
      }
    res <- cbind(as.data.frame(res),by)
   }
   #cp <- cp[names(res)]
   #if(length(cp)) code.plan(res) <- cp
   res
}

# makeDF <- function(X,as.vars){
#   dimX <- dim(X)
#   dimnamesX <- dimnames(X)
#   alldims <- seq_along(dimX)
#   if(is.character(as.vars)){
#     dimlabels <- names(dimnamesX)
#     as.vars <- match(as.vars,dimlabels)
#   }
#   if(!(as.vars %in% alldims)) stop("wrong as.vars argument")
#   as.rows <- setdiff(alldims,as.vars)
#   row.dimnames <- dimnamesX[as.rows]
#   col.dimnames <- dimnamesX[as.vars]
#   X <- aperm(X,c(as.rows,as.vars))
# }