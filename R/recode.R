setGeneric("recode", function(x,...,
                              copy=getOption("recode_copy",identical(otherwise,"copy")),
                              otherwise=NA) standardGeneric("recode"))
setMethod("recode","item",function(x,...,
                                   copy=getOption("recode_copy",identical(otherwise,"copy")),
                                   otherwise=NA){
  recodings <- match.call(expand.dots=FALSE)$...
  recodings <- recodings[nzchar(sapply(recodings,paste,collapse=""))]
  if(length(otherwise)!=1) stop("otherwise= argument must have length 1")
  
  if(any(sapply(sapply(recodings,"[[",1),paste)!="<-"))
    stop("invalid recoding request")
  if(!length(recodings)) return(x)
  newcodes <- lapply(recodings,"[[",2)
  conditions <- lapply(recodings,"[[",3)
  has.range <- paste(lapply(conditions,"[[",1)) == "range"
  if(any(has.range)){
    oldcodes <- conditions
    has.min <- paste(lapply(conditions[has.range],"[[",2)) == "min"
    has.max <- paste(lapply(conditions[has.range],"[[",3)) == "max"
    if(any(has.min)){
      min.list <- list(min=min(x))
      conditions[has.range][has.min] <- lapply(
                conditions[has.range][has.min],
                function(x) do.call("substitute",list(x,min.list))
                )
    }
    if(any(has.max)){
      max.list <- list(max=max(x))
      conditions[has.range][has.max] <- lapply(
                conditions[has.range][has.max],
                function(x) do.call("substitute",list(x,max.list))
                )
    }
  }
  else
      oldcodes <- sapply(conditions,eval)
  NA_recoded <- any(unlist(suppressWarnings(sapply(oldcodes,is.na))))
      
  conditions[has.range] <- lapply(conditions[has.range],
                          function(x)
                          as.call(list(as.symbol("&"),
                            as.call(list(as.symbol("<="),x[[2]],quote(x@.Data))),
                            as.call(list(as.symbol("<="),quote(x@.Data),x[[3]]))
                            )
                          ))
  conditions[!has.range] <- lapply(conditions[!has.range],
                          function(x)
                          as.call(list(as.symbol("%in%"),quote(x@.Data),x))
                          )
  torecode <- sapply(conditions,eval,envir=environment(),enclos=parent.frame())
  if(!is.matrix(torecode)) torecode <- t(torecode)
  newcodes <- sapply(newcodes,eval,parent.frame())
  nevtrue <- colSums(torecode) == 0
  if(any(nevtrue)){
    nrecng <- paste(recodings[nevtrue])
    if(length(nrecng)>1)
      warning("recodings ",paste(nrecng,collapse=", ")," have no consequences")
    else
      warning("recoding ",nrecng," has no consequences")
  }
  ambiguous <- rowSums(torecode) > 1
  if(any(ambiguous))
    stop("recoding request is ambiguous")
  #y <- if(is.character(newcodes)) as.character(x) else x
  y <- x
  labels(y) <- NULL
  if(is.character(newcodes) && is.numeric(y)) {
    newcodes <- structure(
      seq_along(newcodes),
      names=newcodes
      )
  }
  #newcodes <- newcodes[!nevtrue]
  #torecode <- torecode[,!nevtrue,drop=FALSE]
  for(i in seq(along=newcodes)){
    if(!nevtrue[i])
      y[torecode[,i]] <- as.vector(newcodes[i],mode=storage.mode(y))
  }
  if(!copy){
    recoded <- as.logical(rowSums(torecode))
    tmp <- as.vector(otherwise,mode=storage.mode(y))
    length(otherwise) <- length(y)
    otherwise[] <- tmp
    y[!recoded] <- otherwise[!recoded]
    nNA <-sum(is.na(otherwise[!recoded])) - sum(is.na(x[!recoded]))
    if(nNA > 0)
      warning("recoding created ",nNA," NAs")
    if(!NA_recoded)
      y[is.na(x)] <- NA
}
  newvlab <- newcodes[nzchar(names(newcodes))]
  bijective <- FALSE
  lab.y <- lab.x <- labels(x)
  if(!is.list(oldcodes)){
      # This happens only in one-to-one recodings
      if(length(oldcodes) == length(newcodes) &&
          all(oldcodes %in% newcodes)){
          if(copy) bijective <- TRUE
          else if(length(lab.x) && all(lab.x@values %in% oldcodes))
              bijective <- TRUE
      }
  }
  if(length(lab.x) && bijective && !length(names(newcodes))){
      ii <- match(oldcodes,lab.x@values)
      jj <- match(newcodes,lab.y@values)
      lab.y@.Data[jj] <- lab.x@.Data[ii]
      labels(y) <- lab.y
  }
  else if(length(lab.x) && copy){
    lab.y.val <- lab.y@values
    lab.oldcodes <- lapply(conditions,Substitute,list(x=lab.y.val))
    lab.torecode <- sapply(lab.oldcodes,eval,envir=environment(),enclos=parent.frame())
    for(i in seq(along=newcodes)){
      lab.y.val[lab.torecode[,i]] <- as.vector(newcodes[i],mode=storage.mode(lab.y.val))
    }
    lab.y.val <- sort(unique(lab.y.val))
    ii <- match(lab.y.val,lab.y@values)
    lab.y.lab <- lab.y@.Data[ii]
    labels(y) <- new("value.labels",lab.y.lab,values=lab.y.val)
    if(length(newvlab))
      labels(y) <- labels(y) + newvlab
  }
  else if(length(newvlab)){
    if(length(names(otherwise)))
      newvlab <- new("value.labels",names(newvlab),values=newvlab) + otherwise
    labels(y) <- newvlab
    if(measurement(y) %in% c("interval","ratio"))
        measurement(y) <- "nominal"
  }
#   else if(length(newvlab)){
#     if(identical(otherwise,"copy"))
#       labels(y) <- labels(y) + newvlab
#     else
#       labels(y) <- newvlab
#     }

  return(y)
})

setMethod("recode","vector",function(x,...,
                                     copy=getOption("recode_copy",identical(otherwise,"copy")),
                                     otherwise=NA){
    call <- match.call()

    if(length(otherwise)!=1) stop("otherwise= argument must have length 1")

    if("recodes" %in% names(call)
       || is.character(call[[3]])
       ) {
        return(car_recode(var=x,...))
    }
    recodings <- match.call(expand.dots=FALSE)$...
    if("to.factor" %in% names(recodings)
       && is.logical(recodings[["to.factor"]])
       ) {
        warning("'to.factor' argument no longer in use")
        recodings["to.factor"] <- NULL
    }
    recodings <- recodings[nzchar(sapply(recodings,paste,collapse=""))]
    if(any(sapply(sapply(recodings,"[[",1),paste)!="<-"))
        stop("invalid recoding request")

    if(!length(recodings)) return(x)
    newcodes <- lapply(recodings,"[[",2)
    oldcodes <- lapply(recodings,"[[",3)
    NA_recoded <- any(unlist(suppressWarnings(sapply(oldcodes,is.na))))
    has.range <- paste(lapply(oldcodes,"[[",1)) == "range"
    if(any(has.range)){
        has.min <- paste(lapply(oldcodes[has.range],"[[",2)) == "min"
        has.max <- paste(lapply(oldcodes[has.range],"[[",3)) == "max"
        if(any(has.min)){
            min.list <- list(min=min(x, na.rm=TRUE))
            oldcodes[has.range][has.min] <- lapply(
                oldcodes[has.range][has.min],
                function(x) do.call("substitute",list(x,min.list))
            )
        }
        if(any(has.max)){
            max.list <- list(max=max(x, na.rm=TRUE))
            oldcodes[has.range][has.max] <- lapply(
                oldcodes[has.range][has.max],
                function(x) do.call("substitute",list(x,max.list))
            )
        }
    }
    oldcodes[has.range] <- lapply(oldcodes[has.range],
                                  function(x)
                                      as.call(list(as.symbol("&"),
                                                   as.call(list(as.symbol("<="),x[[2]],as.symbol("x"))),
                                                   as.call(list(as.symbol("<="),as.symbol("x"),x[[3]]))
                                                   )
                                              ))
    oldcodes[!has.range] <- lapply(oldcodes[!has.range],
                                   function(x)
                                       as.call(list(as.symbol("%in%"),as.symbol("x"),x))
                                   )
    torecode <- sapply(oldcodes,eval,envir=environment())
    if(!is.matrix(torecode)) torecode <- t(torecode)
    newcodes <- sapply(newcodes,eval,parent.frame())
    nevtrue <- colSums(torecode, na.rm=TRUE) == 0
    if(any(nevtrue)){
        nrecng <- paste(recodings[nevtrue])
        if(length(nrecng)>1)
            warning("recodings ",paste(nrecng,collapse=", ")," have no consequences")
        else
            warning("recoding ",nrecng," has no consequences")
    }
    ambiguous <- rowSums(torecode, na.rm=TRUE) > 1
    if(any(ambiguous))
        stop("recoding request is ambiguous")
    y <- if(is.character(newcodes)) as.character(x) else x
    newcodes <- newcodes[!nevtrue]
    torecode <- torecode[,!nevtrue,drop=FALSE]
    for(i in seq(along=newcodes)){
        y[torecode[,i]] <- newcodes[i]
    }
    if(!copy){
        if(is.character(otherwise)) newcodes <- c(newcodes, otherwise)
        recoded <- as.logical(rowSums(torecode))
        recoded[is.na(recoded)] <- TRUE
        tmp <- as.vector(otherwise,mode=storage.mode(y))
        length(otherwise) <- length(y)
        otherwise[] <- tmp
        y[!recoded] <- otherwise[!recoded]
        nNA <- sum(is.na(otherwise[!recoded])) - sum(is.na(x[!recoded]))
        if(nNA > 0)
            warning("recoding created ",nNA," NAs")
        if(!NA_recoded)
            y[is.na(x)] <- NA
    }
    if(is.character(y)) {
        levels <- sort(unique(y[y%nin%newcodes]))
        levels <- c(newcodes,levels)
        y <- factor(y,levels=levels)
    }
    return(y)
})

setMethod("recode","factor",function(x,...,copy=getOption("recode_copy",identical(otherwise,"copy")),
                                     otherwise=NA){
  recodings <- match.call(expand.dots=FALSE)$...

  if(length(otherwise)!=1) stop("otherwise= argument must have length 1")

  if("to.factor" %in% names(recodings)
    && is.logical(recodings[["to.factor"]])
    ) {
      warning("'to.factor' argument no longer in use")
      recodings["to.factor"] <- NULL
    }
  recodings <- recodings[nzchar(sapply(recodings,paste,collapse=""))]
  if(any(sapply(sapply(recodings,"[[",1),paste)!="<-"))
    stop("invalid recoding request")

  if(!length(recodings)) return(x)
  newcodes <- sapply(recodings,"[[",2)
  if(copy)
      newcodes <- as.character(newcodes) # 'copy=TRUE' requires the result also to be a factor ... 
  if(is.character(newcodes))
      to_factor <- TRUE
  else
      to_factor <- FALSE
  oldcodes <- lapply(recodings,"[[",3)
  oldcodes <- lapply(oldcodes,eval,envir=environment())
  torecode <- sapply(oldcodes,function(o) x %in% o)
  if(!is.matrix(torecode)) torecode <- t(torecode)
  nevtrue <- colSums(torecode) == 0
  if(any(nevtrue)){
    nrecng <- paste(recodings[nevtrue])
    if(length(nrecng)>1)
      warning("recodings ",paste(nrecng,collapse=", ")," have no consequences")
    else
      warning("recoding ",nrecng," has no consequences")
  }
  ambiguous <- rowSums(torecode) > 1
  if(any(ambiguous))
    stop("recoding request is ambiguous")

  if(to_factor){
      y <- integer(length=length(x))
      for(i in 1:ncol(torecode)){
          y[torecode[,i]] <- i
      }
      if(copy){
          olevels <- levels(unique(x[y==0,drop=TRUE]))
          max.y <- max(y)
          for(i in seq(along=olevels)){
              y[x == olevels[i]] <- max.y + i
          }
          y <- factor(y,levels=1:max(y),labels=c(newcodes,olevels))
      }
      else if(is.character(otherwise)){
          max.y <- max(y)
          y[y == 0] <- max.y + 1
          y <- factor(y,levels=1:max(y),labels=c(newcodes,otherwise[1]))
      }
      else {
          y <- factor(y,levels=1:max(y),labels=newcodes)
          recoded <- as.logical(rowSums(torecode))
          nNA <- sum(is.na(y[!recoded]) & !is.na(x[!recoded]))
          if(nNA > 0)
              warning("recoding created ",nNA," NAs")
      }
  }
  else {
      y <- vector(mode=storage.mode(newcodes),
                  length=length(x))
      for(i in 1:ncol(torecode))
          y[torecode[,i]] <- newcodes[i]
      recoded <- as.logical(rowSums(torecode))
      recoded[is.na(recoded)] <- TRUE
      tmp <- as.vector(otherwise,mode=storage.mode(y))
      length(otherwise) <- length(y)
      otherwise[] <- tmp
      y[!recoded] <- otherwise[!recoded]      
      nNA <- sum(is.na(y[!recoded]) & !is.na(x[!recoded]))
      if(nNA > 0)
          warning("recoding created ",nNA," NAs")
  }
  return(y)
})

