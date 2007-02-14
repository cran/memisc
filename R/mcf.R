# sink("ppp.log",split=TRUE)
rename <- function(x,...){
  subst <- c(...)  
  for(i in 1:length(subst)){
    names(x)[names(x)==names(subst[i])] <- subst[i]
  }
  return(x)
}

relabel <- function(x,...)
  UseMethod("relabel")

relabel.default <- function(x,...){
  if(!is.null(attr(x,"labels"))) labels <- attr(x,"labels")
  else if (!is.null(attr(x,"value.labels"))) labels <- attr(x,"value.labels")
  else if (!is.null(attr(x,"variable.labels"))) labels <- attr(x,"variable.labels")
  else labels <- names(x)
  subst <- c(...)  
  for(i in 1:length(subst)){
    labels[labels==names(subst[i])] <- subst[i]
  }
  if(!is.null(attr(x,"labels"))) attr(x,"labels") <- labels
  else if (!is.null(attr(x,"value.labels"))) attr(x,"value.labels") <- labels
  else if (!is.null(attr(x,"variable.labels"))) attr(x,"variable.labels") <- labels
  else names(x) <- labels
  return(x)
}

relabel.factor <- function(x,...){
  subst <- c(...)  
  for(i in 1:length(subst)){
    levels(x)[levels(x)==names(subst[i])] <- subst[i]
  }
  return(x)
}


dimrename <- function(x,dim=1,...){
  subst <- c(...)  
  for(i in 1:length(subst)){
    for(j in dim)
      dimnames(x)[[j]][dimnames(x)[[j]]==names(subst[i])] <- subst[i]
  }
  return(x)
}

colrename <- function(x,...)
  dimrename(x,dim=2,...)

rowrename <- function(x,...)
  dimrename(x,dim=1,...)

# m <- matrix(1,2,2)
# rownames(m) <- letters[1:2]
# colnames(m) <- LETTERS[1:2]
# m
# m <- dimrename(m,1,a="erstes",b="zweites")
# m
# m <- dimrename(m,1,A="erstes",B="zweites")
# m
# m <- dimrename(m,2,"A"="erstes",B="zweites")
# m


recode <- function(x,...)
  UseMethod("recode")
# recode.numeric <- function(x,...){
#   attribs <- attributes(x)
#   subst <- c(...)
#   for(i in 1:length(subst)){
#     fo <- subst[[i]]
#     lhs <- eval(fo[[2]])
#     rhs <- eval(fo[[3]])
#     x[x %in% lhs] <- rhs
#   }
#   attributes(x) <- attribs
#   return(x)
# }


# recode.factor <- function(x,...){
#   subst <- c(...)
#   newcodes <- seq(along=subst)
#   oldlabels <- lapply(subst,function(fo) eval(fo[[2]]))
#   newlabels <- sapply(subst,function(fo) eval(fo[[3]]))
#   res <- rep(NA,length(x))
#   for(i in seq(along=subst)){
#     res[x %in% oldlabels[[i]]] <- newcodes[i]
#   }
#   factor(res,levels=newcodes,labels=newlabels)
# }

recode.numeric <- function(x,...,otherwise="NA",to.factor=FALSE){
  mycall <- match.call()
  mycall$otherwise <- mycall$to.factor <- NULL
  recodings <- as.list(mycall[-(1:2)])
  if(missing(to.factor))
    to.factor <- any(sapply(recodings,function(r)is.character(eval(r[[2]]))))
  recoded <- rep(FALSE,length(x))
  res <- rep(NA,length(x))
  if(!to.factor){
    for(r in recodings){
      new <- as.numeric(eval(r[[2]]))
      old <- r[[3]]
      if(as.character(old[[1]])=="range"){
        old[2:3] <- lapply(old[2:3],function(ex){
            switch(as.character(ex)[1],
              min=min(x),
              max=max(x),
              ex
            )
          })
        old <- eval(old)
        to.change <- x >= old[1] & x <= old[2]
        }
      else
        to.change <- x %in% as.numeric(eval(r[[3]]))
      
      recoded[to.change] <- TRUE
      res[to.change] <- new[1]
    }
    if(!all(recoded) && otherwise!="NA"){
      if(otherwise=="copy")
        res[!recoded] <- x[!recoded]
      else
        res[!recoded] <- as.numeric(otherwise[1])
    }
  }
  else{
    labels <- sapply(recodings,function(r)as.character(eval(r[[2]])))
    codes <- match(labels,unique(labels))
    for(i in seq(along=recodings)){
      r <- recodings[[i]]
      new <- codes[i]
      old <- r[[3]]
      if(as.character(old[[1]])=="range"){
        old[2:3] <- lapply(old[2:3],function(ex){
            switch(as.character(ex)[1],
              min=min(x),
              max=max(x),
              ex
            )
          })
        old <- eval(old)
        to.change <- x >= old[1] & x <= old[2]
        }
      else
        to.change <- x %in% as.numeric(eval(r[[3]]))

      recoded[to.change] <- TRUE
      res[to.change] <- new[1]
    }
    if(!all(recoded) && otherwise!="NA"){
      if(otherwise=="copy"){
        otherlabels <- as.character(unique(x[!recoded]))
        othercodes <- max(codes) + seq(length(otherlabels))
        labels <- c(labels,otherlabels)
        codes <- c(codes,othercodes)
        i <- match(x[!recoded],unique(x[!recoded]))
        res[!recoded] <- othercodes[i]
      }
      else{
        othercode <- max(codes)+1
        res[!recoded] <- othercode
        codes <- c(codes,othercode)
        labels <- c(labels,as.character(otherwise))
      }
    }
    res <- factor(res,levels=codes,labels=labels)
  }
  return(res)
}

recode.factor <- function(x,...,otherwise="NA"){
  mycall <- match.call()
  mycall$otherwise <- NULL
  if(length(mycall)<=2) return(x)
  recodings <- as.list(mycall[-(1:2)])
  tmp <- levels(x)
  recoded <- rep(FALSE,nlevels(x))
  for(i in seq(along=recodings)){
    r <- recodings[[i]]
    if(r[[1]]==as.symbol("<-")){
      old <- as.character(eval(r[[3]]))
      new <- as.character(eval(r[[2]]))
      recoded[tmp %in% old] <- TRUE
      tmp[tmp %in% old] <- new[1]
    }
  }
  if(!all(recoded)){
    if(otherwise=="copy")
      newlevels <- unique(tmp)
    else if(otherwise=="NA")
      newlevels <- unique(tmp[recoded])
    else {
      tmp[!recoded] <- as.character(otherwise[1])
      newlevels <- unique(tmp)
    }
  }
  codetable <- match(tmp,newlevels)
  i <- match(x,levels(x))
  factor(codetable[i],levels=unique(codetable),labels=newlevels)
}


recode.default <- recode.numeric

#  debug(recode.numeric)
  
#   df <- data.frame(x = 1:7)
# 
#   transform(df,
#       r2=recode(x,   7  <- 1,
#              range(min,3)<- 2,
#               4  <- 3),
#       r2=recode(x,   7  <- "a",
#              range(min,3)<- "b",
#               4  <- "c")
#       )



cases <- function(...){
  subst <- list(...)
  deflabels <- sapply(match.call()[-1],deparse)
  
  codes <- seq(along=subst)
  labels <- names(subst)
  if(length(labels))
    labels <- ifelse(labels=="",deflabels,labels)
  else labels <- deflabels

  len <- max(sapply(subst,length))
  #if(length(len)>1) stop("arguments do not match")
  
  res <- rep(0,len)
  done <- rep(FALSE,len)
  for(i in codes){
    res[subst[[i]] & !done] <- i
    done <- done | subst[[i]]
  }
  return(factor(res,levels=codes,labels=labels))
}


reorder.array <- function(x,dim=1,names=NULL,indices=NULL,FUN=mean,...){
  if(length(dim)>1) dim <- dim[1]
  if(!missing(names) && missing(indices)){
    other.names <- setdiff(dimnames(x)[[dim]],names)
    sort.names <- c(names,other.names)
    sort.indices <- match(dimnames(x)[[dim]],sort.names)
  }
  else if(!missing(indices)){
    other.indices <- setdiff(1:dim(x)[dim],indices)
    sort.indices <- c(indices,other.indices)
  } else {
    results <- apply(x,dim,FUN)
    sort.indices <- order(results)
  } 
  all.indices <- lapply(dim(x),function(d)1:d)
  all.indices[[dim]] <- sort.indices
  do.call("[",c(list(x),all.indices))
}

reorder.matrix <- reorder.array

