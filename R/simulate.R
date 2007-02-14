Simulate <- simulate.default <- function(
    object,
    conditions,
    ...,
    replications=1,
    names=NULL,
    trace=0
    ){
    m <- match.call()
    if(inherits(m$object,"name"))
      FUN <- match.fun(m$object)
    else{
      expr <- m$object
      if(inherits(expr[[1]],"name") 
            && as.character(expr[[1]]) %in% c("quote","expression","substitute"))
            expr <- expr[[2]]
      if(!inherits(expr,"call")) warning(paste(expr,"may lead to a constant result"))
      vars <- all.vars(expr)
      arglist <- rep(alist(x=),length(vars))
      names(arglist) <- vars
      FUN <- function(){}
      formals(FUN) <- arglist
      body(FUN) <- expr
    }
    conditions <- as.data.frame(conditions)
    results <- vector(mode="list",length=nrow(conditions))
    for(i in 1:nrow(conditions)){
            if(trace){
                cat("\n---------------------------------------------------\n")
                print(conditions[i,])
                }
            myfun <- function(repl){
                if(trace && ( !(repl %% trace) || repl==replications))
                  cat("Replication",repl,"\n")
                do.call(FUN,c(conditions[i,,drop=FALSE],list(...)))
              }
            results[[i]] <- sapply(1:replications,myfun)
            if(length(dim(results[[i]])))  
                  results[[i]] <- as.data.frame(t(results[[i]]))
            else 
                  results[[i]] <- data.frame(result=results[[i]])
        }
#     browser()
    results <- do.call("rbind",results)
    if(!missing(names)) names(result) <- names
    ii <- rep(1:nrow(conditions),rep(replications,nrow(conditions)))
    conditions <- conditions[ii,]
    cbind(conditions,results)
}


# simulate()
