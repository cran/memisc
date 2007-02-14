# source("applyTemplate.R")
# source("trimws.R")

# stars.template <- function(x) UseMethod("stars.template")
# ci.template <- function(x) UseMethod("ci.template")
# sumstat.template <- function(x) UseMethod("sumstat.template")

.SummaryTemplates <- list()
.CoefTemplates <- list()


.CoefTemplates$default <- c(est="($est:#)($p:*)",
                                          se="(($se:#))")
.CoefTemplates$stat <- c(est="($est:#)($p:*)",
                                      stat="(($stat:#))")
.CoefTemplates$all <- c(est="($est:#)($p:*)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )
.CoefTemplates$all.nostar <- c(est="($est:#)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )

.CoefTemplates$horizontal <- t(c(est="($est:#)($p:*)",
                                          se="(($se:#))"))

.CoefTemplates$ci <- c(est="($est:3)",
                                      se="(($se:#))",
                                      ci="[($lwr:#);($upr:#)]")

.CoefTemplates$ci.vertical <- c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )

.CoefTemplates$ci.horizontal<- matrix(c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        ),ncol=2,nrow=2,byrow=TRUE,
                                        dimnames=list(
                                          c("est","ci"),
                                          c("est","se")
                                          ))



setCoefTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  OldCoefTemplates <- .CoefTemplates
  for(coef.style in argnames){
      .CoefTemplates[[coef.style]] <<- arg[[coef.style]]
  }
  return(invisible(OldCoefTemplates))
}

getFirstMatch <- function(x,n){
  for(n. in n){
    if(n. %in% names(x)) return(x[[n.]])
  }
  return(x[["default"]])
}

getCoefTemplate <- function(style){
  if(missing(style)) return(.CoefTemplates)
  else return(.CoefTemplates[[style]])
}


getSummary <- function(obj,alpha=.05) UseMethod("getSummary")

getSummary.lm <- function(obj,
            alpha=.05
            ){
  smry <- summary(obj)
  coef <- smry$coef

  numdf <- unname(smry$fstatistic[2])
  dendf <- unname(smry$fstatistic[3])

  lower <- coef[,1] + coef[,2]*qt(p=alpha/2,df=dendf)
  upper <- coef[,1] + coef[,2]*qt(p=1-alpha/2,df=dendf)

  coef <- cbind(coef,lower,upper)
  
  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  sigma <- smry$sigma
  r.squared <- smry$r.squared
  adj.r.squared <- smry$adj.r.squared
  F <- unname(smry$fstatistic[1])
  p <- pf(F,numdf,dendf,lower.tail=FALSE)
  N <- sum(smry$df[1:2])
  ll <- logLik(obj)
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          sigma         = sigma,
          r.squared     = r.squared,
          adj.r.squared = adj.r.squared,
          F             = F,
          numdf         = numdf,
          dendf         = dendf,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)
  
  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat)
}


getSummary.glm <- function(obj,
            alpha=.05){
  
  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
    else sum(smry$df[1:2])
  
  coef <- smry$coef

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,lower,upper)
  
  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  phi <- smry$dispersion
  LR <- smry$null.deviance - smry$deviance
  df <- smry$df.null - smry$df.residual
  
  ll <- logLik(obj)
  deviance <- deviance(obj)


  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-smry$null.deviance/N)
    #LM.pwr <- exp(-smry$deviance/N)
    
    McFadden <- 1- smry$deviance/smry$null.deviance
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
    }
  else {
    LR <- NA
    df <- NA
    p <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
    }

  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          phi         = phi,
          LR             = LR,
          df         = df,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          McFadden      = McFadden,
          Cox.Snell       = Cox.Snell,
          Nagelkerke    = Nagelkerke,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)
  
  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat)
}


.SummaryTemplates$lm <- 
  c(
          "R-squared"     = "($r.squared:f#)",
          "adj. R-squared" = "($adj.r.squared:f#)",
          sigma         = "($sigma:#)",
          F             = "($F:f1#)",
          p             = "($p:#)",
          "Log-likelihood"  = "($logLik:f1#)",
          Deviance      = "($deviance:f1#)",
          AIC           = "($AIC:f1#)",
          BIC           = "($BIC:f1#)",
          N             = "($N:d)"
  )

.SummaryTemplates$glm <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          phi         = "($phi:#)",
          "Likelihood-ratio" = "($LR:f1#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f1#)",
          Deviance      = "($deviance:f1#)",
          AIC           = "($AIC:f1#)",
          BIC           = "($BIC:f1#)",
          N             = "($N:d)"
  )

getSummaryTemplate <- function(x){
  if(missing(x)) return(.SummaryTemplates)
  if(is.character(x)) cls <- x
  else cls <- class(x)
  return(getFirstMatch(.SummaryTemplates,cls))
}

setSummaryTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  OldSummaryTemplates <- .SummaryTemplates
  for(cls in argnames){
      .SummaryTemplates[[cls]] <<- arg[[cls]]
  }
  return(invisible(OldSummaryTemplates))
}

options(coef.style="default")
options(baselevel.sep="-")
options(factor.style="($f): ($l)")
options(float.style="f")

prettyNames <- function(str,
                        contrasts,
                        xlevels,
                        factor.style=getOption("factor.style"),
                        baselevel.sep=getOption("baselevel.sep")
                        ){
   str <- gsub(":"," x ",str,fixed=TRUE)
   for(f in names(contrasts)){
      contrast.f <- contrasts[[f]]
      levels <- xlevels[[f]]
      if(is.character(contrast.f))
        contrast.matrix <- do.call(contrast.f,list(n=levels))
      if(is.matrix(contrast.f))
        contrast.matrix <- contrast.f
      if(!length(colnames(contrast.matrix))){
        oldlabels <- newlabels <- as.character(1:ncol(contrast.matrix))
        }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.treatment",
              "contr.SAS"
              )){
         baselevel <- setdiff(rownames(contrast.matrix),colnames(contrast.matrix))
         newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         oldlabels <- colnames(contrast.matrix)
      }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.sum",
              "contr.helmert"
              )){
         newlabels <- apply(contrast.matrix,2,
                                          function(x)rownames(contrast.matrix)[x>=1])
         oldlabels <- colnames(contrast.matrix)
      }
      else {
        oldlabels <- newlabels <- colnames(contrast.matrix)
      }
      from <- paste(f,oldlabels,sep="")
      to <- sapply(newlabels,
        function(l)applyTemplate(c(f=f,l=l),template=factor.style))
      for(i in 1:length(from))
        str <- gsub(from[i],to[i],str,fixed=TRUE)
   }
   str
}

mtable <- function(...,
                    coef.style=getOption("coef.style"),
                    summary.stats=TRUE,
                    factor.style=getOption("factor.style"),
                    getSummary=function(obj,...)UseMethod("getSummary"),
                    float.style=getOption("float.style"),
                    digits=min(3,getOption("digits"))
                    ){
  args <- list(...)
  if(length(args)==1 && inherits(args[[1]],"by"))
    args <- args[[1]]
  argnames <- names(args)
  if(!length(argnames)) {
    m<-match.call()
    m$coef.style <- NULL
    m$summary.stats <- NULL
    argnames <- sapply(m[-1],as.character)
  }
  n.args <- length(args)
  #coef.template <- eval(coef.template,parent.frame())
  #sumstat.template <- eval(sumstat.template,parent.frame())
  arg.classes <- lapply(args,class)
  if(any(sapply(arg.classes,length))==0) stop("don't know how to handle these arguments")
  summaries <- lapply(args,getSummary)
  stemplates <- lapply(args,getSummaryTemplate)
  sumstats <- lapply(seq(n.args),function(i){
        drop(applyTemplate(summaries[[i]]$sumstat,
            template=stemplates[[i]]))
      })
  ctemplate <- getCoefTemplate(coef.style)
  ctdims <- dim(ctemplate)
  lctdims <- length(ctdims)
  if(lctdims>2) stop("can't handle templates with dim>2")
  as.row <- if(lctdims) c(1,3) else as.row <- 1:2
  coef.dim <- if(lctdims) 3 else coef.dim <- 2
  coefs <- lapply(seq(n.args),function(i){
        coef.i <- summaries[[i]]$coef
        contrasts.i <- args[[i]]$contrasts
        xlevels.i <- args[[i]]$xlevels
        rownames(coef.i) <- prettyNames(rownames(coef.i),
                        contrast=contrasts.i,
                        xlevels=xlevels.i,
                        factor.style=factor.style)
        ans <- apply(coef.i,1,function(x)applyTemplate(x,
            template=ctemplate,float.style=float.style))
        if(length(dim(ctemplate))){
          newdims <- c(dim(ctemplate),dim(ans)[-1])
          newdimnames <- c(dimnames(ctemplate),dimnames(ans)[-1])
          newdimnames <- lapply(1:length(newdims),function(i){
              if(length(newdimnames[[i]])) return(newdimnames[[i]])
              else return(as.character(1:newdims[i]))
              })
          dim(ans) <- newdims
          dimnames(ans) <- newdimnames
        } else rownames(ans) <- names(ctemplate)
        return(ans)
      })
  coefs <- comb.arrays(coefs)
  n.dims <- length(dim(coefs))
  as.col <- setdiff(1:n.dims,as.row)
  if(all(lctdims)) kill.header <- length(as.col)
  else kill.header <- 0
  kill.col <- 2
  #if(n.dims>4) stop("getSummary should return an array of at most dimension 3")
  
  dimnames(coefs)[[n.dims]] <- argnames
  
  sumstats <- comb.vectors(sumstats)
  colnames(sumstats) <- argnames
  sumstats <- sumstats[summary.stats,,drop=FALSE]
  coefs[is.na(coefs)] <- ""

  calls <- lapply(args,function(x)x$call)
  names(calls) <- argnames
  
  sumstats[is.na(sumstats)] <- ""
  structure(list(coefficients=as.table(coefs),summaries=as.table(sumstats),
    calls=calls,
    as.row=as.row,
    as.col=as.col,
    kill.col=kill.col,
    kill.header=kill.header,
    coef.dim=coef.dim),
    class="mtable")
}




comb.arrays <- function(args){
  dims <- sapply(args,dim)
  n.args <- length(args)
  if(is.list(dims)) stop("Dimensions do not match")

  cdimnames <- vector(mode="list",length=nrow(dims))
  for(i in 1:nrow(dims)){
    for(j in 1:n.args){
      if(j==1) cdimnames[[i]] <- dimnames(args[[1]])[[i]]
      else cdimnames[[i]] <- union(cdimnames[[i]], dimnames(args[[j]])[[i]])
    }
  }
  cdims <- sapply(cdimnames,length)
  cdims <- c(cdims,n.args)
  res <- array(NA,dim=cdims,dimnames=c(cdimnames,list(NULL)))
#   str(res)
  for(j in 1:n.args){
    rhs <- list(as.symbol("["),as.call(list(as.symbol("[["),as.symbol("args"),j)))
    rhs <- as.call(rhs)
    lhs <- c(as.symbol("[<-"),as.symbol("res"),dimnames(args[[j]]),list(j))
    ex <- as.call(c(lhs,rhs))
    res <- eval(ex)
  }
  res
}

comb.vectors <- function(args){
  n.args <- length(args)
  for(j in 1:n.args){
    if(j==1) cnames <- names(args[[1]])
    else cnames <- union(cnames,names(args[[j]]))
    }
  res <- matrix(NA,nrow=length(cnames),ncol=n.args)
  rownames(res) <- cnames
  for(j in 1:n.args){
    res[names(args[[j]]),j] <- args[[j]]
    }
  res
}


centerAt <- function(x,at=getOption("OutDec"),integers=c("dot","right","left"),skip=0){
  has.dot <- setdiff(grep(at,x,fixed=TRUE),skip)
  if(!any(has.dot>0)) return(x)
  x <- trimws(x)
  is.int <- setdiff(seq(x),union(has.dot,skip))
  splitted <- strsplit(x[has.dot],at,fixed=TRUE)
  left <- sapply(splitted,function(x)x[1])
  maxleft <- max(nchar(left))
  right <- sapply(splitted,function(x)paste(x[-1],collapse="."))
  maxright <- max(nchar(right))
  maxcentered <- maxleft+maxright+1

  if(any(is.int>0)){
    integers <- match.arg(integers)
    if(integers=="right"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxcentered)
    }
    if(integers=="left"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="left",
                          width=maxcentered)
    }
    if(integers=="dot"){
      maxleft <- max(maxleft,max(nchar(as.character(x[is.int]))))
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxleft)
      fintegers <- paste(fintegers,format(" ",width=maxright))
    }
    centered <- paste(left,right,sep=".")
    maxcentered <- max(nchar(centered))
    x[has.dot] <- centered
    x[is.int] <- fintegers
  } else {
    left <- format(left,justify="right",width=maxleft)
    right <- format(right,justify="left",width=maxright)
    centered <- paste(left,right,sep=".")
    x[has.dot] <- centered
  }
  if(any(skip))
    x[skip] <- format(x[skip],width=maxcentered,justify="centre")

  return(x)
}


format.mtable <- function(x,
          coef.title="Coefficients",
          summary.title="Summaries",
          colsep="\t",
          rowsep="\n",
          trim = TRUE,
          trimleft=trim,
          trimright= trim,
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
          ){

  if(forLaTeX){
    if(missing(trimleft)) trimleft <- FALSE
    if(missing(trimright)) trimright <- FALSE
    if(missing(center.at)) center.at <- getOption("OutDec")
  }
          
  coldims <- dim(x$coefficients)[x$as.col]
  nhrows <- length(coldims)

  coefnames <- dimnames(x$coefficients)[[x$coef.dim]]
  coefnames <- gsub(" x ",interaction.sep,coefnames,fixed=TRUE)
  dimnames(x$coefficients)[[x$coef.dim]] <- coefnames
  coefs <- ftable(as.table(x$coefficients),row.vars=rev(x$as.row),
    col.vars=rev(x$as.col)
    )
#   browser()  
  fcoefs <- format(coefs,quote=FALSE)
#  fcoefs <- trimws(fcoefs,left=trimleft,right=trimright)

  ckill.col <- c(x$kill.col,max(x$kill.col)+1)
  header <- fcoefs[seq(nhrows),-ckill.col,drop=FALSE]
  if(x$kill.header)
    header <- header[-x$kill.header,,drop=FALSE]
  fcoefs <- fcoefs[-seq(nhrows+1),-ckill.col,drop=FALSE]
#   cat(t(coefs),sep=c(rep(colsep,ncol(coefs)-1),rowsep))
  headrows <- 1:nrow(header)
  
  if(length(x$summaries)){
    sdim <- dim(x$summaries)
    summaries <- array("",c(sdim[1],dim(x$coefficients)[x$as.col]))
    targetix <- c(list(TRUE),rep(list(1),length(dim(summaries))-2),list(TRUE))
    ex <- as.call(c(as.symbol("[<-"),as.symbol("summaries"),targetix,list(x$summaries)))
    summaries <- eval(ex)
    dimnames(summaries) <- c(dimnames(x$summaries)[1],dimnames(x$coefficients)[x$as.col])
    summaries <- ftable(as.table(summaries),row.vars=1,col.vars=rev(2:length(dim(summaries))))
#      browser()
    fsummaries <- format(summaries,quote=FALSE)

    fsummaries <- fsummaries[-seq(nhrows+1),-x$kill.col,drop=FALSE]

    fcoefs <- rbind(
        t(as.matrix(c(coef.title,rep("",ncol(coefs))))),
        fcoefs
        )
    fsummaries <- rbind(
        t(as.matrix(c(summary.title,rep("",ncol(coefs))))),
        fsummaries
        )
    
#    fsummaries <- trimws(fsummaries,left=trimleft,right=trimright)
    res <- rbind(header,fcoefs,fsummaries)
    coefrows <- nrow(header) + seq(nrow(fcoefs))
    summrows <- max(coefrows) + seq(nrow(fsummaries))
  }
  else{
    res <- rbind(header,fcoefs)
    coefrows <- nrow(header) + seq(nrow(fcoefs))
    summrows <- 0
    }

  if(forLaTeX){
    #res[coefrows,-1] <- sub("(\\*+)","\^\{\\1\}",res[coefrows,-1])
    #res[coefrows,-1] <- sub("([eE])([-+]?[0-9]+)","\\\\textrm\{\\1\}\\2",res[coefrows,-1])
    res[coefrows,-1] <- sub("(\\*+)","^{\\1}",res[coefrows,-1])
    res[coefrows,-1] <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",res[coefrows,-1])
  }
  
  res <- format(res)
    
  if(length(center.at)){
    align.integers <- match.arg(align.integers)
    res[,-1] <- apply(res[,-1,drop=FALSE],2,centerAt,
                                      at=center.at,
                                      integers=align.integers,
                                      skip=1:nhrows)
  }
  else
    res <- trimws(res,left=trimleft,right=trimright)

  if(forLaTeX){
    ans <- "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    ans <- c(ans,"%")
    ans <- c(ans,"% Calls:")
    calls <- x$calls
    for(i in seq(calls)){
        tmp <- paste("% ",names(calls)[i],": ",sep="")
        tmp <- paste(tmp,paste(trimws(deparse(calls[[i]])),collapse=" "),"")
        ans <- c(ans,tmp)
      }
    ans <- c(ans,"%")
    ans <- c(ans,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    tabspec <- rep("l",ncol(res))
    tabspec[-1] <- colspec
    tabbegin <- paste("\\begin{tabular}{",paste(tabspec,collapse=""),"}",sep="")
    tabend <- "\\end{tabular}"
    ans <- c(ans,tabbegin)
    if(length(toprule)){
      ans <- c(ans,toprule)
    }
    pcoldims <- cumprod(coldims)
    for(i in headrows){
      if(i < headrows){
        mcols <- rev(cumprod)[i+1]
      }
        else mcols <- 1

      for(j in 2:ncol(res))
        ans <- c(ans,paste(" & \\multicolumn{",mcols,"}{c}{",trimws(res[i,j]),"}",sep=""))
      ans <- c(ans,"\\\\")
    }
    
    if(length(midrule)){
      ans <- c(ans,midrule)
    }
    for(i in coefrows){
      cr <- paste(paste(res[i,],collapse=" & "),"\\\\")
      ans <- c(ans,cr)
    }
    
    if(any(summrows)){
      if(length(midrule)){
      ans <- c(ans,midrule)
      }
      for(i in summrows){
        sr <- paste(paste(res[i,],collapse=" & "),"\\\\")
        ans <- c(ans,sr)
      }
    }

    
    if(length(bottomrule)){
      ans <- c(ans,bottomrule)
    }
    ans <- c(ans,tabend)
  }
  else {
      ans <- c()
      colwidths <- sapply(seq(ncol(res)),function(i)max(nchar(res[,i])))
      if(any(nchar(topsep))){
        top <- sapply(seq(ncol(res)),function(i)
            paste(rep(topsep,colwidths[i]),collapse=""))
        top <- paste(top,collapse=rep(topsep,nchar(colsep)))
        ans <- c(ans,top)
      }
      #ans <- paste(ans,t(res),sep=c(rep(colsep,ncol(res)-1),rowsep))
      for(i in headrows){
        hr <- paste(res[i,],collapse=colsep)
        ans <- c(ans,hr)
        }
        
      if(any(nchar(sectionsep))){
        secrule <- sapply(seq(ncol(res)),function(i)
            paste(rep(sectionsep,colwidths[i]),collapse=""))
        secrule <- paste(secrule,collapse=rep(sectionsep,nchar(colsep)))
        ans <- c(ans,secrule)
      }

      for(i in coefrows){
        cr <- paste(res[i,],collapse=colsep)
        ans <- c(ans,cr)
        }
      
      if(any(summrows)){
        if(any(nchar(sectionsep))){
          ans <- c(ans,secrule)
        }
        for(i in summrows){
          sr <- paste(res[i,],collapse=colsep)
          ans <- c(ans,sr)
          }
      }

      
      if(any(nchar(bottomsep))){
        bottom <- sapply(seq(ncol(res)),function(i)
            paste(rep(bottomsep,colwidths[i]),collapse=""))
        bottom <- paste(bottom,collapse=rep(bottomsep,nchar(colsep)))
        ans <- c(ans,bottom)
      }
      ans <- paste(paste(ans,collapse=rowsep),rowsep,sep="")
  }
  return(ans)
}

print.mtable <- function(x,trim=FALSE,center.at=getOption("OutDec"),
      colsep=" ",
      topsep="=",bottomsep="=",sectionsep="-",...){
  calls <- x$calls
  cat("\nCalls:\n")
  for(i in seq(calls)){
      cat(names(calls)[i],": ",sep="")
      print(calls[[i]])
    }
  cat("\n")
  cat(format.mtable(x,trimleft=trimleft,trimright=trimright,center.at=center.at,
      colsep=colsep,topsep=topsep,bottomsep=bottomsep,sectionsep=sectionsep,...))
}

toLatex.mtable <- function(object,...){
  structure(format.mtable(x=object,...,forLaTeX=TRUE),
  class="Latex")
}


write.mtable <- function(object,file="",...){
  cat(format.mtable(object,...),file=file)
}


relabel.mtable <- function(x,...){
 n.cdims <- length(dim(x$coefficients))
 n.sdims <- length(dim(x$summaries))
 for(i in 1:n.cdims){
  x$coefficients <- dimrename(x$coefficients,dim=i,...)
  }
 for(i in 1:n.sdims){
  x$summaries <- dimrename(x$summaries,dim=i,...)
  }
 return(x)
}