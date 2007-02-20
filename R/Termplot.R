getStripText <- function(which.given,which.panel,stripTexts){
  aa <- list(stripTexts)
  aa <- c(aa,as.list(c(which.given,which.panel)))
  do.call("[",aa)
}


strip.Termplot <-
function (which.given, which.panel, var.name, factor.levels,
    shingle.intervals = NULL, strip.names = c(FALSE, TRUE), strip.levels = c(TRUE,
        FALSE), sep = " : ", style = 1, horizontal = TRUE, bg = trellis.par.get("strip.background")$col[which.given],
    fg = trellis.par.get("strip.shingle")$col[which.given], par.strip.text = trellis.par.get("add.text"),stripTexts=NULL)
{
    require(grid)
    if (horizontal)
        grid::pushViewport(viewport(y = (which.given - 0.5)/length(which.panel),
            height = 1/length(which.panel), name = paste("strip.default",
                which.given, sep = ".")))
    else grid::pushViewport(viewport(x = 1 - (which.given - 0.5)/length(which.panel),
        width = 1/length(which.panel), name = paste("strip.default",
            which.given, sep = ".")))
    gp.text <- gpar(col = par.strip.text$col, alpha = par.strip.text$alpha,
        lineheight = par.strip.text$lineheight, fontfamily = par.strip.text$fontfamily,
        fontface = lattice:::chooseFace(par.strip.text$fontface, par.strip.text$font),
        cex = par.strip.text$cex)
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length = 2)
    strip.levels <- rep(strip.levels, length = 2)
    formatLabel <- function(s, abbreviate = par.strip.text$abbr,
        minlength = par.strip.text$minl, dot = par.strip.text$dot) {
        if (is.null(abbreviate))
            abbreviate <- FALSE
        if (is.null(minlength))
            minlength <- 4
        if (is.null(dot))
            dot <- FALSE
        if (abbreviate)
            abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)
    if (!is.null(shingle.intervals)) {
        grid.rect(gp = gpar(fill = bg, col = bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level, ]) - t[1])/diff(t)
        if (horizontal)
            grid.rect(x = unit(r %*% c(0.5, 0.5), "npc"), width = max(unit(c(diff(r),
                1), c("npc", "mm"))), gp = gpar(col = fg, fill = fg))
        else grid.rect(y = unit(r %*% c(0.5, 0.5), "npc"), height = max(unit(c(diff(r),
            1), c("npc", "mm"))), gp = gpar(col = fg, fill = fg))
        lattice:::paste.and.draw(name, getStripText(which.given,which.panel,stripTexts), sep = sep,
            horizontal = horizontal, showl = strip.names[2],
            showr = strip.levels[2], gp = gp.text)
    }
    else {
        num <- length(factor.levels)
        if (style != 2)
            grid.rect(gp = gpar(fill = bg, col = bg))
        if (num > 0 && style %in% c(2, 3, 4)) {
            if (horizontal) {
                grid.rect(x = unit((2 * level - 1)/(2 * num),
                  "npc"), width = unit(1/num, "npc"), gp = gpar(fill = fg,
                  col = fg))
            }
            else {
                grid.rect(y = unit((2 * level - 1)/(2 * num),
                  "npc"), height = unit(1/num, "npc"), gp = gpar(fill = fg,
                  col = fg))
            }
        }
        if (style %in% c(1, 3)) {
            lattice:::paste.and.draw(name, getStripText(which.given,which.panel,stripTexts), sep = sep,
                horizontal = horizontal, showl = strip.names[1],
                showr = strip.levels[1], gp = gp.text)
        }
        else if (num > 0) {
            lid <- if (style %in% c(2, 4))
                1:num
            else level
            if (horizontal) {
                grid.text(label = factor.levels[lid], x = (2 *
                  lid - 1)/(2 * num), gp = gp.text)
            }
            else {
                grid.text(label = factor.levels[lid], y = (2 *
                  lid - 1)/(2 * num), rot = 90, gp = gp.text)
            }
        }
    }
    strip.border <- trellis.par.get("strip.border")
    grid.rect(gp = gpar(col = rep(strip.border$col, length = which.given)[which.given],
        lty = rep(strip.border$lty, length = which.given)[which.given],
        lwd = rep(strip.border$lwd, length = which.given)[which.given],
        alpha = rep(strip.border$alpha, length = which.given)[which.given],
        fill = "transparent"))
    grid::upViewport()
}




Termplot <- function(object,...) UseMethod("Termplot")

Termplot.default <- function(object,
        ...,
        variables=NULL,
        col.term = 2,
        lty.term = 1,
        lwd.term = 1.5,
        col.se = "orange",
        lty.se = 2,
        lwd.se = 1,
        col.res = "gray",
        cex = 1,
        pch = par("pch"),
        col.smth = "darkred",
        lty.smth = 2,
        lwd.smth = 1,
        span.smth = 2/3,
        aspect="fill",
        xlab=NULL,
        ylab=NULL,
        main=paste(deparse(object$call),collapse="\n"),
        models=c("rows","columns"),
        layout=NULL
  ){
  if(!require(lattice)) stop("Termplot needs the lattice package")
  if(!missing(object) && !length(object$terms) && length(object[[1]]$terms)){
    m0 <- m <- match.call()
    m[[1]] <- Termplot.lmList
    return(eval(m,parent.frame()))
  }
    
  
  other.args  <- list(...)
  if(length(other.args)){
    other.args <- other.args[sapply(other.args,function(o)length(o$terms)>0)]
    m0 <- m <- match.call()
    if(missing(object)) 
        model.args <- 1:length(other.args) + 1
    else
        model.args <- 1:(length(other.args)+1) + 1
    m[[1]] <- Termplot.lmList
    m[model.args] <- NULL
    m$object <- as.call(c(as.name("list"),as.list(m0[model.args])))
    return(eval(m,parent.frame()))
    }

  terms <- terms(object)
  if(!missing(variables))
    variables <- intersect(all.vars(delete.response(terms)),variables)
  else
    variables <- all.vars(delete.response(terms))
  varFactor <- factor(variables,levels=variables)
  #browser()
  
  Predictions <- prediction.frame(object,
                    se.fit=TRUE,
                    type="variables",
                    residuals="partial")

  xannot <- sapply(variables,function(v){
            x <- Predictions[[v]]
            if(is.factor(x)){
              list(
                at = seq(along=levels(x)),
                labels = levels(x),
                limits = c(.7,nlevels(x)+.3)
                )
            }
            else{
              pretty.x <- pretty(x)
              if(length(pretty.x)>length(unique(x)))
                pretty.x <- unique(x)
              limits <- range(pretty.x)
              step <- diff(pretty.x[1:2])
              limits[1] <- limits[1]-.3*step
              limits[2] <- limits[2]+.3*step
              list(
                at = pretty.x,
                labels = as.character(pretty.x),
                limits = limits
              )
            }
          })

  x.at <- xannot["at",][levels(varFactor)]
  x.labels <- xannot["labels",][levels(varFactor)]
  x.limits <- xannot["limits",][levels(varFactor)]
  Predictions <-lapply(variables,function(v){
                  select <- paste(v,c("fit","se.fit","resid"),sep=".")
                  P <- Predictions[select]
                  names(P) <- c("fit","se.fit","resid")
                  x <- as.numeric(Predictions[[v]])
                  P <- P[order(x),]
                  x <- x[order(x)]
                  smooth <- lowess(x,P$resid,f=span.smth)$y
                  transform(P,
                            x=x,
                            smooth=smooth,
                            upper=fit+2*se.fit,
                            lower=fit-2*se.fit,
                            var =rep(varFactor[varFactor==v],nrow(P))
                            )
                  })
  Predictions <- do.call("rbind",Predictions)
  #browser()
  xyplot(resid+smooth+upper+lower+fit~x|var,data=Predictions,
          type=c("l","p"),
          lwd=c(0,lwd.smth,lwd.se,lwd.se,lwd.term),
          lty=c(0,lty.smth,lty.se,lty.se,lty.term),
          col=c(col.res,col.smth,col.se,col.se,col.term),
          pch=rep(pch,5),
          cex=c(cex,0,0,0,0),
          scales=list(x=list(relation="free",
              at=x.at,
              labels=x.labels,
              limits=x.limits
            )),
          aspect=aspect,
          xlab=xlab,ylab=ylab,
          main=main)
          
}
# debug(Termplot.default)

Termplot.lmList <- function(object ,...,
        variables=NULL,
        col.term = 2,
        lty.term = 1,
        lwd.term = 1.5,
        col.se = "orange",
        lty.se = 2,
        lwd.se = 1,
        col.res = "gray",
        cex = 1,
        pch = par("pch"),
        col.smth = "darkred",
        lty.smth = 2,
        lwd.smth = 1,
        span.smth = 2/3,
        aspect="fill",
        xlab=NULL,
        ylab=NULL,
        main=NULL,
        models=c("rows","columns"),
        layout=NULL){
    m <- match.call()
    are.lm <- sapply(object,function(obj)inherits(obj,"lm"))
    if(!all(are.lm)) stop("can only handle objects that inherit from lm")
    #str(object)
    if(!length(names(object)) || names(object)[1]=="object")
        names(object) <- as.character(m$object[-1])

    variables.arg.present <- !missing(variables)
    terms <- lapply(object,function(o)terms(o))
    variables <- lapply(terms,function(trm){
            allvar <- all.vars(delete.response(trm))
            if(variables.arg.present)
                return(allvar[allvar %in% as.character(variables)])
            else return(allvar)  
        })
    variables <- unique(unlist(variables))
    varFactor <- factor(variables,levels=variables)
    Predictions <- lapply(object,prediction.frame,
                    se.fit=TRUE,
                    type="variables",
                    residuals="partial")
    xannot <- lapply(Predictions,function(Predictions){
                xannot <- sapply(variables,function(v){
                    x <- Predictions[[v]]
                    #print(x)
                    if(!length(x)) list(at=NULL,labels=NULL,limits=NULL)
                    else if(is.factor(x)){
                      list(
                        at = seq(along=levels(x)),
                        labels = levels(x),
                        limits = c(.7,nlevels(x)+.3)
                        )
                    }
                    else{
                      pretty.x <- pretty(x)
                      if(length(pretty.x)>length(unique(x)))
                        pretty.x <- unique(x)
                      limits <- range(pretty.x)
                      step <- diff(pretty.x[1:2])
                      limits[1] <- limits[1]-.3*step
                      limits[2] <- limits[2]+.3*step
                      list(
                        at = pretty.x,
                        labels = as.character(pretty.x),
                        limits = limits
                      )
                    }
                })
                list(
                    at = xannot["at",levels(varFactor)],
                    labels = xannot["labels",levels(varFactor)],
                    limits = xannot["limits",levels(varFactor)]
                )
              })
    x.at <- sapply(xannot,"[[","at")
    x.labels <- sapply(xannot,"[[","labels")
    x.limits <- sapply(xannot,"[[","limits") 
    if(is.array(x.at)){
      row.not.empty <- apply(x.at,1,function(x)!all(sapply(x,is.null)))
      col.not.empty <- apply(x.at,2,function(x)!all(sapply(x,is.null)))
      x.at <- x.at[row.not.empty,,drop=FALSE]
      x.at <- x.at[,col.not.empty,drop=FALSE]
      x.labels <- x.labels[row.not.empty,,drop=FALSE]
      x.labels <- x.labels[,col.not.empty,drop=FALSE]
      #x.limits <- x.limits[row.not.empty,,drop=FALSE]
      #x.limits <- x.limits[,col.not.empty,drop=FALSE]
    }
    #browser()
    Predictions <- lapply(Predictions,function(Predictions){
                Predictions <- lapply(variables,function(v){
                  select <- paste(v,c("fit","se.fit","resid"),sep=".")
                  if(!length(intersect(select,names(Predictions)))){
                        #cat("##\n")
                        return(data.frame(
                            fit=numeric(0),
                            se.fit=numeric(0),
                            resid=numeric(0),
                            x=numeric(0),
                            smooth=numeric(0),
                            upper=numeric(0),
                            term=numeric(0)
                        ))
                    }
                  P <- Predictions[select]
                  names(P) <- c("fit","se.fit","resid")
                  x <- as.numeric(Predictions[[v]])
                  #browser()
                  P <- P[order(x),]
                  x <- x[order(x)]
                  smooth <- lowess(x,P$resid,f=span.smth)$y
                  transform(P,
                            x=x,
                            smooth=smooth,
                            upper=fit+2*se.fit,
                            lower=fit-2*se.fit,
                            var =rep(varFactor[varFactor==v],nrow(P))
                            )
                  })
                  do.call("rbind",Predictions[sapply(Predictions,NROW)>0])
            })
    modelFactor <- factor(paste("Model:",names(object)),levels=paste("Model:",names(object)))
    i <- sapply(Predictions,NROW)
    #modelFactor <- rep(modelFactor,i)
    #x.limits <- x.limits[sapply(Predictions,NROW)>0]
    #browser()
    Predictions <- do.call("rbind",Predictions[sapply(Predictions,NROW)>0])
    Predictions <- transform(Predictions,
                    model=rep(modelFactor,i))
    #levels(Predictions$term) <- paste("Term:",levels(Predictions$var))
    x.limits <- x.limits[table(Predictions$var,Predictions$model)>0]
    
  models <- match.arg(models)
  if(models=="rows") perm.cond <- c(1,2)
  if(models=="columns") perm.cond <- c(2,1)

  if(missing(layout)) {
    if(allequal(Predictions$var)) layout <- c(0,length(unique(Predictions$model)))
    if(allequal(Predictions$model)) layout <- c(0,length(unique(Predictions$var)))
    }

  stripTexts <- array("<NA>",
                        dim=c(2,nlevels(varFactor),nlevels(modelFactor)),
                        dimnames=list(c("",""),
                              levels(varFactor),
                              levels(modelFactor)
                            )
                        )

  getTermsWithVars <- function(trm,v){
    tl <- attr(trm,"term.labels")
    fos <- lapply(tl,function(t)asOneSidedFormula(parse(text=t)))
    if(!length(fos)) return("")
    i <- which(sapply(fos,function(fo) v %in% all.vars(fo)))
    paste(tl[i],collapse="+")
  }

  for(j in seq(along=levels(modelFactor))){
    stripTexts[2,,j] <- levels(modelFactor)[j]
    trm <- terms[[j]]
    for(i in seq(along=variables)){
      v <- variables[i]
      vl <- getTermsWithVars(trm,v)
      stripTexts[1,i,j] <- vl
    }
  }
  
  data.has.it <- lapply(Predictions[c("var","model")],function(x)as.numeric(unique(x)))
  data.has.it <- expand.grid(c(list(seq(dim(stripTexts)[1])),data.has.it))
  new.dims <- sapply(data.has.it,function(x)length(unique(x)))
  stripTexts <- stripTexts[as.matrix(data.has.it)]
  dim(stripTexts) <- new.dims
  
  xyplot(resid+smooth+upper+lower+fit~x|var*model,data=Predictions,
          #drop.unused.levels=FALSE,
          as.table=TRUE,
          layout=layout,
          perm.cond=perm.cond,
          type=c("l","p"),
          lwd=c(0,lwd.smth,lwd.se,lwd.se,lwd.term),
          lty=c(0,lty.smth,lty.se,lty.se,lty.term),
          col=c(col.res,col.smth,col.se,col.se,col.term),
          pch=rep(pch,5),
          cex=c(cex,0,0,0,0),
          scales=list(x=list(relation="free",
              at=x.at,
              labels=x.labels,
              limits=x.limits
            )),
          aspect=aspect,
          strip=function(...) strip.Termplot(...,stripTexts=stripTexts),
          xlab=xlab,ylab=ylab,
          main=main)
}
