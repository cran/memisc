mergeSQL <- function(x,y,by=NULL,by.x=by,by.y=by,all=FALSE,all.x=all,all.y=all){
  xSQL <- paste(x,"AS x")
  ySQL <- paste(y,"AS y")
  joinop <- if(all.x && all.y) "FULL OUTER JOIN"
  else if(all.x) "LEFT OUTER JOIN"
  else if(all.y) "RIGHT OUTER JOIN"
  else "INNER JOIN"
  if(length(by.x) && length(by.y)){
    xSQL.by <- paste("x",by.x,sep=".")
    ySQL.by <- paste("y",by.y,sep=".")
    bySQL <- paste(xSQL.by,ySQL.by,sep=" = ")
    join.on <- paste(bySQL,collapse=" AND ")
    join.on <- paste("ON (",join.on,")")
  }
  else {
    joinop <- paste("NATURAL",joinop)
    join.on <- ""
  }
  structure(
    paste(xSQL,joinop,ySQL,join.on),
    x = x,
    y = y,
    by.x = by.x,
    by.y = by.y,
    all.x = all.x,
    all.y = all.y
    )
}

correct.ambiguous <- function(fields,con,table){
  m <- attributes(table)[c("x","y","by.x","by.y","all.x","all.y")]
  if(!length(m)) 
    return(fields)
  
  xfields <- dbListFields(con,m$x)
  yfields <- dbListFields(con,m$y)
  both <- intersect(xfields,yfields)
  sapply(fields,function(field){
    if(field %in% both){
        if(m$all.y && !m$all.x) field <- paste("y",field,sep=".")
        else field <- paste("x",field,sep=".")
      }
    field
    })
}

get.basenames <- function(tfield,table){
  m <- attributes(table)[c("x","y","by.x","by.y","all.x","all.y")]
  if(!length(m)) 
    return(paste(tfield,"=",dQuote(table)))
  else
    return(paste("(",tfield,"=",dQuote(m$x)," OR ",tfield,"=",dQuote(m$y),")"))
}

subsetToWHERE <- function(x){
        WHERE <- gsub("&","AND",x,fixed=TRUE)
        WHERE <- gsub("[[:space:]]+"," ",WHERE,extended=TRUE)
        WHERE <- gsub(" %in% c("," in (",WHERE,fixed=TRUE)

        seqs <- gregexpr("[[:digit:]]+:[[:digit:]]+",WHERE,extended=TRUE)[[1]]
        if(seqs[1]>0){
          seqs.mlen <- attr(seqs,"match.length")
          for(i in seq(along=seqs)){
            seqs.i <- seqs[i]
            seqs.mlen.i <- seqs.mlen[i]
            mtchd <- substr(WHERE,start=seqs.i,stop=seqs.i+seqs.mlen.i-1)
            mtchd2 <- strsplit(mtchd,":")[[1]]
            replnt <- as.character(seq(
                from=as.numeric(mtchd2[1]),
                to=as.numeric(mtchd2[2])
                ))
            replnt <- paste(replnt,collapse=", ")
            WHERE <- sub(mtchd,replnt,WHERE,fixed=TRUE)
          }
        }
        return(WHERE)
}

sql.can.do <- function(fcall){
  if(deparse(fcall[[1]])=="c"){
    funs <- sapply(fcall[-1],function(fc)deparse(fc[[1]]))
    args <- sapply(fcall[-1],function(fc)as.character(fc[-1]))
    can.do <- funs %in% c("mean","sum","max","min","var")
    if(!all(can.do)) return(FALSE)
    sql <- sapply(seq(along=funs),function(i){
      switch(funs[i],
        mean=paste("avg(",args[i],")",sep=""),
        sum=paste("sum(",args[i],")",sep=""),
        max=paste("max(",args[i],")",sep=""),
        min=paste("min(",args[i],")",sep=""),
        var=paste("mean((",args[i],")*(",args[i],"))-mean(",args[i],")*mean(",args[i],")",sep=""),
        )
      })
    sql <- paste(sql,"as",letters[seq(along=sql)])
    sql <- paste(sql,collapse=", ")
    return(structure(TRUE,sql=sql))
  }
  else return(FALSE)
}

tableSQLtarget <- function(field,con,table,WHERE){
  statement <- paste("SELECT",field,"FROM",table,WHERE,"GROUP BY",field,"ORDER BY",field,";")
  uniqvals <- dbGetQuery(con,statement)[[field]]
  selector <- paste("SUM(",field," = ",uniqvals,") AS val_",uniqvals,sep="")
  paste(selector,collapse=", ")
}

wtableSQLtarget <- function(field,weight,con,table,WHERE){
  statement <- paste("SELECT",field,"FROM",table,WHERE,"GROUP BY",field,"ORDER BY",field,";")
  uniqvals <- dbGetQuery(con,statement)[[field]]
  selector <- paste("SUM(",weight,"*(",field," = ",uniqvals,")) AS val_",uniqvals,sep="")
  paste(selector,collapse=", ")
}

aggregateDB <- function (formula,
                        table,
                        connection,
                        catalog="catalog",
                        subset=NULL,
                        names=NULL,
                        showSQL=TRUE,
                        allow.sql.eval=FALSE)
{
    if(!require(RSQLite)) stop("this function requires RSQLite")
    mycall <- match.call()
    fcall <- formula[[2]]
    sql.can.do.fcall <- sql.can.do(fcall)
    table.bySQL <- FALSE
    #browser()
    if(length(fcall) == 1 || !attr(terms(formula),"response")) {
      if(attr(terms(formula),"response")){
        target <- paste("sum(",all.vars(fcall),")",sep="")
        grouping.vars <- all.vars(formula[-2])
        .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),collapse=", ")
        }
      else{
        grouping.vars <- all.vars(formula)
        .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),
                            collapse=", ")
        target <- "count(*) as Freq"
        formula <- update(formula,Freq~.)
        }
      target <- paste(.grouping.vars,target,sep=", ")
      WHERE <- ""
      if(!missing(subset)){
        WHERE <- paste(deparse(mycall$subset),collapse="")
        WHERE <- subsetToWHERE(WHERE)
        WHERE <- paste("WHERE",WHERE) 
      }
      statement <- paste(
                              "SELECT",target,
                              "FROM",as.character(table),
                              WHERE,
                              "GROUP BY",.grouping.vars,
                              "ORDER BY",.grouping.vars,
                              ";"
                            )
      if(showSQL) cat(statement,"\n")
      res <- dbGetQuery(connection,statement)
      names(res)[seq(along=grouping.vars)] <- grouping.vars
      if(all(res$Freq==1)) res$Freq <- NULL
    }
    else if(as.character(fcall[[1]])=="table" && length(fcall[[-1]])==1 && allow.sql.eval){
      WHERE <- ""
      if(!missing(subset)){
        WHERE <- paste(deparse(mycall$subset),collapse="")
        WHERE <- subsetToWHERE(WHERE)
        WHERE <- paste("WHERE",WHERE) 
      }
      target.var <- all.vars(fcall)
      field <- paste(correct.ambiguous(target.var,connection,table))
      target <- tableSQLtarget(field,connection,table,WHERE)
      grouping.vars <- all.vars(formula[-2])
      .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),collapse=", ")
      target <- paste(.grouping.vars,target,sep=", ")
      statement <- paste(
                              "SELECT",target,
                              "FROM",as.character(table),
                              WHERE,
                              "GROUP BY",.grouping.vars,
                              "ORDER BY",.grouping.vars,
                              ";"
                            )
      if(showSQL) cat(statement,"\n")
      res <- dbGetQuery(connection,statement)
      names(res)[seq(along=grouping.vars)] <- grouping.vars
      table.bySQL <- TRUE
    }
    else if(as.character(fcall[[1]])=="wtable" && length(fcall[[-1]]) %in% 1:2 && allow.sql.eval){
      WHERE <- ""
      if(!missing(subset)){
        WHERE <- paste(deparse(mycall$subset),collapse="")
        WHERE <- subsetToWHERE(WHERE)
        WHERE <- paste("WHERE",WHERE) 
      }
      target.var <- all.vars(fcall[1:2])
      if(length(fcall)==3){
        weight <- all.vars(fcall[c(1,3)])
      } else weight <- "1"
      field <- paste(correct.ambiguous(target.var,connection,table))
      target <- wtableSQLtarget(field,connection,table,WHERE)
      grouping.vars <- all.vars(formula[-2])
      .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),collapse=", ")
      target <- paste(.grouping.vars,target,sep=", ")
      statement <- paste(
                              "SELECT",target,
                              "FROM",as.character(table),
                              WHERE,
                              "GROUP BY",.grouping.vars,
                              "ORDER BY",.grouping.vars,
                              ";"
                            )
      if(showSQL) cat(statement,"\n")
      res <- dbGetQuery(connection,statement)
      names(res)[seq(along=grouping.vars)] <- grouping.vars
      table.bySQL <- TRUE
    }
    else if(sql.can.do.fcall && allow.sql.eval){
      target <- attr(sql.can.do.fcall,"sql")
      grouping.vars <- all.vars(formula[-2])
      .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),collapse=", ")
      target <- paste(.grouping.vars,target,sep=", ")
      WHERE <- ""
      if(!missing(subset)){
        WHERE <- paste(deparse(mycall$subset),collapse="")
        WHERE <- subsetToWHERE(WHERE)
        WHERE <- paste("WHERE",WHERE) 
      }
      statement <- paste(
                              "SELECT",target,
                              "FROM",as.character(table),
                              WHERE,
                              "GROUP BY",.grouping.vars,
                              "ORDER BY",.grouping.vars,
                              ";"
                            )
      if(showSQL) cat(statement,"\n")
      res <- dbGetQuery(connection,statement)
      names(res)[seq(along=grouping.vars)] <- grouping.vars
      #browser()
      resp.names <- sapply(fcall,deparse)[-1]
      if(length(names(resp.names))) resp.names <- names(resp.names)
      names(res)[!(names(res) %in% grouping.vars)] <- resp.names
    }
    else{
      grouping.vars <- all.vars(formula[-2])
      .grouping.vars <- paste(correct.ambiguous(grouping.vars,connection,table),collapse=", ")
      if(!missing(subset)){
        WHERE <- paste(deparse(mycall$subset),collapse="")
        WHERE <- subsetToWHERE(WHERE)
        WHERE <- paste("WHERE",WHERE)
      }
      else WHERE <- "WHERE"
      grouping.statement <- paste(
                              "SELECT",.grouping.vars,
                              "FROM",as.character(table),
                              WHERE,
                              "GROUP BY",.grouping.vars,
                              "ORDER BY",.grouping.vars,
                              ";"
                            )
      if(showSQL) cat(grouping.statement,"\n")
      by <- dbGetQuery(connection,grouping.statement)
      #print(grouping.data)
      target.vars <- all.vars(fcall)
      .target.vars <- paste(correct.ambiguous(target.vars,connection,table),collapse=", ")
      missing.subset <- missing(subset)
      workhorse <- function(current){}
      unnfcall <- as.call(unname(as.list(fcall)))
      unnfcall <- deparse(unnfcall)
      newargs <- paste("current$",all.vars(fcall),sep="")
      oldargs <- all.vars(fcall)
      for(i in seq(oldargs)) unnfcall <- gsub(oldargs[i],newargs[i],unnfcall,fixed=TRUE)
      newcall <- parse(text=unnfcall)[[1]]
      body(workhorse) <- newcall
      res <- sapply(1:nrow(by),function(i){
        selector <- by[i,]
        selector <- as.character(selector)
        where.clause <- paste(
                              paste(
                                correct.ambiguous(grouping.vars,connection,table),
                                  "=",selector),
                                collapse=" AND ")
        if(missing.subset) where.clause <- paste(WHERE,where.clause)
        else where.clause <- paste(WHERE,"AND",where.clause)
        target.statement <- paste(
                              "SELECT",.target.vars,
                              "FROM",as.character(table),
                              where.clause,
                              ";"
                            )
        if(showSQL) cat(target.statement,"\n")
        current <- dbGetQuery(connection,target.statement)
        #str(current)
        #eval(fcall,current)
        workhorse(current)
      })
      res <- as.data.frame(t(res))
      names(by) <- grouping.vars
      res <- cbind(as.data.frame(by),res)
    }
#     res <- NULL
#    resp.names <- NULL
    tfield <- "basename"

    labels.group.vars <- lapply(grouping.vars,function(v){
      statement <- paste("SELECT value, value_label FROM",catalog,
                                    "WHERE",get.basenames(tfield,table),"AND variable=",dQuote(v),
                                    ";")
      if(showSQL) cat(statement,"\n")
      transform(dbGetQuery(connection,statement),value=as.numeric(value))
    })
    names(labels.group.vars) <- grouping.vars
    for(i in seq(grouping.vars))
      if(NROW(labels.group.vars[[grouping.vars[i]]])) {
        vls <- as.character(labels.group.vars[[grouping.vars[i]]]$value)
        ff <- factor(res[[grouping.vars[i]]])
        levs <- levels(ff)
        ii <- match(levs,vls)
        match.ok <- !is.na(ii)
        vlab <- labels.group.vars[[grouping.vars[i]]]$value_label
        if(length(vlab[ii[match.ok]])){
          levs[match.ok] <- vlab[ii[match.ok]]
          levels(ff) <- trimws(levs)
          res[[grouping.vars[i]]] <- ff
        }
      }
    is.by.name <- names(res) %in% names(by)
    if(length(fcall)>1 && as.character(fcall[[1]]) %in% c("table","wtable","percent") && !table.bySQL){
      tv <- target.vars[1]
      statement <- paste("SELECT value, value_label FROM",catalog,
                                    "WHERE",get.basenames(tfield,table),"AND variable=",dQuote(tv),
                                    ";")
      if(showSQL) cat(statement,"\n")
      labels.target <- transform(dbGetQuery(connection,statement),value=as.numeric(value))
      vls <- as.character(labels.target$value)
      no.by.names <- names(res)[!is.by.name]
      ii <- match(no.by.names,vls)
      match.ok <- !is.na(ii)
      vlab <- labels.target$value_label
      no.by.names[match.ok] <- vlab[ii[match.ok]]
      names(res)[!is.by.name] <- trimws(no.by.names)
      }
    if(table.bySQL){
      statement <- paste("SELECT value, value_label FROM",catalog,
                                    "WHERE",get.basenames(tfield,table),"AND variable=",
                                    dQuote(target.var),
                                    ";")
      if(showSQL) cat(statement,"\n")
      labels.target <- transform(dbGetQuery(connection,statement),value=as.numeric(value))
      vls <- as.character(labels.target$value)
      no.by.names <- names(res)[!is.by.name]
      ii <- match(no.by.names,paste("val",vls,sep="_"))
      match.ok <- !is.na(ii)
      vlab <- labels.target$value_label
      no.by.names[match.ok] <- vlab[ii[match.ok]]
      names(res)[!is.by.name] <- trimws(no.by.names)
    }
    if(!missing(names)){
        subst <- names
        for(i in 1:length(subst)){
            names(res)[names(res)==names(subst[i])] <- subst[i]
        }
    }
    return(res)
}

