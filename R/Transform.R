# transfrom.data.frame <- function ("_data", ...)
# {
#     e <- eval(substitute(list(...)), .data., parent.frame())
#     tags <- names(e)
#     inx <- match(tags, names(.data.))
#     matched <- !is.na(inx)
#     if (any(matched)) {
#         .data.[inx[matched]] <- e[matched]
#         .data. <- data.frame(.data.)
#     }
#     if (!all(matched))
#         data.frame(.data., e[!matched])
#     else .data.
# }
#

Transform <- function(.data.,.expr.,...)
  UseMethod("Transform")

Transform.data.frame <- function(.data.,.expr.,...){
    pf <- parent.frame()
    exprEnv <- new.env(parent=pf)
    if(!missing(.expr.)){
      ee <- eval(substitute(.expr.),.data.,exprEnv)
      for(x in names(ee)) assign(x,ee[[x]],envir=exprEnv)
    }
    e <- eval(substitute(list(...)), .data., exprEnv)
    tags <- names(e)
    inx <- match(tags, names(.data.))
    matched <- !is.na(inx)
    save.attribs <- attributes(.data.)
    if (any(matched)) {
        .data.[inx[matched]] <- e[matched]
        .data. <- data.frame(.data.)
    }
    if (!all(matched))
        .data. <- data.frame(.data., e[!matched])
    save.attribs$names <-names(.data.)
    attributes(.data.) <- save.attribs
    .data.
}

# library(memisc)
# berkeley <- aggregate(wtable(Admit,Freq)~.,data=UCBAdmissions)
# 
# Transform(berkeley,
#   { tot <- sum(Admitted) + sum(Rejected)
#     list(tot=tot)
#   },
#   Admitted.p = Admitted/tot,
#   Rejected.p = Rejected/tot
# )
# 
# berk1 <- Transform(berkeley,
#     list(tot = sum(Admitted) + sum(Rejected)),
#   Admitted.p = Admitted/tot,
#   Rejected.p = Rejected/tot
# )
# 
# attributes(berk1)
# 
# Transform(berkeley,
#   ratio=Admitted/Rejected,
#   pp = Admitted/sum(Admitted)
#   )
  