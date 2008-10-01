
.memiscEnv <- new.env()
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
.SummaryTemplates$lm <- 
  c(
          "R-squared"     = "($r.squared:f#)",
          "adj. R-squared" = "($adj.r.squared:f#)",
          sigma         = "($sigma:#)",
          F             = "($F:f#)",
          p             = "($p:f#)",
          "Log-likelihood"  = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$glm <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          phi         = "($phi:#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$default <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )


assign("SummaryTemplates",.SummaryTemplates, env=.memiscEnv)
assign("CoefTemplates",.CoefTemplates, env=.memiscEnv)

sampleGeneric <- function(x, size, replace = FALSE, prob=NULL,...)
  UseMethod("sample")
environment(sampleGeneric) <- baseenv()
sample.default <- base::sample
formals(sample.default) <- formals(sampleGeneric)
environment(sample.default) <- baseenv()

car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

car_pkg <-"car"
memisc_env <- environment()


.onLoad <- function(lib,pkg){
  options(codebook.chunk.size=1000)
  options(Simulation.chunk.size=1000)
  options(print.use.value.labels=TRUE)
  options(show.max.obs=25)
  #require(methods )
  #require(Matrix)
  require(utils)
  require(stats)
#   assignInNamespace(".orig.in", base::`%in%`, ns = "base")
#   assignInNamespace("%in%", `%in%`, ns = "base")
#   assignInNamespace(".orig.as.factor", base::as.factor, ns = "base")
#   assignInNamespace("as.factor", as.factor, ns = "base")
#   assignInNamespace(".orig.codes", base::codes, ns = "base")
#   assignInNamespace("codes", codes, ns = "base")
  
  if(any(car_pkg == .packages(TRUE))){
    do.call("require",list(package=car_pkg))
    car_recode <- getFromNamespace("recode",ns=car_pkg)
    assign("car_recode",car_recode,env=memisc_env)
  }
  
  
  options(coef.style="default")
  options(baselevel.sep="/")
  options(factor.style="($f): ($l)")
  options(float.style="f")
  options(signif.symbols=c(
        "***"=.001,
        "**"=.01,
        "*"=.05
    ))
  options(labelled.factor.coerce.NA = FALSE)
#   options(ls.str.pos=-1)
#   formals(ls.str)$pos <- getOption("ls.str.pos")
  assignInNamespace(".sample.orig",base::sample , ns = "base")
  assignInNamespace("sample.default",sample.default , ns = "base")
  assignInNamespace("sample",sampleGeneric , ns = "base")
}


# addShowMethods <- function(pkg){
#   showEnv <- environment(show)
#   memisc4classes_show <- c(
#                         "annotation",
#                         "codebook",
#                         "data.set",
#                         "item.vector",
#                         "spss.fixed.importer",
#                         "spss.portable.importer",
#                         "spss.system.importer",
#                         "Stata.importer",
#                         "value.filter",
#                         "value.labels"
#                         )
#   showAllMethods <- showEnv$.AllMTable
#   for(cl in memisc4classes_show){
#     assign(cl,getMethod("show",cl,where=paste("package",pkg,sep=":")),envir=showAllMethods)
#   }
# }
# 
# 
# 
# showEnv <- environment(show)
# showAllMethods <- showEnv$.AllMTable
# myShowMethods <- sapply(c(
#                         "annotation",
#                         "codebook",
#                         "data.set",
#                         "item.vector",
#                         "spss.fixed.importer",
#                         "spss.portable.importer",
#                         "spss.system.importer",
#                         "Stata.importer",
#                         "value.filter",
#                         "value.labels"
#                         ),
#                       function(cl){
#                         getMethod("show",cl)
#                 })
# 
# 
# 
# .onAttach <- function(lib,pkg){
#    rvers <- getRversion()[[1]]
#    if(!all(rvers >= c(2,8,0))){
#     ## Override method selection by 'show' if 'show'
#     ## on one of the memisc objects
#     ## was called before loading the package
#     showEnv <- environment(show)
#     allShowMethods <- showEnv$.AllMTable
#     myShowMethods <- getFromNamespace("myShowMethods",pkg)
#     suppressMessages(trace("show",
#       tracer=function(...){
#       message("updating 'show' messages")
#       for(cl in names(myShowMethods))
#         assign(cl,myShowMethods[[cl]],envir=allShowMethods)
#       suppressMessages(untrace("show",signature="ANY"))
#       },
#       signature="ANY",print=FALSE,
#       exit=function(...){
#       message("updating 'show' messages")
#       for(cl in names(myShowMethods))
#         assign(cl,myShowMethods[[cl]],envir=allShowMethods)
#       suppressWarnings(suppressMessages(untrace("show",signature="ANY")))
#     }))
#    }
# }

.onUnload <- function(libpath)
{
#     assignInNamespace("as.factor",  base::.orig.as.factor,  ns = "base")
#     assignInNamespace("codes",  base::.orig.codes,  ns = "base")
    assignInNamespace("%in%", base::`.orig.in`, ns = "base")
    
    library.dynam.unload("memisc", libpath)
}
