### R code from vignette source 'anes48.Rnw'

###################################################
### code chunk number 1: anes48.Rnw:71-72
###################################################
options(width=72)


###################################################
### code chunk number 2: anes48.Rnw:74-77
###################################################
library(memisc)
options(digits=3)
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",package="memisc")


###################################################
### code chunk number 3: anes48.Rnw:91-93
###################################################
nes1948 <- spss.portable.file(nes1948.por)
print(nes1948)


###################################################
### code chunk number 4: anes48.Rnw:98-99
###################################################
names(nes1948)


###################################################
### code chunk number 5: anes48.Rnw:102-103
###################################################
description(nes1948)


###################################################
### code chunk number 6: anes48.Rnw:106-107 (eval = FALSE)
###################################################
## codebook(nes1948)


###################################################
### code chunk number 7: anes48.Rnw:130-142
###################################################
vote.48 <- subset(nes1948,
              select=c(
                  v480018,
                  v480029,
                  v480030,
                  v480045,
                  v480046,
                  v480047,
                  v480048,
                  v480049,
                  v480050
                  ))


###################################################
### code chunk number 8: anes48.Rnw:153-154
###################################################
str(vote.48)


###################################################
### code chunk number 9: anes48.Rnw:177-188
###################################################
vote.48 <- rename(vote.48,
                  v480018 = "vote",
                  v480029 = "occupation.hh",
                  v480030 = "unionized.hh",
                  v480045 = "gender",
                  v480046 = "race",
                  v480047 = "age",
                  v480048 = "education",
                  v480049 = "total.income",
                  v480050 = "religious.pref"
        )


###################################################
### code chunk number 10: anes48.Rnw:192-193
###################################################
codebook(vote.48)


###################################################
### code chunk number 11: anes48.Rnw:231-253
###################################################
vote.48 <- within(vote.48,{
  vote3 <- recode(vote,
    1 -> "Truman",
    2 -> "Dewey",
    3:4 -> "Other"
    )
  occup4 <- recode(occupation.hh,
    10:20 -> "Upper white collar",
    30 -> "Other white collar",
    40:70 -> "Blue collar",
    80 -> "Farmer"
    )
  relig3 <- recode(religious.pref,
    1 -> "Protestant",
    2 -> "Catholic",
    3:5 -> "Other,none"
    )
   race2 <- recode(race,
    1 -> "White",
    2 -> "Black"
    )
  })


###################################################
### code chunk number 12: anes48.Rnw:264-265
###################################################
toLatex(xtabs(~vote3+occup4,data=vote.48))


###################################################
### code chunk number 13: anes48.Rnw:273-275
###################################################
toLatex(t(genTable(percent(vote3)~occup4,data=vote.48)),
  digits=c(1,1,1,0))


###################################################
### code chunk number 14: anes48.Rnw:285-287
###################################################
toLatex(t(genTable(percent(vote3)~relig3,data=vote.48)),
  digits=c(1,1,1,0))


###################################################
### code chunk number 15: anes48.Rnw:295-297
###################################################
toLatex(t(genTable(percent(vote3)~race2,data=vote.48)),
  digits=c(1,1,1,0))


###################################################
### code chunk number 16: anes48.Rnw:306-309
###################################################
inc.tab <- t(genTable(percent(vote3)~total.income,data=vote.48))
rownames(inc.tab) <- gsub("$","\\$",rownames(inc.tab),fixed=TRUE)
toLatex(inc.tab,digits=c(1,1,1,0))


###################################################
### code chunk number 17: anes48.Rnw:320-321
###################################################
options(width=60)


###################################################
### code chunk number 18: anes48.Rnw:323-334
###################################################
agg.inc <- aggregate(percent(vote3,ci=TRUE)~total.income,data=vote.48)
agg.inc.errbars <- xyplot(cbind(Percentage,upper,lower)~total.income,
        data=subset(agg.inc,vote3=="Truman"),
        panel=panel.errbars,
        xlab="Household income",
        ylab="Percentage voting for Truman",
        pch=19,ewidth=0.2,
        scales=list(x=list(
          rot=90
          )),
        )


###################################################
### code chunk number 19: anes48.Rnw:337-338 (eval = FALSE)
###################################################
## print(agg.inc.errbars)


###################################################
### code chunk number 20: anes48.Rnw:340-346
###################################################
trellis.device(pdf,file="agg-inc-errbars.pdf",width=6,height=6)
print(agg.inc.errbars)
dev.off()
trellis.device(postscript,file="agg-inc-errbars.eps",width=6,height=6)
print(agg.inc.errbars)
dev.off()


###################################################
### code chunk number 21: anes48.Rnw:348-349
###################################################
options(width=72)


###################################################
### code chunk number 22: anes48.Rnw:364-375
###################################################
agg.occup <- aggregate(percent(vote3,ci=TRUE)~occup4,data=vote.48)
agg.occup.errbars <- xyplot(cbind(Percentage,upper,lower)~occup4,
        data=subset(agg.occup,vote3=="Truman"),
        panel=panel.errbars,
        xlab="Occupation head of household",
        ylab="Percentage voting for Truman",
        pch=19,ewidth=0.2,
        scales=list(x=list(
          rot=90
          )),
        )


###################################################
### code chunk number 23: anes48.Rnw:378-379 (eval = FALSE)
###################################################
## print(agg.occup.errbars)


###################################################
### code chunk number 24: anes48.Rnw:381-387
###################################################
trellis.device(pdf,file="agg-occup-errbars.pdf",width=6,height=6)
print(agg.occup.errbars)
dev.off()
trellis.device(postscript,file="agg-occup-errbars.eps",width=6,height=6)
print(agg.occup.errbars)
dev.off()


###################################################
### code chunk number 25: anes48.Rnw:406-410
###################################################
vote.48 <- within(vote.48,{
  contrasts(occup4) <- contr("treatment",base = 3)
  contrasts(total.income) <- contr("treatment",base = 4)
  })


###################################################
### code chunk number 26: anes48.Rnw:421-431
###################################################
model1 <- glm((vote3=="Truman")~occup4,data=vote.48,
              family="binomial")
model2 <- glm((vote3=="Truman")~total.income,data=vote.48,
              family="binomial")
model3 <- glm((vote3=="Truman")~occup4+total.income,data=vote.48,
              family="binomial")
model4 <- glm((vote3=="Truman")~relig3,data=vote.48,
              family="binomial")
model5 <- glm((vote3=="Truman")~occup4+relig3,data=vote.48,
              family="binomial")


###################################################
### code chunk number 27: anes48.Rnw:438-439
###################################################
mtable(model1,model2,model3,summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N"))


###################################################
### code chunk number 28: anes48.Rnw:449-464
###################################################
toLatex(relabel(mtable(
            "Model 1"=model1,
            "Model 2"=model2,
            "Model 3"=model3,
            summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N")),
          "[(]Intercept[)]"="\\\\emph{Intercept}",
          "[$]"="\\\\$",
          UNDER="under",
          "AND OVER"="and over",
          occup4="Occup. class",
          total.income="Income",
          gsub=TRUE
          ),
          ddigits=5
        )


###################################################
### code chunk number 29: anes48.Rnw:481-492
###################################################
toLatex(relabel(mtable(
              "Model 1"=model1,
              "Model 4"=model4,
              "Model 5"=model5,
              summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N")),
          "[(]Intercept[)]"="\\\\emph{Intercept}",
            occup4="Occup. class",
            relig3="Religion",
            gsub=TRUE
            ),
          ddigits=5)


###################################################
### code chunk number 30: anes48.Rnw:501-502
###################################################
Tp35 <- Termplot(model3,model5,se=TRUE,residuals="none",xrot=90)#,models="columns")


###################################################
### code chunk number 31: anes48.Rnw:504-505 (eval = FALSE)
###################################################
## print(Tp35)


###################################################
### code chunk number 32: anes48.Rnw:507-513
###################################################
trellis.device(pdf,file="Tp35.pdf",width=7,height=7)
print(Tp35)
dev.off()
trellis.device(postscript,file="Tp35.eps",width=7,height=7)
print(Tp35)
dev.off()


