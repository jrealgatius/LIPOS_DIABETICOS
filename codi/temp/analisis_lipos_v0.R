############
install.packages("compareGroups")
install.packages("pander")
install.packages("readxl")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("htmlwidgets")
install.packages("lattice")
install.packages("SNPassoc")
install.packages("haplo.stats")
install.packages("SNPassoc")
install.packages("polspline")
install.packages("rmarkdown")


rm(list=ls())
library("SNPassoc")
library("htmlwidgets")
library("compareGroups")
library("foreign")
library("lattice")
library("Hmisc")
library("ggplot2")
library("pander")
library("readxl")
library("rmarkdown")
library("knitr")
library("data.table")
library("MatchIt")


####        FUNCIO FORMULA GENERA FORMULA A PARTIR DE VARIABLES       #######################
#####       hi envio la columna de variables amb que vull generar la formula pel compare ###########
formula=function(x="taula1",y="outcome") {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  llistataula<-eval(parse(text=pepito))
  z<-as.formula(paste(y, paste(llistataula, collapse=" + "), sep=" ~ "))
}

#######################
#####       FUNCIO SELECTOR DE VARIABLES TAULA DE     #######
selectorvariables=function(taula="table1") {
  
  pepito<-paste("dadestotal[,as.vector(variables[variables$",taula,"==1,]$camp),with=FALSE]",sep="")
  dades<-eval(parse(text=pepito))
}

#####       hi envio la columna de variables amb que vull generar la formula pel compare ###########
#             FUNCIO variables.ajust              ###################
variables.ajust=function(x="taula1") {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  llistataula<-eval(parse(text=pepito))
  z<-paste(llistataula, collapse=" + ")
}


####        FUNCIO OR.ajustats(x,ajust,y)         ###########
#
OR.ajustats=function(x="lipos",ajust="V.ajust",y="prediabetis",d=dadestotal) {
  # inicialitzar 
  num<-paste("length(variables[variables$",x,"==1,]$camp)",sep="")
  num<-eval(parse(text=num))
  ORadj<-matrix(data=NA,ncol=4,nrow = num)
  # noms de columnes en matriu ORadj
  listvariables<-paste("list(variables[variables$",x,"==1,]$camp[1:",num,"],c('OR','Linf','Lsup','p valor'))",sep="")
  dimnames(ORadj)<-eval(parse(text=listvariables))
  #
  #### extrec la variable que vull ajustar
  xtext<-paste("variables[variables$",x,"==1,]",sep="")
  #
  ##  inicio bucle amb totes les variables que vull ajustar
  for (i in 1:num) {
    xeval<-eval(parse(text=xtext))$camp[i]
    # genero la forumla del model 
    myFormula<-paste(y,"~",xeval,"+",variables.ajust(ajust),sep="")
    # ajusto models
    model<-glm(formula= myFormula, family = binomial, data=d)
    model
    # extrec Coeficients dels models i IC i coloco dins de ORadj
    lolo<-cbind(OR=exp(summary.glm(model)$coef[,1]),Linf=exp(summary.glm(model)$coef[,1]-1.96*summary.glm(model)$coef[,2]),Lsup=exp(summary.glm(model)$coef[,1]+1.96*summary.glm(model)$coef[,2]),p_value=summary.glm(model)$coef[,4])
    ORadj[i,]<-cbind(OR=exp(summary.glm(model)$coef[2,1]),Linf=exp(summary.glm(model)$coef[2,1]-1.96*summary.glm(model)$coef[2,2]),Lsup=exp(summary.glm(model)$coef[2,1]+1.96*summary.glm(model)$coef[2,2]),p_value=summary.glm(model)$coef[2,4])
    print(kable(lolo))
  }
  ORadj
}
#
############################    FI DE FUNCIO OR AJUSTATS            ###########################
#                                                                                           ###
########################################    TD1                                 ###############
########################################    TD1                                 ###############
########################################    TD1                                 ###############
########################################    TD1                                 ###############
#
memory.size(max=16000)

setwd ("P:/Endocrine Group/EiN_MVD/DIDAC/JORDI/LIPOS_diabeticos") 
setwd ("G:/Google Drive/CIBERDEM/ESMERALDA/LIPOS_DIABETICOS")

# BD_LIPOprt_N1324.sav

####  Llegir dades    #####
dades<-read.spss('BD_LIPOprt_N1324.sav',use.value.labels = TRUE,to.data.frame=TRUE)

table(dades$DM)
###########################     1. SUBSET DATOS (DM1 VS CONTROLS)         ################
dades<-subset(dades,dades$DM=="No" | dades$DM=="T1D")

###########################     1. SUBSET DATOS (DM1 VS CONTROLS)         ################
dades<-subset(dades,dades$DM=="No" | dades$DM=="T2D")

#
dades<-data.table(dades)
##              ###
dadestotal<-dades

summary(dades)
names(dades)

####  Llegir etiquetes i variables a analitzar ####
variables <- read_excel('VARIABLES.xls')
variables[is.na(variables)]<- 0
#
###################################   etiquetar variables           ################
seleccio<-variables
camp<- as.vector(seleccio$camp) #
descripcio<- as.vector(seleccio$descripcio) #
### etiquetar variables seleccionades     #################################################
for (i in 2:length(descripcio)){if (any(colnames(dadestotal) == camp[i])) {label(dadestotal[[camp[i]]]) <- descripcio[i]}}
##############################################################################
#


##########################        table1RD            ###################
dades<-selectorvariables("matching")


##########  MATCHIT #################################################
set.seed(123)

##    genero la formula del propensity amb les variables D'interes
##  elimino camps --> cohort i idp de llista de variables 
llistaPS<-as.vector(names(dades)) [!as.vector(names(dades)) %in%c("ID","DM")]
#
## genero grup 
dades$cohort<-ifelse(dades$DM=="No",0,1)
#
#
formulaPS<-as.formula(paste("cohort", paste(llistaPS, collapse=" + "), sep=" ~ "))
##  aplicar matching      #
m.out<-matchit(formulaPS,method="nearest",data=dades,caliper=0.05)
####    comprovar el caliper (->>0.05)####
sd(m.out$distance)*0.25
summary(m.out$distance)
summary(subset(m.out$distance,m.out$weights==1))

propensityDM1<-summary(m.out)

PS_pre<-m.out$distance
PS_post<-subset(m.out$distance,m.out$weights==1)

sd(PS_pre)*0.25
sd(PS_post)*0.25

#########################     FI MATCHING                 ###########
###       AGREGO VARIABLES A TAULA TOTAL    ###
###   afegeixo a dadestotal la variable PS 
dadestotal<-data.table(dadestotal,ps=m.out$weights)


### actualitzo descriptiu nomes amb mostra PS 
################################        COMPARE GRUPS       #########################
##  SELECT 
dades<-subset(dadestotal,ps==1)



################################        COMPARE GRUPS a LIPOS        #########################
####    todas   #####

res1 <- compareGroups(DM ~ .-ID, data=dades,include.miss = F,na.exclude=F,include.label=T)
restab1<-createTable(res1, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
restab1

names(dades)


###     LIPOS   ######

#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("lipos",y="DM"), data=dades,include.miss = F,na.exclude=F,include.label=T)
taula2<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
taula2


###   Extreure p-valors i ajustar  ####
pvalors <- getResults(res, "p.overall")
##
##    Ajust per comparativa multiple
# The "BH" (aka "fdr") and "BY" method of Benjamini, Hochberg, and Yekutieli control the false discovery rate, 
# the expected proportion of false discoveries amongst the rejected hypotheses. The false discovery rate is a less stringent condition than the family-wise error rate, so these methods are more powerful than the others.
pvals<-data.table(pvalors)
pvals$variable<-names(pvalors)
pvals$BHpvalor<-p.adjust(pvalors, method = "BH")
pvals$sigBH[pvals$BHpvalor<0.05] <- "Sig"
pvalstaula1<-pvals
kable(pvalstaula1)



###     LIPOS + OTRAS    ######

#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("v.ajust",y="DM"), data=dades,include.miss = F,na.exclude=F,include.label=T)
taula3<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
taula3
###
###


###     LIPOS ajustat       ######
### p-valors ajustats 

variablesajust1<-variables$camp[variables$v.ajust==1]

ajustats<-OR.ajustats(x="lipos",ajust="v.ajust",y="DM",d=dades)

kable(ajustats)



#############################     SELECT NO DLP         ####
table(dades$DLP)
dades<-dades[dades$DLP=="No"]

#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("lipos",y="DM"), data=dades,include.miss = F,na.exclude=F,include.label=T)
taula1.NODLP<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
taula1.NODLP


###   Extreure p-valors i ajustar  ####
pvalors <- getResults(res, "p.overall")
##
##    Ajust per comparativa multiple
# The "BH" (aka "fdr") and "BY" method of Benjamini, Hochberg, and Yekutieli control the false discovery rate, 
# the expected proportion of false discoveries amongst the rejected hypotheses. The false discovery rate is a less stringent condition than the family-wise error rate, so these methods are more powerful than the others.
pvals<-data.table(pvalors)
pvals$variable<-names(pvalors)
pvals$BHpvalor<-p.adjust(pvalors, method = "BH")
pvals$sigBH[pvals$BHpvalor<0.05] <- "Sig"
pvalstaulaNODSL<-pvals
kable(pvalstaulaNODSL)


###     OTRAS    ######
#####     fer el compare amb taula1 taula2 taula3
res <- compareGroups(formula("v.ajust",y="DM"), data=dades,include.miss = F,na.exclude=F,include.label=T)
taula3NODL<-createTable(res, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
taula3NODL
###
###

###     LIPOS ajustat       ######
### p-valors ajustats 

ajustatsNODL<-OR.ajustats(x="lipos",ajust="v.ajust2",y="DM",d=dades)

kable(ajustatsNODL)

variablesajust2<-variables$camp[variables$v.ajust2==1]
variablesajust2


###################################################       fififififiifi     ##############################
##
##
##

save.image("Analisis_DM1vsControls")
##############################      el mateix però DM2 vs controls                             ############
save.image("Analisis_DM2vsControls")















