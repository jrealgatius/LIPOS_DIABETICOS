###                              Analisis LIPOS --------------

###       07/01/2019

### Índex  -------------

####    1. Calcular % de cada grup de lipos (Larg+Medium+Small) 
####    2. Medianas 
####    3. Analisis estratifit per Statines 
##      1. Càrrega de funcions i Lectura   ---------------

rm(list=ls())


##  Esquema -------------------------

##  Comparativa 2 a 2
##  Subestudios: 
##  Població global cru/ajustat 
##  població sense estatines
##  població sense estatines + fibratos
##  Dins DM: HbA1c estratificada >> <7 / 7-8 / >8 (Cru/Ajustat)
##  Descriptiu (Població Normal )
##  Valors de normalitat Lipos 
##      controles de Mollerusa (variable Origen 2) y Can Ruti (variable Origen 4) + eliminar a los que tengan DLP.

##    -Inicialització     ------------------

##  Directori arrel 
###

directori.arrel<-c("C:/Users/Jordi/Google Drive", 
                   "C:/Users/usuari/Google Drive",
                   "C:/Users/43728088M/Google Drive",
                   "C:/Users/jreal/Google Drive",
                   "D:/Google Drive",
                   "G:/Google Drive",
                   "E:/Google Drive")


##    -Càrrega de funcions  ---------------------

library(dplyr)
directori.arrel[file.exists(directori.arrel)] %>% 
  file.path("Stat_codis/funcions_propies.R") %>% 
  source()


##    -Edició de paràmetres     -----------------

fitxer_lectura<-"BD_LIPOS_N1217.sav"

##
## setwd en directori de treball 

"CIBERDEM/ESMERALDA/LIPOS_DIABETICOS" %>% 
  directori_treball(directori.arrel)


##    -Preparació: Lectura de fitxer de dades            ----------------------                     
####  Llegir dades    
# dades<-read.spss("BD_T2D_CT_13_N1223.sav",use.value.labels = TRUE,to.data.frame=TRUE)
# dades<-readRDS("BD_LIPOS_N1217.sav")

dades<-read.spss(fitxer_lectura,use.value.labels = TRUE,to.data.frame=TRUE)

##    -Etiquetar variables  ----------------

dades<-etiquetar(dades,taulavariables = "VARIABLES.xls")

##  1. Calcular % de cada grup de lipos (Larg+Medium+Small)  ----------

# % VLDL
dades<-dades %>% mutate(
  Small_PER_VLDL=Small_VLDL_P_nmolL/VLDL_P_nmolL,
  Medium_PER_VLDL=Medium_VLDL_P_nmolL/VLDL_P_nmolL,
  Large_PER_VLDL=Large_VLDL_P_nmolL/VLDL_P_nmolL
  )

# % LDL
dades<-dades %>% mutate(
  Small_PER_LDL=Small_LDL_P_nmolL/LDL_P_nmolL,
  Medium_PER_LDL=Medium_LDL_P_nmolL/LDL_P_nmolL,
  Large_PER_LDL=Large_LDL_P_nmolL/LDL_P_nmolL
  )

# % HDL
dades<-dades %>% mutate(
  Small_PER_HDL=Small_HDL_P_molL/HDL_P_molL,
  Medium_PER_HDL=Medium_HDL_P_molL/HDL_P_molL,
  Large_PER_HDL=Large_HDL_P_molL/HDL_P_molL
  )


names(dades)

# % HDL
dades<-dades %>% mutate(
  Small_PER_HDL=Small_HDL_P_molL/HDL_P_molL,
  Medium_PER_HDL=Medium_HDL_P_molL/HDL_P_molL,
  Large_PER_HDL=Large_HDL_P_molL/HDL_P_molL
)

# % 

dades<-dades %>% mutate(
  TG_PER_VLDL=VLDL_TG/(VLDL_TG+VLDL_C),
  TG_PER_IDL=IDL_TG/(IDL_TG+IDL_C),
  TG_PER_LDL=LDL_TG/(LDL_TG+LDL_C),
  TG_PER_HDL=HDL_TG/(HDL_TG+HDL_C),
  C_PER_VLDL=VLDL_C/(VLDL_TG+VLDL_C),
  C_PER_IDL=IDL_C/(IDL_TG+IDL_C),
  C_PER_LDL=LDL_C/(LDL_TG+LDL_C),
  C_PER_HDL=HDL_C/(HDL_TG+HDL_C))
  

levels(dades$Prediabetes)

# 1 1. Diferencias  de las lipos en cada grupo -> control, prediabético y diabético (variable Prediabetes) -> 
# Tabla con p_overall y p entre grupos . Todas las variables

library(compareGroups)


##  2. Descriptiu / comparatiu variables clínicas   ------------------

T0.1.1<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method = 1)
T0.1.1


T0.1.2<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method = 2)
T0.1.2


T0.2<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,show.p.mul = T,show.descr = F,show.p.overall = F)
T0.2


##  2.1 Descriptiu / comparatiu variables clínicas població sense estatines -------------

dades_s_statins<-dades %>% filter(Statins=="No")

T0.3<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades_s_statins)
T0.3

T0.4<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades_s_statins)
T0.4


##  3.2 Descriptiu / comparatiu variables clínicas població sense estatines + fibratos-------------

dades_s_statins_fibra<-dades %>% filter(Statins=="No" & Fibrate=="No")

T0.5<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades_s_statins_fibra)
T0.5

T0.6<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades_s_statins_fibra)
T0.6

##  4. Descriptiu / comparatiu Lipos 2 a 2    ------------------------

T1.1.1<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method=1)
T1.1.1

T1.1.2<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method=2,show.p.overall = F)
T1.1.2

T1.2<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,show.p.mul = T,show.descr = F,show.p.overall = F)
T1.2


##    Prediabetis vs Control / Prediabetis vs Diab / Diab vs Pre
## 
##      P valors crus ajustats multiple_testing  ------------

T1.3<-descrTable(formula_compare(x="lipos2",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,show.descr = F,show.p.overall = T)
T1.3

T1.4<-descrTable(formula_compare(x="lipos2",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,show.p.mul = T,show.descr = F,show.p.overall = F)

T1.4

T1.5<-Pvalors_ajustats_compare(T1.3,metodo = "bonferroni")

kable(T1.5,format="pandoc",digits=4)

##  Coeficients crus i ajustats ---------------------

coef_crus<-extreure_coef_glm(dt=dades,outcomes = "lipos",x="Prediabetes",taulavariables ="VARIABLES.xls",z="")
coef_cru<-
  coef_crus[[1]] %>% 
  as_tibble %>% 
  select(Outcome,Cat.X,Bcrude=Estimate,Pcrude=`Pr(>|t|)`)

coef_age.sex.ajust<-extreure_coef_glm(dt=dades,outcomes = "lipos",x="Prediabetes",taulavariables ="VARIABLES.xls",z="age.sex.ajust")
coef_age.sex<-
  coef_age.sex.ajust[[1]] %>% 
  as_tibble %>% 
  select(Outcome,Cat.X,Badj_AgeSex=Estimate,Padj_AgeSex=`Pr(>|t|)`)

coef_ajust<-extreure_coef_glm(dt=dades,outcomes = "lipos",x="Prediabetes",taulavariables ="VARIABLES.xls",z="v.ajust")
coef_ajust<-
  coef_ajust[[1]] %>% 
  as_tibble %>% 
  select(Outcome,Cat.X,Badj=Estimate,P_adj=`Pr(>|t|)`)


taulacoef<-coef_cru %>% 
  cbind(
    select(coef_age.sex,-c(Outcome,Cat.X))
    ) %>% 
  cbind(
    select(coef_ajust,-c(Outcome,Cat.X))
    ) %>% as_tibble

kable(taulacoef,digits = 3,format="pandoc")

##      p-valores 2 a 2 ----------------------

###  Cal fer 3 grups  (No vs PreDiabetis / No vs Diabetes / Pre vs  Diab)

# Modelo 1 Control vs. PreDiabetes       -----------------
# Modelo 2 Control vs. Diabetes       -----------------
# Modelo 3 Prediabetes vs. Diabetes   ------------------

table(dades$Prediabetes)

OR.ajust<-OR.ajustats(x="lipos2",ajust="v.ajust",y="Prediabetes",d=dades,taulavariables="VARIABLES.xls")



###     P valors crus ajustats multitesting  ------------

T1.6<-Pvalors_ajustats_taula(OR.ajust,p.valors='pvalor',metodo = "bonferroni")

T1.6


##  5. Subanálisis 1 -> Sólo en los diabéticos  ------------------------
#         HbA1c estratificada. Tabla con p_overall y p entre grupos de estratificación   ->> <7 / 7-8 / >8  -----------------
# Didac: Bien /regular /mal controlados. Todas las variables 

#### Filtro DM

dadesDM<-dades %>% filter(DM=="T2D")

### Estratificacion HB <7 / 7-8 / >8 HbA1c

dadesDM<-dadesDM %>% mutate(
  HbA1c_cat3=ifelse(HbA1c<7,"<7%",
                    ifelse(HbA1c>=7 & HbA1c<=8,"[7-8%]",">8%")
                    ))

## Levels 

dadesDM<-dadesDM %>% mutate(
  HbA1c_cat3=factor(HbA1c_cat3, levels = c("<7%", "[7-8%]" , ">8%"))
  )
table(dadesDM$HbA1c_cat3)


################# Recode amb funció recode

dadesDM<-dadesDM %>% recodificar(taulavariables="VARIABLES.xls",criteris="recode1")
table(dades$HbA1c_g3)


##  5.1. Taules descriptives Clíniques  --------------------

##################    Clínicas 

T2.0<-descrTable(formula_compare(x="clinicas",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dadesDM,show.p.trend = T,show.p.overall = F)

T2.0

####5.2.  LIPOS 
##  5.2. Taules Lipos                   --------------------

T2.1<-descrTable(formula_compare(x="lipos",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dadesDM,show.p.trend = T,show.p.overall = F)

T2.1

T2.2<-descrTable(formula_compare(x="lipos",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dadesDM,show.p.mul = T,show.descr = F,show.p.overall = F)

T2.2

T2.3 <- Pvalors_ajustats(T1.2,metodo = "bonferroni")

T2.3


###  3.  Subanálisis 2 -> Valores de normalidad de lipos, incluídos los controles de Mollerusa (variable Origen 2) y Can Ruti (variable Origen 4) --------------

# Sólo los 0 (cero) de la variable Prediabetes.

# Eliminar a los que tengan DLP.


#### Filtro:  DLP=No & Prediabetes=No + Controles de Mollerussa+CanRuti
subset_dades<-dades %>% 
  filter(DM=="Control" & Origen!="Clinic") %>% 
  filter(Prediabetes=="No") %>% 
  filter (DLP=="No")

formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls")

subset_dades<-subset_dades %>% as.data.table()

T3.1<-descrTable(~Total_cholesterol + HDL_cholesterol,data=subset_dades,method = 1)

T3.1<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method = 1)
T3.1

T3.2<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method = 4)
T3.2

T3.3<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method=4, Q1 = 0, Q3 = 1)
T3.3



rbind("Mean(SD)"=T3.1,"Median[Q1-Q3]"=T3.2,"Median[Min;Max]"=T3.3)


# 4. Hacer los modelos con la N total y luego eliminando a los pacientes con DLP:



####  Fer models
###     LIPOS ajustat       ######
### p-valors ajustats 


OR.ajust<-OR.ajustats(x="lipos",ajust="v.ajust",y="Prediabetes",d=subset_dades)
OR.crudes<-OR.ajustats(x="lipos",y="Prediabetes",d=subset_dades)

kable(OR.ajust)

OR<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=subset_dades,
               show.ratio = T,show.p.ratio = T,show.descr = T,compute.ratio = T,show.p.overall = F)
OR
kable(OR.crudes)

# Modelo 2 Control vs. Diabetes       -----------------

# Modelo 3 Prediabetes vs. Diabetes   ------------------



save.image("TAULES_LIPOS.Rdata")

