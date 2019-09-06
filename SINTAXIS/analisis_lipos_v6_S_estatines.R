###                              Analisis LIPOS --------------

#        07/01/2019

## 0.Índex  -------------

####    1. Calcular % de cada grup de lipos (Larg+Medium+Small) 
####    2. Medianas 
####    3. Analisis estratifit per Statines 


## 0.Càrrega de funcions i Lectura   ---------------

rm(list=ls())


##  Esquema 
##  Comparativa 2 a 2
##  Subestudios: 
##  Població global cru/ajustat 
##  població sense estatines
##  població sense estatines + fibratos
##  Població DM: HbA1c estratificada >> <7 / 7-8 / >8 (Cru/Ajustat)
##  Descriptiu (Població Normal)
##  Valors de normalitat Lipos 
##      controles de Mollerusa (variable Origen 2) y Can Ruti (variable Origen 4) + eliminar a los que tengan DLP.

##    -Inicialització     ------------------

##  Directori arrel 
###

# Carrega de funcions 

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


##    -Edició de paràmetres     -----------------
fitxer_lectura<-"BD_LIPOS_N1217.sav"

fitxer_output<-"TAULES_LIPOS_S_Estatines.Rdata"

##
## setwd en directori de treball 
# 
# "CIBERDEM/ESMERALDA/LIPOS_DIABETICOS" %>% 
#   directori_treball(directori.arrel)
# 

##    -Preparació: Lectura de fitxer de dades            ----------------------                     
####  Llegir dades    

dades<-readRDS(here::here("dades","BD_LIPOS_N1217_lipos.rds"))

# dades<-read.spss(fitxer_lectura,use.value.labels = TRUE,to.data.frame=TRUE)

##    -Etiquetar variables  ----------------
dades<-etiquetar(dades,taulavariables = "VARIABLES.xls")

## Fase: PREPARACIÓ DE VARIABLES  ------------------------------

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
  
##  2. Recodificació HB <7 / 7-8 / >8 HbA1c  -----------------

dades<-dades %>% mutate(
  HbA1c_cat3=ifelse(HbA1c<7,"<7%",
                    ifelse(HbA1c>=7 & HbA1c<=8,"[7-8%]",">8%")
  ))

## Levels 

dades<-dades %>% mutate(
  HbA1c_cat3=factor(HbA1c_cat3, levels = c("<7%", "[7-8%]" , ">8%"))
)
table(dades$HbA1c_cat3)


#  Recode amb funció recode LLEPALI  

dades<-dades %>% recodificar(taulavariables="VARIABLES.xls",criteris="recode1")



##  Aplicar filtre  (Sin estatinas ni fibratos) --------------------------- 

dades<-dades %>% filter(!(Statins=="Yes" | Fibrate=="Yes")) 


# Copia 
dadestotal<-dades

## Fase: ANALISIS     -------------------------------
## * A) Alteraciones de las lipoproteinas en cada grupo (control, prediabético y diabético) ----------------
# 1.1. Diferencias  de las lipos en cada grupo -> control, prediabético y diabético (variable Prediabetes) -> 
# Tabla con p_overall y p entre grupos . Todas las variables
##  1. Descriptiu / comparatiu variables clínicas (Pre/DM/Cntrol)   ------------------

T0.1.1<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method = 1)
T0.1.1

T0.1.2<-descrTable(formula_compare(x="clinicas",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method = 2,show.p.overall = F)
T0.1.2

##  2. Descriptiu cru/ comparatiu Lipos global y 2 a 2    ------------------------

T1.1.1<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method=1,show.p.overall = F)
T1.1.1

T1.1.2<-descrTable(formula_compare(x="lipos",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,method=2,show.p.overall = F)
T1.1.2

##  3. Diferencies (DM, Pre vs control) i P valors crus ajustats-multitesting  ------------

T1.3<-descrTable(formula_compare(x="lipos2",y="Prediabetes",taulavariables ="VARIABLES.xls"),data=dades,show.descr = F,show.p.overall = F,show.p.mul = T)

# Ajustat multitesting 
T1.5<-Pvalors_ajustats_compare(objecte_compare=T1.3,metodo = "bonferroni",p="p.mul")

kable(T1.5,format="pandoc",digits=4, caption= "P adjusted by multitesting")

##  4. Coeficients crus i ajustats ---------------------

## Capturo taula de coeficients, crus i ajustats
z<-c("","age.sex.ajust","v.ajust")
coeficients<-z %>% map(extreure_coef_glm,dt=dades,outcomes = "lipos2",x="Prediabetes",taulavariables ="VARIABLES.xls")

taula_coef_cru<-
  coeficients[[1]]$coef %>% 
  as_tibble %>% 
  setnames("Estimate","B_crude")

taula_coef_age.sex<-
  coeficients[[2]]$coef %>% 
  as_tibble %>% 
  setnames(c("Estimate","Pr(>|t|)"),
           c("Badj_AgeSex","Padj_AgeSex"))
  
taula_coef_ajust<-
  coeficients[[3]]$coef %>% 
  as_tibble %>% 
  setnames(c("Estimate","Pr(>|t|)"),
           c("B_adj","P_adj"))

taulacoef<-taula_coef_cru %>% 
  cbind(
    select(taula_coef_age.sex,-c(Outcome,Cat.X,`Std. Error`))) %>% 
  cbind(
    select(taula_coef_ajust,-c(Outcome,Cat.X,`Std. Error`))) %>% 
  as_tibble


##  Ajusto per comparacions multiples 
taulacoef_DMvsPREDM<-Pvalors_ajustats_taula(objecte_taula=taulacoef, p.valors='P_adj', metodo="bonferroni") %>% 
  select(-P_adj)

text_taula<-extreure.variables("v.ajust",taulavariables = "VARIABLES.xls") %>% paste(collapse = "+")

kable(taulacoef_DMvsPREDM,digits = 3,format="pandoc",caption=paste0("Adjusted by:",text_taula))

## * B) Alteraciones de las lipoproteinas en pacientes DM segun grado de control  ----------------
##  Analisis subgrup DM: Relació amb control HB (<7 / 7-8 / >8)        ----------------------
##   Subanálisis :Sólo en los diabéticos  
##   HbA1c estratificada. Tabla con p_overall y p entre grupos de estratificación   ->> <7 / 7-8 / >8  -----------------
# Didac: Bien /regular /mal controlados. Todas las variables 
# Filtre 
dades<-dades %>% filter(DM=="T2D")

##  5.1. Taules descriptives Clíniques  --------------------
##################    Clínicas 

T2.0<-descrTable(formula_compare(x="clinicas",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dades,show.p.trend = T,show.p.overall = F)

T2.0

##  5.2. Taules Lipos                   --------------------

T2.1<-descrTable(formula_compare(x="lipos",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dades,show.p.trend = T,show.p.overall = F)

T2.1

T2.2<-descrTable(formula_compare(x="lipos",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dades,show.p.trend = F,method = 4,show.p.overall = F)

T2.2


##  5.3. Multitesting i ajustat   -----------------

T2.3<-descrTable(formula_compare(x="lipos2",y="HbA1c_cat3",taulavariables ="VARIABLES.xls"),data=dades,show.p.mul = T,show.descr = F,show.p.overall = F)

taula_sig<-Pvalors_ajustats_compare(T2.3,metodo="bonferroni",p="p.mul",Sig="Si")

kable(taula_sig,caption="p values adjusted by multitesting",format="pandoc")

## Capturo taula de coeficients, crus i ajustats
z<-c("","age.sex.ajust","v.ajust")
coeficients<-z %>% map(extreure_coef_glm,dt=dades,outcomes = "lipos2",x="HbA1c_cat3",taulavariables ="VARIABLES.xls")

taula_coef_cru<-
  coeficients[[1]]$coef %>% 
  as_tibble %>% 
  setnames("Estimate","B_crude")

taula_coef_age.sex<-
  coeficients[[2]]$coef %>% 
  as_tibble %>% 
  setnames(c("Estimate","Pr(>|t|)"),
           c("Badj_AgeSex","Padj_AgeSex"))

taula_coef_ajust<-
  coeficients[[3]]$coef %>% 
  as_tibble %>% 
  setnames(c("Estimate","Pr(>|t|)"),
           c("B_adj","P_adj"))

taulacoef<-taula_coef_cru %>% 
  cbind(
    select(taula_coef_age.sex,-c(Outcome,Cat.X,`Std. Error`))) %>% 
  cbind(
    select(taula_coef_ajust,-c(Outcome,Cat.X,`Std. Error`))) %>% 
  as_tibble


##  Ajusto per comparacions multiples 
taulacoef_HB<-Pvalors_ajustats_taula(objecte_taula=taulacoef, p.valors='P_adj', metodo="bonferroni") %>% 
  select(-P_adj)

text_taula<-extreure.variables("v.ajust",taulavariables = "VARIABLES.xls") %>% paste(collapse = "+")

kable(taulacoef_HB,digits = 3,format="pandoc",caption=paste0("Adjusted by:",text_taula))


## 5.4. Correlació directa de HB y lipos ####

Correlacions_HB<-extreure_cor(var1="HbA1c",var="lipos2",d="dades",taulavariables="VARIABLES.xls")

# Coeficients crus no ajustats by GLM

taula_coef_hb_crus<-extreure_coef_glm(dt=dades,outcomes="lipos2",x="HbA1c",z="",taulavariables="VARIABLES.xls")
TAULA_HB_Lipos_crus<-Pvalors_ajustats_taula(objecte_taula=taula_coef_hb_crus$coef, p.valors='Pr(>|t|)', metodo="bonferroni")
kable(TAULA_HB_Lipos_crus,caption=taula_coef_hb_crus$caption,format="pandoc",digits = 3)

# Coeficients ajustats by GLM

taula_coef_hb_adj<-extreure_coef_glm(dt=dades,outcomes="lipos2",x="HbA1c",z="v.ajust",taulavariables="VARIABLES.xls")
TAULA_HB_Lipos_adj<-Pvalors_ajustats_taula(objecte_taula=taula_coef_hb_adj$coef, p.valors='Pr(>|t|)', metodo="bonferroni")
kable(TAULA_HB_Lipos_adj,caption=taula_coef_hb_adj$caption,format="pandoc",digits = 3)

## * C) Describir valores de normalidad (DLP=No & Prediabetes=No + Controles de Mollerussa+CanRuti) -------------

## Filtro:  DLP=No & Prediabetes=No + Controles de Mollerussa+CanRuti

dades<-dadestotal

subset_dades<-dades %>% 
  filter(DM=="No" & Origen!="Clinic") %>% 
  filter(Prediabetes=="No") %>% 
  filter (DLP=="No")

subset_dades<-subset_dades %>% as.data.table()

T3.1<-descrTable(~Total_cholesterol + HDL_cholesterol,data=subset_dades,method = 1)

T3.1<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method = 1)
T3.1

T3.2<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method = 4)
T3.2

T3.3<-descrTable(formula_compare(x="lipos",y="",taulavariables ="VARIABLES.xls"),data=subset_dades,method=4, Q1 = 0, Q3 = 1)
T3.3


rbind("Mean(SD)"=T3.1,"Median[Q1-Q3]"=T3.2,"Median[Min;Max]"=T3.3)


# --------------------------------  FI      ----------------------------

# Salvar-ho ---------------------

save.image(fitxer_output)


# Subanalisis: ---------------- 

# Modelo 1 Control vs prediabetis 
# Modelo 2 Control vs. Diabetes   
# Modelo 3 Prediabetes vs. Diabetes 
# Població sense DLP y/o fibratos 
# Població sense estatines 
# Població sense estatines + fibratos 
# Població sense estatines + fibratos + DLP



