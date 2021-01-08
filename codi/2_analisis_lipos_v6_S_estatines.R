###                              Analisis LIPOS --------------

#        23/12/2019

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
# fitxer_lectura<-"BD_LIPOS_N1217.sav"

fitxer_output<-"TAULES_LIPOS_S_Estatines.Rdata"

##
## setwd en directori de treball 
# 
# "CIBERDEM/ESMERALDA/LIPOS_DIABETICOS" %>% 
#   directori_treball(directori.arrel)
# 

##    -Preparació: Lectura de fitxer de dades            ----------------------                     
####  Llegir dades    

# dades<-readRDS(here::here("dades","BD_LIPOS_N1217_lipos.rds"))
dades<-read.spss(here::here("dades","BD_LIPOS_N1217_nou.sav"),use.value.labels = T,to.data.frame = T)
dades<-dades %>% mutate (ID=stringr::str_trim(as.character(ID))) %>% as_tibble()


# dades_lipos<-haven::read_spss(here::here("dades","LIPOS_TOTAL_N1644.sav"))
dades_lipos<-read.spss(here::here("dades","LIPOS_TOTAL_N1644.sav"),use.value.labels = T,to.data.frame = T)

dades_lipos<-dades_lipos %>% rename(ID=ID_Client) %>% 
  mutate(ID=stringr::str_trim(as.character(ID))) %>% 
  mutate(Sample_ID_Client=stringr::str_trim(as.character(Sample_ID_Client))) %>% 
  as_tibble()

# Eliminar repetit ID=="T2D319" sense Sample_ID_Client
dades_lipos<-dades_lipos %>% filter(!(ID=="T2D319" & Sample_ID_Client==""))


# Seleccionar lipos
dades_lipos<-dades_lipos %>% select("ID","VLDL_C":"LDL_PHDL_P")

# Treure les lipos de dades 
dades<-dades %>% dplyr::select(-c("VLDL_C":"LDL_PHDL_P"))

# Juntat novament dades 
dades<-dades %>% left_join(dades_lipos,by="ID")


# Capturar LIPOS ACTUALS
# dades<-read.spss(fitxer_lectura,use.value.labels = TRUE,to.data.frame=TRUE)


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
  Large_PER_HDL=Large_HDL_P_molL/HDL_P_molL)

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


#  Recode automatic amb funció recode LLEPALI  
dades<-dades %>% recodificar(taulavariables="VARIABLES.xls",criteris="recode1")


# Combinació Sex+IMC
dades<-dades %>% mutate(imc_sex=paste0(Sex,":",BMI.cat4))
# Missings NA 
dades<-dades %>% mutate(imc_sex=if_else(imc_sex== "Women:NA" | imc_sex=="Men:NA", NA_character_,imc_sex))


saveRDS(dades,here::here("dades","BD_preparades_N929.Rds"))


