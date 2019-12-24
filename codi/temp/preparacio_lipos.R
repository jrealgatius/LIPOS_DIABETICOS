#       PREPARACIÓ LIPOS       -----------------------

rm(list=ls())

#       Carrega de Funcions / Directori arrel  ----------------

directori.arrel<-c("C:/Users/Jordi/Google Drive", 
                   "C:/Users/usuari/Google Drive",
                   "C:/Users/43728088M/Google Drive",
                   "C:/Users/jreal/Google Drive",
                   "D:/Google Drive",
                   "G:/Google Drive",
                   "E:/Google Drive")

library(dplyr)
directori.arrel[file.exists(directori.arrel)] %>% 
  file.path("Stat_codis/funcions_propies.R") %>% 
  source()


#### setwd en directori de treball 

"CIBERDEM/ESMERALDA/LIPOS_DIABETICOS" %>% 
  directori_treball(directori.arrel)



# 1. Lectura Bdades     -----------------
####  Llegir dades  
dades<-read.spss("BD_LIPOS_N1218_nou.sav",use.value.labels = TRUE,to.data.frame=TRUE)

library(dplyr)
library(stringr)
names(dades)

#### 1. Generar id.lipos -----------
dades<-dades %>% mutate (
  ID=str_replace_all(as.character(ID), " ",""),
  id.lipos=paste0(Origen,".",ID))



###  2. Llegir taules LIPOS excel --------------

list.files(pattern ="xls")

##  2.1. LLegir ficher MAURICIO 2 

LIPOS_MAURICIO<-readxl::read_excel("Conjunto_FIS_LiposcaleResults_MauricioD002.xls",skip = 6)
LIPOS_MAURICIO

names(LIPOS_MAURICIO)
## 2.1.1. Netejar noms de variables -------------
LIPOS_MAURICIO<-netejar.noms.variables(LIPOS_MAURICIO)

names(LIPOS_MAURICIO)

## 2.1.2. Genero id lipos (Lleida + CanRuti) ---------------
LIPOS_MAURICIO<-LIPOS_MAURICIO %>% 
  mutate(origen=ifelse(str_detect(X__1,"BDN"),"Can Ruti","Lleida"),
         id.lipos=paste0(origen,".",X__1))


###  2.2. Llegir Llipos ORTEGA  ---------------
LIPOS_EORTEGA<-readxl::read_excel("LiposcaleResults_EOrtega.xls",skip = 6)
LIPOS_EORTEGA

## 2.2.1. Netejar noms de variables ---------
LIPOS_EORTEGA<-netejar.noms.variables(LIPOS_EORTEGA)
##  2.2.2. Genero id lipos ortega  ------------------
LIPOS_EORTEGA<-LIPOS_EORTEGA %>% 
  mutate (id.lipos=paste0("Clinic",".",Sample_ID))


# 2.3.  Llegeixo MAURICIO 3 T1D   -----------------------------

LIPOS_T1DDEBUTS<-readxl::read_excel("LiposcaleResultsMauricio003.xls",skip = 6)

# 2.3.1. Netejar noms de variables  -------------------
LIPOS_T1DDEBUTS<-netejar.noms.variables(LIPOS_T1DDEBUTS)

#  Lleida + CanRuti

##  2.3.2. Genero id.lipos  ---------------
LIPOS_T1DDEBUTS<-LIPOS_T1DDEBUTS %>% 
  mutate(id.lipos=ifelse(str_detect(Sample_ID_Client,"BDN") | str_detect(Sample_ID_Client,"CT") ,
                         paste0("Can Ruti",".",Sample_ID_Client),
                         paste0("Lleida",".",X__1)))


names(LIPOS_T1DDEBUTS)

# 2.4. Llegir Mauricio 1  ------------------

LIPOS_MOLLERUSSA<-readxl::read_excel("Mollerussa_LiposcaleResults_MauricioD001.xls",skip = 6)

## 2.4.1.  Netejar noms  --------------------

LIPOS_MOLLERUSSA<-netejar.noms.variables(LIPOS_MOLLERUSSA)

##  2.4.2. Genero id lipos --------------------
LIPOS_MOLLERUSSA<-LIPOS_MOLLERUSSA %>% 
  mutate (id.lipos=paste0("Mollerussa",".",ID))


##  3. Fusionar totes les taules LIPOS   ---------------------

LIPOS_TOTAL<-dplyr::bind_rows(LIPOS_MAURICIO,LIPOS_EORTEGA,LIPOS_T1DDEBUTS,LIPOS_MOLLERUSSA)

LIPOS_TOTAL<-LIPOS_TOTAL %>% select(-ID, BT_ID)

### Seleccionar registres unics en LIPOS TOTAL

LIPOS_UNICS<-LIPOS_TOTAL %>% group_by(id.lipos) %>%  slice(1) %>% ungroup 


##  4. FUSIONAR AMB dades    ------------------

dades<-dades %>% left_join(LIPOS_UNICS,by="id.lipos")


# 5. Eliminar LIPOS ANTIC  -----------------------

dades<-dades %>% select(-c(
  VLDLPnmolL,LargeVLDLPnmolL,MediumVLDLPnmolL,SmallVLDLPnmolL,LDLPnmolL,LargeLDLPnmolL,MediumLDLPnmolL,SmallLDLPnmolL,HDLPµmolL,
LargeHDLPµmolL,MediumHDLPµmolL,SmallHDLPµmolL,VLDLZnm,LDLZnm,HDLZnm,NonHDLPnmolL,TotalPHDLP,LDLPHDLP))

## 6. Salvar dades  ----------------------

saveRDS(dades,"BD_LIPOS_N1218_lipos.rds")

### Save en STATA       ---------------

write.dta(dades, "BD_LIPOS_N1218_lipos.dta")

###################################################################################






