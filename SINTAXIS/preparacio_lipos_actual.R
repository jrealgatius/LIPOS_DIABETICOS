#       PREPARACIÓ LIPOS       -----------------------

# Substituir per noves LIPOS 

gc()
rm(list=ls())

# Carrega de funcions 

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# 1. Lectura Bdades     -----------------
####  Llegir dades  
dades<-read.spss(here::here("dades","BD_LIPOS_N1217.sav"),use.value.labels = TRUE,to.data.frame=TRUE)


#### 1. Generar id.lipos -----------
dades<-dades %>% mutate (
  ID=str_replace_all(as.character(ID), " ",""),
  id.lipos=paste0(Origen,".",ID))

# RENOMBRAR Sample_id
dades<-dades %>% rename(Sample_ID=Sample_ID_y)


###  2. Llegir taules LIPOS excel --------------
list.files(path="dades/LIPOS_ACTUALS",pattern ="xls")

##  2.1. LLegir ficher MAURICIO 2 

dir_lipos<-"dades/LIPOS_ACTUALS"
here::here(dir_lipos)

LIPOS_MAURICIO<-readxl::read_excel(here::here(dir_lipos,"LiposcaleResultsMauricioD002.xls"),skip = 6)
LIPOS_MAURICIO

names(LIPOS_MAURICIO)
## 2.1.1. Netejar noms de variables -------------
LIPOS_MAURICIO<-netejar.noms.variables(LIPOS_MAURICIO)


###  2.2. Llegir Llipos ORTEGA  ---------------
LIPOS_EORTEGA<-readxl::read_excel(here::here(dir_lipos,"LiposcaleResultsOrtegaE002.xls"),skip = 6)
LIPOS_EORTEGA

## 2.2.1. Netejar noms de variables ---------
LIPOS_EORTEGA<-netejar.noms.variables(LIPOS_EORTEGA)

# 2.3.  Llegeixo MAURICIO 3 T1D   -----------------------------
LIPOS_T1DDEBUTS<-readxl::read_excel(here::here(dir_lipos,"LiposcaleResultsMauricioD003.xls"),skip = 6)
# 2.3.1. Netejar noms de variables  -------------------
LIPOS_T1DDEBUTS<-netejar.noms.variables(LIPOS_T1DDEBUTS)

# RENOMBRAR Sample_id
LIPOS_T1DDEBUTS<-LIPOS_T1DDEBUTS %>% rename(Sample_ID=Sample_ID_BT)

# 2.4. Llegir Mauricio 1  ------------------

LIPOS_MOLLERUSSA<-readxl::read_excel(here::here(dir_lipos,"LiposcaleResultsMauricioD001.xls"),skip = 6)
## 2.4.1.  Netejar noms  --------------------
LIPOS_MOLLERUSSA<-netejar.noms.variables(LIPOS_MOLLERUSSA)

##  3. Fusionar totes les taules LIPOS   ---------------------
LIPOS_MAURICIO<-LIPOS_MAURICIO %>% select(-ID)
LIPOS_TOTAL<-dplyr::bind_rows(LIPOS_MAURICIO,LIPOS_EORTEGA,LIPOS_T1DDEBUTS,LIPOS_MOLLERUSSA)

### Seleccionar registres unics en LIPOS TOTAL
LIPOS_UNICS<-LIPOS_TOTAL %>% group_by(Sample_ID) %>%  slice(1) %>% ungroup 

# trec espais en ID
LIPOS_UNICS$Sample_ID<-trimws(LIPOS_UNICS$Sample_ID)
dades$Sample_ID<-trimws(dades$Sample_ID)

# 4. FUSIONAR AMB dades substituint dades antigues    ------------------

dades<-dades %>% select(1:55) %>% left_join(LIPOS_UNICS,by="Sample_ID")

## 6. Salvar dades  ----------------------

saveRDS(dades,here::here("dades","BD_LIPOS_N1217_lipos.rds"))

### Save en STATA       ---------------



write.dta(dades, here::here("dades","BD_LIPOS_N1217_lipos.dta"))

###################################################################################





