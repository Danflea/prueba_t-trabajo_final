####CALCULO DE ERRORES ESTANDAR 2S ####
rm(list=ls())
setwd('C:\Users\frusc\Desktop\Análisis Estadistico - Danilo\Trabajos_encargados\ENLA muestral 2S 2023')

library(foreign)
library(sampling)
library(survey)
library(writexl)
library(dplyr)
library(readxl)

bd<-read_excel('BD_2S ENLA muestral 2023.xlsx')

###################### Lectura ##########################################
bd_L<-bd %>% filter(!is.na(grupo_EM_2S_2023_CT)==1)
bd_L<-bd_L %>% select(ID_IE,sexo,gestion2,area,grupo_EM_2S_2023_CT,M500_EM_2S_2023_CT,peso_CT,nom_dre,Estrato_DRE)
bd_L$grupo_L<-as.factor(bd_L$grupo_EM_2S_2023_CT)

options(survey.lonely.psu='certainty')

m<-data.frame(cbind(curso=0,est=0,est1=0,grupo_L1=0,grupo_L2=0,grupo_L3=0,grupo_L4=0,M500_L=0,se.grupo_L1=0,
                    se.grupo_L2=0,se.grupo_L3=0,se.grupo_L4=0,se.M500_L=0))

dis_L<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_CT,data=bd_L,pps='brewer')
res<-data.frame(svymean(~grupo_EM_2S_2023_CT+M500_EM_2S_2023_CT,design=dis_L,na.rm=T))


m1<-cbind('Lectura','Nacional','General',res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])

colnames(m1)<-c('curso','est','est1','grupo_L1','grupo_L2','grupo_L3','grupo_L4','M500_L','se.grupo_L1',
                'se.grupo_L2','se.grupo_L3','se.grupo_L4','se.M500_L')
m<-rbind(m,m1)

### Sexo
res<-data.frame(svyby(~grupo_EM_2S_2023_CT+M500_EM_2S_2023_CT,~sexo,design=dis_L,svymean))
m1<-cbind(rep('Lectura',2),rep('sexo',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_L1','grupo_L2','grupo_L3','grupo_L4','M500_L','se.grupo_L1',
                'se.grupo_L2','se.grupo_L3','se.grupo_L4','se.M500_L')
m<-rbind(m,m1)

### Area
res<-data.frame(svyby(~grupo_EM_2S_2023_CT+M500_EM_2S_2023_CT,~area,design=dis_L,svymean))
m1<-cbind(rep('Lectura',2),rep('area',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_L1','grupo_L2','grupo_L3','grupo_L4','M500_L','se.grupo_L1',
                'se.grupo_L2','se.grupo_L3','se.grupo_L4','se.M500_L')
m<-rbind(m,m1)


### Gestion
res<-data.frame(svyby(~grupo_EM_2S_2023_CT+M500_EM_2S_2023_CT,~gestion2,design=dis_L,svymean))
m1<-cbind(rep('Lectura',2),rep('gestion',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_L1','grupo_L2','grupo_L3','grupo_L4','M500_L','se.grupo_L1',
                'se.grupo_L2','se.grupo_L3','se.grupo_L4','se.M500_L')
m<-rbind(m,m1)


### DRE
dre<-unique(bd_L$nom_dre)
dre<-dre[order(dre)]
for(i in 1:length(dre)){
  sub<-bd_L[bd_L$nom_dre==dre[i],]
  dis_L<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_CT,data=sub,pps='brewer')
  res<-data.frame(svymean(~grupo_EM_2S_2023_CT+M500_EM_2S_2023_CT,design=dis_L,na.rm=T))
  m1<-cbind('Lectura','DRE',dre[i],res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])
  colnames(m1)<-c('curso','est','est1','grupo_L1','grupo_L2','grupo_L3','grupo_L4','M500_L','se.grupo_L1',
                  'se.grupo_L2','se.grupo_L3','se.grupo_L4','se.M500_L')
  m<-rbind(m,m1)
}

m<-m %>% select(curso,est,est1,grupo_L1,se.grupo_L1,grupo_L2,se.grupo_L2,grupo_L3,se.grupo_L3,
                grupo_L4,se.grupo_L4,M500_L,se.M500_L)

colnames(m)<-c('Curso','Estrato','Estrato1','Previo al inicio','ee_Previo al inicio','En inicio','ee_En inicio',
               'En proceso','ee_En proceso','Satisfactorio','ee_Satisfactorio','Medida promedio','ee_Medida promedio')

write_xlsx(m[-1,],'EM_2S_L_2023.xlsx')


###################### Matematica ##########################################
bd_M<-bd %>% filter(!is.na(grupo_EM_2S_2023_MA)==1)
bd_M<-bd_M %>% select(ID_IE,sexo,gestion2,area,grupo_EM_2S_2023_MA,M500_EM_2S_2023_MA,peso_MA,nom_dre,Estrato_DRE)
bd_M$grupo_M<-as.factor(bd_M$grupo_EM_2S_2023_MA)

options(survey.lonely.psu='certainty')

m<-data.frame(cbind(curso=0,est=0,est1=0,grupo_M1=0,grupo_M2=0,grupo_M3=0,grupo_M4=0,M500_M=0,se.grupo_M1=0,
                    se.grupo_M2=0,se.grupo_M3=0,se.grupo_M4=0,se.M500_M=0))

dis_M<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_MA,data=bd_M,pps='brewer')
res<-data.frame(svymean(~grupo_EM_2S_2023_MA+M500_EM_2S_2023_MA,design=dis_M,na.rm=T))


m1<-cbind('Matematica','Nacional','General',res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])

colnames(m1)<-c('curso','est','est1','grupo_M1','grupo_M2','grupo_M3','grupo_M4','M500_M','se.grupo_M1',
                'se.grupo_M2','se.grupo_M3','se.grupo_M4','se.M500_M')
m<-rbind(m,m1)

### Sexo
res<-data.frame(svyby(~grupo_EM_2S_2023_MA+M500_EM_2S_2023_MA,~sexo,design=dis_M,svymean))
m1<-cbind(rep('Matematica',2),rep('sexo',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_M1','grupo_M2','grupo_M3','grupo_M4','M500_M','se.grupo_M1',
                'se.grupo_M2','se.grupo_M3','se.grupo_M4','se.M500_M')
m<-rbind(m,m1)

### Area
res<-data.frame(svyby(~grupo_EM_2S_2023_MA+M500_EM_2S_2023_MA,~area,design=dis_M,svymean))
m1<-cbind(rep('Matematica',2),rep('area',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_M1','grupo_M2','grupo_M3','grupo_M4','M500_M','se.grupo_M1',
                'se.grupo_M2','se.grupo_M3','se.grupo_M4','se.M500_M')
m<-rbind(m,m1)


### Gestion
res<-data.frame(svyby(~grupo_EM_2S_2023_MA+M500_EM_2S_2023_MA,~gestion2,design=dis_M,svymean))
m1<-cbind(rep('Matematica',2),rep('gestion',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_M1','grupo_M2','grupo_M3','grupo_M4','M500_M','se.grupo_M1',
                'se.grupo_M2','se.grupo_M3','se.grupo_M4','se.M500_M')
m<-rbind(m,m1)


### DRE
dre<-unique(bd_M$nom_dre)
dre<-dre[order(dre)]
for(i in 1:length(dre)){
  sub<-bd_M[bd_M$nom_dre==dre[i],]
  dis_M<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_MA,data=sub,pps='brewer')
  res<-data.frame(svymean(~grupo_EM_2S_2023_MA+M500_EM_2S_2023_MA,design=dis_M,na.rm=T))
  m1<-cbind('Matematica','DRE',dre[i],res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])
  colnames(m1)<-c('curso','est','est1','grupo_M1','grupo_M2','grupo_M3','grupo_M4','M500_M','se.grupo_M1',
                  'se.grupo_M2','se.grupo_M3','se.grupo_M4','se.M500_M')
  m<-rbind(m,m1)
}

m<-m %>% select(curso,est,est1,grupo_M1,se.grupo_M1,grupo_M2,se.grupo_M2,grupo_M3,se.grupo_M3,
                grupo_M4,se.grupo_M4,M500_M,se.M500_M)

colnames(m)<-c('Curso','Estrato','Estrato1','Previo al inicio','ee_Previo al inicio','En inicio','ee_En inicio',
               'En proceso','ee_En proceso','Satisfactorio','ee_Satisfactorio','Medida promedio','ee_Medida promedio')

write_xlsx(m[-1,],'EM_2S_M_2023.xlsx')


###################### Ciencias sociales ##########################################

bd_CS<-bd %>% filter(!is.na(grupo_EM_2S_2023_CS)==1)
bd_CS<-bd_CS %>% select(ID_IE,sexo,gestion2,area,grupo_EM_2S_2023_CS,M500_EM_2S_2023_CS,peso_CS,nom_dre,Estrato_DRE)
bd_CS$grupo_CS<-as.factor(bd_CS$grupo_EM_2S_2023_CS)

options(survey.lonely.psu='certainty')

m<-data.frame(cbind(curso=0,est=0,est1=0,grupo_CS1=0,grupo_CS2=0,grupo_CS3=0,grupo_CS4=0,M500_CS=0,se.grupo_CS1=0,
                    se.grupo_CS2=0,se.grupo_CS3=0,se.grupo_CS4=0,se.M500_CS=0))

dis_CS<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_CS,data=bd_CS,pps='brewer')
res<-data.frame(svymean(~grupo_EM_2S_2023_CS+M500_EM_2S_2023_CS,design=dis_CS,na.rm=T))


m1<-cbind('Ciencias sociales','Nacional','General',res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])

colnames(m1)<-c('curso','est','est1','grupo_CS1','grupo_CS2','grupo_CS3','grupo_CS4','M500_CS','se.grupo_CS1',
                'se.grupo_CS2','se.grupo_CS3','se.grupo_CS4','se.M500_CS')
m<-rbind(m,m1)

### Sexo
res<-data.frame(svyby(~grupo_EM_2S_2023_CS+M500_EM_2S_2023_CS,~sexo,design=dis_CS,svymean))
m1<-cbind(rep('Ciencias sociales',2),rep('sexo',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_CS1','grupo_CS2','grupo_CS3','grupo_CS4','M500_CS','se.grupo_CS1',
                'se.grupo_CS2','se.grupo_CS3','se.grupo_CS4','se.M500_CS')
m<-rbind(m,m1)

### Area
res<-data.frame(svyby(~grupo_EM_2S_2023_CS+M500_EM_2S_2023_CS,~area,design=dis_CS,svymean))
m1<-cbind(rep('Ciencias sociales',2),rep('area',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_CS1','grupo_CS2','grupo_CS3','grupo_CS4','M500_CS','se.grupo_CS1',
                'se.grupo_CS2','se.grupo_CS3','se.grupo_CS4','se.M500_CS')
m<-rbind(m,m1)


### Gestion
res<-data.frame(svyby(~grupo_EM_2S_2023_CS+M500_EM_2S_2023_CS,~gestion2,design=dis_CS,svymean))
m1<-cbind(rep('Ciencias sociales',2),rep('gestion',2),res[,1],res[,4]*100,res[,c(2,3,5)]*100,res[,6],res[,9]*100,res[,c(7,8,10)]*100,res[,11])
colnames(m1)<-c('curso','est','est1','grupo_CS1','grupo_CS2','grupo_CS3','grupo_CS4','M500_CS','se.grupo_CS1',
                'se.grupo_CS2','se.grupo_CS3','se.grupo_CS4','se.M500_CS')
m<-rbind(m,m1)


### DRE
dre<-unique(bd_CS$nom_dre)
dre<-dre[order(dre)]
for(i in 1:length(dre)){
  sub<-bd_CS[bd_CS$nom_dre==dre[i],]
  dis_CS<-svydesign(id=~ID_IE,strata=~Estrato_DRE,nest=TRUE,weights=~peso_CS,data=sub,pps='brewer')
  res<-data.frame(svymean(~grupo_EM_2S_2023_CS+M500_EM_2S_2023_CS,design=dis_CS,na.rm=T))
  m1<-cbind('Ciencias sociales','DRE',dre[i],res[3,1]*100,res[1,1]*100,res[2,1]*100,res[4,1]*100,res[5,1],res[3,2]*100,res[1,2]*100,res[2,2]*100,res[4,2]*100,res[5,2])
  colnames(m1)<-c('curso','est','est1','grupo_CS1','grupo_CS2','grupo_CS3','grupo_CS4','M500_CS','se.grupo_CS1',
                  'se.grupo_CS2','se.grupo_CS3','se.grupo_CS4','se.M500_CS')
  m<-rbind(m,m1)
}

m<-m %>% select(curso,est,est1,grupo_CS1,se.grupo_CS1,grupo_CS2,se.grupo_CS2,grupo_CS3,se.grupo_CS3,
                grupo_CS4,se.grupo_CS4,M500_CS,se.M500_CS)

colnames(m)<-c('Curso','Estrato','Estrato1','Previo al inicio','ee_Previo al inicio','En inicio','ee_En inicio',
               'En proceso','ee_En proceso','Satisfactorio','ee_Satisfactorio','Medida promedio','ee_Medida promedio')

write_xlsx(m[-1,],'EM_2S_CS_2023.xlsx')