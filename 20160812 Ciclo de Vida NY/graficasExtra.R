#defino el directorio de trabajo hacia la carpeta que contiene a este archivo


#librerías------------------------------------------------------------------------------------------------

library(dplyr)

#cargo y junto las bases-----------------------------------------------------------------------------

datos<-readRDS('datos.rds')
datosB<-readRDS('baterias.rds')

datos$cluster<-datosB$clusterB

bateria<-datos %>%
  dplyr::select(contains('P8'),contains('P9'),contains('P10'),contains('cluster')) %>%
  filter(!is.na(cluster)) %>%
  data.frame

# bateria$cluster<-as.factor(bateria$cluster)

#saco una muestra balanceada------------------------------------------------------------------------------------

muestra<-bateria[0,]
for(i in 1:max(bateria$cluster)){
  temporal<-bateria %>%
    filter(cluster==i)
  muestraN<-sample_n(replace=T,tbl=temporal,size=100)
  muestra<-rbind(muestra,muestraN)
}

muestra$cluster<-as.factor(muestra$cluster)

#checo la importancia de las variables en la asignación de cluster------------------------------------------------

library(randomForest)

bosque<-randomForest(data=muestra,
                     x=muestra[,-51],
                     y=muestra[,51],
                     ntree=10000,do.trace = 100,importance = T)
bosque
varImpPlot(bosque)

importancias<-bosque$importance[,'MeanDecreaseGini']

importancias<-as.data.frame(importancias)
importancias$variable<-rownames(importancias)
# importancias<-importancias %>% arrange(-importancias)
# importancias

variablesOrdenadas<-match(importancias$variable,names(muestra))

bosque2<-randomForest(data=muestra,
                     x=muestra[,variablesOrdenadas[1:10]],
                     y=muestra[,51],
                     ntree=10000,do.trace = 100,importance = T)
bosque2
varImpPlot(bosque2,type=2)

#me quedo con la importancia de todo-----------------------------------------------------------------
#creo la matriz para graficar la cosa linda

resultado<-bateria %>%
  mutate_each(funs(as.numeric)) %>%
  group_by(cluster) %>%
  summarise_each(funs(mean)) %>%
  as.data.frame()
  
resultado<-t(resultado)
resultado<-resultado[-1,]
resultado<-as.data.frame(resultado)
names(resultado)<-1:6
resultado$importancias<-importancias$importancias

resultado

#grafico esa cosa----------------------------------------------------------------------------------------

