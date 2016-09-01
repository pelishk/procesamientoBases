library(Hmisc)
library(dplyr)
library(poLCA)

#datos--------------------------------------------------------------------
# datos<-spss.get('Ciclo de Vida 12-08-2016 15-26-18.sav')
datos<-haven::read_spss('Ciclo de Vida 12-08-2016 15-26-18.sav')
datos<-datos%>%data.frame

bateria<-datos %>%
  dplyr::select(contains('P8'),contains('P9'),contains('P10')) %>%
  data.frame



#reordeno niveles en P9
bateria[,24]<-haven::as_factor(bateria[,24])
bateria[,25]<-haven::as_factor(bateria[,25])
bateria[,26]<-haven::as_factor(bateria[,26])

#convierto P8 y P10 en ordinales
for(i in c(1:23,27:50)){
  bateria[,i]<-haven::as_factor(bateria[,i])  
  bateria[,i]<-factor(bateria[,i],ordered = T)
}
str(bateria)
#nombres de las variables
nombres<-c(
  'sinPresiones',
  'realizaLoQueDesea',
  'comienzaResponsabilidades',
  'necesitaTrabajo',
  'tiempoSuficiente',
  'trabajaPorFuturo',
  'necesitaNegocio',
  'vidaSencilla',
  'facilConseguirTrabajo',
  'ingresosInvierteHijos',
  'realizado',
  'ingresoEstable',
  'deseaViajar',
  'necesitaAuto',
  'ahorraConstantemente',
  'necesitaHipoteca',
  'necesitaCreditoEmergencias',
  'ahorraFuturoHijos',
  'prefiereTarjeta',
  'comodoServiciosBanco',
  'ahorraCompras',
  'prefiereRecibirCash',
  'contribuyeHogar',
  'apoyaPadres',
  'vivePadres',
  'dependePadres',
  'preocupaVejez',
  'ahorraFiesta',
  'gastaFashion',
  'gastaGeek',
  'gastaBasicos',
  'ahorraDie',
  'pedirEnfermedades',
  'pagaPrestamos',
  'ahorraEnfermedades',
  'estudiosNecesarios',
  'ahorraNot',
  'creditoFacil',
  'necesitaAmueblar',
  'trabajoVsEstudio',
  'ahorraNewPlayer',
  'seguroInesperado',
  'finanzasTranquilo',
  'gustaPlazos',
  'invierteSolo',
  'necesidadesSinBanco',
  'valoraExpertos',
  'comparaTasas',
  'totalero',
  'minimero'
)

names(bateria)<-nombres

#las dimensiones son las siguientes:
indices<-list(
  indiceInformacion=c(
    'minimero',
    'comparaTasas',
    'valoraExpertos',
    'gustaPlazos'
  ),
  indiceProBanco=c(
    'comodoServiciosBanco',
    'prefiereTarjeta'
  ),
  indiceGastalon=c(
    'necesitaAuto',
    'deseaViajar',
    'apoyaPadres',
    'gastaGeek',
    'gastaFashion'
  ),
  indiceLogro=c(
    'ingresoEstable',
    'realizado',
    'tiempoSuficiente',
    'realizaLoQueDesea',
    'sinPresiones',
    'ahorraCompras',
    'ahorraConstantemente'
  ),
  indiceTranquilidadFinanciera=c(
    'preocupaVejez',
    'ahorraEnfermedades',
    'ahorraDie',
    'totalero',
    'finanzasTranquilo',
    'creditoFacil',
    'facilConseguirTrabajo',
    'vidaSencilla',
    'ahorraFiesta'
  ),
  indiceAntiBanco=c(
    'necesidadesSinBanco',
    'invierteSolo',
    'prefiereRecibirCash'
  ),
  indiceInicio=c(
    'trabajaPorFuturo',
    'comienzaResponsabilidades',
    'gastaBasicos',
    'contribuyeHogar'
  ),
  indiceCreditoEmergencias=c(
    'seguroInesperado',
    'necesitaCreditoEmergencias'
  ),
  indiceInsolvencia=c(
    'necesitaAmueblar',
    'necesitaHipoteca',
    'necesitaNegocio',
    'necesitaTrabajo',
    'ahorraNewPlayer',
    'trabajoVsEstudio',
    'ahorraNot',
    'pagaPrestamos',
    'pedirEnfermedades'
  ),
  indiceFamilia=c(
    'ahorraFuturoHijos',
    'ingresosInvierteHijos'
  ),
  indiceNini=c(
    'dependePadres',
    'vivePadres'
  )
)




#checo que los nombres de las dimensiones sean correctos
for(i in 1:length(indices)){
  if(!all(indices[[i]] %in% names(bateria))){
    stop(
      paste("Dude...",indices[[i]][!indices[[i]] %in% names(bateria)])
    )
  }
}

#comenzamos los clusters con clases latentes----------------------------------
#para que la interpretación sea más sencilla ponemos las variables
#en el orden de la lista "índices"
variables<-c(unlist(indices))

names(datos)[1:100]
variables2<-variables
variables<- c(variables,c("NSE","Edad_Rango","A4","A6","A7","A3"))

for(i in 1:length(c("NSE","Edad_Rango","A4","A6","A7","A3"))){
  datos[,c("NSE","Edad_Rango","A4","A6","A7","A3")[i]]<-haven::as_factor(datos[,c("NSE","Edad_Rango","A4","A6","A7","A3")[i]])
}

#reordeno niveles en P9

bateria<-cbind(bateria,datos[,c("NSE","Edad_Rango","A4","A6", "A7","A3")])
str(bateria)
bateria$NSE <- ordered(bateria$NSE, levels=rev(levels(bateria$NSE)))
str(bateria)


table(bateria$dependePadres)
# Modifico A4
bateria$Pareja<- 0
bateria[bateria$A4=="Casado(a)" | bateria$A4=="Unión libre","Pareja"]<- 2
bateria[bateria$A4=="Soltero(a)","Pareja"]<- 1
bateria[bateria$A4=="Divorciado(a)" | bateria$A4=="Viudo(a)","Pareja"]<- 3
bateria$Pareja<- factor(bateria$Pareja, labels=c("Soltero", "Casado/UnionLibre","Divorciado/Viudo"))
bateria <- bateria[,-53]
bateria <- bateria[,-53]


bateria$A7<-as.numeric(as.character(bateria$A7))

bateria$Hijos<- 0
bateria$A7[is.na(bateria$A7)]<-0
bateria[bateria$A7==1 | bateria$A7==2,"Hijos"]<- 1
bateria[bateria$A7>2,"Hijos"]<- 2
bateria<- bateria[,-53]
bateria$Hijos<-as.factor(bateria$Hijos)
# bateriaCluster<-bateria[,match(c(variables,"NSE","Edad.Rango","A4","A6"),names(bateria))]



##########################################################################################################3
##########################################################################################################3
##########################################################################################################3
##########################################################################################################3
##########################################################################################################3
##########################################################################################################3
##########################################################################################################3





restar<-c("dependePadres", "vivePadres","estudiosNecesarios","prefiereRecibirCash","ahorraNewPlayer")
bateria <- cbind(bateria, datos$ResponseID)
# Segmento6Total
bateriaCluster<-bateria[,-match(restar,names(bateria))]
misId <- bateriaCluster$`datos$ResponseID`
names(bateriaCluster)
bateriaCluster<-bateriaCluster[,-match(c("A3","datos$ResponseID"),names(bateriaCluster))]

bateriaCluster<-bateriaCluster %>%
  mutate_each(funs(as.numeric))

variables<-names(bateriaCluster)
#corro los diferentes modelos
eval(parse(text=paste(
  'f=cbind(',paste(variables,collapse=','),')~1'
  ,sep='')))
set.seed(38457)
fit6<-poLCA(bateriaCluster,formula = f,nclass = 6,nrep = 50)
bateriaCluster$cluster6 <- as.numeric(fit6$predclass)
bateriaCluster$ResponseID <- misId

datos$cluster <- bateriaCluster$cluster6[match(datos$ResponseID,bateriaCluster$ResponseID)]

table(datos$cluster, datos$A3)

pdf('6 clusters.pdf',height=12,width=100)
plot(fit6)
dev.off()


# Segmento6Bancarizados

bateriaCluster<-bateria[bateria$A3=="Sí (Bancarizado)",-match(restar,names(bateria))]
misId <- bateriaCluster$`datos$ResponseID`
names(bateriaCluster)
bateriaCluster<-bateriaCluster[,-match(c("A3","datos$ResponseID"),names(bateriaCluster))]

bateriaCluster<-bateriaCluster %>%
  mutate_each(funs(as.numeric))

variables<-names(bateriaCluster)
#corro los diferentes modelos
eval(parse(text=paste(
  'f=cbind(',paste(variables,collapse=','),')~1'
  ,sep='')))
set.seed(38457)
fit6B<-poLCA(bateriaCluster,formula = f,nclass = 6,nrep = 50)
bateriaCluster$cluster6B <- as.numeric(fit6B$predclass)
bateriaCluster$ResponseID <- misId


datos$clusterB <- bateriaCluster$cluster6B[match(datos$ResponseID,bateriaCluster$ResponseID)]

table(datos$clusterB, datos$A3)
table(datos$clusterB, datos$cluster)

pdf('6 clustersB.pdf',height=12,width=100)
plot(fit6B)
dev.off()

sum(table(datos$clusterB))


write.csv(
  datos[,c("ResponseID","cluster","clusterB")],
  "baseFii.csv"
)

haven::write_sav(
  datos,
  "baseFINAL2.sav"
)
