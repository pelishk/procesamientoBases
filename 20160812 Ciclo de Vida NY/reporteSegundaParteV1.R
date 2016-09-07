

#### PRIMERO CORRER LAS FUNCIONES EN FRECUENTATOR

# 
library(brostatistics)


datos<- readRDS(
  "datos.rds"
)

baterias<- readRDS(
  "baterias.rds"
)



datos <- cbind(datos,data.frame(cluster=baterias$cluster))
datos <- cbind(datos,data.frame(clusterB=baterias$clusterB))

datos$cluster <- factor(datos$cluster)
datos$clusterB <- factor(datos$clusterB)

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
datos$Total<- factor(rep(x = 1,nrow(datos)),levels = 1,labels = "Total")
datos$NSE<-factor(datos$NSE)
#############

tiempoCasados<-as.numeric(datos$A5.MESES)
tiempoCasados<- tiempoCasados/12
tiempoCasados<-rowSums(cbind(tiempoCasados,as.numeric(datos$A5)),na.rm = T)
tiempoCasados[is.na(datos$A5.MESES) & is.na(datos$A5)]<-NA
tiempoCasados<- round(tiempoCasados,0)
cortes<-list(
  "4 años o menos"= c(0:4),
  "De 5 a 9 años"= c(5:9),
  "De 10 a 14 años"= c(10:14),
  "De 15 a 19 años"= c(15:19),
  "20 años o más"= c(20:200)
)
datos$A5tiempoJuntos<-NA
for(i in 1:length(cortes)){
  misCN<- names(cortes)[i]
  misCCn<- cortes[[i]]
  datos[tiempoCasados %in% misCCn,"A5tiempoJuntos"]<-i
}
datos$A5tiempoJuntos<- factor(datos$A5tiempoJuntos, labels = names(cortes))
#############


hijos<-as.numeric(as.character(datos$A7))
cortes<-list(
  "Uno"= c(1),
  "Dos"= c(2),
  "Tres"= c(3),
  "Cuatro o más"= c(4:100)
)
datos$A7Hijos<-NA
for(i in 1:length(cortes)){
  # i<-3
  misCN<- names(cortes)[i]
  misCCn<- cortes[[i]]
  datos[hijos %in% misCCn,"A7Hijos"]<-i
}
datos$A7Hijos<- factor(datos$A7Hijos, labels = names(cortes))
#############

hijos<-as.numeric(as.character(datos$A8))
cortes<-list(
  "Uno"= c(1),
  "Dos"= c(2),
  "Tres"= c(3),
  "Cuatro o más"= c(4:100)
)
datos$A8Hijos<-NA
for(i in 1:length(cortes)){
  # i<-3
  misCN<- names(cortes)[i]
  misCCn<- cortes[[i]]
  datos[hijos %in% misCCn,"A8Hijos"]<-i
}
datos$A8Hijos<- factor(datos$A8Hijos, labels = names(cortes))
#############


#############










################# Hay que juntar la edad de todos los hijos en una sola variable de edad :)




hijitos<-list(
  "A9Primero"=c("A9.Hijo.1.a.os","A9.Hijo.1.ameses"),
  "A9Segundo"=c("A9.Hijo.2.a.os","A9.Hijo.2.meses"),
  "A9Tercero"=c("A9.Hijo.3.a.os","A9.Hijo.3.meses"),
  "A9Cuarto"=c("A9.Hijo.4.a.os","A9.Hijo.4.meses"),
  "A9Quinto"=c("A9.Hijo.5.a.os","A9.Hijo.5.meses"),
  "A9Sexto"=c("A9.Hijo.6.a.os","A9.Hijo.6.meses")
)

for(i in 1:length(hijitos)){
  # i <- 1
  hijitosSub <- hijitos[[i]]
  nhijitosSub <- names(hijitos)[i]
  edadhijo<-as.numeric(datos[,hijitosSub[2]])
  edadhijo<- edadhijo/12
  edadhijo<-rowSums(cbind(edadhijo,as.numeric(datos[,hijitosSub[1]])),na.rm = T)
  edadhijo[is.na(datos[,hijitosSub[2]]) & is.na(datos[,hijitosSub[1]])]<-NA
  edadhijo<- round(edadhijo,0)
  cortes<-list(
    "menos de 4 años"= c(0:4),
    "entre 5 y 9 años"= c(5:9),
    "entre 10 y 14 años"= c(10:14),
    "más de 15 años"= c(15:200)
  )
  datos[,nhijitosSub]<-NA
  for(i in 1:length(cortes)){
    misCN<- names(cortes)[i]
    misCCn<- cortes[[i]]
    datos[edadhijo %in% misCCn,nhijitosSub]<-i
  }
  datos[,nhijitosSub]<- factor(datos[,nhijitosSub], levels = 1:4,labels = names(cortes))
}
hijitosEdad <- c("A9Primero","A9Segundo","A9Tercero","A9Cuarto","A9Quinto","A9Sexto")

######################## P1

misP1Top <- nombresR(datos,"P1")[1:14]
misP1OM <- nombresR(datos,"P1")[20:33]

fullP <- data.frame(misP1Top,misP1OM,stringsAsFactors = F)

listaNombres<-list()
for(i in 1:nrow(fullP)){
  yoSoy<- fullP[i,1]
  yoSoy<- paste(yoSoy, "TopShare",sep="")
  listaNombres[[i]]<-paste(fullP[i,])
  names(listaNombres)[[i]]<-yoSoy
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-"FALSE"
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]=="TRUE"
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-"TRUE"
  }
}

misP1TopShare <- names(listaNombres)
table(datos$P1.BanamexTopShare)

######################## P2

misP2 <- nombresR(datos,"P2")[1:14]
fullP <- data.frame(misP1Top,misP1OM,misP2,stringsAsFactors = F)
listaNombres<-list()
for(i in 1:nrow(fullP)){
  yoSoy<- fullP[i,1]
  yoSoy<- paste(yoSoy, "ConocimientoTotal",sep="")
  listaNombres[[i]]<-paste(fullP[i,])
  names(listaNombres)[[i]]<-yoSoy
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-"FALSE"
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]=="TRUE"
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-"TRUE"
  }
}

miConocimientoTotal <- names(listaNombres)
table(datos$P1.BanamexConocimientoTotal)

######################## P2.5
nombresR(xx = datos,yy = "P2.5.")

misP2.5 <- data.frame(
  medio =nombresR(xx = datos,yy = "P2.4")[1:19],
  bancoAzteca=  nombresR(xx = datos,yy = "P2.5.Banco.Azteca"),
  banamex=  nombresR(xx = datos,yy = "P2.5.Banamex"),
  banorte=nombresR(xx = datos,yy = "P2.5.Banorte"),
  bancomer=nombresR(xx = datos,yy = "P2.5.Bancomer"),
  hsbc=nombresR(xx = datos,yy = "P2.5.HSBC"),
  santander=nombresR(xx = datos,yy = "P2.5.Santander"),
  scotiabank=nombresR(xx = datos,yy = "P2.5.Scotiabank"),
  coppel=nombresR(xx = datos,yy = "P2.5.Coppel"),
  otro=nombresR(xx = datos,yy = "P2.5.Otro..Especificar"),
  stringsAsFactors = F
)

######################## P4
nombresR(xx = datos,yy = "P4")

misP4 <- data.frame(
  medio =nombresR(xx = datos,yy = "P3")[1:19],
  bancoAzteca=  nombresR(xx = datos,yy = "P4.Banco.Azteca"),
  banamex=  nombresR(xx = datos,yy = "P4.Banamex"),
  banorte=nombresR(xx = datos,yy = "P4.Banorte"),
  bancomer=nombresR(xx = datos,yy = "P4.Bancomer"),
  hsbc=nombresR(xx = datos,yy = "P4.HSBC"),
  santander=nombresR(xx = datos,yy = "P4.Santander"),
  scotiabank=nombresR(xx = datos,yy = "P4.Scotiabank"),
  coppel=nombresR(xx = datos,yy = "P4.Coppel"),
  otro=nombresR(xx = datos,yy = "P4.Otro..Especificar"),
  stringsAsFactors = F
)

# matrix(data = misP2.5,byrow = F,nrow = 10)
######################## P6
nombresR(xx = datos,yy = "P6.Servicio")

misP6 <- data.frame(
  medio =nombresR(xx = datos,yy = "P3")[1:19],
  bancoAzteca=  nombresR(xx = datos,yy = "P6.Servicio.BancoAzteca")[seq(1,38,by = 2)],
  banamex=  nombresR(xx = datos,yy = "P6.Servicio.Banamex")[seq(1,38,by = 2)],
  banorte=nombresR(xx = datos,yy = "P6.Servicio.Banorte")[seq(1,38,by = 2)],
  bancomer=nombresR(xx = datos,yy = "P6.Servicio.Bancomer")[seq(1,38,by = 2)],
  hsbc=nombresR(xx = datos,yy = "P6.Servicio.Hsbc"),
  santander=nombresR(xx = datos,yy = "P6.Servicio.Santander")[seq(1,38,by = 2)],
  scotiabank=nombresR(xx = datos,yy = "P6.Servicio.Scotiabank")[seq(1,38,by = 2)],
  coppel=nombresR(xx = datos,yy = "P6.Servicio.Coppel")[seq(1,38,by = 2)],
  otro=nombresR(xx = datos,yy = "P6.Servicio.OTRO"),
  stringsAsFactors = F
)


######################## P6
nombresR(xx = datos,yy = "P6A")

misP6A <- data.frame(
  bancoAzteca=  nombresR(xx = datos,yy = "P6A.BancoAzteca")[seq(1,38,by = 2)],
  banamex=  nombresR(xx = datos,yy = "P6A.Banamex"),
  banorte=nombresR(xx = datos,yy = "P6A.Banorte")[seq(1,38,by = 2)],
  bancomer=nombresR(xx = datos,yy = "P6A.Bancomer")[seq(1,38,by = 2)],
  hsbc=nombresR(xx = datos,yy = "P6A.Hsbc"),
  santander=nombresR(xx = datos,yy = "P6A.Santander")[seq(1,38,by = 2)],
  scotiabank=nombresR(xx = datos,yy = "P6A.Scotiabank")[seq(1,38,by = 2)],
  coppel=nombresR(xx = datos,yy = "P6A.Coppel")[seq(1,38,by = 2)],
  otro=nombresR(xx = datos,yy = "P6A.otro"),
  stringsAsFactors = F
)

######################## P7

### Quienes son usuarios de Azteca? Debo eliminar a los inconsistentes...


bancoAztecaUsers<-misP4$bancoAzteca

datos[,"BazUser"]<-FALSE
for(i in 1:length(bancoAztecaUsers)){
  # i<-1
  hijito<- bancoAztecaUsers[i]
  logico<- datos[,hijito]=="TRUE"
  logico[is.na(logico)]<-FALSE
  datos[logico,"BazUser"]<-TRUE
}

#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################

# Voy a eliminar los casos inconsistentes de datos...

# Inconsistentes

bancarizados<- datos[datos$A3=="Sí (Bancarizado)",c("ResponseID","A3","BazUser","P7.3.Filtro")]
bancarizados$P7.3.FiltroMirror <- bancarizados$P7.3.Filtro
bancarizados$P7.3.FiltroMirror[is.na(bancarizados$P7.3.FiltroMirror)] <- "No"

# Son bancarizados, usaron banco azteca pero no está indicado
bancarizados$kill <- (bancarizados$BazUser==T & bancarizados$P7.3.FiltroMirror=="No")

idParaBorrar <- bancarizados$ResponseID[bancarizados$kill]  

datos <- datos[!datos$ResponseID %in% idParaBorrar,]




#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################



##########################################################################################


bandera1<-c("Total","A3","F1Genero", "NSE","Edad.Rango","Plaza","cluster", "clusterB")
bandera2<-c("Total","P2.4.Tarjeta.de.n.mina","P2.4.Tarjeta.de.d.bito...Ahorro...C","P2.4.Tarjeta.de.cr.dito","P2.4.Tarjeta.Azteca","P2.4.Cuenta.de.ahorro","P2.4.Cuenta..de.cheques","P2.4.Inversiones","P2.4.Seguros","P2.4.Pr.stamo.Personal","P2.4.Cr.dito.en.tienda.para.compra.","P2.4.Cr.dito.Hipotecario","P2.4.Cr.dito.Automotriz","P2.4.Cr.dito.para.desarrollo.de.Neg","P2.4.Cr.dito.Prendario..Empe.o","P2.4.Cr.dito.Grupal","P2.4.Pr.stamo.de.nomina","P2.4.Otro.cr.dito.o.pr.stamo.por.pa","P2.4.Tarjeta.departamental","P2.4.Cr.dito.de.motocicleta")
bandera3<-c("Total","P2.4.Tarjeta.de.n.mina","P2.4.Tarjeta.de.d.bito...Ahorro...C","P2.4.Tarjeta.de.cr.dito","P2.4.Tarjeta.Azteca","P2.4.Cuenta.de.ahorro","P2.4.Cuenta..de.cheques","P2.4.Inversiones","P2.4.Seguros","P2.4.Pr.stamo.Personal","P2.4.Cr.dito.en.tienda.para.compra.","P2.4.Cr.dito.Hipotecario","P2.4.Cr.dito.Automotriz","P2.4.Cr.dito.para.desarrollo.de.Neg","P2.4.Cr.dito.Prendario..Empe.o","P2.4.Cr.dito.Grupal","P2.4.Pr.stamo.de.nomina","P2.4.Otro.cr.dito.o.pr.stamo.por.pa","P2.4.Tarjeta.departamental","P2.4.Cr.dito.de.motocicleta")
bandera4<-c("Total","P11.Env.os.de.dinero","P11.Pago.de.servicios..Tel.fono..A","P11.Banca.m.vil","P11.Tarjeta.de.cr.dito","P11.Tarjeta..Azteca","P11.Cuenta.de.cheques","P11.Pr.stamos.personales.para.viaj","P11.Pr.stamos.personales.para.educ","P11.Pr.stamos.personales.para.mejo","P11.Pr.stamos.personales.para.fies","P11.Seguro.de.vida","P11.Seguro.de.m.dico","P11.Seguro.para.la.casa","P11.Seguro.de.auto","P11.Seguro.de.gastos.funerarios","P11.Ahorro.para.el.futuro","P11.Ahorro.para.eventos.sociales","P11.Ahorro.para.emergencias","P11.Ahorro.planeado.cuenta.de.ahor","P11.Inversi.n.para.el.negocio","P11.Pr.stamo.hipotecario","P11.Cr.dito.automotriz","P11.Pr.stamos.de.nomina","P11.Pr.stamo.para.gastos.personale","P11.Tarjeta.de.debito","P11.OTRO...Especificar")



# 
datos$E16 <- factor(datos$E16)




nombresR(datos,"E1.")[37:41]

# Tiene false en 2 o en 3? 

datos$FiltroE10A <- TRUE
datos[datos$A3=="No (No Bancarizado)","FiltroE10A"] <- FALSE
datos[datos$E1.Tableta=="FALSE","FiltroE10A"] <- FALSE
datos[datos$E1.Computadora=="FALSE","FiltroE10A"] <- FALSE

datos$FiltroE10A <- FALSE
datos[datos$A3=="Sí (Bancarizado)" & datos$E1.Tableta=="TRUE" & datos$E1.Computadora=="TRUE","FiltroE10A"] <- T

table(datos$FiltroE10A)


table(datos$E10A.Interntet)
39+93
# 
# nombresR(x = datos,ll = "E5.")[37:45]
# nombresR(x = datos,ll = "E6.")[37:43]
# nombresR(x = datos,ll = "E7.")[37:41]
# nombresR(x = datos,ll = "E9")[39:47]
# nombresR(x = datos,ll = "E10")[2:10]
# nombresR(x = datos,ll = "E10")[2:10]
# names(datos)
# 
# nombresR(x = datos,ll = "E9.Internet")[1:9]
# nombresR(x = datos,ll = "E10.Internet")
# 
# 
# datos[datos$A3=="Sí (Bancarizado)",]
# 
# table(datos$E2==levels(datos$E2)[3])
# table(datos$E10A.Interntet)
# table(datos$E10.Internet.Otro)
# 
# levels(table(datos$E2))
# levels(table(datos$E2))
# levels(datos$E2)[3]
# 
# 39+93
# 
# nrow(datos[datos$E2==3])

reporteF<-list(
  E1=frecuentator(fTtabla=datos,fTvariables=nombresR(xx = datos,yy = "E1.")[37:41],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E2=frecuentator(fTtabla=datos,fTvariables="E2",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E3=frecuentator(fTtabla=datos,fTvariables="E3",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E4=frecuentator(fTtabla=datos,fTvariables="E4",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E5=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3],],fTvariables=nombresR(xx = datos,yy = "E5.")[37:45],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E6=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3],],fTvariables=nombresR(xx = datos,yy = "E6.")[37:43],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E7=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3],],fTvariables=nombresR(xx = datos,yy = "E7.")[37:41],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E8=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3],],fTvariables="E8",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E9=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3] & datos$E8==levels(datos$E8)[1],],fTvariables=nombresR(xx = datos,yy = "E9")[39:47],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E10=frecuentator(fTtabla=datos[datos$E2==levels(datos$E2)[3] & datos$E8==levels(datos$E8)[1],],fTvariables=nombresR(xx = datos,yy = "E10")[2:10],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E10A=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)",],fTvariables="E10A.Interntet",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E9A=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)" & datos$E10A.Interntet=="Sí",],fTvariables=nombresR(xx = datos,yy = "E9.Internet")[1:9],fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E10AA=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)" & datos$E10A.Interntet=="Sí",],fTvariables=nombresR(xx = datos,yy = "E10.Internet"),fTlevels=F,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E11=frecuentator(fTtabla=datos,fTvariables="E11",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E12=frecuentator(fTtabla=datos,fTvariables="E12",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E13=frecuentator(fTtabla=datos[datos$E12=="Sí",],fTvariables="E13",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E14=frecuentator(fTtabla=datos[datos$E12=="Sí",],fTvariables="E14",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E15=frecuentator(fTtabla=datos[datos$E12=="Sí",],fTvariables="E15",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E16=frecuentator(fTtabla=datos[datos$E12=="Sí",],fTvariables="E16",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E17=frecuentator(fTtabla=datos[datos$E12=="Sí",],fTvariables="E17",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E18=frecuentator(fTtabla=datos,fTvariables="E18",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E19=frecuentator(fTtabla=datos[datos$E18=="Sí",],fTvariables="E19",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E20=frecuentator(fTtabla=datos[datos$E18=="Sí",],fTvariables="E20",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E21=frecuentator(fTtabla=datos,fTvariables="E21",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  E22=frecuentator(fTtabla=datos[datos$E21!=levels(datos$E21)[1],],fTvariables="E22",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F)
)
39+93
reporteFINAL <- data.frame()
for(finali in 1:length(reporteF)){
  
  elTemporal <- reporteF[[finali]]
  
  elTemporal <- rbind(names(elTemporal),elTemporal)
  
  names(elTemporal) <- paste(LETTERS,1:length(elTemporal),sep="")
  
  elTemporal[1,1] <- names(reporteF)[finali]
  
  salto <- elTemporal[0,]
  
  elTemporal <- rbind(elTemporal,NA)
  
  
  reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)
  
}

write.csv(reporteFINAL, paste("./resultados/ReporteFinal Segundo.csv",sep = ""),fileEncoding = "Latin1",na = "")
