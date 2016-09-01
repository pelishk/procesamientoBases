

#### PRIMERO CORRER LAS FUNCIONES EN FRECUENTATOR

# 

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

misP1Top <- nombresR(x = datos,ll = "P1")[1:14]
misP1OM <- nombresR(x = datos,ll = "P1")[20:33]

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

misP2 <- nombresR(x = datos,ll = "P2")[1:14]
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
nombresR(x = datos,ll = "P2.5.")

misP2.5 <- data.frame(
  medio =nombresR(x = datos,ll = "P2.4")[1:19],
  bancoAzteca=  nombresR(x = datos,ll = "P2.5.Banco.Azteca"),
  banamex=  nombresR(x = datos,ll = "P2.5.Banamex"),
  banorte=nombresR(x = datos,ll = "P2.5.Banorte"),
  bancomer=nombresR(x = datos,ll = "P2.5.Bancomer"),
  hsbc=nombresR(x = datos,ll = "P2.5.HSBC"),
  santander=nombresR(x = datos,ll = "P2.5.Santander"),
  scotiabank=nombresR(x = datos,ll = "P2.5.Scotiabank"),
  coppel=nombresR(x = datos,ll = "P2.5.Coppel"),
  otro=nombresR(x = datos,ll = "P2.5.Otro..Especificar"),
  stringsAsFactors = F
)

######################## P4
nombresR(x = datos,ll = "P4")

misP4 <- data.frame(
  medio =nombresR(x = datos,ll = "P3")[1:19],
  bancoAzteca=  nombresR(x = datos,ll = "P4.Banco.Azteca"),
  banamex=  nombresR(x = datos,ll = "P4.Banamex"),
  banorte=nombresR(x = datos,ll = "P4.Banorte"),
  bancomer=nombresR(x = datos,ll = "P4.Bancomer"),
  hsbc=nombresR(x = datos,ll = "P4.HSBC"),
  santander=nombresR(x = datos,ll = "P4.Santander"),
  scotiabank=nombresR(x = datos,ll = "P4.Scotiabank"),
  coppel=nombresR(x = datos,ll = "P4.Coppel"),
  otro=nombresR(x = datos,ll = "P4.Otro..Especificar"),
  stringsAsFactors = F
)

# matrix(data = misP2.5,byrow = F,nrow = 10)
######################## P6
nombresR(x = datos,ll = "P6.Servicio")

misP6 <- data.frame(
  medio =nombresR(x = datos,ll = "P3")[1:19],
  bancoAzteca=  nombresR(x = datos,ll = "P6.Servicio.BancoAzteca")[seq(1,38,by = 2)],
  banamex=  nombresR(x = datos,ll = "P6.Servicio.Banamex")[seq(1,38,by = 2)],
  banorte=nombresR(x = datos,ll = "P6.Servicio.Banorte")[seq(1,38,by = 2)],
  bancomer=nombresR(x = datos,ll = "P6.Servicio.Bancomer")[seq(1,38,by = 2)],
  hsbc=nombresR(x = datos,ll = "P6.Servicio.Hsbc"),
  santander=nombresR(x = datos,ll = "P6.Servicio.Santander")[seq(1,38,by = 2)],
  scotiabank=nombresR(x = datos,ll = "P6.Servicio.Scotiabank")[seq(1,38,by = 2)],
  coppel=nombresR(x = datos,ll = "P6.Servicio.Coppel")[seq(1,38,by = 2)],
  otro=nombresR(x = datos,ll = "P6.Servicio.OTRO"),
  stringsAsFactors = F
)


######################## P6
nombresR(x = datos,ll = "P6A")

misP6A <- data.frame(
  bancoAzteca=  nombresR(x = datos,ll = "P6A.BancoAzteca")[seq(1,38,by = 2)],
  banamex=  nombresR(x = datos,ll = "P6A.Banamex"),
  banorte=nombresR(x = datos,ll = "P6A.Banorte")[seq(1,38,by = 2)],
  bancomer=nombresR(x = datos,ll = "P6A.Bancomer")[seq(1,38,by = 2)],
  hsbc=nombresR(x = datos,ll = "P6A.Hsbc"),
  santander=nombresR(x = datos,ll = "P6A.Santander")[seq(1,38,by = 2)],
  scotiabank=nombresR(x = datos,ll = "P6A.Scotiabank")[seq(1,38,by = 2)],
  coppel=nombresR(x = datos,ll = "P6A.Coppel")[seq(1,38,by = 2)],
  otro=nombresR(x = datos,ll = "P6A.otro"),
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





nombresR(x = datos,ll = "P2.4")[1:20]



reporte<-list(
  A3=frecuentator(fTtabla=datos,fTvariables="A3",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  F1Genero=frecuentator(fTtabla=datos,fTvariables="F1Genero",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  NSE=frecuentator(fTtabla=datos,fTvariables="NSE",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  EdadRango=frecuentator(fTtabla=datos,fTvariables="Edad.Rango",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  Plaza=frecuentator(fTtabla=datos,fTvariables="Plaza",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  # Segmento=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)",]="Sí (Bancarizado)",],fTvariables="SegmentoBancarizados",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  cluster=frecuentator(fTtabla=datos,fTvariables="cluster",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  clusterB=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)",],fTvariables="clusterB",fTlevels=T,fbanner=bandera1,fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  # ClusterVersusCluster=frecuentator(fTtabla=datos,fTvariables="cluster",fTlevels=T,fbanner="clusterB",fTponderador=NULL,fTsobreQuien= NULL,fTtotal=T,fTprop=F),
  ####################  
  A1=frecuentator(fTtabla=datos,fTvariables="A1",fTlevels=T,fbanner=bandera1),
  A4=frecuentator(fTtabla=datos,fTvariables="A4",fTlevels=T,fbanner=bandera1),
  A5=frecuentator(fTtabla=datos[datos$A4=="Casado(a)" | datos$A4=="Unión libre",],fTvariables="A5tiempoJuntos",fTlevels=T,fbanner=bandera1),
  A6=frecuentator(fTtabla=datos,fTvariables="A6",fTlevels=T,fbanner=bandera1),
  A7=frecuentator(fTtabla=datos[datos$A6=="Sí",],fTvariables="A7Hijos",fTlevels=T,fbanner=bandera1),
  A8=frecuentator(fTtabla=datos[datos$A6=="Sí",],fTvariables="A8Hijos",fTlevels=T,fbanner=bandera1),
  A9=frecuentator(fTtabla=datos[datos$A6=="Sí",],fTvariables=hijitosEdad,fTlevels=T,fbanner=bandera1),
  A10=frecuentator(fTtabla=datos,fTvariables="A10",fTlevels=T,fbanner=bandera1),
  A11=frecuentator(fTtabla=datos,fTvariables="A11",fTlevels=T,fbanner=bandera1),
  ####################  
  
  P1Top=frecuentator(fTtabla=datos,fTvariables=misP1Top,fTlevels=F,fbanner=bandera1),
  P1TopShare=frecuentator(fTtabla=datos,fTvariables=misP1TopShare,fTlevels=F,fbanner=bandera1),
  P2ConocimientoTotal=frecuentator(fTtabla=datos,fTvariables=miConocimientoTotal,fTlevels=F,fbanner=bandera1),
  P2UsadoAlgunaVez=frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P2.1"),fTlevels=F,fbanner=bandera1),
  P2UsadoUltimos6Meses=frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P2.2"),fTlevels=F,fbanner=bandera1),
  P2UsaConMayorFrecuencia=frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P2.3"),fTlevels=F,fbanner=bandera1),
  ####################
  P2_4=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)",],fTvariables=nombresR(x = datos,ll = "P2.4")[1:20],fTlevels=F,fbanner=bandera1)
)
####################
listaP25 <- list()
for(p25i in 1:nrow(misP2.5)){
  # p25i <- 6
  miProducto <- misP2.5[p25i,1]
  misBancos <- unlist(misP2.5[p25i,-1])
  listaP25[[p25i]] <- frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)" & datos[,miProducto]==TRUE,],fTvariables=misBancos,fTlevels=F,fbanner=bandera1)
}
names(listaP25) <- misP2.5$medio
####################
reporte2 <- list(
  P3=frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)",],fTvariables=nombresR(x = datos,ll = "P3")[1:20],fTlevels=F,fbanner=bandera1)
)
####################
listaP4 <- list()
for(p4i in 1:nrow(misP4)){
  # p4i <- 6
  miProducto <- misP4[p4i,1]
  misBancos <- unlist(misP4[p4i,-1])
  listaP4[[p4i]] <- frecuentator(fTtabla=datos[datos$A3=="Sí (Bancarizado)" & datos[,miProducto]==TRUE,],fTvariables=misBancos,fTlevels=F,fbanner=bandera1)
}
names(listaP4) <- misP4$medio
####################
# Para cada producto
listaP6 <- list()
for(p4i in 1:nrow(misP4)){
  # p4i <- 2
  miProducto <- misP4[p4i,1]
  misBancos <- as.character(misP4[p4i,-1])
  # Para cada banco...
  for(p4t in 1:length(misBancos)){
    # p4t<-5
    miBanco<- misBancos[p4t]
    ProductoBancoSub<-datos[datos$A3=="Sí (Bancarizado)" & datos[,miProducto]==TRUE & datos[,miBanco]==TRUE,]
    if(nrow(ProductoBancoSub)>0){
      #Sólo si tengo casos...
      miTemp<-misP6[p4i,p4t+1]
      if(sum(table(datos[,miTemp]))>0){
        miTemp <- frecuentator(fTtabla=ProductoBancoSub,fTvariables=miTemp,fTlevels=T,fbanner=bandera1)
        listaP6[[(length(listaP6)+1)]]<-miTemp
        names(listaP6)[(length(listaP6))]<-paste(miProducto,miBanco,misP6[p4i,p4t+1],sep="_")        
      }
    }
  }
}
####################
# Para cada producto
listaP6A <- list()
for(p4i in 1:nrow(misP4)){
  # p4i <- 1
  miProducto <- misP4[p4i,1]
  misBancos <- as.character(misP4[p4i,-1])
  # Para cada banco...
  for(p4t in 1:length(misBancos)){
    # p4t<-2
    miBanco<- misBancos[p4t]
    miTemp<-misP6[p4i,p4t+1]    
    # Dijo 1 o 2?
    ProductoBancoSub<-datos[datos$A3=="Sí (Bancarizado)" & datos[,miProducto]==TRUE & datos[,miBanco]==TRUE & (datos[,miTemp]=="Folletos y/o volantes" | datos[,miTemp]=="Stands"),]
    if(nrow(ProductoBancoSub)>0){
      #Sólo si tengo casos...
      miTemp<-misP6A[p4i,p4t]
      if(sum(table(ProductoBancoSub[,miTemp]))>0){
        miTemp <- frecuentator(fTtabla=ProductoBancoSub,fTvariables=miTemp,fTlevels=T,fbanner=bandera1)
        listaP6A[[(length(listaP6A)+1)]]<-miTemp
        names(listaP6A)[(length(listaP6A))]<-paste(miProducto,miBanco,misP6[p4i,p4t+1],misP6A[p4i,p4t],sep="_")        
      }
    }
  }
}
####################
reporte3 <- list(
  P71=frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P7")[1],fTlevels=T,fbanner=bandera1),
  P72=frecuentator(fTtabla=datos[datos$P7=="Sí",],fTvariables=nombresR(x = datos,ll = "P7.2")[1:13],fTlevels=F,fbanner=bandera1),
  P73=frecuentator(fTtabla=datos[datos$BazUser==T,],fTvariables="P7.3",fTlevels=T,fbanner=bandera1),
  P73=frecuentator(fTtabla=datos[datos$BazUser==T,],fTvariables=nombresR(x = datos,ll = "P7.4")[1:13],fTlevels=F,fbanner=bandera1)
)
####################
listaP8 <- list()
misP8 <- nombresR(x = datos,ll = "P8")
for(mip8i in 1:length(misP8)){
  miP8tempo <- frecuentator(fTtabla=datos,fTvariables=misP8[mip8i],fTlevels=T,fbanner=bandera1)
  listaP8[[(length(listaP8)+1)]]<-miP8tempo
  names(listaP8)[(length(listaP8))]<-misP8[mip8i]
}
####################
listaP9 <- list()
misP9 <- nombresR(x = datos,ll = "P9")
for(mip9i in 1:length(misP9)){
  miP9tempo <- frecuentator(fTtabla=datos,fTvariables=misP9[mip9i],fTlevels=T,fbanner=bandera1)
  listaP9[[(length(listaP9)+1)]]<-miP9tempo
  names(listaP9)[(length(listaP9))]<-misP9[mip9i]
}
####################
listaP10 <- list()
misP10 <- nombresR(x = datos,ll = "P10")
for(mip10i in 1:length(misP10)){
  miP10tempo <- frecuentator(fTtabla=datos,fTvariables=misP10[mip10i],fTlevels=T,fbanner=bandera1)
  listaP10[[(length(listaP10)+1)]]<-miP10tempo
  names(listaP10)[(length(listaP10))]<-misP10[mip10i]
}
####################
reporte4 <- list(
  P11=frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P11")[1:26],fTlevels=F,fbanner=bandera1),
  P114=frecuentator(fTtabla=datos[datos$BazUser==T,],fTvariables="P11.4",fTlevels=T,fbanner=bandera1),
  P115=frecuentator(fTtabla=datos[datos$BazUser==T,],fTvariables="P11.5",fTlevels=T,fbanner=bandera1),
  P12=frecuentator(fTtabla=datos,fTvariables="P12",fTlevels=T,fbanner=bandera1),
  P13=frecuentator(fTtabla=datos,fTvariables="P13",fTlevels=T,fbanner=bandera1),
  P13A=frecuentator(fTtabla=datos[datos$P13=="Sí",],fTvariables="P13.A",fTlevels=T,fbanner=bandera1),
  P14=frecuentator(fTtabla=datos,fTvariables="P14",fTlevels=T,fbanner=bandera1),
  P14A=frecuentator(fTtabla=datos[datos$P14=="Sí",],fTvariables="P14.A",fTlevels=T,fbanner=bandera1)
)
####################
listaP15 <- list()
misP15 <- nombresR(x = datos,ll = "P15")
for(mip15i in 1:length(misP15)){
  miP15tempo <- frecuentator(fTtabla=datos,fTvariables=misP15[mip15i],fTlevels=T,fbanner=bandera1)
  listaP15[[(length(listaP15)+1)]]<-miP15tempo
  names(listaP15)[(length(listaP15))]<-misP15[mip15i]
}
####################




nopl <- frecuentator(fTtabla=datos,fTvariables=nombresR(x = datos,ll = "P8"),fTlevels=F,fbanner=bandera1)

nombresR(x = datos,ll = "P11")[1:26]





sum(table(datos$P14.A))

BazUser

table(datos$P7.3.Filtro)

sum(table(datos$P11.4))
sum(table(datos$P11.5))


as.data.frame(table(datos$P15.Acude.a.prestamistas.informale))
as.data.frame(table(datos$P1.Banco.Azteca))
as.data.frame(table(datos$P1.Banco.Azteca))
####################
################################################################################################
################################################################################################
reporteF <- c(
    reporte,
    listaP25,
    reporte2,
    listaP4,
    listaP6,
    listaP6A,
    reporte3,
    listaP8,
    listaP9,
    listaP10,
    reporte4,
    listaP15
    )

reporteF <- reporte4
##### Puedo hacer un data frame gigante?
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

write.csv(reporteFINAL, paste("./resultados/ReporteFinal.csv",sep = ""),fileEncoding = "Latin1",na = "")

# for(i in 1:length(reporteF)){
#   write.csv(reporteF[[i]], paste("./resultados/",1000+i,"_",names(reporteF[i]),".csv",sep = ""),fileEncoding = "Latin1")
# }
################################################################################################
################################################################################################

getwd()
