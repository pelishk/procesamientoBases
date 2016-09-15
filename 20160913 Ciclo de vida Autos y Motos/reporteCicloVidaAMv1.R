devtools::install_github("brostatistics","pelishk")
library(dplyr)
library(brostatistics)
library(sjmisc)


# datos <- gaseosa(xfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv",yfile1 = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx")
# 
# names(datos)[188] <- "P15_12_1"
# 
# saveRDS(datos,"datos.rds")

datos <- readRDS(
  "datos.rds"
)

##########################################################################################
##########################################################################################
# Modificaciones necesarias

# Saco variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")

# Usuarios de autos o motos
misP <- nombresR(datos,"P15_2")

misP <- matrix(data = misP,nrow = 6,ncol = 5)

listaNombres<-list()
for(i in 1:nrow(misP)){
  # i <- 1
  yoSoy <- misP[i,1]
  yoSoy <- paste("Rr_", yoSoy,sep="")
  listaNombres[[i]]<-paste(misP[i,])
  names(listaNombres)[[i]]<-yoSoy  
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-FALSE
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]==1
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-TRUE
  }
}

# ··········

##########################################################################################

# Banner
bandera1 <- c("Total","F1Genero","NSE","Edad_Rango","Plaza")

nombresR(datos,"11")
names(datos)[1:100]

table(
  datos$P10
)
datos[datos$P10=="Automóvil",]


resultados <- list(
  # Banner vs Banner
  Genero = frecuentator(fTtabla = datos,fTvariables = "F1Genero",fTlevels = T,fbanner = bandera1),
  NSE = frecuentator(fTtabla = datos,fTvariables = "NSE",fTlevels = T,fbanner = bandera1),
  Edad_Rango = frecuentator(fTtabla = datos,fTvariables = "Edad_Rango",fTlevels = T,fbanner = bandera1),
  Plaza = frecuentator(fTtabla = datos,fTvariables = "Plaza",fTlevels = T,fbanner = bandera1),
  P9 = frecuentator(fTtabla = datos,fTvariables = "P9",fTlevels = T,fbanner = bandera1),
  P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
  P11 = frecuentator(fTtabla = datos,fTvariables = "A4",fTlevels = T,fbanner = bandera1),
  P12 = frecuentator(fTtabla = datos,fTvariables = "A6",fTlevels = T,fbanner = bandera1),
  P14 = frecuentator(fTtabla = datos,fTvariables = "A11",fTlevels = T,fbanner = bandera1)
)
exportator(resultados, "./resultados/resultadosPrimeros.csv")


resultadosAutos <- list(
  P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
  P15Autos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"Rr_P15_2"),fTlevels = F,fbanner = bandera1)
)
exportator(resultadosAutos, "./resultados/resultadosAutos.csv")


  
  