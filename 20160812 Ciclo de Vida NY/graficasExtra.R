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

#cambio los nombres de los atributos

names(bateria)<-c('actualmente se encuentra sin presiones de gastos',
                  'actualmente realiza las actividades que desea',
                  'comienza a tener responsabilidades',
                  'siente la necesidad de conseguir un trabajo',
                  'siente que el tiempo le es suficiente para hacer todas sus actividades',
                  'está trabajando para el futuro',
                  'tiene la necesidad de iniciar un negocio',
                  'las cosas en la vida son sencillas',
                  'le ha sido fácil conseguir trabajo',
                  'casi todos sus ingresos los invierte en sus hijos',
                  'se siente realizado en la vida',
                  'cuenta con un ingreso estable',
                  'constantemente desea viajar',
                  'tiene la necesidad e comprar un automóvil',
                  'ahorra constantemente',
                  'tiene la necesidad e tener un crédito para comprar su casa',
                  'le gustaría tener una línea de crédito para emergencias',
                  'ahorra dinero para el futuro de sus hijos/su familia',
                  'prefiere utilizar una tarjeta de crédito o débito para realizar compras',
                  'se siente cómodo usando los servicios de un banco',
                  'ahorra dinero para hacer compras',
                  'prefiere pervibir pagos en efectivo que depósitos a su cuenta',
                  'contribuye al gasto del hogar',
                  'apoya económicamente a sus padres',
                  'vive en la casa de sus padres',
                  'depende económicamente de sus padres',
                  'actualmente se preocupa por asegurar su tranquilidad en la vejez',
                  'ahorra para eventos festivos especiales',
                  'gasta una buena parte de su ingreso en zapatos, vestido y accesorios',
                  'gasta buena parte de sus ingresos en tecnología',
                  'gasta buena parte de sus ingresos en alimentación, transporte y servicios',
                  'ahorra para gastos funerarios',
                  'llega a pedir préstamos para cubrir enfermedades o accidentes',
                  'constantemente paga deudas de préstamos',
                  'se previene ahorrando para alguna enfermedad que llegara a surgir',
                  'usted piensa que para tener éxito económico son muy necesarios los estudios',
                  'trata de ahorrar pero sus estudios no se lo permiten',
                  'es fácil para usted obtener un crédito',
                  'tiene la necesidad de quipar y amueblar su casa',
                  'es más importante trabajar duro que estudiar',
                  'se prepara económicamente para el nacimiento de un bebé',
                  'le gustaría estar asegurado para eventos inesperados (e.g. salud, muerte, robo)',
                  'cuando piensa en sus finanzas, se siente muy tranquilo',
                  'le gusta pagar a plazos/ pagos diferidos',
                  'considera que elige sus inversiones independientemente, sin mucha ayuda del banco',
                  'pregiere satisfacer sus necesidades financieras sin involucrarse con un banco',
                  'realmente valora revibir orientación por parte de expertos sobre su situación financiera',
                  'siempre está buscando en el mercado la mejor tasa para su crédito',
                  'siempre paga el saldo total de su tarjeta de crédito cada mes',
                  'cuando tiene que pagar su tarjeta de cŕedito sólo paga el saldo mínimo',
                  'cluster'
                  )



#promedios por cluster
resultado<-bateria %>%
  mutate_each(funs(as.numeric)) %>%
  group_by(cluster) %>%
  summarise_each(funs(mean)) %>%
  as.data.frame()

#promedio pblacional
promediototal<-bateria %>%
  mutate_each(funs(as.numeric)) %>%
  summarise_each(funs(mean))
resultado$promedioPob<-promediototal[1:50]
  
#las junto
resultado<-rbind(resultado,promediototal)
resultado$cluster[7]<-7

resultado<-t(resultado)
resultado<-resultado[-1,]
resultado<-as.data.frame(resultado)
names(resultado)<-1:7
resultado$importancias<-importancias$importancias
resultado$atributo<-names(bateria)[1:50]
resultado<-resultado %>%
  arrange(importancias)
resultado$orden<-1:50
resultado$promedio<-(resultado[,1]+resultado[,2]+resultado[,3]+resultado[,4]+resultado[,5])/5
resultado$ordenFactor<-as.factor(resultado$orden)
names(resultado)[1:7]<-c('r1','r2','r3','r4','r5','r6','total')
resultado

#recalculo para que todos los promedios salgan alineados

resultado<-resultado %>%
  mutate(rr1=r1-total,
         rr2=r2-total,
         rr3=r3-total,
         rr4=r4-total,
         rr5=r5-total,
         rr6=r6-total)
resultado$totalr<-0

head(resultado)
#grafico esa cosa para cada grupo de atributos----------------------------------------------------------------------------------------

#este pedazo aplica para todos
parametrosColores<-c("r1"="gold",
                     "r2"="green",
                     'r3'="hotpink",
                     "r4"="red",
                     "r5"="blue",
                     "r6"="gray")


#informado
resultadoSub<-resultado[c(50,48,47,44),]
rownames(resultadoSub)<-resultadoSub$atributo

g1<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de información')

#afín banco
resultadoSub<-resultado[c(9,35),]
rownames(resultadoSub)<-resultadoSub$atributo

g2<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de afinidad bancaria')

#atributos de estilo de vida
resultadoSub<-resultado[c(28,15,3,6,41),]
rownames(resultadoSub)<-resultadoSub$atributo

g3<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de estilo de vida')

#atributos de logro
resultadoSub<-resultado[c(13,12,4,42,23,5,26),]
rownames(resultadoSub)<-resultadoSub$atributo

g4<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de logro')

#tranquilidad financiera
resultadoSub<-resultado[c(27,32,38,44,11,14,37,50,21),]
rownames(resultadoSub)<-resultadoSub$atributo

g5<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de tranquilidad financiera')

#aversión bancaria
resultadoSub<-resultado[c(33,30,25),]
rownames(resultadoSub)<-resultadoSub$atributo

g6<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de aversión bancaria')

#atributos de madurez
resultadoSub<-resultado[c(16,7,18,31),]
rownames(resultadoSub)<-resultadoSub$atributo

g7<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de independización')

#emergencias
resultadoSub<-resultado[c(29,49),]
rownames(resultadoSub)<-resultadoSub$atributo

g8<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de emergencia')

#insolvencia
resultadoSub<-resultado[c(47,46,19,34,45,22,8,48,49),]
rownames(resultadoSub)<-resultadoSub$atributo

g9<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de insolvencia')

#familia
resultadoSub<-resultado[c(24,20),]
rownames(resultadoSub)<-resultadoSub$atributo

g10<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de familia')

#nini
resultadoSub<-resultado[c(1,2),]
rownames(resultadoSub)<-resultadoSub$atributo

g11<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos de dependencia económica')


#importancia
resultadoSub<-resultado[c(50:41),]
rownames(resultadoSub)<-paste(resultadoSub$atributo)
resultadoSub$atributo<-factor(resultadoSub$atributo,rev(resultadoSub$atributo))

g12<-ggplot(resultadoSub, aes(x=totalr,y=atributo))+
  geom_point(size=14,alpha=.15)+
  geom_point(size=13,color='white',alpha=.15)+
      geom_text(size=5,aes(label=round(promedio,2)))+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr1,y=atributo,colour='r1'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr2,y=atributo,colour='r2'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr3,y=atributo,colour='r3'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr4,y=atributo,colour='r4'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr5,y=atributo,colour='r5'),size=3.5,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo),size=4,alpha=.5)+
  geom_point(data=resultadoSub,aes(x=rr6,y=atributo,colour='r6'),size=3.5,alpha=.5)+
  scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
  ylab('')+
  xlab('')+
  xlim(-1.5,1.5)+
  ggtitle('Atributos más importantes en la clasificación de clusters')


#exporto todas las gráficas-----------------------------------------------------------

pdf('atributos de información.pdf',width=12,height=8)
g1
dev.off()
pdf('atributos de afinidad bancaria.pdf',width=12,height=8)
g2
dev.off()
pdf('atributos de estilo de vida.pdf',width=12,height=8)
g3
dev.off()
pdf('atributos de logro.pdf',width=12,height=8)
g4
dev.off()
pdf('atributos de tranquilidad financiera.pdf',width=12,height=8)
g5
dev.off()
pdf('atributos de aversión bancaria.pdf',width=12,height=8)
g6
dev.off()
pdf('atributos de independización.pdf',width=12,height=8)
g7
dev.off()
pdf('atributos de emergencia.pdf',width=12,height=8)
g8
dev.off()
pdf('atributos de insolvencia.pdf',width=12,height=8)
g9
dev.off()
pdf('atributos de familia.pdf',width=12,height=8)
g10
dev.off()
pdf('atributos de dependencia económica.pdf',width=12,height=8)
g11
dev.off()
pdf('atributos más importantes en la clasificación.pdf',width=12,height=8)
g12
dev.off()

# 
# pdf('dqa funcional completo.pdf',width=14,height=8)
# g1
# dev.off()
# 
# 
# 
# #sólo los 10 atributos que más ayudan en la clasificación
# resultado10<-resultado[41:50,]
# library(ggplot2)
# 
# parametrosColores<-c("r1"="gold",
#                      "r2"="green",
#                      'r3'="hotpink",
#                      "r4"="red",
#                      "r5"="blue",
#                      "r6"="gray")
# 
# g1<-ggplot(resultado10, aes(x=promedio,y=orden))+
#   geom_point(size=9,alpha=.15)+
#   geom_point(size=8,color='white',alpha=.15)+
#   geom_point(size=6,aes(shape=atributo))+
#   theme(legend.position="bottom")+
#   scale_shape_manual(values=c(65:100),labels=resultado10$atributo)+
#   geom_point(data=resultado10,aes(x=r1,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r1,y=orden,colour='r1'),size=1.8)+
#   geom_point(data=resultado10,aes(x=r2,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r2,y=orden,colour='r2'),size=1.8)+
#   geom_point(data=resultado10,aes(x=r3,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r3,y=orden,colour='r3'),size=1.8)+
#   geom_point(data=resultado10,aes(x=r4,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r4,y=orden,colour='r4'),size=1.8)+
#   geom_point(data=resultado10,aes(x=r5,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r5,y=orden,colour='r5'),size=1.8)+
#   geom_point(data=resultado10,aes(x=r6,y=orden),size=2)+
#   geom_point(data=resultado10,aes(x=r6,y=orden,colour='r6'),size=1.8)+
#   scale_colour_manual(name='',values=parametrosColores,labels=c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6'))+
#   ylab('Discriminación')
# 
# pdf('dqa funcional completo.pdf',width=14,height=8)
# g1
# dev.off()
# 
