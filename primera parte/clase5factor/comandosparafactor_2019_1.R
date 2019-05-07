#FACTOR
#programa para datos de la clase
install.packages("factanal")
install.packages("psy")
install.packages("nFactors")
install.packages('GPArotation')
install.packages("Rgraphviz")

#cargar librerias
library(factanal)
library(psy)
library(nFactors)
library(GPArotation)
library(Rgraphviz)
library(stats)



#leer datos
datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)

#divido la base de datos
datos_cont<-datos[,c(3:7)] 
head(datos_cont)
row.names(datos_cont) <- datos[,c(2)] 


#matriz de correlaciones
corr_matrix<-cor(datos_cont) #matriz de correlaciones para todas las variables
corr_matrix

#seleccionar sólo las variables correlacionadas
datos_corrfa<-datos_cont[,c(1:4)]
attach(datos_corrfa)

cortest.bartlett(datos_trans)
KMO(datos_trans)

#probar normalidad con la prueba Kolmogorov
ks.test(Consumption,pnorm,mean(Consumption),sd(Consumption))
ks.test(log(Consumption),pnorm,mean(log(Consumption)),sd(log(Consumption)))

ks.test(Investment,pnorm,mean(Investment),sd(Investment))
ks.test(log(Investment),pnorm,mean(log(Investment)),sd(log(Investment)))

ks.test(Governmemt_expenditure,pnorm,mean(Governmemt_expenditure),sd(Governmemt_expenditure))
ks.test(log(Governmemt_expenditure),pnorm,mean(log(Governmemt_expenditure)),sd(log(Governmemt_expenditure)))

ks.test(Population_000s,pnorm,mean(Population_000s),sd(Population_000s))
ks.test(log(Population_000s),pnorm,mean(log(Population_000s)),sd(log(Population_000s)))

datos_trans<-data.frame(log(Consumption),log(Investment),log(Governmemt_expenditure),log(Population_000s))
head(datos_trans)
write.csv(datos_trans, file="datos_trans.csv",sep=';',row.names=FALSE)
datos_trans=datos
scree(datos_trans) ##grafico de codo para evaluar los datos que sirvan 
fa.parallel(datos_trans)         
#hacer factor inicial
fit<-factanal(datos_trans,1,method="ml",scores="regression",rotation="varimax") ##1- es el numero de factores, ml es maximaverosimilitud, la rotacion es para que sea mas facil la interpretacion
fit
scree.plot(fit$correlation) #screeplot
ev <- eigen(cor(datos_trans))
ev

#calcular los scores
fit$scores
#guardar los scores
scores<-fit$scores
write.csv(scores, file="scores.csv",row.names=FALSE)#recuerde que debe cambiar las comas por puntos en excel

#otros cálculos
communality<-1-fit$uniquenesses
communality
fit$loadings

#otros gráficos

fa.diagram(fit$loadings)






