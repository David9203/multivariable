#install.packages("lmtest")
#install.packages("normtest")
#install.packages("rsq")
install.packages("randtests")
install.packages("car")

library(lmtest)
library(normtest)
library(randtests)
library(car)
library(rsq)

datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
row.names(datos) <- datos[,c(1)]

#REGRESIÓN MULTIVARIADA
ks.test(price,pnorm,mean(price),sd(price))
powerTransform(price)
priceT<-(price)^-1.4


#FACTOR
head(datos)
cuantit_MM<-datos[,c(4:10)]
corr_matrix_MM<-cor(cuantit_MM)
corr_matrix_MM
fit_MM<-factanal(cuantit_MM,3,method="ml",scores="regression",rotation="varimax")
fit_MM
scores_MM<-fit_MM$scores
write.csv(scores_MM, file="scores_MM.csv",row.names=FALSE, sep='.')
scores_MM<-read.csv(file=file.choose(),header=T,sep=',')
attach(scores_MM)


#https://www.statmethods.net/advstats/glm.html
#MLG
head(datos)
attach(datos)
MLG <- glm(priceT ~ Factor1 +Factor2 +Factor3 +foreign )  #en este caso foreign es categorica
summary(MLG)
             


anova(MLG)
predict(MLG, type="response") # predicted values
residMLG<-residuals(MLG, type="deviance") # residuals
rsq(MLG)   ##Este Rcuadrado es el quasi rcuadrado, el 52% es aceptable
rsq(MLG,adj=TRUE)

#SUPUESTOS 
#ESPECIFICACIÓN DEL MODELO
reset(MLG)

#NO AUTOCORRELACIÓN
runs.test(residMLG)

#NORMALIDAD DE LOS RESIDUOS
jb.norm.test(residMLG, nrepl=2000)

#HOMOSCEDASTICIDAD OJO QUE SÓLO ES PARA VARIABLES CONTINUAS
x<-datos[,c(3:5)]
bartlett.test(x)
leveneTest(y=price_T, group=foreign)
			plot(residMLG)

