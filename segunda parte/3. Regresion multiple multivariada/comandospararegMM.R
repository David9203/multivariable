#install.packages("lmtest")
#install.packages("normtest")
library(lmtest)
library(normtest)
library(randtests)
library(car)

datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
row.names(datos) <- datos[,c(1)]

#REGRESIÓN MULTIVARIADAks.test(price,pnorm,mean(price),sd(price))
powerTransform(price)
priceT<-(price)^-1.4

ks.test(mpg,pnorm,mean(mpg),sd(mpg))
ks.test(priceT,pnorm,mean(priceT),sd(priceT))

#con ambas variables
MM_regres <- lm(cbind(priceT,mpg) ~ headroom+trunk+weight+length+turn+displacement+gear_ratio) 
summary(MM_regres)
summary(manova(MM_regres)) #LA manova nos dice cuales son las variables que son importantes para ambas, 
head(residuals(MM_regres))
head(fitted(MM_regres))

#por separado las regresiones dan lo mismo 
price_regres <- lm(priceT ~ headroom+trunk+weight+length+turn+displacement+gear_ratio) 
summary(price_regres)

mpg_regres <- lm(mpg ~ headroom+trunk+weight+length+turn+displacement+gear_ratio) 
summary(mpg_regres)

#OJO REVISAR SUPUESTOS

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
#nuevaregres
MM_nuevaregres <- lm(cbind(priceT,mpg) ~ Factor1 +Factor2 +Factor3) 
summary(MM_nuevaregres)
summary(manova(MM_nuevaregres))

#https://www.statmethods.net/advstats/glm.html
#MLG
head(datos)
attach(datos)
MLG <- glm(priceT ~ Factor1 +Factor2 +Factor3 +foreign )
summary(MLG)
anova(MLG)

