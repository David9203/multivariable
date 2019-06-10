

library(stats)
library(psy)
library(factoextra)
library(lmtest)
library(normtest)
library(randtests)
library(car)
library(rsq)
library(ordinal)

datos<-read.csv(file=file.choose(),header=T,sep=',')
attach(datos)
head(datos)
head(datos)
length(datos)
x=datos[,c(1:28)]
y=target
rmscut=cut(RMS, breaks = 2, labels = c(0, 1))
rmscut

estandarizar<-function(x){
	for (i in 1:length(x)) {   ##transformar datos a datos positivos 
 	#datos[i]=dates[i]-min(dates[i])  #para poner todos los datos positivos 
 	x[i] = (x[i]-min(x[i]))/(max(x[i])-min(x[i]))  #para poner todos los 	datos de 0 a 1 
     }
 return (x)
 }
xstandarized=estandarizar(x)
xstandarized

correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
       library(ggcorrplot)   #para grafico de correlaciones 
    ggcorrplot(corr_matrix,lab = TRUE, lab_size=2)
    }


    correlationanalisis(datos)

a=datos$ADIm3
b=datos$ADIm4
new <- rbind(a, b)




#REGRESIÓN MULTIVARIADA
ks.test(price,pnorm,mean(price),sd(price))
powerTransform(price)
priceT<-(price)^-1.4

#Principal
res.pca <- prcomp(x, scale = TRUE)
varpca=res.pca$x[,1:4]
var<-get_pca_var(res.pca)
    correlationanalisis(varpca)
get_eig(res.pca)



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
MLG <- glm(y ~ varpca[,c(1:3)], family=binomial(link=logit))  #en este caso foreign es categorica
summary(MLG)
             

#https://idus.us.es/xmlui/bitstream/handle/11441/77492/Arias%20Ben%C3%ADtez%20Miguel%20TFG.pdf?sequence=1&isAllowed=y
MLG<-clm(as.factor(y) ~ varpca[,c(1:3) +rmscut  )#ADIm3+datos$ADIm4+ACIft+ADI+) 
MLG
summary(MLG)
drop1(MLG,test="Chi")

clm.control()$gradTol #Un gradiente absoluto peque;o es necesario para la convergencia del modelo (es necesario que max.grad este por debajo de este valor)

anova(MLG)
anova(MLG)
predict(MLG, type="response") # predicted values
residMLG<-residuals(MLG, type="deviance") # residuals
rsq(MLG)   ##Este Rcuadrado es el quasi rcuadrado, el 52% es aceptable
rsq(MLG,adj=TRUE)
confint(MLG, type="Wald")

#SUPUESTOS 
#ESPECIFICACIÓN DEL MODELO
reset(MLG)

#NO AUTOCORRELACIÓN
runs.test(residMLG)

#NORMALIDAD DE LOS RESIDUOS
jb.norm.test(residMLG, nrepl=2000)

#HOMOSCEDASTICIDAD OJO QUE SÓLO ES PARA VARIABLES CONTINUAS
x<-datos[,c(18:28)]
bartlett.test(x)
white.test(x=varpca,y=y)

leveneTest(y=price_T, group=foreign)
			plot(residMLG)
