library(factanal)
library(factoextra)
library(psy)
library(nFactors)
library(GPArotation)
library(Rgraphviz)
library(stats)
library(lmtest)
library(normtest)
library(randtests)
library(car)
library(EnvStats)
datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
row.names(datos) <- datos[,c(1)]

 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
       library(ggcorrplot)   #para grafico de correlaciones 
    ggcorrplot(corr_matrix,lab = TRUE, lab_size=2)
    }
    
   Data <- subset( datos, select = -foreign ) ##saco la var long por que no aporta nada
    head(Data)
    values<-Data[,3:length(Data)]
    correlationanalisis(values)
    
  shapiro.test(values[,1])
  
 powerTransform(values[,1])	
  lam= -0.3533898 
valu1=  boxcoxTransform(values[,1],lam)
  shapiro.test(valu1)
  
  
  
  shapiro.test(log(values[,2]))
  powerTransform(values[,2])	
  lam=   0.6197182 
valu2=  boxcoxTransform(values[,2],lam) 
    shapiro.test(valu2)
ks.test(valu2,pnorm,mean(valu2),sd(valu2))

  
   powerTransform(values[,3])	
  lam= 0.8142939
valu3=  boxcoxTransform(values[,3],lam)
  shapiro.test(valu3)


shapiro.test(values[,4])
  shapiro.test(log(values[,4]))
  powerTransform(values[,4])	
  lam=   0.5026624
valu4=  boxcoxTransform(values[,4],lam) 
    shapiro.test(valu4)
ks.test(valu4,pnorm,mean(valu4),sd(valu4))

i=5
shapiro.test(values[,i])
valu5=values[,5]


i=6
shapiro.test(values[,i])
  shapiro.test(log(values[,i]))
  powerTransform(values[,i])	
  lam=     0.2986733 
valu6=  boxcoxTransform(values[,i],lam) 
    shapiro.test(valu6)
ks.test(valu6,pnorm,mean(valu6),sd(valu6))

i=7
shapiro.test(values[,i])
  shapiro.test(log(values[,i]))
  powerTransform(values[,i])	
  lam=    0.001216884
valu7=  boxcoxTransform(values[,i],lam) 
    shapiro.test(valu7)
ks.test(valu7,pnorm,mean(valu7),sd(valu7))

i=8
shapiro.test(values[,i])
  shapiro.test(log(values[,i]))
  powerTransform(values[,i])	
  lam=   0.09708738
valu8=  boxcoxTransform(values[,i],lam) 
    shapiro.test(valu8)
ks.test(valu8,pnorm,mean(valu8),sd(valu8))

i=9
shapiro.test(values[,i])
  shapiro.test(log(values[,i]))
  powerTransform(values[,i])	
  lam=    0.7186669
valu9=  boxcoxTransform(values[,i],lam) 
    shapiro.test(valu9)
ks.test(valu9,pnorm,mean(valu9),sd(valu9))


i=9
ks.test(values[,i],pnorm,mean(values[,i]),sd(values[,i]))

data.frame(log(Consumption),log(Investment),log(Governmemt_expenditure),log(Population_000s))


valuesbox=data.frame(valu1,valu2,valu3,valu4,valu5,valu6,valu7,valu8,valu9)

    correlationanalisis(valuesbox)
    summary(valuesbox)
    x=valuesbox

for (i in 1:length(x)) {   ##transformar datos a datos positivos 
#datos[i]=dates[i]-min(dates[i])  #para poner todos los datos positivos 
 x[i] = (x[i]-min(x[i]))/(max(x[i])-min(x[i]))  #para poner todos los datos de 0 a 1 
}
  head(x)
    summary(x)
    correlationanalisis(x)
  
  
fa.parallel(x)         
KMO(valuesbox)
cortest.bartlett(valuesbox)
 fit<-factanal(x,3,method="ml",scores="regression",rotation="varimax")
fit
scores<-fit$scores
scores
cor(scores)
    
    
ks.test(price,pnorm,mean(price),sd(price))
ks.test(log(price),pnorm,mean(log(price)),sd(log(price)))

simple_mpg <- lm(log(price) ~ scores[,1]) 
summary(simple_mpg)
anova(simple_mpg)
confint(simple_mpg)

#1. Especificación del modelo
reset(simple_mpg)

2. #Homoscedasticidad
x<-scores[,c(1:2)]
bartlett.test(scores[,1])

#3. Autocorrelación: si es serie de tiempo dwtest(def_regres)
resid_def<-residuals(simple_mpg)
plot(num, resid_def)
par(mfrow = c(2, 2))
plot(def_regres)
runs.test(resid_def)

#4. Normalidad de los residuales
jb.norm.test(resid_def, nrepl=2000)

x=scores[,1]
x= (x-min(x))/(max(x)-min(x))  #para poner todos los datos de 0 a 1 
x=x+1
  powerTransform(x)	
valufit=  boxcoxTransform(x,1.702808) 
    shapiro.test(valufit)

  powerTransform(price)	
valuprice=  boxcoxTransform(price,-1.436082) 
shapiro.test(valuprice)

simple_mpg <- lm(valuprice ~ valufit) 
summary(simple_mpg)
anova(simple_mpg)
confint(simple_mpg)

bartlett.test(valufit)

write.csv(valuprice, file="/Users/nesdav/Documents/estadisticamultivariada/segunda parte/regresion 2 clase/homework/price.csv",row.names=FALSE, sep='.')
write.csv(valufit, file="/Users/nesdav/Documents/estadisticamultivariada/segunda parte/regresion 2 clase/homework/valufit.csv",row.names=FALSE, sep='.')
scores<-read.csv(file=file.choose(),header=T,sep=',')

