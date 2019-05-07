install.packages("lmtest")
install.packages("normtest")
install.packages("randtest")
library(lmtest)
library(normtest)
library(randtests)




datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
row.names(datos) <- datos[,c(1)]
#Gráficos de dispersión
pairs(~mpg+price+headroom+trunk+weight+length+turn+displacement+gear_ratio, main="Simple Scatterplot Matrix")
scatter.smooth(x=mpg, y=price, main="Scatter plot")
scatter.smooth(x=headroom, y=price, main="Scatter plot")
scatter.smooth(x=trunk, y=price, main="Scatter plot")
scatter.smooth(x=weight, y=price, main="Scatter plot")
scatter.smooth(x=length, y=price, main="Scatter plot")
scatter.smooth(x=turn, y=price, main="Scatter plot")
scatter.smooth(x=displacement, y=price, main="Scatter plot")
scatter.smooth(x=gear_ratio, y=price, main="Scatter plot")

#prueba de normalidad para Y
ks.test(price,pnorm,mean(price),sd(price))
ks.test(log(price),pnorm,mean(log(price)),sd(log(price)))

#ajuste de la regresión simple
simple_mpg <- lm(log(price) ~ mpg) 
summary(simple_mpg)
anova(simple_mpg)
confint(simple_mpg)
#residuales
resid_mpg<-residuals(simple_mpg)
par(mfrow = c(2, 2))
plot(simple_mpg)
plot(num, resid_mpg)
#The diagnostic plots show residuals in four different ways:
#1. Residuals vs Fitted. Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
#2. Normal Q-Q. Used to examine whether the residuals are normally distributed. It’s good if residuals points follow the straight dashed line.
#3. Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity. 
#4. Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis. 


#REGRESIÓN MÚLTIPLE
multip_regres <- lm(log(price) ~ mpg+headroom+trunk+weight+length+turn+displacement+gear_ratio) 
summary(multip_regres)

#validación de supuestos
# multicolinealidad
cuantit<-datos[,c(3:10)]
corr_matrix<-cor(cuantit)
corr_matrix
fit<-factanal(cuantit,3,method="ml",scores="regression",rotation="varimax")
fit
scores<-fit$scores
scores
cor(scores)
write.csv(scores, file="scores_price.csv",row.names=FALSE, sep='.')
scores<-read.csv(file=file.choose(),header=T,sep=',')
attach(scores)
scores
#nueva regresión
new_regres<- lm(log(price) ~Factor1+Factor2+Factor3)
summary(new_regres)

#regresión definitiva:sólo variables significativas
def_regres<- lm(log(price) ~Factor1+Factor2)
summary(def_regres)
anova(def_regres)


#1. Especificación del modelo
reset(def_regres)

2. #Homoscedasticidad
x<-scores[,c(1:2)]
bartlett.test(x)

#3. Autocorrelación: si es serie de tiempo dwtest(def_regres)
resid_def<-residuals(def_regres)
plot(num, resid_def)
par(mfrow = c(2, 2))
plot(def_regres)
runs.test(resid_def)

#4. Normalidad de los residuales
jb.norm.test(resid_def, nrepl=2000)


##intento con pca

pca
row.names(pca) <-FALSE
res.pca <- prcomp(cuantit, scale = TRUE)
pca<-res.pca$x[,c(1:2)]
write.csv(res.pca$x[,c(1:2)], file="scores_price.csv",row.names=FALSE, sep='.')
scores<-read.csv(file=file.choose(),header=T,sep=',')

var<-get_pca_var(res.pca)
var$coor
get_eig(res.pca)

attach(pca)
PC1

new_regres<- lm(log(price) ~ log(PC1))
summary(new_regres)

cor(scores)

#1. Especificación del modelo
reset(def_regres)

2. #Homoscedasticidad
x<-scores[,c(1:2)]
bartlett.test(x)

#3. Autocorrelación: si es serie de tiempo dwtest(def_regres)
resid_def<-residuals(new_regres)
plot(num, resid_def)
par(mfrow = c(2, 2))
plot(def_regres)
runs.test(resid_def)

#4. Normalidad de los residuales
jb.norm.test(resid_def, nrepl=2000)


## 