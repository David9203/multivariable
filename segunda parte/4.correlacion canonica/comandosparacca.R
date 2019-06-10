
install.packages("CCA")
install.packages("car")
library(CCA)
library(car)



datos<-read.csv(file=file.choose(),header=T,sep=';')
row.names(datos) <- datos[,c(1)]
head(datos)
corr_matrix<-cor(datos[,2:13])
corr_matrix
X <- as.matrix(datos[,7:13])
Y <- as.matrix(datos[,2:6]) 


#CANÓNICA
canon<-cc(X,Y)
canon
scores<-canon$scores
scores
plt.cc(canon,d1=1,d2=2,type = "i",var.label=TRUE, ind.names=row.names(datos))
plt.cc(canon,d1=1,d2=2,type = "v",var.label=TRUE, ind.names=row.names(datos))
plt.cc(canon,d1=1,d2=2,type = "b",var.label=TRUE, ind.names=row.names(datos))

#MÚLTIPLE MULTIVARIADA
multiv_regres <- lm(Y ~ X)
summary(multiv_regres)

#Validar supuestos


