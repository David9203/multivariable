library(factanal)
library(psy)
library(nFactors)
library(GPArotation)
library(Rgraphviz)
library(stats)
library(factoextra)

#HACER ANÁLISIS EXPLORATORIO

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
write.csv(corr_matrix, file="corr_matrix.csv",row.names=FALSE)
data=datos
#BUSCAR LAS TRANSFORMACIONES DE POTENCIA
datos_trans<-data.frame(log(datos$Consumption),log(datos$Investment),log(datos$Governmemt_expenditure),log(datos$Population_000s))
head(datos_trans)
write.csv(datos_trans, file="datos_trans.csv",sep=';',row.names=FALSE)

#matriz de correlaciones datos transformadas
corr_matrix_trans<-cor(datos_trans) #matriz de correlaciones para todas las variables
corr_matrix_trans
write.csv(corr_matrix_trans, file="corr_matrix_rans.csv",row.names=FALSE)

#hago el PCA con los datos transformados para poder comparar con el factor
res.pca <- prcomp(datos_trans, scale = TRUE)
res.pca
get_pca_var(res.pca)
get_eig(res.pca)
pred<-predict(res.pca)
pred
write.csv(pred, file="componentes.csv",sep=';',row.names=FALSE)
















