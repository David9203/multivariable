
library(factanal)
library(psy)
library(nFactors)
library(GPArotation)
library(Rgraphviz)
library(stats)



##hacer analisis exploratorio


datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
dim(datos)
dates=datos
for (i in 7:35) {    ##transformar datos a datos positivos 
 datos[i]=dates[i]-min(dates[i])
  }
  
  Data <- subset( datos, select = -Longitud ) ##saco la var long por que no aporta nada
  Data <- subset( Data, select = -BNT ) ##Saco la var BNT por que no aporta

datanum<-Data[,c(7:33)]  #Cojo solo las variables 
 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
    }
    head(datanum)
    
     for(i in 1:length(datanum)){
 	a[i]=shapiro.test(datanum[,i]) 
 }
a
#Se demuestra que estan normalizados, por lo tanto se procede a hacer pca. 
  write.csv(datanum, file="numeros.csv", sep=';',row.names=FALSE)

res.pca <- prcomp(datanum, scale = TRUE)
res.pca

get_eig(res.pca)

##se grafica
plot(res.pca)  #varianzas
fviz_eig(res.pca)
fviz_pca_ind(res.pca, repel=TRUE)#gráfico individuos y PCA
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 100))#screeplot
fviz_pca_biplot(res.pca, repel = TRUE, addlabels = TRUE) #biplot
fviz_pca_var(res.pca, col.var = "black") #grafico de pesos de los componentes
fviz_pca_var(res.pca, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)#grafico de pesos de los componentes a color
fviz_pca_ind(res.pca,label = "none", habillage = nompais,addEllipses = TRUE, palette = "jco")


 
    #Buscar las transformaciones de potencia 
 datos_trans=datanum
cortest.bartlett(datos_trans)  ##pruebas para evaluar si factor es buen analisis 
KMO(datos_trans)##si tiene un porcentaje alto sirve
head(datos_trans)
scree(datos_trans) ##grafico de codo para evaluar los datos que sirvan 
fa.parallel(datos_trans) 


fit<-factanal(datos_trans,1,method="ml",scores="regression",rotation="varimax") ##1- es el numero de factores, ml es maximaverosimilitud, la rotacion es para que sea mas facil la interpretacion
fit
  write.csv(ev$vectors[,c(1:4)], file="/Users/nesdav/Downloads/ESTADISTICAREDUCCION/fit.csv", sep=';',row.names=FALSE)
ev$vectors[,c(1:4)]
scree.plot(fit$correlation) #screeplot
ev <- eigen(cor(datos_trans))
 $values
ev$vectors[,c(1:4)]
factor.plot(fit, cut=0.5)
dim(datos_trans)
fa.diagram(fit)
dim(ev$vectors[,c(1:4)])
library(stringr)
permanence<-dates[,c(4)] ## Se crea un vector con la salida
permanence
library(stringi)
permanence<-stri_replace_all_fixed(permanence,"Baja",0) ##como hago para tirar dos comandos en una linea?
permanence<- stri_replace_all_fixed(permanence,"Alta",2)  ###reemplaza altos por 1 bajos por cero
permanence<- stri_replace_all_fixed(permanence,"Media",1)
  write.csv(as.matrix(permanence), file="/Users/nesdav/Downloads/ESTADISTICAREDUCCION/fit.csv", sep=';',row.names=FALSE)



###Cluster en donde cojo los datos de factor analisis 
datos_cluster_FACTOR=datos_trans
#CLUSTER FACTOR 
dist_matrix_FA <- dist(datos_cluster_FACTOR, method = "euclidean")
  write.csv(as.matrix(dates[,c(5)]), file="/Users/nesdav/Downloads/ESTADISTICAREDUCCION/fit.csv", sep=';',row.names=FALSE)
hier_cluster_FA <- hclust(dist_matrix_FA^2, method="ward") 
plot(hier_cluster_FA,hang=-1)
cluster4_FA<-cutree(hier_cluster_FA,k=3)
plot(cluster4_FA)

res.pca[,c(1:4)]

write.csv2(cluster4_FA, file="/Users/nesdav/Downloads/ESTADISTICAREDUCCION/cluster4_FA.csv",row.names=F)
datos_cluster_PCA=
#CLUSTER FACTOR 
dist_matrix_PCA <- dist(datos_cluster_PCA, method = "euclidean")
hier_cluster_PCA <- hclust(dist_matrix_PCA^2, method="ward") 
plot(hier_cluster_PCA,hang=-1)
cluster4_PCA<-cutree(hier_cluster_PCA,k=2)
head(dates[,c(4)])
write.csv2(dates[,c(4)], file="/Users/nesdav/Downloads/ESTADISTICAREDUCCION/cluster4_FA.csv",row.names=F)

write.csv2(cluster4_PCA, file="cluster4_PCA.csv",row.names=F)

par(mfrow = c(2, 1))
plot(hier_cluster_FA,hang=-1,font=1, cex.axis=0.8)
plot(hier_cluster_PCA,hang=-1,,font=1, cex.axis=0.8)
466149478F04B014
66743453