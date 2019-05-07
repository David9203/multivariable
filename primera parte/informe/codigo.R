
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
x=dates

pie(table(datos$Transformación))
library(car)
scatterplotMatrix(datanum[,1:4],groups=datos$Transformación)


library(scatterplot3d)
scatterplot3d(res.pca$x[,1], res.pca$x[,2] ,res.pca$x[,3], color=c("red", "green", "blue")[unclass(datos$Transformación)])


for (i in 3:length(x)) {   ##transformar datos a datos positivos 
 #datos[i]=dates[i]-min(dates[i])  #para poner todos los datos positivos 
 x[i] = (x[i]-min(x[i]))/(max(x[i])-min(x[i])).  #para poner todos los datos de 0 a 1 
 
  }
  head(x)
  
  Data <- subset( datos, select = -Longitud ) ##saco la var long por que no aporta nada
  Data <- subset( Data, select = -BNT ) ##Saco la var BNT por que no aporta
dim(Data)
head(Data)
datanum<-Data[,c(7:33)]  #Cojo solo las variables 
 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
       library(ggcorrplot)   #para grafico de correlaciones 
    ggcorrplot(corr_matrix,lab = TRUE, lab_size=2)
    }
    head(datanum)
    correlationanalisis(datanum)
     for(i in 1:length(datanum)){
+ 	a[i]=shapiro.test(datanum[,i]) 
+ }
a
#Se demuestra que estan normalizados, por lo tanto se procede a hacer pca. 
  write.csv(corr_matrix, file="numeros.csv", sep=';',row.names=FALSE)

res.pca <- prcomp(datanum, scale = TRUE)
res.pca$x[,1:4]
var<-get_pca_var(res.pca)
var$coor
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


fit<-factanal(datos_trans,4,method="ml",scores="regression",rotation="varimax") ##1- es el numero de factores, ml es maximaverosimilitud, la rotacion es para que sea mas facil la interpretacion
fit$scores

fitprue<-factanal(dataprueba,4,method="ml",scores="regression",rotation="varimax") ##1- es el numero de factores, ml es maximaverosimilitud, la rotacion es para que sea mas facil la interpretacion
fitprue$scores
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
permanence <- as.numeric(permanence)

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



fit$scores

install.packages("MASS")
library(MASS)
library(pvclust)
datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
row.names(datos) <- datos[,c(2)]

#HACER CLUSTER PARA CONFIRMAR CON DISCRIMINANTE

#ANÁLISIS DISCRIMINANTE
#PARA CLUSTER 2 FA
discrim_cluster2FA <- lda(permanence ~ fit$scores )  ##analisis discriminante lineal (lda)
#Primera posicion variable categorica, cluster=y, las x son Factor1, real no estaba correlacionada, pero se usa para el cluster y para analisis discriminante
cluster4_FA ~ fit$scores 
discrim_cluster2FA

anova_cluster2FA <- lm(permanence ~ fit$scores )
anova(anova_cluster2FA)
#siempre se hace anova, nos dice cual de las variables discrimina o no
#anova nos muestra el valor p (Pr(>F)), nos dice si discrimina o no ,mayor que 0.05 no discrimina, menor si.

#determinar el porcentaje de individuos correctamente clasificados

discrim_cluster2FA.prediction <- predict(discrim_cluster2FA)$class
table(permanence, discrim_cluster2FA.prediction, dnn = c('Actual Group','Predicted Group'))
nrows<-length(permanence)
percentaje<-sum(discrim_cluster2FA.prediction == permanence)/nrows
percentaje

prueba.prediction <- predict(dataprueba)$class



library(scatterplot3d)
scatterplot3d(fit$scores[,1], fit$scores[,2] ,fit$scores[,3], color=c("red", "green", "blue")[unclass(discrim_cluster2FA.prediction)])
scatterplot3d(fit$scores[,1], fit$scores[,2] ,fit$scores[,3], color=c("red", "green", "blue")[unclass(permanence)])
#hacer discriminante para pca. 

