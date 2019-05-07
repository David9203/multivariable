#install.packages("pvclust")
library(pvclust)
datos<-read.csv(file=file.choose(),header=T,sep=';')
head(datos)
attach(datos)
row.names(datos) <- datos[,c(2)]
datos_cluster_FACTOR<-data.frame(Factor1,PC1,Real_GDP_SD)
row.names(datos_cluster_FACTOR) <- datos[,c(2)]
datos_cluster_PCA<-data.frame(PC1,Real_GDP_SD)
row.names(datos_cluster_PCA) <- datos[,c(2)]

#CLUSTER FACTOR 
dist_matrix_FA <- dist(datos_cluster_FACTOR, method = "euclidean")
hier_cluster_FA <- hclust(dist_matrix_FA^2, method="ward") 
plot(hier_cluster_FA,hang=-1)
cluster4_FA<-cutree(hier_cluster_FA,k=4)
write.csv2(cluster4_FA, file="cluster4_FA.csv",row.names=F)

#CLUSTER FACTOR 
dist_matrix_PCA <- dist(datos_cluster_PCA, method = "euclidean")
hier_cluster_PCA <- hclust(dist_matrix_PCA^2, method="ward") 
plot(hier_cluster_PCA,hang=-1)
cluster4_PCA<-cutree(hier_cluster_PCA,k=4)
write.csv2(cluster4_PCA, file="cluster4_PCA.csv",row.names=F)

par(mfrow = c(2, 1))
plot(hier_cluster_FA,hang=-1,font=1, cex.axis=0.8)
plot(hier_cluster_PCA,hang=-1,,font=1, cex.axis=0.8)