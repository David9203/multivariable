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
discrim_cluster2FA <- lda(cluster2_FA ~ Factor1+Real_GDP_SD)  ##analisis discriminante lineal (lda)
#Primera posicion variable categorica, cluster=y, las x son Factor1, real no estaba correlacionada, pero se usa para el cluster y para analisis discriminante

discrim_cluster2FA

anova_cluster2FA <- lm(cluster2_FA ~ Factor1+Real_GDP_SD)
anova(anova_cluster2FA)
#siempre se hace anova, nos dice cual de las variables discrimina o no
#anova nos muestra el valor p (Pr(>F)), nos dice si discrimina o no ,mayor que 0.05 no discrimina, menor si.

#determinar el porcentaje de individuos correctamente clasificados

discrim_cluster2FA.prediction <- predict(discrim_cluster2FA)$class
table(datos$cluster2_FA, discrim_cluster2FA.prediction, dnn = c('Actual Group','Predicted Group'))
nrows<-length(datos$cluster2_FA)
percentaje<-sum(discrim_cluster2FA.prediction == datos$cluster2_FA)/nrows
percentaje


#hacer discriminante para pca. 

discrim_cluster2PCA <- lda(cluster2_PCA ~ Factor1+Real_GDP_SD)  ##analisis discriminante lineal (lda)
discrim_cluster2PCA

anova_cluster2PCA <- lm(cluster2_PCA ~ Factor1+Real_GDP_SD)
anova(anova_cluster2PCA)


discrim_cluster2PCA.prediction <- predict(discrim_cluster2PCA)$class
table(datos$cluster2_PCA, discrim_cluster2PCA.prediction, dnn = c('Actual Group','Predicted Group'))
nrows<-length(datos$cluster2_PCA)
percentaje<-sum(discrim_cluster2PCA.prediction == datos$cluster2_PCA)/nrows
percentaje
