datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)
dim(datos)

i=15
sd(datanum[,i], na.rm=TRUE)/ 
+    mean(datanum[,i], na.rm=TRUE)*100
[1] 76.67039


datanum<-read.csv(file=file.choose(),header=T,sep=';')
attach(datanum)
head(datanum)

ADI<-read.csv(file=file.choose(),header=T,sep=';')
attach(ADI)
head(ADI)

ADIm4<-read.csv(file=file.choose(),header=T,sep=';')
attach(ADIm4)
head(ADIm4)

ADI[,1]
  datanum <- subset( datanum, select = -ADIm4 ) ##saco la var long por que no aporta nada
str(datanum)


dataprueba<-read.csv(file=file.choose(),header=T,sep=';')
attach(dataprueba)
head(dataprueba)
dim(dataprueba)
dataprueba<-dataprueba[,c(7:35)]  #Cojo solo las variables
   dataprueba <- subset( dataprueba, select = -BNT ) ##Saco la var BNT por que no aporta

dataprueba[,7]

datanum$ADI<-ADI[,1]
datanum$ADIm4<-ADIm4[,1] #agrega dato al dataframe
  write.csv(datanum, file="/Users/nesdav/Documents/estadisticamultivariada/informe/newdatanum.csv", sep=';',row.names=FALSE)


  Data <- subset( datos, select = -Longitud ) ##saco la var long por que no aporta nada
  Data <- subset( datos, select = -BNT ) ##Saco la var BNT por que no aporta
data<-Data[,c(7:33)]  #Cojo solo las variables 
head(data)
correlationanalisis(datanum)

cov_matrix<-cov(data) ## saca matrices de covarianza  
cov_matrix
ggcorrplot(cov_matrix,lab = TRUE, lab_size=2)

boxplot(datanum)







summary(datanum)
datosnormfil=datanum
res.pca <- prcomp(datosnormfil, scale = TRUE)
res.pca

library(MVN)
Data<- data
result <- mvn(Data, mvnTest = "mardia")
result$multivariateNormality



#install.packages("factoextra")

library(factoextra)
var<- get_pca_var(res.pca)
head(var$coord)

library("factoextra")
get_eig(res.pca)#t

plot(res.pca)  #varianzas

fviz_eig(res.pca)
fviz_pca_ind(res.pca, repel=TRUE)#gráfico individuos y PCA

fviz_pca_biplot(res.pca, repel = TRUE, addlabels = TRUE) #biplot
fviz_pca_var(res.pca, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)#grafico de pesos de los componentes a color

pred<-predict(res.pca)
pred[,1:4]

