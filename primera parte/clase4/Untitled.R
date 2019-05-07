##sobre pca y datos de bosque seco tropical
##Mucho cuidado con los puntos y comas, en r se trabaja con puntos para decimales
library(stats)
library(psy)
library(factoextra)

#cargo los datos
datos<-read.csv(file=file.choose(),header=T,sep=';')
head(datos)

#Backup pa no volver a cargar 
dates=datos
for (i in 7:35) {   ##transformar datos a datos positivos 
 datos[i]=dates[i]-min(dates[i])
  }
  
  Data <- subset( datos, select = -Longitud ) ##saco la var long por que no aporta nada
  Data <- subset( Data, select = -BNT ) ##Saco la var BNT por que no aporta

# Para el analisis de correlaciones 
 datanum<-Data[,c(7:33)]  #Cojo solo las variables 
 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
    write.csv(corr_matrix, file="Downloads/correlaciones.csv",sep=';',row.names=FALSE)
    library(ggcorrplot)   #para grafico de correlaciones 
    ggcorrplot(corr_matrix)
 }
 
 ##Shapiro test para los datos. 
 for(i in 1:length(datanum)){
+ 	a[i]=shapiro.test(datanum[,i]) 
+ }

#Se demuestra que estan normalizados, por lo tanto se procede a hacer pca. 

res.pca <- prcomp(datanum, scale = TRUE)
res.pca

get_eig(res.pca)#tabla de eigenvalores varianza acumulada
