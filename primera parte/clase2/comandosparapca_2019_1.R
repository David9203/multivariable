install.packages("factoextra")

library(stats)
library(psy)
library(factoextra)
library(car)

#cargo los datos
datos<-read.csv(file=file.choose(),header=T,sep=';')
head(datos)
#divido la base de datos
datos_cont<-datos[,c(3:7)]  ##coja todas las filas, y las columnas de la 3 a la 7 
head(datos_cont)
row.names(datos_cont) <- datos[,c(2)]   ##los nombres de filas llevele lo que tengo en dato de la columna 2 para que los  grafique

#matriz de correlaciones
corr_matrix<-cor(datos_cont) ## saca matrices de correlaciones 
corr_matrix
write.csv(corr_matrix, file="correlaciones.csv",sep=';',row.names=FALSE)


#matriz de covarianza 
cov_matrix<-cov(datos_cont) ## saca matrices de covarianza  
cov_matrix
write.csv(cov_matrix, file="covarianza.csv",sep=';',row.names=FALSE) ## escribe un archivo csv si no le guarda csv se guarda con csv2.

 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
    }
    head(datanum)
    
     for(i in 1:length(datanum)){
 	a[i]=shapiro.test(datanum[,i]) 
}
head(datos_cont)
correlationanalisis(datos_cont)


#para probar si una variable tiene distribucion normal , hago la prueba de bondad de ajuste 
# con prueba de shapiro 

shapiro.test(Consumption) ## no funciono entonces le indico el numero de la columna
shapiro.test(datos_cont[,c(5)])  ##nos da el valor p, si es menor que 0,05 no es una distribucin normal 
 shapiro.test(log(datos_cont[,c(2)]))  ##nos da el valor p, si es menor que 0,05 no es una distribucin normal 
datacont<-datos_cont
datos_cont<-log(datos_cont)
 
ks.test(datos_cont$Consumption ,pnorm) ## Kolmogorov para verificar si es  normal, en este caso no lo es por lo tanto se hace una transformacion de potencia 
powerTransform(datos_cont[,c(1)]) ## nos dice que la elevemos a un valor de log natural, y lo elevamos



#TAREA: hacer gráfico matriz de correlaciones

library(corrplot)
corrplot(datos_cont, method="circle")

#instalar paquetes para corrmatri
if(!require(devtools)) install.packages("devtools")

#llamar librerias 
> library(ggcorrplot)

#para grafico de correlaciones 
ggcorrplot(corr_matrix)


##2clase REDUCCION DE DIMENSIONES


#calculo el PCA
res.pca <- prcomp(datos_cont, scale = TRUE)
res.pca

#solicito las tablas
get_pca_var(res.pca)
get_eig(res.pca)#tabla de eigenvalores varianza acumulada

#las gráficas https://rstudio-pubs-static.s3.amazonaws.com/323416_ab58ad22d9e64ba2831569cf3d14a609.html
plot(res.pca)
fviz_eig(res.pca)
fviz_pca_ind(res.pca, repel=TRUE)#gráfico individuos y PCA
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 100))#screeplot
fviz_pca_biplot(res.pca, repel = TRUE, addlabels = TRUE) #biplot
fviz_pca_var(res.pca, col.var = "black") #grafico de pesos de los componentes
fviz_pca_var(res.pca, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)#grafico de pesos de los componentes a color
fviz_pca_ind(res.pca,label = "none", habillage = nompais,addEllipses = TRUE, palette = "jco")

#calculo de los componentes
pred<-predict(res.pca)p
write.csv(pred, file="/Downloads/componentes.csv",sep=';',row.names=FALSE)
