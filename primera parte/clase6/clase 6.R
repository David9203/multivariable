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

dates=datos
for (i in 7:35) {   ##transformar datos a datos positivos 
 datos[i]=dates[i]-min(dates[i])
  }
  
  Data <- subset( datos, select = -Longitud ) ##saco la var long por que no aporta nada
  Data <- subset( Data, select = -BNT ) ##Saco la var BNT por que no aporta

datanum<-Data[,c(7:33)]  #Cojo solo las variables 
 correlationanalisis<-function(datanum){
    corr_matrix<-cor(datanum) ## saca matrices de correlaciones 
    corr_matrix
    
    
    #Buscar las transformaciones de potencia 