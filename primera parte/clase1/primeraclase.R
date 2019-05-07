clear install.packages("factanal")
library(factanal)
datos<-read.csv(file=file.choose(), header=T, sep=";")
head(datos)