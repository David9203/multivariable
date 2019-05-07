#http://www.sthda.com/english/wiki/print.php?id=228
#pagina con todo el analisis, bien explicada
install.packages("ca")
install.packages("FactoMineR")
install.packages("gplots")
install.packages("graphics")
install.packages("corrplot")
library(ca)
library(FactoMineR)
library(gplots)
library(graphics)
library(factoextra)
library(corrplot)


datos<-read.csv(file=file.choose(),header=T,sep=';')

dt <- as.table(as.matrix(datos)).  #se comvierte a una tabla.

balloonplot(t(dt), main ="Profesores", xlab ="", ylab="",label = FALSE, show.margins = FALSE)
#nos muestra donde aparece mas la respuesta(cuando la mayoria de las personas ponen mas votos)
#para poner el numero de conteos se cambia a label true 

mosaicplot(dt, shade = TRUE, las=2,main = "Profesores")
# a las revistas les gusta mucho esta grafica. Estudiar. 

res.ca <- CA(datos, graph = TRUE) #CA es analisis de correspondencias, tiene la misma interpretacion de pca o factor, se tienn los dos ejes, la dim 1 averiguar como hacer grafico en 3 dimenciones. me dice para cada una 
#de las respuestas como se agrupan, en las partes positivas y negaticas representan los pesos .
summary(res.ca, nb.dec = 2, ncp = 2) ##resumen, con prueba, si el valor p es menor que alfa=0.05 se rechaza h cero por lo tanto si hay una relacion entre las variables
eigenvalues <- get_eigenvalue(res.ca)  #pertenece a facto extra

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))
plot(res.ca)
fviz_ca_biplot(res.ca)
fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE))

#Para visualizar sÛlo los renglones (preguntas) que preguntas y respuestas contribuyen mas a la reduccion de dimencion
row <- get_ca_row(res.ca)
row
head(row$coord)
fviz_ca_row(res.ca)
head(row$contrib)

#contribuciÛn por dimensiÛn
corrplot(row$contrib, is.corr=FALSE). ##que tanto aportan las preguntas 
#a la dimensiÛn 1
fviz_contrib(res.ca, choice = "row", axes = 1). ##lo que aporta cada pregunta a la dimencion 1
#a la dimensiÛn 2
# Contributions of rows on Dim.2
fviz_contrib(res.ca, choice = "row", axes = 2)
# Total contribution on Dim.1 and Dim.2
fviz_contrib(res.ca, choice = "row", axes = 1:2) #mas del 80 porciento de la informacion mayor que 5
