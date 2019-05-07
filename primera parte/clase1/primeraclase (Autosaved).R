##firts homework of multivariable statistics curse
##the homework is about to univariable statistics analysis and plotting 
library(tidyverse) ##usseful library to data science https://www.tidyverse.org/learn/
setwd('Downloads/') ## put working directory 

datos<-read.csv(file="RegMultiv.csv",header=T, sep=";") #the data is read 
head(datos).   #show part of the data 
price = datos['price'];    #list a column of dataframe in a new variable 

library(ggplot2).  #install.packages('ggplot2')
qplot(log(price),data=datos,geom="density") #Density trace
qplot(log(price),data=datos)   # just x supplied = histogram

#boxplot
ggplot(data = datos, aes(x = "", y = price)) + 
+   geom_boxplot() + 
+   theme(axis.title.x = element_blank())

qplot(price, num, data = datos) #scatterplot

quantile(datos$price, c(0.25, 0.5, 0.75), type = 1) # Quartile
  
summary(datos['price']) # a summary of minimum, maximun, main quantiles, mean
sd(datos$price) ##standar deviation of data 
str(datos)  ## structure of data 
   
   #kurtosis and skewness
install.packages("fBasics")
library(fBasics)
skewness(datos['price'])
kurtosis(datos['price'])
sd(datos$price)
   
   #frecuency tables 
install.packages("epiDisplay")
library(epiDisplay)
tab1(datos$price, sort.group = "decreasing", cum.percent = TRUE)

install.packages("summarytools")
library(summarytools)
summarytools::freq(datos$price, order = "freq")

  