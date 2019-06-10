library(caret)

datos<-read.csv(file=file.choose(),header=T,sep=',')
attach(datos)
head(datos)
head(datos)
length(ytrain)
x=datos[,c(1:26)]
y=as.ordered(target)

summary(x)
res.pca <- prcomp(x, scale = TRUE)
varpca=res.pca$x[,1:4]
var<-get_pca_var(res.pca)
    correlationanalisis(varpca)
get_eig(res.pca)
write.csv(varpca, file="scores_price.csv",row.names=FALSE, sep=',')
scores<-read.csv(file=file.choose(),header=T,sep=',')
attach(scores)
scores
scores["target"]<-NA
scores["target"]<-y




# 2. Partition data
#scores=datos
ind <- sample(2, nrow(scores), replace = TRUE, prob = c(0.8,0.2))
train <- scores[ind==1,]
test <- scores[ind==2,]
xtrain=train[,c(1:4)]
ytrain=as.ordered(train[,c(5)])
xtest=test[,c(1:4)]
ytest=as.ordered(test[,c(5)])



# 3. Ordinal Logitic Regression or Proportional Odds Logistic Regression
library(MASS)
model <- polr(target~ .-PC4, train, Hess = TRUE)

summary(model)

# 4. p-Value Calculation
(ctable <- coef(summary(model)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# 5. Prediction
pred <- predict(model, train)
print(pred, digits = 3)



# 6. Confusion Matrix & Error for Training data
(tab <- table(pred,ytrain))
1-sum(diag(tab))/sum(tab)



# 7. Confusion Matrix & Error for Test data
pred1 <- predict(model,test)
(tab1 <- table(pred1, ytest))
1-sum(diag(tab1))/sum(tab1)

#Assumptions
residMLG<-residuals(model, type="deviance") # residuals
drop1(model,test="Chi")

runs.test(residMLG)
brant(modelasdf)

# End


