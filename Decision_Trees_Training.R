#Decision Trees Training
install.packages("tree")
library(tree)
install.packages("ISLR")
library(ISLR)

attach(Carseats)
View(Carseats)
str(Carseats)
colnames(Carseats)
High=ifelse(Sales<=8,"No","Yes")
class(Carseats)
Carseats = data.frame(Carseats,High)
View(Carseats)

tree.Carseats=tree(High ~.-Sales,Carseats)
summary(tree.Carseats)
plot(tree.Carseats)
install.packages("ggplot2")
library(ggplot2)
text(tree.Carseats,pretty=0)
tree.Carseats

#Building Train and Test
set.seed(3)
train=sample(1:nrow(Carseats),200)
train
View(train)
Carseats.test=Carseats[-train,]
High.test=High[-train]
View(High.test)

#building model on all except sales
tree.Carseats=tree(High~.-Sales,Carseats,subset=train)

#testing prediction
tree.pred=predict(tree.Carseats,Carseats.test,type="class")
table(tree.pred,High.test)

#Improving Results by Pruning
cv.carseats=cv.tree(tree.Carseats,FUN = prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=(c(1,2)))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.Carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
prune.carseats

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)


############Regression Trees###########
install.packages("MASS")
library(MASS)
set.seed(1)
Boston <-Boston
View(Boston)
train=sample(1:nrow(Boston),nrow(Boston)/2)
class(train)
nrow(train)
ncol(train)
View(train)
?sample
colnames(Boston)

tree.Boston=tree(medv~.,Boston,subset=train)
View(tree.Boston)
summary(tree.Boston)
plot(tree.Boston)
text(tree.Boston,pretty=0)

cv.boston=cv.tree(tree.Boston)
plot(cv.boston$size,cv.boston$dev,type="b")
prune.boston=prune.tree(tree.Boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.Boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)

abline(0,1)
View(abline)
mean((yhat -boston.test)*(yhat -boston.test))
mean(mtcars$mpg)


