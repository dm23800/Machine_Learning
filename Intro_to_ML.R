#Intro to ML
data(iris)
View(iris)
colnames(iris)
levels(iris$Species)
str(iris)
qplot(Petal.Length,Petal.Width,color=Species,data=iris)
setInternet2()
install.packages("class")
library(class)
dim(iris)
head(iris)
ind <- sample(2,nrow(iris),replace=T,prob=c(.6,.4))
View(ind)

prc_train <- iris[ind==1,1:4]
View(prc_train)
prc_test <- iris[ind!=1,1:4]

prc_train_labels <-iris[ind==1,5]
View(prc_train_labels)
prc_test_lables <-iris[ind!=1,5]
View(prc_test_lables)


###########KNN Algorithm############
set.seed(100)
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=10)
library(gmodels)
CrossTable(prc_test_pred,prc_test_lables,prop.chisq = FALSE)
?CrossTable
###########Random Forest Algorithm############
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)

install.packages('e1071', dependencies=TRUE)
library(e1071)
set.seed(100)
modfit <- train(prc_train,prc_train_labels,method="rf")
prc_test_pred=predict(modfit,prc_test)
CrossTable(prc_test_pred,prc_test_lables,prop.chisq=FALSE)

########SVM#########
svm_train <- svm(prc_train,prc_train_labels,cost=1000,gamma=0.001)
prc_test_pred=predict(svm_train,prc_test)
CrossTable(prc_test_pred,prc_test_lables,prop.chisq=FALSE)


########GBM######



