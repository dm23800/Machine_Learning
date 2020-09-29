getwd()
View(mtcars)
setwd("/Users/debashree/Desktop/R_Training/")

#Creating vectors manually using the c function

a <- c(6,3,18.4)

titanic <-read.csv("train.csv")
titanic2 <- read.table("train.csv",sep=',',header = TRUE)
class(titanic2)
males <- subset(titanic,Sex=="male")
class(males)
View(males)

males2 <- titanic[which(titanic$Sex=="male"),]
View(males2)
colnames(titanic)


#Subset only two columns with the required filter
name_age <- titanic[,c("Name","Age")]
View(name_age)
name_age <- titanic[,c(4,6)]

#To Select consecutive columns
name_age <- titanic[,c(3:5)]
View(name_age)

#Reading the var names into another variable
varnames <- names(titanic)
varnames

#Exploring the dataset
str(titanic)

#First 10 and Last 10 rows
head(titanic,n=10)
tail(titanic,n=10)

#Number of rows and columns in a dataset
dim(titanic)
nrow(titanic)
ncol(titanic)

#summary of dataset
summary(titanic)

#Unique Values within each var
unique(titanic$Embarked)

#Unique rows within each class of var
t <- table(titanic$Sex)
t
class(t)
unique(titanic$Survived)

#Subsetting distinct combinations of embarkment and sex
install.packages("dplyr")
library(dplyr)

dist_eg <- distinct(titanic,Survived,Sex)
dist_eg

#Datatype manipulation

class(titanic)
is.character(titanic$Name)
is.integer(titanic$SibSp)       
class(titanic$Name)

#Typecasting
titanic$value_char = as.character(titanic$Name)
class(titanic$value_char)

str(titanic)

titanic$value_char=as.integer(titanic$value_char)
str(titanic)
head(titanic)

#Ordering a dataset
titanic_basedonfare <- titanic[order(-titanic$Fare),]
titanic_basedonfare_asc <- titanic[order(titanic$Fare),]

head(titanic$Fare)
head(titanic_basedonfare$Fare)
head(titanic_basedonfare_asc$Fare)

#Dropping Elements
#Assigning Null Values
A <- titanic[-c(1:3),]
A

#Using sqldf
install.packages("sqldf")
library(sqldf)
colnames(titanic)
em_fare <- sqldf("select Name, Embarked,Fare from titanic")
em_fare

colnames(titanic)[1] <- "PassID"
colnames(titanic)

colnames(titanic)[c(1,5,6)] = c("Passid","Sex","Age")
colnames(titanic)
colnames(titanic)[1]

install.packages("gmodels")
library("gmodels")

#Missing Value Treatment
missing_fun = function (a) {return 
  (length(a)- length(a[!is.na(a)]))
}

ncol(titanic)
titanic <- titanic[,c(1:12)]
View(titanic)

#Create a table for basic data exploration
#Viewing the number of missing in each var
missing_values <- data.frame(vars=names(titanic),N=apply(titanic,2,length),
                             nmiss=apply(titanic,2,missing_fun))
View(missing_values)

#Table of NA T/F Flags
is_na_table <- is.na(titanic)
View(is_na_table)
nrow(is.na(titanic))
sum(is.na(titanic))

#Substituting Null Values with Avg
titanic[which(is.na(titanic$Fare==TRUE)),"Fare"] <- mean(titanic$Fare,na.rm=TRUE)

#Substituting Null values with zeroes
titanic[which(is.na(titanic$fare))] <-0

#writing the table to local
write.csv (titanic,"titanic_filtered.csv",row.names = FALSE)

#Merging/Joining Tables
#Add a column of FC_NAME in the dataset
colnames(titanic)
titanic_1 <- titanic[50:100,c("Passid","Sex")]
View(titanic_1)
titanic_2 <- titanic[75:125,c("Passid","Age","Survived")]
#Inner Join
titanic_3 <- merge(titanic_1,titanic_2,by.x="Passid")
View(titanic_3)

#Outer Join
titanic_4 <- merge(titanic_1,titanic_2,by.x="Passid",all=T)

#Left Outer Join
titanic_5 <- merge(titanic_1,titanic_2,by.x="Passid",all.x=T)

#Right Outer Join
titanic_6 <- merge(titanic_1,titanic_2,by.x="Passid",all.y=T)

#Cross Join
titanic_7 <- merge(titanic_1,titanic_2,by=NULL)

#Pattern Matching
titan_brac <- grep("P",titanic$Ticket)
titan_brac

#To see the values at those indices
titan_brac <- grep("P",titanic$Ticket,value=TRUE)
class(titan_brac)

#sub(pattern,replacement,x)
#gsub(pattern,replacement,x)

head(titanic$Name)

#Replace only first occurence
titan_sub=sub("A","/",titanic$Name,ignore.case=TRUE)
View(titan_sub)

#Replace all occurences
titan_sub1=gsub("A","/",titanic$Name,ignore.case=TRUE)
View(titan_sub1)

#substr(x,start,stop)
titanic$Name[1:4]
titanic_substr <- substr(titanic$Name[1:4],1,3)
titanic_substr
colnames(titanic)
#aggregate function
titanic_aggre <-aggregate(titanic$Passid,by =
                            list(titanic$Embarked,titanic$Sex),FUN = length )

colnames(titanic_aggre) = c("Embarked","Sex","#Passengers")
View(titanic_aggre)

titanic_aggre2 <-aggregate(titanic$Fare,by= list(titanic$Embarked,titanic$Sex),function(x) mean(x))
head(titanic_aggre2)


#reshape data
library(reshape2)

#acast returns matric
#dcast returns dataframe

titanic_acast <- acast(titanic_aggre,Embarked~Sex,sum)
titanic_acast <- dcast(titanic_aggre,Embarked~Sex,sum)

#############Logistic Regression############
#Read csv

titanic1 <-read.csv("train.csv")
View(titanic1)
colnames(titanic1)
str(titanic1)
titanic1$cabinavailability <- ifelse(titanic$Cabin == "","NOT AVAILABLE","AVAILABLE")
titanic1$Survived <- as.factor(titanic1$Survived)
titanic1$Pclass <- as.factor(titanic1$Pclass)
titanic1$cabinavailability <- as.factor(titanic1$cabinavailability)
class(titanic1$cabinavailability)

#Train and Test
set.seed(1)
train <- sort(sample(nrow(titanic1),0.7*nrow(titanic1)))
View(train)
?sample
dev <- titanic1[train,]
test <- titanic1[-train,]


sapply(test,class)
?sapply

#Fitting a logistic regression model - fitting binomial
fit <- glm(Survived ~ Pclass + Sex + Age + cabinavailability, data=dev , family=binomial)
summary(fit)

#predicting the probability of survival
test$pred <- predict(fit,test,type="response")
?predict

#preparing the confusion matrix
conf_matrix <-table (test$Survived,test$pred >0.5)
View(conf_matrix)
rownames(conf_matrix)=paste ("Actual",rownames(conf_matrix),sep=":") 
colnames(conf_matrix)=paste("Pred",colnames(conf_matrix),sep=":")  
View(conf_matrix)  

print(conf_matrix)  

accuracy <- (conf_matrix[1,1]+ conf_matrix[2,2])/
  (conf_matrix[1,1]+ conf_matrix[2,2]+conf_matrix[1,2]+ conf_matrix[2,1]) 
print(accuracy)


#######Clustering#########
food = read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv")
View(food)
str(food)
table(food$Country)
summary(food)
dim(food)
is.na(food)
sapply(food,class)
head(food)
set.seed(1)

#the nstart algorithm tell the k means to do that many random starts and then keep the best
#with 20 25 random starts you will generally get the best population

food[,c("WhiteMeat","RedMeat")]
grpmeat <- kmeans(food[,c("WhiteMeat","RedMeat")],centers=3 ,nstart=10)
?kmeans
grpmeat

plot(food$RedMeat,food$WhiteMeat,type= "n",xlim=c(3,19),xlab = "Red Meat",ylab= "White Meat")
text(x=food$RedMeat,y=food$WhiteMeat,labels=food$Country,col=grpmeat$cluster+1)


#Clustering 2
#same analysis but now with clusters of all proten groups, n=7
set.seed(1)
food[,-1] # All X's
grpprotein <-kmeans(food[,-1],centers=7,nstart=10)
grpprotein
grpprotein$cluster
unique(grpprotein$cluster)

plot(food$RedMeat,food$WhiteMeat,type="n",xlim=c(3,19),xlab="Red Meat",ylab="White Meat")
text(x=food$RedMeat,y=food$WhiteMeat,labels=food$Country,col=grpprotein$cluster+1)

#Elbow method for finding the optimal clusters
set.seed(123)
#Compute and plote wss from k=2 to k=8
k.max <-8
data <-food[,-1]
wss <- sapply(1:k.max,function (k){kmeans(data,k,nstart=10)$tot.withinss})
wss
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab= "Number of Cluster K",
     ylab="Total within clusters sum of squares")

#Optimal k is 2 here

###############
install.packages(car)
library(car)
library(ggplot2)
library(MASS)

Prestige <-Prestige
View(Prestige)
str(Prestige)
head(Prestige)
summary(Prestige)

subset.data=subset(Prestige,select=c("education","income","women","prestige"))

#Drawing a scatterplot matrix of edu,income,women and prestige using the pairs function
pairs(subset.data,pch=16,col="blue",main="matrix Scatterplot of edu, income, women and prestige")
?pairs

#Picture of relationship between education and income
plot(subset.data$education,subset.data$income,xlab="Education",ylab="Income",col="blue",
     pch=16,main="Scatterplot of education vs Income")

#Same plot done nicely with ggplot2
ggplot(subset.data) + geom_point(aes(x=education,y=income),col="blue",size=3)+
  ggtitle("Education vs Income Scatterplot")+theme(plot.title=element_text(hjust=.5))
  
#Calculate the slope and intercept of line of best fit
coef(lm(Income ~ Education, data=subset.data))

#Barplot
plot(Prestige$type,main="Bar plot of occupational types",col="cyan",ylab="count")
  
###########Regression###########
###SLRM
#Histogram on Education
IncomeHist <- ggplot(Prestige,aes(Income)) +geom_histogram()
IncomeHist+ geom_histogram(fill="blue",colour="green")


train=sample(1:nrow(Prestige),floor(0.8*nrow(Prestige)))
traindata = Prestige[train,]
testdata = Prestige[-train,]
View(testdata)
m1=lm(income ~ education,data=traindata)
summary(m1)
anova(m1)
pred = predict(m1,newdat=testdata)
library(ModelMetrics)

#Calculating train rmse
pred_train =predict(m1,newdat=traindata)
obs_train=traindata$income
rmse(obs_train,pred_train)

#Calculating test rmse
pred_test =predict(m1,newdat=testdata)
obs_test=testdata$income
rmse(obs_test,pred_test)

anova(m1)

m2=lm(income ~ education + I(education*education),data=traindata)
?lm
summary(m2)

#Calculating train rmse
pred_train =predict(m2,newdat=traindata)
obs_train=traindata$income
rmse(obs_train,pred_train)

#Calculating test rmse
pred_test =predict(m2,newdat=testdata)
obs_test=testdata$income
rmse(obs_test,pred_test)

#Plot a Correlation Graph
datacor=cor(Prestige[1:4])
install.packages("corrplot")
library(corrplot)
corrplot(datacor,method="number")

#MLRM#
m2=lm(income ~ education + I(education*education),data=traindata)
colnames(traindata)
mlrm_m1 =lm(income ~ education + I(education*education)+ women, data=traindata)
summary(mlrm_m1)
mlrm_m2 =lm(income ~ education + I(education*education)+ women+prestige, data=traindata)
summary(mlrm_m2)

mlrm_m3 =lm(log(income) ~ education + I(education*education)+ women+prestige, data=traindata)
summary(mlrm_m3)

#Calculating Train rmse
pred_train2=predict(mlrm_m3,newdata=traindata)
obs_train2=traindata$income
rmse(obs_train2,pred_train2)

#Calculating Test rmse
pred_test2=predict(mlrm_m3,newdata=testdata)
obs_test2=testdata$income
rmse(obs_test2,pred_test2)

#Residual Plots
plot(mlrm_m1,pch=16,which=1)
plot(mlrm_m2,pch=16,which=1)
install.packages("ggplot2")
install.packages("devtools")
sessionInfo()
install.packages("KernSmooth")
library(KernSmooth)
devtools.aes()
devtools.check()

