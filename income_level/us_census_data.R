setwd("G:\\Learn\\DATASCIENCE\\us-census-data")
names(adult_training1)<-c("Age","Workclass","fnlwgt","Education","Education_number","Marital_status","Occupation","Relationship","Race","Gender","Capital_gain","Capital_loss","Hours_per_week","Native_country","Salary")
adult_training1 = adult_training
summary(adult_training1)
#Age

class(adult_training1$Age)
summary(adult_training1$Age)
table(adult_training1$Age)
#Occupation

class(adult_training1$Occupation)
table(adult_training1$Occupation)


k <- subset(adult_training1$Occupation,adult_training1$Occupation == "?")


adult_training1<-adult_training1[-which(adult_training1$Occupation == "?"),]
adult_training1$Occupation<-as.factor(adult_training1$Occupation)

#Work class

class(adult_training1$Workclass)

table(adult_training1$Workclass)

adult_training1$Workclass <- as.factor(adult_training1$Workclass)



#fnlwgt

class(adult_training1$fnlwgt)
summary(adult_training1$fnlwgt)

quantile(adult_training1$fnlwgt,seq(0,1,0.05))

out_fnlwgt <- quantile(adult_training1$fnlwgt,seq(0.95,1,0.01))

adult_training1$fnlwgt <- ifelse(adult_training1$fnlwgt>out_fnlwgt[5],out_fnlwgt[5],adult_training1$fnlwgt)

#Education

class(adult_training1$Education)
summary(adult_training1$Education)

#Education_Number

adult_training1$Education_number <- as.factor(adult_training1$Education_number)

#Marital Status

class(adult_training1$Marital_status)
which(is.na(adult_training1$Marital_status))
adult_training1$Marital_status <- as.factor(adult_training1$Marital_status)


#Country
table(adult_training1$Native_country)
#adult_training_dummy<-adult_training1
unique(adult_training1$Native_country)
#adult_training1<-adult_training1[-which(adult_training1$Native_country == "?"),]
str(adult_training1)

adult_training1$Native_country <- as.factor(adult_training1$Native_country)


#RelationShip
table(adult_training1$Relationship)
adult_training1$Relationship<-as.factor(adult_training1$Relationship)

#RACE
table(adult_training1$Race)
adult_training1$Race<-as.factor(adult_training1$Race)
adult_training1$Race<-str_replace_all(adult_training1$Race,fixed("Other"),"White")

#GENDER
table(adult_training1$Gender)
adult_training1$Gender<-as.factor(adult_training1$Gender)

#Capital gain
class(adult_training1$Capital_gain)
summary(adult_training1$Capital_gain)
table(adult_training1$Capital_gain)
out_fnlwgt<-quantile(adult_training1$Capital_gain,seq(0.95,1,0.01))
adult_training1$Capital_gain <- ifelse(adult_training1$Capital_gain>out_fnlwgt[5],out_fnlwgt[5],adult_training1$Capital_gain)


#Capital loss
summary(adult_training1$Capital_loss)
out_fnlwgt<-quantile(adult_training1$Capital_loss,seq(0.95,1,0.01))

#hours
summary(adult_training1$Hours_per_week)
quantile(adult_training1$Hours_per_week,seq(0.95,1,0.001))

#Salary
adult_training1$Salary<-ifelse(adult_training1$Salary =='<=50K',0,1)
table(adult_training1$Salary)
class(adult_training1$Salary)
View(adult_training1)

#Train Test splitting
library(caTools)
set.seed(300)
spl <- sample.split(adult_training1$Salary,SplitRatio = 0.7)
train_data<-subset(adult_training1,spl == TRUE)
test_data<-subset(adult_training1,spl==FALSE)
View(train_data)

train_data_dummy<-train_data
test_data_dummy<-test_data

#Decision Tree Model Building
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

model1=rpart(Salary~., data = train_data,method = "class",minbucket=50)
prp(model1)
summary(model1)

predict_out <- predict(model1,newdata = test_data,type = "class")

table(test_data$Salary,predict_out)
(6468+1162)/(6468+1162+328+1090)

table(adult_training1$Salary)

model1$cptable

model_log <- glm(Salary ~ . ,data = train_data,family = binomial)
predict_log <- predict(model_log,newdata = test_data,type = "response")
table(test_data$Salary,predict_log>=0.5)
(6307+1377)/(6307+1377+875+489)


library(caret)

cpGrid <- expand.grid(mtry=seq(1,6,1))

model_rf_caret <- train(as.factor(Salary) ~ .,data = train_data,method = 'rf',tuneGrid = cpGrid,ntree = 200)

library(randomForest)

model_rf <- randomForest((Salary) ~ Age+Workclass+fnlwgt+Education_number+Marital_status+Occupation+Relationship+Race+Gender+Capital_gain+Capital_loss+Hours_per_week+Native_country,data = train_data,ntree = 200,nodesize = 25)
str(train_data)
#Boosting

sparse_mat_train <- sparse.model.matrix(Salary ~ .-1,data = train_data)
dtrain <- xgb.DMatrix(sparse_mat_train,label = train_data$Salary)
class(dtrain)
View(dtrain)

sparse_mat_test <- sparse.model.matrix(Salary ~ .-1,data = test_data)
dtest <- xgb.DMatrix(sparse_mat_test,label = test_data$Salary)
class(dtest)

watchlist = list(train = dtrain,test = dtest)

model_boost <- xgb.train(data=dtrain,eta=0.01,max_depth=1,objective="binary:logistic",nrounds = 2000,verbose=2,watchlist=watchlist)

xgb_predict <- predict(model_boost,newdata = dtest )

table(test_data$Salary,xgb_predict >= 0.5)

6545+1119

7664/(251+1133+7664)

(6791+330)/9048
6520+1210
7730/9048


cv<-xgb.cv(data=dtrain,eta=0.01,max_depth=1,objective="binary:logistic",verbose=1,nfold = 5,prediction = T,nround=2000)

xgb_predict_cv <- predict(cv,newdata = dtest )

table(test_data$Salary,xgb_predict >= 0.5)



