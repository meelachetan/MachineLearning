getwd()
setwd("C:/Users/Vandita/Downloads")
library(readxl)
Data_Train <- read_excel("Data_Train.xlsx")
View(Data_Train)

Data_Train1 <- Data_Train

#Name

length(unique(Data_Train$Name))

Data_Train$Name <- NULL

#Location

class(Data_Train$Location)
unique(Data_Train$Location)

Data_Train$Location <- as.factor(Data_Train$Location)

table(Data_Train$Location)

levels(Data_Train$Location)

subset(Data_Train$Location,is.na(Data_Train$Location) == TRUE)

#Year


class(Data_Train$year1)
Data_Train$year1 <- Data_Train$Year
unique(Data_Train$Year)

Data_Train$year1 <- as.factor(Data_Train$year1)


#Kilometers_Driven

class(Data_Train$Kilometers_Driven)

summary(Data_Train$Kilometers_Driven)

out_kd <- quantile(Data_Train$Kilometers_Driven,probs = seq(0.95,1,0.01))

Data_Train$Kilometers_Driven[Data_Train$Kilometers_Driven > out_kd[5]] <- out_kd[5]

#subset(Data_Train$Kilometers_Driven,is.na(Data_Train$Kilometers_Driven) == TRUE)

summary(Data_Train$Kilometers_Driven)


#Fuel_Type

class(Data_Train$Fuel_Type)

unique(Data_Train$Fuel_Type)

table(Data_Train$Fuel_Type)

Data_Train$Fuel_Type1 <- Data_Train$Fuel_Type

Data_Train$Fuel_Type1 <- as.factor(Data_Train$Fuel_Type1)

Data_Train$Fuel_Type <- str_replace_all(Data_Train$Fuel_Type,"LPG","CNG")


Data_Train$Fuel_Type <- as.factor(Data_Train$Fuel_Type)

#Transmission

class(Data_Train$Transmission)

unique(Data_Train$Transmission)

Data_Train$Transmission <- as.factor(Data_Train$Transmission)

subset(Data_Train$Transmission,is.na(Data_Train$Transmission) == TRUE)

#Owner_Type

Data_Train$Owner_Type1 <- Data_Train$Owner_Type

Data_Train$Owner_Type2 <- Data_Train$Owner_Type1

class(Data_Train$Owner_Type)

unique(Data_Train$Owner_Type)

Data_Train$Owner_Type <- factor(Data_Train$Owner_Type,order=T,levels = c( "First","Second","Third","Fourth & Above") )
summary(Data_Train$Owner_Type1)

#Data_Train$Owner_Type1 <- as.factor(Data_Train$Owner_Type1)


#Mileage
class(Data_Train$Mileage)

Data_Train$Mileage1 <- Data_Train$Mileage

table(Data_Train$Mileage)

Data_Train$Mileage <- str_replace_all(Data_Train$Mileage," km/kg","")

Data_Train$Mileage <- str_replace_all(Data_Train$Mileage," kmpl","")

Data_Train$Mileage <- as.numeric(Data_Train$Mileage)

summary(Data_Train$Mileage)

subset(Data_Train$Mileage1,is.na(Data_Train$Mileage1) == TRUE)

#Engine

class(Data_Train$Engine)

Data_Train$Engine1 <- Data_Train$Engine

subset(Data_Train$Engine,is.na(Data_Train$Engine) == TRUE)

Data_Train$Engine <- str_replace_all(Data_Train$Engine," CC","")

unique(Data_Train$Engine)

Data_Train$Engine <- as.numeric(Data_Train$Engine)

summary(Data_Train$Engine)

out_engine <- quantile(Data_Train$Engine,probs = seq(0.95,1,0.01),na.rm = TRUE)

Data_Train$Engine[Data_Train$Engine>out_engine[5]] <- out_engine[5] 

summary(Data_Train$Engine)

#Power

class(Data_Train$Power)

unique(Data_Train$Power)

Data_Train$Power <- str_replace_all(Data_Train$Power," bhp","")

Data_Train$Power <- as.numeric(Data_Train$Power)

summary(Data_Train$Power)

#Seats

class(Data_Train$Seats)

summary(Data_Train$Seats)

unique(Data_Train$Seats)

Data_Train$Seats1 <- Data_Train$Seats
Data_Train$Seats <- Data_Train$Seats1

Data_Train$Seats <- as.factor(Data_Train$Seats)
summary(Data_Train$Seats)


#New_Price

class(Data_Train$New_Price)


length(subset(Data_Train$New_Price,is.na(Data_Train$New_Price) == FALSE))

Data_Train$New_Price <- NULL

#Price

class(Data_Train$Price)


sapply(Data_Train,function(x){sum(is.na(x))})
simple1 <- simple
simple <- Data_Train[,7:10]
str(simple)

impute <- mice(simple,m=5)
impute_final <- complete(impute)
summary(impute_final)

sapply(impute_final,function(x){sum(is.na(x))})

Data_Train$Mileage <- simple$Mileage
Data_Train$Mileage <- impute_final$Mileage

subset(Data_Train$Mileage,is.na(Data_Train$Mileage)==TRUE)

Data_Train$Engine <- impute_final$Engine

Data_Train$Power <- impute_final$Power

Data_Train$Seats <- impute_final$Seats

summary(Data_Train$Seats)

Data_Train$Seats <- as.factor(Data_Train$Seats)

sapply(Data_Train,function(x){sum(is.na(x))}) 

Data_Train_imputed <- Data_Train

Data_Train_imputed$Fuel_Type1 <- NULL
Data_Train_imputed$Owner_Type1 <- NULL
Data_Train_imputed$Mileage1 <- NULL
Data_Train_imputed$Engine1 <- NULL
Data_Train_imputed$Seats1 <- NULL

sapply(Data_Train_imputed,function(x){sum(is.na(x))}) 
str(Data_Train_imputed)
cor(Data_Train_imputed$Engine,Data_Train_imputed$Power)
plot(Data_Train_imputed$Engine,Data_Train_imputed$Power)

View(Data_Train_imputed)
table(train$Seats)
table(test$Seats)
#Model

set.seed(1)
summary(train)
split<-sample.split(Data_Train_imputed$Price,SplitRatio = 0.7)
train<-subset(Data_Train_imputed,split==TRUE)
test<-subset(Data_Train_imputed,split==FALSE)
model1 <- lm(Price ~ .-Year-Engine-Owner_Type,data=train)

summary(model1)

summary(train)

write.csv(train,"C://Users//Vandita//Downloads//out_train.csv",col.names = TRUE)

Price_pred <- predict(model1,newdata = test)

test1 <- test

which(test$Seats == 0)

test[1090,]$Seats <- 5


SSE = sum((test$Price-Price_pred)^2)
SST = sum((test$Price-mean(train$Price))^2)
1-SSE/SST











































































