## Importing packages
library(tidyverse) 
library(MASS) 
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(ROCR)

emp_general<-read.csv("../input/general_data.csv", stringsAsFactors = F)
emp_suvey<-read.csv("../input/employee_survey_data.csv",stringsAsFactors = F)
emp_manager_suvey<-read.csv("../input/manager_survey_data.csv",stringsAsFactors = F)
emp_intime<-read.csv("../input/in_time.csv",stringsAsFactors = F)
emp_outime<-read.csv("../input/out_time.csv",stringsAsFactors = F)


#Exploring the dataset
str(emp_general) 
str(emp_suvey)   
str(emp_manager_suvey) #4410 obs. of  3 variables
str(emp_intime)    #4410 obs. of  262 variables
str(emp_outime)   


colnames(emp_intime)[1]<-"EmployeeID"
colnames(emp_outime)[1]<-"EmployeeID"

#Exploring all the dataframes
length(unique(tolower(emp_general$EmployeeID)))   
length(unique(tolower(emp_suvey$EmployeeID)))     
length(unique(tolower(emp_manager_suvey$EmployeeID))) 
length(unique(tolower(emp_intime$EmployeeID)))        
length(unique(tolower(emp_outime$EmployeeID)))        

setdiff(emp_general$EmployeeID,emp_suvey$EmployeeID)  
setdiff(emp_general$EmployeeID,emp_manager_suvey$EmployeeID)
setdiff(emp_general$EmployeeID,emp_intime$EmployeeID)   
setdiff(emp_general$EmployeeID,emp_outime$EmployeeID)   

emp_id<-emp_intime[1]
emp_intime <- as.data.frame(lapply( emp_intime[, -1], as.POSIXlt))
emp_outime <- as.data.frame(lapply( emp_outime[, -1], as.POSIXlt))

setdiff(colnames(emp_intime), colnames(emp_outime)) 

#calculating difference of times to get actual login hours
emp_login <-  emp_outime-emp_intime


emp_login <- as.data.frame(lapply(emp_login, round, digits=2))

#To delete all such columns when everybody was NA's. 
emp_login <- emp_login[,colSums(is.na(emp_login))<nrow(emp_login)]
emp_login<-cbind(emp_id,emp_login)

#Converting all the columns into numeric
emp_login<- as.data.frame(sapply(emp_login, as.numeric))


emp_login$emp_avg_login <- apply(emp_login[,-1], 1, mean, na.rm=TRUE)


boxplot(emp_login$emp_avg_login)
emp_login$emp_avg_login<-round(emp_login$emp_avg_login, 0)

emp_login$emp_login_cat<- ifelse(emp_login$emp_avg_login > 8, "overtime", ifelse(emp_login$emp_avg_login>7 & emp_login$emp_avg_login<=8,"regular", "early logout"))

#extrating only 3 columns to merge with other dataset's
emp_login<-emp_login[, c(1,251,252)]

#Merging all the dataset and creating master dataframe as "hrdata"
hrdata<-merge(emp_general,emp_suvey, by="EmployeeID", all = F)
hrdata<- merge(hrdata,emp_manager_suvey, by="EmployeeID", all = F)
hrdata<- merge(hrdata,emp_login, by="EmployeeID", all = F)


str(hrdata)

#Missing values 
sum(is.na(hrdata))
(colSums(is.na(hrdata))/nrow(hrdata))*100  

#NA value computation
hrdata$NumCompaniesWorked[which(is.na(hrdata$NumCompaniesWorked))]<-median(hrdata$NumCompaniesWorked, na.rm = TRUE)
hrdata$TotalWorkingYears[which(is.na(hrdata$TotalWorkingYears))]<-median(hrdata$TotalWorkingYears, na.rm = TRUE)
hrdata$EnvironmentSatisfaction[which(is.na(hrdata$EnvironmentSatisfaction))]<-median(hrdata$EnvironmentSatisfaction, na.rm = TRUE)
hrdata$JobSatisfaction[which(is.na(hrdata$JobSatisfaction))]<-median(hrdata$JobSatisfaction, na.rm = TRUE)
hrdata$WorkLifeBalance[which(is.na(hrdata$WorkLifeBalance))]<-median(hrdata$WorkLifeBalance, na.rm = TRUE)

sum(is.na(hrdata)) 

hrdata<-hrdata[, -c(9,16,18)]

hrdata$Education[which(hrdata$Education==1)]<-'Below College'
hrdata$Education[which(hrdata$Education==2)]<-'College'
hrdata$Education[which(hrdata$Education==3)]<-'Bachelor'
hrdata$Education[which(hrdata$Education==4)]<-'Master'
hrdata$Education[which(hrdata$Education==5)]<-'Doctor'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==1)]<-'Low'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==2)]<-'Medium'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==3)]<-'High'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==4)]<-'Very High'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==1)]<-'Low'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==2)]<-'Medium'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==3)]<-'High'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==4)]<-'Very High'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==1)]<-'Low'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==2)]<-'Medium'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==3)]<-'High'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==4)]<-'Very High'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==1)]<-'Bad'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==2)]<-'Good'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==3)]<-'Better'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==4)]<-'Best'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==1)]<-'Low'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==2)]<-'Good'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==3)]<-'Excellent'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==4)]<-'Outstanding'


##################Explorartory Data Analysis ##############

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="none")

plot_grid(ggplot(hrdata, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar(position ="fill"),ggplot(hrdata, aes(x=factor(Department),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(Gender),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,align = "h") 


plot_grid(ggplot(hrdata, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(position ="fill"), ggplot(hrdata, aes(x=factor(emp_login_cat),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(Education),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,align = "h") 

plot_grid(ggplot(hrdata, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(position ="fill"), ggplot(hrdata, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,ggplot(hrdata, aes(x=factor(JobRole),fill=Attrition))+ geom_bar(position ="fill")+bar_theme1,align = "h") 



box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(),legend.position="none")

plot_grid(ggplot(hrdata, aes(Age))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)



plot_grid(ggplot(hrdata, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)



plot_grid(ggplot(hrdata, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$MonthlyIncome, seq(0,1,0.01))



plot_grid(ggplot(hrdata, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)



plot_grid(ggplot(hrdata, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$TotalWorkingYears, seq(0,1,0.01))




plot_grid(ggplot(hrdata, aes(hrdata$YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$YearsSinceLastPromotion, seq(0,1,0.01))


plot_grid(ggplot(hrdata, aes(hrdata$YearsAtCompany))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$YearsAtCompany, seq(0,1,0.01))



plot_grid(ggplot(hrdata, aes(hrdata$YearsWithCurrManager))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$YearsWithCurrManager, seq(0,1,0.01))


plot_grid(ggplot(hrdata, aes(hrdata$emp_avg_login))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$emp_avg_login))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$emp_avg_login, seq(0,1,0.01))

plot_grid(ggplot(hrdata, aes(hrdata$JobLevel))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$JobLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)



plot_grid(ggplot(hrdata, aes(hrdata$NumCompaniesWorked))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$NumCompaniesWorked, seq(0,1,0.01))




plot_grid(ggplot(hrdata, aes(hrdata$StockOptionLevel))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$StockOptionLevel, seq(0,1,0.01))



plot_grid(ggplot(hrdata, aes(hrdata$TrainingTimesLastYear))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)
quantile(hrdata$TrainingTimesLastYear, seq(0,1,0.01))


#####Outlier treatment

box <- boxplot.stats(hrdata$YearsAtCompany)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsAtCompany %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$emp_avg_login)
out <- box$out
ad1 <- hrdata[ !hrdata$emp_avg_login %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$NumCompaniesWorked)
out <- box$out
ad1 <- hrdata[ !hrdata$NumCompaniesWorked %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$StockOptionLevel)
out <- box$out
ad1 <- hrdata[ !hrdata$StockOptionLevel %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$TotalWorkingYears)
out <- box$out
ad1 <- hrdata[ !hrdata$TotalWorkingYears %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$TrainingTimesLastYear)
out <- box$out
ad1 <- hrdata[ !hrdata$TrainingTimesLastYear %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$YearsSinceLastPromotion)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsSinceLastPromotion %in% out, ]
hrdata <- ad1
box <- boxplot.stats(hrdata$YearsWithCurrManager)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsWithCurrManager %in% out, ]
hrdata <- ad1


# Correlation between numeric variables
library(GGally)
ggpairs(hrdata[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike","TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "emp_avg_login","JobLevel","StockOptionLevel","TrainingTimesLastYear")])


hrdata$EmployeeID<-as.factor(hrdata$EmployeeID)

num_var<-sapply(hrdata, is.numeric)
hrdata[num_var]<-lapply(hrdata[num_var], scale)
hrdata$EmployeeID<-as.numeric(hrdata$EmployeeID)


hrdata$Attrition<- ifelse(hrdata$Attrition=="Yes",1,0)

Attritionrate <- sum(hrdata$Attrition)/nrow(hrdata)
Attritionrate   

str(hrdata)


Cat_var<- hrdata[, c("Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus", "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating", "emp_login_cat")]


hrdata_factor<- data.frame(sapply(Cat_var, function(x) factor(x)))
str(hrdata_factor)


dummies<- data.frame(sapply(hrdata_factor, function(x) data.frame(model.matrix(~x-1,data =hrdata_factor))))


hrdata<-hrdata[, !(names(hrdata)) %in% c( "Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus",  "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating", "emp_login_cat","EmployeeID")]

hrdata_final<-cbind(hrdata,dummies)
str(hrdata_final) #2228 obs. of 66 variables

# splitting the data between train and test
set.seed(100)
indices = sample.split(hrdata_final$Attrition, SplitRatio = 0.7)
train = hrdata_final[indices,]
test = hrdata_final[!(indices),]

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 


library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2) 


library(car)
vif(model_2)


model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + JobLevel + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_3) 
vif(model_3)

#Remove BusinessTravel.xNon.Travel as it is not significant p=0.118110   >0.05   
model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + JobLevel + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_4) 
vif(model_4)


#Remove JobLevel as it is not significant p=0.107124     >0.05 
model_5<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_5) 
vif(model_5)

model_6<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_6) 
vif(model_6)


model_7<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_7)
vif(model_7)


model_8<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_8) 
vif(model_8)

 
model_9<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_9) 
vif(model_9)


model_10<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_10) 
vif(model_10)


model_11<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_11) 
vif(model_11)


model_12<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_12) 
vif(model_12)


model_13<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_13)
vif(model_13)


model_14<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director + JobRole.xResearch.Scientist + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_14) 
vif(model_14)



model_15<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_15) 
vif(model_15)


model_16<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_16) 
vif(model_16)

model_17<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_17) 
vif(model_17)


model_18<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences  + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_18) 
vif(model_18)


model_19<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_19) 
vif(model_19)


model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_20) 
vif(model_20)


model_21<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_21)
vif(model_21)


model_22<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_22) 
vif(model_22)


model_23<-glm(formula = Attrition ~  NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_23) 
vif(model_23)


model_24<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_24) 
vif(model_24)

model_25<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xLow +  emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_25) 
vif(model_25)


#summary(model_21) #AIC: 2174
########################################################################

# With 11 significant variables in the model
#All are highly significant and having VIF value less than 2
final_model<- model_25

#######################################################################
### Model Evaluation


test_pred = predict(final_model, type = "response", newdata = test[,-2])


test_pred
 
summary(test_pred)
test$prob <- test_pred
#View(test)

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_Attrition,test_pred_Attrition) 


test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)
test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf  
#Accuracy=85%
#Attrition Accuracy(Sensitivity)= 46%
#Non Attrition Accuracy(Specificity)=92%
#######################################################################


