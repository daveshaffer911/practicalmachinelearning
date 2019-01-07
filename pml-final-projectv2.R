#Created by Dave Shaffer - Jan 6, 2019 for the Practical Machine Learning Course Project

#use libraries
library(ggplot2)
library(caret)
library(sm)
library(corrplot)
library(randomForest)
library(pROC)

#set seed for reproducable results
set.seed(123)

#set working directory
setwd("c:/rstudo/projects/fml-final-project")

#read training and testing data csv file and store in dataframe
plmTrainingDdata <- read.csv(file = "pml-training.csv", stringsAsFactors = FALSE)
plmTestingDdata <- read.csv(file = "pml-testing.csv", stringsAsFactors = FALSE)

#explore the structure of the data
#str(plmTrainingDdata)

#check the dimension of the data
#dim(plmTrainingDdata)

#check out the first couple of rows of the data
#head(plmTrainingDdata,5)

#explore data using summary
#summary(plmTrainingDdata)

#Hypothesis 1 - Class B - throwing elbows forward - should see arm sensor anomaly - more movement than in Class A.
featurePlot(x=plmTrainingDdata[,c("accel_arm_x","accel_arm_y","accel_arm_z")], y=plmTrainingDdata$classe, plot="pairs")

qq=qplot(accel_arm_z, accel_arm_y,colour=classe, data= plmTrainingDdata)
qq+ geom_smooth(method='lm' , formula = y~x)

#Hypothesis 2 - Class C - lift dumbbell only half way -	should see wrist sensor anemonol- only see ? has much movement as in Class A, 
#Additionally, Class D - lowering the dumbbell only ? way should see wrist sensor anomaly - only see ? as much movement as in Class A
featurePlot(x=plmTrainingDdata[,c("roll_forearm","pitch_forearm","yaw_forearm")], y=plmTrainingDdata$classe, plot="pairs")

qplot(accel_dumbbell_y, yaw_dumbbell,colour=classe, data= plmTrainingDdata)
qq+ geom_smooth(method='lm' , formula = y~x)

#Hypothesis 3 - Class E - throwing hips forward -	should see belt sensor anomaly - should see forward movement where little or now movement should be seen in Class A
featurePlot(x=plmTrainingDdata[,c("roll_belt","pitch_belt","yaw_belt")], y=plmTrainingDdata$classe, plot="pairs")
qq=qplot(roll_belt, yaw_belt,colour=classe, data= plmTrainingDdata)
qq+ geom_smooth(method='lm' , formula = y~x)
qq

# plot densities 
sm.density.compare(plmTrainingDdata$roll_belt, plmTrainingDdata$classe)

densityplot(~ accel_belt_z, group = classe, data = plmTrainingDdata, auto.key = TRUE)
densityplot(~ accel_arm_y | user_name, group = classe, data = plmTrainingDdata, auto.key = TRUE)

ggplot(plmTrainingDdata) + geom_density(aes(x = roll_belt, color = classe)) 
ggplot(plmTrainingDdata) + geom_density(aes(x = roll_belt, fill = classe), alpha = 0.2)

featurePlot(x=plmTrainingDdata[,c("roll_belt","pitch_forearm","magnet_dumbbell_z")], y=plmTrainingDdata$classe, plot="pairs", auto.key = list(columns = 5))

featurePlot(x=plmTrainingDdata[,c("roll_belt","gyros_dumbbell_x","gyros_dumbbell_z")], y=plmTrainingDdata$classe, plot="pairs")
qq=qplot(roll_belt, pitch_forearm,colour=classe, data= plmTrainingDdata)
qq+ geom_smooth(method='lm' , formula = y~x)

#create a grouped histogram of all five classe variables
par(mfrow =c (3,2), mar = c(6,3,1,1))
hist(subset(plmTrainingDdata, classe=="A")$roll_belt, col="blue",xlim = c(0,175), ylim = c(0,2500),breaks = 20)
hist(subset(plmTrainingDdata, classe=="B")$roll_belt, col="blue",xlim = c(0,175), ylim = c(0,2500),breaks = 20)
hist(subset(plmTrainingDdata, classe=="C")$roll_belt, col="blue",xlim = c(0,175), ylim = c(0,2500),breaks = 20)
hist(subset(plmTrainingDdata, classe=="D")$roll_belt, col="blue",xlim = c(0,175), ylim = c(0,2500),breaks = 20)
hist(subset(plmTrainingDdata, classe=="E")$roll_belt, col="blue",xlim = c(0,175), ylim = c(0,2500),breaks = 20)

#make classe a factor variable
plmTrainingDdata[,'classe'] <- as.factor(plmTrainingDdata[,'classe'])
plmTestingDdata[,'classe'] <- as.factor(plmTestingDdata[,'classe'])

#find unique values in data columns
apply(plmTrainingDdata,2,function(x) length(unique(x)))

#clean data
## remove na
plmTrainingDdata <- plmTrainingDdata[, colSums(is.na(plmTrainingDdata)) == 0]
plmTestingDdata <- plmTestingDdata[, colSums(is.na(plmTestingDdata)) == 0]
#dim(plmTrainingDdata)

## remove columns with character datatype 
plmTrainingDdata <- plmTrainingDdata[, !sapply(plmTrainingDdata, is.character)]
plmTestingDdata <- plmTestingDdata[, !sapply(plmTestingDdata, is.character)]
str(plmTrainingDdata)

## remove remaining non-numeric
#plmnum = plmTrainingDdatanonas[sapply(plmTrainingDdatanonas,function(x) is.numeric(x))]

#remove raw_timestamp_part_1 and num_window from dataframe - not useful in model and causing bad results
drops <- c("X","raw_timestamp_part_1","raw_timestamp_part_2","num_window")
plmTrainingDdata <-plmTrainingDdata[ , !(names(plmTrainingDdata) %in% drops)]
plmTestingDdata <-plmTestingDdata[ , !(names(plmTestingDdata) %in% drops)]

#investigate correlation
cor(plmTrainingDdata$accel_belt_y, plmTrainingDdata$roll_belt)
cor(plmTrainingDdata$accel_belt_z, plmTrainingDdata$roll_belt)
cor(plmTrainingDdata$total_accel_belt, plmTrainingDdata$roll_belt)
cor(plmTrainingDdata$total_accel_belt, plmTrainingDdata$accel_belt_z)
cor(plmTrainingDdata$gyros_forearm_z, plmTrainingDdata$gyros_dumbbell_z)
cor(plmTrainingDdata$gyros_forearm_z, plmTrainingDdata$gyros_dumbbell_x)

#get correlation matrix
m = cor(plmTrainingDdata)

#print correlation matrix and review 
m

#plot correlation 
corrplot(m, method="circle")

#create random forrest model
rfmod <-randomForest(classe~.,data=plmTrainingDdata, ntree=1000)
rfmod

#calculate and plot importance of variables
importance(rfmod)
varImpPlot(rfmod)

#make predictions from the created rf model using the testing data
predWithClass <- predict(rfmod,plmTestingDdata,type='class')
t<-table(predictions=predWithClass,actual=plmTestingDdata$problem_id)
t

#plot the ROC curve and calculate the area under the curve (AUC) metric
predWithProbs <- predict(rfmod,plmTestingDdata,type='prob')
auc<-auc(plmTestingDdata$problem_id ,predWithProbs[,1])
plot(roc(plmTestingDdata$problem_id ,predWithProbs[,1]))