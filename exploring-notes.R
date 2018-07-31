

library(ggplot2); library(caret);
install.packages('e1071', dependencies=TRUE)


featurePlot(x=data[,c("roll_belt","pitch_belt","yaw_belt")],
            y = data$classe,
            plot="pairs")

qplot(pitch_belt,yaw_belt,colour=classe,data=data)
qplot(roll_belt,yaw_belt,colour=classe,data=data)
qplot(magnet_belt_y,accel_belt_y,colour=classe,data=data)
qplot(gyros_belt_z,magnet_belt_y,colour=classe,data=data)

qplot(magnet_belt_x,amplitude_pitch_belt,colour=classe,data=data)
qplot(roll_forearm,amplitude_pitch_belt,colour=classe,data=data)
## at zero gyros_arm_x is much more likely to be classe A
## at zero roll_forearm x is more likely to be classe A
## total_accel_forearm more likely to be near 32 for classe A

inTrain <- createDataPartition(y=data$classe,
                               p=0.85, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

lm1 <- lm(magnet_dumbbell_x ~ accel_arm_x,data=training)
summary(lm1)

plot(training$gyros_arm_x, training$roll_forearm,pch=19,col=data$classe,xlab="gyros",ylab="roll forearm")

nw = training$new_window %in% c("yes")

fm <- lm(classe ~ ., data = nwd)
sm <- step(fm, direction = "backward")

nwd = training[nw,]
head(nwd)
library(caret)
set.seed(5001)
mod <- train(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_y + roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
             ,method="rf",data=training)
print(mod$finalModel)



tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

mod <- train(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_y + roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
             ,method="rf",data=training, trControl=tc)

library(randomForest)
getTree(mod$finalModel, k=2)
? getTree
pnb = predict(mod,testing)

testing$predRight <- pnb==testing$classe
table(testing$predRight)

t = table(testing$predRight)
as.data.frame(t)[2,][2]
as.data.frame(t)[1,][2]  
table(pnb, testing$classe)

confusionMatrix(pnb, testing$classe)


boxplot(yaw_dumbbell ~ classe, data = training, col = "red")

l = testing %in% c("A")
l
count(training[,l])
summary(pnb)
plot(mod$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(mod$finalModel, use.n=TRUE, all=TRUE, cex=.8)

predict(mod,newdata=testing)

boxplot(training$yaw_dumbbell, col = "blue")

table(mod)

plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

predict(modFit,newdata=testing)

plot(roll_belt, colour=classe, data=data)

heatmap(data$roll_arm)
plot(data$roll_arm,xlab="Column",ylab="Column Mean",pch=19,col=data$classe)

plot(data$yaw_belt,xlab="Column",ylab="Column Mean",pch=19,col=data$classe)

plot(data$pitch_belt,xlab="Column",ylab="Column Mean",pch=19,col=data$classe)

plot(data$roll_belt,xlab="Column",ylab="Column Mean",pch=19,col=data$classe)


plot(data$magnet_belt_y,xlab="Column",ylab="Column Mean",pch=19,col=data$classe)

qplot(data$magnet_belt_y,colour=classe,data=data,geom="density")

coln = colnames(data)
coln
for (i in coln) {
  str = paste(i,".png", sep="")
  #png(str,width=480,height=480,units="px",bg="white")
  #data$i
  qplot(data[[i]],colour=classe,data=data,geom="density")
  ggsave(str)
  #dev.off()
  
  print(str)
}
str



## Example: Iris Data


data(iris); library(ggplot2)
names(iris)
table(iris$Species)


inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)


qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

library(caret)
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)



coln = colnames(data)
coln
for (i in coln) {
  str = paste(i,".png", sep="")
  p <- ggplot(training, aes_string("classe", i))
  p + geom_boxplot()
  ggsave(str)
  print(str)
}


p <- ggplot(training,aes_string("classe", i))
p + geom_boxplot()



smData = data.frame
col = c('magnet_dumbbell_x' , 'accel_arm_x' , 'gyros_arm_x' , 'gyros_arm_y' , 'roll_forearm' , 'yaw_dumbbell' , 'accel_belt_z' , 'pitch_forearm', 'classe')


# Delete columns with all missing values
training<-training[,colSums(is.na(training)) == 0]
testing<-testing[,colSums(is.na(testing)) == 0]
smData <- training[col]
smDataTest <- testing[col]


tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

rf <- train(classe ~ ., data = training, method = "rf", trControl= tc)
svmr <- train(classe ~ ., data = training, method = "svmRadial", trControl= tc)
NN <- train(classe ~ ., data = training, method = "nnet", trControl= tc, verbose=FALSE)

  library(rpart)
library(rpart.plot)
  modFitA1 <- rpart(classe ~ ., data=smData, method="class")
  

  p = predict(modFitA1,smDataTest)
  confusionMatrix(p, smDataTest, type="class")
  
  
  rpart.plot(modFitA1, main="Classification Tree", extra=102, under=TRUE, faclen=0)
  
 str(p)
  str(smDataTest)
  str(modFitA1)
  



  trainingset <- read.csv("data/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
  testingset <- read.csv("data/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
  
  # Perform exploratory analysis - 
  # dim(trainingset); dim(testingset); summary(trainingset); summary(testingset); str(trainingset); str(testingset); head(trainingset); head(testingset);               
  
  # Delete columns with all missing values
  trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
  testingset <-testingset[,colSums(is.na(testingset)) == 0]
  
  # Delete variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). 
  trainingset   <-trainingset[,-c(1:7)]
  testingset <-testingset[,-c(1:7)]
  
  # partition the data so that 75% of the training dataset into training and the remaining 25% to testing
  traintrainset <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
  TrainTrainingSet <- trainingset[traintrainset, ] 
  TestTrainingSet <- trainingset[-traintrainset, ]
  
  model1 <- rpart(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_y + roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm, data=training, method="class")
  
  prediction1 <- predict(model1, TestTrainingSet, type = "class")
  confusionMatrix(prediction1, TestTrainingSet$classe)
  
  # Plot the Decision Tree
  rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)
  
  
  str(TrainTrainingSet)
  
  library(randomForest)
  
  model2 <- randomForest(classe ~. , data=TrainTrainingSet, method="class")
  prediction2 <- predict(model2, TestTrainingSet, type = "class")
  confusionMatrix(prediction2, TestTrainingSet$classe)
  
  
  
  model2 <- randomForest(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
                         , data=TrainTrainingSet, method="class")
  prediction2 <- predict(model2, TestTrainingSet, type = "class")
  confusionMatrix(prediction2, TestTrainingSet$classe)
  
  m = lm(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
               , data=TrainTrainingSet)
  
  classA <- TrainTrainingSet[TrainTrainingSet$classe == 'A',]
  classA <- TrainTrainingSet[TrainTrainingSet$classe == 'B',]
  colnames(TrainTrainingSet)
  TrainTrainingSet$classe
  m = lm(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z 
         , data=TrainTrainingSet)
  
  coef(m)
  p = predict(m, TestTrainingSet)
  confusionMatrix(p, TestTrainingSet$classe)
  str(TrainTrainingSet)
  
  summary(m)
  
  model2 <- randomForest(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
                         , data=training, method="class")
  prediction2 <- predict(model2, testing, type = "class")
  confusionMatrix(prediction2, testing$classe)tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

  tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

  rf <- train(classe ~ ., data = training, method = "rf", trControl= tc)
  
  rf <- train(classe ~ ., data = training, method = "rf", trControl= tc)
  
  
  svmr <- train(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm, data = TrainTrainingSet, method = "svmRadial", trControl= tc)  
  
svmr <- train(classe ~ ., data = TrainTrainingSet, method = "svmRadial", trControl= tc)
NN <- train(classe ~ ., data = training, method = "nnet", trControl= tc, verbose=FALSE)

require(e1071)

svmr <- svm(classe ~ magnet_dumbbell_x + accel_arm_x + gyros_arm_x + gyros_arm_x +roll_forearm + yaw_dumbbell + accel_belt_z + pitch_forearm
            ,data = TrainTrainingSet)  

  pRF <- predict(svmr, TestTrainingSet)
  confusionMatrix(pRF, TestTrainingSet$classe)

library(randomForest)
  
  
  