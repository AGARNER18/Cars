install.packages("caret")
install.packages("VGAM")
install.packages("nnet")
install.packages("e1071")
library(e1071)
library("nnet")
library(VGAM)
library(caret)

# set working directory
setwd("C:/Users/Amber/Desktop/Big Data/class_kaggle")

# load cars data
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

# look at data 
summary(car_eval)
str(car_eval)

# find number of missing in each variable
apply(is.na(car_eval), 2, sum) # no missing values
#**********************************************GLM*****************************************************
# build vector generalized linear model
model1<-vglm(class~buying+maint+doors+persons+lug_boot+safety,family = "multinomial",data=car_eval)

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]
probability<-predict(model1,x,type="response") 
car_eval$pred_log_reg<-apply(probability,1,which.max) 
car_eval$pred_log_reg[which(car_eval$pred_log_reg=="1")]<-levels(car_eval$class)[1] 
car_eval$pred_log_reg[which(car_eval$pred_log_reg=="2")]<-levels(car_eval$class)[2] 
car_eval$pred_log_reg[which(car_eval$pred_log_reg=="3")]<-levels(car_eval$class)[3] 
car_eval$pred_log_reg[which(car_eval$pred_log_reg=="4")]<-levels(car_eval$class)[4]

mtab<-table(car_eval$pred_log_reg,car_eval$class)
confusionMatrix(mtab)

model1_acc<-0.9402 # training set accuracy

# validate model with hold out data
car_eval_test<-read.csv("cars-test.csv")
id_test<-car_eval_test[,1]
car_eval_test<-car_eval_test[,-1]
# select only the predictors: update with new data
x_test<-car_eval_test[,1:6] 

# select only the target: need to be updated with new data
y_test<-car_eval_test[,7]
probability<-predict(model1,x_test,type="response") 
car_eval_test$pred_log_reg<-apply(probability,1,which.max) 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="1")]<-levels(car_eval_test$class)[1] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="2")]<-levels(car_eval_test$class)[2] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="3")]<-levels(car_eval_test$class)[3] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="4")]<-levels(car_eval_test$class)[4]

mtab_test<-table(car_eval_test$pred_log_reg,car_eval_test$class)
confusionMatrix(mtab_test)

model2_acc_test<-0.9198

valid<-read.csv("cars-hold-validate.csv")
id<-valid[,1]
valid<-valid[,-1]
valid$class<-NA
x<-valid[,-7]
probability<-predict(model1,valid,type="response") 
valid$pred_log_reg<-apply(probability,1,which.max) 
valid$pred_log_reg[which(valid$pred_log_reg=="1")]<-"acc" 
valid$pred_log_reg[which(valid$pred_log_reg=="2")]<-"good" 
valid$pred_log_reg[which(valid$pred_log_reg=="3")]<-"unacc"
valid$pred_log_reg[which(valid$pred_log_reg=="4")]<-"vgood"


predictions <- data.frame(car.id=id, class=valid$pred_log_reg)

write.csv(predictions, "vglm_output.csv")

#*******************************************NNET**********************************************************

# load cars data
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

model2<-nnet(class~., data=car_eval, size=4)

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]

car_eval$pred_nnet<-predict(model2,x,type="class") 
#Accuracy of the model 
mtab<-table(car_eval$pred_nnet,car_eval$class) 
confusionMatrix(mtab)

model2_acc<-0.9887

car_test<-read.csv("cars-test.csv")

id<-car_test[,1]
car_test<-car_test[,-1]

# select only the predictors: update with new data
x<-car_test[,1:6] 

# select only the target: need to be updated with new data
y<-car_test[,7]

car_test$pred_nnet<-predict(model2,x,type="class") 
#Accuracy of the model 
mtab<-table(car_test$pred_nnet,car_test$class) 
confusionMatrix(mtab)

model2_acc_test<-0.9444

car_valid<-read.csv("cars-hold-validate.csv")
id<-car_valid[,1]
car_valid<-car_valid[,-1]
car_valid$class<-NA
# select only the predictors: update with new data
x<-car_valid[,1:6] 

# select only the target: need to be updated with new data
y<-car_valid[,7]

car_valid$pred_nnet<-predict(model2,x,type="class") 

predictions <- data.frame(car.id=id, class=car_valid$pred_nnet)

write.csv(predictions, "nnet_output.csv")

#***********************************************
# load cars data
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

library(RWeka) 
#Build the model 
model3<-J48(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval) 

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]

car_eval$pred_j48<-predict(model3,x) 
#Accuracy of the model 
mtab<-table(car_eval$pred_j48,car_eval$class) 
confusionMatrix(mtab) 

model3_acc<-0.9247

car_test<-read.csv("cars-test.csv")

id<-car_test[,1]
car_test<-car_test[,-1]

# select only the predictors: update with new data
x<-car_test[,1:6] 

# select only the target: need to be updated with new data
y<-car_test[,7]

car_test$pred_j48<-predict(model3,x,type="class") 
#Accuracy of the model 
mtab<-table(car_test$pred_j48,car_test$class) 
confusionMatrix(mtab)

model3_acc_test<-0.8981

car_valid<-read.csv("cars-hold-validate.csv")
id<-car_valid[,1]
car_valid<-car_valid[,-1]
car_valid$class<-NA
# select only the predictors: update with new data
x<-car_valid[,1:6] 

# select only the target: need to be updated with new data
y<-car_valid[,7]

car_valid$pred_j48<-predict(model3,x,type="class") 

predictions <- data.frame(car.id=id, class=car_valid$pred_j48)

write.csv(predictions, "c4.5_output.csv")

#*************************************************************
library(RWeka)
# load cars data
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

library(RWeka) 
#Build the model 
model4<-PART(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval) 

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]

car_eval$pred_part<-predict(model4,x)
part<-predict(model4,x)
#Accuracy of the model 
mtab<-table(car_eval$pred_part,car_eval$class) 
confusionMatrix(mtab) 

model4_acc<-0.9732

car_test<-read.csv("cars-test.csv")

id<-car_test[,1]
car_test<-car_test[,-1]

# select only the predictors: update with new data
x<-car_test[,1:6] 

# select only the target: need to be updated with new data
y<-car_test[,7]

car_test$pred_part<-predict(model4,x,type="class") 
part_test<-predict(model4,x,type="class")
#Accuracy of the model 
mtab<-table(car_test$pred_part,car_test$class) 
confusionMatrix(mtab)

model4_acc_test<-0.9568

car_valid<-read.csv("cars-hold-validate.csv")
id<-car_valid[,1]
car_valid<-car_valid[,-1]
car_valid$class<-NA
# select only the predictors: update with new data
x<-car_valid[,1:6] 

# select only the target: need to be updated with new data
y<-car_valid[,7]

car_valid$pred_part<-predict(model4,x,type="class") 
part_valid<-predict(model4,x,type="class")

predictions <- data.frame(car.id=id, class=car_valid$pred_part)

write.csv(predictions, "part_output.csv")

#**************************************************************
library(ipred) 
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

#Build the model 
model5<-bagging(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval, nbagg=2000)

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]

car_eval$pred_bag<-predict(model5,x)
bag<-predict(model5,x)
#Accuracy of the model 
mtab<-table(car_eval$pred_bag,car_eval$class) 
confusionMatrix(mtab) 

model5_acc<-1

car_test<-read.csv("cars-test.csv")

id<-car_test[,1]
car_test<-car_test[,-1]

# select only the predictors: update with new data
x<-car_test[,1:6] 

# select only the target: need to be updated with new data
y<-car_test[,7]

car_test$pred_bag<-predict(model5,x,type="class") 
bag_test<-predict(model5,x,type="class") 
#Accuracy of the model 
mtab<-table(car_test$pred_bag,car_test$class) 
confusionMatrix(mtab)

model5_acc_test<-0.9815

car_valid<-read.csv("cars-hold-validate.csv")
id<-car_valid[,1]
car_valid<-car_valid[,-1]
car_valid$class<-NA
# select only the predictors: update with new data
x<-car_valid[,1:6] 

# select only the target: need to be updated with new data
y<-car_valid[,7]

car_valid$pred_bag<-predict(model5,x,type="class") 
bag_valid<-predict(model5,x,type="class") 

predictions <- data.frame(car.id=id, class=car_valid$pred_bag)

write.csv(predictions, "bag_output2.csv")

#******************************************************************
library(randomForest) 

car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

#Build the model 
model6<-randomForest(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval, ntree=6000)

# select only the predictors: update with new data
x<-car_eval[,1:6] 

# select only the target: need to be updated with new data
y<-car_eval[,7]

car_eval$pred_rf<-predict(model6,x) 
#Accuracy of the model 
mtab<-table(car_eval$pred_rf,car_eval$class) 
confusionMatrix(mtab) 

model6_acc<-0.9979

car_test<-read.csv("cars-test.csv")

id<-car_test[,1]
car_test<-car_test[,-1]

# select only the predictors: update with new data
x<-car_test[,1:6] 

# select only the target: need to be updated with new data
y<-car_test[,7]

car_test$pred_rf<-predict(model6,x,type="class") 
#Accuracy of the model 
mtab<-table(car_test$pred_rf,car_test$class) 
confusionMatrix(mtab)

model6_acc_test<-0.9691

car_valid<-read.csv("cars-hold-validate.csv")
id<-car_valid[,1]
car_valid<-car_valid[,-1]
car_valid$class<-NA
# select only the predictors: update with new data
x<-car_valid[,1:6] 

# select only the target: need to be updated with new data
y<-car_valid[,7]

car_valid$pred_rf<-predict(model6,x,type="class") 

predictions <- data.frame(car.id=id, class=car_valid$pred_rf)

write.csv(predictions, "rf_output.csv")

#****************************************************************
# load cars data
car_eval<-read.csv("cars-train.csv")

head(car_eval)
id<-car_eval[,1]
car_eval<-car_eval[,-1]

car_final<-car_eval
car_final$part<-part
car_final$bag<-bag
names(car_final)

# build vector generalized linear model
model7<-vglm(class~buying+maint+doors+persons+lug_boot+safety+part+bag,family = "multinomial",data=car_final)

# select only the predictors: update with new data
x<-car_final[,-7] 

# select only the target: need to be updated with new data
y<-car_final[,7]
probability<-predict(model7,x,type="response") 
car_final$pred_log_reg<-apply(probability,1,which.max) 
car_final$pred_log_reg[which(car_final$pred_log_reg=="1")]<-levels(car_final$class)[1] 
car_final$pred_log_reg[which(car_final$pred_log_reg=="2")]<-levels(car_final$class)[2] 
car_final$pred_log_reg[which(car_final$pred_log_reg=="3")]<-levels(car_final$class)[3] 
car_final$pred_log_reg[which(car_final$pred_log_reg=="4")]<-levels(car_final$class)[4]

mtab<-table(car_final$pred_log_reg,car_final$class)
confusionMatrix(mtab)

model1_acc<-1

# validate model with hold out data
car_eval_test<-read.csv("cars-test.csv")
car_eval_test$part<-part_test
car_eval_test$bag<-bag_test
names(car_eval_test)
id_test<-car_eval_test[,1]
car_eval_test<-car_eval_test[,-1]
# select only the predictors: update with new data
x_test<-car_eval_test[,-7] 

# select only the target: need to be updated with new data
y_test<-car_eval_test[,7]
probability<-predict(model7,x_test,type="response") 
car_eval_test$pred_log_reg<-apply(probability,1,which.max) 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="1")]<-levels(car_eval_test$class)[1] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="2")]<-levels(car_eval_test$class)[2] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="3")]<-levels(car_eval_test$class)[3] 
car_eval_test$pred_log_reg[which(car_eval_test$pred_log_reg=="4")]<-levels(car_eval_test$class)[4]

mtab_test<-table(car_eval_test$pred_log_reg,car_eval_test$class)
confusionMatrix(mtab_test)

model2_acc_test<-0.9815

valid<-read.csv("cars-hold-validate.csv")
valid$part<-part_valid
valid$bag<-bag_valid
id<-valid[,1]
valid<-valid[,-1]
valid$class<-NA
x<-valid[,-7]
probability<-predict(model7,valid,type="response") 
valid$pred_log_reg<-apply(probability,1,which.max) 
valid$pred_log_reg[which(valid$pred_log_reg=="1")]<-"acc" 
valid$pred_log_reg[which(valid$pred_log_reg=="2")]<-"good" 
valid$pred_log_reg[which(valid$pred_log_reg=="3")]<-"unacc"
valid$pred_log_reg[which(valid$pred_log_reg=="4")]<-"vgood"


predictions <- data.frame(car.id=id, class=valid$pred_log_reg)

write.csv(predictions, "ensemble_output.csv")










