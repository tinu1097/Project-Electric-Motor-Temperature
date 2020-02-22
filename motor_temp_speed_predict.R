#...........All required libraries................#

library(readr)    ##for read data ##
library(moments)  ##for 3rd and 4th moments of business decision#
library(plyr)     ##plyr is a set of tools that solves a common set of problems##
library(tidyr)    ##used for transformation#
library(psych)    ##for multilivariate analysis##
library(ggplot2)  ##for visualization##
library(dplyr)    #used for gathering, data transformations##
library(corpcor) ##checking collinearity problem##
library(GGally)   ##for visualization##
library(gridExtra) ##for merge plots##
library(corrplot) ##correlation plot##
library(funModeling) ##visualization##
library(DataExplorer) 
library(caret)      
library(caTools)  #for spliting data train and test##
library(car)     ##in this business problem we used  vif and av plot##
library(mlbench)
library(glmnet) #for penalize regression##
library(rpart) ##for decision tree#
library(ranger) ##for random forest##
library(Metrics)  ##for rmse##
library(rpart.plot) ##for decision plot##

#.................Train Data .........................# 
motor_train<- read.csv("C:/Users/Hp/Downloads/E_motor_temp_project/train.csv")
test_data<- read.csv("C:/Users/Hp/Downloads/E_motor_temp_project/test.csv")
test_data<-test_data[,-1]
#....EDA(Exploratery Data Analysis) on Training Data set......#

E_motor_train <-motor_train[-1] ##excluded first column
dim(E_motor_train)
attach(E_motor_train) 
summary(E_motor_train)
colSums(is.na(E_motor_train)) ##checking null values#
describe(E_motor_train) 

### 4 moments of business decision ####
plyr::colwise(mean)(E_motor_train[c(1:12)])    ##mean for all numeric values#
plyr::colwise(median)(E_motor_train[c(1:12)])  ##median for all numeric values#
plyr::colwise(var)(E_motor_train[c(1:12)])      ##variance for all numeric values#
plyr::colwise(sd)(E_motor_train[c(1:12)])       ##std for all numeric values#
plyr::colwise(range)(E_motor_train[c(1:12)])     ##range for all numeric values#
plyr::colwise(skewness)(E_motor_train[c(1:12)])  ##skewness for all numeric values#
plyr::colwise(kurtosis)(E_motor_train[c(1:12)])  ##kurtosis for all numeric values#

#...................Visualization(Graphical Representation).......................#

#number of measurements per profile_id#
unique(E_motor_train$profile_id)
#E_motor_train$profile_id <- as.factor(E_motor_train$profile_id)
a <- plyr::count(profile_id) ##each profile_id how many time come##
ggplot(data = a)+geom_bar(mapping=aes(a$x,a$freq),stat = 'identity',width = 0.8,fill='yellow',color="black")+theme_light()+geom_label(aes(a$x,a$freq),label=a$freq,)

b <- a[order(a$freq),] ##in order by frequency##
ggplot(data = b,aes(x=reorder(as.factor(b$x),b$freq),y=b$freq,fill=as.factor(a$x)))+geom_bar(stat = 'identity')+coord_flip()+labs(title = 'profile id vs measurements',x='profile id',y='measurements')


#Minimum,Medium and Maximum motor_speed for each profile id #

m_speed=E_motor_train%>%group_by(profile_id)%>%summarise(max_speed=max(motor_speed),med_speed=median(motor_speed),min_speed=min(motor_speed))
str(m_speed)

plot1 <- ggplot(data = m_speed,aes(x=profile_id,y=max_speed,group=1))+geom_point(color='blue')+geom_line(color='red')+ggtitle('max_speed')+theme_bw()
plot2 <- ggplot(data = m_speed,aes(x=profile_id,y=min_speed,group=1))+geom_point(color='green')+geom_line(color='brown')+ggtitle('min_speed')+theme_bw()
grid.arrange(plot1,plot2,nrow=3) ##plot for max_speed and min_speed##

##Group based on clockwise and anticlockwise motorspeed##
mot_direc=E_motor_train%>%mutate(clock_anticlockwise=ifelse(motor_speed>0,'clock','anticlock'))
mot_direc[c(1:10),]

ggplot(data=mot_direc,aes(x=profile_id,fill=clock_anticlockwise))+geom_bar(position = "dodge")

#Counts Plot#
g <- ggplot(E_motor_train, aes(profile_id,pm,col=profile_id))
g + geom_count(show.legend=F) +
  labs(y="pm", 
       x="profile_id", 
       title="Counts Plot profile id vs pm")


#........histogram for numeric values.........#

plot_num(E_motor_train) ##colorfull representation of all histogram ##

#......Density Plot.....#
ggplot(data=E_motor_train,aes(x = E_motor_train$profile_id, fill = E_motor_train$pm)) +
  geom_density(alpha = 0.3, color = 'green')

#violin plot
#E_motor_train%>%gather(-X,-profile_id,key='key',value = 'value')%>%ggplot(aes(key,value))+geom_violin(fill='steelblue')+theme_dark()


##random profile_id##
uniq_id <- as.data.frame(unique(profile_id))
profile <- uniq_id[sample(nrow(uniq_id),1,replace = FALSE,prob = NULL),]

#heatmap for random profile_id
corr <- cor(E_motor_train[E_motor_train$profile_id==uniq_id[profile,],-c(13)])
heatmap(corr,main = (paste0('heatmap for profileid :',profile))) ##inbuilt function##

#we assign new variable##
plot_data <- E_motor_train[E_motor_train$profile_id==uniq_id[profile,],-c(13)]

##we can check normality by QQ plot  for random profile_id##
plot_data%>%gather()%>%ggplot(aes(sample=value))+facet_wrap(~key,scales = 'free')+stat_qq(col='green')+stat_qq_line(col='skyblue')+theme_dark()

#pairs for random profile_id
ggpairs(E_motor_train[E_motor_train$profile_id==uniq_id[profile,],-c(13)],)

##Correaltion for all variables##
cor(E_motor_train[sapply(E_motor_train, function(x) !is.factor(x))])

##graphical representation for all correlation## 
corrplot(cor(E_motor_train[,-c(13)]),method = 'number',type = 'lower',bg = "black",diag = FALSE)
corrplot(cor(E_motor_train[,-c(13)]),method = 'circle',outline = TRUE)
##partial correlation matrix pure-correlation b/w the variables##
cor2pcor(cor(E_motor_train[sapply(E_motor_train, function(x) !is.factor(x))]))
###we can see that many input varables  correlated to each other this is the multicolinearity problem##
#PCA technique is fruitful choice but after inserting PCA our train and test RMSE increse##
#so we can safely conclude that PCA technique is not useful for this perticular business problem#

#.........box plot for all numeric values.......#

for (i in c(1:13)) {
  name <- names(E_motor_train)[i]
  boxplot(E_motor_train[,i],main = name,col="blue",horizontal = TRUE)
  readline("press enter to continue")
}
#more colourfull representation#
E_motor_train%>%gather(-profile_id,key='key',value = 'value')%>%ggplot(aes(key,value))+geom_boxplot(fill='steelblue')+theme_dark()

#......outliers treatment............#
out_treatment <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

E_motor_train$ambient <- out_treatment(E_motor_train$ambient)
E_motor_train$torque <- out_treatment(E_motor_train$torque)
E_motor_train$i_q <- out_treatment(E_motor_train$i_q)

#............standarization............ #

motor_norm<-scale(E_motor_train)
motor_norm<-as.data.frame(motor_norm)
describe(motor_norm)
str(motor_norm)
colnames(motor_norm)
attach(motor_norm)

#.............feature selection.............#
set.seed(111)
results <- rfe(motor_norm[,-9], motor_norm[,9], sizes=c(1:13), rfeControl = rfeControl(functions=lmFuncs, method="repeatedcv", number=10),metric = "RMSE")
print(results)   #summarize the results
predictors(results) #list the chosen features
plot(results, type=c("g", "o"))  # plot the results

#..........spliting data train and test..........#

set.seed(111)
split<-(sample.split(motor_norm$pm,SplitRatio=0.70))
train_motor <- subset(motor_norm,split==TRUE)
test_motor <- subset(motor_norm,split==FALSE)
colnames(train_motor)


#for compare train test data  ###
list_models<- setNames(data.frame(matrix(ncol = 6,nrow = 0)),c("S.N.","modelname","Train_RMSE","Test_RMSE","TRain_Accu","Test_Accu"))

#.............model building for training data...............#

#Multilinear regression#
library(caret)
model<-lm(pm~.,data = train_motor)
summary(model) ##0.77#
predicted<-predict(model,train_motor) #predicted 
predicted1<-predict(model,test_motor)

###Accuracy##
train_accu<-cor(predicted,train_motor$pm)  ##0.87
test_accu<-cor(predicted1,test_motor$pm) ##0.87

##RMSE value for train and test data ###
rmse_tr<-sqrt(mean((train_motor$pm-predicted)^2)) 
rmse_te<-sqrt(mean((test_motor$pm-predicted1)^2))  

##VIF(variance inflation factor)
vif(model)
#we can see that only ambient,u_d,i_d,profile_id ,VIF values under 10 ##

# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model,id.n=5,id.cex=0.7)  #by avplots we can see that 
##we see that torque and i_q showed no contribution at all##

## rmse and accuracy comparison for all models ###
list_models<-rbind(list_models,data.frame (S.N.=1,modelname="multilinear model",
                                           Train_RMSE=sqrt(mean((train_motor$pm-predicted)^2)),Test_RMSE=sqrt(mean((test_motor$pm-predicted1)^2)),
                                           TRain_Accu= cor(predicted,train_motor$pm),Test_Accu=cor(predicted1,test_motor$pm)))



#Lasso Regression

#library(caret)
#library(glmnet) #for penalize regression##
set.seed(1234)
custom<-trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = T)


lasso_model<-train(pm~.,
             train_motor,
             method = "glmnet",
             tuneGrid = expand.grid(alpha = 1,
                                    lambda = 0),
             trControl = custom)

#prediction#
predicted_l_train<-predict(lasso_model,train_motor)
predicted_l_test<-predict(lasso_model,test_motor)

cor(predicted_l_train,train_motor$pm) ##0.87#
cor(predicted_l_test,test_motor$pm)   #0.87#

##RMSE value for train and test data ###
rmse_lasso_train<-sqrt(mean((train_motor$pm-predicted_l_train)^2)) 
rmse_lasso_test<-sqrt(mean((test_motor$pm-predicted_l_test)^2))

#plot#
plot(varImp(lasso_model,scale = F))
summary(lasso_model)

##for compare all models ###
list_models<-rbind(list_models,data.frame (S.N.=2,modelname="lasso model ",
                                           Train_RMSE=sqrt(mean((train_motor$pm-predicted_l_train)^2)),Test_RMSE=sqrt(mean((test_motor$pm-predicted_l_test)^2)),
                                           TRain_Accu= cor(predicted_l_train,train_motor$pm),Test_Accu=cor(predicted_l_test,test_motor$pm)))

#RIDGE REGRESSION

set.seed(1234)
custom<-trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = T)


ridge_model<-train(pm~.,
             train_motor,
             method = "glmnet",
             tuneGrid = expand.grid(alpha = 0,
                                    lambda =0),
             trControl = custom)


##prediction##
predicted_r_trin<-predict(ridge_model,train_motor)
predicted_r_test<-predict(ridge_model,test_motor)

##Accuracy##
cor(predicted_r_trin,train_motor$pm) ##0.84#
cor(predicted_r_test,test_motor$pm)   #0.84#

##RMSE value for train and test data ###
rmse_ridge_train<-sqrt(mean((train_motor$pm-predicted_r_trin)^2))  
rmse_ridge_test<-sqrt(mean((test_motor$pm-predicted_r_test)^2))     

#Ridge
plot(varImp(ridge_model,scale = F))
summary(ridge_model)

##for compare all models ###
list_models<-rbind(list_models,data.frame (S.N.=3,modelname="ridge model",
                                           Train_RMSE=sqrt(mean((train_motor$pm-predicted_r_trin)^2)),Test_RMSE=sqrt(mean((test_motor$pm-predicted_r_test)^2)),
                                           TRain_Accu= cor(predicted_r_trin,train_motor$pm),Test_Accu=cor(predicted_r_test,test_motor$pm)))


##...........ELASTIC NET..................##

set.seed(1234)
custom<-trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = T)


elastic_net<-train(pm~.,
                   train_motor,
                   method = 'glmnet',
                   tuneGrid = expand.grid(alpha = seq(0,1,length=5),
                                          lambda = 0,
                   trcontrol = custom))
##prediction##
predicted_el_train<-predict(elastic_net,train_motor)
predicted_el_test<-predict(elastic_net,test_motor)

##Accuracy##
cor(predicted_el_train,train_motor$pm) ##0.87#
cor(predicted_el_test,test_motor$pm)   #0.87#

##RMSE value for train and test data ###
rmse_el_train<-sqrt(mean((train_motor$pm-predicted_el_train)^2))  ##0.47
rmse_el_test<-sqrt(mean((test_motor$pm-predicted_el_test)^2))     ##0.47

#Elastic_net plot ##
plot(varImp(elastic_net,scale = F))
plot(elastic_net$finalModel, xvar = 'lambda',labet=T)
 
##for compare all models ###
list_models<-rbind(list_models,data.frame (S.N.=4,modelname="Elasticnet model",
                                           Train_RMSE=sqrt(mean((train_motor$pm-predicted_el_train)^2)),Test_RMSE=sqrt(mean((test_motor$pm-predicted_el_test)^2)),
                                           TRain_Accu= cor(predicted_el_train,train_motor$pm),Test_Accu=cor(predicted_el_test,test_motor$pm)))



##...........Decision tree..............##

set.seed(1234)
#library(rpart)

tree_model<-rpart(pm~.,data = train_motor, method = "anova",
           control=rpart.control(minsplit=20,cp=0.01))

#summary(tree_model)
printcp(tree_model)
plotcp(tree_model)


#visualizing the model
library(rpart.plot)
rpart.plot(tree_model)

summary(round(predicted))
summary(round(predicted_tr))

##Prediction for train and test data ##
predicted_tr<-predict(tree_model,train_motor)
predicted <- predict(tree_model,test_motor)

##Accurcy##
cor(predicted_tr, train_motor$pm)
cor(predicted, test_motor$pm)  

##RMSE value for train and test data ###
rmse_dt_train<-sqrt(mean((train_motor$pm-predicted_tr)^2))  
rmse_dt_test<-sqrt(mean((test_motor$pm-predicted)^2))     

##for compare all models ###
list_models<-rbind(list_models,data.frame (S.N.=5,modelname="Decision tree ",
                                           Train_RMSE=sqrt(mean((train_motor$pm-predicted_tr)^2)),Test_RMSE=sqrt(mean((test_motor$pm-predicted)^2)),
                                           TRain_Accu= cor(predicted_tr, train_motor$pm),Test_Accu=cor(predicted, test_motor$pm)))


#.....................xgboost...................#
library(xgboost) 
set.seed(100)
xg <- xgboost(data = as.matrix(train_motor[,-9]),label = as.matrix(train_motor$pm ),nrounds = 20)
pred_xg_t<-predict(xg,newdata = as.matrix(train_motor[,-9]))
predxg <- predict(xg,newdata = as.matrix(test_motor[,-9]))
cor(pred_xg_t,train_motor$pm)
cor(predxg,test_motor$pm)    
rmse(pred_xg_t,train_motor$pm)
rmse(predxg,test_motor$pm)   
plot(predxg,test_motor$pm)

list_models<-rbind(list_models,data.frame (S.N.=3,modelname="xgboost",
                                           Train_RMSE=rmse(predict(xg,newdata = as.matrix(train_motor[,-9])),train_motor$pm),Test_RMSE=rmse(predict(xg,newdata = as.matrix(test_motor[,-9])),test_motor$pm),
                                           TRain_Accu= cor(predict(xg,newdata = as.matrix(train_motor[,-9])),train_motor$pm),Test_Accu=cor(predict(xg,newdata = as.matrix(test_motor[,-9])),test_motor$pm)))



#...................Random forest....................#
set.seed(100)
randfor <- ranger(pm~.,data = train_motor, num.trees = 100,mtry = 4,importance = 'none')
#Predictions#
predrandfor_tr<-predict(randfor,train_motor)
predrandfor_te <- predict(randfor,test_motor)
#Accuracy#
cor(predrandfor_tr$predictions,train_motor$pm)
cor(predrandfor_te$predictions,test_motor$pm)
#RMSE#
rmse_randfor_tr<-rmse(train_motor$pm,predrandfor_tr$predictions)
rmse_randfor<-rmse(test_motor$pm,predrandfor_te$predictions)
##plot
plot(test_motor$pm,predrandfor_te$predictions)

list_models<-rbind(list_models,data.frame (S.N.=3,modelname="random forest",
                                           Train_RMSE=rmse(train_motor$pm,predrandfor_tr$predictions),Test_RMSE=rmse(test_motor$pm,predrandfor_te$predictions),
                                           TRain_Accu= cor(predrandfor_tr$predictions,train_motor$pm),Test_Accu=cor(predrandfor_te$predictions,test_motor$pm)))



#.......Final Model(Random Forest).........#

set.seed(111)
random <- ranger(pm~.,data = motor_norm, num.trees = 100,mtry = 4,importance = 'none')
#Predictions#
predrand <- predict(random,test)
prediction_test<-predrand$predictions
setwd("C:/Users/Hp/Downloads/E_motor_temp_project/")
write.file.csv(prediction_test,file = "prediction_test.csv",col.names = TRUE)





