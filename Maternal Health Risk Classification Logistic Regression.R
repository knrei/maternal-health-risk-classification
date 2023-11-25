library(caTools)
library(caret)
library(readxl)
library(plotrix)

a="D:/S2/Analisis Data Lanjut/Data/logistic regression dan lorens/Data"
data_path = a
if (data_path == a) {
  data = read_excel(paste0(data_path, '/Book3.xlsx')); head(data)
  
  str(data)
  data$RiskLevel = as.factor(ifelse(data$RiskLevel == "high risk", 1, 0))
  
  str(data)
  colnames(data)[match('RiskLevel', colnames(data))] = 'y'
  
} else if (data_path == b) {}

if (data_path == a) {
  split_data<-function(training){
    ########################################
    n_data<-nrow(data)                     #                       
    data_tr<-round(training*n_data)        # 
    i_train<-1:n_data<=data_tr             #
    train_reg<<-data[i_train,]             #
    test_reg<<-data[!i_train,]             #
    ########################################
    par(mfrow=c(1,2))
    DiagnosisTr = table(train_reg$y)
    DiagnosisTs = table(test_reg$y)
    kat = c("0 = ","1 = ") 
    persentase1 = round(DiagnosisTr/sum(DiagnosisTr)*100) 
    persentase2 = round(DiagnosisTs/sum(DiagnosisTs)*100)
    kat1 = paste(kat,persentase1)
    kat2 = paste(kat,persentase2)
    kat1 = paste(kat1,'%',sep ='')
    kat2 = paste(kat2,'%',sep ='')
    pie3D(DiagnosisTr,labels=kat1,col=c('misty rose','thistle'),
          main="Percentage of Testing Data")
    pie3D(DiagnosisTs,labels=kat2,col=c('misty rose','thistle'),
          main="Percentage of Training Data")
    cat("      n_Obs ","n_Var ","\n","Train:",dim(train_reg),"\n", "Testing:",dim(test_reg))
    
    model<-function(alpha){
      par(mfrow=c(1,1))
      column_names<-c(paste0('X',1:(ncol(train_reg)-1)),'y')
      colnames(train_reg)<<-column_names
      colnames(test_reg)<<-column_names
      data_final<<-train_reg
      logistic_model <- glm(y ~ .,data = data_final,family = "binomial")
      summary_model<-summary(logistic_model)
      
      significant_vars<-names(which(summary_model$coefficients[,'Pr(>|z|)'] > alpha))
      data_final<-data_final[,!(colnames(data_final)%in% significant_vars)]
      final_model <<- glm(y ~ .,data = data_final,family = "binomial")
      summary(final_model)
    }
}else if(data_path == b){}

split_data(training=0.70)        #ubah training= 70% data digunakan untuk trainning (203 data)
model(alpha=1)                 #ubah berdasarkan taraf signifikan (alpha) 

library(corrplot)
library(PerformanceAnalytics)
library(plotrix)
x <- c(272, 406)
labels <- c("high risk", "low risk")
pie3D(x, labels=x, col=c('misty rose','thistle'),
      main="Proportion of High risk and Low Risk Data")
chart.Correlation(data[1:6], histogram = TRUE, method = "pearson")

library(car)
vif(final_model)

summary(final_model)
exp(cbind(OR = coef(final_model), confint(final_model)))
qchisq(0.95, 5)

##################################################################### prediction:
test_final<-test_reg[,colnames(data_final)]
predict_reg <- predict(final_model,test_final, type = "response")
predicted_val <- ifelse(predict_reg >0.5, 1, 0)

actual_val = test_final$y

predicted_val = as.factor(predicted_val)
actual_val = as.factor(actual_val)
confusionMatrix(predicted_val, actual_val)


## Because X1 and X3 are not significant for model, we eliminate them and we built new model
# new model
data_final<<-train_reg
model2 <- glm(y ~ X2 + X4 + X5 + X6, data = data_final,family = "binomial")
summary(model2)
vif(model2)
exp(cbind(OR = coef(model2), confint(model2)))
qchisq(0.95, 3)


##################################################################### prediction:
test_final<-test_reg[,colnames(data_final)]
predict_reg <- predict(model2,test_final, type = "response")
predicted_val <- ifelse(predict_reg >0.5, 1, 0)

actual_val = test_final$y

predicted_val = as.factor(predicted_val)
actual_val = as.factor(actual_val)
confusionMatrix(predicted_val, actual_val)

