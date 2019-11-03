library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(ROCR)
library(parallel)
library(Boruta)
library(VIM)
library(mice)
library(knitr)
library(ROSE)
library(MASS)

model.fit.evaluate <- function(model.formula, train.data, test.data) {
  auclist = NULL
  accuracylist = NULL
  recalllist = NULL
  precisionlist = NULL
  k =1
  set.seed(123)
  
  training <-train.data
  testing.org<-test.data
  
  testing = testing.org[1:nrow(testing.org), names(testing.org)[names(testing.org) != 'target']]
  
  model <- glm(formula = model.formula,
               family = "binomial", data = training)
  predicted <- predict(model, newdata = testing,type="response")
  pred <- prediction(predicted, testing.org$target)
  ind = which.max(round(slot(pred, 'cutoffs')[[1]],1) == 0.5)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  
  auc.tmp <- performance(pred,"auc");
  auc <- as.numeric(auc.tmp@y.values)
  auclist[k] = auc
  
  acc.perf = performance(pred, measure = "acc")
  acc = slot(acc.perf, "y.values")[[1]][ind]
  accuracylist[k] = acc
  
  prec.perf = performance(pred, measure = "prec")
  prec = slot(prec.perf, "y.values")[[1]][ind]
  precisionlist[k] = prec
  
  recall.perf = performance(pred, measure = "tpr")
  recall = slot(recall.perf, "y.values")[[1]][ind]
  recalllist[k] = recall
  
    
    
  return(list("AUC" = mean(auclist), "Accuracy" = mean(accuracylist),  "Recall" = mean(recalllist), "Precision" = mean(precisionlist)))
}

df.metrix <<- NULL
print.model.matrix = function(model.name, matrixobj)
{
  
  print(paste("Printing Metrix for model: ", model.name))
  for(i in 1 : length(matrixobj))
  {
    df = data.frame("Model" = model.name, "Metrix"=names(matrixobj)[[i]], "Value" = matrixobj[[i]])
    df.metrix <<- rbind(df, df.metrix)
    print(paste(names(matrixobj)[[i]], ":", matrixobj[[i]]))
  }
  
}
setwd("D:/MSDS/Data621_Group4/HW-4/")
train.df = read.csv("./Data/insurance_training_data.csv", stringsAsFactors = FALSE, na.strings=c("NA","NaN", " ", ""))

train.df$PARENT1 = ifelse(train.df$PARENT1 == 'No', 0, 1)
train.df$PARENT1 = as.numeric(train.df$PARENT1)

train.df$MSTATUS = ifelse(train.df$MSTATUS == 'z_No', 0, 1)
train.df$MSTATUS = as.numeric(train.df$MSTATUS)
train.df$SEX = ifelse(train.df$SEX == 'M', 0, 1)
train.df$SEX = as.numeric(train.df$SEX)
train.df$EDUCATION = as.numeric(factor(train.df$EDUCATION, order = TRUE, levels = c("<High School", "z_High School", "Bachelors", "Masters", "PhD")))
train.df$JOB = as.numeric(factor(train.df$JOB, order = TRUE, levels = c("Student", "Home Maker", "z_Blue Collar", "Clerical", "Professional", 'Manager', 'Lawyer', 'Doctor')))
train.df$CAR_USE = ifelse(train.df$CAR_USE == "Private", 0, 1)
train.df$CAR_USE  = as.numeric(train.df$CAR_USE)
train.df$CAR_TYPE = as.numeric(factor(train.df$CAR_TYPE, order = TRUE, levels = c("Minivan", "z_SUV", "Van", "Pickup", "Panel Truck", 'Sports Car')))
train.df$RED_CAR = ifelse(train.df$RED_CAR == "no", 0, 1)
train.df$RED_CAR  = as.numeric(train.df$RED_CAR)
train.df$REVOKED = ifelse(train.df$REVOKED == "No", 0, 1)
train.df$REVOKED  = as.numeric(train.df$REVOKED)
train.df$URBANICITY = ifelse(train.df$URBANICITY == "z_Highly Rural/ Rural", 0, 1)
train.df$URBANICITY  = as.numeric(train.df$URBANICITY)

ConvertQuatitative = function(x)
{
  quan.cols =  
    if(typeof(x) == "character")
    {
      x = gsub("\\$", "", x)
      x = gsub("\\,", "", x)
      x = as.numeric(x)
    }
  return(x)
}

train.df = apply(train.df[], 2, function(x) ConvertQuatitative(x)) %>%data.frame()%>% dplyr::filter(TARGET_AMT != 0)

comp.data <- mice(train.df,m=2,maxit=10,meth='pmm',seed=500)
train.df = complete(comp.data)

bc <- boxcox(train.df$BLUEBOOK ~ train.df$TARGET_AMT)

lambda <- bc$x[which.max(bc$y)]


ggplot(train.df, aes(x =(BLUEBOOK^lambda -1)/lambda , y =log(TARGET_AMT)^2  , color = as.factor(TARGET_FLAG))) +
  geom_point()

train.df$TARGET_AMT_TRAN = log(train.df$TARGET_AMT)
train.df$BLUEBOOK_TRAN = (train.df$BLUEBOOK^lambda -1)/lambda

fit.ols = lm(train.df$TARGET_AMT_TRAN ~ train.df$BLUEBOOK_TRAN)
plot(train.df$TARGET_AMT_TRAN ~ train.df$BLUEBOOK_TRAN)
abline(fit.ols$coefficients,lty=2)
qqnorm(fit.ols$residuals)
summary(fit.ols)


Weighted_fit <- rlm(TARGET_AMT ~ BLUEBOOK, data = train.df, method = "MM")

plot(train.df$TARGET_AMT ~ train.df$BLUEBOOK)
abline(Weighted_fit$coefficients,lty=2)
qqnorm(Weighted_fit$residuals)
summary(Weighted_fit)

hist(log(train.df$TARGET_AMT^0.5))
train.df$AGE_SEX =log(1 + train.df$AGE) * (1+train.df$SEX) 
train.df.class = train.df[, !names(train.df)%in%c('INDEX', 'TARGET_AMT')]
train.df.class = dplyr::rename(train.df.class, target = TARGET_FLAG)

dt = sort(sample(nrow(train.df.class), nrow(train.df.class)*.7))

train.data = train.df.class[dt,]
test.data = train.df.class[-dt,]


model.metrix = cross.validation("target ~ INCOME + HOME_VAL + MSTATUS  + JOB + CAR_USE + CAR_TYPE + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + URBANICITY", train.data, test.data)
print.model.matrix("Base Model", model.metrix)

train.data.balanced <- ROSE(target ~ ., data = train.data, seed = 1)$data
table(train.data.balanced$target)

model.metrix = cross.validation("target ~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS+ SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK  + TIF + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY + AGE_SEX", train.data.balanced, test.data)
print.model.matrix("Base Model", model.metrix)


set.seed(123)
dt = sort(sample(nrow(train.df.class), nrow(train.df.class)*.7))
training.org <-train.df.class[dt,]
testing.org<-train.df.class[-dt,]

testing = testing.org[1:nrow(testing.org), names(testing.org)[names(testing.org) != 'target']]

model <- glm(formula = "target ~ INCOME + HOME_VAL + MSTATUS  + JOB + CAR_USE + CAR_TYPE + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + URBANICITY",
             family = "binomial", data = training.org)

predicted <- predict(model, newdata = testing,type="response")
lables = ifelse(predicted > 0.5, 1, 0)
testing.org$predtarget = lables
pred <- prediction(predicted, testing.org$target)
ind = which.max(round(slot(pred, 'cutoffs')[[1]],1) == 0.5)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auclist[k] = auc

acc.perf = performance(pred, measure = "acc")
acc = slot(acc.perf, "y.values")[[1]][ind]
accuracylist[k] = acc

prec.perf = performance(pred, measure = "prec")
prec = slot(prec.perf, "y.values")[[1]][ind]
precisionlist[k] = prec

recall.perf = performance(pred, measure = "tpr")
recall = slot(recall.perf, "y.values")[[1]][ind]
recalllist[k] = recall
