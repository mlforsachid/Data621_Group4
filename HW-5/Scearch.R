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
library(DAAG)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(arm)
opts_knit$set(root.dir = "D:/MSDS/Data621_Group4/HW-5/")

# Set working directory and load the training data
train.df = read.csv("./Data/wine-training-data.csv", stringsAsFactors = FALSE, na.strings=c("NA","NaN", " ", ""))

head(train.df)
print(paste("Number of columns = ", ncol(train.df)))
print(paste("Number of rows = ", nrow(train.df)))

str(train.df)

miss.cols = apply(train.df, 2, function(x) any(is.na(x)))
print(paste("Number of columns with missing values = ", length(names(miss.cols[miss.cols==TRUE]))))
print(paste("Names of columns with missing values = ", paste(names(miss.cols[miss.cols==TRUE]), collapse = ', ')))

hist(train.df$TARGET)

train.df = train.df[1:nrow(train.df), 2:ncol(train.df)]

ggr_plot <- aggr(train.df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train.df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#AGE, YOJ, INCOME, HOME_VAL, JOB, CAR_AGE"

comp.data <- mice(train.df,m=2,maxit=10,meth='pmm',seed=500)
train.df = complete(comp.data)

res2<-rcorr(as.matrix(train.df))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

ggplot(train.df, aes(x= STARS, y = TARGET)) +
  geom_point(aes(colour = factor(TARGET)))

train.df$StarTran = ifelse(train.df$STARS == 1, 2, train.df$STARS)

ggplot(train.df, aes(x= AcidIndex, y = TARGET)) +
  geom_point(aes(colour = factor(TARGET)))

train.df$AcidIndexTran = ifelse(train.df$AcidIndex <=6,  0, train.df$AcidIndex)
train.df$AcidIndexTran = ifelse((train.df$AcidIndexTran > 6 & train.df$AcidIndexTran < 12),  1, train.df$AcidIndexTran)
train.df$AcidIndexTran = ifelse(train.df$AcidIndexTran >= 12 ,  2, train.df$AcidIndexTran)

ggplot(train.df, aes(x= LabelAppeal, y = TARGET)) +
  geom_point(aes(colour = factor(TARGET)))

train.df$LabelAppealTran = ifelse(train.df$LabelAppeal <0,  0, train.df$LabelAppeal)

model.fit.evaluate.mcr <- function(model, test.data, test.values) {
  output = predict(model, test.data)
  mat = table(output, test.values)
  mcr = sum(diag(mat))/sum(mat)
  return(mcr)
}

model.fit.evaluate.rmse <- function(model, test.data, test.values) {
  output = predict(model, test.data)
  rmse = (sum((output - test.values)^2)) ^ 0.5
}