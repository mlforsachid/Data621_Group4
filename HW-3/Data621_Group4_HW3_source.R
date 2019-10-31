
options(scipen = 999, digits=6, width=150)

library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(ROCR)
library(parallel)
library(Boruta)
library(kableExtra)

# Set working directory and load the training data

mydir = "C:/Users/Michael/Dropbox/priv/CUNY/MSDS/201909-Fall/DATA621_Nasrin/20191030_HW3_Crime/HW-3"
setwd(mydir)
train.df = read.csv("./Data/crime-training-data_modified.csv")


# 1. Data Exploration

## **Load Data**


numhead = 10
head(train.df,n = numhead) %>% 
  kable(row.names = 1:numhead) %>% 
  kable_styling(c("striped", "bordered")) 

print(paste("Number of columns (features) = ", ncol(train.df)))
print(paste("Number of rows (observations) = ", nrow(train.df)))


#### **Let's analyze the datatypes of each column:**

str(train.df)
targetAsFactor <- factor(train.df$target, labels=c("LowCrime","HighCrime"))
str(targetAsFactor)
summary(targetAsFactor)

## **Check missing values**


miss.cols = apply(train.df, 2, function(x) any(is.na(x)))
print(paste("Number of columns with missing values = ", length(colnames(miss.cols))))

## **Check Class Imbalance**


ggplot(train.df, aes(target, fill = targetAsFactor)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=1) +
  labs(title="Bar Chart: Counts for target variable", 
       caption="Source: Crime dataset for major city (Boston)") 

## **Boruta: Variable Importance**


set.seed(1)

Boruta(target ~ .  , data=train.df)->Bor.hvo
print(Bor.hvo)
plot(Bor.hvo,  cex.axis=0.75, las=2, main="Boruta algorithm for Feature Selection")

## **Check variable correlation**



  res2<-rcorr(as.matrix(train.df))
  respearson=rcorr(as.matrix(train.df),type = "pearson")
  resspearman=rcorr(as.matrix(train.df),type = "spearman")
  res3 <- cor(as.matrix(train.df))

#### Pearson rank correlation
  corrplot(corr = respearson$r, type = "upper", outline = T, order="hclust", 
           p.mat = respearson$P, sig.level = 0.05, insig = "blank", addCoef.col = "black",
           title = "\nRank Correlation (Pearson)",
           number.cex = 0.9, number.font = 1, number.digits = 2 )

#### Spearman rank correlation
  corrplot(corr = resspearman$r, type = "upper", outline = T,  order="hclust", 
           p.mat = resspearman$P, sig.level = 0.05, insig = "blank", addCoef.col = "black",
           title = "\nRank Correlation (Spearman)",
           number.cex = 0.9, number.font = 1, number.digits = 2)

#### actual correlations (not rank correlations)
  corrplot(corr = res3, type = "upper", outline = T,  order="hclust", 
           sig.level = 0.05, insig = "blank", addCoef.col = "black",
           title = "\nActual correlations (not rank correlations)",
           number.cex = 0.9, number.font = 1, number.digits = 2 )

## **Look for interactions**

### **Scatter plot of age vs. dis**


ggplot(train.df, aes(x = dis, y=age, color=targetAsFactor)) +
geom_point() + ggtitle("Percentage of aged housing vs. Distance from employment centers")


### **Scatter plot of indus vs. dis**

ggplot(train.df, aes(x = dis, y=indus, color=targetAsFactor)) +
geom_point() + ggtitle("Proportion of non-retail business acres per suburb vs. \nDistance from employment centers")

### **Scatter plot of nox vs. dis**

ggplot(train.df, aes(x = dis, y=nox, color=targetAsFactor)) +
geom_point() + ggtitle("Nitrogen Oxides Concentration vs. \nDistance from employment centers")


# 2. Data Preparation

## **Add Interaction Term**

train.df$intterm =  ((train.df$dis / (train.df$nox * train.df$indus)) + train.df$age) ^ 0.5

### **Scatter plot showing effect of newly added interaction term for categorizing data**

ggplot(train.df, aes(x = seq(1:nrow(train.df)), y=intterm, color=targetAsFactor)) +
geom_point() + ggtitle("Interaction term for each of the 466 cases") + labs(x = "Each case in training dataset (n=466)")

## **Bucketize rad variable**

train.df$radtrans = round(log(1+train.df$rad))

ggplot(train.df, aes(x=radtrans, fill=targetAsFactor))+
geom_bar(position=position_dodge()) +
geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black",
    position = position_dodge(0.9), size=3.5) +
ggtitle(label="Crime Rate bucketed by accessibility to radial highways",
        subtitle="1: low access; 2: medium access; 3: high access")

ggplot(train.df, aes(x = seq(1:nrow(train.df)), y=radtrans, color=targetAsFactor)) +
geom_point() +  
  geom_jitter(width = 0, height = 0.1) + 
  ggtitle("Bucketed radial highway accessibility for each of the 466 cases") + 
  labs(x = "Each case in training dataset (n=466)",
       y = "Radial highway accessibility: 1=low / 2=medium / 3=high")

## **Bucketize tax rate variable** 


# round(log(1+tax)) gives results in the set {5,6,7}
# So, subtracting 4 from the above makes the results {1,2,3} which makes more sense.
train.df$taxtrans = round(log(1+train.df$tax))-4

ggplot(train.df, aes(x=taxtrans, fill=targetAsFactor))+
geom_bar(position=position_dodge()) +
geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black",
    position = position_dodge(0.9), size=3.5) +
ggtitle(label="Crime Rate bucketed by property tax rate",
        subtitle="1: low tax; 2: medium tax; 3: high tax")


ggplot(train.df, aes(x = seq(1:nrow(train.df)), y=taxtrans, color=targetAsFactor)) +
geom_point() +  
  geom_jitter(width = 0, height = 0.1) + 
  ggtitle("Bucketed tax rate for each of the 466 cases") + 
  labs(x = "Each case in training dataset (n=466)",
       y = "tax rate: 1=low / 2=medium / 3=high")

## **Bucketize "lstat" (lower status percent) variable**    

train.df$lstattrans = round(log(train.df$lstat))

ggplot(train.df, aes(x=lstattrans, fill=targetAsFactor))+
geom_bar(position=position_dodge()) +
geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black",
    position = position_dodge(0.9), size=3.5) +
ggtitle(label="Crime Rate bucketed by transformed 'lower status' rate",
        subtitle="1: very low; 2: low; 3: medium; 4: high")


ggplot(train.df, aes(x = seq(1:nrow(train.df)), y=lstattrans, color=targetAsFactor)) +
geom_point() +  
  geom_jitter(width = 0, height = 0.1) + 
  ggtitle("Bucketed 'lstat' ('lower Status') rate for each of the 466 cases") + 
  labs(x = "Each case in training dataset (n=466)",
       y = "lstat (lower status) rate: 1=very low / 2=low / 3=medium / 4=high")

## **Bucketize ZN variable** (zoning for large lots) 

train.df$zntrans = round(log(1+train.df$zn^0.5))/2+1

ggplot(train.df, aes(x=as.factor(train.df$zntrans), fill=targetAsFactor))+
  geom_bar(position=position_dodge()) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black",
    position = position_dodge(0.9), size=3.5) +
ggtitle(label="Crime Rate bucketed by transformed 'ZN' (zoning) rate") + 
xlab("1: No large lots ; 2: Large lots")

ggplot(train.df, aes(x = seq(1:nrow(train.df)), y=zntrans, color=targetAsFactor)) +
geom_point() +  
  geom_jitter(width = 0, height = 0.1) + 
  scale_y_continuous(name="ZN (zoning): 1=No large lots; 2=Large lots", breaks=c(1,2)) +

  ggtitle("Bucketed 'ZN' (zoning) for each of the 466 cases") + 
  labs(x = "Each case in training dataset (n=466)")

# 3. Build Models

## **Model fitting and evaluation**

cross.validation <- function(K, model.formula, LCdata) {
  auclist = NULL
  accuracylist = NULL
  recalllist = NULL
  precisionlist = NULL
  fold.size <- nrow(LCdata) / K
  
  for(k in 1:K){
    
    
    testing.index <- (k-1)*fold.size + 1:fold.size
    
    training <- LCdata[-testing.index,]
    testing.org <- LCdata[testing.index,]
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
  }
  return(list("AUC" = mean(auclist), "Accuracy" = mean(accuracylist),  "Recall" = mean(recalllist), "Precision" = mean(precisionlist)))
}

df.metrix <<- NULL
print.model.matrix = function(model.name, matrixobj)
{
  
  print(paste("Printing Metrics for model: ", model.name))
  for(i in 1 : length(matrixobj))
  {
    df = data.frame("Model" = model.name, "Metrix"=names(matrixobj)[[i]], "Value" = matrixobj[[i]])
    df.metrix <<- rbind(df, df.metrix)
    print(paste(names(matrixobj)[[i]], ":", matrixobj[[i]]))
  }
  
}



## **Cross Validation**

## **1. Base Model**

model.metrix = cross.validation(5, "target~ rad+tax+lstat+age+indus+nox+zn+dis", train.df)
print.model.matrix("Base Model", model.metrix)

## **2. Model with Interaction Term**

model.metrix = cross.validation(5, "target~ rad+tax+lstat+age+indus+nox+zn+dis + intterm", train.df)
print.model.matrix("Model with interaction term", model.metrix)

## **3. Model with transformed variables**

model.metrix = cross.validation(5, "target~ rad+tax+lstat+age+indus+nox+zn+dis + intterm + radtrans + taxtrans", train.df)
print.model.matrix("Model with transformed variable term", model.metrix)
## **4. Model including all original variables**

model.metrix = cross.validation(5, "target~ rad+tax+lstat+age+indus+nox+zn+dis + medv+rm+ptratio +chas", train.df)
print.model.matrix("Model with all original variables", model.metrix)
## **5. Model with all original variables except chas**

model.metrix = cross.validation(5, "target~ rad+tax+lstat+age+indus+nox+zn+dis + medv+rm+ptratio", train.df)
print.model.matrix("Model with all original variables but chas", model.metrix)

# 4. Select Models

## Model #3 is best

ggplot(df.metrix, aes(x = Model, y=Value, fill=Metrix)) +
  geom_bar(stat='identity', position=position_dodge()) +
  labs(title="Bar Chart : Model performance evaluation", 
            caption="Source: Crime Rate dataset of a major city") +
geom_text(aes(label=Value), vjust=0.25, hjust=1.1, color="black",
    position = position_dodge(0.9), size=3.5) +
coord_flip()


### **Discussion:** 


selected.metrix <- df.metrix[df.metrix$Model %in% c('Model with all original variables but chas', 
                                              'Model with transformed variable term'), ]
ggplot(selected.metrix, aes(x = Metrix, y=Value, fill=Model)) +
  geom_bar(stat='identity', position=position_dodge()) +
  labs(title="Best 2 models performance evaluation", 
            caption="Source: Crime Rate dataset of a major city") +
  geom_text(aes(label=Value), vjust=0.25, hjust=1.1, color="black",
    position = position_dodge(0.9), size=3.5) +
  coord_flip()


# 5. Appendix

### **Fit model and predict on evaluation dataset**

training = train.df
testing = read.csv("./Data/crime-evaluation-data_modified.csv", stringsAsFactors = FALSE)
testing$intterm =  ((testing$dis / (testing$nox * testing$indus)) + testing$age) ^ 0.5

testing$radtrans = round(log(1+testing$rad))
testing$taxtrans = round(log(1+testing$tax))-4

model.formula = "target~ rad+tax+lstat+age+indus+nox+zn+dis + intterm + radtrans + taxtrans"


model <- glm(formula = model.formula,
             family = "binomial", data = training)
predicted <- predict(model, newdata = testing,type="response")
lables = ifelse(predicted > 0.5, 1, 0)

testing$targetproba = round(predicted,1)
testing$target = lables
testing = data.frame(testing)
write.table(testing, "./PredictedOutcome_V2.csv", row.names = FALSE, sep=",")

