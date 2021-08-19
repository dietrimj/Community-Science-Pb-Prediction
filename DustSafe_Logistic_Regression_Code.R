#Packages needed for logistic regression modeling and filtering
library(dplyr)
library(tidyverse)
library(caret)

#' Read in Dec. 2020-July 2021 DustSafe Data
Indy1 <- MME_NA_DustSafe_2021

#' Add a Normal Curve to histogram (Thanks to Peter Dalgaard)
#' Household dust Pb
x <- na.omit(Indy1$Pb)
h<-hist(x, breaks=100, col="red", xlab="House Dust Pb (mg/kg)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

#'Correlation between independent variables to test if violate assumptions of multicollinearity
p <- cor.test(Indy1$InteriorPeeling,Indy1$Housing, method=c("pearson"))
(p)

#'Filter for missing data of variables in initial model
IndyPredict <- na.omit(Indy1[,c(4, 24:25, 30, 28)])

#'Change to factor data
#'"Low" (< 80 mg/kg Pb) and "High" (> 80 mg/kg Pb)
IndyPredict$Pb_level_cat <- as.factor(IndyPredict$Pb_level_cat)

#'Split the data into training and test set with 80 mg/kg for high dust Pb threshold
set.seed(123)
training.samples <- IndyPredict$Pb_level_cat %>%
  createDataPartition(p=0.7, list=FALSE)
train.data  <- IndyPredict[training.samples, ]
test.data <- IndyPredict[-training.samples, ]

#'Multiple logistic regression
glm.fit <- glm(Pb_level_cat ~  InteriorPeeling + Housing, data = train.data, family = binomial)
summary(glm.fit)

#'Model probability of success for binomial factor variable
glm.probs <- glm.fit %>% predict(test.data, type = "response")
head(glm.probs)

#'Checking the dummy coding
contrasts(test.data$Pb_level_cat) #So a probability of 0.92 means 92% chance of a low Pb (<80 mg/kg) in dust

#'ROC curve to help set predictive thresholds
library(ROCR)

ROCRpred <- prediction(glm.probs, test.data$Pb_level_cat)

#' Performance function
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

#' Plot ROC curve
plot(ROCRperf)
#' Add colors
plot(ROCRperf, colorize=TRUE)
#' Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))

#'Classify if high or low dust Pb based on probability of predictive power from model
glm.pred <- ifelse(glm.probs > 0.8, "Low", "High")
#'Confusion matrix
table(glm.pred, test.data$Pb_level_cat)
#'Mean proportion of correct predictions
mean(glm.pred == test.data$Pb_level_cat)
