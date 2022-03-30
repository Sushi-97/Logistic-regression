library(corrplot)

setwd("D:/study/MBA/2nd term/PPA")

BCD <- read.csv("BCD.csv",stringsAsFactors = TRUE)
str(BCD)
summary(BCD)

BCD<- na.omit(BCD)

#convert dependent variable into 1 and 0
BCD$class <- ifelse(BCD$class == "malignant",1,0)


#check the correlation
cr <- cor(BCD)
corrplot.mixed(cr)


#convert dependent to categorical
BCD$class <- as.factor(BCD$class)


#Logistic Regression
model1 <- glm(class~ thick, data=BCD,family=binomial)
summary(model1)

#Significant variable
#Null deviance(how well your response variable is predicted with only intercept) 
#Residual deviance(how well your response variable is predicted in presence of independent variables(Should be low)) 
#AIC(Alkakki info Criterion: Trade of between complexity and Explanability(Should be on a lower End))

model2 <- glm(class~ thick+u.size,data=BCD,family=binomial)
summary(model2)

model3 <- glm(class~ thick+u.size+u.shape, data=BCD,family=binomial)
summary(model3)

model4 <- glm(class~ thick+u.size+u.shape+adhsn, data=BCD,family=binomial)
summary(model4)

model5 <- glm(class~ thick+u.size+u.shape+adhsn+nucl, data=BCD,family=binomial)
summary(model5)

model6 <- glm(class~ thick+u.size+u.shape+adhsn+nucl+s.size, data=BCD,family=binomial)
summary(model6)

model7 <- glm(class~ thick+u.size+u.shape+adhsn+nucl+s.size+chrom, data=BCD,family=binomial)
summary(model7)

model8 <- glm(class~ thick+u.size+u.shape+adhsn+nucl+s.size+chrom+n.nuc, data=BCD,family=binomial)
summary(model8)

model9 <- glm(class~ thick+u.size+u.shape+adhsn+nucl+s.size+chrom+n.nuc+mit, data=BCD,family=binomial)
summary(model9)

#removal of variables
model10 <- glm(class~ thick+u.shape+adhsn+nucl+s.size+chrom+n.nuc+mit, data=BCD,family=binomial)
summary(model10)

model11 <- glm(class~ thick+u.shape+adhsn+nucl+chrom+n.nuc+mit, data=BCD,family=binomial)
summary(model11)

#rejecting model12 with removal of mit as the value of AIC and residual deviance is increasing 
model12 <- glm(class~ thick+u.shape+adhsn+nucl+chrom+n.nuc, data=BCD,family=binomial)
summary(model12)

res <- predict(model11, BCD, type = "response")
head(res)
head(BCD$class)

table(Actualvalue=BCD$class, predictedvalue=res>0.20)
table(Actualvalue=BCD$class, predictedvalue=res>0.225)
table(Actualvalue=BCD$class, predictedvalue=res>0.25)

#15/11/2021
# ROCR curve- Tradeoff between True +ve(TP/P) rate and false +ve rate(FP/N)
library(ROCR)
ROCRpred <- prediction(res,BCD$class)
ROCRpref <- performance(ROCRpred,"tpr","fpr")
plot(ROCRpref, colorize=TRUE, print.cutoff.at.=seq(0.2,by=0.3))
