#Loading dataset
credit <- read.csv(file.choose(), sep=",") 
str(credit)
summary(credit)
head(credit)

#install packages
install.packages("aod")
library(aod)
library(ggplot2)

#dataset overview
summary(credit)
str(credit)

xtabs(~default_nextmonth+sex,data=credit)

#set default_nextmonth as factor
credit$default_nextmonth<-factor(credit$default_nextmonth,levels = c(0, 1))

#building the model
model1<-glm(default_nextmonth~sex+education+marriage+age+pay_2+pay_3+pay_4+pay_5+pay_6+bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6+pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6, data=credit,family="binomial")
summary(model1)

#visualization
op<-par(mfrow=c(2,2),pty="s")
#plot
plot(model1)

#confint
confint(model1)
confint.default(model1)


#odds ratios only
exp(coef(model1))
exp(cbind(OR = coef(model1), confint(model1)))

#Order result to see which variable increases the most the possibility of defaulting..
ORC_SUM<-exp(cbind(OR = coef(model1), confint(model1)))
ORC_SUM
ORC_SUM<-data.frame(ORC_SUM)
ORC_SUM[order(ORC_SUM$OR, decreasing=TRUE),]

#PredictedPob + confidence interval calculation
credit2 <- cbind(credit, predict(model1, newdata = credit, type = "link", se = TRUE))
credit2
credit2 <- within(credit2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

#save result
getwd()
setwd("c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando")
write.csv(credit2,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando\\log_reg_probped.csv", row.names = FALSE)

#double check PredictedProb
credit2[order(credit2$PredictedProb,decreasing=TRUE),]

#testing model1 against null model
with(model1, null.deviance - deviance)
with(model1, df.null - df.residual)
with(model1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(model1)                                                                                                    
