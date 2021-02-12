#credit - adatimport & conversion of credit$default_nextmonth to factor
credit <- read.csv(file.choose(), sep=",") 
str(credit)
summary(credit)

#transform default_nextmonth as factor
credit$default_nextmonth<-as.factor(credit$default_nextmonth) 

#set tables
table(credit$sex)
table(credit$age)
table(credit$marriage)
table(credit$default_nextmonth)

#historgams - check data
library(ggplot2)
#plot age  and eductaion
ggplot(data=credit, aes(x=age))+geom_histogram(binwidth = 2, aes(fill=education), colour="Black")+xlim(21,75)+coord_cartesian(ylim=c(0,2200))

#plot age and sex
ggplot(data=credit, aes(x=age))+geom_histogram(binwidth = 2, aes(fill=sex), colour="Black")+xlim(21,75)+coord_cartesian(ylim=c(0,2200))                

#aggregate payx different values separately in order to try another way of plotting
count_pay_2<-aggregate(data.frame(count = credit$pay_2), list(value = credit$pay_2), length)
count_pay_3<-aggregate(data.frame(count = credit$pay_3), list(value = credit$pay_3), length)
count_pay_4<-aggregate(data.frame(count = credit$pay_4), list(value = credit$pay_4), length)
count_pay_5<-aggregate(data.frame(count = credit$pay_5), list(value = credit$pay_5), length)
count_pay_6<-aggregate(data.frame(count = credit$pay_6), list(value = credit$pay_6), length)
count_pay_6
c23<-merge(count_pay_2,count_pay_3, by.x="value", by.y="value")
colnames(c23)<-c('values', 'pay_2','pay_3')
c23

c234<-merge(c23,count_pay_4, by.x="values", by.y="value")
colnames(c234)<-c('values', 'pay_2','pay_3','pay_4')
c234

c2345<-merge(c234,count_pay_5, by.x="values", by.y="value")
colnames(c2345)<-c('values', 'pay_2','pay_3','pay_4','pay_5')
c2345

c23456<-merge(c2345,count_pay_6, by.x="values", by.y="value")
colnames(c23456)<-c('values', 'pay_2','pay_3','pay_4','pay_5','pay6')
c23456

#plotting with reshape2 
install.packages("reshape2") 
library("reshape2")
?melt()
df1 <- melt(c23456, "values")
df1
write.csv(df1,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando\\dect_count_payx.csv", row.names = FALSE)

g1 <- ggplot(df1, aes(x = values, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-40, hjust=.1))
  
#set order of rows randomly in order to eliminate any preset ordering
set.seed(12345)
credit_rand <- credit[order(runif(22152)), ]

#set training and test data
head(credit$bill_amt1)
head(credit_rand$bill_amt1)
credit_train <- credit_rand[1:18000, ]
credit_test <- credit_rand[18001:22152, ]
prop.table(table(credit_train$default_nextmonth))
prop.table(table(credit_test$default_nextmonth))

#import C50 package
install.packages("C50") 
library(C50)

#credit_model <- C5.0(credit_train[-21], credit_train$default)
credit_model <- C5.0(credit_train[-24], credit_train$default_nextmonth)
str(credit_train)
credit_model
summary(credit_model)
credit_pred <- predict(credit_model, credit_test)
credit_pred

#install gmodels package
install.packages('gmodels')
library(gmodels)

#create crosstable
?CrossTable
CrossTable(credit_test$default_nextmonth, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
credit_boost10 <- C5.0(credit_train[-24], credit_train$default_nextmonth, trials = 10)
credit_boost10
summary(credit_boost10)
plot(credit_boost10)
plot(credit_model)

#boost result
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default_nextmonth, credit_boost_pred10,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

#corrigate with cost table
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
credit_cost <- C5.0(credit_train[-24], credit_train$default_nextmonth, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default_nextmonth, credit_cost_pred,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))
plot(credit_cost)
