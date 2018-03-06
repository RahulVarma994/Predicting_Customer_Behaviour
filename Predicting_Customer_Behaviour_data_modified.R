cus <- read.csv(file = "D:/marketinganalysis/Predicting_Customer_Behaviour_data_modified.csv")
colnames(cus)
#doing chisquare test on marital and PURCHASE
library(MASS) 
tbl = table(cus$MARITAL, cus$PURCHASE) 
chisq.test(tbl) 
# null hypothesis is two are independent 
# X-squared = 0.19439, df = 1, p-value = 0.6593(we accepted null so their is no dependent on this two variables)
################

# chisquAre on product and age 

library(dplyr)

cusagegroup <- cus%>% mutate(agegroup = ifelse(USER_AGE %in% 0:25,"1",ifelse(USER_AGE %in% 25:30 ,"2","3" )))
dim(cusagegroup)
tbl = table(cusagegroup$agegroup,cusagegroup$PURCHASE)

chisq.test(tbl)
                              
# null hypothesis is two are independent 
#X-squared = 2.3156, df = 2, p-value = 0.3142(we accepted null so their is no dependent on this two variables)                         

# which product  and brands have more access duration
new_cus <- cusagegroup%>% group_by(PRODUCT,BRANDCODE) %>% summarise( avgtime = mean(DURATION))%>%arrange(-avgtime)
###########################
# does last login has any effect on purchase 
#chisquAre on last login and purchase 

cusagegroup = cusagegroup%>%mutate(range=cut(LASTLOGIN, breaks= 5, labels=c("1","2","3","4","5")))
tbl <- table(cusagegroup$range,cusagegroup$PURCHASE)
chisq.test(tbl)
# null hypothesis is two are independent 
#X-squared = 3.4071, df = 4, p-value = 0.4921(we accepted null so their is no dependent on this two variables) 
###########################################################
# does user job has any effect on purchase
tbl <- table(cusagegroup$USER_JOB_DET,cusagegroup$PURCHASE)
chisq.test(tbl)
# null hypothesis is two are independent 
#X-squared = 1.9076, df = 4, p-value = 0.75281(we accepted null so their is no dependent on this two variables)
###########################################################
# income and payment
new1 <- cusagegroup%>% filter(PURCHASE == 1)
cusagegroup = new1%>%mutate(incomerange=cut(USER_INCOME, breaks= 6, labels=c("1","2","3","4","5","6")))
tbl <- table(cusagegroup$incomerange,cusagegroup$PAYMENT_DET)
chisq.test(tbl)
# null hypothesis is two are independent 
#X-squared = 3194.3, df = 15, p-value < 2.2e-16(we reject null so their is  dependentance on this two variables)
#########################################################

# applying logestic regression
library(caTools)
set.seed(123)
split = sample.split(cus$PURCHASE, SplitRatio = 0.70)
training_set = subset(cus, split == TRUE)
#head(training_set)
test_set = subset(cus, split == FALSE)
#head(test_set)


classifier = glm(formula = PURCHASE ~ DURATION+ PRICE +USER_INCOME,
                 family = binomial,  
                 data = training_set)

summary(classifier)
pred <- predict(classifier2, test_set[,-16])
y_pred = ifelse(pred > 0.5, 1, 0)
confmat = table(test_set[, 16], y_pred)
confmat
accuracy<- sum(diag(confmat))/sum(confmat)

accuracy


classifier2 = glm(formula = PURCHASE ~ DURATION +USER_INCOME + PRICE,
                  family = binomial,  
                  data = training_set)


pred2 <- predict(classifier2, test_set[,-16])
y_pred2 = ifelse(pred2 > 0.5, 1, 0)
confmat2 = table(test_set[, 16], y_pred2)
confmat2
accuracy2<- sum(diag(confmat2))/sum(confmat2)

accuracy2
 

classifier3 = glm(formula = PURCHASE ~ PRODUCT +USER_INCOME + PRICE + NO_ITEMS ,
                  family = binomial,  
                  data = training_set)


pred3 <- predict(classifier3, test_set[,-16])
y_pred3 = ifelse(pred3 > 0.5, 1, 0)
confmat3 = table(test_set[, 16], y_pred3)
confmat3
accuracy3<- sum(diag(confmat3))/sum(confmat3)

accuracy3
###################
classifier4 = glm(formula = PURCHASE ~ PRODUCT +USER_INCOME + PRICE + NO_ITEMS + BIRTH_YEAR+User_hist,
                  family = binomial,  
                  data = training_set)


pred4 <- predict(classifier4, test_set[,-16])
y_pred4 = ifelse(pred4 > 0.5, 1, 0)
confmat4 = table(test_set[, 16], y_pred4)
confmat4
accuracy4<- sum(diag(confmat4))/sum(confmat4)

accuracy4