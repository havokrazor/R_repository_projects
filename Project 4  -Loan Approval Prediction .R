read.csv('cutomer_loan.csv') -> customer_loan
View(customer_loan)
customer_loan  -> customer_loan1
## Check Missing Values
colnames(customer_loan1)

sapply(customer_loan1, function(state) sum(is.na(state)))

library(dplyr)

#b
mutate(customer_loan1, dti= debts / income) -> customer_loan1

customer_loan1$loan_decision_status <- ifelse(customer_loan1$loan_decision_type == 'Denied', 0, 1)

as.factor(customer_loan1$loan_decision_status) -> customer_loan1$loan_decision_status

select(customer_loan1, 3,4,6,7,8,11,13,14) ->    customer_loan_refined

customer_loan_refined$gender   <- as.numeric(as.factor(customer_loan_refined$gender))-1
customer_loan_refined$marital_status <- as.numeric(as.factor(customer_loan_refined$marital_status))-1
customer_loan_refined$occupation<- as.numeric(as.factor(customer_loan_refined$occupation))-1
customer_loan_refined$loan_type<- as.numeric(as.factor(customer_loan_refined$loan_type))-1


library(caTools)

sample.split(customer_loan_refined$loan_decision_status, SplitRatio = 0.7) -> split_loan
subset(customer_loan_refined , split_loan == T) -> train_loan 
subset(customer_loan_refined, split_loan == F) -> test_loan

norm_Scale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

train_loan[,-8]%>%mutate_all(norm_Scale) -> train1
test_loan[,-8]%>%mutate_all(norm_Scale) -> test1


#Feature scaling and applying PCA , scaling using scale. parameter
traintest_combined <- rbind(train_loan, test_loan)

prin_comp <- prcomp(traintest_combined[,-8], scale. = T)

summary(prin_comp)

plot(prin_comp)


names(prin_comp)

prin_comp$center

prin_comp$rotation[1:5,]

biplot(prin_comp, scale = 0)

plot(prin_comp)

data.frame(loan_decision_status = traintest_combined$loan_decision_status, prin_comp$x) -> train_data


### Select only 2 PCA
train_data <- train_data[,1:3]

train_data$loan_decision_status <- as.factor(train_data$loan_decision_status)


sample_tag<- sample.split(train_data$loan_decision_status,SplitRatio = 0.7)
trainm <- subset(train_data,sample_tag)
testm <- subset(train_data,sample_tag==F)

library(e1071)
naiveBayes(trainm[,-1],trainm$loan_decision_status) -> model_naive
predictn <-predict(model_naive,newdata = testm)

library(caret)
confusionMatrix(predictn,testm$loan_decision_status)
