library(dplyr)
library(haven)

salary <- read.csv("Salaries.csv")
View(salary)
str(salary)

# filtering useful features
salary <- salary %>% select(JobTitle, Year, BasePay, OvertimePay, OtherPay, TotalPay)
View(salary)
class(salary)

# checking data type
sapply(salary, class)

#checking for unique jobtitles
jobtitles <- unique(salary$JobTitle)
jobtitles

#counting jobtitles available in the data
count_jobtitle <- salary %>% group_by(JobTitle) %>% count(JobTitle)
count_jobtitle

# summary satistics
sumary_stat <- salary %>% group_by(Year) %>% summarise(across(where(is.numeric), mean ))
sumary_stat

sumary_sd <- salary %>% group_by(Year) %>% summarise(across(where(is.numeric), sd))
sumary_sd
summary(salary[,3:6])

# data preprocessing

#check for messing values
sum(is.na(salary))
salary %>% summarise_all(funs(sum(is.na(.))))

View(colSums(is.na(salary)))

# list rows with missing data
missingdata <- salary[!complete.cases(salary),]
missingdata
sum(is.na(missingdata))

# delecting the missing values
clean_data <- na.omit(salary)
clean_data
sum(is.na(clean_data))

# handing missing data by imputation (MICE)
install.packages("mice")
library(tidyverse)
library(mice)

# starting the mice process
# looking the paterns of the missing data

md.pattern(salary)

# plot
install.packages("VIM")
library(VIM)
agg_plot <- aggr(salary, col = c("navyblue", 'red'), numbers = TRUE,
                 sortvars = TRUE,
                 labels = names(salary),
                 cex.axis = 0.8,
                 cex.numbers = 0.7,
                 gap = 4,
                 ylab = c("Histogram of missing value", "pattern"))


# using Mice method to impute the missing values. 
imputed_data_def <- mice(salary, m=5)
imputed_data_def
attributes(imputed_data_def)
summary(imputed_data_def)

#Basepay
imputed_data_def$imp$BasePay

#complete data
salary_comp <- complete(imputed_data_def)
salary_comp
view(salary_comp)
aggr(salary_comp)

# comparing the original data with the imputed
# change grap view to 2x2
par(mfrow = c(2,2))

# checking the distrubution of the imputed and origial
boxplot(salary$BasePay, main = "data with missing values")
boxplot(salary_comp$BasePay, main = "data with imputed values")

# comparing the two dataset using t-test, since the pvalue is above 0.05
# we accept the null hypothesis that there is no effect of imputation to the actal dataset
t.test(salary$BasePay,salary_comp$BasePay)

# uswing random forest 
imputed_data <- mice(salary, m=5, method = "rf")
summary(imputed_data)
imputed_data$imp$BasePay

# complete data for RF 
salary_comp_rf <- complete(imputed_data, 1)
salary_comp_rf
view(salary_comp_rf)
# checking if there is any missing data
sum(is.na(salary_comp_rf))

# comparing the distribution of the imputed and original data
t.test(salary$BasePay,salary_comp_rf$BasePay)


#------------------- EDA ---------------------------------------------------------
sumary_sta <- salary_comp_rf %>% group_by(Year) %>% 
  summarise(across(where(is.numeric), mean ))
sumary_sta


#-------------------- predictive model --------------------------------------------------

library(caret)

set.seed(2344)
y <- salary_comp_rf$TotalPay
#salary_comp_rf$TotalPay <- factor(salary_comp_rf$TotalPay)
trainingindex <- createDataPartition(y, p=0.8, list = FALSE)
trainingset <- salary_comp_rf[trainingindex,]
testset <- salary_comp_rf[-trainingindex,]
View(trainingset)

# fitting the model
contrl <- trainControl(method = "cv", number = 10, p=.9)
fit_glm_cv <- train(TotalPay ~ BasePay + OvertimePay + OtherPay,
                    method = "glm", data = trainingset, 
                    trControl = contrl,
                    preProc = c("center", "scale"))

fit_glm_cv

# looking at variable importance
varImp(fit_glm_cv)

# applying model for prediction
pred <- predict(fit_glm_cv, testset, type = "raw")
pred


install.packages("MLmetrics")
library(MLmetrics)

acc <- MLmetrics::Accuracy(pred, testset$TotalPay)
acc


# confusion matrix
test_cm <- confusionMatrix(pred, testset$TotalPay)



library(randomForest)
set.seed(2333)
contro1 <- trainControl(method = "cv", number = 10, p=.9)
fit_rf <- randomForest(TotalPay ~ BasePay + OvertimePay + OtherPay, data = trainingset)

fit_rf
varImp(fit_rf)
plot(fit_rf)