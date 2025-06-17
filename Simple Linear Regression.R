#Simple Linaer Regression (is is said to be simple linear regression when their 
#is only one independant variable)
#Salary
#File (Regression-Data-Salary)

#CONCEPT 1 Change Col Names (it is done in excel to make it easier)

#CONCEPT 2 Import Data
salary=read.csv(file.choose())

#CONCEPT 3 Prepare and clean data (missing values) not required

#CONCEPT 4 (Structure and Summary)
str(salary)
summary(salary)

#CONCEPT 5 Factoring and Encoding not required

#CONCEPT 6 Split Data
set.seed(123)
split=sample.split(salary$Salary, SplitRatio = 0.8)
training_set=subset(salary, split == TRUE)
test_set=subset(salary, split == FALSE)

#CONCEPT 7 Feature Scaling(not requird in Regeression Problem)

#CONCEPT 8 Build the Model(Training data used)
#Linear model = lm(formula,data)
#where formula = dependant variable~independant variable and data = training_set
regressor=lm(formula = Salary~YearsExperience,data = training_set)
summary(regressor)
#NOTE- in the console section stars are represented in that if there is no stars 
#represented or available v will not proceed the prediction

#CONCEPT 9 Test the Modeland Comparing(Test Data used)
#Testing
y_pred=predict(regressor,data=test_set)
#Comparing
y_pred
test_set$Profit

#CONCEPT 10 Visualization 
#FOR TRAINING SET
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

#FOR TEST SET
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')
