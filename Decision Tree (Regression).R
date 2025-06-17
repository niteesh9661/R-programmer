# Decision Tree using REGRESSION

#File name - Position Salaries at data sample

#CONCEPT 1 Change Col Names (it is done in excel to make it easier)

#CONCEPT 2 Import Data
salaries=read.csv(file.choose())
salaries = salaries[2:3]

#CONCEPT 3 Prepare and clean data (missing values) not required

#CONCEPT 4 (Structure and Summary)
str(salaries)
summary(salaries)

#CONCEPT 5 Factoring and Encoding not required

#CONCEPT 6 Split Data
set.seed(123)
split=sample.split(salaries$Salary, SplitRatio = 2/3)
training_set=subset(salaries, split == TRUE)
test_set=subset(salaries, split == FALSE)``
 
#CONCEPT 7 Feature Scaling not required as it is only applicable for classification not for regression


#CONCEPT 8 Build the Model(Training data used)
#Linear model = rpart(formula,data)
#where formula = dependant variable~independant variable and data = training_set
#Activate RPART
regressor=rpart(formula=Salary~.,data = salaries,control=rpart.control(minsplit = 1))
summary(regressor)

#CONCEPT 9 Test the Model and Comparing(Test Data used)
#Testing
y_pred = predict(regressor, data.frame(Level = 7.5))
#Comparing
y_pred



#CONCEPT 10 Visualization
# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(salaries$Level), max(salaries$Level), 0.01)
ggplot() +
  geom_point(aes(x = salaries$Level, y = salaries$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the tree
plot(regressor)
text(regressor)