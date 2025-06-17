# Logistic Regression
#File - Logistic Regression - Data - Social networks

#CONCEPT 1 Change Col Names (it is done in excel to make it easier)

#CONCEPT 2 Import Data
dataset = read.csv(file.choose())
dataset = dataset[3:5]

#CONCEPT 3 Prepare and clean data (missing values) not required

#CONCEPT 4 (Structure and Summary)
str(dataset)
summary(dataset)

#CONCEPT 5 Factoring and Encoding
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#CONCEPT 6 Split Data
#(Activate catools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#CONCEPT 7 Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#CONCEPT 8 Build the Model(Fitting Logistic Regression to the Training set)
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

#CONCEPT 9 Test the Model and Comparing(Predicting the Test set results)
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

y_pred
test_set$Purchased

cm=table(test_set$Purchased,y_pred>0.5)
cm

#CONCEPT 10 Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)

#CONCEPT 11 Visualization
library(ElemStatLearn)
#Visualising the Training set results
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))