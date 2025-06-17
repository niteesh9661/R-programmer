#Mutiple Linear Regression(it is said to be mutiple regression when there are
#one or more independant variables
#50 STARTUPS
#File (Regression-Data-50 startups)

#CONCEPT 1 Change Col Names (it is done in excel to make it easier)

#CONCEPT 2 Import Data
startup=read.csv(file.choose())

# Change colname

#CONCEPT 3 Prepare and clean data (missing values) not required

#CONCEPT 4 (Structure and Summary)
str(startup)
summary(startup)

#CONCEPT 5 Factoring and Encoding 
startup$State=factor(startup$State,
                     levels = c('New York','California','Florida'),
                     labels = c(1,2,3))

#CONCEPT 6 Split Data
#(Activate catools)
set.seed(123)
plit =sample.split(startup$Profit, splitRatio = 0.8)
training_set=subset(startup, split == TRUE)
test_set=subset(startup, split == FALSE)

#CONCEPT 7 Feature Scaling not required

#CONCEPT 8 Build the Model(Training data used)
#Linear model = lm(formula,data)
#where formula = dependant variable~independant variable and data = training_set
regressor=lm(formula=Profit~.,data = training_set)
summary(regressor)
#(Here in the console we can observe that there are many rows without stars which
#means they are not in relation with dependant variable hence keep the row with 
#stars and eliminate rest rows and keep any of the one to keep model as 
#multiple linear regression hence rebuild the model)

#REBUILD THE MODEL
regressor=lm(formula=Profit~R.D.Spend+State,data = training_set)
summary(regressor)

#CONCEPT 9 Test the Model and Comparing(Test Data used)
#Testing
y_pred=predict(regressor,data=test_set)
#Comparing
y_pred
test_set$Profit

#CONCEPT 10 Visualization
#In mutiple regression for Visualization v need to activate GGPLOT and GGIRAPH and GGIRAPHEXTRA
ggPredict(regressor,se=TRUE,interactive = FALSE)

#With GGPLOT
ggplot() +
  geom_point(aes(x=training_set$R.D.Spend, y=training_set$Profit),colour = 'red') +
  geom_line(aes(x=training_set$R.D.Spend, y=predict(regressor, newdata = training_set)),colour = 'blue') +
  ggtitle('Profit v/s R.D.Spend (Training set)') +
  xlab('R.D. Spend') +
  ylab('Profit')

ggplot() +
  geom_point(aes(x=test_set$R.D.Spend, y=test_set$Profit),colour = 'red') +
  geom_line(aes(x=training_set$R.D.Spend, y=predict(regressor, newdata = training_set)),colour = 'blue') +
  ggtitle('Profit v/s RD Spend (Test set)') +
  xlab('RD Spend') +
  ylab('Profit')

