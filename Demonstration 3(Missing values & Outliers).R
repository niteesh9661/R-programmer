#DEMONSTRATION 3
#file= Datasample- NA Outliers

#CONCEPT 1 Missing values(Missing values can be done by 3 methods)

#Step 1 Import Data
dataset=read.csv(file.choose())

#Step 2 Explore Data
str(dataset)
summary(dataset)

#Step 3 Prepare and clean data (missing values)
#METHOD 1(LAST CARRY FORWARD METHOD)
dataset.NAlocf=na.locf(dataset)
#METHOd 2(Fill with same number method)
dataset.NAfill=na.fill(dataset,33)
#Activate ZOO package
#Method 3(AVERAGE METHOD)
dataset$Sales_After[is.na(dataset$Sales_After)] = mean(dataset$Sales_After, na.rm = TRUE)

#CONCEPT 2 OUTLIERS CONCEPT
summary(dataset)
#Outliers is done because to fix the errors errors can be identified when v do 
#the summary and in console v can observe that the mydata min is 
#2.683 and max is 999.00 which is impossible and in the excel if v do a graph
#we can also observe the graph is different at some points in order to fix it 
#outliers are used to justify and correct the data
#Activate FORECAST package
dataset$Sales_After=tsclean(dataset$Sales_After)
#after outliers when we do summary we can observe the changes in min and max


#EXTRA(If they ask EDA it is nothing but exploring data such as str,summary and graph)
str(dataset)
summary(dataset)
ggplot(data=dataset, aes(x=Sales_After))+geom_histogram(binwidth = 10,fill='white',color='red')

