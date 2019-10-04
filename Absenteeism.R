#To clean the environment
rm(list=ls())

#To set up our working directory
setwd("E:/edWisor/Assignments & Solutions/Project_Employee")

#To check if our working directory is correct
getwd()

#To load the necessary packages required for our project
lib_req = c('readxl','caret','dplyr','class','randomForest','leaps','cowplot','ggplot2','rpart','DMwR','corrgram')
lapply(lib_req, require, character.only = TRUE)

#To load our data
emp_data = read_xls("Absenteeism_at_work_Project.xls")
emp_data = as.data.frame(emp_data) #transforming the data into a dataframe


###########################################################EXPLORING THE DATA####################################################

#Understanding of data viz., view the data, strucuture, dimension, summary, column names of the data respectively
View(emp_data) #To view the data
str(emp_data) #All the variables are given as numericals
dim(emp_data) #740 rows 21 columns
summary(emp_data) #To check the basic quantitative stats
colnames(emp_data) #to check all the column names

#From the above analysis, let's divide the data into continuous and categorical variables

continuous_variables = c('Distance from Residence to Work', 'Service time', 'Age',
                         'Work load Average day', 'Transportation expense',
                         'Hit target', 'Weight', 'Height', 
                         'Body mass index', 'Absenteeism time in hours')

categorical_variables = c("ID","Reason for absence","Month of absence","Day of the week",
                          "Seasons","Disciplinary failure","Education","Son","Social drinker",
                          "Social smoker","Pet")

#All the variables are considered as num type while importing the data
#Transforming categorical_variables into factor type

for (k in categorical_variables){
  emp_data[,k] = factor(emp_data[,k],labels = 1:length(levels(factor(emp_data[,k]))))
}

#Check the structure of our data
str(emp_data)

#To make a copy of dataset to use in the further steps
data_employee = emp_data
data_employee = as.data.frame(data_employee)
View(data_employee)

##################################################MISSING VALUE ANALYSIS###############################################################

#To find the missing values in the dataset and check whether to impute those values or drop those variables or observations

x = sapply(data_employee,function(x){sum(is.na(x))})
missing_values = data.frame(x)

#To get the row names as new column
missing_values$Variables = row.names(missing_values)

#Reset the rownames i.e., setting the index values
row.names(missing_values) = NULL

#Rename the column 1 name
names(missing_values)[1] = "Missing_percentage"

#Calculate the missing values percentage and if it is > 30%, we should remove that column from our data
missing_values$Missing_percentage= ((missing_values$Missing_percentage/nrow(data_employee))*100)

#Reorder the columns
missing_values = missing_values[,c(2,1)]

#Sorting the rows in descending order w.r.t. missing percentage values
missing_values = missing_values[order(-missing_values$Missing_percentage),]
View(missing_values)

#Bar plot to visualise top 7 missing percentage values
ggplot(data = missing_values[1:7,], mapping = aes(x = reorder(Variables, -Missing_percentage),y = Missing_percentage)) + 
  geom_bar(stat = 'identity', fill = 'blue') + xlab("Variable Name") + ylab("Missing Percentage") + 
  ggtitle("Missing Value Percentages") + theme_classic()

#Imputing the missing values
#Creating a missing value and check which method will give us the nearest value
#data_employee[["Body mass index"]][5] #Actual value = 30
#data_employee[["Body mass index"]][5] = 30 #Creating a missing value to check the best method for missing value imputation
#data_employee[["Body mass index"]][5] = mean(data_employee[["Body mass index"]], na.rm = T) #Mean value = 26.68
#data_employee[["Body mass index"]][5] = median(data_employee[["Body mass index"]], na.rm = T) #Median value = 25
data_employee = knnImputation(data = data_employee, k = 7) #kNN method value = 29.68

#To check if any NA values still exist
sum(is.na(data_employee))

#Saving our dataset without NAs
write.csv(data_employee, "Data_without_NA.csv", row.names = F)

#data_employee is free from missing values
#emp_data is our original data

#################################################EXPLORE THE DISTRIBUTION OF VARIABLES USING THE PLOTS#############################################

#Get the data with only numeric columns
num_index = sapply(data_employee, is.numeric)
numeric_data = data_employee[,num_index]
View(numeric_data)
View(data_employee)

#Get the data with only category data(factor) columns
cat_data = data_employee[,!num_index]

#For continuous variables let's use Histograms to represent the data
hist_transportexp = ggplot(data = numeric_data,aes(x =numeric_data[,1])) + 
  ggtitle("Transportation expense") + geom_histogram(bins = 25)
hist_height = ggplot(data = numeric_data, aes(x = numeric_data[,8])) +
  ggtitle("Distribution of height") + geom_histogram(bins = 25)
hist_bmi = ggplot(data = numeric_data, aes(x = numeric_data[,9])) +
  ggtitle("Body Mass Index distribution") + geom_histogram(bins = 25)
hist_absenteeism = ggplot(data = numeric_data, aes(x = numeric_data[,10])) +
  ggtitle("Absenteeism Time in Hours") + geom_histogram(bins = 25)
hist_distance = ggplot(data = numeric_data, aes(x = numeric_data[,2])) + 
  ggtitle("Distance from Residence") + geom_histogram(bins = 25)
hist_servicetime = ggplot(data = numeric_data, aes(x = numeric_data[,3])) +
  ggtitle("Service Time") + geom_histogram(bins = 25)
hist_age = ggplot(data = numeric_data, aes(x = numeric_data[,4])) +
  ggtitle("Age") + geom_histogram(bins = 25)
hist_avgworkloadpday = ggplot(data = numeric_data, aes(x = numeric_data[,5])) +
  ggtitle("Work load avg/day") + geom_histogram(bins = 25)
hist_hittarget = ggplot(data = numeric_data, aes(x = numeric_data[,6])) +
  ggtitle("Hit Target") + geom_histogram(bins = 25)
hist_weight = ggplot(data = numeric_data, aes(x = numeric_data[,7])) +
  ggtitle("Weight") + geom_histogram(bins = 25)


#To arrange all the plots of numerical variables distribution in one page
gridExtra::grid.arrange(hist_transportexp,hist_height,hist_bmi,hist_absenteeism,hist_weight,ncol = 3)
gridExtra::grid.arrange(hist_distance,hist_servicetime,hist_age,hist_avgworkloadpday,hist_hittarget,ncol = 3)


#Distribution of factor(categorical) data using bar plot
bar_id = ggplot(data = cat_data, aes(x = cat_data[,1])) + geom_bar() + 
  ggtitle("ID distribution") + theme_bw()
bar_reason = ggplot(data = cat_data, aes(x = cat_data[,2])) + geom_bar() + 
  ggtitle("Reason for Absence") + theme_bw()
bar_month = ggplot(data = cat_data, aes(x = cat_data[,3])) + geom_bar() + 
  ggtitle("Month of Absence") + theme_bw()
bar_day = ggplot(data = cat_data, aes(x = cat_data[,4])) + geom_bar() + 
  ggtitle("Day of the week") + theme_bw()
bar_seasons = ggplot(data = cat_data, aes(x = cat_data[,5])) + geom_bar() + 
  ggtitle("Seasons") + theme_bw()
bar_discipfail = ggplot(data = cat_data, aes(x = cat_data[,6])) + geom_bar() + 
  ggtitle("Disciplinary failure") + theme_bw()
bar_edu = ggplot(data = cat_data, aes(x = cat_data[,7])) + geom_bar() + 
  ggtitle("Education") + theme_bw()
bar_son = ggplot(data = cat_data, aes(x = cat_data[,8])) + geom_bar() + 
  ggtitle("Son") + theme_bw()
bar_socdrinker = ggplot(data = cat_data, aes(x = cat_data[,9])) + geom_bar() + 
  ggtitle("Social drinker") + theme_bw()
bar_socsmoker = ggplot(data = cat_data, aes(x = cat_data[,10])) + geom_bar() + 
  ggtitle("Social Smoker") + theme_bw()
bar_pet = ggplot(data = cat_data, aes(x = cat_data[,11])) + geom_bar() + 
  ggtitle("Pet") + theme_bw()

#Plotting all the above plots in one page
gridExtra::grid.arrange(bar_day,bar_discipfail,bar_edu,bar_id,bar_month,bar_pet,ncol = 3)
gridExtra::grid.arrange(bar_reason,bar_seasons,bar_socdrinker,bar_socsmoker,bar_son, ncol = 3)


######################################################## OUTLIER ANALYSIS ###########################################################

#Get the data with only numeric columns
num_index = sapply(data_employee, is.numeric)
numeric_data = data_employee[,num_index]
View(numeric_data)

#Get the data with only factor columns
cat_data = data_employee[,!num_index]
View(cat_data)

#Check for outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  assign(paste0("boxplot",i), ggplot(data = data_employee, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.75) +
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

gridExtra::grid.arrange(boxplot1,boxplot2,boxplot3,boxplot4,ncol=2)
gridExtra::grid.arrange(boxplot5,boxplot6,boxplot7,boxplot8,ncol=2)
gridExtra::grid.arrange(boxplot9,boxplot10,ncol=2)

#Imputing NAs into Outliers

#Check the number of missing values
for(i in colnames(numeric_data)){
  val = data_employee[,i][data_employee[,i] %in% boxplot.stats(data_employee[,i])$out]
  print(paste(i,length(val)))
  data_employee[,i][data_employee[,i] %in% val] = NA
}

#Imputing NAs with values using KNN Imputation method
data_employee = knnImputation(data = data_employee, k = 5)
#To ensure no NA values are left in our dataset
sum(is.na(data_employee))
View(data_employee)

#data_employee is now clean without missing values and outliers

##########################################FEATURE ENGINEERING############################################

#Extracting a few new variables from the existing variables
data_employee$age_bin = cut(data_employee$Age, breaks = c(25,35,45,60), labels = c("Young","Middle_aged","Old_age"))
data_employee$distance_bin = cut(data_employee$`Distance from Residence to Work`, breaks = c(0,30,55), labels = c("Nearest","Farthest"))
data_employee = data.frame(data_employee)
View(data_employee)
dim(data_employee) # 740 23
class(data_employee$age_bin)#To check if it is factor type or not
class(data_employee$distance_bin)#To check if it is factor type or not

#Plotting bar plots for the new variables

bar_agebin = ggplot(data = data_employee, aes(x = data_employee$age_bin)) + geom_bar() + 
  ggtitle("Age Bin distribution") + theme_bw()
bar_distancebin = ggplot(data = data_employee, aes(x = data_employee$distance_bin)) + geom_bar() + 
  ggtitle("Distance Bin distribution") + theme_bw()

#Plotting  them in a single page

gridExtra::grid.arrange(bar_agebin,bar_distancebin,ncol=1)

#########################################FEATURE SELECTION#################################################
#Checking for multicollinearity for continuous variables

cor = cor(numeric_data)
corrgram(cor,type = 'cor',lower.panel = panel.pie,diag.panel = panel.density,upper.panel = panel.conf,text.panel = panel.txt)

#As per the correlation plot, we observed that Body Mass Index and Weight are correlated to each other.Hence removing it

data_employee = subset.data.frame(data_employee, select = -c(Body.mass.index))
dim(data_employee)# 740 22
View(data_employee)

#data_employee is the cleaned data with no missing values, no outliers and no multicolinearity in continuous variables

#Chisquared test of independence
factor_idex = sapply(data_employee, is.factor)
factor_data = data_employee[,factor_idex]
#View(factor_data)

#for (i in 1:length(colnames(factor_data))){
  ##print(names(factor_data[i]))
  ##print(chisq.test(table(data_employee$Absenteeism.time.in.hours,factor_data[,i])))}

#Using random forest to find the important variables
imp_var <- randomForest(Absenteeism.time.in.hours~., data = data_employee, importance = T)
imp_df = data.frame(varImp(imp_var))
imp_df$imp_Variables = row.names(imp_df)
row.names(imp_df) = NULL
imp_df = imp_df[,c(2,1)]
imp_df = imp_df[order(imp_df$Overall, decreasing = T),]
View(imp_df)

fin_variables = imp_df$imp_Variables[1:16]#Top 16 variables
fin_modeldata = data_employee[,fin_variables]
fin_modeldata = cbind(fin_modeldata,data_employee$Absenteeism.time.in.hours)
colnames(fin_modeldata)[17] = "Absenteeism.time.in.hours"
View(fin_modeldata)
#fin_modeldata is our cleaned data. This will be passed to various ML algos

########################################################FEATURE SCALING#########################################################
#As the numeric data is not uniformly distributed, we will scale the data by using Normalization technique

num_index_finmodeldata = sapply(fin_modeldata,is.numeric)
num_data_finmodeldata = fin_modeldata[,num_index_finmodeldata]
num_columns = colnames(num_data_finmodeldata)
num_columns = num_columns[-9] #To remove our target variable
View(num_columns)

#Using normalisation technique
for(i in num_columns){
  print(i)
  fin_modeldata[,i] = (fin_modeldata[,i] - min(fin_modeldata[,i]))/
    (max(fin_modeldata[,i]) - min(fin_modeldata[,i]))
}

View(fin_modeldata)
#fin_modeldata = fin_modeldata[order(fin_modeldata$Reason.for.absence ,decreasing = T),]
#All the numeric data is now with normalised values i.e., in between 0 and 1
##########################################################DEVELOPMENT OF THE MODEL###################################################
#Divide the data into test and train
#Train data
set.seed(1)
train_index = sample(1:nrow(fin_modeldata),0.8*nrow(fin_modeldata)) #By random Sampling
train_data = fin_modeldata[train_index,]
dim(train_data) #592 17
test_data = fin_modeldata[-train_index,]
dim(test_data) #148 17



#####################################################MACHINE LEARNING MODELS#########################################################

##################LINEAR REGRESSION###############
#RMSE : 2.714622
#MAPE : 1.83199

linear_m = lm(train_data$Absenteeism.time.in.hours~.,data = train_data) #here Y variable is named as data_employee$Absenteeism.time.in.hours
summary(linear_m)
#Predict for new test cases
predict_linear = predict(linear_m,test_data[-17])
RMSE(predict_linear,test_data$Absenteeism.time.in.hours)
#Creating a function to find MAPE
MAPE = function(actual,predicted){
  mean(abs(actual - predicted))
}
MAPE(test_data$Absenteeism.time.in.hours,predict_linear)

#Plot a graph for actual vs predicted values
plot(test_data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(predict_linear,col="blue")


##################DECISION TREE######################

#Build decision tree model using rpart
#RMSE : 2.679105
#MAPE : 1.761385
decision_m = rpart(train_data$Absenteeism.time.in.hours~.,data = train_data, method = "anova")
predict_dt = predict(decision_m,test_data[-17])
RMSE(predict_dt,test_data$Absenteeism.time.in.hours)
MAPE(test_data$Absenteeism.time.in.hours,predict_dt)
plot(test_data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(predict_linear,col="blue")

#################RANDOM FOREST#######################
#RMSE : 2.679105
#MAPE : 1.691822

random_m = randomForest(train_data$Absenteeism.time.in.hours~.,data = train_data, ntree = 200)
predict_random = predict(random_m,test_data[-17])
RMSE(predict_dt,test_data$Absenteeism.time.in.hours)
MAPE(test_data$Absenteeism.time.in.hours,predict_random)
plot(test_data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(predict_linear,col="blue")












