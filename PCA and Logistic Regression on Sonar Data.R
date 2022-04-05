# Prediction of mine vs rock 
path <- file.choose()
data <- read.csv(path, header = F)
summary(data)
str(data)

# converting sonar vs mine column V61 to factor
data$V61 <- as.factor(data$V61)
str(data)

# train -test split data

#install.packages("caTools")
library(caTools)
set.seed(111) # for repeatability
split <- sample.split(data, SplitRatio = 0.80)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

table(train$V61)
# there are 82 data rows for mine and 75 data rows for rock we can say that training data
# does not have imbalance problem

library(corrplot)
corr_data <- cor(data[, -61])
corrplot.mixed(corr_data, order = "AOE", number.cex = 0.5)


# the data has multicollinearity problem also there are 60 variables to predict MINE vs ROCK 
# We can use PCA for dimension reduction

PC <- prcomp(train[,-61], scale. = TRUE, center = TRUE )
attributes(PC)
summary(PC)

screeplot(PC, type = 'l')

# prediction with PCs

train_pc <- predict(PC, train)
train_pc <- data.frame(train_pc, train$V61)
test_pc <- predict(PC, test)
test_pc <- data.frame(test_pc, test$V61)

# logistic regression

library(nnet)
train_pc$train.V61 <- relevel(train_pc$train.V61, ref = "M")
mymodel <- multinom(train.V61~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 +PC8 + PC9 + PC10 + PC11 + PC12 + PC13 , data = train_pc)
# PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 +PC8 + PC9 + PC10 + PC11 + PC12 + PC13
# till PC13 approx 95 % of variability has been recorded
summary(mymodel)

p <- predict(mymodel, train_pc)
conf_train <- table(Predicted = p , Actual = train_pc$train.V61)
conf_train
accuracy_train <- sum(diag(conf_train))*100/sum(conf_train)
paste("The accuracy of training data using PCA and Logistic Regression is ", accuracy_train, "%", sep = "")

p_test <- predict(mymodel, test_pc)
conf_test <- table(Predicted = p_test ,Actual =  test_pc$test.V61)
conf_test
accuracy_test <- sum(diag(conf_test))*100/sum(conf_test)
paste("The accuracy of testing data using PCA and Logistic Regression is ", accuracy_test, "%", sep = "")
