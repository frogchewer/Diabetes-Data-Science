library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(klaR)
library(naivebayes)


#Load Data Set, Split into Test and Train using function

diabetes_data <- read.csv("Desktop/computerScience/Rfiles/Data 101 Project/diabetes_data.csv")

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

diabetes_data = diabetes_data[sample(1:nrow(diabetes_data)), ]

diabetes_data <- diabetes_data %>% 
  mutate(Diabetes = factor(Diabetes, levels = c(0,1), labels = c("No Diabetes", "Diabetes")), 
  Sex = factor(Sex, levels = c(0,1), labels = c("Female", "Male")),
  HighChol = factor(HighChol, levels = c(0,1), labels = c("Low or Normal Chol", "High Chol")),
  #HighBP = factor(HighBP, levels = c(0,1), labels = c("Low or Normal BP", "High BP")),
  CholCheck = factor(CholCheck, levels = c(0,1), labels = c("No Recent Check", "Recent Check")),
  Smoker = factor(Smoker, levels = c(0,1), labels = c("Nonsmoker", "Smoker")),
  HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0,1), labels = c("No Heart Trouble", "Heart Trouble")),
  PhysActivity = factor(PhysActivity, levels = c(0,1), labels = c("Not Active", "Active")),
  Fruits = factor(Fruits, levels = c(0,1), labels = c("Does not eat fruits", "Eats Fruits")),
  Veggies = factor(Veggies, levels = c(0,1), labels = c("Does not eat veggies", "Eats veggies")),
  HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0,1), labels = c("Does not drink heavily", "Drinks heavily")),
  DiffWalk = factor(DiffWalk, levels = c(0,1), labels = c("No Difficulty Walking", "Difficulty Walking")),
  Stroke = factor(Stroke, levels = c(0,1), labels = c("No Stroke", "Stroke")),
  Age = factor(Age, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13), labels = c("18-24","25-29","30-34" ,"35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79" ,"80+")),
  )



#Summary
summary(diabetes_data)
head(diabetes_data)
tapply(diabetes_data$GenHlth, diabetes_data$Diabetes, mean)
health <- table(diabetes_data$GenHlth, diabetes_data$Diabetes)
health
barplot(health[,1])
barplot(health[,2])

summary(subset(diabetes_data, diabetes_data$Diabetes == "Diabetes"))
summary(subset(diabetes_data, diabetes_data$Diabetes != "Diabetes"))

#rpart fit to dataset for a particular quality

data_train <- create_train_test(diabetes_data, 0.8, train = TRUE)
data_test <- create_train_test(diabetes_data, 0.8, train = FALSE)
fit <- rpart(Diabetes~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

#test rpart results
pred_rpart=predict(fit, data_test,type="class")

#Confusion Matrix
tab_rpart = table(pred_rpart,data_test$Diabetes)
caret::confusionMatrix(tab_rpart)

#nb for the same quality
nb_mod <- NaiveBayes(Diabetes~., data=data_train, laplace = 1)

#test nb results
pred <- predict(nb_mod, data_test)

#Confusion Matrix
tab_nb <- table(pred$class, data_test$Diabetes)
caret::confusionMatrix(tab_nb)

