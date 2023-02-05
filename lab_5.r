# Group-Name-FANTASIC FOUR
# MEMBERS
# 1.ADARSH KUMAR SINGH-202051007
# 2.AYUSH SINGH-202051042
# 3.ABHISHEK KUMAR YADAV-202051004
# 4.ANILKUMAWAT-202051026


course.grades<-read.table("/content/2020_bn_nb_data.txt",head=TRUE)
head(course.grades)
install.packages("epiDisplay")
install.packages("bnclassify")
install.packages("e1071")
install.packages("caret")
install.packages("bnlearn")
library(bnlearn)
library(epiDisplay)
library(bnclassify)
library(e1071)
library(caret)
# 1.Consider grades earned in each of the courses as random variables and learn the dependencies between courses.

### Using bic score
course.grades<- lapply(course.grades,as.factor)
course.grades<- data.frame(course.grades)
course.grades.net<- hc(course.grades[,-9],score = 'bic')
# print(Plot using bic score)

plot(course.grades.net)



### Using k2 score


course.grades<- lapply(course.grades,as.factor)
course.grades<- data.frame(course.grades)
course.grades.net<- hc(course.grades[,-9],score = 'k2')

# print(Plot using k2 score)
plot(course.grades.net)

# 2. Using the data, learn the CPTs for each course node

course.grades.fit <- bn.fit(course.grades.net,course.grades[,-9])
course.grades.fit$EC100
course.grades.fit$EC160
course.grades.fit$PH100
course.grades.fit$IT101
course.grades.fit$IT161
course.grades.fit$MA101
course.grades.fit$HS101
course.grades.fit$PH160

# 3.What grade will a student get in PH100 if he earns DD in EC100, CC in IT101 and CD in MA101.

course.grades.PH100Grade <- data.frame((cpdist(course.grades.fit, nodes=c("PH100"), evidence= (EC100 == "DD") & (IT101 == "CC") & (MA101 == "CD"))))
tab1(course.grades.PH100Grade, sort.group = "decreasing", main = "Distribution of grades in PH100 with given evidence")

# 4.The last column in the data file indicates whether a student qualifies for an internship program or not. From the given data, take 70 percent data for training and build a naive Bayes classifier (considering that the grades earned in different courses are independent of each other) which takes in the student’s performance and returns the qualification status with a probability. Test your classifier on the remaining 30 percent data. Repeat this experiment for 20 random selection of training and testing data. Report results about the accuracy of your classifier.


# splitting data into training and testing sets
split_data <- function() {
  split <- sample(c(rep(0, 0.7*nrow(course.grades)), rep(1, 0.3*nrow(course.grades))))
  data_train <- course.grades[split == 0,]
  data_test <- course.grades[split == 1,]
  list("data_train" = data_train, "data_test" = data_test)
}
library(ggplot2)
library(e1071)
data <- split_data()
data_test <- data$data_test
data_train <- data$data_train
nb.grades_indep <- nb(class = "QP", dataset = data_train)
nb.grades_indep <- lp(nb.grades_indep, data_train, smooth = 0)
p_indep <- predict(nb.grades_indep, data_test)


# Add the predictions to the data_test data frame
data_test$prediction <- p_indep
# Plot the model's predictions for the test data
ggplot(data_test, aes(x = prediction, fill = prediction)) + 
  geom_bar(alpha = 0.5) + 
  ggtitle("Naive Bayes Model Predictions") + 
  xlab("Predicted Class") + 
  ylab("Count")

  for (i in 1:20){
  data <- split_data()
  data_test <- data$data_test
  data_train <- data$data_train
  nb.grades_indep <- nb(class = "QP", dataset = data_train)
  nb.grades_indep <- lp(nb.grades_indep, data_train, smooth = 0)
  p_indep <- predict(nb.grades_indep, data_test)
  print(accuracy(p_indep, data_test$QP))
}

# 5.Repeat 4, considering that the grades earned in different courses may be dependent.


nb.grades_dep <- tan_cl("QP", data_train)
nb.grades_dep <- lp(nb.grades_dep, data_train, smooth = 1)
p_dep <- predict(nb.grades_dep, data_test)
data_test$prediction <- p_dep
ggplot(data_test, aes(x = prediction, fill = prediction)) + 
  geom_bar(alpha = 0.5) + 
  ggtitle("Naive Bayes Model Predictions") + 
  xlab("Predicted Class") + 
  ylab("Count")
  # Repeating this 20 times

for (i in 1:20){
  data <- split_data()
  data_test <- data$data_test
  data_train <- data$data_train
  nb.grades_dep <- tan_cl("QP", data_train)
  nb.grades_dep <- lp(nb.grades_dep, data_train, smooth = 1)
  p_dep <- predict(nb.grades_dep, data_test)
  print(accuracy(p_dep, data_test$QP))
}

