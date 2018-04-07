####     Classification method error rates: Smarket dataset     ####

## LR: 44.04%
## LDA: 44.04%
## QDA: 40.07%
## KNN: 46.82%

################################################################
library(MASS)
library(ISLR)

attach(Smarket)

# split dataset into training and testing dataset

train = (Year < 2005)
test = !train

training_data = Smarket[train,]
testing_data = Smarket[test,]
direction_test = Direction[test]

## Logistic regression
logistic_model = glm(Direction ~ Lag1 + Lag2, data = training_data, family = binomial)
logistic_probs = predict(logistic_model, testing_data, type = "response")
logistic_pred = rep("Down", 252)
table(logistic_pred, direction_test)
mean(logistic_pred != direction_test)

## LDA
#fit model using training dataset
lda_model = lda(Direction ~ Lag1 + Lag2, data = training_data)

#validate model using test datset
lda_pred = predict(lda_model, testing_data)
names(lda_pred)
lda_pred_direction = lda_pred$class

# confusion matrix
table(lda_pred_direction, direction_test)

# misclassification error
mean(lda_pred_direction != direction_test)


## QDA
qda_model = qda(Direction ~ Lag1 + Lag2,  data = training_data)

# validtae model using test data
qda_pred = predict(qda_model, testing_data)
qda_pred_direction = qda_pred$class

# confusion matrix
table(qda_pred_direction, direction_test)

#misclassification error
mean(qda_pred_direction != direction_test)


## KNN

library(class)

# here we need to standardise our data. for ex in case of age and income data
# For KNN income difference of 500 much more than age difference of 70
# so income will drive the classidficaation results. And age will have almost no effect.

# since we are using only Lag1 and Lag2 standardise only those two.
std_data = scale(Smarket[, c(2, 3)])

trainig_data = std_data[train,]
testing_data = std_data[test,]
training_direction = Direction[train]
set.seed(1)
knn_pred_direction = knn(trainig_data, testing_data, training_direction, 3)

table(knn_pred_direction, direction_test)
mean(knn_pred_direction != direction_test)