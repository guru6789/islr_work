#### KNN an logistic applied to Caravan data set from ISLR library ####

library(ISLR)
library(class)
attach(Caravan)

# standardising all but last column which is qualitative
# all variables are given mean zero and std. deviation 1
std_data = scale(Caravan[, -86])

# divide training and testing data
test = 1:1000
training_data = std_data[-test,]
testing_data = std_data[test,]

# divide response variable also
training_purchase = Purchase[-test]
testing_purchase = Purchase[test]

# we need to set random variable before we run KNN coz if several observations
# are tied, KNN will randomly break the tie. therefore a seed must be set
# to ensure reproducability of results.
set.seed(1)

knn_pred = knn(training_data, testing_data, training_purchase, k = 5)

# error rate
mean(testing_purchase != knn_pred)

mean(testing_purchase != "No")

table(knn_pred, testing_purchase)

## Logistic regression

glm_fit = glm(Purchase ~ ., data = training_data, family = binomial)


