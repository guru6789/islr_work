#ISLR package has the data
library(ISLR)
attach(Smarket)
#This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
#beginning of 2001 until the end of 2005

#column names of Smarket data
names(Smarket)
#"Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"      "Lag5"      "Volume"   
#"Today"     "Direction"


#corelation between lag variables and today's returns are close to zero as expected.The only relationship
#is between year and volume which increasing over time.
cor(Smarket[, -9])

#split datset into testing data and training dataset.
training = (Year < 2005)
testing = !training

training_data = Smarket[training, ]
testing_data = Smarket[testing, ]

direction_testing = Direction[testing]

#fitting logistic regression model to predict stock direction using Lag and volume.
#glm function fits generalised linear models. Agument family = binomial tells R to run logistic regrssion
#rahther than some ype pf generalised linear model.
stock_model = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = training_data, family = binomial)

#p values for all Lag variables are high which indicates there is no strong relationship.
summary(stock_model)

#Use fitted model to predict
#The type="response" option tells R to output probabilities of the form P(Y = 1|X)
#ths just predict the probablities and doesn't predict class wheather its up or down.
model_pred_probs = predict(stock_model, testing_data, type = "response")

#create a vector wth same length as testing data i.e 252 with all elements as Down.
#then use probablities above to change the value to Up.
model_pred_direction = rep("Down", 252)
model_pred_direction[model_pred_probs > 0.5] = "Up" 

#Confusion matrix to check accuracy of model
table(model_pred_direction, direction_testing)

#                     direction_testing
#model_pred_direction Down Up
#                Down   77 97
#                Up     34 44
#Diagonal values - no of times prediction was correct
#Off-diagonal values - no of times prediction was wrong.

#to compute misclassification error
# 0.5198413 which is very high error. coz variabes are not good enough for our model.
mean(model_pred_direction != direction_testing)