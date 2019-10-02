# install.packages("caret")
library(caret)
library(dplyr)

setwd("C:/Users/admin/Desktop/R/Coursera - Predicting Student Loan Repayment/Dataset")

schoolData <- read.csv("ImpT&TCombined.csv")

# backup -- will use in the end
backUp <- schoolData$INSTNM[is.na(schoolData$COMPL_RPY_3YR_RT)]

# Let's remove the INSTNM feature right here
schoolData <- schoolData[, c(2:15)]

str(schoolData)
summary(schoolData)

# splitting into train and test set
train <- schoolData %>% filter(!is.na(COMPL_RPY_3YR_RT))
test <- schoolData %>% filter(is.na(COMPL_RPY_3YR_RT))


# Now creating data partitions for training set and validation set
# We make a 75-25 split.
set.seed(162)
forTraining <- createDataPartition(train$COMPL_RPY_3YR_RT, p = 0.75, list = FALSE)

training <- train[forTraining, ]
validation <- train[-forTraining, ]

# Now, build a model using caret with trainControl having Cross Validation

# cross-validation
control <- trainControl(method = "cv", number = 10)     # can be "method = oob" too, for randomforest, cforest


# now train a model -- maybe first I should exclude INSTNM from list of predictors
# First use Linear Model (Linear Regression)

# predList <- names(training[, c(2:15)])
modelFit <- train(COMPL_RPY_3YR_RT ~ ., data = training, method = "lm", metric = "RMSE", trControl = control)


# check the performance on the training set, how well my model fits the training set using cross-validation
print(modelFit)
# RMSE = 0.1032599
# Rsquared = 0.7418828

# now checking variable importance. Can be useful for feature selection
varImp(modelFit)
importance <- varImp(modelFit, scale = FALSE)
print(importance)
plot(importance, top = 22)    # remove top to view all
# As we can see, many features were very important, while some features were very less important, eg, GRAD_DEBT_MDN
# and ATDCOST. Surprisingly, ATDCOST is somewhere on the least important side.
# On the other hand, CDR3 (Cohort Default Rates) has a critical impact on the predictions. 
# Also Median Earnings (MD_EARN_WNE_P8) of the individuals after 8 years of enrollment
# is very important. Age entry, median household income, etc are some other important features.


# Let's make predictions for the validation set, which is our hold out set from the training set
valPredictions <- predict(modelFit, validation)
head(valPredictions)
summary(valPredictions)

# Apparantly, the value predicted seems to go higher than 1. Let's bring all such values to 1.
# BTW, I don't have to do this because this is what my model has predicted, and this should be reflected in the RMSE score
# as well!
for(i in 1:length(valPredictions)) {
  if(valPredictions[i] > 1) {
    valPredictions[i] = 1
  }
}
summary(valPredictions)
# since repayment rate is a fraction, it should be atmost 1.

# Now will compute RMSE on the hold out from the training set using the custom function webclipped
# BTW, Caret has an inbuilt RMSE function. Can use that instead. 
# Just for the record, there is a function rmse() in "Metrics" package which is created by Ben Hamner
# Co-Founder of Kaggle

RMSE(validation$COMPL_RPY_3YR_RT, valPredictions)
# RMSE = 0.09805046; way below the threshold!!!!
# after some refinement, RMSE = 0.09797155

# Let's verify the above using the custom function

RMSE_Cust <- function(m, o) {
  sqrt(mean((m - o)^2))        # m = model fitted values; o = observed values
}

RMSE_Cust(valPredictions, validation$COMPL_RPY_3YR_RT)
# again 0.09805046


# finally, we make predictions on our test set. We don't have any way to verify that though.
Predictions <- predict(modelFit, test)

# finally, write predictions into a CSV file
predictionsDf <- data.frame(INSTNM = backUp, COMPL_RPY_3YR_RT = Predictions)

# I would correct the predictions for this too
summary(predictionsDf$COMPL_RPY_3YR_RT)
predictionsDf$COMPL_RPY_3YR_RT[predictionsDf$COMPL_RPY_3YR_RT > 1] <- 1
summary(predictionsDf$COMPL_RPY_3YR_RT)

# include the INSTNM backup into the test CSV too
test$INSTNM <- backUp
test <- test[, c(15, 1:14)]

write.csv(test, file = "test.csv", row.names = FALSE)
write.csv(predictionsDf, file = "TestPredictions.csv", row.names = FALSE)

# COMPLETED >>> 10/10/2017 3.52pm (at office) ||| Total 3 R scripts
# UPDATE... COMPLETED >>> Same day at 11.39pm (at room) (needed to correct the predictions going > 1)
# COURSERA >> Challenge: Predict Students' Ability to Repay Educational Loans
# Accuracy achieved very well
# This challenge was provided by Michael Boerrigter and presented by Claire Smith
# Started by me on 25/09/2017


