library(dplyr)
library(pROC)
loan_data <- readRDS("./data/loan_data_ch1.rds")

## voy a cortar los outliers
#outlier_cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5*IQR(loan_data$annual_inc) 

#loan_data <- loan_data %>%
#  filter(annual_inc <= outlier_cutoff)

## voy a crear la columna empcat

my_func <- function(x) {
  if (is.na(x)) {
    return("Missing")
  } else if(0<=x & x<15) {
    return("0-15")
  } else if(15<=x & x<30) {
    return("15-30")
  } else if(30<=x & x<45) {
    return("30-45")
  } else {
    return("45+")
  }
  
}

temp <- loan_data$emp_length
#print(temp)
loan_data$emp_length <- NULL
print(loan_data$emp_lenght)

loan_data$emp_cat <- sapply(temp, my_func)
loan_data$emp_cat <- as.factor(loan_data$emp_cat)


## voy a crear la variable ir_cat
# Make the necessary replacements in the coarse classification example below 
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))

loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"

loan_data$ir_cat <- as.factor(loan_data$ir_cat)

#summary(loan_data$emp_cat)

loan_data$int_rate <- NULL

# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data))

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set <- loan_data[-index_train, ]

# Build a glm model with variable ir_cat as a predictor
log_model_cat <- glm(loan_status ~ ir_cat, family = "binomial", data = training_set)


# Print the parameter estimates 
##log_model_cat

### Creación del modelo completo
# Change the code below to construct a logistic regression model using all available predictors in the data set

log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
#log_model_full$xlevels$ir_cat[5] <- "Missing"
#summary(log_model_full)

# Change the code below to construct a logistic regression model using all available predictors in the data set

log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
#log_model_full$xlevels$ir_cat[5] <- "Missing"
##summary(log_model_full)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")
# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full > 0.15, 1,0)

# Construct a confusion matrix
##table(test_set$loan_status, pred_cutoff_15)

#summary(test_set)
true_val <- test_set$loan_status

# Fit the logit, probit and cloglog-link logistic regression models
#print(length(training_set))

log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                        family = binomial(link = probit), data = training_set)

log_model_cloglog <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                         family = binomial(link = cloglog), data = training_set)

# Make predictions for all models using the test set

#print(length(test_set))

predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")

#print(length(predictions_logit))

# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

#print(length(class_pred_logit))

# Make a confusion matrix for the three models
tab_class_logit <- table(true_val,class_pred_logit)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)

# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)

### The strategy table and strategy curve
# Have a look at the function strategy_bank
strategy_bank <- function(prob_of_def){
  cutoff=rep(NA, 21)
  bad_rate=rep(NA, 21)
  accept_rate=seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good=test_set$loan_status[pred_i==0]
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
  return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}

# Apply the function strategy_bank to both predictions_cloglog and predictions_loss_matrix
strategy_cloglog <- strategy_bank(predictions_cloglog)
#predictions_loss_matrix <- as.numeric(pred_loss_matrix)
#strategy_loss_matrix <- strategy_bank(predictions_loss_matrix)

# Obtain the strategy tables for both prediction-vectors
##strategy_cloglog$table


# Plot the strategy functions
##par(mfrow = c(1,1))
##plot(strategy_cloglog$accept_rate, strategy_cloglog$bad_rate, 
  ##   type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
    ## lwd = 2, main = "logistic regression")

# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$loan_status, predictions_logit)
ROC_probit <- roc(test_set$loan_status, predictions_probit)
ROC_cloglog <- roc(test_set$loan_status, predictions_cloglog)
ROC_all_full <- roc(test_set$loan_status, predictions_all_full) 

# Draw all ROCs on one plot
##plot(ROC_logit)
##lines(ROC_probit, col="blue")
##lines(ROC_cloglog, col="red")
##lines(ROC_all_full, col="green")

# Construct the objects containing ROC-information
##ROC_undersample <- roc(test_set$loan_status, predictions_undersample)

##predictions_prior <- as.numeric(pred_prior)
##predictions_weights <- as.numeric(pred_weights)

#ROC_prior <- roc(test_set$loan_status, predictions_prior)
ROC_loss_matrix <- roc(test_set$loan_status, predictions_loss_matrix)  
ROC_weights <- roc(test_set$loan_status, predictions_weights)

# Draw the ROC-curves in one plot
##plot(ROC_undersample)
###plot(ROC_prior, col="blue")  
###lines(ROC_loss_matrix, col="red")
###lines(ROC_weights, col="green")    

# Compute the AUCs
##auc(ROC_undersample)
#(auc(ROC_prior))
#(auc(ROC_loss_matrix))
(auc(ROC_weights))