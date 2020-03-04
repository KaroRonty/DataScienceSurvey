library(caret)
library(doParallel)

# Shuffle the data and drop factor levels
set.seed(2)
split <- regression %>%
  filter(ConvertedComp < 1e6) %>%  
  sample_frac() %>% 
  droplevels()

# Split into training and test data 70/30
training <- split %>% 
  slice(1:(nrow(split) * 0.7))
test <- split %>% 
  slice((nrow(split) * 0.7 + 1):nrow(split))

# Make model matrices for both sets for modeling
training_mm <- cbind(ConvertedComp = training$ConvertedComp,
                     model.matrix(ConvertedComp ~ .,
                                  data = training)) %>% 
  as.data.frame()
test_mm <- cbind(ConvertedComp = test$ConvertedComp,
                 model.matrix(ConvertedComp ~ .,
                              data = test)) %>% 
  as.data.frame()

# Use all available cores
registerDoParallel(cores = detectCores())

# Perform 10-fold cross validation
cv <- trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 3,
                   allowParallel = TRUE)

# Train the models
time <- Sys.time()

linear <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
                training_mm %>% pull(ConvertedComp),
                method = "lm",
                trControl = cv)

glmnet <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
                training_mm %>% pull(ConvertedComp),
                method = "glmnet",
                trControl = cv)

xgboost <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
                 training_mm %>% pull(ConvertedComp),
                 method = "xgbTree",
                 trControl = cv)

knn <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
             training_mm %>% pull(ConvertedComp),
             method = "knn",
             trControl = cv)

mars <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
              training_mm %>% pull(ConvertedComp),
              method = "earth",
              trControl = cv)

svm <- train(training_mm %>% select(-ConvertedComp) %>% as.matrix(),
             training_mm %>% pull(ConvertedComp),
             method = "svmLinear",
             trControl = cv)
(time <- Sys.time() - time)

# Functions for calculating accuracy metrics
calculate_rsq <- function(model){
  invisible(capture.output(pred <- predict(get(model), newdata = test_mm)))
  return(cor(test$ConvertedComp, pred, use = "pairwise.complete.obs")^2)
}

calculate_mape <- function(model){
  invisible(capture.output(pred <- predict(get(model), newdata = test_mm)))
  return(mean(abs(test$ConvertedComp - pred)) / mean(test$ConvertedComp))
}

# Calculate accuracy metrics for all models
models <- c("linear", "glmnet", "xgboost", "knn", "mars", "svm")

suppressWarnings(sapply(models, calculate_rsq))
suppressWarnings(sapply(models, calculate_mape))

# Calculate MAE of constant prediction
mean(abs(test$ConvertedComp - mean(training$ConvertedComp))) /
  mean(test$ConvertedComp)