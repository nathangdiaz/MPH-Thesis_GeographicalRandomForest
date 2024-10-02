###### attempting leave one out cross validation on traditional random forest model 

### preparation 
library(tidyverse)
library(caret)
library(randomForest)
library(sf)

set.seed(926) 

svi_df = read_csv(here::here("01_Data", "svi_df.csv")) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-...1)

tracts = tracts(state = "RI", year = 2022, cb = TRUE)

svi_df = inner_join(tracts, svi_df, by = c("GEOID" = "fips"))

df = svi_df %>% st_drop_geometry() %>% 
  select(rpl_themes, starts_with("e_"))

# create custom 
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Training Model 1 
mod1 = train(
  rpl_themes ~., 
  data = df, 
  method = customRF, 
  trControl = trainControl(method = "LOOCV"), 
  tuneGrid = expand.grid(.mtry = c(3:8), 
                         .ntree = c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)))

plot(mod1)

# Training Model 1 
mod2 = train(
  rpl_themes ~., 
  data = df, 
  method = customRF, 
  trControl = trainControl(method = "cv", number = 10), 
  tuneGrid = expand.grid(.mtry = c(3:8), 
                         .ntree = c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)))

plot(mod1)
# view final model
mod2$finalModel
# view best tuning parameters 
mod2$bestTune

#######################################
# Appendix A: Additional Codes

## Cross Validation of Performance Metrics

I am attempting to create code that preformed cross-validation when calculating performance metrics. 

```{r}
# Train the model
model1 = train(rpl_themes ~ ., data = train_data, 
               method = "rf",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(mtry = 5),
               ntree = 500)  # Set ntree directly

model2 = train(rpl_themes ~ ., data = train_data, 
               method = "rf",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(mtry = 4),
               ntree = 550)

model3 = train(rpl_themes ~ ., data = train_data, 
               method = "rf",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(mtry = 4),
               ntree = 450)

model4 = train(rpl_themes ~ ., data = train_data, 
               method = "rf",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(mtry = 9),
               ntree = 501)

# Create a data frame with the results
results_df = data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  mtry = c(5,4,4,9),
  ntree = c(500,550,450,501),
  MAE = c(model1$results$MAE, model2$results$MAE, model3$results$MAE, model4$results$MAE),
  RMSE = c(model1$results$RMSE, model2$results$RMSE, model3$results$RMSE, model4$results$RMSE),
  R_Squared = c(model1$results$Rsquared, model2$results$Rsquared, model3$results$Rsquared, model4$results$Rsquared)
)

# Print the results using kable
kable(results_df, caption = "Performance Metrics for Each Model", 
      digits = 3, align = c("l", "c", "c", "c", "c", "c", "c"))
```
