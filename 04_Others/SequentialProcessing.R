### RF Model 2 - Sequential Processing With RMSE Metric

**Background**: This model training process uses a combination of sequential processing and cross-validation. First, tuning the `mtry` parameter by using cross-validation to find the best value for each iteration. The model runs 10 times (i.e., the for loop) because given the nature of the building random forest models, the value of m within the loop changes. Therefore, preforming the function 10 times and taking the average of the most optimal mtry value it calculates and prints the average of the best *mtry* values. During the second step, the *ntree* is changing and cross-validated while *mtry* is held constant.

####################
##### RF Mod 2 #####
####################

### setting seed
set.seed(926)

### Step 1: Find the best `mtry` value
# Create an empty list to store the results
results_list = vector("list", 10)
# Loop to repeat the code 10 times
for (i in 1:10) {
  # Train the random forest model with 10-fold cross-validation
  rf_mod2 = train(rpl_themes ~ ., data = df, method = "rf", 
                  ntree = 500, # Start with a default number of trees
                  trControl = trainControl(method = "cv", number = 10),
                  tuneGrid = expand.grid(mtry = c(3:8)))
  
  # print model results 
  # print(rf_mod2)
  plot(rf_mod2)
  
  # Extract the best number of predictors (mtry) from the model
  m = rf_mod2$bestTune$mtry
  
  # Store the result in the list
  results_list[[i]] = m
}

mean_mtry = round(mean(unlist(results_list)))

# Step 2: Find the best `ntree` value using cross-validation with the optimal `mtry`
store_maxtrees = list()
ntree_values = c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000) # List of `ntree` values to test

for (ntree in ntree_values) {
  rf_maxtrees = train(rpl_themes ~ ., data = df, method = "rf",
                      tuneGrid = expand.grid(mtry = mean_mtry), # Use the fixed best `mtry`
                      trControl = trainControl(method = "cv", number = 10),
                      ntree = ntree)
  # print model results 
  # print(rf_maxtrees)
  
  store_maxtrees[[as.character(ntree)]] <- rf_maxtrees
}

# 500 subjectively made 
# summary(resamples(store_maxtrees))

# creating the first model
rf_mod2 = train(rpl_themes ~., 
                data = df, 
                method = "rf", 
                trControl = trainControl(method = "cv", number = 10), 
                tuneGrid = expand.grid(mtry = mean_mtry), 
                ntree = 300, 
                importance = TRUE)

# Print the results
rf_mod2$finalModel

The following graph illustrates preformance metrics across different values of *ntree*, and the selection for the final *ntree* value stems from having the lowest RMSE/MAE and the highest R-Squared Value.

# Initialize an empty data frame to store all metrics
metrics_df <- data.frame()

# Loop through each model in store_maxtrees
for (ntree in names(store_maxtrees)) {
  model <- store_maxtrees[[ntree]]
  
  # Extract metrics for the current model
  model_metrics <- data.frame(
    ntree = as.numeric(ntree),                  # Capture the ntree value
    RMSE = model$results$RMSE,                  # Extract RMSE
    MAE = model$results$MAE,                    # Extract MAE
    Rsquared = model$results$Rsquared,          # Extract Rsquared
    mtry = model$bestTune$mtry                  # Capture the mtry value
  )
  
  # Append the metrics to the main data frame
  metrics_df <- rbind(metrics_df, model_metrics)
}


# Create the line graph
a=ggplot(metrics_df, aes(x = ntree, y = RMSE)) +
  geom_point(size = 2) +
  labs(title = "RMSE Across Different ntree",
       x = "Number of Trees (ntree)",
       y = "Root Mean Squared Error",
       shape = "ntree") +
  theme_bw()

b=ggplot(metrics_df, aes(x = ntree, y = MAE)) +
  geom_point(size = 2) +
  labs(title = "MAE Across Different ntree",
       x = "Number of Trees (ntree)",
       y = "Mean Absolute Error",
       shape = "ntree") +
  theme_bw()

c=ggplot(metrics_df, aes(x = ntree, y = Rsquared)) +
  geom_point(size = 2) +
  labs(title = "Rsquared Across Different ntree",
       x = "Number of Trees",
       y = "R-squared",
       shape = "ntree") +
  theme_bw()


blank = ggplot() + theme_void()

fig = ggarrange(
  ggarrange(a, b, blank, nrow = 1,
            widths = c(1, 1, 0), legend = "none"),
  ggarrange(blank, c, blank, nrow = 1,
            widths = c(0.5, 1, 0.5), legend = "none"),
  nrow = 2,
  legend = "none"
)
annotate_figure(fig, bottom = text_grob("mtry = 6", hjust = 1, x = 1))

The second model hyperparameters have been set to *mtry* = `r rf_mod2$bestTune`, and *ntrees* = `r rf_mod2$finalModel$ntree`. 