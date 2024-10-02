
########## LOADING PACKAGES #####################

# Install and load necessary packages
# install.packages(c("sp", "sf", "ranger", "caret", "dplyr", "cluster"))

# Load the libraries
library(sp)        # Spatial data handling
library(sf)        # Simple feature data handling
library(ranger)    # Random Forest implementation
library(caret)     # Cross-validation and model tuning
library(dplyr)     # Data manipulation
library(cluster)   # Clustering methods
library(readr)     # Read a csv file 
library(tigris)    # obtain shp files 

########## IMPORTING DATA #####################

# setting seed
set.seed(926) 
# Load and prepare the spatial data
svi_df = read_csv(here::here("01_Data", "svi_df.csv")) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-...1) # if needed
# obtaining SPH files for RI tracts
tracts = tracts(state = "RI", year = 2022, cb = TRUE)
# joining data 
map = inner_join(tracts, svi_df, by = c("GEOID" = "fips")) %>% 
  mutate(fips = GEOID) %>% 
  select(fips, rpl_themes, starts_with("e_"))

########## Defining Functions #####################

# three functions are defined: create_folds_hclust(), customRF(), 

# (1) Define create_folds_hclust a function for hierarchical clustering-based folds
# INPUT:
# - data: A spatial dataframe containing the spatial data to be clustered.
# - k: The number of folds (clusters) to create.
# OUTPUT:
# - fold_indices: A list where each element contains the indices of the rows in `data`
#   that belong to a specific fold or cluster.

create_folds_hclust <- function(data, k) {
  # Convert spatial data to centroid coordinates for clustering
  # st_centroid(data) calculates the centroid of each spatial object in `data`
  # st_coordinates() extracts the x and y coordinates of the centroids
  coords <- st_coordinates(st_centroid(data))
  
  # Perform hierarchical clustering on the centroid coordinates
  # dist(coords) computes the distance matrix between the centroids
  # hclust() performs hierarchical clustering using the Ward's minimum variance method (ward.D2)
  hc <- hclust(dist(coords), method = "ward.D2")
  
  # Cut the hierarchical clustering tree into `k` clusters
  # cutree() assigns each observation (centroid) to one of the `k` clusters
  clusters <- cutree(hc, k = k)
  
  # Create a list of fold indices based on the clusters
  # lapply(1:k, ...) iterates over each cluster from 1 to `k`
  # which(clusters == x) identifies the row indices in `data` that belong to cluster `x`
  fold_indices <- lapply(1:k, function(x) which(clusters == x))
  
  # Return the list of fold indices
  return(fold_indices)
}

# (2) Define customRF(): a custom Random Forest model for exhaustive grid search with caret

# Input:
# - x: Predictor variables (features) for training.
# - y: Response variable (target) for training.
# - param: A list containing the hyperparameters 'mtry' and 'ntree' to be tuned.
# - len: Length of the hyperparameter grid. (Not used in this custom model but typically defined in caret models.)
# - search: Type of search to perform; 'grid' indicates exhaustive grid search.
# - modelFit: The trained Random Forest model.
# - newdata: New data for making predictions.
# - wts, lev, last, weights, classProbs, preProc, submodels: Arguments typically passed by caret during model training and prediction (not directly used here).

# Output:
# - A list defining the custom Random Forest model, including functions for training, predicting, and handling hyperparameters.

customRF <- list(
  type = "Regression",  # Define the model type as regression (since Random Forest can be used for both classification and regression)
  library = "randomForest",  # Specify the package required for the model
  loop = NULL  # Not used in this example, but caret uses it for looping through models with different parameter values
)

# Define the hyperparameters to tune (mtry and ntree) and their data types
customRF$parameters <- data.frame(
  parameter = c("mtry", "ntree"),  # Hyperparameters to be tuned
  class = rep("numeric", 2),  # Data types for the hyperparameters
  label = c("mtry", "ntree")  # Labels for the hyperparameters
)

# Grid function for hyperparameter tuning (left empty as we are not customizing the grid search process itself)
customRF$grid <- function(x, y, len = NULL, search = "grid") {}

# Define the model fitting function using the Random Forest algorithm
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...)  # Train the model with specified mtry and ntree
}

# Define the prediction function for the model
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata)  # Make predictions using the trained model
}

# Define a function for returning predicted probabilities (only applicable if the model supports it)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata, type = "prob")  # Predict probabilities (useful in classification tasks)
}

# Define a function to sort the grid results based on mtry (used by caret)
customRF$sort <- function(x) x[order(x[, 1]),]

# Define a function to extract the levels (classes) in the model (only relevant for classification)
customRF$levels <- function(x) x$classes


# (2) Define customRF(): a custom Random Forest model for exhaustive grid search with caret

# Input:

# Define the function for nested cross-validation with hierarchical clustering-based folds
nested_cv_with_hclust <- function(data, 
                                  response_var,
                                  tuning_grid = expand.grid(mtry = c(3:8), 
                                                            ntree = c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)),
                                  outer_k = 5,
                                  inner_k = 5,
                                  metric = "MSE") {  # Default to "MSE" for regression tasks
  
  # Summary: Validates and ensures the correct metric is specified for evaluation.
  
  # Validate the metric argument
  if (!metric %in% c("MSE", "OOBMSE")) {
    stop("Invalid metric specified. Choose 'MSE' or 'OOBMSE'.")
  }
  
  # Summary: Creates outer folds for cross-validation based on hierarchical clustering.
  
  # Create outer folds
  outer_folds <- create_folds_hclust(data, k = outer_k)
  
  # Summary: Performs nested cross-validation by splitting the data into outer and inner folds, tuning hyperparameters, and evaluating model performance.
  
  # Perform nested cross-validation
  outer_results <- lapply(1:length(outer_folds), function(i) {
    # Access the indices for the current outer fold
    fold_indices <- outer_folds[[i]]
    
    # Split the data into training and testing based on the outer fold
    training_data <- data[-fold_indices, ]  # Exclude the current outer fold for training
    testing_data <- data[fold_indices, ]    # Use the current outer fold for testing
    
    # Summary: Creates inner folds for hyperparameter tuning based on hierarchical clustering within the training data.
    
    # Create inner folds
    inner_folds <- create_folds_hclust(training_data, k = inner_k)  # Define clusters within the training data
    
    # Summary: Performs hyperparameter tuning using inner cross-validation and evaluates model performance for each inner fold.
    
    # Perform inner cross-validation to tune hyperparameters
    inner_results <- lapply(1:inner_k, function(j) {
      # Access the indices for the current inner fold
      inner_fold_indices <- inner_folds[[j]]
      
      # Split the training data into inner training and inner validation sets
      inner_training_data <- training_data[-inner_fold_indices, ]
      inner_validation_data <- training_data[inner_fold_indices, ]
      
      # Train the model on the inner training data using customRF
      model <- train(
        as.formula(paste(response_var, "~ .")),  # Use the response variable specified
        data = inner_training_data,
        method = customRF,  # Use customRF for training
        trControl = trainControl(method = "none"),  # No internal resampling
        tuneGrid = tuning_grid  # Hyperparameter grid
      )
      
      # Summary: Evaluates the model on the inner validation data and calculates performance metrics based on the response variable type.
      
      # Evaluate the model on the inner validation data
      predictions <- predict(model, newdata = inner_validation_data)
      true_values <- inner_validation_data[[response_var]]
      
      if (is.factor(true_values)) {
        # Classification tasks
        accuracy <- mean(predictions == true_values)  # Accuracy for classification
      } else {
        # Regression tasks
        if (metric == "MSE") {
          # Mean Squared Error
          accuracy <- mean((predictions - true_values)^2)  # MSE for regression
        } else if (metric == "OOBMSE") {
          # Out-of-Bag Mean Squared Error
          oob_mse <- model$finalModel$mse[model$bestTune$ntree]  # Extract OOB MSE for best ntree
          accuracy <- oob_mse
        } else {
          stop("Invalid metric specified. Choose 'MSE' or 'OOBMSE'.")
        }
      }
      
      # Store the results for this inner fold
      list(
        fold = j,
        model = model,
        predictions = predictions,
        accuracy = accuracy
      )
    })
    
    # Summary: Selects the best model from inner cross-validation and evaluates its performance on the outer test set.
    
    # Select the best model from inner cross-validation
    best_inner_model <- inner_results[[which.min(sapply(inner_results, function(res) res$accuracy))]]$model
    
    # Evaluate the best inner model on the outer testing data
    outer_predictions <- predict(best_inner_model, newdata = testing_data)
    outer_true_values <- testing_data[[response_var]]
    
    if (is.factor(outer_true_values)) {
      # Classification tasks
      outer_accuracy <- mean(outer_predictions == outer_true_values)  # Accuracy for classification
    } else {
      # Regression tasks
      if (metric == "MSE") {
        outer_accuracy <- mean((outer_predictions - outer_true_values)^2)  # MSE for regression
      } else if (metric == "OOBMSE") {
        outer_accuracy <- best_inner_model$finalModel$mse[best_inner_model$bestTune$ntree]  # Extract OOB MSE
      }
    }
    
    # Store the results for this outer fold
    list(
      fold = i,
      best_inner_model = best_inner_model,
      outer_predictions = outer_predictions,
      outer_accuracy = outer_accuracy
    )
  })
  
  # Summary: Returns the results of the nested cross-validation process including models and performance metrics for each outer fold.
  
  return(outer_results)
}



########## Creating a Random Forest Model #####################


# Set number of folds for outer cross-validation
outer_k <- 5
outer_folds <- create_folds_hclust(map, k = outer_k) # produces a list of the indices


# Initialize a vector to store fold numbers for each observation
map$outer_cluster_id <- NA

# Assign fold numbers to each observation based on outer_folds
for (i in seq_along(outer_folds)) { map$outer_cluster_id[outer_folds[[i]]] = i }
    # Outer_folds[[i]] accesses the ith element of the outer_folds list. 
    # Each element in outer_folds is a vector of indices that belong to a specific fold or cluster.
    # For all the indices listed in outer_folds[[i]], the corresponding entries in map$outer_cluster_id are set to i.


# Outer Loop: Hierarchical Clustering-based Cross-Validation
outer_results <- lapply(1:length(outer_folds), function(i) {
  
  # Split data into training and testing sets based on outer clusters
  training_data = map %>% filter(outer_cluster_id == i)
  testing_data = map %>% filter(outer_cluster_id != i)
  
  # Inner Loop: Hierarchical Clustering-based Cross-Validation for Hyperparameter Tuning
  inner_k = 5 # define the number of folds 
  inner_folds = create_folds_hclust(training_data, k = inner_k) # define clusters within the initial clusters 
  training_data$inner_cluster_id <- NA # initiating a column 
  # assigning the previously define indices into the newly define column
  for (i in seq_along(inner_folds)) {training_data$outer_cluster_id[inner_folds[[i]]] <- i }
  
  # Set up train control with clustering-based inner folds
  train_control = trainControl(
    method = "cv",                        # Cross-validation method
    index = inner_folds,                  # Clustering-based inner folds
  )
  
  # Define a grid of hyperparameters to tune
  tune_grid <- expand.grid(
    mtry = c(3,4,5,6,7),                  # Number of variables randomly sampled at each split
    ntrees = c(350, 400, 450, 500, 550, 600, 650)  # Number of trees in the forest
  )
  
  # Train the Random Forest model using clustering-based inner cross-validation
  rf_model <- train(
    rpl_themes ~ ,                       # Model formula with 'rpl_themes' as target variable
    data = training_data,                 # Training data
    method = "rf",                        # Random Forest method
    trControl = train_control,            # Clustering-based cross-validation control
    tuneGrid = tune_grid                  # Hyperparameter grid
  )
  
  # Evaluate the model on the outer fold's test set
  predictions <- predict(rf_model, newdata = testing_data)
  mse <- mean((predictions - testing_data$rpl_themes)^2)  # Mean Squared Error
  
  # Return the model and MSE for this fold
  list(model = rf_model, mse = mse)
})

# Aggregate results from the outer loop
overall_mse <- mean(sapply(outer_results, function(x) x$mse))
print(overall_mse)


###########################################################################################

```{r}
# Function to calculate spatial autocorrelation range
calculate_autocor_range <- function(data, spatial_col) {
  autocor_range <- blockCV::cv_spatial_autocor(data[[spatial_col]])$range
  return(autocor_range)
}

# Function to create outer folds
create_outer_folds <- function(data, outer_k, autocor_range, spatial_col) {
  outer_folds <- blockCV::cv_spatial(data, k = outer_k, range = autocor_range, spatial_col = spatial_col)
  return(outer_folds)
}

# Function to create inner folds
create_inner_folds <- function(training_data, inner_k, autocor_range, spatial_col) {
  inner_folds <- blockCV::cv_spatial(training_data, k = inner_k, range = autocor_range, spatial_col = spatial_col)
  return(inner_folds)
}

# Function to train the model
train_model <- function(inner_training_data, response_var, tuning_grid, inner_k) {
  model <- caret::train(
    as.formula(paste(response_var, "~ .")),  # Formula using the response variable
    data = inner_training_data,
    method = customRF,  # Method for training (custom Random Forest)
    trControl = caret::trainControl(method = "cv", number = inner_k),  # Inner cross-validation
    tuneGrid = tuning_grid  # Grid of hyperparameters to tune
  )
  return(model)
}

# Function to evaluate the model
evaluate_model <- function(model, validation_data, metric, response_var) {
  predictions <- predict(model, newdata = validation_data)
  true_values <- validation_data[[response_var]]
  
  if (is.factor(true_values)) {
    # Classification: Calculate accuracy
    performance_metric <- mean(predictions == true_values)
  } else {
    # Regression: Calculate specified metric
    if (metric == "MSE") {
      performance_metric <- mean((predictions - true_values)^2)
    } else if (metric == "OOBMSE") {
      performance_metric <- model$finalModel$mse[model$bestTune$ntree]
    } else if (metric == "MAE") {
      performance_metric <- mean(abs(predictions - true_values))
    }
  }
  
  return(performance_metric)
}

# Main function for nested spatial cross-validation with hierarchical clustering
nested_spatial_cv_with_clustering <- function(data, 
                                              response_var,
                                              spatial_col = "geometry",  # Add spatial_col argument
                                              tuning_grid = expand.grid(mtry = c(3:8), 
                                                                        ntree = c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)),
                                              outer_k = 5,
                                              inner_k = 5,
                                              metric = "MSE") {
  
  # Calculate the spatial autocorrelation range
  autocor_range <- calculate_autocor_range(data, spatial_col)
  
  # Create outer folds
  outer_folds <- create_outer_folds(data, outer_k, autocor_range, spatial_col)
  
  # Initialize parallel processing to speed up computation
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  
  # Perform nested cross-validation
  outer_results <- foreach::foreach(i = seq_len(outer_k), .combine = 'c') %dopar% {
    # Access the indices for the current outer fold
    fold_indices <- outer_folds[[i]]
    
    # Split the data into training and testing based on the current outer fold
    training_data <- data[-fold_indices, ]
    testing_data <- data[fold_indices, ]
    
    # Create inner folds for hyperparameter tuning within the training data
    inner_folds <- create_inner_folds(training_data, inner_k, autocor_range, spatial_col)
    
    # Perform hyperparameter tuning using inner cross-validation
    inner_results <- lapply(seq_len(inner_k), function(j) {
      inner_fold_indices <- inner_folds[[j]]
      inner_training_data <- training_data[-inner_fold_indices, ]
      inner_validation_data <- training_data[inner_fold_indices, ]
      
      # Train the model on the inner training data
      model <- train_model(inner_training_data, response_var, tuning_grid, inner_k)
      
      # Evaluate the model on the inner validation data
      performance_metric <- evaluate_model(model, inner_validation_data, metric, response_var)
      
      return(list(
        fold = j,
        model = model,
        performance_metric = performance_metric
      ))
    })
    
    # Select the best model from the inner cross-validation results
    best_inner_model <- inner_results[[which.min(sapply(inner_results, function(res) res$performance_metric))]]$model
    
    # Evaluate the best inner model on the outer testing data
    outer_performance_metric <- evaluate_model(best_inner_model, testing_data, metric, response_var)
    
    return(list(
      fold = i,
      best_inner_model = best_inner_model,
      outer_performance_metric = outer_performance_metric
    ))
  }
  
  # Stop parallel processing and release resources
  parallel::stopCluster(cl)
  
  return(outer_results)
}
```
