```{r}
# Define a function for spatial K-Fold Cross-Validation
spatial_kfold_cv <- function(data, formula, k = 5, ...) {
  # Create k equally sized folds
  set.seed(123)  # For reproducibility
  folds <- createFolds(data[[as.character(formula[[2]])]], k = k)
  
  # Print folds to check their content
  print(folds)
  
  # Initialize vectors to store results
  predictions <- numeric(nrow(data))
  actual <- data[[as.character(formula[[2]])]]
  
  # Loop through each fold
  for (i in 1:k) {
    # Separate the i-th fold as the validation set
    test_indices = folds[[i]]
    train_indices = setdiff(1:nrow(data), test_indices)
    temp_data = data[train_indices, ]
    
    # Check if test_indices is empty
    if (length(test_indices) == 0) stop("Test indices are empty for fold ", i)
    # Print test_indices and train_indices
    cat("\nFold", i, "\n")
    print("Test indices:")
    print(test_indices)
    print("Train indices:")
    print(train_indices)
    print("Training Data:")
    print(temp_data)
    
    
    # Fit the geographically weighted random forest model on the training data
    model <- grf(
      formula = formula,
      dframe =  temp_data,
      ...
    )
    
    # Make predictions for the i-th fold
    predictions[test_indices] <- predict.grf(
      model, 
      data[test_indices, ],
      x.var.name = "lon", 
      y.var.name = "lat", 
      local.w = 1, 
      global.w = 0
    )
  }
  
  # Calculate performance metrics
  mse <- mean((predictions - actual)^2)
  rmse <- sqrt(mse)
  r_squared <- 1 - (sum((predictions - actual)^2) / sum((mean(actual) - actual)^2))
  
  # Return results as a list
  return(list(predictions = predictions, mse = mse, rmse = rmse, r_squared = r_squared))
}

# Run spatial LOOCV on the geographically weighted random forest model
set.seed(123)  # Setting seed for reproducibility
results = spatial_kfold_cv(
  data = df,
  formula = rpl_themes ~ e_pov150 + e_unemp + e_hburd + e_nohsdp + 
    e_uninsur + e_age65 + e_age17 + e_disabl + 
    e_sngpnt + e_limeng + e_minrty + e_munit +
    e_mobile + e_crowd + e_noveh + e_groupq,
  k = 10,
  kernel = "adaptive",
  coords = df_coords,
  bw = best.bw_gwrf_mod1,
  ntree = 500,
  mtry = mtry,
  importance = "impurity")
```

```{r include = FALSE}
# this package only supports categorical response variables 

library(blockCV)
library(automap)

svi_map = svi_df %>% mutate(geometry = st_centroid(geometry))

# identifying the range of spatial autocorrelation in meters
range = blockCV::cv_spatial_autocor(
  x = svi_df, # species data
  column = "rpl_themes")

# this is used for k-fold cross validation
scv1 <- blockCV::cv_spatial(
  x = svi_map,
  column = "rpl_themes", # the response column (binary or multi-class)
  k = 5, # number of folds
  size = 30000, # size of the blocks in metres
  selection = "random", # random blocks-to-fold
  iteration = 50, # find evenly dispersed folds
  progress = FALSE, # turn off progress bar
  biomod2 = TRUE, # also create folds for biomod2
  raster_colors = terrain.colors(10, rev = TRUE) # options from cv_plot for a better colour contrast
) 
```

```{r}
# data does not allow for accessing the which observations belong to which cluster
library(spatialsample)

df = cbind(df, df_coords)
df = sf::st_as_sf(df, coords =c("lon", "lat"), crs = 26919)

cluster_folds = spatialsample::spatial_clustering_cv(data = df, cluster_function = c("kmeans"), v = 4)
autoplot(cluster_folds)

# `splits` will be the `rsplit` object
compute_preds <- function(splits) {
  # fit the model to the analysis set
  mod <- randomForest::randomForest(rpl_themes ~ e_pov150 + e_unemp + e_hburd + e_nohsdp + 
              e_uninsur + e_age65 + e_age17 + e_disabl + 
              e_sngpnt + e_limeng + e_minrty + e_munit +
              e_mobile + e_crowd + e_noveh + e_groupq,
            data = analysis(splits)
  )
  # identify the assessment set
  holdout <- assessment(splits)
  # return the assessment set, with true and predicted price
  tibble::tibble(
    geometry = holdout$geometry,
    Sale_Price = log10(holdout$Sale_Price),
    .pred = predict(mod, holdout)
  )
}

```
```{r}
library(mlr3)


# coordinates needed for spatial partitioning 
head(df_coords)
# select response and predictors to use in the modeling
head(df)
# create task
task = mlr::makeRegrTask(
  data = as.data.frame(df), 
  target = "rpl_themes",
  coordinates = df_coords
)

lrns = listLearners(task, warn.missing.packages = FALSE) %>%
  dplyr::select(class, name, short.name, package) 

filter(lrns, grepl("Random Forest", name)) %>% 
  dplyr::select(class, name, short.name, package)

lrn_rf = makeLearner("regr.ranger",
                     predict.type = "response")

lrn_rf

# performance estimation level
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)

# five spatially disjoint partitions
tune_level = makeResampleDesc("SpCV", iters = 5)

# use 16 randomly selected hyperparameters
ctrl = makeTuneControlRandom(maxit = 16)

# define the outer limits of the randomly selected hyperparameters
ps = makeParamSet(
  makeNumericParam("num.trees", lower = 400, upper = 600, trafo = function(x) 2^x),
  makeNumericParam("mtry", lower = 1, upper = 16, trafo = function(x) 2^x)
)

wrapped_lrn_rf = makeTuneWrapper(learner = lrn_rf,
                                 resampling  = tune_level,
                                 par.set = ps,
                                 control = ctrl,
                                 show.info = TRUE, 
                                 measures = mlr::mse)

configureMlr(on.learner.error = "warn", on.error.dump = TRUE)
library(parallelMap)
if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore", 
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams", 
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 2),
                mc.set.seed = TRUE)
}

if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams",
                      cpus =  round(parallel::detectCores() / 2))
}

result = mlr::resample(learner = wrapped_lrn_rf, 
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::mse)
# stop parallelization
parallelStop()
```