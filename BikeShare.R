# BikeShare.R
# Kaggle Bike Sharing Demand Competition
# Regression Tree Workflow with tidymodels
# --------------------------------------------------------

# --- Libraries ---
library(tidyverse)
library(lubridate)
library(tidymodels)
library(vroom)

set.seed(123)

# --- Load Data ---
train <- vroom("train.csv")
test  <- vroom("test.csv")

# --- Cleaning Section ---
train <- train %>%
  select(-casual, -registered) %>%
  mutate(count = log(count + 1))   # log-transform target

# --- Feature Engineering Section ---
bike_rec <- recipe(count ~ ., data = train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(hour = lubridate::hour(datetime)) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# --- Regression Tree Section ---
# Define regression tree model (CART)
tree_mod <- decision_tree(
  cost_complexity = tune(),  # pruning parameter
  tree_depth = tune(),       # maximum depth of tree
  min_n = tune()             # minimum observations per node
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Workflow with recipe + model
tree_wf <- workflow() %>%
  add_model(tree_mod) %>%
  add_recipe(bike_rec)

# Cross-validation folds
folds <- vfold_cv(train, v = 5)

# Define grid of hyperparameters
tree_grid <- grid_regular(
  cost_complexity(),
  tree_depth(),
  min_n(),
  levels = 3  # 3x3x3 grid = 27 combinations
)

# Tune the regression tree
tree_tune <- tune_grid(
  tree_wf,
  resamples = folds,
  grid = tree_grid,
  metrics = yardstick::metric_set(yardstick::rmse)
)

# Select best model by RMSE
best_tree <- select_best(tree_tune, metric = "rmse")
best_tree

# Finalize workflow with best parameters
final_tree_wf <- finalize_workflow(tree_wf, best_tree)

# Fit final regression tree on all training data
final_tree_fit <- final_tree_wf %>%
  fit(data = train)

# --- Predict Test Set ---
test_pred <- predict(final_tree_fit, test) %>%
  bind_cols(test %>% select(datetime)) %>%
  mutate(count = round(exp(.pred) - 1)) %>%
  select(datetime, count)

# --- Save Submission File ---
vroom_write(test_pred, "submission.csv")

# --------------------------------------------------------
# End of Regression Tree Section
