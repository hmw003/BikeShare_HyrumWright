# BikeShareAnalysis.R
# Linear Regression for Kaggle Bike Sharing
# -----------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)

# --- Load data with vroom ---
train <- vroom("train.csv", col_types = cols(datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
test  <- vroom("test.csv",  col_types = cols(datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# --- Feature Engineering ---
add_datetime_features <- function(df) {
  df %>%
    mutate(
      hour       = hour(datetime),
      day        = day(datetime),
      month      = month(datetime),
      year       = ifelse(year(datetime) == 2011, 0, 1),
      dayofweek  = wday(datetime, week_start = 1) - 1, # Monday = 0
      is_weekend = ifelse(dayofweek >= 5, 1, 0)
    )
}

train <- add_datetime_features(train)
test  <- add_datetime_features(test)

# --- Target: log(count) ---
train$log_count <- log1p(train$count)

# --- Fit linear regression ---
# (exclude casual and registered as predictors)
lm_model <- lm(
  log_count ~ season + weather + holiday + workingday +
    temp + atemp + humidity + windspeed +
    hour + day + month + year + dayofweek + is_weekend,
  data = train
)

summary(lm_model)

# --- Predict on test set ---
test$pred_log <- predict(lm_model, newdata = test)
test$pred     <- pmax(0, expm1(test$pred_log))  # back-transform and clip negatives

# --- Build submission (fixed) ---
submission <- test %>%
  mutate(
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S"),
    count = round(pred)
  ) %>%
  select(datetime, count)

# --- Save submission with vroom_write ---
vroom_write(submission, "submission.csv", delim = ",")

cat("Submission saved to submission.csv\n")
print(head(submission))


weather_plot <- ggplot(data = bike, aes(x = weather)) +
  geom_bar()

temp_plot <- ggplot(data=bike, aes(x=temp, y=count)) +
  geom_point() +
  geom_smooth(se = FALSE)

workday_plot <- ggplot(data = bike, aes(x = workingday)) +
  geom_bar()

windspeed_plot <- ggplot(bike, aes(x= windspeed, y= count)) + 
  geom_point()

(weather_plot +windspeed_plot) / (temp_plot + workday_plot)
