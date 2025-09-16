library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

bike<- vroom("train.csv")

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

### HOMEWORK 2

test <- vroom("test.csv", col_types = cols(datetime = col_character()))

# Fit log-linear regression model
my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(log(count) ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed,
      data = bike)

# Predict on test set
log_predictions <- predict(my_linear_model, new_data = test)

# Back-transform and clip negative counts
bike_predictions <- pmax(exp(log_predictions$.pred), 0)

# Prepare submission
submission <- tibble(
  datetime = test$datetime,
  count = bike_predictions
)

# Save CSV
vroom_write(submission, "submission.csv", delim = ",")
