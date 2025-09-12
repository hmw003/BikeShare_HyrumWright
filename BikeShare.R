library(tidyverse)
library(tidymodels)
library(patchwork)

train<- vroom("train.csv")

weather_plot <- ggplot(data = train, aes(x = weather)) +
  geom_bar()

temp_plot <- ggplot(data=train, aes(x=temp, y=count)) +
  geom_point() +
  geom_smooth(se = FALSE)

workday_plot <- ggplot(data = train, aes(x = workingday)) +
  geom_bar()

windspeed_plot <- ggplot(train, aes(x= windspeed, y= count)) + 
  geom_point()

(weather_plot +windspeed_plot) / (temp_plot + workday_plot)
  