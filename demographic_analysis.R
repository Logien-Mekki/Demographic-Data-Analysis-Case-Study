# Install required packages
install.packages(c("ggplot2", "dplyr", "forecast", "cluster", "tibble", "ggplot2", "tidyr"))
library(ggplot2)
library(dplyr)
library(forecast)
library(cluster)
library(tibble)

# Example: Load demographic data (replace with your actual dataset)
data <- read.csv("path/to/demographic_data.csv")

# Explore the dataset
head(data)
summary(data)

# Data Cleaning
data_cleaned <- data %>%
  filter(!is.na(population), !is.na(income)) %>%
  mutate(year = as.factor(year))

# Trend Analysis using Linear Regression
model <- lm(population ~ year + income + age_group, data = data_cleaned)
summary(model)

# Time Series Forecasting (Population)
ts_data <- ts(data_cleaned$population, frequency = 12, start = c(2002, 1))
model_forecast <- auto.arima(ts_data)
forecast_population <- forecast(model_forecast, h = 10)
plot(forecast_population)

# Clustering: K-means to group regions based on demographics
clustering <- kmeans(data_cleaned[, c("income", "age_group")], centers = 3)
data_cleaned$cluster <- clustering$cluster

# Visualize the results
ggplot(data_cleaned, aes(x = income, y = age_group, color = factor(cluster))) +
  geom_point() +
  labs(title = "Clustering of Regions Based on Income and Age Group")
