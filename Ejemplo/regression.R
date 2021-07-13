library(dplyr)

# Get current directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# Set working directory to current directory
setwd(current_dir)

# Load dataset
advertising <- read.csv('advertising.csv', sep = ';', header = T, fileEncoding = 'utf-8')

# Show a few observations
glimpse(advertising)

# Statistical summary
summary(advertising)

# Pair dispersion plot
pairs(advertising, col = 'red')

# Single variable regression
lm_fit_sales_TV <- lm(Sales ~ TV, data = advertising)

lm_fit_sales_TV

# Model results
summary(lm_fit_sales_TV)


# Plotting the fitting line
plot(advertising$TV, advertising$Sales, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')
abline(lm_fit_sales_TV, col = 'blue')

# Predictions over new data
new_advertising = data.frame(TV = c(100, 150, 200, 250))
predicted_values <- predict(lm_fit_sales_TV, new_advertising, interval = 'confidence')
new_advertising <- cbind(new_advertising, predicted_values)
new_advertising


# Multiple Linear Regression
lm_fit_sales_all <- lm(Sales ~ TV + Radio + Newspaper, data = advertising)

lm_fit_sales_all


# Model results
summary(lm_fit_sales_all)

