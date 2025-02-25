# R Project: Crop Yield Prediction Using Regression

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Sample crop yield dataset
data <- data.frame(
  Rainfall = c(100, 120, 130, 90, 110, 105, 140, 95, 125, 135),
  Fertilizer = c(50, 55, 60, 40, 52, 47, 65, 42, 58, 62),
  Yield = c(2.8, 3.1, 3.5, 2.5, 3.0, 2.7, 3.8, 2.6, 3.3, 3.6)
)

# Ensure column names are correctly referenced
colnames(data) <- c("Rainfall", "Fertilizer", "Yield")

# Check for missing values and handle them
data <- na.omit(data)

# Ensure there is sufficient data before fitting the model
if(nrow(data) > 1) {
  # Fit linear regression model
  model <- lm(Yield ~ Rainfall + Fertilizer, data = data)
  
  # Display model summary
  print(summary(model))
  
  # Predict yield based on inputs
  predicted_yield <- predict(model, newdata = data)
  data$Predicted_Yield <- predicted_yield
  
  # Visualization
  p <- ggplot(data, aes(x=Yield, y=Predicted_Yield)) +
    geom_point(color='blue') +
    geom_abline(intercept=0, slope=1, linetype='dashed', color='red') +
    ggtitle("Actual vs Predicted Crop Yield") +
    xlab("Actual Yield") +
    ylab("Predicted Yield")
  
  print(p)
  
  # Save results
  write.csv(data, "crop_yield_predictions.csv", row.names = FALSE)
  print("Crop yield predictions saved for reporting.")
} else {
  stop("Error: Insufficient data for modeling. Please provide more data.")
}
