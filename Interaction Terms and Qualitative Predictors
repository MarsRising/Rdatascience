#Step 1 Loading the Data Step

# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})


# Print the first six rows
print("head")
head(mtcars2, 6)

#Step 2 Subsetting Data and Correlation Matrix 

myvars <- c("mpg","wt","drat")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

#Step 3 Multiple Regression with Interaction Term

# Create the multiple regression model and print summary statistics. Note that this model includes the interaction term. 
model1 <- lm(mpg ~ wt + drat + wt:drat, data=mtcars_subset)
summary(model1)

#Step 4 Adding in a Qualitative Predictor

# Subsetting data to only include the variables that are needed
myvars <- c("mpg","wt","drat","am")
mtcars_subset <- mtcars2[myvars]

# Create the model
model2 <- lm(mpg ~ wt + drat + wt:drat + am, data=mtcars_subset)
summary(model2)

#Step 5 Fitted Values

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

#Step 6 Residuals

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

#Step 7 Diagnostic Plots - Residuals against Fitted Values

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

#Step 8 Diagnostic Plots - Q-Q Plot

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

#Step 9 Confidence Interval for Parameter Estimates

# confidence intervals for model parameters
print("confint")
conf_90_int <- confint(model2, level=0.90) 
round(conf_90_int, 4)

#Step 10 Predictions, Prediction Interval, and Confidence Interval 

newdata <- data.frame(wt=3.88, drat=3.05, am='1')

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

