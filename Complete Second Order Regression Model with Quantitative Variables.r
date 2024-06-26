#Load Data
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

plot(housing$price, housing$school_rating, 
     main = "Scatterplot of Price vs. Average School Rating",
     xlab = "Price", ylab = "Average School Rating",
     xlim=c(0, 2000000),
     ylim=c(0,10),
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$price, housing$crime, 
     main = "Scatterplot of Price vs. The Crime Rate per 100,000 people",
     xlab = "Price", ylab = "Crime Rate per 100,000 people",
     xlim=c(0, 2000000),
     ylim=c(0,500),
     col="red", 
     pch = 19, frame = FALSE)

# Create the second order regression model and print the statistics
model2 <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2) , data=housing)
summary(model2)

fitted_values <- fitted.values(model2) 
fitted_values

residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(school_rating=9.80, crime=81.02)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(school_rating=4.28, crime=215.50)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

#NESTED MODELS F-TEST

housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Create the reduced second order regression model and print the statistics
model3 <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
summary(model3)

# Create the complete model
fit_complete <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2) , data=housing)

# Create the reduced model
fit_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)

# Perform the F-test
anova(fit_complete, fit_reduced)
