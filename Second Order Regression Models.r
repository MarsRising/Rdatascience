#Step 1 Load Data

economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first six rows
print("head")
head(economic, 6)

#Step 2 Scatterplot of Wage Growth and GDP Growth

plot(economic$gdp, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and GDP",
     xlab = "GDP", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

#Step 3 Scatterplot of Wage Growth and Inflation

plot(economic$inflation, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and Inflation",
     xlab = "Inflation", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

#Step 4 Quadratic (Second Order) Model with One Quantitative Variables

# Create the second order regression model and print the statistics
model1 <- lm(wage_growth ~ gdp + I(gdp^2), data=economic)
summary(model1)

#Step 5 Prediction, Prediction Interval, and Confidence Interval

newdata <- data.frame(gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

#Step 6 Complete Second Order Model (Two Quantitative Variables)

# Create the second order regression model and print the statistics
model2 <- lm(wage_growth ~ inflation + gdp + inflation:gdp + I(inflation^2) + I(gdp^2) , data=economic)
summary(model2)

#Step 7 Prediction, Prediction Interval, Confidence Interval

newdata <- data.frame(inflation=2.1, gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

#Step 8 Complete Second Order Model (One Quantitative and Qualitative Variable)

# Create the second order regression model and print the statistics
model3 <- lm(wage_growth ~ inflation + economy + inflation:economy + I(inflation^2) + I(inflation^2):economy, data=economic)
summary(model3)

#Step 9 Prediction, Prediction Interval, and Confidence Interval 

newdata <- data.frame(inflation=2.1, economy='recession')

print("prediction interval")
prediction_pred_int <- predict(model3, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model3, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

