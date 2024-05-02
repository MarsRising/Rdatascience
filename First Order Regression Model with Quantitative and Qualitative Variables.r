#Prepare Data

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

#Scatterplot Price vs Living Area
plot(housing$price, housing$sqft_living, 
     main = "Scatterplot of Price vs. The Living Area",
     xlab = "Price", ylab = "The Living Area",
     xlim=c(0, 2000000),
     ylim=c(0, 7000),
     col="red", 
     pch = 19, frame = FALSE)

#Scatterplot Price vs Age of Home
plot(housing$price, housing$age, 
     main = "Scatterplot of Price vs. The Age of the Home",
     xlab = "Price", ylab = "The Age of the Home",
     xlim=c(0, 2000000),
     ylim=c(0, 150),
     col="red", 
     pch = 19, frame = FALSE)

# Selecting price, sqft_living, and age variables to subset the data
myvars <- c("price","sqft_living","age")
housing_subset <- housing[myvars]

# Print the first 10 rows
print("head")
head(housing_subset, 10)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_subset, method = "pearson")
round(corr_matrix, 4)

# Subsetting data to only include the variables that are needed
myvars <- c("price","sqft_living","sqft_above","age", "bathrooms", "view")
housing_subset <- housing[myvars]

# Create the model
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing_subset)
summary(model1)

fitted_values <- fitted.values(model1) 
fitted_values

residuals <- residuals(model1)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(sqft_living=2150, sqft_above=1050, age=15, bathrooms=3, view="0")

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(sqft_living=4250, sqft_above=2100, age=5, bathrooms=5, view="2")

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)
