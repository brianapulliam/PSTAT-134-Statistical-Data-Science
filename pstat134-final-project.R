library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(naniar)
library(glmnet)
library(mice)
library(ggplot2)
library(mgcv)


###############################################################################

# API Call

res <- GET("https://api.covidtracking.com/v1/states/daily.json")
states_data <- fromJSON(rawToChar(res$content))

# view(states_data)
# The states_data dataset contains records data from
# Jan 2020 to Mar 2021 for all states and territories

# States & Territories in states_data
unique(states_data$state)

# State of interest: AL & WA 

# Num Variables/Columns
names(states_data) 
# Contains 56 variables (COLS). Which to keep and which to drop?


###############################################################################

# Data Cleaning

AL <- states_data %>% 
  filter(state == "AL") %>% 
  select(which(colMeans(is.na(.)) <= 0.35),"death",
         "recovered","onVentilatorCumulative",
         -state,-hash,-checkTimeEt,-dateChecked,
         -hospitalizedCurrently,-hospitalizedCumulative,
         -probableCases,-dateModified,-checkTimeEt,
         -lastUpdateEt,-negative)
AL <- AL %>% select_if(~ length(unique(.)) > 1)


WA <- states_data %>% 
  filter(state == "WA") %>% 
  select(which(colMeans(is.na(.)) <= 0.35),"death",
         "recovered",,"onVentilatorCumulative")
WA <- WA %>% select_if(~ length(unique(.)) > 1) #


common_columns <- intersect(names(AL), names(WA)) 

# Subset each data frame to keep only the common columns
AL <- AL %>% select(all_of(common_columns))
WA <- WA %>% select(all_of(common_columns))


###############################################################################

# Imputation

# Perform multiple imputations for AL and WA datasets
# Impute missing data using predictive mean matching (regression)
# Generate 5 imputed datasets and average them for more robust imputation
imputed_AL <- mice(AL%>%select(-date), m = 5, method = 'pmm', maxit = 50, seed = 500)
AL_complete <- complete(imputed_AL)

imputed_WA <- mice(WA%>%select(-date), m = 5, method = 'pmm', maxit = 50, seed = 500)
WA_complete <- complete(imputed_WA)


###############################################################################

# Formatting Date
AL_complete$date <- AL$date
WA_complete$date <- WA$date
WA_complete <- WA_complete %>% drop_na(positive, positiveCasesViral)

vis_miss(AL_complete)
vis_miss(WA_complete)
names(WA_complete)
ncol(AL_complete)
nrow(AL_complete)
ncol(WA_complete)
nrow(WA_complete)

# Convert date column to Date format
AL_complete$date <- as.Date(as.character(AL_complete$date), format = "%Y%m%d")
WA_complete$date <- as.Date(as.character(WA_complete$date), format = "%Y%m%d")

# Set the baseline date
baseline_date <- as.Date("2020-01-01")

# Compute days since the baseline date
AL_complete$daysSinceStart <- as.numeric(AL_complete$date - baseline_date)
WA_complete$daysSinceStart <- as.numeric(WA_complete$date - baseline_date)


###############################################################################

# Deaths Over Time Visualization

# Plot for Alabama
ggplot(AL_complete, aes(x = daysSinceStart, y = death)) +
  geom_line(color = "blue") +
  labs(title = "Deaths Over Time (Alabama)", 
       x = "Days Since Jan 1, 2020", y = "Deaths") +
  theme_minimal()

# Plot for Washington
ggplot(WA_complete, aes(x = daysSinceStart, y = death)) +
  geom_line(color = "red") +
  labs(title = "Deaths Over Time (Washington)", 
       x = "Days Since Jan 1, 2020", y = "Deaths") +
  theme_minimal()


###############################################################################

# Variable Selection

# AL lasso regression
x = model.matrix(death~., (AL_complete%>%select(-date)))[,-1]
y = AL_complete$death

grid = 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
plot(lasso.mod, xvar="lambda", label = TRUE)

set.seed(1) 
cv.out.lasso=cv.glmnet(x, y, alpha = 1)
plot(cv.out.lasso)
abline(v = log(cv.out.lasso$lambda.min), col="red", lwd=3, lty=2)

bestlam_AL = cv.out.lasso$lambda.min
bestlam_AL

out = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam_AL)[1:12,]
lasso.coef

# WA lasso regression
x = model.matrix(death~., (WA_complete%>%select(-date)))[,-1]
y = WA_complete$death

grid = 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
plot(lasso.mod, xvar="lambda", label = TRUE)

set.seed(1) 
cv.out.lasso=cv.glmnet(x, y, alpha = 1)
plot(cv.out.lasso)
abline(v = log(cv.out.lasso$lambda.min), col="red", lwd=3, lty=2)

bestlam_WA = cv.out.lasso$lambda.min
bestlam_WA

out = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam_WA)[1:12,]
lasso.coef


###############################################################################

# Model Fitting

# Fit the GAM for the AL_complete data using the selected variables from LASSO
gam_AL <- gam(death ~ s(positive) + s(totalTestResults) + s(hospitalized) + 
                      s(positiveCasesViral) + s(deathIncrease) +
                      s(daysSinceStart), 
              data = AL_complete, 
              family = gaussian())
summary(gam_AL)
# Plot the smooths
plot(gam_AL, pages = 1, residuals = TRUE, rug = TRUE)
# Check GAM diagnostics
gam.check(gam_AL)
# Plot residuals vs fitted
plot(gam_AL$residuals ~ gam_AL$fitted.values, 
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# Fit the GAM for the WA_complete data using the selected variables from LASSO
gam_WA <- gam(death ~ s(positive) + s(total) + s(daysSinceStart), 
              data = WA_complete, 
              family = gaussian(link = "identity"))

summary(gam_WA)
# Plot the smooths
plot(gam_WA, pages = 1, residuals = TRUE, rug = TRUE)
# Check GAM diagnostics
gam.check(gam_WA)
# Plot residuals vs fitted
plot(gam_WA$residuals ~ gam_WA$fitted.values, 
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")


###############################################################################

# Model Training and Testing
# Alabama
set.seed(123) # Ensure reproducibility
AL_train_index <- sample(1:nrow(AL_complete), size = 0.7 * nrow(AL_complete)) # 70% training
AL_train_data <- AL_complete[AL_train_index, ]
AL_test_data <- AL_complete[-AL_train_index, ]

trained_gam_AL <- gam(death ~ s(positive) + s(totalTestResults) + s(hospitalized) + 
                s(positiveCasesViral) + s(deathIncrease) +
                s(daysSinceStart), data = AL_train_data) 
              
AL_predictions <- predict(trained_gam_AL, newdata = AL_test_data)

# Mean Squared Error (MSE)
mse <- mean((AL_test_data$death - AL_predictions)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(AL_test_data$death - AL_predictions))

# Print results
print(list(MSE = mse, RMSE = rmse, MAE = mae))

              
# Plot of Actual and Predicted Deaths in Alabama over Time
plot_data <- data.frame(
  daysSinceStart = AL_test_data$daysSinceStart,
  Actual = AL_test_data$death,
  Predicted = AL_predictions
)


ggplot(plot_data, aes(x = daysSinceStart)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  labs(title = "Actual vs Predicted Deaths Over Time (Alabama)", 
       x = "Days Since Jan 1, 2020", y = "Deaths") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Washington
set.seed(123) # Ensure reproducibility
WA_train_index <- sample(1:nrow(WA_complete), size = 0.7 * nrow(WA_complete)) # 70% training
WA_train_data <- WA_complete[WA_train_index, ]
WA_test_data <- WA_complete[-WA_train_index, ]

trained_gam_WA <- gam(death ~ s(positive) + s(total) + s(daysSinceStart), 
                      data = WA_train_data) 

WA_predictions <- predict(trained_gam_WA, newdata = WA_test_data)

# Mean Squared Error (MSE)
WA_mse <- mean((WA_test_data$death - WA_predictions)^2)

# Root Mean Squared Error (RMSE)
WA_rmse <- sqrt(WA_mse)

# Mean Absolute Error (MAE)
WA_mae <- mean(abs(WA_test_data$death - WA_predictions))

# Print results
print(list(MSE = WA_mse, RMSE = WA_rmse, MAE = WA_mae))


# Plot of Actual and Predicted Deaths in WASHINGTON over Time
WA_plot_data <- data.frame(
  daysSinceStart = WA_test_data$daysSinceStart,
  Actual = WA_test_data$death,
  Predicted = WA_predictions
)


ggplot(WA_plot_data, aes(x = daysSinceStart)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  labs(title = "Actual vs Predicted Deaths Over Time (Washington)", 
       x = "Days Since Jan 1, 2020", y = "Deaths") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

