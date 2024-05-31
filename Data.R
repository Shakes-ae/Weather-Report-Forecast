install.packages('tidyverse')
install.packages('dplyr')
install.packages('reshape2')
install.packages('GGally')
install.packages('readr')
install.packages("ggplot2", dependencies = TRUE)
install.packages('forecast', dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages('heatmaply')
install.packages('Hmisc')
install.packages('openair')
install.packages('e107')
install.packages('urca')
install.packages('MTS')
install.packages("tsibble")
install.packages('summarytools')
install.packages('tidyr')
install.packages('corrplot')
install.packages('pracma')
install.packages('tseries')
install.packages('MARSS')

library(MARSS)
library(tseries)
library(pracma)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(reshape2)
library(caret)
library(readr)
library(forecast)
library(corrr)
library(heatmaply)
library(Hmisc)
library(openair)
library(e1071)
library(urca)
library(MVar)
library(MTS)
library(tsibble)
library(summarytools)
library(corrplot)

#Load the dataset
df <- read.csv('WRFdata_May2018.csv')
View(df)

#Extracting the row (location) needed for analysis
df_selected_row <- df[737,]
View(df_selected_row)

#Removing longitude and latitude from selected row
df_selected_row <- df_selected_row[-c(1,2)]
View(df_selected_row)

#Getting dates and time from the dataset
#Storing the column names(dates) of the selected row in a variable
column_names <- colnames(df_selected_row)
View(column_names)

#function to extract date-time information as it is arranged in the original dataset
date_time <- function(column_names){
  dates <- grep("^X[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}$", column_names, value = TRUE)
  return(dates)
}

#Applying extracted date-time function to each element in the column_name
Dates <- lapply(column_names, date_time)

#Converting dates from a list to a single vector converting all elements
Dates <- unlist(Dates)
print(Dates)

#Reshaping the entire dataset
df_to_vector <- as.vector(df_selected_row)
View(df_to_vector)

df_to_matrix <- matrix(as.numeric(df_to_vector), ncol = 10, byrow = TRUE)
View(df_to_matrix)

length(Dates)

Dates[length(Dates)+1]<- "X31.05.2018.21.00"
length(Dates)

#Renaming the columns
transformed_data <- data.frame(Time = Dates, 
                              TSK = df_to_matrix[,1],
                              PSFC = df_to_matrix[,2],
                              U10 = df_to_matrix[,3],
                              V10 = df_to_matrix[,4],
                              Q2 = df_to_matrix[,5],
                              Rainc = df_to_matrix[,6],
                              Rainnc = df_to_matrix[,7],
                              Snow = df_to_matrix[,8],
                              TSLB = df_to_matrix[,9],
                              SMOIS = df_to_matrix[,10])
View(transformed_data)

#Data Cleaning
missing_values <- colSums(is.na(transformed_data))
missing_values

#replacing missing values with mean values of each column
for (i in seq_along(transformed_data)) {
  if (missing_values[i] > 0) {
    mean_value <- mean(transformed_data[[i]], na.rm = TRUE)
    transformed_data[[i]][is.na(transformed_data[[i]])] <- mean_value
  }
}
View(transformed_data)

missing_values <- colSums(is.na(transformed_data))
missing_values

#Checking for outliers
boxplot(transformed_data$PSFC, main = "Boxplot for Surface Pressure", horizontal = TRUE, col = "lightblue")

boxplot(transformed_data$TSLB, main = "Boxplot for Soil Temperature", horizontal = TRUE, col = "yellow")

boxplot(transformed_data$TSK, main = "Boxplot for Surface Temperature", horizontal = TRUE, col = "red")

boxplot(transformed_data$Q2, main = "Boxplot for Specific Humidity", horizontal = TRUE, col = "purple")

boxplot(transformed_data$Rainc, main = "Boxplot for Accumulated Precipitation", horizontal = TRUE, col = "blue")

boxplot(transformed_data$Rainnc, main = "Boxplot for Non-convective Rain", horizontal = TRUE, col = "green")

duplicated(transformed_data)

#Calculating Wind Speed
x_wind <- transformed_data$U10
y_wind <- transformed_data$V10

WS <- sqrt(x_wind^2 + y_wind^2)

transformed_data$WS <- WS

#Calculating Wind Direction
transformed_data$WD <- ifelse(transformed_data$U10==0,
                              ifelse(transformed_data$V10 > 0, 90, 270),
                              atan(transformed_data$V10/transformed_data$U10)*180/pi)
View(transformed_data)

#Converting the Time column to date-time format

typeof(transformed_data$Time)
class(transformed_data$Time)
transformed_data$Time<- sub("X", "", transformed_data$Time)

transformed_data$Time <- as.POSIXct(transformed_data$Time, format = "%d.%m.%Y.%H.%M")
View(transformed_data)

typeof(transformed_data$Time)
class(transformed_data$Time)


count(transformed_data)
str(transformed_data)
glimpse(transformed_data)

#EDA
#checking the data information
View(head(transformed_data))
#An overview of the dataset
summary(transformed_data)
#Checking for missing values
sapply(transformed_data, function(x) sum(is.na(x)))
#Exploring the distribution of each column
View(dfSummary(transformed_data))
#Visualising the distribution of each column
transformed_data %>% 
  gather(key = "variable", value = "value") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~ variable, scales = "free")
#Correlation analysis of the dataset
cor(transformed_data[, sapply(transformed_data, is.numeric)])
#Visualising correlation matrix
corrplot(cor(transformed_data[, sapply(transformed_data, is.numeric)]), method = "number")
#Time series analysis of each column in the dataset
plot(transformed_data$Time, transformed_data$TSK)
plot(transformed_data$Time, transformed_data$PSFC)
plot(transformed_data$Time, transformed_data$Q2)
plot(transformed_data$Time, transformed_data$Rainc)
plot(transformed_data$Time, transformed_data$Rainnc)
plot(transformed_data$Time, transformed_data$Snow)
plot(transformed_data$Time, transformed_data$TSLB)
plot(transformed_data$Time, transformed_data$SMOIS)
plot(transformed_data$Time, transformed_data$WS)
plot(transformed_data$Time, transformed_data$WD)

#Analysis of wind patterns (Wind Speed and Direction) in the dataset
#plotting the Wind Rose diagram to see the patterns of wind speed and wind direction
transformed_data$Time <- as.factor(transformed_data$Time)
windRose(transformed_data, ws = "WS", wd = "WD", main = "Wind Rose Diagram for May 2018")

#Selecting features to be used for analysis
relevant_columns <- c("WS", "TSK", "PSFC", "Q2", "Rainc", "Rainnc", "TSLB", "SMOIS")
filtered_data <- transformed_data[, relevant_columns]
View(filtered_data)

#Getting the correlation matrix of selected features (TSK, Q2 and SMOIS)
filtered_data_cor_matrix <- cor(filtered_data)
View(filtered_data_cor_matrix)

#Testing for Null and Alternate Hypothesis/Correlation Test
res<- rcorr(filtered_data_cor_matrix)
#P-value
res$P
#Correlation Coefficients
res$r
#heatmap of the correlation matrix of surface temperature, specific humidity and soil moisture
heatmaply(filtered_data_cor_matrix, main = "Correlation Matrix")

#Scatterplot of Wind Direction and Wind Speed
ggplot(data = transformed_data, aes(x = WD, y = WS)) +
  
  geom_point() +
  labs(title = "Scatterplot of Wind Direction vs. Wind Speed", x = "Wind Direction", y = "Wind Speed")

#Checking for significant relationship between Surface Pressure and Soil Temperature
#Scatterplot to view the relationship between Surface pressure and Soil Temperature
ggplot(transformed_data, aes(x = PSFC, y = TSLB)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Surface Pressure vs Soil Temperature",
       x = "Surface Pressure (Pa)",
       y = "Accumulated Precipitation (mm)")

correlation_PSFC_Rainc <- cor.test(transformed_data$PSFC, transformed_data$TSLB)
print(correlation_PSFC_Rainc)

model <- lm(TSLB ~ PSFC, data = transformed_data)
summary(model)

#Machine learning modeling to forecast soil temperature 
#Selection of columns needed for the analysis
ml_relevant_column <- c("Time", "TSK", "Q2", "Rainc", "TSLB", "SMOIS", "Snow")
ml_filtered_data <- transformed_data[, ml_relevant_column]

#splitting the data into train set and test set
set.seed(123)
train_data <- ml_filtered_data[1: (nrow(ml_filtered_data)*0.80), ]
test_data <- ml_filtered_data[(nrow(ml_filtered_data)*0.80 + 1):nrow(ml_filtered_data), ]

#training the models
train_model <- train(TSK ~ ., data = train_data, method = "lm")
train_model_rf <- train(TSK ~ ., data = train_data, method = "rf")
train_model_svm <- train(TSK ~ ., data = train_data, kernel = "radial")
train_model_svmL <- train(TSK ~ ., data = train_data, kernel = "linear")
train_model_svmP <- train(TSK ~ ., data = train_data, kernel = "polynomial")

#predictions
prediction_lm <- predict(train_model, newdata = test_data)
prediction_rf <- predict(train_model_rf, newdata = test_data)
prediction_svm <- predict(train_model_svm, newdata = test_data)
prediction_svmL <- predict(train_model_svmL, newdata = test_data)
prediction_svmP <- predict(train_model_svmP, newdata = test_data)

#Getting the mean absolute error
mae_lm <- mean(abs(prediction_lm - test_data$TSK)^2)
mae_rf <- mean(abs(prediction_rf - test_data$TSK)^2)
mae_svm <- mean(abs(prediction_svm - test_data$TSK)^2)
mae_svmL <- mean(abs(prediction_svmL - test_data$TSK)^2)
mae_svmP <- mean(abs(prediction_svmP - test_data$TSK)^2)

#Getting the best model
models <- list(train_model, train_model_rf, train_model_svm, train_model_svmL, train_model_svmP)
maes <- c(mae_lm, mae_rf, mae_svm, mae_svmL, mae_svmP)
model_names <- c("Linear Model", "Random Forest", "SVM(Radial)", "SVM(Linear)", "SVM(Polynomial)")

model_performance <- data.frame(Model = model_names, MAE = maes) %>%
  arrange(MAE) %>%
  print()

#best_model <- models[[which.min(maes)]]
#print(best_model)

#comparing best model with the test data
prediction_svmL <- data.frame(prediction_svmL)
View(prediction_svmL)
View(test_data)




wind_speed <- transformed_data$WS
View(wind_speed)
hist(wind_speed, main = "Distribution of Wind Speeds in Co Kerry, May 2018", xlab = "Wind Speed (m/s)", col = "blue")

density_plot <- density(wind_speed)
plot(density_plot, main = "Distribution of Wind Speeds in Co Kerry, May 2018", xlab = "Wind Speed (m/s)", col = "red")

#Checking for trends in humidity and time-series forecast of humidity levels
#Checking the trend of Q2 in May
summary(transformed_data$Q2)

#line graph to see the trend of humidity levels in May
ggplot(transformed_data, aes(x = Time, y = Q2)) + 
  geom_line() + 
  labs(x = "Time", y = "2-meter Specific Humidity") + 
  theme_classic()

#Decomposing the humidity levels into its seasonal trend and residual components
humidity_ts <- ts(transformed_data$Q2, frequency = 8)
decomp <- stl(humidity_ts, s.window = "periodic")
plot(decomp)

#performing the ADF test
df_test <- ur.df(humidity_ts)
summary(df_test)

#time series forecasting
fit <- auto.arima(humidity_ts)
forecast <- forecast(fit, h = 30)
plot(forecast)

summary(fit)
summary(forecast)

#calculating the mean absolute error and mean squared error
mae <- mean(abs(forecast$residuals))
mse <- mean(forecast$residuals^2)
cat("MAE:", mae, "\nMSE:", mse)

#Relationship between wind speed and precipitation events, and time-series forecast
#getting the correlation between wind speed, accumulated rain and non-convective rain
transformed_data_melted <- melt(transformed_data, id.vars = "WS", 
                            measure.vars = c("Rainc", "Rainnc"), 
                            variable.name = "Precipitation_Type", 
                            value.name = "Precipitation_Amount")

cor_matrix <- cor(transformed_data[, c("WS", "Rainc", "Rainnc")])
print(cor_matrix)
heatmaply(cor_matrix, main = "Correlation Matrix")

cor_matrix <- rcorr(as.matrix(transformed_data[, c("WS", "Rainc", "Rainnc")]))
print(cor_matrix)

#Scatterplot to indicate relationship
ggplot(transformed_data_melted, aes(x = WS, y = Precipitation_Amount, color = Precipitation_Type)) + 
  geom_point() + 
  labs(x = "Wind Speed", y = "Precipitation Amount", color = "Precipitation Type") + 
  theme_classic()

ggplot(transformed_data_melted, aes(x = WS, y = Precipitation_Amount, color = Precipitation_Type)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Wind Speed", y = "Precipitation Amount", color = "Precipitation Type") + 
  theme_classic()

#Time Series Forecast of Wind Speed, Accumulated Precipitation and Non-Convective Rain
#Setting the time column as the index
df_time_series <- transformed_data %>% 
  arrange(Time) %>% 
  mutate(index = row_number()) %>% 
  pivot_wider(names_from = Time, values_from = c(TSK, PSFC, U10, V10, Q2, Rainc, Rainnc, Snow, TSLB, SMOIS, WS, WD)) %>% 
  select(-index)

#Resample the data to ensure equal time intervals
df_resampled <- transformed_data %>% 
  mutate(Time = seq(min(Time), max(Time), by = "3 hours"))

#creating a new dataframe with needed features
features <- c("WS", "Rainc", "Rainnc")
df_features <- df_resampled[, features]

#convert the dataframe to a timeseries object
df_ts <- ts(df_features, frequency = 24 * 7)

#Testing for stationarity
df_test1 <- adf.test(df_ts[,1])
df_test2 <- adf.test(df_ts[,2])
df_test3 <- adf.test(df_ts[,3])

df_test1
df_test2
df_test3

#training the ARIMA model
fit <- auto.arima(transformed_data$WS, xreg = cbind(transformed_data$Rainc, transformed_data$Rainnc))

#Forecasting the model
forecast <- forecast(fit, h = 30, xreg = cbind(transformed_data$Rainc, transformed_data$Rainnc))

#get the summary of forecast and plot forecast
summary(fit)
summary(forecast)
plot(forecast)

#calculating the mean absolute error and mean squared error
mae <- mean(abs(forecast$residuals))
mse <- mean(forecast$residuals^2)
cat("MAE:", mae, "\nMSE:", mse)
