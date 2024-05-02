##         LIBRARY IMPORTATION                          ####
# Importing necessary libraries
library(readxl)
library(dplyr)
library(psych)
library(tidyverse)
library(ggplot2)
library(datarium)
library(qqplotr)
library(car)
library(corrplot)
library(caret)
library(e1071)

##          DATA EXPLORATION                            ####     

# Loading the dataset into R 
jobs <- read_excel("jobs.xlsx")

# Displaying the first few rows of the dataset to understand its structure
head(jobs)

# Understanding the structure of the dataset
str(jobs)

# Observing the dimension of the data
dim(jobs)

glimpse(jobs)


##      DATA PROCESSING AND CLEANING                    ####

# Renaming columns for easy encoding
jobs <- jobs %>%
  rename(
    'country' = 'Country Name',
    'year' = 'Time',
    'employment_services' = 'Employment in services (% of total employment) (modeled ILO estimate) [SL.SRV.EMPL.ZS]',
    'gdp_per_capita' = 'GDP per capita (constant 2005 US$) [NY.GDP.PCAP.KD]',
    'gini_index' = 'GINI index (World Bank estimate) [SI.POV.GINI]',
    'labor_force_participation_rate' = 'Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate) [SL.TLF.CACT.ZS]',
    'Manuf_Value' = 'Manufacturing, value added (% of GDP) [NV.IND.MANF.ZS]',
    'total_employment' = 'Total employment, total (ages 15+) [SL.EMP.TOTL]',
    'unemployment_total' = 'Unemployment, total (% of total labor force) (modeled ILO estimate) [SL.UEM.TOTL.ZS]',
    'unemployment_youth' = 'Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) [SL.UEM.1524.ZS]',
    'urban_population' = 'Urban population (% of total) [SP.URB.TOTL.IN.ZS]',
    'wage_and_salaried_workers' = 'Wage and salaried workers, total (% of total employment) (modeled ILO estimate) [SL.EMP.WORK.ZS]'
  )

#Displaying Name change
colnames(jobs)

# Changing type from chr to dbl and checking the class of each column after the conversion
cols.num <- 3:12
jobs[cols.num] <- sapply(jobs[cols.num], as.numeric)
sapply(jobs, class)

# Checking for missing data in each column using sapply
print("Columns with Missing Data:")
sapply(jobs, function(col) sum(is.na(col)))

# Dropping columns with missing values
jobs <- jobs %>%
  select(-which(colnames(jobs) %in% c(
    'Manuf_Value', 'gini_index'
  )))

glimpse(jobs)

# Regional Grouping of Countries

# Creating a new column 'region' and initialing it with NA
jobs$region <- NA

# Assigning regions to countries
jobs$region[jobs$country %in% c("South Africa", "Nigeria", "Ghana", "Kenya", "Sierra Leone")] <- "Sub-Saharan Africa"
jobs$region[jobs$country %in% c("Denmark", "France", "Germany", "Italy", "United Kingdom")] <- "Europe"
jobs$region[jobs$country %in% c("China", "Japan", "Malaysia", "Korea, Rep.", "Australia")] <- "East Asia"


## HISTOGRAMS FOR OUTLIER DETECTION AND NORMALITY CHECK ####

# Hitogram for jobs
ggplot(jobs) +
  aes(x = employment_services) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")
theme_minimal()

ggplot(jobs) +
  aes (x = gdp_per_capita) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") 
theme_minimal()

ggplot(jobs) +
  aes (x = labor_force_participation_rate) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") 
theme_minimal()

ggplot(jobs) +
  aes (x = total_employment) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")
theme_minimal()

ggplot(jobs) +
  aes(x = unemployment_total) +
  geom_histogram(bins = 30L,  fill = "#0c4c8a")
theme_minimal()

ggplot (jobs) +
  aes (x = unemployment_youth) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")
theme_minimal()

ggplot (jobs) +
  aes (x = wage_and_salaried_workers) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")
theme_minimal()

ggplot (jobs) +
  aes (x = urban_population) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")
theme_minimal()


# BOX PLOT
# Box plots for each variable with respect to 'region' with rainbow-colored outliers
qplot(region, employment_services, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, gdp_per_capita, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, labor_force_participation_rate, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, total_employment, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, unemployment_total, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, unemployment_youth, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, wage_and_salaried_workers, data = jobs, geom = "boxplot", fill = I("red"))

qplot(region, urban_population, data = jobs, geom = "boxplot", fill = I("red"))


##          DESCRIPTIVE STATISTICAL ANALYSIS            ####

# Summary statistics to explore all job indicators
summary_stats <- describe(jobs)
print(summary_stats)

# Mean of Indicators per country
each_country_mean <- jobs %>% 
  group_by(country) %>% 
  summarise(across(c(employment_services, gdp_per_capita,labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(mean = mean))) 

# Median of Indicators per country
each_country_median <- jobs %>% 
  group_by(country) %>% 
  summarise(across(c(employment_services, gdp_per_capita,labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(median = median))) 

# Mean of Indicators per region
each_region_mean <- jobs %>% 
  group_by(region) %>% 
  summarise(across(c(employment_services, gdp_per_capita,labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(mean = mean))) 

# Median of Indicators per region
each_region_median <- jobs %>% 
  group_by(region) %>% 
  summarise(across(c(employment_services, gdp_per_capita,labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(median = median)))

# Standard deviation of Indicators per region
each_region_sd <- jobs %>% 
  group_by(region) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(sd = sd)))

# Standard deviation of Indicators per region
each_country_sd <- jobs %>% 
  group_by(country) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(sd = sd)))

# Skewness of Indicators per country
each_country_skew <- jobs %>% 
  group_by(country) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(skew = skew)))

# Kurtosis of indicators for per country
each_country_kurtosis <- jobs %>% 
  group_by(country) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(kurtosis = kurtosis)))

# Skewness of Indicators per region
each_region_skew <- jobs %>% 
  group_by(region) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(skew = skew)))

# Kurtosis of indicators per region
each_region_kurtosis <- jobs %>% 
  group_by(region) %>% 
  summarise(across(c(employment_services, gdp_per_capita, labor_force_participation_rate,
                     total_employment, unemployment_total, unemployment_youth,
                     urban_population, wage_and_salaried_workers), list(kurtosis = kurtosis)))


##   CORRELATION ANALYSIS FOR THE INDICATORS            ####

# Removing character value 'country' and 'region' to find correlation
jobs_new <- jobs[, -which(names(jobs) %in% c('country', 'region'))]
glimpse(jobs_new)

# Calculate Spearman's rank correlation for all variables Correlation plot
cor(jobs_new, method = 'spearman')

# Correlation plot
corrplot(cor(jobs_new),
         type = "lower",
         tl.col = "black",
         method = 'number',
         tl.srt = 45)

# Correlation tests with 'employment_services'
cor.test(jobs_new$employment_services, jobs_new$gdp_per_capita, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$unemployment_youth, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$total_employment, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$unemployment_total, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$urban_population, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$labor_force_participation_rate, method = 'spearman')
cor.test(jobs_new$employment_services, jobs_new$wage_and_salaried_workers, method = 'spearman')


##    REGRESSION ANALYSIS FOR THE INDICATORS            ####

# Sub setting the data for the year 2016
tr <- jobs[jobs$year == "2016",]

# Creating a pairs plot for selected columns
pairs(tr[, c(3:10)], lower.panel = NULL, pch = 19, cex = 0.2)

# Removing unnecessary columns
jb <- tr[, -which(names(tr) %in% c('year', 'country', 'region'))]

# Checking the model to see if there's linearity
pairs(jb[, c(1:8)], lower.panel = NULL, pch = 19, cex = 0.2)

# Calculating Spearman correlation for the 'jb' dataset
cor_matrix <- cor(jb, method = 'spearman')

# Plotting the correlation matrix
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", 
         method = 'number', 
         tl.srt = 45)

# Using lm() for performing linear regression analysis
model1 <- lm(employment_services ~ wage_and_salaried_workers, data = jb)
model1

# Displaying  summary of the regression model
summary(model1)

# Checking for Linearity
crPlots(model1)

# Checking for Normality of Residuals
plot(model1, which = 1)

# Checking for Independence of Residuals
plot(model1, which = 2)

# Checking for Homoscedasticity
plot(model1, which = 3)


# 'jb' been my data frame
new_data <- data.frame(wage_and_salaried_workers = c(84.849, 18.505, 26.801, 37.918, 9.986, 90.916, 88.085))  

# Predict using the model
predictions <- predict(model1, newdata = new_data)

# Combine actual and predicted values
results <- cbind(new_data, actual = jb$employment_services[1:7], predicted = predictions)

# Display the results
print(results)

## MULTIPLE LINEAR REGRESSION 
# Using lm() for performing multiple linear regression analysis
model <- lm(employment_services ~ wage_and_salaried_workers + gdp_per_capita, data = jb)
model

# Displaying summary of the regression model
summary(model)

# Checking for multicollinearity using VIF
vif_values <- car::vif(model)
print(vif_values)

# Checking for Linearity
crPlots(model)

# Checking for Normality of Residuals
plot(model, which = 1)

# Checking for Independence of Residuals
plot(model, which = 2)

# Checking for Homoscedasticity
plot(model, which = 3)

##   TIME SERIES ANALYSIS FOR EMPLOYMENT SITUATION      ####
library(TTR)
library(forecast)
library(tseries)

# Defining a new ES_ts - Employment Situation time series
ES_ts <- each_country_mean[c('employment_services_mean')]
glimpse(ES_ts)

# Employment Situation time series
timeseries <- ts(ES_ts, start = c(2002))
timeseries
plot.ts(timeseries)

acf(timeseries)

# Print the characteristics of the time series
print(timeseries)

# ADF test for stationarity
# Null hypothesis (H0): Series is non-stationary
# Alternative hypothesis (H1): Series is stationary
adf_test_result <- adf.test(timeseries)
adf_test_result

##       MODEL1 HOLT-WINTERS SERIES                     #####
ES_forecast <- HoltWinters(timeseries, gamma = FALSE)
ES_forecast
ES_forecast$fitted
plot(ES_forecast)

# SSE - sum of squared errors
ES_forecast$SSE

# Shift the graph up
plot(HoltWinters(timeseries, gamma = FALSE))

require("forecast")
# Forecasting for 8 steps ahead
ES_forecast2 <- forecast(ES_forecast, h = 8)
ES_forecast2
plot(ES_forecast2)
ES_forecast$fitted
ES_forecast2$x

# Autocorrelation plot of residuals
acf(ES_forecast2$residuals, lag.max = 4, na.action = na.pass)

# Partial Autocorrelation plot of residuals
pacf(ES_forecast2$residuals, lag.max = 4, na.action = na.pass)

# Plotting Residuals against time
plot(ES_forecast2$residuals, ylab = "ES_forecast2$residuals", xlab = "Time")

# Plotting Residuals: Forecast errors plotted on the time graph
plot.ts(ES_forecast2$residuals)

# Ljung-Box test for autocorrelation of residuals
Box.test(ES_forecast2$residuals, lag = 4, type = "Ljung-Box")


plotForecastErrors <- function(forecasterrors)
{
  # Remove missing values
  forecasterrors <- na.omit(forecasterrors)
  
  # making a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(ES_forecast2$residuals)



##       MODEL2 ARIMA TIME SERIES                       ####
# Load necessary libraries
library(tseries)

# Display the time series
timeseries

# ADF test for stationarity
# Null hypothesis (H0): Series is non-stationary
# Alternative hypothesis (H1): Series is stationary
adf_test_result <- adf.test(timeseries)
adf_test_result

# Check ACF plot for autocorrelation
acf(timeseries)

# Fit ARIMA model
ES_ar <- auto.arima(timeseries)
ES_ar

# Autocorrelation plot of residuals
acf(ES_ar$residuals)

# Partial Autocorrelation plot of residuals
pacf(ES_ar$residuals)

# Plotting residuals
plot.ts(ES_ar$residuals)

# Forecasting
ES_ar_forecast <- forecast(ES_ar, h = 5)
ES_ar_forecast

# Ljung-Box test for autocorrelation of residuals
Box.test(ES_ar$residuals, type = "Ljung-Box")

# Plotting forecast
plot(ES_ar_forecast)

# Plotting forecast errors 
plotForecastErrors(ES_ar_forecast$residuals)

# Filtering out NA values from forecast errors
filtered_forecast_errors <- ES_ar_forecast$residuals[!is.na(ES_ar_forecast$residuals)]

## Plotting filtered forecast errors
plotForecastErrors(filtered_forecast_errors)

##         MODEL COMPARISON                             ####
# Function to calculate Mean Absolute Error (MAE) and Mean Squared Error (MSE)
calculate_accuracy <- function(actual, forecasted) {
  # Calculate accuracy metrics
  me <- mean(actual - forecasted, na.rm = TRUE)
  mae <- mean(abs(actual - forecasted), na.rm = TRUE)
  mse <- mean((actual - forecasted)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mpe <- mean(((actual - forecasted) / actual) * 100, na.rm = TRUE)
  mape <- mean(abs(((actual - forecasted) / actual) * 100), na.rm = TRUE)
  mase <- mean(abs(actual - forecasted) / mean(abs(diff(actual))), na.rm = TRUE)
  acf1 <- acf(actual - forecasted, lag.max = 1, plot = FALSE)$acf[2]
  
  # Return a named vector of accuracy metrics
  return(c(ME = me, MAE = mae, MSE = mse, RMSE = rmse, MPE = mpe, MAPE = mape, MASE = mase, ACF1 = acf1))
}

# Holt-Winters Forecasting
ES_forecast_hw <- HoltWinters(timeseries, gamma = FALSE)
ES_forecast_hw2 <- forecast(ES_forecast_hw, h = 15)
ES_forecast_hw2

# ARIMA Forecasting
ES_arima <- auto.arima(timeseries)
ES_arima_forecast <- forecast(ES_arima, h = 15)
ES_arima_forecast

# Extracting actual values for the forecast period
actual_values <- as.numeric(ES_ts[['employment_services_mean']])

# Extracting forecasted values
forecast_hw <- ES_forecast_hw2$mean
forecast_arima <- as.numeric(ES_arima_forecast$mean)

# Comparing mean and accuracy for Holt-Winters
accuracy_hw <- calculate_accuracy(actual_values, forecast_hw)
cat("Holt-Winters Model Accuracy:\n")
cat("ME:", accuracy_hw["ME"], "\n")
cat("MAE:", accuracy_hw["MAE"], "\n")
cat("MSE:", accuracy_hw["MSE"], "\n")
cat("RMSE:", accuracy_hw["RMSE"], "\n")
cat("MPE:", accuracy_hw["MPE"], "\n")
cat("MAPE:", accuracy_hw["MAPE"], "\n")
cat("MASE:", accuracy_hw["MASE"], "\n")
cat("ACF1:", accuracy_hw["ACF1"], "\n\n")

# Comparing mean and accuracy for ARIMA
accuracy_arima <- calculate_accuracy(actual_values, forecast_arima)
cat("ARIMA Model Accuracy:\n")
cat("ME:", accuracy_arima["ME"], "\n")
cat("MAE:", accuracy_arima["MAE"], "\n")
cat("MSE:", accuracy_arima["MSE"], "\n")
cat("RMSE:", accuracy_arima["RMSE"], "\n")
cat("MPE:", accuracy_arima["MPE"], "\n")
cat("MAPE:", accuracy_arima["MAPE"], "\n")
cat("MASE:", accuracy_arima["MASE"], "\n")
cat("ACF1:", accuracy_arima["ACF1"], "\n")



##       TESTING FOR HYPOTHESIS                         ####

# Filter the dataset for the selected regions
selected_regions <- c("Sub-Saharan Africa", "Europe", "East Asia")
filtered_data <- jobs[jobs$region %in% selected_regions, ]

# Create a density plot for the "employment_services" variable
ggplot(filtered_data, aes(x = employment_services, fill = region)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Employment Services for Three Regions",
       x = "Employment Services",
       y = "Density") +
  theme_minimal()

# Q-Q Plot Assuming 
jobs <- filtered_data$employment_services

ggplot(mapping = aes(sample=filtered_data$employment_services)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("sample")

# Extract data for Europe
europe_data <- filtered_data$employment_services[filtered_data$region == "Europe"]

# Q-Q Plot for Europe
ggplot(mapping = aes(sample = europe_data)) +
  stat_qq_point(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  labs(title = "Q-Q Plot for Europe", x = "Theoretical", y = "Sample")

# Extract data for Sub-Saharan
africa_data <- filtered_data$employment_services[filtered_data$region == "Sub-Saharan Africa"]

# Assuming 'africa_data' is the variable you want to plot
ggplot(mapping = aes(sample = africa_data)) +
  stat_qq_point(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  labs(title = "Q-Q Plot for Africa", x = "Theoretical", y = "Sample")

# Extract data for Europe
asia_data <- filtered_data$employment_services[filtered_data$region == "East Asia"]

# Q-Q Plot for Asia
ggplot(mapping = aes(sample = asia_data)) +
  stat_qq_point(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  labs(title = "Q-Q Plot for Asia", x = "Theoretical", y = "Sample")


# Importing required packages
library(dplyr)
library(tidyr)
library(stats)

# Creating a grouped dataset based on the 'region' variable
region <- jobs %>%
  group_by(region = case_when(
    region %in% c("Sub-Saharan Africa") ~ "Sub-Saharan Africa",
    region %in% c("Europe") ~ "Europe",
    region %in% c("East Asia") ~ "East Asia"
  )) %>%
  summarise(employment_services = mean(employment_services, na.rm = TRUE))

# Shapiro-Wilk Test on the original dataset
by(jobs$employment_services, jobs$region, shapiro.test)

# Shapiro-Wilk Test on the original dataset
shapiro_test_results <- by(jobs$employment_services, jobs$region, shapiro.test)

# Display the results
shapiro_test_results

# Log Transformation
jobs$log_employment_services <- log(jobs$employment_services)

# Square Root Transformation
jobs$sqrt_employment_services <- sqrt(jobs$employment_services)

# Cube Root Transformation
jobs$cbrt_employment_services <- sign(jobs$employment_services) * abs(jobs$employment_services)^(1/3)

# Shapiro-Wilk Test on Log-transformed data
by(jobs$log_employment_services, jobs$region, shapiro.test)

# Shapiro-Wilk Test on Square Root-transformed data
by(jobs$sqrt_employment_services, jobs$region, shapiro.test)

# Shapiro-Wilk Test on Cube Root-transformed data
by(jobs$cbrt_employment_services, jobs$region, shapiro.test)


# Kruskal-Wallis Test
kruskal_test_result <- kruskal.test(employment_services ~ region, data = jobs)

# Display the Kruskal-Wallis Test result
print(kruskal_test_result)

# Pairwise Wilcoxon Tests with Bonferroni correction
pairwise_wilcox_test_result <- pairwise.wilcox.test(
  jobs$employment_services, jobs$region,
  p.adjust.method = "bonferroni"
)

# Display the Pairwise Wilcoxon Tests result
print(pairwise_wilcox_test_result)

# Create a new dataset without Asia
jobs_no_asia <- jobs %>%
  filter(region != "East Asia")

# Mann-Whitney-Wilcoxon Test
mann_whitney_test_result <- wilcox.test(
  employment_services ~ region,
  data = jobs_no_asia,
  alternative = "two.sided"  # You can adjust the alternative hypothesis as needed
)

# Display the Mann-Whitney-Wilcoxon Test result
print(mann_whitney_test_result)





