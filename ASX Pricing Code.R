# Forecasting Assignment - 1

library(readr)
library(TSA)
library(timeSeries)
library(forecast)
library(urca)
library(tseries)
library(moments)
library(ggplot2)
library(dynlm)
library(lmtest)

data = read.csv("C:/Users/Sherodhi/Downloads/ASX_data.csv")
class(data)

# Cleaning the data

clean_data <- function(df) {
  # Clean column names
  colnames(df) <- c("ASX_price", "Gold_price", "Oil_price", "Copper_price")
  
  # Remove commas and quotes, convert to numeric
  df$Gold_price <- as.numeric(gsub("[,\"]", "", df$Gold_price))
  df$Copper_price <- as.numeric(gsub("[,\"]", "", df$Copper_price))
  df$Oil_price <- as.numeric(df$Oil_price)
  df$ASX_price <- as.numeric(df$ASX_price)
  
  return(df)
}

data_clean <- clean_data(data)

#Adding dates column by creating time sequence from Jan 2004, since it doesn't exist in the given data

dates <- seq(as.Date("2004-01-01"), by = "month", length.out = nrow(data_clean))

# Attaching that column to data

data_clean$Date <- dates
head(data_clean)
class(data_clean)

#Hence it's data frame, we convert it to time series
# Here I excluded the date column because ts() function does not require or use actual dates. 
# It just needs start and frequency
ts_data <- ts(data_clean[, c("ASX_price", "Gold_price", "Oil_price", "Copper_price")],
              start = c(2004, 1),
              frequency = 12)

#===========================================================================================
# Descriptve Analysis
#===========================================================================================

library(moments)

descriptive_stats <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  
  stats <- data.frame(
    Variable = colnames(numeric_data),
    Min = apply(numeric_data, 2, min, na.rm = TRUE),
    Q1 = apply(numeric_data, 2, quantile, probs = 0.25, na.rm = TRUE),
    Median = apply(numeric_data, 2, median, na.rm = TRUE),
    Mean = apply(numeric_data, 2, mean, na.rm = TRUE),
    Q3 = apply(numeric_data, 2, quantile, probs = 0.75, na.rm = TRUE),
    Max = apply(numeric_data, 2, max, na.rm = TRUE),
    SD = apply(numeric_data, 2, sd, na.rm = TRUE),
    Skewness = apply(numeric_data, 2, function(x) skewness(x, na.rm = TRUE)),
    Kurtosis = apply(numeric_data, 2, function(x) kurtosis(x, na.rm = TRUE))
  )
  
  # Round numeric columns only
  stats[, 2:ncol(stats)] <- round(stats[, 2:ncol(stats)], 3)
  
  return(stats)
}

# Run
desc_stats <- descriptive_stats(data_clean)
print(desc_stats)


# Making a ts plot

#Plotting the time series for each column separately

par(mfrow = c(2,2))  # 2x2 layout
plot(ts_data[,"ASX_price"], main="ASX Price", col="blue", ylab="Price", xlab="Time")
plot(ts_data[,"Gold_price"], main="Gold Price", col="gold", ylab="Price", xlab="Time")
plot(ts_data[,"Oil_price"], main="Oil Price", col="black", ylab="Price", xlab="Time")
plot(ts_data[,"Copper_price"], main="Copper Price", col="red", ylab="Price", xlab="Time")
par(mfrow=c(1,1))  # reset



# Scaling the data to plot all the columns together
ts_scaled <- scale(ts_data)

# Set reasonable margins
par(mar = c(5, 4, 4, 4))  # bottom, left, top, right

# Plot the scaled series
plot(ts_scaled, plot.type = "s", col = c("blue", "gold", "black", "red"),
     main = "Scaled Time Series of ASX, Gold, Oil, Copper",
     ylab = "Scaled Value", xlab = "Time")

# Add a non-overlapping legend in bottom right with smaller text
legend("bottomright",
       legend = colnames(ts_data),
       col = c("blue", "gold", "black", "red"),
       lty = 1,
       cex = 0.75,  # shrink text
       bty = "n",   # remove box border
       inset = 0.02)  # slight inward offset



#correlation
cor(ts_data)

#=======================================================================================
#STATIONARY ANALYSIS
#======================================================================================

#Checking the stationarity by doing several tests on every column

par(mfrow= c(1,2)) # For the plots to be next to each other

# Checking for ASX Price
acf(ts_data[,"ASX_price"], main = "ACF of ASX Price")
pacf(ts_data[,"ASX_price"], main = "PACF of ASX Price")

# ADF test for ASX Price
adf_asx <- ur.df(ts_data[,"ASX_price"], type = "none", selectlags = "AIC")
summary(adf_asx)

# PP test for ASX Price
pp_asx <- ur.pp(ts_data[,"ASX_price"], type = "Z-tau", lags = "short")
summary(pp_asx)

# Checking for Gold Price
acf(ts_data[,"Gold_price"], main = "ACF of Gold Price")
pacf(ts_data[,"Gold_price"], main = "PACF of Gold Price")

# ADF test for Gold Price
adf_gold <- ur.df(ts_data[,"Gold_price"], type = "none", selectlags = "AIC")
summary(adf_gold)

# PP test for Gold Price
pp_gold <- ur.pp(ts_data[,"Gold_price"], type = "Z-tau", lags = "short")
summary(pp_gold)

# Checking for Oil Price
acf(ts_data[,"Oil_price"], main = "ACF of Oil Price")
pacf(ts_data[,"Oil_price"], main = "PACF of Oil Price")

# ADF test for Oil Price
adf_oil <- ur.df(ts_data[,"Oil_price"], type = "none", selectlags = "AIC")
summary(adf_oil)

# PP test for Oil Price
pp_oil <- ur.pp(ts_data[,"Oil_price"], type = "Z-tau", lags = "short")
summary(pp_oil)

# Checking for Copper Price
acf(ts_data[,"Copper_price"], main = "ACF of Copper Price")
pacf(ts_data[,"Copper_price"], main = "PACF of Copper Price")

# ADF test for Copper Price
adf_copper <- ur.df(ts_data[,"Copper_price"], type = "none", selectlags = "AIC")
summary(adf_copper)

# PP test for Copper Price
pp_copper <- ur.pp(ts_data[,"Copper_price"], type = "Z-tau", lags = "short")
summary(pp_copper)

# Box-Cox analysis for all variables
# get lambdas for each variable
lambda_asx    <- BoxCox.lambda(ts_data[,"ASX_price"])
lambda_gold   <- BoxCox.lambda(ts_data[,"Gold_price"])
lambda_oil    <- BoxCox.lambda(ts_data[,"Oil_price"])
lambda_copper <- BoxCox.lambda(ts_data[,"Copper_price"])

# print them
lambda_asx
lambda_gold
lambda_oil
lambda_copper

# Apply log transform only to Oil_price
ts_data[,"Oil_price"] <- log(ts_data[,"Oil_price"])

# Check transformation visually
par(mfrow = c(2,1))
plot(ts_data[,"Oil_price"], main = "Oil Price (Transformed: log)", ylab = "log(Oil)")
plot(diff(ts_data[,"Oil_price"]), main = "Differenced Log Oil Price", ylab = "Diff log(Oil)")
par(mfrow = c(1,1))

# ADF test on log(Oil_price)
adf_oil <- adf.test(ts_data[,"Oil_price"])

print(adf_oil)

# Take log first
log_oil <- log(ts_data[,"Oil_price"])

# First differencing
diff_log_oil <- diff(log_oil)

# ADF test on differenced log series
adf_oil_diff <- adf.test(diff_log_oil)
print(adf_oil_diff)

# Quick plot to check
plot(diff_log_oil, main="Differenced log(Oil Price)", ylab="Diff log(Oil)", xlab="Time")

library(urca)

# Differencing each series
asx_diff    <- diff(ts_data[, "ASX_price"])
gold_diff   <- diff(ts_data[, "Gold_price"])
oil_diff    <- diff(ts_data[, "Oil_price"])
copper_diff <- diff(ts_data[, "Copper_price"])

# ADF tests on differenced series (no constant or trend)
adf_asx_diff    <- ur.df(asx_diff, type = "none", selectlags = "AIC")
adf_gold_diff   <- ur.df(gold_diff, type = "none", selectlags = "AIC")
adf_oil_diff    <- ur.df(oil_diff, type = "none", selectlags = "AIC")
adf_copper_diff <- ur.df(copper_diff, type = "none", selectlags = "AIC")

summary(adf_asx_diff)
summary(adf_gold_diff)
summary(adf_oil_diff)
summary(adf_copper_diff)



#############################################################################################
# Decomposition
#############################################################################################

# STL decomposition for each variable

# ASX Price
asx_decom <- stl(ts_data[,"ASX_price"], t.window=15, s.window="periodic", robust=TRUE)
plot(asx_decom, main="STL Decomposition: ASX Price")

# Gold Price
gold_decom <- stl(ts_data[,"Gold_price"], t.window=15, s.window="periodic", robust=TRUE)
plot(gold_decom, main="STL Decomposition: Gold Price")

# Oil Price
oil_decom <- stl(ts_data[,"Oil_price"], t.window=15, s.window="periodic", robust=TRUE)
plot(oil_decom, main="STL Decomposition: Oil Price")

# Copper Price
copper_decom <- stl(ts_data[,"Copper_price"], t.window=15, s.window="periodic", robust=TRUE)
plot(copper_decom, main="STL Decomposition: Copper Price")


##########################################################
# Modelling
##########################################################
library(dynlm)
library(dLagM)
# Combine differenced data into a data frame
data_d <- data.frame(
  ASX    = asx_diff,
  Gold   = gold_diff,
  Oil    = oil_diff,
  Copper = copper_diff
)

# Individual DLM models for each commodity
model_gold <- dlm(x = as.vector(data_d$Gold),
                  y = as.vector(data_d$ASX),
                  q = 10)

model_oil <- dlm(x = as.vector(data_d$Oil),
                 y = as.vector(data_d$ASX),
                 q = 9)

model_copper <- dlm(x = as.vector(data_d$Copper),
                    y = as.vector(data_d$ASX),
                    q = 10)

# View summaries
summary(model_gold)
summary(model_oil)
summary(model_copper)

# Create empty results data frame
results <- data.frame(
  Model = character(),
  q     = integer(),
  k     = integer(),
  AIC   = numeric(),
  BIC   = numeric(),
  stringsAsFactors = FALSE
)

# Loop over q, k, and each commodity
for (q in 1:3) {
  for (k in 1:6) {
    for (var in c("Gold", "Oil", "Copper")) {
      tryCatch({
        model <- polyDlm(
          x = as.vector(data_d[[var]]),  # use your data_d
          y = as.vector(data_d$ASX),
          q = q,
          k = k
        )
        
        results <- rbind(results, data.frame(
          Model = var,
          q     = q,
          k     = k,
          AIC   = AIC(model),
          BIC   = BIC(model)
        ))
      }, error = function(e) NULL)
    }
  }
}

print(results)

gold_model_poly_custom <- polyDlm(
  x = as.vector(data_d$Gold),
  y = as.vector(data_d$ASX),
  q = 3,
  k = 1
)

Oil_model_poly_custom <- polyDlm(
  x = as.vector(data_d$Oil),
  y = as.vector(data_d$ASX),
  q = 2,
  k = 1
)

Copper_model_poly_custom <- polyDlm(
  x = as.vector(data_d$Copper),
  y = as.vector(data_d$ASX),
  q = 2,
  k = 1
)

summary(gold_model_poly_custom)
summary(Oil_model_poly_custom)
summary(Copper_model_poly_custom)

# Koyck DLM models with unique names
gold_koyck_model_custom   <- koyckDlm(x = as.vector(data_d$Gold),
                                      y = as.vector(data_d$ASX))

oil_koyck_model_custom    <- koyckDlm(x = as.vector(data_d$Oil),
                                      y = as.vector(data_d$ASX))

copper_koyck_model_custom <- koyckDlm(x = as.vector(data_d$Copper),
                                      y = as.vector(data_d$ASX))

# View summary of gold model
summary(gold_koyck_model_custom)
summary(oil_koyck_model_custom)
summary(copper_koyck_model_custom)

# Separate ARDL models for each commodity with unique names
library(dLagM)
gold_ardl_model_custom   <- dLagM::ardlDlm(x = as.vector(data_d$Gold),
                                    y = as.vector(data_d$ASX),
                                    p = 1, q = 1)

oil_ardl_model_custom    <- dLagM::ardlDlm(x = as.vector(data_d$Oil),
                                    y = as.vector(data_d$ASX),
                                    p = 1, q = 1)

copper_ardl_model_custom <- dLagM::ardlDlm(x = as.vector(data_d$Copper),
                                    y = as.vector(data_d$ASX),
                                    p = 1, q = 1)

# View summary of gold model
summary(gold_ardl_model_custom)
summary(oil_ardl_model_custom )
summary(copper_ardl_model_custom)




# Combine all models into a named list
models <- list(
  gold_poly  = gold_model_poly_custom,
  oil_poly   = Oil_model_poly_custom,
  copper_poly = Copper_model_poly_custom,
  gold_koyck = gold_koyck_model_custom,
  oil_koyck  = oil_koyck_model_custom,
  copper_koyck = copper_koyck_model_custom,
  gold_ardl  = gold_ardl_model_custom,
  oil_ardl   = oil_ardl_model_custom,
  copper_ardl = copper_ardl_model_custom
)

# Compute AIC and BIC
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)

# Create a results table
model_table <- data.frame(
  Model = names(models),
  AIC = aic_values,
  BIC = bic_values
)

# Print nicely
print(model_table)

# sort by AIC
model_table_sorted <- model_table[order(model_table$AIC), ]
print(model_table_sorted)

library(dLagM)
library(forecast)  # for checkresiduals

# Loop over q = 1 to 10 for Gold
cat("=== Gold DLM ===\n")
for (i in 1:10){
  model_gold <- dlm(x = as.vector(data_d$Gold), y = as.vector(data_d$ASX), q = i)
  cat("q =", i, "AIC =", AIC(model_gold), "BIC =", BIC(model_gold), "\n")
}

# Loop over q = 1 to 10 for Oil
cat("\n=== Oil DLM ===\n")
for (i in 1:10){
  model_oil <- dlm(x = as.vector(data_d$Oil), y = as.vector(data_d$ASX), q = i)
  cat("q =", i, "AIC =", AIC(model_oil), "BIC =", BIC(model_oil), "\n")
}

# Loop over q = 1 to 10 for Copper
cat("\n=== Copper DLM ===\n")
for (i in 1:10){
  model_copper <- dlm(x = as.vector(data_d$Copper), y = as.vector(data_d$ASX), q = i)
  cat("q =", i, "AIC =", AIC(model_copper), "BIC =", BIC(model_copper), "\n")
}

checkresiduals(model_gold, main = "Residuals of Gold DLM")
checkresiduals(model_oil, main = "Residuals of Oil DLM")
checkresiduals(model_copper, main = "Residuals of Copper DLM")









