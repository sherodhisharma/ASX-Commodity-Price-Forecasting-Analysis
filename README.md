# ASX-Commodity-Price-Forecasting-Analysis

A time series forecasting and econometric modelling project analysing the relationship between the Australian Stock Exchange (ASX All Ordinaries Index) and major commodity prices including Gold, Oil, and Copper.

This project applies statistical forecasting techniques, stationarity testing, decomposition methods, and distributed lag models using R.

---

## Project Overview

This project investigates how commodity price movements influence the ASX All Ordinaries Price Index using historical monthly financial data from 2004 onwards.

The analysis focuses on:

- Detecting and handling non-stationarity in financial time series
- Exploring trends, seasonality, and volatility
- Applying forecasting and decomposition techniques
- Building distributed lag models to explain ASX price movements
- Comparing econometric models using AIC and BIC criteria

This project was completed as part of the **MATH1307 Forecasting** course at RMIT University.

---

## Dataset

The dataset contains monthly observations for:

- ASX All Ordinaries Price Index
- Gold Price (AUD)
- Crude Oil Price (Brent USD/bbl)
- Copper Price (USD/tonne)

### Time Period
January 2004 – 2017

---

## Tools & Technologies

- R Programming
- Time Series Analysis
- Econometric Modelling
- Forecasting Techniques
- Statistical Testing

### Libraries Used

```r
forecast
tseries
urca
dLagM
ggplot2
TSA
lmtest
```

---

## Key Analysis Performed

### 1. Data Cleaning & Preparation

- Renamed variables for clarity
- Converted non-numeric values into numeric format
- Created multivariate time series objects
- Applied transformations where required

### 2. Exploratory Data Analysis

- Summary statistics
- Correlation analysis
- Time series visualisation
- Scaled comparison plots

### 3. Stationarity Testing

Performed:

- Augmented Dickey-Fuller (ADF) Test
- Phillips-Perron (PP) Test
- ACF/PACF Analysis

#### Findings

- All original series were non-stationary
- First differencing successfully achieved stationarity

### 4. Box-Cox Transformation

- Applied variance stabilisation techniques
- Log transformation applied to Oil prices due to volatility

### 5. STL Decomposition

Each series was decomposed into:

- Trend
- Seasonality
- Residuals

#### Key Finding

- Strong trend behaviour observed across all financial series
- Seasonal effects existed but were relatively weak

### 6. Distributed Lag Modelling

Implemented:

- Standard Distributed Lag Models (DLM)
- Polynomial Distributed Lag Models
- Koyck Distributed Lag Models
- ARDL Models

---

## Key Findings

- Commodity prices showed moderate positive correlation with the ASX index
- Copper prices demonstrated the strongest explanatory relationship with ASX movements

### Best Performing Model

**Copper Polynomial Distributed Lag Model**

- q = 3
- k = 1

### Model Performance

- Lowest AIC: **2103.677**
- Lowest BIC: **2115.902**

Despite being the best-performing model, the explanatory power remained relatively low (~10%), suggesting that broader macroeconomic and financial factors also influence ASX market behaviour significantly.

---

## Visualisations Included

The project includes:

- Time series trend plots
- Scaled comparison charts
- ACF/PACF plots
- STL decomposition visualisations
- Residual diagnostic plots

---


## Learning Outcomes

Through this project, I developed practical experience in:

- Financial time series analysis
- Forecasting methodology
- Econometric modelling
- Statistical testing
- Model comparison and diagnostics
- Data transformation techniques
- Interpreting financial market behaviour

---

## Future Improvements

Potential extensions for this project include:

- ARIMA/SARIMA forecasting
- VAR (Vector Autoregression) models
- Machine Learning forecasting approaches
- Inclusion of macroeconomic indicators
- Out-of-sample forecasting evaluation

---
