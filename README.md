# Optimizing Hair Salon Marketing Resource Allocation Using Customer Traffic Forecast

This repository contains the reference code for Group 1's final project in the **Business Analytics Using Forecasting** course (2026).

> **Note:** Due to a non-disclosure agreement (NDA) with the company, the original dataset cannot be publicly shared. Please replace it with your own dataset and rename the file to `data.csv` before running the code.


## Data Preprocessing
1. Rename your dataset file to `data.csv`.
2. Aggregate the data to the monthly level by branches.
3. Impute the missing value by average value.

## Model Selection
We use several forecasting models and compare their predictive performance. Each model is implemented in a separate file named *forecast_\<method\>.R*.

For example, the naive forecasting model is saved as `forecast_naive.R`.

The forecasting models used in this project are:

- Naive
- ETS (Exponential Smoothing)
- TSLM (Linear Regression)
- ARIMA / ARMA


## Best Model Forecast
The script `best_model.R` demonstrates how to generate future forecasts using the selected best model.

Please note that the provided code only includes an example for a single branch. To generate future forecasts for other branches, modify the model type and corresponding parameters according to the best-performing model identified for each branch during the model selection stage.
