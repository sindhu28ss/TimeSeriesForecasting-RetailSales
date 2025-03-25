# TimeSeriesForecasting-RetailSales

<p align="center">
  <img src="https://github.com/sindhu28ss/TimeSeriesForecasting-RetailSales/blob/main/images/image.webp" width="400">
</p>

Understanding sales patterns in the retail and food services sector is critical for strategic planning, especially when influenced by seasonality, long-term growth trends, and unforeseen disruptions like the pandemic. This project leverages historical monthly sales data from the U.S. Census Bureau (1992–2023) to build and compare multiple time series forecasting models.

The primary objective is to uncover temporal patterns and generate accurate forecasts that can aid businesses, analysts, and policymakers in making informed, data-driven decisions. Models range from classical regression-based approaches to advanced Auto ARIMA, enabling both interpretability and precision in capturing real-world sales dynamics.

## 🔄 Forecasting Workflow

This project follows a structured eight-step forecasting process:

### 1️⃣ Define the Goal  
Forecast monthly U.S. retail and food services sales by modeling trend and seasonality in historical data. The most accurate model will be selected, with monthly updates ensuring continued performance. 

### 2️⃣ Get the Data
- **Source:** [U.S. Census Bureau - Monthly Retail Trade Survey (MRTS)](https://www.census.gov/econ/currentdata/dbsearch?program=MRTS)
- **Period:** January 1992 – December 2023
- **Frequency:** Monthly
- **Units:** Sales in Millions of USD
- **Target Variable:** Retail and Food Services Sales (adjusted)

### 3️⃣ Explore & Visualize
- Sales show a strong upward trend and clear seasonality, with lows at the start of each year and peaks toward year-end. A sharp dip in 2020 reflects the impact of COVID-19.

<p align="left">
  <img src="https://github.com/sindhu28ss/TimeSeriesForecasting-RetailSales/blob/main/images/dataplot.png" width="400">
  <img src="https://github.com/sindhu28ss/TimeSeriesForecasting-RetailSales/blob/main/images/autocorr.png" width="400">
</p>

- Autocorrelation plots reveal significant lagged correlations—strongest at lag 1 and tapering off around lag 12—confirming both trend and seasonal components in the data.

### 4️⃣ Data Preprocessing
- Filtered relevant period (Jan 1992 – Dec 2023). Parsed `Period` and `Value` column and ensured proper datetime formatting and monthly frequency.

### 5️⃣ Partition the Series
- **Training Set:** Jan 1992 – Dec 2016 (300 records)  
- **Validation Set:** Jan 2017 – Dec 2023 (84 records)

### 6️⃣ Apply Forecasting Models
- **Model 1: Regression-Based**
  - Linear trend + seasonality  
  - Quadratic trend + seasonality
- **Model 2: Two-Level Forecasting**
  - Regression (Linear/Quadratic) + Trailing Moving Average
- **Model 3: Holt-Winters Additive**
- **Model 4: Auto ARIMA** (selected as the best-performing model)
<p align="left">
  <img src="https://github.com/sindhu28ss/TimeSeriesForecasting-RetailSales/blob/main/images/Auto%20arima.png" width="800">
</p>

### 7️⃣ Compare Performance
- Evaluated models using RMSE and MAPE on the validation set.
- **Auto ARIMA** achieved the lowest error, outperforming other models.

| Model                                                                 | RMSE      | MAPE   |
|-----------------------------------------------------------------------|-----------|--------|
| Linear Regression Model with Seasonality                              | 35049.51  | 6.211  |
| Quadratic Regression Model with Seasonality                           | 12287.83  | 2.477  |
| Two-Level: Linear Regression + Trailing Moving Average                | 11962.23  | 1.433  |
| Two-Level: Quadratic Regression + Trailing Moving Average             | 11558.28  | 1.481  |
| Holt-Winter Model                                                     | 8061.11   | 0.841  |
| **Auto ARIMA Model**                                                  | **7714.60** | **0.841** |
| Naive Model                                                           | 8076.42   | 0.920  |
| Seasonal Naive (SNaive) Model                                         | 28144.10  | 5.296  |


### 8️⃣ Implement Forecast
- Final forecasts were generated using **Auto ARIMA**, which emerged as the most accurate model based on RMSE and MAPE scores.
- Two-level models with trailing moving averages (both linear and quadratic) also performed well and serve as strong alternatives.
- The **Holt-Winters model** was not selected due to its limited ability to handle complex seasonal patterns in the data.
- Forecasts can be updated monthly, with a recommended **re-evaluation every 6 months** to ensure continued accuracy and relevance for future planning.

