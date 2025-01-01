# Effect of covid on US flights 


## Overview
This project analyzes the impact of COVID-19 on flight volumes across multiple airlines, focusing on seasonal trends, holiday travel, and the distribution of flight distances. Using the CITIES_4 dataset, the study incorporates various visualization and statistical techniques to reveal how flight patterns were influenced during the pandemic.


## Analysis
- **Overall Flight Volume Analysis**:
Visualize changes in monthly flight volumes across carriers before, during, and after the peak of the COVID-19 pandemic.
- **Holiday Travel Analysis**:
Investigate flight volumes during Thanksgiving and Christmas holiday periods to assess seasonal changes in travel behavior.
- **Flight Distance Analysis**:
Compare the impact on short, medium, and long-distance flights across different carriers.
- **Carrier-Specific Insights**:
Provide a breakdown of flight volume trends and variations for each carrier.

## Methods
**Data Preprocessing**
  - The make_datetime_100 function was used to convert year, month, day, and time fields into datetime objects.
  - Data was filtered for valid departure and arrival times to ensure accuracy.
**Time Series Visualization**:
  - Examined monthly and holiday-specific flight volumes.
  - Compared trends across years and carriers.
**Holiday Analysis**:
- Focused on travel weeks (Thanksgiving: weeks 47-48; Christmas: weeks 49-52).
- Analyzed carrier-specific flight patterns during holiday periods.


## Tools and Libraries
R Packages:
- tidyverse, lubridate, gridExtra, anyflights, tidyr, ggplot2.




