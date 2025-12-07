# Bayesian Modeling of U.S. Airline Arrival Delays
This project uses Bayesian linear regression to analyze whether winter months (December–February) lead to longer arrival delays in U.S. commercial flights. The analysis is based on a Kaggle dataset containing flight performance records from 2018–2024.

## Research Question
Do winter months cause longer arrival delays?

Answer: No.  
Winter delays are not meaningfully different from non-winter delays.
The estimated difference is ≈ 1 minute, which is extremely small compared to the ≈ 40 minutes of natural variability in individual flight delays.

---

## Key Insight
Airline choice matters more than the season.

Example:  
Switching from American → Southwest is associated with about 7 minutes shorter average delays.

---

## Methods
- Bayesian Linear Regression  
- Priors on intercept and coefficients  
- Posterior predictive simulations  
- Model validation via posterior predictive checks

Tools used: R, `brms`, `tidyverse`, `ggplot2`

---

## How to Run
1. Clone this repository
   ```bash
   git clone https://github.com/ryanbrush30/Linear-Regression-R-code---U.S.-Airline-Delays
   cd bayesian-airline-delays
Install R packages (once)

In R or RStudio:
r
Copy code
install.packages(c("tidyverse", "brms"))

# ggplot2 is included in tidyverse
Download the dataset from Kaggle
Dataset: Flight Delay Dataset 2018–2024
URL: https://www.kaggle.com/datasets/shubhamsingh42/flight-delay-dataset-2018-2024

Save the CSV as:
data/flight_data_2018_2024.csv

Run the analysis script

In R or RStudio:
r
Copy code
source("R/bayesian_flight_delay_model.R")

This will:
Load and clean the data
Fit the Bayesian regression model
Produce tables and figures used in the report
View the report
Open the final report in:
report/Bayesian_Linear_Regression_US_Airline_Arrival_Delays.pdf
orreport/Bayesian_Linear_Regression_US_Airline_Arrival_Delays.docx

Files in This Repository
File/Folder	Description
R
R script for data processing, modeling, and visualizationreport	
Final written project report (PDF 
DOCX)figures	
Model and data visualizationsdata
(Optional) Location to place the Kaggle CSV file
README.md	Project summary (this file)

Data Source
Flight Delay Dataset 2018–2024
Kaggle: https://www.kaggle.com/datasets/shubhamsingh42/flight-delay-dataset-2018-2024

Author
Ryan Brush
Data Scientist
LinkedIn: https://www.linkedin.com/in/ryancbrush
