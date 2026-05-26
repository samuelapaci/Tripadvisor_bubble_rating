# Effect of Bubble Rating on Hotel Views: A Matching Analysis

This repository contains a Causal Inference and Impact Evaluation project focused on estimating the causal impact of having a high TripAdvisor bubble rating on the number of monthly hotel views. 

Using observational data from hotels in Rome, the study implements **Propensity Score Matching (PSM)** and **Mahalanobis Distance Matching** to control for confounding variables (such as hotel class, price, and number of reviews) and isolate the true treatment effect.

This project was developed for the *Causal Inference and Impact Evaluation* course within the Master’s degree program in *Analytics and Data Science for Economics and Management* (A.Y. 2023/2024).

## Authors
* Isabella Cappiello
* Marie Doka
* Samuela Paci
* Martina Pagan

## Project Structure & Files
* **`GW3_casual_inference.R`**: The core R script containing data pre-processing, covariate adjustment, implementation of matching algorithms (`MatchIt` library), balance checks (`cobalt`, `tableone`), and treatment effect estimation.
* **`GW3.pptx`**: Presentation slide deck summarizing the research question, matching methodology, empirical results, and study limitations.
* **`data_rdd_finito.xlsx`**: Main dataset containing metrics for 4,599 hotels collected in Rome (December 2019). It includes:
  * `data_rdd.csv` / `Foglio1.csv`: Formatted datasets used for empirical analysis.
  * `Metadata.csv`: Data dictionary explaining variables like `score_adjusted`, `bubble_rating`, `views`, `location_grade`, and specific amenities.

## Tech Stack & Tools
* **Programming Language:** R
* **Key Packages:** `MatchIt` (matching methods), `cobalt` (balance plots), `dplyr` (data manipulation), `ggplot2` (data visualization), `tableone` (descriptive tables), `sandwich` (robust standard errors).
* **Data Sourcing:** Web-scraped TripAdvisor metrics for hotels in Rome, Italy (Snapshot: December 2019).

## Methodology & Key Results
1. **Treatment Definition:**
   Hotels are assigned to the treatment group ($T=1$) if their continuous adjusted score is strictly greater than 4 (`score_adjusted > 4`), which rounds up to a high TripAdvisor bubble rating.
2. **Confounder Control:**
   To balance the treated and control groups, the model matches units based on key covariates: hotel star class (grouped into Low, Mid, and High Stars), location grade walking distance score, total number of reviews, average prices, active discounts, and platform awards (Travellers' Choice, GreenLeaders, Certificate of Excellence).
3. **Key Findings:**
   The post-matching linear regression and t-test analyses indicate that the differences in monthly page views between the balanced treated and control groups are not statistically significant. This suggests that a half-bubble rating change does not drastically alter user visibility on its own, aligning with existing literature (e.g., Pokryshevskaya & Antipov, 2020). This is likely due to TripAdvisor's extensive multi-attribute filtering system (price, specific amenities, location), which reduces consumer reliance solely on the macro bubble rating.
4. **Limitations Addressed:**
   * High zero-inflation in the outcome variable (`views`).
   * Seasonality bias (data restricted to December).
   * Potential omitted variable bias from unobserved customer search behaviors and cross-platform competition (Booking.com, Google Travel).
