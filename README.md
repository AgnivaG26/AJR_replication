---
title: 'Replication of Acemoglu, Johnson, and Robinson (2001)'
created: '2025-08-15T14:16:39.512Z'
modified: '2025-08-15T14:18:36.595Z'
---

# Replication of Acemoglu, Johnson, and Robinson (2001)

[![Status: Complete](https://img.shields.io/badge/status-complete-success)](#)
[![Made with R](https://img.shields.io/badge/Made%20with-R-1f425e.svg)](https://www.r-project.org/)

## 1. Project Overview

This repository contains a complete replication of the seminal 2001 paper by Daron Acemoglu, Simon Johnson, and James A. Robinson, **"The Colonial Origins of Comparative Development: An Empirical Investigation"** (AER). This project was undertaken to demonstrate proficiency in R, data analysis, and the replication of complex econometric research.

The analysis is presented in a comprehensive R Markdown report, which includes detailed interpretations of each of the paper's eight tables.

---
## 2. Final Report

The final, fully-rendered HTML report, including all tables and analysis, is published on RPubs and can be viewed here:

[**View the Complete Replication Report on RPubs**](https://rpubs.com/AgnivaG26/1336430)

---
## 3. Research Question and Identification Strategy

The paper addresses a fundamental question in development economics: **Do institutions have a causal effect on economic prosperity?**

To overcome the endogeneity inherent in the relationship between institutions and income, the authors employ an **Instrumental Variable (IV)** strategy. The instrument must satisfy two conditions:

* **Relevance:** It must be a strong predictor of current institutions.
* **Exclusion Restriction:** It must affect current income *only* through its effect on institutions.

The proposed instrument is the **potential mortality rates of European settlers** during the colonial period. The causal chain is theorized as follows:

$$
\text{Settler Mortality} \rightarrow \text{Settlements} \rightarrow \text{Early Institutions} \rightarrow \text{Current Institutions} \rightarrow \text{Current Economic Performance}
$$

This replication empirically validates each link in this chain and confirms the paper's main findings.

---
## 4. Repository Structure

This repository is organized as follows:

* `AJR.Rmd`: The main R Markdown file containing all the code, analysis, and text for the final report.
* `/data/`: A directory containing all the original STATA datasets (`.dta`) required to run the replication (e.g., `maketable1.dta`, `maketable2.dta`, etc.).
* `/scripts/`: A directory containing the individual, standalone R scripts for generating each of the eight tables.
* `README.md`: This file, providing an overview of the project.

---
## 5. How to Replicate

To replicate this analysis on your own machine, follow these steps:

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/AgnivaG26/AJR-replication.git
    cd AgnivaG26
    ```

2.  **Install R Packages:**
    Open R or RStudio and run the following command to ensure you have all the necessary packages.

    ```r
    install.packages(c("haven", "dplyr", "fixest", "modelsummary", "gt", "purrr", "broom", "tidyr", "kableExtra", "rmdformats"))
    ```

3.  **Set the Data Path:**
    All the R scripts and the `.Rmd` file read data from the `/data/` directory. Ensure your working directory is set to the project's root folder in RStudio, or adjust the file paths in the scripts as needed. All paths are currently set to: `D:/replication-ajr/`.

4.  **Run the Analysis:**
    The easiest way to see all results is to open the `ajr_replication_report.Rmd` file in RStudio and click the **"Knit"** button. This will generate a self-contained HTML file with all the tables and analysis.

---
## 6. Tools and Packages Used

* **Language:** R
* **Data Manipulation:** `dplyr`, `tidyr`
* **Data Input:** `haven` (for `.dta` files)
* **Econometric Modeling:** `fixest` (for high-performance OLS and 2SLS)
* **Table Generation:** `modelsummary`, `gt`, `kableExtra`
* **Reporting:** `knitr`, `rmarkdown`, `rmdformats`

---
## 7. Original Paper Reference

Acemoglu, D., Johnson, S., & Robinson, J. A. (2001). The Colonial Origins of Comparative Development: An Empirical Investigation. *American Economic Review*, 91(5), 1369–1401.
