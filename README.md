# sohn

Miscellaneous R functions for papers & blogposts by Uri Simonsohn.

## Installation

```r
# Install from GitHub with `groundhog` (for version control)
groundhog::groundhog.library("urisohn/sohn", date)   #date is a date to load with version control 

# Or install from GitHub with `devtools`
devtools::install_github("urisohn/sohn")
```

## Overview

Common data visualization and formatting I have used in papers & Data Colada blog posts.
Created with help from Cursor (AI)

## Functions

Functions are organized into the following categories. Click on a category to expand and see available functions:

<details>
<summary><b>📊 Graphing</b></summary>

- `cdf.by()`: CDF plots for multiple variables in one graph
- `fhist()`: Frequency distribution without binning
- `scatter.gam()`: Scatter plots with GAM smooth lines

</details>

<details>
<summary><b>📈 Statistical Analyses</b></summary>

- `lmr()`: Linear models with robust standard errors

</details>

<details>
<summary><b>✨ Formatting</b></summary>

- `format.pvalue()`: Format p-values for clean display in figures and tables
- `message.col()`: Print colored messages to console

</details>

<details>
<summary><b>🔄 Simulations</b></summary>

- `counter()`: Adaptive progress reporter for simulations

</details>

<details>
<summary><b>🗂️ Data Management</b></summary>

- `namedList()`: Create lists with automatic naming from variable names
- `convert_to_sql()`: Convert CSV files to SQL INSERT statements

</details>



## Dependencies

- `mgcv` (for `scatter.gam()`)
- `sandwich` (for `lmr()`)

## Author

**Uri Simonsohn**  
Email: urisohn@gmail.com

## License

GPL-3

## Version

0.1.1

