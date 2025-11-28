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

Functions are organized into the following categories:

### Graphing

Functions for data visualization and plotting.

#### `scatter.gam()`

Creates a scatter plot with a GAM (Generalized Additive Model) smooth line, with options to display data points and three-way spline summary points.

**Example:**
```r
x <- rnorm(100)
y <- 2*x + rnorm(100)
scatter.gam(x, y, data.dots = TRUE, three.dots = TRUE)
```

#### `cdf.by()`

Plots CDFs as multiple lines in the same plot, plotting a given y by different values of x.

**Example:**
```r
y <- rnorm(100)
x <- rep(c("A", "B", "C"), c(30, 40, 30))
cdf.by(y, x, col = c("red", "green", "blue"), lwd = 2)
```

#### `fhist()`

Plots distribution of a variable.  
Unlike histograms, no binning.  
Unlike barplots, all possible values included in x-axis.

**Example:**
```r
x <- c(1, 1, 2, 2, 2, 5, 5)
fhist(x, col = "steelblue", xlab = "Value", ylab = "Frequency")
```

### Statistical Analyses

Functions for statistical modeling and result formatting.

#### `lmr()`

Fits linear models with robust standard errors. Supports both heteroskedasticity-robust (HC) and cluster-robust standard errors. The resulting model object is modified so that `predict()` uses robust standard errors for prediction intervals.

**Example:**
```r
data <- data.frame(x = rnorm(100), y = rnorm(100))
fit <- lmr(y ~ x, data = data)
summary(fit)
```

#### `format.pvalue()`

Formats p-values for clean display in figures and tables, adds p= or p<, and rounds to four decimal points.

**Example:**
```r
format.pvalue(c(0.05, 0.001, 0.00001))
# [1] "= .05"    "= .001"   "< .0001"

format.pvalue(0.05, include_p = TRUE)
# [1] "p = .05"
```

### Simulations

Functions for running and monitoring simulations.

#### `counter()`

Adaptive progress reporter for simulations. Automatically determines an optimal reporting interval based on execution time.

**Example:**
```r
report <- counter()
for (i in 1:1000) {
  # Your simulation code here
  report(i)  # Reports progress adaptively
}
```

### Data Management

Functions for data manipulation and utilities.

#### `namedList()`

Creates a list where objects are automatically named based on their variable names.  
The existing name is a default, you can also set new names with it.

**Example:**
```r
x <- 1:5
y <- letters[1:3]
z_MAT <- matrix(1:4, nrow = 2)

my_list <- namedList(x, y, z=z_MAT)
names(my_list)  # "x" "y" "z"
```

#### `convert_to_sql()`

Converts CSV files to SQL INSERT statements. Optionally generates CREATE TABLE statements.

**Example:**
```r
convert_to_sql("data.csv", "data.sql", create_table = TRUE)
```

#### `message.col()`

Prints colored messages to the console with optional bold formatting.

**Example:**
```r
message.col("This is a colored message", col = "cyan", font = 2)
```



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

