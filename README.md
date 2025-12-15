# sohn

Miscellaneous R functions for papers, blogposts, & teaching by Uri Simonsohn.

## Installation

```r
# Install from GitHub with `groundhog` (for version control)
groundhog::groundhog.library("urisohn/sohn", date)   #date used for version control 

# Or install from GitHub with `devtools`
devtools::install_github("urisohn/sohn")
```

## Overview

Functions I often use and are not (sufficiently?) available in existing packages.

## Functions

### 📊 Graphing

<details>
<summary><code>plot_cdf()</code>: CDF for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
plot_cdf(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
plot_cdf(y, x, col = c("red", "blue"))  # Custom colors
```
</details>

<details>
<summary><code>plot_density()</code>: Density for multiple groups in one plot</summary>

```r
y <- rnorm(100)
x <- rep(c("A", "B"), 50)
plot_density(y, x)  # Uses default colors (red4, dodgerblue for 2 groups)
plot_density(y, x, col = c("red", "blue"))  # Custom colors
plot_density(y, x, show.means = FALSE)  # Hide mean segments
```
</details>

<details>
<summary><code>plot_freq()</code>: Frequency distribution without binning, with value labels</summary>

```r
x <- c(1, 1, 2, 2, 2, 5, 5)
plot_freq(x)

# Grouped frequency plot
df <- data.frame(value = c(1, 1, 2, 2, 2, 5, 5), group = c("A", "A", "A", "B", "B", "A", "B"))
plot_freq(value, by = group, data = df)

# Show percentages instead of frequencies
plot_freq(value, by = group, data = df, freq = FALSE)

# Customize legend and labels
plot_freq(value, by = group, data = df, legend.title = "Group", col.text = "black")
```
</details>

<details>
<summary><code>scatter.gam()</code>: Scatter plots with GAM smooth lines</summary>

```r
x <- rnorm(100)
y <- 2*x + rnorm(100)
scatter.gam(x, y)
```
</details>

<details>
<summary><code>text2()</code>: Adds to text(): align='center' , bg='yellow'</summary>

```r
plot(1:10, 1:10, type = "n")
   text2(2, 8, "Left", align = "left", bg = "lightblue")
   text2(5, 8, "Center", align = "center", bg = "lightgreen")
   text2(8, 8, "Right",    align = "right", bg = "lightyellow")
   text2(5, 5, "Red Text", col = "red", bg = "white")
```
</details>

<details>
<summary><code>resize_images()</code>: Saves any image (or all in folder) as PNG with set width.</summary>

```r
# Resize a single image file
   resize_images("path/to/image.svg", width = 800)

# Resize all images in a folder to 800px width
   resize_images("path/to/images", width = 800)

# Resize images to different widths
   resize_images("path/to/images", width = c(800, 1200, 600))
```
</details>

### 📈 Statistical Analyses

<details>
<summary><code>table2()</code>: Enhances base table(): (1) variable names are shown, (2) proportions are an option</summary>

```r
df <- data.frame(
  group = c("A", "A", "B", "B", "A"),
  status = c("X", "Y", "X", "Y", "X")
)

# table() does not show var names, table2() does
table (df$group, df$status)
table2(df$group, df$status)

# can report proportinos (building in prop.table() )
table2(df$group, df$status, prop = "all")    # Overall proportions
table2(df$group, df$status, prop = "row")    # Row proportions
table2(df$group, df$status, prop = "column") # Column proportions
```
</details>

<details>
<summary><code>desc_var()</code>: Descriptive statistics by group (or full dataset)</summary>

```r
# With grouping
df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
desc_var(y, group, data = df)

# Without grouping (full dataset)
desc_var(y, data = df)

# Direct vectors
y <- rnorm(100)
group <- rep(c("A", "B"), 50)
desc_var(y, group)

# With custom decimal places
desc_var(y, group, data = df, decimals = 2)
```
</details>

<details>
<summary><code>desc_var()</code>: Descriptive statistics by group (or full dataset)</summary>

```r
# With grouping
df <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
desc_var(y, group, data = df)

# Without grouping (full dataset)
desc_var(y, data = df)

# Direct vectors
y <- rnorm(100)
group <- rep(c("A", "B"), 50)
desc_var(y, group)

# With custom decimal places
desc_var(y, group, data = df, decimals = 2)
```
</details>

<details>
<summary><code>t.test2()</code>: Enhances base t.test: (1) console shows mean diff & var names, (2) output is dataframe, not list</summary>

```