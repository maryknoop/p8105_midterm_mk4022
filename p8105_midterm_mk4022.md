p8105\_midterm\_mk4022
================
Mary Knoop
10/24/2019

Load librarires for
    analysis

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(readxl)
```

Problem 1 Import and clean the data. Format the data to use appropriate
variable names; fill in missing values with data where appropriate (as
indicated in the header information); create character and ordered
factors for categorical variables.

``` r
posture_data = 
  read_excel("/Users/maryknoop/Desktop/Data_Sci/p8105_midterm_mk4022/data/p8105_mtp_data.xlsx", skip = 8) %>%
  janitor::clean_names() %>%
  mutate(
    eop_size_mm = replace_na(eop_size_mm, 0),
    eop_size = factor(eop_size, c("0", "1", "2", "3", "4", "5"), c("0-5", "5-10", "10-15", "15-20", "20-25", "25+"), ordered = TRUE), 
    sex = factor(sex, c(0, 1), c("female", "male")),
    fhp_category = factor(fhp_category,c(0, 1, 2, 3, 4, 5, 6, 7),c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"), ordered = TRUE),
    age_group = factor(age_group, c("2", "3", "4", "5", "6", "7", "8"), c("18-30", "31-40", "41-50", "51-60", "60+", "60+", "60+"), ordered = TRUE)) %>%
    filter(age_group != "1")

posture_data
```

    ## # A tibble: 1,219 x 9
    ##    sex     age age_group eop_size_mm eop_size eop_visibility_… eop_shape
    ##    <fct> <dbl> <ord>           <dbl> <ord>               <dbl>     <dbl>
    ##  1 male     18 18-30            14.8 10-15                   2         3
    ##  2 male     26 18-30            17.1 15-20                   2         3
    ##  3 fema…    22 18-30             0   0-5                     1        NA
    ##  4 male     20 18-30            23.9 20-25                   2         1
    ##  5 fema…    27 18-30             0   0-5                     0        NA
    ##  6 fema…    28 18-30             7.9 5-10                    2         2
    ##  7 male     21 18-30             9.4 5-10                    2         2
    ##  8 fema…    25 18-30             0   0-5                     0        NA
    ##  9 male     30 18-30            16.5 15-20                   2         2
    ## 10 male     27 18-30            22.8 20-25                   2         1
    ## # … with 1,209 more rows, and 2 more variables: fhp_size_mm <dbl>,
    ## #   fhp_category <ord>

``` r
posture_table = posture_data %>%
filter(age_group != "1") %>%
group_by(sex, age_group) %>%
summarize (n= n()) %>%
pivot_wider(
names_from = sex, 
values_from = n
) %>%
knitr::kable ()

posture_table
```

| age\_group | female | male |
| :--------- | -----: | ---: |
| 18-30      |    151 |  152 |
| 31-40      |    102 |  102 |
| 41-50      |    106 |  101 |
| 51-60      |     99 |  101 |
| 60+        |    155 |  150 |

Problem 2
