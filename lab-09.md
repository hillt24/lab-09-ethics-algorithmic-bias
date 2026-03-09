Lab 09: Algorithmic Bias
================
Tiffani J. Hill
03.04.2026

## Load Packages and Data

First, let’s load the necessary packages:

``` r
library(tidyverse)
library(fairness)
library(janitor)
```

### The data

For this lab, we’ll use the COMPAS dataset compiled by ProPublica. The
data has been preprocessed and cleaned for you. You’ll have to load it
yourself. The dataset is available in the `data` folder, but I’ve
changed the file name from `compas-scores-two-years.csv` to
`compas-scores-2-years.csv`. I’ve done this help you practice debugging
code when you encounter an error.

``` r
compas <- read_csv("data/compas-scores-2-years.csv") %>% 
  clean_names() %>% 
  rename(
    decile_score = decile_score_12,
    priors_count = priors_count_15
  )
```

    ## New names:
    ## Rows: 7214 Columns: 53
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (19): name, first, last, sex, age_cat, race, c_case_number, c_charge_de... dbl
    ## (19): id, age, juv_fel_count, decile_score...12, juv_misd_count, juv_ot... lgl
    ## (1): violent_recid dttm (2): c_jail_in, c_jail_out date (12):
    ## compas_screening_date, dob, c_offense_date, c_arrest_date, r_offe...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `decile_score` -> `decile_score...12`
    ## • `priors_count` -> `priors_count...15`
    ## • `decile_score` -> `decile_score...40`
    ## • `priors_count` -> `priors_count...49`

``` r
glimpse(compas)
```

    ## Rows: 7,214
    ## Columns: 53
    ## $ id                      <dbl> 1, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 18…
    ## $ name                    <chr> "miguel hernandez", "kevon dixon", "ed philo",…
    ## $ first                   <chr> "miguel", "kevon", "ed", "marcu", "bouthy", "m…
    ## $ last                    <chr> "hernandez", "dixon", "philo", "brown", "pierr…
    ## $ compas_screening_date   <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ sex                     <chr> "Male", "Male", "Male", "Male", "Male", "Male"…
    ## $ dob                     <date> 1947-04-18, 1982-01-22, 1991-05-14, 1993-01-2…
    ## $ age                     <dbl> 69, 34, 24, 23, 43, 44, 41, 43, 39, 21, 27, 23…
    ## $ age_cat                 <chr> "Greater than 45", "25 - 45", "Less than 25", …
    ## $ race                    <chr> "Other", "African-American", "African-American…
    ## $ juv_fel_count           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ decile_score            <dbl> 1, 3, 4, 8, 1, 1, 6, 4, 1, 3, 4, 6, 1, 4, 1, 3…
    ## $ juv_misd_count          <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ juv_other_count         <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ priors_count            <dbl> 0, 0, 4, 1, 2, 0, 14, 3, 0, 1, 0, 3, 0, 0, 1, …
    ## $ days_b_screening_arrest <dbl> -1, -1, -1, NA, NA, 0, -1, -1, -1, 428, -1, 0,…
    ## $ c_jail_in               <dttm> 2013-08-13 06:03:42, 2013-01-26 03:45:27, 201…
    ## $ c_jail_out              <dttm> 2013-08-14 05:41:20, 2013-02-05 05:36:53, 201…
    ## $ c_case_number           <chr> "13011352CF10A", "13001275CF10A", "13005330CF1…
    ## $ c_offense_date          <date> 2013-08-13, 2013-01-26, 2013-04-13, 2013-01-1…
    ## $ c_arrest_date           <date> NA, NA, NA, NA, 2013-01-09, NA, NA, 2013-08-2…
    ## $ c_days_from_compas      <dbl> 1, 1, 1, 1, 76, 0, 1, 1, 1, 308, 1, 0, 0, 1, 4…
    ## $ c_charge_degree         <chr> "F", "F", "F", "F", "F", "M", "F", "F", "M", "…
    ## $ c_charge_desc           <chr> "Aggravated Assault w/Firearm", "Felony Batter…
    ## $ is_recid                <dbl> 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…
    ## $ r_case_number           <chr> NA, "13009779CF10A", "13011511MM10A", NA, NA, …
    ## $ r_charge_degree         <chr> NA, "(F3)", "(M1)", NA, NA, NA, "(F2)", NA, NA…
    ## $ r_days_from_arrest      <dbl> NA, NA, 0, NA, NA, NA, 0, NA, NA, 0, NA, NA, N…
    ## $ r_offense_date          <date> NA, 2013-07-05, 2013-06-16, NA, NA, NA, 2014-…
    ## $ r_charge_desc           <chr> NA, "Felony Battery (Dom Strang)", "Driving Un…
    ## $ r_jail_in               <date> NA, NA, 2013-06-16, NA, NA, NA, 2014-03-31, N…
    ## $ r_jail_out              <date> NA, NA, 2013-06-16, NA, NA, NA, 2014-04-18, N…
    ## $ violent_recid           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ is_violent_recid        <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
    ## $ vr_case_number          <chr> NA, "13009779CF10A", NA, NA, NA, NA, NA, NA, N…
    ## $ vr_charge_degree        <chr> NA, "(F3)", NA, NA, NA, NA, NA, NA, NA, "(F2)"…
    ## $ vr_offense_date         <date> NA, 2013-07-05, NA, NA, NA, NA, NA, NA, NA, 2…
    ## $ vr_charge_desc          <chr> NA, "Felony Battery (Dom Strang)", NA, NA, NA,…
    ## $ type_of_assessment      <chr> "Risk of Recidivism", "Risk of Recidivism", "R…
    ## $ decile_score_40         <dbl> 1, 3, 4, 8, 1, 1, 6, 4, 1, 3, 4, 6, 1, 4, 1, 3…
    ## $ score_text              <chr> "Low", "Low", "Low", "High", "Low", "Low", "Me…
    ## $ screening_date          <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ v_type_of_assessment    <chr> "Risk of Violence", "Risk of Violence", "Risk …
    ## $ v_decile_score          <dbl> 1, 1, 3, 6, 1, 1, 2, 3, 1, 5, 4, 4, 1, 2, 1, 2…
    ## $ v_score_text            <chr> "Low", "Low", "Low", "Medium", "Low", "Low", "…
    ## $ v_screening_date        <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ in_custody              <date> 2014-07-07, 2013-01-26, 2013-06-16, NA, NA, 2…
    ## $ out_custody             <date> 2014-07-14, 2013-02-05, 2013-06-16, NA, NA, 2…
    ## $ priors_count_49         <dbl> 0, 0, 4, 1, 2, 0, 14, 3, 0, 1, 0, 3, 0, 0, 1, …
    ## $ start                   <dbl> 0, 9, 0, 0, 0, 1, 5, 0, 2, 0, 0, 4, 1, 0, 0, 0…
    ## $ end                     <dbl> 327, 159, 63, 1174, 1102, 853, 40, 265, 747, 4…
    ## $ event                   <dbl> 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…
    ## $ two_year_recid          <dbl> 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…

\###Exercise 1 There are 7214 rows, which represent individual
observations. There are 53 columns.

``` r
nrow(compas)
```

    ## [1] 7214

``` r
ncol(compas)
```

    ## [1] 53

``` r
colnames(compas)
```

    ##  [1] "id"                      "name"                   
    ##  [3] "first"                   "last"                   
    ##  [5] "compas_screening_date"   "sex"                    
    ##  [7] "dob"                     "age"                    
    ##  [9] "age_cat"                 "race"                   
    ## [11] "juv_fel_count"           "decile_score"           
    ## [13] "juv_misd_count"          "juv_other_count"        
    ## [15] "priors_count"            "days_b_screening_arrest"
    ## [17] "c_jail_in"               "c_jail_out"             
    ## [19] "c_case_number"           "c_offense_date"         
    ## [21] "c_arrest_date"           "c_days_from_compas"     
    ## [23] "c_charge_degree"         "c_charge_desc"          
    ## [25] "is_recid"                "r_case_number"          
    ## [27] "r_charge_degree"         "r_days_from_arrest"     
    ## [29] "r_offense_date"          "r_charge_desc"          
    ## [31] "r_jail_in"               "r_jail_out"             
    ## [33] "violent_recid"           "is_violent_recid"       
    ## [35] "vr_case_number"          "vr_charge_degree"       
    ## [37] "vr_offense_date"         "vr_charge_desc"         
    ## [39] "type_of_assessment"      "decile_score_40"        
    ## [41] "score_text"              "screening_date"         
    ## [43] "v_type_of_assessment"    "v_decile_score"         
    ## [45] "v_score_text"            "v_screening_date"       
    ## [47] "in_custody"              "out_custody"            
    ## [49] "priors_count_49"         "start"                  
    ## [51] "end"                     "event"                  
    ## [53] "two_year_recid"

## Exercise 2

The number of defendants is equivalent to the number of rows. Each entry
has a unique id number.

\###Exercise 3

``` r
compas %>% 
  ggplot(aes(x = decile_score)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-09_files/figure-gfm/risk-score-1.png)<!-- --> \###Exercise 4

``` r
compas %>% 
  ggplot(aes(x = decile_score)) +
  geom_histogram() +
  facet_wrap(~sex)
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-09_files/figure-gfm/visual-by-demographics-1.png)<!-- -->

``` r
compas %>% 
  ggplot(aes(x = decile_score)) +
  geom_histogram() +
  facet_wrap(~race)
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-09_files/figure-gfm/visual-by-demographics-2.png)<!-- -->

``` r
compas %>% 
  ggplot(aes(x = decile_score)) +
  geom_histogram() +
  facet_wrap(~age_cat)
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-09_files/figure-gfm/visual-by-demographics-3.png)<!-- -->

``` r
compas %>% 
  ggplot(aes(x = decile_score, color = race))+
  geom_bar() +
  facet_wrap(~age_cat)
```

![](lab-09_files/figure-gfm/challenge-visual-1.png)<!-- -->

``` r
compas %>% 
  ggplot(aes(x = two_year_recid, y = decile_score)) +
geom_count()
```

![](lab-09_files/figure-gfm/recidivisism-1.png)<!-- --> Not exactly.

``` r
compas_accuracy <- lm(compas$two_year_recid ~ compas$decile_score)
summary(compas_accuracy)
```

    ## 
    ## Call:
    ## lm(formula = compas$two_year_recid ~ compas$decile_score)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7861 -0.3584 -0.2362  0.4583  0.7638 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         0.175146   0.010254   17.08   <2e-16 ***
    ## compas$decile_score 0.061094   0.001921   31.80   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.466 on 7212 degrees of freedom
    ## Multiple R-squared:  0.123,  Adjusted R-squared:  0.1229 
    ## F-statistic:  1011 on 1 and 7212 DF,  p-value: < 2.2e-16

``` r
cor(compas$decile_score, compas$two_year_recid)
```

    ## [1] 0.3507026
