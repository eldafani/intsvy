Examples with PISA 2022
================

``` r
library("devtools")
install_github("eldafani/intsvy")
library("intsvy")
```

## Select data

Check names of variables in student and school files and names of
participating countries for data selection. Datasets are stored in
directory, *dir*.

``` r
pisa.var.label(folder = dir,
               student.file="CY08MSP_STU_QQQ.sav",
               school.file="CY08MSP_SCH_QQQ.sav", output = dir)
```

Select data for three countries, student’s SES, sex, and school’s
location

``` r
pisa <- pisa.select.merge(folder = dir,
                          student.file="CY08MSP_STU_QQQ.sav",
                          school.file="CY08MSP_SCH_QQQ.sav",
                          student= c("ESCS", "ST004D01T"),
                          school = c("SC001Q01TA"), 
                          countries = c("ARG", "FRA", "PER"))
```

    ## File character set is 'UTF-8'.

    ## Converting character set to UTF-8.

    ## Warning: 1 variables have duplicated labels:
    ##   LANGN

    ## File character set is 'UTF-8'.
    ## Converting character set to UTF-8.

## Calculate average mathematics score by country

The average mathematics score and its associated standard error are
presented, first for Argentina, second for France and third for Peru.

``` r
pisa.mean.pv(pvlabel = paste0("PV", 1:10, "MATH"), by = "CNT", data = pisa)
```

    ##   CNT  Freq   Mean s.e.    SD  s.e
    ## 1 ARG 12111 377.53 2.25 74.42 1.06
    ## 2 FRA  6770 473.94 2.49 91.06 1.07
    ## 3 PER  6968 391.24 2.34 77.91 1.17

## Calculate average mathematics score by country and sex

``` r
pisa.mean.pv(pvlabel = paste0("PV", 1:10, "MATH"), by = c("CNT", "ST004D01T"), data = pisa)
```

    ##   CNT ST004D01T Freq   Mean s.e.    SD  s.e
    ## 1 ARG         1 6094 372.04 2.55 71.74 1.37
    ## 2 ARG         2 6017 383.03 2.42 76.63 1.29
    ## 3 FRA         1 3406 469.14 2.51 85.87 1.39
    ## 4 FRA         2 3364 478.93 3.44 95.90 1.30
    ## 5 PER         1 3474 383.65 2.48 74.58 1.50
    ## 6 PER         2 3494 398.82 2.82 80.38 1.37

## Calculate proficiency levels by country

### Cutoff scores for mathematics performance

``` r
mathcut <- c(357.77, 420.07, 482.38, 544.68, 606.99, 669.3)
```

### Produce table with results

``` r
pisa.ben.pv(pvlabel= paste0("PV", 1:10, "MATH"), cutoff= mathcut, by="CNT", data=pisa)
```

    ##    CNT          Benchmark Percentage Std. err.
    ## 1  ARG At or above 357.77      57.91      1.33
    ## 2  ARG At or above 420.07      27.07      1.18
    ## 3  ARG At or above 482.38       8.93      0.61
    ## 4  ARG At or above 544.68       2.01      0.27
    ## 5  ARG At or above 606.99       0.30      0.08
    ## 6  ARG  At or above 669.3       0.02      0.02
    ## 7  FRA At or above 357.77      89.04      0.69
    ## 8  FRA At or above 420.07      71.19      1.11
    ## 9  FRA At or above 482.38      46.97      1.21
    ## 10 FRA At or above 544.68      23.10      0.95
    ## 11 FRA At or above 606.99       7.39      0.51
    ## 12 FRA  At or above 669.3       1.14      0.20
    ## 13 PER At or above 357.77      64.31      1.24
    ## 14 PER At or above 420.07      33.83      1.18
    ## 15 PER At or above 482.38      13.06      0.78
    ## 16 PER At or above 544.68       3.31      0.39
    ## 17 PER At or above 606.99       0.49      0.12
    ## 18 PER  At or above 669.3       0.03      0.03

## Estimate regression of reading on sex by country

``` r
pisa.reg.pv(pvlabel= paste0("PV", 1:10, "READ"), x="ST004D01T", by = "CNT", data=pisa)
```

    ## $ARG
    ##             Estimate Std. Error t value
    ## (Intercept)   422.10       4.43   95.35
    ## ST004D01T     -14.25       2.53   -5.63
    ## R-squared       0.01       0.00    2.82
    ## 
    ## $FRA
    ##             Estimate Std. Error t value
    ## (Intercept)   503.98       6.33   79.66
    ## ST004D01T     -20.21       4.26   -4.74
    ## R-squared       0.01       0.00    2.44
    ## 
    ## $PER
    ##             Estimate Std. Error t value
    ## (Intercept)   420.41       5.35   78.58
    ## ST004D01T      -8.11       3.10   -2.61
    ## R-squared       0.00       0.00    1.28

## Estimate regression of mathematics on SES by country

``` r
pisa.reg.pv(pvlabel= paste0("PV", 1:10, "MATH"), x="ESCS", by = "CNT", data=pisa)
```

    ## $ARG
    ##             Estimate Std. Error t value
    ## (Intercept)   398.46       2.03  196.74
    ## ESCS           25.69       1.20   21.45
    ## R-squared       0.15       0.01   11.56
    ## 
    ## $FRA
    ##             Estimate Std. Error t value
    ## (Intercept)   475.59       1.92  248.05
    ## ESCS           45.52       1.52   29.86
    ## R-squared       0.21       0.01   16.38
    ## 
    ## $PER
    ##             Estimate Std. Error t value
    ## (Intercept)   421.74       2.43  173.77
    ## ESCS           26.16       1.08   24.29
    ## R-squared       0.17       0.01   11.92
