Examples with SEA-PLM 2019
================

``` r
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("haven")
```

## Read data

``` r
dir <- "/home/eldani/eldani/International LSA/SEA-PLM/Data/SEA-PLM_Regional_Students-sav"
student <- read_spss(file.path(dir, "SEA-PLM_Regional_Students.sav"))
dir <- "/home/eldani/eldani/International LSA/SEA-PLM/Data/SEA-PLM_Regional_Schools-sav"
school <- read_spss(file.path(dir, "SEA-PLM_Regional_Schools.sav"))
```

## Merge data

``` r
sea <- left_join(student, school, by = c("CNT", "SchID"))
```

## Create grouping variables

``` r
sea$Sex <- factor(sea$Gender, 1:2, labels = c("Female", "Male"))

sea$Language <- factor(sea$S_LANG, 0:1, labels = c("No", "Yes"))

sea$Location <- factor(sea$SC09Q01, 1:5, labels = c("Rural", rep("Urban", 4)))
```

## Proficiency reading levels by CNT

``` r
intsvy.ben.pv(pvnames= paste0("PV", 1:5, "_R"), cutoff = c(304, 317),
              by= c("CNT"), data=sea, config = sea_conf)
```

    ##    CNT       Benchmark Percentage Std. err.
    ## 1  KHM At or above 304      27.50      1.44
    ## 2  KHM At or above 317      11.02      1.01
    ## 3  LAO At or above 304       8.91      0.80
    ## 4  LAO At or above 317       2.50      0.41
    ## 5  MMR At or above 304      26.88      1.30
    ## 6  MMR At or above 317      10.74      0.78
    ## 7  MYS At or above 304      76.61      1.80
    ## 8  MYS At or above 317      58.26      2.06
    ## 9  PHL At or above 304      21.53      1.62
    ## 10 PHL At or above 317       9.56      1.21
    ## 11 VNM At or above 304      92.34      1.03
    ## 12 VNM At or above 317      81.88      1.42

## Proficiency reading levels by CNT and sex

``` r
intsvy.ben.pv(pvnames= paste0("PV", 1:5, "_R"), cutoff =   c(304, 317), 
              by= c("CNT", "Sex"), data=sea, config = sea_conf)
```

    ##    CNT    Sex       Benchmark Percentage Std. err.
    ## 1  KHM Female At or above 304      32.68      1.85
    ## 2  KHM Female At or above 317      13.79      1.46
    ## 3  KHM   Male At or above 304      22.04      1.33
    ## 4  KHM   Male At or above 317       8.11      0.78
    ## 5  LAO Female At or above 304      10.50      1.12
    ## 6  LAO Female At or above 317       2.99      0.52
    ## 7  LAO   Male At or above 304       7.42      0.96
    ## 8  LAO   Male At or above 317       2.03      0.43
    ## 9  LAO   <NA> At or above 304       0.00      0.00
    ## 10 LAO   <NA> At or above 317       0.00      0.00
    ## 11 MMR Female At or above 304      29.59      1.50
    ## 12 MMR Female At or above 317      12.09      1.07
    ## 13 MMR   Male At or above 304      24.41      1.59
    ## 14 MMR   Male At or above 317       9.51      0.91
    ## 15 MYS Female At or above 304      82.41      1.60
    ## 16 MYS Female At or above 317      65.87      2.15
    ## 17 MYS   Male At or above 304      70.66      2.29
    ## 18 MYS   Male At or above 317      50.43      2.27
    ## 19 PHL Female At or above 304      25.10      1.95
    ## 20 PHL Female At or above 317      10.84      1.37
    ## 21 PHL   Male At or above 304      18.07      1.88
    ## 22 PHL   Male At or above 317       8.33      1.56
    ## 23 VNM Female At or above 304      93.44      1.03
    ## 24 VNM Female At or above 317      84.20      1.46
    ## 25 VNM   Male At or above 304      91.31      1.31
    ## 26 VNM   Male At or above 317      79.69      1.70

## Mean reading achievement by groups

``` r
intsvy.mean.pv(pvnames=paste0("PV", 1:5, "_R"), by= c("CNT"), data=sea, config = sea_conf)
```

    ##   CNT Freq   Mean s.e.    SD  s.e
    ## 1 KHM 5396 290.12 0.82 21.88 0.43
    ## 2 LAO 4698 275.06 0.78 20.62 0.42
    ## 3 MMR 5707 291.73 0.78 19.98 0.37
    ## 4 MYS 4479 318.91 1.14 23.56 0.62
    ## 5 PHL 6083 287.72 0.91 20.59 0.59
    ## 6 VNM 4837 336.46 0.88 22.18 0.65

``` r
intsvy.mean.pv(pvnames=paste0("PV", 1:5, "_R"), by= c("CNT", "Location"), data=sea, config = sea_conf)
```

    ##    CNT Location Freq   Mean s.e.    SD  s.e
    ## 1  KHM    Rural 2487 284.94 1.03 20.74 0.57
    ## 2  KHM    Urban 2842 295.04 1.26 21.85 0.52
    ## 3  KHM     <NA>   67 287.79 5.60 19.04 4.23
    ## 4  LAO    Rural 3262 272.88 1.00 20.25 0.56
    ## 5  LAO    Urban 1187 283.40 1.66 20.21 0.69
    ## 6  LAO     <NA>  249 267.42 2.78 16.43 1.60
    ## 7  MMR    Rural 2900 289.72 0.93 18.87 0.44
    ## 8  MMR    Urban 2718 295.38 1.41 21.23 0.74
    ## 9  MMR     <NA>   89 294.25 6.81 22.57 1.83
    ## 10 MYS    Rural 1267 316.21 2.24 23.62 1.08
    ## 11 MYS    Urban 3212 320.10 1.39 23.43 0.77
    ## 12 PHL    Rural 2229 282.02 1.48 18.83 0.97
    ## 13 PHL    Urban 3854 291.65 1.39 20.82 0.82
    ## 14 VNM    Rural 2364 332.35 1.33 22.18 0.90
    ## 15 VNM    Urban 2441 341.08 1.16 21.27 0.85
    ## 16 VNM     <NA>   32 338.09  NaN 20.09  NaN

``` r
intsvy.mean.pv(pvnames=paste0("PV", 1:5, "_R"), by= c("CNT", "Sex"), 
               data=sea[!is.na(sea$Sex), ], config = sea_conf)
```

    ##    CNT    Sex Freq   Mean s.e.    SD  s.e
    ## 1  KHM Female 2766 293.48 0.92 21.66 0.51
    ## 2  KHM   Male 2630 286.59 0.87 21.54 0.44
    ## 3  LAO Female 2304 275.99 0.97 21.24 0.52
    ## 4  LAO   Male 2393 274.20 0.81 19.98 0.49
    ## 5  MMR Female 2705 293.24 0.83 20.03 0.45
    ## 6  MMR   Male 3002 290.35 0.87 19.83 0.44
    ## 7  MYS Female 2283 323.38 1.12 21.97 0.56
    ## 8  MYS   Male 2196 314.33 1.31 24.24 0.78
    ## 9  PHL Female 3012 290.68 1.00 19.83 0.57
    ## 10 PHL   Male 3071 284.85 1.10 20.90 0.93
    ## 11 VNM Female 2330 338.29 0.98 21.83 0.72
    ## 12 VNM   Male 2507 334.73 0.99 22.36 0.74

``` r
intsvy.mean.pv(pvnames= paste0("PV", 1:5, "_R"), by= c("CNT", "Language"), data=sea, config = sea_conf)
```

    ##    CNT Language Freq   Mean s.e.    SD  s.e
    ## 1  KHM       No  321 280.32 2.53 26.65 1.27
    ## 2  KHM      Yes 5075 290.75 0.77 21.39 0.39
    ## 3  LAO       No 1849 267.61 0.89 19.26 0.50
    ## 4  LAO      Yes 2849 280.11 0.93 19.98 0.47
    ## 5  MMR       No 1262 279.73 1.47 18.59 0.64
    ## 6  MMR      Yes 4445 295.48 0.68 18.89 0.35
    ## 7  MYS       No  903 309.16 1.77 24.78 0.80
    ## 8  MYS      Yes 3576 321.36 1.24 22.59 0.80
    ## 9  PHL       No 5656 287.72 0.87 20.09 0.49
    ## 10 PHL      Yes  427 287.73 3.36 26.24 2.15
    ## 11 VNM       No  432 317.12 3.28 26.03 1.72
    ## 12 VNM      Yes 4405 338.62 0.67 20.61 0.35

## Mean writing and math achievement by CNT

``` r
intsvy.mean.pv(pvnames=paste0("PV", 1:5, "_W"), by= c("CNT"), data=sea, config = sea_conf)
```

    ##   CNT Freq   Mean s.e.    SD  s.e
    ## 1 KHM 5396 284.82 1.01 27.24 0.46
    ## 2 LAO 4698 283.47 1.04 30.65 0.69
    ## 3 MMR 5707 298.48 0.89 20.10 0.55
    ## 4 MYS 4479 317.50 0.88 18.84 0.54
    ## 5 PHL 6083 288.28 1.13 27.73 0.55
    ## 6 VNM 4837 327.45 0.89 22.07 0.54

``` r
intsvy.mean.pv(pvnames=paste0("PV", 1:5, "_M"), by= c("CNT"), data=sea, config = sea_conf)
```

    ##   CNT Freq   Mean s.e.    SD  s.e
    ## 1 KHM 5396 289.41 0.82 20.74 0.49
    ## 2 LAO 4698 278.63 0.82 20.62 0.48
    ## 3 MMR 5707 287.92 0.61 17.23 0.33
    ## 4 MYS 4479 314.71 1.08 21.84 0.63
    ## 5 PHL 6083 287.88 0.84 19.99 0.52
    ## 6 VNM 4837 341.45 1.04 23.99 0.64

## Regression of reading achievement on SES

``` r
intsvy.reg.pv(pvnames= paste0("PV", 1:5, "_R"), x= "SES", by= c("CNT"), data=sea, config = sea_conf)
```

    ## $KHM
    ##             Estimate Std. Error t value
    ## (Intercept)   290.55       0.50  575.65
    ## SES             8.21       0.50   16.27
    ## R-squared       0.13       0.48    0.27
    ## 
    ## $LAO
    ##             Estimate Std. Error t value
    ## (Intercept)   276.02       0.53  522.57
    ## SES             9.00       0.50   18.12
    ## R-squared       0.18       0.49    0.36
    ## 
    ## $MMR
    ##             Estimate Std. Error t value
    ## (Intercept)   292.75       0.52  561.98
    ## SES             6.65       0.49   13.68
    ## R-squared       0.10       0.48    0.21
    ## 
    ## $MYS
    ##             Estimate Std. Error t value
    ## (Intercept)   319.25       0.64  495.28
    ## SES             7.87       0.64   12.23
    ## R-squared       0.11       0.64    0.17
    ## 
    ## $PHL
    ##             Estimate Std. Error t value
    ## (Intercept)   287.77       0.47  611.11
    ## SES            11.44       0.46   24.85
    ## R-squared       0.31       0.46    0.69
    ## 
    ## $VNM
    ##             Estimate Std. Error t value
    ## (Intercept)   337.08       0.51  655.34
    ## SES             9.48       0.55   17.30
    ## R-squared       0.17       0.51    0.35

## Regression of math achievement on SES and sex

``` r
intsvy.reg.pv(pvnames= paste0("PV", 1:5, "_M"), x= c("SES", "Sex"), by= c("CNT"), data=sea, config = sea_conf)
```

    ## $KHM
    ##             Estimate Std. Error t value
    ## (Intercept)   291.69       0.59  497.74
    ## SES             8.05       0.54   14.85
    ## SexMale        -3.79       0.64   -5.95
    ## R-squared       0.15       0.53    0.28
    ## 
    ## $LAO
    ##             Estimate Std. Error t value
    ## (Intercept)   279.73       0.65  431.22
    ## SES             9.12       0.64   14.33
    ## SexMale        -0.30       0.65   -0.46
    ## R-squared       0.18       0.63    0.29
    ## 
    ## $MMR
    ##             Estimate Std. Error t value
    ## (Intercept)   289.01       0.46  632.41
    ## SES             6.05       0.43   13.96
    ## SexMale        -0.34       0.48   -0.71
    ## R-squared       0.11       0.43    0.26
    ## 
    ## $MYS
    ##             Estimate Std. Error t value
    ## (Intercept)   316.45       0.71  444.76
    ## SES             8.71       0.71   12.20
    ## SexMale        -2.77       0.73   -3.76
    ## R-squared       0.16       0.68    0.24
    ## 
    ## $PHL
    ##             Estimate Std. Error t value
    ## (Intercept)   289.48       0.56  519.02
    ## SES            10.18       0.53   19.26
    ## SexMale        -3.01       0.57   -5.26
    ## R-squared       0.27       0.53    0.51
    ## 
    ## $VNM
    ##             Estimate Std. Error t value
    ## (Intercept)   342.13       0.65  523.72
    ## SES             9.39       0.71   13.26
    ## SexMale        -0.12       0.69   -0.17
    ## R-squared       0.15       0.64    0.23
