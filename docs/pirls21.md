Examples with PIRLS 2021
================

``` r
library("devtools")
install_github("eldafani/intsvy")
library("intsvy")
```

## Read data for selected variables and countries

The object *dir* specifies the directory path where the PIRLS 2021 data
is located (eg. “/home/data”). Variable selection can be done with aid
of *pirls.var.label(dir)*.

``` r
pirls <- pirls.select.merge(folder= dir,
                            student= c("ITSEX", "ASBG03"),
                            home= c("ASBHSES", "ASDHSES"), 
                            school= c("ACBG05B"))
```

## Calculate average reading score by country

``` r
pirls.mean.pv(pvlabel = paste0("ASRREA0", 1:5), by = "IDCNTRYL", data = pirls)
```

    ##                           IDCNTRYL  Freq   Mean s.e.     SD  s.e
    ## 1                   Abu Dhabi, UAE 10381 439.56 3.53 141.71 1.90
    ## 2                          Albania  4213 512.74 3.10  78.38 1.42
    ## 3                        Australia  5487 540.13 2.21  82.06 1.45
    ## 4                          Austria  4806 529.76 2.17  68.64 1.16
    ## 5                       Azerbaijan  5209 440.27 3.59  89.45 1.61
    ## 6                          Bahrain  5208 458.39 2.87 109.24 2.03
    ## 7                Belgium (Flemish)  5114 510.72 2.30  66.57 1.12
    ## 8                 Belgium (French)  4279 494.27 2.66  72.61 1.29
    ## 9                           Brazil  4941 418.88 5.33 122.58 3.01
    ## 10                        Bulgaria  4043 539.89 3.01  88.00 2.51
    ## 11 Canada, Newfoundland & Labrador  2445 523.32 3.21  78.79 1.63
    ## 12                 Canada, Alberta  3020 538.69 3.56  77.42 1.88
    ## 13        Canada, British Columbia  4675 535.30 3.55  78.46 1.95
    ## 14                  Canada, Quebec  3739 551.00 2.73  63.76 1.45
    ## 15                  Chinese Taipei  5555 543.76 2.15  68.96 1.29
    ## 16                         Croatia  3937 556.55 2.52  68.69 1.44
    ## 17                          Cyprus  4589 510.87 2.87  78.16 1.33
    ## 18                  Czech Republic  6621 539.65 2.29  73.00 1.29
    ## 19                         Denmark  4821 539.00 2.23  73.12 1.38
    ## 20                       Dubai,UAE  7711 551.86 1.50  99.94 1.53
    ## 21                           Egypt  7979 378.23 5.41 109.93 2.55
    ## 22                         England  4150 557.55 2.47  76.32 1.30
    ## 23                         Finland  7018 549.30 2.40  73.98 1.47
    ## 24                          France  5339 513.74 2.54  71.01 1.46
    ## 25                         Georgia  5241 494.03 2.60  81.81 1.68
    ## 26                         Germany  4611 523.97 2.13  76.59 1.13
    ## 27                   Hong Kong SAR  3830 572.82 2.74  66.52 2.09
    ## 28                         Hungary  5312 539.42 3.43  81.33 2.30
    ## 29           Iran, Islamic Rep. of  5962 412.77 4.88  99.78 3.48
    ## 30                         Ireland  4663 577.33 2.47  76.69 1.36
    ## 31                          Israel  4890 510.05 2.22  88.32 1.61
    ## 32                           Italy  5440 537.18 2.20  66.44 1.05
    ## 33                          Jordan  6150 380.53 5.40 114.28 2.93
    ## 34                      Kazakhstan  7023 503.59 2.73  75.35 1.14
    ## 35                          Kosovo  4557 420.55 3.13  83.63 1.64
    ## 36                          Latvia  4369 527.68 2.61  74.65 2.05
    ## 37                       Lithuania  4623 552.22 2.27  70.91 1.30
    ## 38                      Montenegro  4489 487.16 1.65  77.37 1.04
    ## 39              Macedonia, Rep. of  2929 442.15 5.32  90.47 2.18
    ## 40                           Malta  3030 514.64 2.73  84.06 1.90
    ## 41                         Morocco  7017 372.39 4.53 106.23 2.91
    ## 42                     Netherlands  4313 527.24 2.52  66.80 1.46
    ## 43                     New Zealand  5557 521.47 2.31  89.32 1.31
    ## 44                Northern Ireland  4050 565.93 2.51  81.37 1.44
    ## 45                          Norway  5382 538.79 1.99  73.86 1.18
    ## 46                            Oman  5321 429.48 3.70 109.22 1.89
    ## 47                          Poland  4179 549.12 2.18  71.87 1.52
    ## 48                        Portugal  6111 519.76 2.26  71.91 1.32
    ## 49                           Qatar  5258 484.73 3.75  97.63 1.45
    ## 50              Russian Federation  5217 567.11 3.57  71.28 1.78
    ## 51                    Saudi Arabia  4778 448.55 3.60  90.18 1.85
    ## 52                          Serbia  4037 513.58 2.78  73.52 1.61
    ## 53                       Singapore  6719 587.14 3.14  85.71 2.08
    ## 54                 Slovak Republic  4841 529.07 2.73  76.70 2.22
    ## 55                        Slovenia  5110 519.66 1.86  70.09 1.13
    ## 56                    South Africa 12422 288.22 4.42 129.40 2.86
    ## 57                           Spain  8551 521.21 2.20  68.94 1.31
    ## 58                          Sweden  5175 543.52 2.14  79.42 1.48
    ## 59                          Turkey  6032 496.45 3.39  87.50 1.75
    ## 60            United Arab Emirates 27448 483.08 1.83 126.67 1.44
    ## 61                   United States  1657 547.54 6.76  87.33 4.36
    ## 62                      Uzbekistan  5846 436.84 2.85  81.28 1.39
    ## 63                       Macao SAR  5093 535.56 1.28  70.88 0.87
    ## 64       Moscow City, Russian Fed.  5745 598.23 2.10  62.56 0.97
    ## 65                South Africa (6)  9317 384.27 4.51 128.15 2.80

## Calculate average reading score by country and sex

``` r
pirls.mean.pv(pvlabel = paste0("ASRREA0", 1:5), by = c("IDCNTRYL", "ITSEX"), 
              data = pirls[!is.na(pirls$ITSEX), ])
```

    ##                            IDCNTRYL ITSEX  Freq   Mean s.e.     SD  s.e
    ## 1                    Abu Dhabi, UAE     1  5305 457.00 3.88 133.04 2.41
    ## 2                    Abu Dhabi, UAE     2  5076 421.69 5.31 147.95 2.71
    ## 3                           Albania     1  2049 523.19 3.54  76.90 1.90
    ## 4                           Albania     2  2164 502.88 3.40  78.49 1.80
    ## 5                         Australia     1  2730 548.85 2.52  78.76 1.99
    ## 6                         Australia     2  2757 531.52 2.82  84.31 1.88
    ## 7                           Austria     1  2348 537.06 2.57  69.23 1.48
    ## 8                           Austria     2  2458 522.69 2.55  67.30 1.36
    ## 9                        Azerbaijan     1  2447 449.76 4.09  87.72 2.11
    ## 10                       Azerbaijan     2  2762 431.92 3.97  90.10 1.99
    ## 11                          Bahrain     1  2508 482.91 3.91  99.10 2.00
    ## 12                          Bahrain     2  2700 433.99 3.24 113.32 2.73
    ## 13                Belgium (Flemish)     1  2518 514.70 2.57  64.54 1.30
    ## 14                Belgium (Flemish)     2  2595 506.91 2.80  68.23 1.59
    ## 15                 Belgium (French)     1  2101 499.39 3.25  72.01 1.65
    ## 16                 Belgium (French)     2  2178 489.33 2.93  72.83 1.71
    ## 17                           Brazil     1  2387 430.76 5.98 118.51 4.06
    ## 18                           Brazil     2  2543 407.79 6.11 125.32 3.03
    ## 19                         Bulgaria     1  1976 547.82 3.01  86.66 2.37
    ## 20                         Bulgaria     2  2067 532.64 3.99  88.58 3.34
    ## 21  Canada, Newfoundland & Labrador     1  1226 530.41 3.14  75.91 2.16
    ## 22  Canada, Newfoundland & Labrador     2  1219 516.13 4.28  80.98 2.22
    ## 23                  Canada, Alberta     1  1482 546.39 4.07  76.07 2.55
    ## 24                  Canada, Alberta     2  1537 531.32 4.20  77.96 2.41
    ## 25         Canada, British Columbia     1  2313 542.20 3.47  77.04 2.36
    ## 26         Canada, British Columbia     2  2362 528.78 4.29  79.21 2.38
    ## 27                   Canada, Quebec     1  1884 556.28 3.33  63.31 1.84
    ## 28                   Canada, Quebec     2  1853 545.69 2.88  63.78 1.89
    ## 29                   Chinese Taipei     1  2701 550.61 2.48  68.30 1.79
    ## 30                   Chinese Taipei     2  2854 537.31 2.39  68.96 1.53
    ## 31                          Croatia     1  1935 561.96 3.01  69.21 1.68
    ## 32                          Croatia     2  2002 551.49 3.00  67.80 1.92
    ## 33                           Cyprus     1  2339 515.24 3.20  77.59 1.75
    ## 34                           Cyprus     2  2250 506.31 3.14  78.49 1.65
    ## 35                   Czech Republic     1  3287 541.45 2.81  71.04 1.72
    ## 36                   Czech Republic     2  3334 537.91 2.68  74.81 1.47
    ## 37                          Denmark     1  2479 545.02 2.54  71.80 1.34
    ## 38                          Denmark     2  2342 532.58 2.81  73.96 2.04
    ## 39                        Dubai,UAE     1  3868 556.50 2.66  97.03 1.75
    ## 40                        Dubai,UAE     2  3843 547.08 2.54 102.63 2.28
    ## 41                            Egypt     1  3935 386.34 5.69 106.28 3.08
    ## 42                            Egypt     2  4044 370.29 6.42 112.83 2.89
    ## 43                          England     1  2135 562.39 3.05  74.93 1.66
    ## 44                          England     2  2008 552.51 3.14  77.43 1.79
    ## 45                          Finland     1  3521 558.12 2.72  71.74 1.56
    ## 46                          Finland     2  3497 540.53 2.74  75.12 1.81
    ## 47                           France     1  2667 520.72 2.95  69.21 1.75
    ## 48                           France     2  2672 506.75 2.71  72.08 1.99
    ## 49                          Georgia     1  2542 505.75 2.81  77.57 1.88
    ## 50                          Georgia     2  2699 482.97 3.12  84.14 1.94
    ## 51                          Germany     1  2245 531.81 2.47  75.65 1.55
    ## 52                          Germany     2  2365 516.47 2.51  76.75 1.56
    ## 53                    Hong Kong SAR     1  1955 576.90 2.79  64.04 2.39
    ## 54                    Hong Kong SAR     2  1875 568.62 3.34  68.73 2.37
    ## 55                          Hungary     1  2676 546.79 3.67  77.90 2.51
    ## 56                          Hungary     2  2636 532.04 3.98  83.97 2.72
    ## 57            Iran, Islamic Rep. of     1  2857 421.81 7.45  98.56 4.59
    ## 58            Iran, Islamic Rep. of     2  3105 404.97 5.93 100.15 3.98
    ## 59                          Ireland     1  2303 582.99 3.27  77.87 2.00
    ## 60                          Ireland     2  2360 571.91 2.81  75.13 1.48
    ## 61                           Israel     1  2444 512.22 2.76  86.69 1.81
    ## 62                           Israel     2  2446 507.93 2.56  89.84 1.95
    ## 63                            Italy     1  2712 540.70 2.41  65.87 1.13
    ## 64                            Italy     2  2728 533.73 2.42  66.81 1.38
    ## 65                           Jordan     1  3024 398.32 6.76 107.35 3.53
    ## 66                           Jordan     2  3126 362.29 7.95 118.22 3.83
    ## 67                       Kazakhstan     1  3516 512.17 2.79  70.95 1.44
    ## 68                       Kazakhstan     2  3507 494.88 3.28  78.61 1.53
    ## 69                           Kosovo     1  2310 430.70 3.13  79.90 2.29
    ## 70                           Kosovo     2  2247 409.78 3.81  86.11 2.10
    ## 71                           Latvia     1  2132 541.51 2.59  71.74 2.40
    ## 72                           Latvia     2  2237 514.33 3.25  74.96 2.44
    ## 73                        Lithuania     1  2307 562.99 2.53  67.52 1.45
    ## 74                        Lithuania     2  2316 541.50 2.75  72.56 1.76
    ## 75                       Montenegro     1  2172 497.46 2.02  74.55 1.45
    ## 76                       Montenegro     2  2317 477.56 2.16  78.70 1.52
    ## 77               Macedonia, Rep. of     1  1478 454.29 5.75  89.38 2.93
    ## 78               Macedonia, Rep. of     2  1451 429.37 5.96  89.83 2.96
    ## 79                            Malta     1  1407 518.12 3.56  83.15 2.29
    ## 80                            Malta     2  1623 511.65 3.19  84.72 2.34
    ## 81                          Morocco     1  3385 389.52 4.55 103.46 3.15
    ## 82                          Morocco     2  3631 356.31 5.17 106.29 3.69
    ## 83                      Netherlands     1  2184 533.52 2.90  65.92 2.01
    ## 84                      Netherlands     2  2126 520.83 2.77  67.10 1.61
    ## 85                      New Zealand     1  2731 531.17 2.86  87.10 1.67
    ## 86                      New Zealand     2  2825 512.22 2.74  90.44 1.65
    ## 87                 Northern Ireland     1  2095 577.54 2.92  76.93 1.96
    ## 88                 Northern Ireland     2  1955 553.20 3.11  84.16 1.85
    ## 89                           Norway     1  2647 547.05 2.30  71.30 1.61
    ## 90                           Norway     2  2735 530.87 2.36  75.39 1.55
    ## 91                             Oman     1  2687 447.12 4.20 101.81 2.29
    ## 92                             Oman     2  2634 411.58 4.08 113.49 2.50
    ## 93                           Poland     1  2016 559.75 2.48  69.86 1.73
    ## 94                           Poland     2  2163 539.69 2.71  72.31 2.15
    ## 95                         Portugal     1  2961 522.65 2.26  68.47 1.32
    ## 96                         Portugal     2  3150 517.07 2.67  74.88 1.82
    ## 97                            Qatar     1  2746 492.75 4.22  93.23 1.83
    ## 98                            Qatar     2  2512 476.23 4.75 101.39 2.30
    ## 99               Russian Federation     1  2568 573.90 3.37  69.78 1.97
    ## 100              Russian Federation     2  2649 560.58 4.51  72.09 2.26
    ## 101                    Saudi Arabia     1  2430 463.50 4.98  87.92 2.65
    ## 102                    Saudi Arabia     2  2348 428.11 4.94  89.22 2.27
    ## 103                          Serbia     1  1952 518.37 3.40  72.29 2.50
    ## 104                          Serbia     2  2085 509.04 3.16  74.37 2.15
    ## 105                       Singapore     1  3288 596.26 2.99  80.66 1.83
    ## 106                       Singapore     2  3431 578.48 3.75  89.38 2.53
    ## 107                 Slovak Republic     1  2474 533.11 2.89  75.57 2.35
    ## 108                 Slovak Republic     2  2367 524.76 3.24  77.66 2.47
    ## 109                        Slovenia     1  2493 529.00 2.06  65.29 1.22
    ## 110                        Slovenia     2  2617 510.75 2.30  73.28 1.60
    ## 111                    South Africa     1  6157 317.20 4.35 122.24 2.91
    ## 112                    South Africa     2  6252 260.14 5.03 129.99 3.31
    ## 113                           Spain     1  4096 522.24 2.59  68.43 1.81
    ## 114                           Spain     2  4455 520.30 2.54  69.38 1.58
    ## 115                          Sweden     1  2588 551.04 2.52  77.33 2.06
    ## 116                          Sweden     2  2587 536.08 2.33  80.75 1.48
    ## 117                          Turkey     1  2989 505.10 3.76  85.45 2.18
    ## 118                          Turkey     2  3043 488.06 3.59  88.65 1.79
    ## 119            United Arab Emirates     1 14007 497.08 2.67 117.57 1.60
    ## 120            United Arab Emirates     2 13441 468.41 3.58 133.99 1.93
    ## 121                   United States     1   801 551.19 7.20  87.44 4.92
    ## 122                   United States     2   855 543.78 7.08  87.16 4.79
    ## 123                      Uzbekistan     1  2822 449.23 3.13  77.03 1.65
    ## 124                      Uzbekistan     2  3024 425.20 3.46  83.42 1.74
    ## 125                       Macao SAR     1  2555 540.44 1.52  69.33 1.18
    ## 126                       Macao SAR     2  2538 530.68 1.88  72.06 1.32
    ## 127       Moscow City, Russian Fed.     1  2852 603.88 2.16  60.28 1.20
    ## 128       Moscow City, Russian Fed.     2  2893 592.70 2.48  64.23 1.31
    ## 129                South Africa (6)     1  4864 408.35 4.47 121.99 3.09
    ## 130                South Africa (6)     2  4446 358.59 5.25 129.57 3.20

### Calculate MPL by country

``` r
pirls.ben.pv(pvlabel= paste0("ASRREA0", 1:5), cutoff = 400, by="IDCNTRYL", data=pirls)
```

    ##                           IDCNTRYL       Benchmark Percentage Std. err.
    ## 1                   Abu Dhabi, UAE At or above 400      61.09      1.18
    ## 2                          Albania At or above 400      92.12      0.94
    ## 3                        Australia At or above 400      94.24      0.55
    ## 4                          Austria At or above 400      95.92      0.43
    ## 5                       Azerbaijan At or above 400      67.43      1.49
    ## 6                          Bahrain At or above 400      71.18      0.93
    ## 7                Belgium (Flemish) At or above 400      94.31      0.52
    ## 8                 Belgium (French) At or above 400      89.37      0.92
    ## 9                           Brazil At or above 400      61.25      1.88
    ## 10                        Bulgaria At or above 400      92.88      0.94
    ## 11 Canada, Newfoundland & Labrador At or above 400      92.51      0.74
    ## 12                 Canada, Alberta At or above 400      94.97      0.77
    ## 13        Canada, British Columbia At or above 400      94.32      0.76
    ## 14                  Canada, Quebec At or above 400      98.78      0.34
    ## 15                  Chinese Taipei At or above 400      96.86      0.40
    ## 16                         Croatia At or above 400      98.08      0.44
    ## 17                          Cyprus At or above 400      91.79      0.67
    ## 18                  Czech Republic At or above 400      96.01      0.53
    ## 19                         Denmark At or above 400      95.95      0.60
    ## 20                       Dubai,UAE At or above 400      91.80      0.41
    ## 21                           Egypt At or above 400      44.57      2.00
    ## 22                         England At or above 400      97.03      0.40
    ## 23                         Finland At or above 400      96.48      0.53
    ## 24                          France At or above 400      93.86      0.66
    ## 25                         Georgia At or above 400      87.16      0.97
    ## 26                         Germany At or above 400      93.59      0.52
    ## 27                   Hong Kong SAR At or above 400      98.28      0.41
    ## 28                         Hungary At or above 400      94.13      1.01
    ## 29           Iran, Islamic Rep. of At or above 400      58.60      2.01
    ## 30                         Ireland At or above 400      97.78      0.38
    ## 31                          Israel At or above 400      87.96      0.82
    ## 32                           Italy At or above 400      97.18      0.33
    ## 33                          Jordan At or above 400      46.64      1.97
    ## 34                      Kazakhstan At or above 400      90.70      0.79
    ## 35                          Kosovo At or above 400      62.08      1.54
    ## 36                          Latvia At or above 400      94.29      0.74
    ## 37                       Lithuania At or above 400      97.11      0.40
    ## 38                      Montenegro At or above 400      86.91      0.68
    ## 39              Macedonia, Rep. of At or above 400      69.96      2.23
    ## 40                           Malta At or above 400      90.24      0.91
    ## 41                         Morocco At or above 400      40.58      1.63
    ## 42                     Netherlands At or above 400      96.45      0.68
    ## 43                     New Zealand At or above 400      89.88      0.64
    ## 44                Northern Ireland At or above 400      96.83      0.46
    ## 45                          Norway At or above 400      95.82      0.56
    ## 46                            Oman At or above 400      62.20      1.37
    ## 47                          Poland At or above 400      97.21      0.47
    ## 48                        Portugal At or above 400      94.18      0.58
    ## 49                           Qatar At or above 400      80.39      1.17
    ## 50              Russian Federation At or above 400      98.36      0.37
    ## 51                    Saudi Arabia At or above 400      71.07      1.62
    ## 52                          Serbia At or above 400      92.80      0.75
    ## 53                       Singapore At or above 400      96.66      0.47
    ## 54                 Slovak Republic At or above 400      93.65      0.80
    ## 55                        Slovenia At or above 400      94.38      0.47
    ## 56                    South Africa At or above 400      19.45      1.18
    ## 57                           Spain At or above 400      95.23      0.57
    ## 58                          Sweden At or above 400      95.28      0.60
    ## 59                          Turkey At or above 400      85.89      1.19
    ## 60            United Arab Emirates At or above 400      75.03      0.66
    ## 61                   United States At or above 400      94.51      1.45
    ## 62                      Uzbekistan At or above 400      69.84      1.38
    ## 63                       Macao SAR At or above 400      95.92      0.38
    ## 64       Moscow City, Russian Fed. At or above 400      99.68      0.08
    ## 65                South Africa (6) At or above 400      44.00      1.49

## Estimate regression of reading on sex by country

``` r
pirls.reg.pv(pvlabel= paste0("ASRREA0", 1:5), x="ITSEX", by = "IDCNTRYL", data=pirls[!is.na(pirls$ITSEX), ])
```

    ## $`Abu Dhabi, UAE`
    ##             Estimate Std. Error t value
    ## (Intercept)   492.32       9.04   54.47
    ## ITSEX         -35.31       6.32   -5.59
    ## R-squared       0.02       0.01    2.83
    ## 
    ## $Albania
    ##             Estimate Std. Error t value
    ## (Intercept)   543.49       5.84   93.07
    ## ITSEX         -20.30       3.21   -6.32
    ## R-squared       0.02       0.01    3.20
    ## 
    ## $Australia
    ##             Estimate Std. Error t value
    ## (Intercept)   566.17       4.74  119.38
    ## ITSEX         -17.32       2.98   -5.81
    ## R-squared       0.01       0.00    2.96
    ## 
    ## $Austria
    ##             Estimate Std. Error t value
    ## (Intercept)   551.44       4.65  118.70
    ## ITSEX         -14.38       2.73   -5.27
    ## R-squared       0.01       0.00    2.67
    ## 
    ## $Azerbaijan
    ##             Estimate Std. Error t value
    ## (Intercept)   467.60       6.76   69.20
    ## ITSEX         -17.84       3.74   -4.77
    ## R-squared       0.01       0.00    2.36
    ## 
    ## $Bahrain
    ##             Estimate Std. Error t value
    ## (Intercept)   531.84       7.80   68.19
    ## ITSEX         -48.93       4.51  -10.85
    ## R-squared       0.05       0.01    5.71
    ## 
    ## $`Belgium (Flemish)`
    ##             Estimate Std. Error t value
    ## (Intercept)   522.49       4.58  114.15
    ## ITSEX          -7.79       2.79   -2.79
    ## R-squared       0.00       0.00    1.41
    ## 
    ## $`Belgium (French)`
    ##             Estimate Std. Error t value
    ## (Intercept)   509.45       5.72   89.03
    ## ITSEX         -10.06       3.18   -3.16
    ## R-squared       0.00       0.00    1.59
    ## 
    ## $Brazil
    ##             Estimate Std. Error t value
    ## (Intercept)   453.73       10.3   44.05
    ## ITSEX         -22.97        6.0   -3.83
    ## R-squared       0.01        0.0    1.93
    ## 
    ## $Bulgaria
    ##             Estimate Std. Error t value
    ## (Intercept)   563.00       5.66   99.46
    ## ITSEX         -15.18       3.86   -3.93
    ## R-squared       0.01       0.00    2.04
    ## 
    ## $`Canada, Newfoundland & Labrador`
    ##             Estimate Std. Error t value
    ## (Intercept)   544.69       5.80   93.90
    ## ITSEX         -14.28       4.01   -3.56
    ## R-squared       0.01       0.00    1.79
    ## 
    ## $`Canada, Alberta`
    ##             Estimate Std. Error t value
    ## (Intercept)   561.45       7.29   76.96
    ## ITSEX         -15.07       4.35   -3.47
    ## R-squared       0.01       0.01    1.76
    ## 
    ## $`Canada, British Columbia`
    ##             Estimate Std. Error t value
    ## (Intercept)   555.62       5.30  104.85
    ## ITSEX         -13.42       3.34   -4.02
    ## R-squared       0.01       0.00    2.04
    ## 
    ## $`Canada, Quebec`
    ##             Estimate Std. Error t value
    ## (Intercept)   566.87       5.67   99.96
    ## ITSEX         -10.59       3.02   -3.50
    ## R-squared       0.01       0.00    1.78
    ## 
    ## $`Chinese Taipei`
    ##             Estimate Std. Error t value
    ## (Intercept)   563.91       4.11  137.07
    ## ITSEX         -13.30       2.28   -5.84
    ## R-squared       0.01       0.00    2.94
    ## 
    ## $Croatia
    ##             Estimate Std. Error t value
    ## (Intercept)   572.44       5.55  103.06
    ## ITSEX         -10.48       3.30   -3.18
    ## R-squared       0.01       0.00    1.63
    ## 
    ## $Cyprus
    ##             Estimate Std. Error t value
    ## (Intercept)   524.17       5.00  104.88
    ## ITSEX          -8.93       2.68   -3.33
    ## R-squared       0.00       0.00    1.71
    ## 
    ## $`Czech Republic`
    ##             Estimate Std. Error t value
    ## (Intercept)   544.99       5.20  104.73
    ## ITSEX          -3.54       3.04   -1.17
    ## R-squared       0.00       0.00    0.59
    ## 
    ## $Denmark
    ##             Estimate Std. Error t value
    ## (Intercept)   557.45       4.78  116.74
    ## ITSEX         -12.43       2.98   -4.17
    ## R-squared       0.01       0.00    2.13
    ## 
    ## $`Dubai,UAE`
    ##             Estimate Std. Error t value
    ## (Intercept)   565.92       6.70   84.48
    ## ITSEX          -9.42       4.31   -2.18
    ## R-squared       0.00       0.00    1.10
    ## 
    ## $Egypt
    ##             Estimate Std. Error t value
    ## (Intercept)   402.38       9.30   43.27
    ## ITSEX         -16.04       5.61   -2.86
    ## R-squared       0.01       0.00    1.44
    ## 
    ## $England
    ##             Estimate Std. Error t value
    ## (Intercept)   572.26       6.07   94.31
    ## ITSEX          -9.87       3.74   -2.64
    ## R-squared       0.00       0.00    1.37
    ## 
    ## $Finland
    ##             Estimate Std. Error t value
    ## (Intercept)   575.71       4.63  124.38
    ## ITSEX         -17.59       2.66   -6.61
    ## R-squared       0.01       0.00    3.30
    ## 
    ## $France
    ##             Estimate Std. Error t value
    ## (Intercept)   534.69       4.80  111.30
    ## ITSEX         -13.97       2.55   -5.48
    ## R-squared       0.01       0.00    2.69
    ## 
    ## $Georgia
    ##             Estimate Std. Error t value
    ## (Intercept)   528.54       4.79  110.32
    ## ITSEX         -22.78       2.90   -7.85
    ## R-squared       0.02       0.00    4.00
    ## 
    ## $Germany
    ##             Estimate Std. Error t value
    ## (Intercept)   547.15       4.44  123.35
    ## ITSEX         -15.34       2.62   -5.85
    ## R-squared       0.01       0.00    2.94
    ## 
    ## $`Hong Kong SAR`
    ##             Estimate Std. Error t value
    ## (Intercept)   585.18       4.45  131.48
    ## ITSEX          -8.28       2.77   -2.99
    ## R-squared       0.00       0.00    1.54
    ## 
    ## $Hungary
    ##             Estimate Std. Error t value
    ## (Intercept)   561.54       5.82   96.52
    ## ITSEX         -14.75       3.37   -4.38
    ## R-squared       0.01       0.00    2.23
    ## 
    ## $`Iran, Islamic Rep. of`
    ##             Estimate Std. Error t value
    ## (Intercept)   438.66      15.55   28.21
    ## ITSEX         -16.85       9.11   -1.85
    ## R-squared       0.01       0.01    0.93
    ## 
    ## $Ireland
    ##             Estimate Std. Error t value
    ## (Intercept)   594.06       6.18   96.16
    ## ITSEX         -11.07       3.52   -3.15
    ## R-squared       0.01       0.00    1.60
    ## 
    ## $Israel
    ##             Estimate Std. Error t value
    ## (Intercept)   516.50       5.13  100.72
    ## ITSEX          -4.28       2.97   -1.44
    ## R-squared       0.00       0.00    0.75
    ## 
    ## $Italy
    ##             Estimate Std. Error t value
    ## (Intercept)   547.68       3.72  147.20
    ## ITSEX          -6.97       2.01   -3.48
    ## R-squared       0.00       0.00    1.76
    ## 
    ## $Jordan
    ##             Estimate Std. Error t value
    ## (Intercept)   434.34      15.52   27.99
    ## ITSEX         -36.03      10.31   -3.50
    ## R-squared       0.02       0.01    1.81
    ## 
    ## $Kazakhstan
    ##             Estimate Std. Error t value
    ## (Intercept)   529.46       4.45  118.92
    ## ITSEX         -17.29       2.74   -6.32
    ## R-squared       0.01       0.00    3.24
    ## 
    ## $Kosovo
    ##             Estimate Std. Error t value
    ## (Intercept)   451.62       4.89   92.37
    ## ITSEX         -20.92       3.07   -6.82
    ## R-squared       0.02       0.00    3.33
    ## 
    ## $Latvia
    ##             Estimate Std. Error t value
    ## (Intercept)   568.70       4.76  119.47
    ## ITSEX         -27.18       3.15   -8.63
    ## R-squared       0.03       0.01    4.43
    ## 
    ## $Lithuania
    ##             Estimate Std. Error t value
    ## (Intercept)   584.48       4.52  129.25
    ## ITSEX         -21.49       2.76   -7.79
    ## R-squared       0.02       0.01    3.91
    ## 
    ## $Montenegro
    ##             Estimate Std. Error t value
    ## (Intercept)   517.36       4.15  124.55
    ## ITSEX         -19.90       2.62   -7.60
    ## R-squared       0.02       0.00    3.86
    ## 
    ## $`Macedonia, Rep. of`
    ##             Estimate Std. Error t value
    ## (Intercept)   479.22       9.19   52.12
    ## ITSEX         -24.92       5.19   -4.80
    ## R-squared       0.02       0.01    2.43
    ## 
    ## $Malta
    ##             Estimate Std. Error t value
    ## (Intercept)   524.58       6.98   75.18
    ## ITSEX          -6.47       4.09   -1.58
    ## R-squared       0.00       0.00    0.80
    ## 
    ## $Morocco
    ##             Estimate Std. Error t value
    ## (Intercept)   422.73       6.48   65.24
    ## ITSEX         -33.21       3.70   -8.98
    ## R-squared       0.02       0.01    4.41
    ## 
    ## $Netherlands
    ##             Estimate Std. Error t value
    ## (Intercept)   546.20       4.71  115.85
    ## ITSEX         -12.68       2.55   -4.97
    ## R-squared       0.01       0.00    2.45
    ## 
    ## $`New Zealand`
    ##             Estimate Std. Error t value
    ## (Intercept)   550.12       5.40  101.90
    ## ITSEX         -18.95       3.18   -5.95
    ## R-squared       0.01       0.00    2.99
    ## 
    ## $`Northern Ireland`
    ##             Estimate Std. Error t value
    ## (Intercept)   601.89       5.54  108.55
    ## ITSEX         -24.34       3.42   -7.13
    ## R-squared       0.02       0.01    3.57
    ## 
    ## $Norway
    ##             Estimate Std. Error t value
    ## (Intercept)   563.23       4.06  138.81
    ## ITSEX         -16.18       2.40   -6.76
    ## R-squared       0.01       0.00    3.42
    ## 
    ## $Oman
    ##             Estimate Std. Error t value
    ## (Intercept)   482.67       6.93   69.63
    ## ITSEX         -35.55       3.84   -9.27
    ## R-squared       0.03       0.01    4.73
    ## 
    ## $Poland
    ##             Estimate Std. Error t value
    ## (Intercept)   579.81       4.73  122.66
    ## ITSEX         -20.06       2.95   -6.80
    ## R-squared       0.02       0.01    3.47
    ## 
    ## $Portugal
    ##             Estimate Std. Error t value
    ## (Intercept)   528.23       3.35  157.75
    ## ITSEX          -5.58       2.02   -2.76
    ## R-squared       0.00       0.00    1.40
    ## 
    ## $Qatar
    ##             Estimate Std. Error t value
    ## (Intercept)   509.28       7.91   64.42
    ## ITSEX         -16.53       4.98   -3.32
    ## R-squared       0.01       0.00    1.68
    ## 
    ## $`Russian Federation`
    ##             Estimate Std. Error t value
    ## (Intercept)   587.23       5.45  107.84
    ## ITSEX         -13.32       3.70   -3.60
    ## R-squared       0.01       0.00    1.84
    ## 
    ## $`Saudi Arabia`
    ##             Estimate Std. Error t value
    ## (Intercept)   498.89      10.81   46.15
    ## ITSEX         -35.39       6.77   -5.23
    ## R-squared       0.04       0.01    2.66
    ## 
    ## $Serbia
    ##             Estimate Std. Error t value
    ## (Intercept)   527.69       6.16   85.71
    ## ITSEX          -9.32       3.52   -2.65
    ## R-squared       0.00       0.00    1.35
    ## 
    ## $Singapore
    ##             Estimate Std. Error t value
    ## (Intercept)   614.04       4.32  142.24
    ## ITSEX         -17.78       2.72   -6.53
    ## R-squared       0.01       0.00    3.48
    ## 
    ## $`Slovak Republic`
    ##             Estimate Std. Error t value
    ## (Intercept)   541.47       4.65  116.48
    ## ITSEX          -8.36       2.78   -3.01
    ## R-squared       0.00       0.00    1.54
    ## 
    ## $Slovenia
    ##             Estimate Std. Error t value
    ## (Intercept)   547.25       3.77  144.98
    ## ITSEX         -18.25       2.35   -7.77
    ## R-squared       0.02       0.00    3.91
    ## 
    ## $`South Africa`
    ##             Estimate Std. Error t value
    ## (Intercept)   374.27       6.15   60.87
    ## ITSEX         -57.07       3.55  -16.07
    ## R-squared       0.05       0.01    8.27
    ## 
    ## $Spain
    ##             Estimate Std. Error t value
    ## (Intercept)   524.17       4.57  114.62
    ## ITSEX          -1.94       2.64   -0.73
    ## R-squared       0.00       0.00    0.37
    ## 
    ## $Sweden
    ##             Estimate Std. Error t value
    ## (Intercept)   566.00       4.24  133.56
    ## ITSEX         -14.96       2.31   -6.48
    ## R-squared       0.01       0.00    3.25
    ## 
    ## $Turkey
    ##             Estimate Std. Error t value
    ## (Intercept)   522.14       5.61   93.00
    ## ITSEX         -17.04       2.85   -5.99
    ## R-squared       0.01       0.00    2.99
    ## 
    ## $`United Arab Emirates`
    ##             Estimate Std. Error t value
    ## (Intercept)   525.75       7.48   70.33
    ## ITSEX         -28.67       5.22   -5.49
    ## R-squared       0.01       0.00    2.79
    ## 
    ## $`United States`
    ##             Estimate Std. Error t value
    ## (Intercept)   558.59       9.58   58.28
    ## ITSEX          -7.41       4.38   -1.69
    ## R-squared       0.00       0.00    0.84
    ## 
    ## $Uzbekistan
    ##             Estimate Std. Error t value
    ## (Intercept)   473.27       5.50   86.11
    ## ITSEX         -24.04       3.36   -7.15
    ## R-squared       0.02       0.01    3.61
    ## 
    ## $`Macao SAR`
    ##             Estimate Std. Error t value
    ## (Intercept)   550.21       3.32  165.52
    ## ITSEX          -9.77       2.23   -4.37
    ## R-squared       0.00       0.00    2.18
    ## 
    ## $`Moscow City, Russian Fed.`
    ##             Estimate Std. Error t value
    ## (Intercept)   615.05       3.44  178.55
    ## ITSEX         -11.17       2.08   -5.36
    ## R-squared       0.01       0.00    2.72
    ## 
    ## $`South Africa (6)`
    ##             Estimate Std. Error t value
    ## (Intercept)   458.10       6.51   70.35
    ## ITSEX         -49.75       3.87  -12.86
    ## R-squared       0.04       0.01    6.53
