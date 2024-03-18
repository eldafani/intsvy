Examples with TIMSS 2019
================

``` r
library("devtools")
install_github("eldafani/intsvy")
library("intsvy")
```

## Grade 4

### Read data

The object dir specifies the directory path where the TIMSS 2019 data is
located (eg. “/home/data”). Variable selection can be done with aid of
timssg4.var.label(dir)

``` r
timss <- timssg4.select.merge(folder= dir,
         student= c("ITSEX", "ASBG05A", "ASBG05B", "ASBG05C", "ASBG05D",
                    "ASBG03"), 
         home= c("ASBH14", "ASBH10", "ASDHEDUP", "ASDHOCCP", "ASBH17A", "ASBH17B"),
         school= c("ACBG05B"), 
         countries = c("ARE", "CHL", "CAN", "HUN"))
```

### Calculate mean math performance by education system

``` r
timss.mean.pv(pvlabel=paste0("ASMMAT0", 1:5), by= "IDCNTRYL", data=timss)
```

    ##               IDCNTRYL  Freq   Mean s.e.    SD  s.e
    ## 1               Canada 13653 511.56 1.86 76.24 1.19
    ## 2                Chile  4174 440.97 2.72 74.92 1.59
    ## 3              Hungary  4571 523.43 2.64 77.62 1.45
    ## 4 United Arab Emirates 25834 481.39 1.71 98.53 0.87

### Calculate mean math performance by education system and student’s sex

``` r
timss.mean.pv(pvlabel= paste0("ASMMAT0", 1:5), by= c("IDCNTRYL", "ITSEX"), data=timss)
```

    ##                IDCNTRYL ITSEX  Freq   Mean  s.e.     SD   s.e
    ## 1                Canada     1  6731 501.92  2.54  74.27  1.50
    ## 2                Canada     2  6884 520.79  1.95  76.96  1.32
    ## 3                Canada  <NA>    38 496.52 12.87  73.07 10.20
    ## 4                 Chile     1  2076 436.66  3.43  73.16  1.94
    ## 5                 Chile     2  2098 445.21  3.13  76.37  2.18
    ## 6               Hungary     1  2212 517.63  3.03  75.67  1.93
    ## 7               Hungary     2  2359 528.82  3.06  79.02  1.79
    ## 8  United Arab Emirates     1 12885 477.43  2.53  95.42  1.07
    ## 9  United Arab Emirates     2 12879 485.60  2.27 101.43  1.30
    ## 10 United Arab Emirates  <NA>    70 441.38 13.20  82.65 12.13

### International benchmarks by education system

``` r
intsvy.ben.pv(pvnames= paste0("ASMMAT0", 1:5), cutoff =   c(400, 475, 550, 625), 
              by= c("IDCNTRYL", "ITSEX"), data=timss, config = timss4_conf)
```

    ##                IDCNTRYL ITSEX       Benchmark Percentage Std. err.
    ## 1                Canada     1 At or above 400      90.94      0.76
    ## 2                Canada     1 At or above 475      64.75      1.37
    ## 3                Canada     1 At or above 550      26.60      1.37
    ## 4                Canada     1 At or above 625       4.33      0.70
    ## 5                Canada     2 At or above 400      93.79      0.58
    ## 6                Canada     2 At or above 475      72.52      1.14
    ## 7                Canada     2 At or above 550      36.61      1.23
    ## 8                Canada     2 At or above 625       8.38      0.82
    ## 9                Canada  <NA> At or above 400      93.11      4.66
    ## 10               Canada  <NA> At or above 475      62.04     10.31
    ## 11               Canada  <NA> At or above 550      19.92      9.81
    ## 12               Canada  <NA> At or above 625       5.50      4.61
    ## 13                Chile     1 At or above 400      68.78      2.04
    ## 14                Chile     1 At or above 475      30.42      1.85
    ## 15                Chile     1 At or above 550       6.01      0.72
    ## 16                Chile     1 At or above 625       0.43      0.13
    ## 17                Chile     2 At or above 400      71.95      1.81
    ## 18                Chile     2 At or above 475      35.61      1.69
    ## 19                Chile     2 At or above 550       8.57      0.85
    ## 20                Chile     2 At or above 625       0.77      0.21
    ## 21              Hungary     1 At or above 400      93.05      1.00
    ## 22              Hungary     1 At or above 475      71.58      1.72
    ## 23              Hungary     1 At or above 550      35.07      1.55
    ## 24              Hungary     1 At or above 625       7.33      0.94
    ## 25              Hungary     2 At or above 400      93.43      0.98
    ## 26              Hungary     2 At or above 475      75.56      1.56
    ## 27              Hungary     2 At or above 550      41.93      1.71
    ## 28              Hungary     2 At or above 625      10.63      0.94
    ## 29 United Arab Emirates     1 At or above 400      78.53      0.89
    ## 30 United Arab Emirates     1 At or above 475      51.68      1.28
    ## 31 United Arab Emirates     1 At or above 550      23.57      0.90
    ## 32 United Arab Emirates     1 At or above 625       5.79      0.45
    ## 33 United Arab Emirates     2 At or above 400      78.45      0.98
    ## 34 United Arab Emirates     2 At or above 475      55.26      1.04
    ## 35 United Arab Emirates     2 At or above 550      28.78      0.87
    ## 36 United Arab Emirates     2 At or above 625       7.94      0.40
    ## 37 United Arab Emirates  <NA> At or above 400      73.02      7.56
    ## 38 United Arab Emirates  <NA> At or above 475      37.10      7.11
    ## 39 United Arab Emirates  <NA> At or above 550       7.80      4.02
    ## 40 United Arab Emirates  <NA> At or above 625       0.32      0.53

### International benchmarks by education system and student’s sex

``` r
intsvy.ben.pv(pvnames= paste0("ASMMAT0", 1:5), cutoff =   c(400, 475, 550, 625), 
              by= c("IDCNTRYL", "ITSEX"), data=timss, config = timss4_conf)
```

    ##                IDCNTRYL ITSEX       Benchmark Percentage Std. err.
    ## 1                Canada     1 At or above 400      90.94      0.76
    ## 2                Canada     1 At or above 475      64.75      1.37
    ## 3                Canada     1 At or above 550      26.60      1.37
    ## 4                Canada     1 At or above 625       4.33      0.70
    ## 5                Canada     2 At or above 400      93.79      0.58
    ## 6                Canada     2 At or above 475      72.52      1.14
    ## 7                Canada     2 At or above 550      36.61      1.23
    ## 8                Canada     2 At or above 625       8.38      0.82
    ## 9                Canada  <NA> At or above 400      93.11      4.66
    ## 10               Canada  <NA> At or above 475      62.04     10.31
    ## 11               Canada  <NA> At or above 550      19.92      9.81
    ## 12               Canada  <NA> At or above 625       5.50      4.61
    ## 13                Chile     1 At or above 400      68.78      2.04
    ## 14                Chile     1 At or above 475      30.42      1.85
    ## 15                Chile     1 At or above 550       6.01      0.72
    ## 16                Chile     1 At or above 625       0.43      0.13
    ## 17                Chile     2 At or above 400      71.95      1.81
    ## 18                Chile     2 At or above 475      35.61      1.69
    ## 19                Chile     2 At or above 550       8.57      0.85
    ## 20                Chile     2 At or above 625       0.77      0.21
    ## 21              Hungary     1 At or above 400      93.05      1.00
    ## 22              Hungary     1 At or above 475      71.58      1.72
    ## 23              Hungary     1 At or above 550      35.07      1.55
    ## 24              Hungary     1 At or above 625       7.33      0.94
    ## 25              Hungary     2 At or above 400      93.43      0.98
    ## 26              Hungary     2 At or above 475      75.56      1.56
    ## 27              Hungary     2 At or above 550      41.93      1.71
    ## 28              Hungary     2 At or above 625      10.63      0.94
    ## 29 United Arab Emirates     1 At or above 400      78.53      0.89
    ## 30 United Arab Emirates     1 At or above 475      51.68      1.28
    ## 31 United Arab Emirates     1 At or above 550      23.57      0.90
    ## 32 United Arab Emirates     1 At or above 625       5.79      0.45
    ## 33 United Arab Emirates     2 At or above 400      78.45      0.98
    ## 34 United Arab Emirates     2 At or above 475      55.26      1.04
    ## 35 United Arab Emirates     2 At or above 550      28.78      0.87
    ## 36 United Arab Emirates     2 At or above 625       7.94      0.40
    ## 37 United Arab Emirates  <NA> At or above 400      73.02      7.56
    ## 38 United Arab Emirates  <NA> At or above 475      37.10      7.11
    ## 39 United Arab Emirates  <NA> At or above 550       7.80      4.02
    ## 40 United Arab Emirates  <NA> At or above 625       0.32      0.53
