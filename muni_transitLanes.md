Analysis of Transit Only Lane Violations for SFMTA
================
Anthony Chau
07-10-2018

``` r
muni <- read.csv('muni_transitLanes.csv', strip.white = TRUE)
muni <- as.data.frame(muni)

# Check variables in muni
str(muni)
```

    ## 'data.frame':    17178 obs. of  18 variables:
    ##  $ Object.ID           : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Ticket.Number       : int  100029164 100029166 100029167 100000050 100029169 100029170 100029171 100029172 100029173 100029174 ...
    ##  $ Citation.Issue.Date : Factor w/ 1764 levels "1/1/2010","1/1/2011",..: 1293 1293 1293 1299 1299 1299 1299 1299 1299 1299 ...
    ##  $ Citaton.Issue.Month : logi  NA NA NA NA NA NA ...
    ##  $ Citation.Issue.Time : Factor w/ 816 levels "02:38","04:04",..: 639 513 513 735 265 383 810 387 712 733 ...
    ##  $ Location            : Factor w/ 3287 levels "04 04TH  ST",..: 2289 1401 1401 3110 1728 1684 29 3056 42 2148 ...
    ##  $ Violation.Code      : Factor w/ 11 levels "NO VIOL","T32A.1",..: 9 9 9 10 9 9 9 10 2 10 ...
    ##  $ Violation           : Factor w/ 11 levels "BUS ZONE","DBL PARK",..: 2 2 2 1 2 2 2 1 10 1 ...
    ##  $ Fine.Amount         : int  65 65 65 250 65 65 65 250 60 250 ...
    ##  $ Citation.Status     : Factor w/ 3 levels "Closed","Open",..: 1 2 1 1 1 1 1 1 2 2 ...
    ##  $ Amount.Paid         : Factor w/ 168 levels "$                  -",..: 6 151 151 6 151 151 6 89 159 6 ...
    ##  $ Amount.Due          : num  0 85 0 0 0 0 0 0 60 275 ...
    ##  $ Suspend.Code        : Factor w/ 74 levels "","10 10 2HR XCD",..: 57 56 56 57 56 56 67 71 56 1 ...
    ##  $ Suspend.Process.Date: Factor w/ 2035 levels "","01/01/2010 12:00:00 AM +0000",..: 1096 1079 1079 1103 1079 1079 1616 1178 1079 1 ...
    ##  $ Suspend.Until.Date  : Factor w/ 1949 levels "","01/01/2010 12:00:00 AM +0000",..: 1936 1168 1168 1936 1168 1168 560 1751 1168 1 ...
    ##  $ Disposition.Code    : Factor w/ 24 levels "","1 RU RV UPHD",..: 1 1 1 1 1 1 7 1 1 1 ...
    ##  $ Last.Edited.Date    : logi  NA NA NA NA NA NA ...
    ##  $ Geom                : Factor w/ 3020 levels "","(37.710799135, -122.447754977)",..: 761 1065 1065 1060 1468 1482 969 1091 964 1409 ...

``` r
summary(muni)
```

    ##    Object.ID     Ticket.Number       Citation.Issue.Date
    ##  Min.   :    1   Min.   :        0   9/25/2012:   59    
    ##  1st Qu.: 4295   1st Qu.:794816108   7/19/2010:   54    
    ##  Median : 8590   Median :814718448   8/20/2012:   50    
    ##  Mean   : 8590   Mean   :799193094   9/20/2012:   50    
    ##  3rd Qu.:12884   3rd Qu.:831587312   9/6/2012 :   50    
    ##  Max.   :17178   Max.   :977404142   7/28/2011:   48    
    ##                                      (Other)  :16867    
    ##  Citaton.Issue.Month Citation.Issue.Time          Location    
    ##  Mode:logical        16:16  :  212       593 POST ST  :   53  
    ##  NA's:17178          16:12  :  206       506 SUTTER ST:   51  
    ##                      16:06  :  194       695 POST ST  :   49  
    ##                      16:09  :  191       948 SUTTER ST:   46  
    ##                      16:13  :  183       906 SUTTER ST:   42  
    ##                      16:15  :  181       795 POST ST  :   40  
    ##                      (Other):16011       (Other)      :16897  
    ##    Violation.Code      Violation     Fine.Amount     Citation.Status 
    ##  T32A.1   :5334   TWAWY ZN#1:5334   Min.   :  0.0   Closed   :15375  
    ##  V22500H  :2995   DBL PARK  :2995   1st Qu.: 83.0   Open     : 1719  
    ##  TRC7.2.40:2535   PRK PROHIB:2535   Median : 85.0   Unapplied:   84  
    ##  V22500I  :2433   BUS ZONE  :2433   Mean   :112.7                    
    ##  T32A.2   :2305   TWAWY ZONE:2305   3rd Qu.: 98.0                    
    ##  TRC7.2.41:1498   PK PHB OTD:1498   Max.   :279.0                    
    ##  (Other)  :  78   (Other)   :  78                                    
    ##                   Amount.Paid     Amount.Due            Suspend.Code  
    ##  $85.00                 :2381   Min.   :  0.0   730 730TRNLN  :10898  
    ##  $110.00                :1214   1st Qu.:  0.0                 : 1876  
    ##  $                    - :1150   Median :  0.0   731 731MP EXP : 1279  
    ##  $95.00                 :1012   Mean   : 18.9   22 22 GEN VAL :  439  
    ##  $83.00                 : 869   3rd Qu.:  0.0   119 TOWZVLD   :  428  
    ##  $                     -: 846   Max.   :392.0   803 803ELIGREF:  406  
    ##  (Other)                :9706                   (Other)       : 1852  
    ##                    Suspend.Process.Date
    ##                              : 1879    
    ##  08/23/2010 12:00:00 AM +0000:  279    
    ##  09/25/2012 12:00:00 AM +0000:   49    
    ##  03/30/2013 12:00:00 AM +0000:   48    
    ##  11/03/2012 12:00:00 AM +0000:   46    
    ##  09/28/2012 12:00:00 AM +0000:   45    
    ##  (Other)                     :14832    
    ##                     Suspend.Until.Date      Disposition.Code
    ##                              : 1879                 :15382  
    ##  12/30/2099 12:00:00 AM +0000:  918    26 COLFFDMV  :  366  
    ##  12/30/1999 12:00:00 AM +0000:  681    25 OFCR ERROR:  305  
    ##  11/21/2010 12:00:00 AM +0000:  276    1 RU RV UPHD :  302  
    ##  10/19/2012 12:00:00 AM +0000:   47    6 HD HR DNID :  212  
    ##  10/16/2012 12:00:00 AM +0000:   46    21 MAIL DELAY:  164  
    ##  (Other)                     :13331    (Other)      :  447  
    ##  Last.Edited.Date                              Geom      
    ##  Mode:logical     (37.7878276666, -122.411554908):   53  
    ##  NA's:17178       (37.7892799831, -122.408766174):   51  
    ##                   (37.7876142157, -122.413245749):   49  
    ##                   (37.7883576057, -122.416017457):   46  
    ##                   (37.7884422191, -122.415356011):   42  
    ##                   (37.7874052933, -122.414882672):   40  
    ##                   (Other)                        :16897

Cleaning the Data
=================

First, we remove the Citation.Issue.Month column from the data frame since the month is already included in the Citation.Issue.Date column. And, we remove the Last.Edited.Date column because this is not relevant for our analysis.

``` r
muni$Citaton.Issue.Month <- NULL
muni$Last.Edited.Date <- NULL
```

Now, let's focus our attention on the date and time data within this dataset.

Initially, the citation issue date and time were stored as factor variables. We collapse date and time into a single column and convert it to a datetime object. This will make the data easier to work with in our analysis.

``` r
# Convert citation dates and times to a Datetime object

muni$Citation.Date <- strptime(paste(muni$Citation.Issue.Date, 
                                            muni$Citation.Issue.Time), 
                                      format ='%m/%d/%Y %H:%M')

# Drop original citation issue date and time

muni$Citation.Issue.Date <- NULL
muni$Citation.Issue.Time <- NULL
```

We want to collaspe observations in the location column to only record a unique factor level for a street name.

``` r
library(stringr)

muni$Location <- as.character(muni$Location)

muni$Location <- str_replace_all(muni$Location, "[:punct:]", "")

### Testing code

# Split location into street number and street name
# Logic: If a word boundary is proceeded by a digit, split the word

splitAddress <- strsplit(muni$Location, "(?<=\\d)\\b ", perl=T)

# Create new matrix with street name column and street name column
y <- do.call(rbind, splitAddress)
y <- as.data.frame(y)


# Change column names
colnames(y) <- c('Street.Number', 'Street.Name')

# Make new column in muni df
muni$Street.Name <- y$Street.Name

# Check df
str(muni)
```

    ## 'data.frame':    17178 obs. of  16 variables:
    ##  $ Object.ID           : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Ticket.Number       : int  100029164 100029166 100029167 100000050 100029169 100029170 100029171 100029172 100029173 100029174 ...
    ##  $ Location            : chr  "643 OFARRELL" "372 OFARRELL" "372 OFARRELL" "924 GEARY" ...
    ##  $ Violation.Code      : Factor w/ 11 levels "NO VIOL","T32A.1",..: 9 9 9 10 9 9 9 10 2 10 ...
    ##  $ Violation           : Factor w/ 11 levels "BUS ZONE","DBL PARK",..: 2 2 2 1 2 2 2 1 10 1 ...
    ##  $ Fine.Amount         : int  65 65 65 250 65 65 65 250 60 250 ...
    ##  $ Citation.Status     : Factor w/ 3 levels "Closed","Open",..: 1 2 1 1 1 1 1 1 2 2 ...
    ##  $ Amount.Paid         : Factor w/ 168 levels "$                  -",..: 6 151 151 6 151 151 6 89 159 6 ...
    ##  $ Amount.Due          : num  0 85 0 0 0 0 0 0 60 275 ...
    ##  $ Suspend.Code        : Factor w/ 74 levels "","10 10 2HR XCD",..: 57 56 56 57 56 56 67 71 56 1 ...
    ##  $ Suspend.Process.Date: Factor w/ 2035 levels "","01/01/2010 12:00:00 AM +0000",..: 1096 1079 1079 1103 1079 1079 1616 1178 1079 1 ...
    ##  $ Suspend.Until.Date  : Factor w/ 1949 levels "","01/01/2010 12:00:00 AM +0000",..: 1936 1168 1168 1936 1168 1168 560 1751 1168 1 ...
    ##  $ Disposition.Code    : Factor w/ 24 levels "","1 RU RV UPHD",..: 1 1 1 1 1 1 7 1 1 1 ...
    ##  $ Geom                : Factor w/ 3020 levels "","(37.710799135, -122.447754977)",..: 761 1065 1065 1060 1468 1482 969 1091 964 1409 ...
    ##  $ Citation.Date       : POSIXlt, format: "2008-07-10 17:33:00" "2008-07-10 15:27:00" ...
    ##  $ Street.Name         : Factor w/ 50 levels " 3RD ST"," 4TH ST",..: 31 31 31 17 17 17 17 17 17 17 ...

``` r
# Check unique street names
unique(muni$Street.Name)
```

    ##  [1] OFARRELL        GEARY           MISSION         POST ST        
    ##  [5] GEARY ST        3RD ST          SUTTER          OFARRELL ST    
    ##  [9] SACRAMENTO      MISSION ST      MAIN             3RD ST        
    ## [13] CLAY            STOCKTON        POST            4TH ST         
    ## [17] MARKET          OFARRELL STREET OFALLELL        SUTTER ST      
    ## [21] 04TH ST         STOCKTON ST     04TH  ST         OFARRELL ST   
    ## [25] SACRAMENTO ST   03RD ST          4TH ST          GEARY ST      
    ## [29] O4TH STREET     566MARKET ST    POTRERO AVE     MISSION STREET 
    ## [33] MARKET ST       GEARY BLVD      SAN BRUNO AVE   22ND ST        
    ## [37] CLAY ST         HAIGHT ST       TOWNSEND ST     SAN JOSE AVE   
    ## [41] STEUART ST      SCOTT ST        CHESTNUT ST     KEARNY ST      
    ## [45] STOCKTON TUNL   FOLSOM          BUSH ST         TRUMBWELL      
    ## [49] WILDE           KEARNY         
    ## 50 Levels:  3RD ST  4TH ST  GEARY ST  OFARRELL ST 03RD ST ... WILDE

``` r
# Trim leading white space
trim.leading <- function (x)  sub("^\\s+", "", x)
muni$Street.Name <- trim.leading(muni$Street.Name)

# Refactor duplicate/misspelled streets
# Find more efficient way of doing this
muni$Street.Name[muni$Street.Name %in% c("GEARY", "GEARY ST")] <- "GEARY BLVD"
muni$Street.Name[muni$Street.Name %in% c("OFARRELL", "OFALLELL", 
                                         "OFARRELL ST", 
                                         "OFARRELL STREET")] <- "O'FARRELL ST"
muni$Street.Name[muni$Street.Name %in% c("04TH ST", "04TH  ST",
                                         "O4TH STREET")] <- "4TH ST"
muni$Street.Name[muni$Street.Name %in% c("03RD ST")] <- "3RD ST"
muni$Street.Name[muni$Street.Name %in% c("MISSION")] <- "MISSION ST"
muni$Street.Name[muni$Street.Name %in% c("MARKET", "566MARKET ST")] <- "MARKET ST"
muni$Street.Name[muni$Street.Name %in% c("STOCKTON", 
                                         "STOCKTON TUNL")] <- "STOCKTON ST"
muni$Street.Name[muni$Street.Name %in% c("SACRAMENTO")] <- "SACRAMENTO ST"
muni$Street.Name[muni$Street.Name %in% c("POST")] <- "POST ST"
muni$Street.Name[muni$Street.Name %in% c("KEARNY")] <- "KEARNY ST"
muni$Street.Name[muni$Street.Name %in% c("SUTTER")] <- "SUTTER ST"
muni$Street.Name[muni$Street.Name %in% c("CLAY")] <- "CLAY ST"
muni$Street.Name[muni$Street.Name %in% c("MAIN")] <- "MAIN ST"
muni$Street.Name[muni$Street.Name %in% c("FOLSOM")] <- "FOLSOM ST"
muni$Street.Name[muni$Street.Name %in% c("TRUMBWELL")] <- "TRUMBWELL ST"
muni$Street.Name[muni$Street.Name %in% c("WILDE")] <- "WILDE ST"

unique(muni$Street.Name)
```

    ##  [1] "O'FARRELL ST"   "GEARY BLVD"     "MISSION ST"     "POST ST"       
    ##  [5] "3RD ST"         "SUTTER ST"      "SACRAMENTO ST"  "MAIN ST"       
    ##  [9] "CLAY ST"        "STOCKTON ST"    "4TH ST"         "MARKET ST"     
    ## [13] "POTRERO AVE"    "MISSION STREET" "SAN BRUNO AVE"  "22ND ST"       
    ## [17] "HAIGHT ST"      "TOWNSEND ST"    "SAN JOSE AVE"   "STEUART ST"    
    ## [21] "SCOTT ST"       "CHESTNUT ST"    "KEARNY ST"      "FOLSOM ST"     
    ## [25] "BUSH ST"        "TRUMBWELL ST"   "WILDE ST"
