Stats 245 Final Project
================
Anthony Chau
November 25, 2018

-   [**Abstract**](#abstract)
-   [**Introduction**](#introduction)
-   [**Statistical Methods**](#statistical-methods)
    -   [**Exploratory Data Analysis**](#exploratory-data-analysis)
    -   [**Model Building**](#model-building)
    -   [**Model Diagonstics**](#model-diagonstics)
-   [**Results**](#results)
-   [**Discussion**](#discussion)

**Abstract**
============

A vector ARMA model is fit to a multivariate time series of the price (in dollars) of a Big Mac for 4 countries: United States, Japan, Brazil, and Russia. The fitted model implies that the price of a Big Mac in the United States is negatively influenced by the lagged price of a Big Mac in the United States and Brazil and positively influencd by the lagged price of a Big Mac in Japan and Russia. Similar interpretations for the price of a Big Mac in Japan, Brazil, and Russia dependent on the lagged prices in other countries can also be determined. The fitted model indicates that most estimated coefficients are insignificant. However, this result may be because of the sparsity of each time series. Lastly, residual diagnostics indicate that each individual time series are not white noise.

**Introduction**
================

The Big Mac Index is a term popularized by *The Economist* as a means of measuring the purchasing power parity. The idea is that the exchange rates between two countries can be determined by the ratio of the value of goods that can be bought with local currency in each country. *The Economist* has used the index to determine if a currency is overvalued or undervalued compared to the United States Dollar.

The dataset provided by *The Economist* contains biyearly data on 37 countries from 2011-2018. Each observation in the dataset contains the country, local price of a Big Mac, the dollar price of a Big Mac, various adjusted prices, and computed exchange rates with common currencies. *The Economist* also provides a "full" version of this dataset, which contains older data but the time measurements are inconsistent. For this reason, we use the "adjusted" version of the data.

We will investigate the relationship between local prices (in Dollars) of a Big Mac in 4 countries: United States, Japan, Brazil, and Russia. Because we are focusing on 4 countries, we will use a multivariate vector ARMA model to model 4 seperate time series at once. After running the model, we interpret the cross-correlation coefficients between different countries computed by R. The cross-corrleation coefficients measure the influence of the local price (in Dollars) of a Big Mac in one country on the local price (in Dollars) of a Big Mac in a different country.

**Statistical Methods**
=======================

**Exploratory Data Analysis**
-----------------------------

First, we plot the local price (in Dollars) of a Big Mac for each country. We observe that across our chosen countries, the local price of a Big Mac has been increasing steadily only in the United States. So we remove this upward trend by taking the first difference for the United States series. For the other countries, we assume stationarity since the ACF of the prices exhibit exponential decay.

We extract the local price (in Dollars) from the 4 chosen countries and plot the ACF and PACF of the time series for each country. We notice that the ACF tails off and the PACF cuts off after the first lag for each country. This observation suggests that we will include a autoregressive term of order 1 in our final model.

``` r
# load data
bigmac <- read.csv('big-mac-adjusted-index.csv')

# convert date column to Date variable
bigmac$date <- as.Date(bigmac$date)


singleCountry <- function(country){
  subset(bigmac, name == country, 
         select = c("date", "dollar_price"))
}

## Japanese Yen 
japan <- singleCountry("Japan")
japan$dollar_price <- ts(japan$dollar_price)

# plot of local price (in Dollars) of Big Mac
plot(japan$dollar_price, xlab = "Year", ylab = "Local Price (in Dollars) of Big Mac", 
     main = "Japan", xaxt = "n")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = 2010:2018)
```

![](bigmac_files/figure-markdown_github/EDA-1.png)

``` r
# acf/pacf
acf(japan$dollar_price, ylab = "ACF Series for Local Price (in Dollars) of Big Mac", main = "Japan")
```

![](bigmac_files/figure-markdown_github/EDA-2.png)

``` r
pacf(japan$dollar_price, ylab = "PACF Series for Local Price (in Dollars) of Big Mac", main = "Japan")
```

![](bigmac_files/figure-markdown_github/EDA-3.png)

``` r
## US Dollar
usa <- singleCountry("United States")
usa$dollar_price <- ts(usa$dollar_price)

# plot of US price of Big Mac
plot(usa$dollar_price, xlab = "Year", ylab = "Local Price (USD) of Big Mac", 
     main = "USA", xaxt = "n")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = 2010:2018)
```

![](bigmac_files/figure-markdown_github/EDA-4.png)

``` r
# plot of first difference of US price of Big Mac
diffUSA <- diff(usa$dollar_price)
plot(diffUSA, xlab = "Year", ylab = "Differenced Local Price (USD) of Big Mac", 
     main = "USA", xaxt = "n")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = 2010:2018)
```

![](bigmac_files/figure-markdown_github/EDA-5.png)

``` r
# acf/pacf
acf(diffUSA, ylab = "ACF Series for Differenced Local Price (USD) of Big Mac", 
main = "United States")
```

![](bigmac_files/figure-markdown_github/EDA-6.png)

``` r
pacf(diffUSA, ylab = "PACF Series for Differenced Local Price (USD) of Big Mac", main = "United States")
```

![](bigmac_files/figure-markdown_github/EDA-7.png)

``` r
## Brazil real

brazil <- singleCountry("Brazil")
brazil$dollar_price <- ts(brazil$dollar_price)

# plot of Brazilian price (in Dollars) of Big Mac
plot(brazil$dollar_price, xlab = "Year", ylab = "Local Price (in Dollars) of Big Mac", 
     main = "Brazil", xaxt = "n")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = 2010:2018)
```

![](bigmac_files/figure-markdown_github/EDA-8.png)

``` r
# acf/pacf
acf(brazil$dollar_price, ylab = "ACF Series for Local Price (in Dollars) of Big Mac", 
main = "Brazil")
```

![](bigmac_files/figure-markdown_github/EDA-9.png)

``` r
pacf(brazil$dollar_price, ylab = "PACF Series for Local Price (in Dollars) of Big Mac", 
main = "Brazil")
```

![](bigmac_files/figure-markdown_github/EDA-10.png)

``` r
## Russian ruble

russia <- singleCountry("Russia")
russia$dollar_price <- ts(russia$dollar_price)

# plot of Russian price (in Dollars) of Big Mac
plot(russia$dollar_price, xlab = "Year", ylab = "Local Price (in Dollars) of Big Mac", 
     main = "Russia", xaxt = "n")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = 2010:2018)
```

![](bigmac_files/figure-markdown_github/EDA-11.png)

``` r
# acf/pacf
acf(russia$dollar_price, ylab = "ACF Series for Local Price (in Dollars) of Big Mac", 
main = "Russia")
```

![](bigmac_files/figure-markdown_github/EDA-12.png)

``` r
pacf(brazil$dollar_price, ylab = "PACF Series for Local Price (in Dollars) of Big Mac", 
main = "Russia")
```

![](bigmac_files/figure-markdown_github/EDA-13.png)

**Model Building**
------------------

We fit a vector ARMA model because we want to model multiple time series together to determine if there are any relationships between pairs of time series. Looking at an individual time series is less interesting for this dataset because we would not be able to answer questions about the relationships between the prices of Big Macs between countries. We only consider the VAR model of order 1 because we do not have many observations for each time series.

``` r
set.seed(1234)
library(vars)
```

    ## Warning: package 'vars' was built under R version 3.4.4

    ## Warning: package 'strucchange' was built under R version 3.4.4

    ## Warning: package 'zoo' was built under R version 3.4.4

    ## Warning: package 'sandwich' was built under R version 3.4.2

    ## Warning: package 'urca' was built under R version 3.4.4

    ## Warning: package 'lmtest' was built under R version 3.4.4

``` r
# combine differenced prices of all countries
allCountries <- cbind(japan$dollar_price, diffUSA, brazil$dollar_price, russia$dollar_price)

# handle NA value with US difference prices
allCountries[1,2] <- 0


# fit vector ARMA of order 1
vectorFit <- VAR(allCountries, p = 1, type = "both")

summary(vectorFit)
```

    ## 
    ## VAR Estimation Results:
    ## ========================= 
    ## Endogenous variables: japan.dollar_price, diffUSA, brazil.dollar_price, russia.dollar_price 
    ## Deterministic variables: both 
    ## Sample size: 14 
    ## Log Likelihood: 16.658 
    ## Roots of the characteristic polynomial:
    ## 0.6968 0.5914 0.3083 0.3083
    ## Call:
    ## VAR(y = allCountries, p = 1, type = "both")
    ## 
    ## 
    ## Estimation results for equation japan.dollar_price: 
    ## =================================================== 
    ## japan.dollar_price = japan.dollar_price.l1 + diffUSA.l1 + brazil.dollar_price.l1 + russia.dollar_price.l1 + const + trend 
    ## 
    ##                          Estimate Std. Error t value Pr(>|t|)  
    ## japan.dollar_price.l1   0.6198021  0.2880338   2.152   0.0636 .
    ## diffUSA.l1             -1.3939477  1.1561721  -1.206   0.2624  
    ## brazil.dollar_price.l1 -0.2448830  0.2040141  -1.200   0.2643  
    ## russia.dollar_price.l1  0.4496231  0.3321998   1.353   0.2129  
    ## const                   1.6412199  1.1799616   1.391   0.2017  
    ## trend                   0.0003147  0.0268957   0.012   0.9909  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.3109 on 8 degrees of freedom
    ## Multiple R-Squared: 0.546,   Adjusted R-squared: 0.2622 
    ## F-statistic: 1.924 on 5 and 8 DF,  p-value: 0.1958 
    ## 
    ## 
    ## Estimation results for equation diffUSA: 
    ## ======================================== 
    ## diffUSA = japan.dollar_price.l1 + diffUSA.l1 + brazil.dollar_price.l1 + russia.dollar_price.l1 + const + trend 
    ## 
    ##                          Estimate Std. Error t value Pr(>|t|)   
    ## japan.dollar_price.l1   0.0003337  0.0533118   0.006  0.99516   
    ## diffUSA.l1             -0.9322096  0.2139945  -4.356  0.00242 **
    ## brazil.dollar_price.l1 -0.0843874  0.0377607  -2.235  0.05588 . 
    ## russia.dollar_price.l1  0.1977810  0.0614864   3.217  0.01230 * 
    ## const                   0.1505580  0.2183976   0.689  0.51008   
    ## trend                   0.0026319  0.0049781   0.529  0.61137   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.05754 on 8 degrees of freedom
    ## Multiple R-Squared: 0.7338,  Adjusted R-squared: 0.5674 
    ## F-statistic:  4.41 on 5 and 8 DF,  p-value: 0.03169 
    ## 
    ## 
    ## Estimation results for equation brazil.dollar_price: 
    ## ==================================================== 
    ## brazil.dollar_price = japan.dollar_price.l1 + diffUSA.l1 + brazil.dollar_price.l1 + russia.dollar_price.l1 + const + trend 
    ## 
    ##                        Estimate Std. Error t value Pr(>|t|)
    ## japan.dollar_price.l1   0.07904    0.54863   0.144    0.889
    ## diffUSA.l1              2.02826    2.20220   0.921    0.384
    ## brazil.dollar_price.l1  0.06398    0.38859   0.165    0.873
    ## russia.dollar_price.l1  0.64719    0.63275   1.023    0.336
    ## const                   3.01524    2.24752   1.342    0.217
    ## trend                  -0.02831    0.05123  -0.553    0.596
    ## 
    ## 
    ## Residual standard error: 0.5922 on 8 degrees of freedom
    ## Multiple R-Squared: 0.4862,  Adjusted R-squared: 0.1651 
    ## F-statistic: 1.514 on 5 and 8 DF,  p-value: 0.2863 
    ## 
    ## 
    ## Estimation results for equation russia.dollar_price: 
    ## ==================================================== 
    ## russia.dollar_price = japan.dollar_price.l1 + diffUSA.l1 + brazil.dollar_price.l1 + russia.dollar_price.l1 + const + trend 
    ## 
    ##                        Estimate Std. Error t value Pr(>|t|)
    ## japan.dollar_price.l1  -0.05633    0.39448  -0.143    0.890
    ## diffUSA.l1             -0.34485    1.58344  -0.218    0.833
    ## brazil.dollar_price.l1 -0.12811    0.27941  -0.458    0.659
    ## russia.dollar_price.l1  0.50062    0.45497   1.100    0.303
    ## const                   2.17684    1.61602   1.347    0.215
    ## trend                  -0.02605    0.03684  -0.707    0.499
    ## 
    ## 
    ## Residual standard error: 0.4258 on 8 degrees of freedom
    ## Multiple R-Squared: 0.2682,  Adjusted R-squared: -0.1892 
    ## F-statistic: 0.5863 on 5 and 8 DF,  p-value: 0.7115 
    ## 
    ## 
    ## 
    ## Covariance matrix of residuals:
    ##                     japan.dollar_price  diffUSA brazil.dollar_price
    ## japan.dollar_price             0.09665 0.004060             0.04350
    ## diffUSA                        0.00406 0.003311             0.00314
    ## brazil.dollar_price            0.04350 0.003140             0.35064
    ## russia.dollar_price            0.03118 0.008659             0.14859
    ##                     russia.dollar_price
    ## japan.dollar_price             0.031182
    ## diffUSA                        0.008659
    ## brazil.dollar_price            0.148590
    ## russia.dollar_price            0.181282
    ## 
    ## Correlation matrix of residuals:
    ##                     japan.dollar_price diffUSA brazil.dollar_price
    ## japan.dollar_price              1.0000 0.22698             0.23629
    ## diffUSA                         0.2270 1.00000             0.09216
    ## brazil.dollar_price             0.2363 0.09216             1.00000
    ## russia.dollar_price             0.2356 0.35344             0.58936
    ##                     russia.dollar_price
    ## japan.dollar_price               0.2356
    ## diffUSA                          0.3534
    ## brazil.dollar_price              0.5894
    ## russia.dollar_price              1.0000

The estimated coefficient matrix is obtained, with the first column as Japan, the second column as the United States, the third column as Brazil and the last column as Russia. The rows represents the price of a Big Mac in Japan, United States, Brazil, and Russia dependent on the lagged price in every other country, according to the column specification.

We focus on the second row, representing the price of a Big Mac in the United States, dependent on the lagged price of a Big Mac in Japan, US, Brazil and Russia. We observe that a unit increase in the lagged price of a Big Mac in Brazil and the United States leads to a decrease in the price of a Big Mac in the United States. Whereas, a unit increase in the lagged price of a Big Mac in Japan and Russia leads to an increase in the price of a Big Mac in the United States.

$$
\\left(\\begin{array}{cccc} 
-0.6198021 & -1.3939477 & -0.2448830 & 0.4496231\\\\
0.0003337 & -0.9322096 & -0.0843874 & 0.1977810\\\\
0.07904 & 2.02826 & 0.06398 & 0.64719\\\\
-0.05633 & -0.34485 & -0.12811 & -0.50062
\\end{array}\\right)
$$

**Model Diagonstics**
---------------------

We plot the acf for the residuals of each indvidual series and for the pairs of series. In most of the plots, the correlation of the residuals is negligible. However, our visual inspection disagrees with the result of the multivariate Ljung-box test. We reject with significance level 0.05 and therfore conclude that the residuals are not white noise. This result undermines the correctness of our model since we assumed that the residuals are white noise before fitting the model.

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 3.4.4

``` r
# auto-correlation (diagonals) and cross-correlation (off-diagonal) plots
acf(resid(vectorFit), 12)
```

![](bigmac_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# multivariate Ljung-Box test
serial.test(vectorFit, lags.pt = 10, type = "PT.adjusted")
```

    ## 
    ##  Portmanteau Test (adjusted)
    ## 
    ## data:  Residuals of VAR object vectorFit
    ## Chi-squared = 180.38, df = 144, p-value = 0.02148

**Results**
===========

The vector ARMA model gives 4 different models for each country. However, not every model performed well in terms of coefficient signifiance and overall model significance. The US model performed the best a R-squared value of 0.74, outperforming all other models. The Japan model performed the next best according to the R-squared value, followed by the Brazil and Russian model. In addition, two of the coefficients of the US model are significant. These coefficients included the lagged US price and the lagged Russian price. Every other coefficient was independently insignificant.

Further analysis of the data is necessary to build a model which can more closely estimate the relationship of local prices of Big Macs between different countries. The inclusion of other datasets containing other variables which could influence the price of a Big Mac should be considered.

From a practical perspective, these results suggest that it is difficult to isolate a relationship between the price of a good (Big Macs in this context) in one country, and the price of the same good in other countries. There are undoubtedly many other factors which would impact the local price of a good, which are not included in this model. Hence, the idea proposed by *The Economist* that the ratio of the value of goods that can be bought with local currency can estimate the exchange rates between countries is not perfect.

**Discussion**
==============

First, we discuss some limitations of the data analysis.

The model choice for our objective is quite complex, considering we are jointly analyzing the time series for 4 countries. This data set is unique as the number of observations in a time series is quite low (15). This constraint limits our model choice to simple models because we want to avoid estimating more parameters than the number of observations.

Another limitation of the data is the frequency in which the data was collected. The data is biyearly and only spans about 7 years. It is plausible to believe that the price of a Big Mac is demand inelastic, that is, the price does not change dramatically given a unit change in demand and this is the reason for the infrequent collection of data. But, the sparsity of data makes model estimation and estimation difficult.

Furthermore, if the idea is to approximate the exchange rate between countries by the ratio of the value of goods that can be bought with local currency, then the frequency of change should match on both sides of the equation. Exchange rates change frequently and constantly. But, the velocity of change for exchange rates do not match the frequency which the data for the prices of Big Macs was gathered. If data for the prices of Big Macs was gathered more frequently, then perhaps the model would perform better. Or we could find a different good whose price is demand elastic which would result in a better approximation to the exchange rate.

Also, the model is assuming that only the prices of a Big Mac in other countries affect the local price of a Big Mac. This is an oversimplistic assumption which does not reflect reality. There are many other factors which could influence the local price of a Big Mac. These other factors are not controlled for in our analysis and represent an opportunity for future analysis.

Lastly, we wrap up the analysis with final thoughts on the data analysis.

Overall, the results indicate that the estimated models did not perform well in estimating the prices of a Big Mac dependent on the prices of a Big Mac in other countries. If it is difficult to determine the relationship between prices of Big Macs in different countries, then the use of the ratio of the value of goods a local currency can buy to approximate exchange rates is unreliable. Hence, we should consider other potential variables and their relationships to better approximate the exchange rates between countries.
