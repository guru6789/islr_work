Exploring the use of the validation set approach in order to estimate the test error rates that result from fitting various linear models on the `Auto` data set.

split the set of observations into two halves, by selecting a random subset of 196 observations out of the original 392 observations.

``` r
options(warn = -1)
library(ISLR)
set.seed(1)
train = sample(392, 196)
```

``` r
attach(Auto)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)      #MSE for linear regression fit
```

    ## [1] 26.14142

Now we'll use polynomials to test error for quadratic and cubic functions.

``` r
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)      #MSE for linear regression fit
```

    ## [1] 19.82259

``` r
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)      #MSE for linear regression fit
```

    ## [1] 19.78252

If we choose a different training set instead, then we will obtain somewhat different errors on the validation set.

``` r
set.seed(2)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
```

    ## [1] 23.29559

``` r
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
```

    ## [1] 18.90124

``` r
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
```

    ## [1] 19.2574

These results are consistent with our previous findings: a model that predicts `mpg` using a quadratic function of `horsepower` performs better than a model that involves only a linear function of horsepower.

############################################################### 

Leave-One-Out Cross-Validation(LOOCV) \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)      #if `family` is not supplied them glm will do linear regression.
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta       #cross validation results.
```

    ## [1] 24.23151 24.23114

Repeat this procedure for increasingly complex polynomial fits.

``` r
cv.err = NULL
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.err
```

    ##  [1] 24.23151 19.24821 19.33498 19.42443 19.03321 18.97864 18.83305
    ##  [8] 18.96115 19.06863 19.49093

############################################################## 

k-Fold Cross-Validation \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
set.seed(17)
kfv.err = NULL
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  kfv.err[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
kfv.err
```

    ##  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609
    ##  [8] 19.71201 18.95140 19.50196
