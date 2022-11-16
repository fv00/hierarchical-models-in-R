---
title: "Solución taller sobre capítulo 4 parte 1"
author: "Santiago Franco"
date: "10/11/2022"
output:
  pdf_document: default
  html_document: default
---


```r
library(foreign)
```

```
## Warning: package 'foreign' was built under R version 4.1.2
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## 1.


```r
datos <- read.dta("polls.dta")

# Usando solo la inf del survey = 9158
datis <- subset(datos, survey == 9158)

library(lme4)
```

```
## Loading required package: Matrix
```

```r
M1 <- glmer(bush ~ female + black + (1 | state), 
                        data=datis, family=binomial(link="logit"))
```

## 2


```r
length(unique(datis$state))
```

```
## [1] 49
```

## 3



```r
summary(M1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: bush ~ female + black + (1 | state)
##    Data: datis
## 
##      AIC      BIC   logLik deviance df.resid 
##   2666.7   2689.1  -1329.3   2658.7     2011 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.7276 -1.0871  0.6673  0.8422  2.5271 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  state  (Intercept) 0.1692   0.4113  
## Number of obs: 2015, groups:  state, 49
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.44523    0.10139   4.391 1.13e-05 ***
## female      -0.09705    0.09511  -1.020    0.308    
## black       -1.74161    0.20953  -8.312  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##        (Intr) female
## female -0.551       
## black  -0.119 -0.005
```

begin{align} 
y_{ij} &\sim  Bernoulli(\hat{p_{ij}}) \\ 
\text{logit}(\hat{p_{ij}}) &= 0.44523 -0.09705_{female} -1.74161_{black}  + b_{0i} \\
b_0 &\sim N(0, 4.052)
\end{align}

## 4

Se tiene que el ser mujer disminuye en 0.09705 unidades el valor de la función logit asociada al modelo y que el ser una persona de color disminuye en 1.74161 unidades el valor de la función logit asociada al modelo.

## 5 Modelo para state 39


```r
random_effects <- ranef(M1)$state
random_effects[39,]
```

```
## [1] 0.357593
```


\begin{align} 
y_{39j} & \sim  Bernoulli(\hat{p_{39j}}) \\ 
\text{logit}(\hat{p_{39j}}) &= 0.44523 -0.09705_{female} -1.74161_{black}  + 0.357593
\end{align}

## 6


```r
datos_nuevos <- rbind(c(0,0),
                      c(0,1),
                      c(1,1),
                      c(1,0))
colnames(datos_nuevos) <- c("female", "black")
datos_nuevos <- data.frame(datos_nuevos)
datos_nuevos['state'] <- 39

predict(M1, newdata=datos_nuevos, type='response')
```

```
##         1         2         3         4 
## 0.5656377 0.1857997 0.1715638 0.5416600
```

