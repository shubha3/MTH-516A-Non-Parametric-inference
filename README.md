# MTH-516A-Non-Parametric-inference
This repo contains the R codes, figures, and datasets used in the project for the course - `MTH516A: Non-Parametric Inference` at IIT Kanpur during the academic year 2022-2023.

## Project Members
  - Manas Mishra
  - Rachita Mondal
  - Shubha Sankar Banerjee

## Project Title
`Estimating the Distribution of Linear Regression Estimates using Fast and Robust Bootstrap `

## Abstract
> Principal Component Analysis is a widely studied methodology as it is a useful technique for dimension reduction. 
In this report, we discuss Sparse Principal Component Analysis (SPCA), which is a modification over PCA. 
This method is able to resolve the interpretation issue of PCA. Additionally, it provides sparse loadings to
the principal components. The main idea of SPCA comes from the relationship between PCA problem and regression analysis.
We also discuss GAS-PCA, which is a generalization over SPCA and this method performs better than SPCA,
even in finite sample cases. Our report is mainly based on [1] and its extension [2].





Before running please install Rtools and run the following commands(if devtools is pre installed)

``` r
library(devtools)

install_github("msalibian/FRB")
```
