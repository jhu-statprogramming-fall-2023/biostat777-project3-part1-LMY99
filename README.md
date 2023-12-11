# Hiperglm package

Author: Mingyuan Li

Description: This package is part of the class assignments in 140.778: Statistical Computing, Algorithm, and Software Development taught by Dr.Aki Nishimura at BSPH. The package includes linear regression and logistic regression implemented through optimization in R and Cpp.

Exported Function: 
hiper_glm: Perform high-performance generalized linear model. Supports linear and logistic model, generates an S3 object of class "hglm".
print.hglm: S3 method of print, print useful information about an hglm object.
coef.hglm: S3 method of coef, print the coefficients of an hglm object.

Example:
```r
X <- matrix(rnorm(200),100,2)
y <- rnorm(100)
hiper_glm(X,y,model='linear')
```

URL to original Github Link: <https://github.com/LMY99/hiperglm>

URL to deployed website: <https://jhu-statprogramming-fall-2023.github.io/biostat777-project3-part1-LMY99/>

Website Customization:

- Uses LUX style
- Changes font
- Changes the background/foreground color
- Changes the navbar structure
- Adds a link to the personal website created in Project 1.