## Importing data ##
library(readxl)
data = read_excel("Dataset.xlsx")
View(data)

## Variables formation ##
Y = data[,2:5]
X = data[,6:11]

## Data descriptives ##
summary(data)

## Canonical Correlation Analysis ##
library(ggplot2)
library(GGally)
library(CCA)
library(CCP)

ggpairs(Y)
ggpairs(X)

# Simple correlations #
correl = matcor(Y, X)
correl
win.graph()
img.matcor(correl, type = 2)

# Raw canonical correlations #
cc1 = cc(Y, X)
cc1$cor
barplot(cc1$cor, main = "Canonical correlations", xlab = "Dimensions",
        ylab = "Correlation")

# Coefficients of raw canonical variables #
cc1[3:4]

# Standardized canonical coefficients #
s1 = diag(sqrt(diag(cov(Y))))
s1%*%cc1$xcoef

s2 = diag(sqrt(diag(cov(X))))
s2%*%cc1$ycoef

## Significance test ##
rho = cc1$cor
n = dim(Y)[1]
p = length(Y)
q = length(X)

p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")
