# Imputation

# MICE -------------------------------------------------------------------------
# https://www.jstatsoft.org/article/view/v045i03
# https://rdrr.io/cran/mice/man/mice.html

library(mice)
nhanes

summary(nhanes)
md.pattern(nhanes)

imp1 <- mice(nhanes, method = "mean", m = 1, maxit = 1)
complete(imp1)

imp2 <- mice(nhanes)
imp2
complete(imp2)


# missForest -------------------------------------------------------------------
# https://academic.oup.com/bioinformatics/article/28/1/112/219101

library(missForest)
imp3 <- missForest(nhanes)
imp3

nhanes2 <- nhanes
nhanes2$hyp <- as.factor(nhanes2$hyp)
imp4 <- missForest(nhanes2)
imp4
imp4$ximp


# KNN --------------------------------------------------------------------------
# https://www.bioconductor.org/packages/release/bioc/html/impute.html

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("impute")

library(impute)
imp5 <- impute.knn(nhanes)
imp5 <- impute.knn(as.matrix(nhanes))
