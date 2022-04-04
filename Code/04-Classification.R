# Classification

data(iris)
iris2 <- iris
iris2$sp <- ifelse(iris2$Species == "versicolor", 1, 0)
iris2$sp <- as.factor(iris2$sp)
iris2$Species <- NULL
table(iris2$sp)

# Logistic regression ----------------------------------------------------------

fit.log <- glm(sp ~ ., data = iris2, family = binomial)
summary(fit.log)

predict(fit.log, iris2)

probs <- predict(fit.log, iris2, type = "response")
pred.log <- rep(0, 150)
pred.log[probs > 0.5] <- 1
table(pred.log, iris2$sp)



# Discriminant analysis --------------------------------------------------------

library(MASS)

# LDA

fit.lda <- lda(sp ~ ., data = iris2)
summary(fit.lda)

pred.lda <- predict(fit.lda, iris2)
table(pred.lda$class, iris2$sp)

# QDA

fit.qda <- qda(sp ~ ., data = iris2)

pred.qda <- predict(fit.qda, iris2)
table(pred.qda$class, iris2$sp)



# Random forest ----------------------------------------------------------------

library(randomForest)

fit.rf <- randomForest(sp ~ ., data = iris2, importance = TRUE)
fit.rf

pred.rf <- predict(fit.rf, iris2)
table(pred.rf, iris2$sp)

importance(fit.rf)
varImpPlot(fit.rf)



# Support vector machines ------------------------------------------------------

library(e1071)

# Linear kernel

fit.svm.lin <- svm(sp ~ ., data = iris2, kernel = "linear")
summary(fit.svm.lin)

pred.svm.lin <- predict(fit.svm.lin, iris2)
table(pred.svm.lin, iris2$sp)

# Polynomial kernel

fit.svm.poly <- svm(sp ~ ., data = iris2, kernel = "polynomial")

pred.svm.poly <- predict(fit.svm.poly, iris2)
table(pred.svm.poly, iris2$sp)

# Radial basis kernel

fit.svm.rad <- svm(sp ~ ., data = iris2, kernel = "radial")

pred.svm.rad <- predict(fit.svm.rad, iris2)
table(pred.svm.rad, iris2$sp)


