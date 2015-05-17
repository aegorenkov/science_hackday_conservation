library(boot)
library(rpart)

form = formula(protected~MeanPrecip + Percent_of_HUC_Rare)
fold = 10
x <- na.omit(data.model)
n <- nrow(x)
prop <- n%/%fold
newseq <- rank(runif(n))
k <- as.factor((newseq - 1) %/% prop + 1)

y <- unlist(strsplit(as.character(form), ""))[2]
vec.accuracy <- vector(length = fold)
vec.iaccuracy <- vector(length = fold)
for (i in seq(fold)) {
  fit <- rpart(form, data = x[k != i, ], method = "class")
  fit.prune <- prune(fit, cp = .000001)
  fcast <- predict(fit.prune, newdata = x)[,1]
  cm <- table(x$protected[k == i], ifelse(fcast[k == i] > .5, 1, 0))
  icm <- table(x$protected[k != i], ifelse(fcast[k != i] > .5, 1, 0))
  accuracy <- (cm[1, 1] + cm[2, 2])/sum(cm)
  iaccuracy <- (icm[1, 1] + icm[2, 2])/sum(icm)
  #accuracy <- sum((x$protected[k == i]-fcast)**2, na.rm= TRUE)
  vec.accuracy[i] <- accuracy
  vec.iaccuracy[i] <- iaccuracy
}

avg.accuracy <- mean(vec.accuracy)
avg.iaccuracy <- mean(vec.iaccuracy)
avg.error <- 1 - avg.accuracy
avg.ierror <- 1 - avg.iaccuracy
cv <- data.frame(Accuracy = avg.accuracy, 
                 Error = avg.error,
                 InAccuracy = avg.iaccuracy, 
                InError = avg.ierror)
cv