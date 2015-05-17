library(boot)
setwd("~/DCHackday")
data.model <- read.csv("datafinal.csv")

odds <- function(x) {
  exp(x)/(exp(x)+1)  
}


form = formula(protected~  Percent_of_HUC_Rare + MeanPrecip)

fold = 10
x <- na.omit(data.model)
n <- nrow(x)
prop <- n%/%fold
newseq <- rank(runif(n))
k <- as.factor((newseq - 1) %/% prop + 1)

y <- unlist(strsplit(as.character(form), ""))[2]
vec.accuracy <- vector(length = fold)
for (i in seq(fold)) {
  fit <- glm(form, family = binomial("logit"), data=x)
  fcast <- odds(predict(fit, newdata = x[k == i,]))
  cm <- table(x$protected[k == i], ifelse(fcast > .4, 1, 0))
  accuracy <- (cm[1, 1] + cm[2, 2])/sum(cm)
  #accuracy <- sum((x$protected[k == i]-fcast)**2, na.rm= TRUE)
  vec.accuracy[i] <- accuracy
}

avg.accuracy <- mean(vec.accuracy)
avg.error <- 1 - avg.accuracy
cv <- data.frame(Accuracy = avg.accuracy, 
                 Error = avg.error)

cv