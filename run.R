library(randomForest)
library(dplyr)
library(ggplot2)

# generate dataset first
# inside band is one group
# outside band is another group

f <- function(x) {
  2 * sin(x)
}
n = 10000

group.1 <- tibble(x = runif(n) * 20, y = f(x) + runif(n, -1, 1), group = 0) %>%
  filter(abs(y - f(x)) > .5)
group.2 <- tibble(x = runif(n/2) * 20, y = f(x) + runif(n/2, -.4, .4), group = 1)

data <- bind_rows(group.1, group.2) # bind groups together
data <- data[sample(1:nrow(data)), ] # shuffle rows

total = nrow(data)
train <- data[1:(nrow(data)/2),]
test <- data[(nrow(data)/2):nrow(data),]
rf <- randomForest(group ~ ., train)

results <- ifelse(predict(rf, newdata = test) > .5, 1, 0)
accuracy <- length(which(results == test$group)) / nrow(test)
print(accuracy * 100)