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
group.1 <- tibble(x = runif(n) * 20, y = f(x) + runif(n, -1, 1), group = '1') %>%
  filter(abs(y - f(x)) > .5)

group.2 <- tibble(x = runif(n/2) * 20, y = f(x) + runif(n/2, -.4, .4), group = '2')
data <- bind_rows(group.1, group.2)

ggplot(data) +
  geom_point(aes(x, y, color = group))

