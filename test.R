library(tidyverse)
data <- read_csv("data.xlsx")

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()

