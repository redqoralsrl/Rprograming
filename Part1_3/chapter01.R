install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean.hwy=mean(hwy)) %>%
  arrange(desc(mean.hwy))

mpg %>%
  filter(manufacturer=="ford") %>%
  group_by(model) %>%
  arrange(desc(hwy))

lm.mpg <- lm(data=mpg, hwy ~ displ)
summary(lm.mpg)

qplot(data = mpg, x = displ, y = hwy)

