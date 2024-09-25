library(ggplot2)
library(tidyverse)

df = read.csv("Mall_Customers.csv")

df$CustomerID = NULL

df = df %>% 
  mutate(Gender = ifelse(Gender == "Male", 1,0)) %>%
  rename(Income = Annual.Income..k..) %>%
  rename(Score = Spending.Score..1.100.)

ggplot(df, mapping = aes()) +
  geom_point(mapping = aes(x = Income, y = Score, colour = Gender, size = Age))

model = lm(Score ~ Income + Age + Gender, data = df)
summary(model)