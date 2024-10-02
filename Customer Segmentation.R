library(ggplot2)
library(tidyverse)
library(lme4)
library(mgcv)
library(reshape2)
library(ggpubr)
library(scatterplot3d)

df = read.csv("Mall_Customers.csv")

# Wrangling ------------

df$CustomerID = NULL

df = df %>% 
  mutate(Gender = ifelse(Gender == "Male", 1,0)) %>%
  rename(Income = Annual.Income..k..) %>%
  rename(Score = Spending.Score..1.100.)

# Plots ---------------

p = ggplot(df, aes(x = Score)) +
  geom_bar(fill = 'orange', colour = "black") +
  ggtitle("Distribution of Spending score") +
  labs(x = "Spening score", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

q = ggplot(df, aes(x = Income)) +
  geom_bar(fill = 'orange', colour = "black") +
  ggtitle("Distribution of Annual Income") +
  labs(x = "Income", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

r = ggplot(df, aes(x = Age)) +
  geom_bar(fill = 'orange', colour = "black") +
  ggtitle("Distribution of Age") +
  labs(x = "Age", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p, q, r, ncol = 2, nrow = 2)

# Exploration -----------------

corr = round(cor(df), 3)
meltcorr = melt(corr)

ggplot(meltcorr, aes(Var1, Var2, fill = value)) +
  geom_tile()

ggplot(df, mapping = aes()) +
  geom_point(mapping = aes(x = Income, y = Score, colour = Gender, size = Age))

ggplot(df, mapping = aes()) +
  geom_point(mapping = aes(x = Income, y = Score, size = Age)) +
  facet_wrap(~Gender)

# Linear modelling ------------------

model = lm(Score ~ Income + Age + Gender, data = df)
summary(model)

model2 = lm(Score ~ Income + Age*Gender, data = df)
summary(model2)

model3 = lmer(Score ~ Income + Age|Gender, data = df)
summary(model3)

model4 = lm(Score ~ Age, data = df)
summary(model4)

ggplot(df, mapping = aes()) +
  geom_point(mapping = aes(x = Age, y = Score)) +
  facet_wrap(~Gender)

# K Means ---------------

kout = kmeans(df, 5, iter.max = 30, nstart = 20)
kout

n = 10
wss = numeric(n)

for (i in 1:n){
  kout = kmeans(df, centers = i, nstart = 20)
  wss[i] = kout$tot.withinss
}

wss.df = tibble(clusters = 1:n, wss = wss)

ggplot(wss.df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

k5 = kmeans(df, centers = 5, nstart = 20)
df$cluster_id = factor(k5$cluster)

ggplot(df, aes(y = Score, x = Income, colour = cluster_id, size = Age)) +
  geom_point() +
  facet_wrap(~Gender)

# 3D scatterplots -----------

colours = c("red", "blue", "black", "yellow", "pink")
colours = colours[as.numeric(df$cluster_id)]

scatterplot3d(df[c("Score", "Income", "Age")], color = colours, pch = df$Gender + 16)

# GAMs -------------------

gam1 = gam(Score ~ s(Income) + s(Age) + Gender, 
           data = df, family = gaussian)

summary(gam1)
gam.check(gam1)

























