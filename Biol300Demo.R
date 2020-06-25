### Root Study R-Script - Angert Lab
### Directed Studies by Rylan McCallum
### Simplified for Biol 300 2-way ANOVA

### load in packages 
library(tidyverse)
library(ggplot2)

### load in csv with raw data
dat <- read.csv("RootData_Biol300.csv")

### Histogram displaying raw data
hist(dat$total_root_length)

#### Exploratory graphing ##########################

### Total Root length X Region
p1 <- ggplot(data=dat, aes(x=Region, y=total_root_length)) +
  geom_boxplot() + xlab("Region") + ylab("Total Root Length")
p1 + theme_classic()

### Total Root length X Treatment 
p2 <- ggplot(data=dat, aes(x=Treatment, y=total_root_length)) +
  geom_boxplot() + xlab("Treatment") + ylab("Total Root Length")
p2 + theme_classic()

### Do regions differ in plasticity across treatments?
p3 <- ggplot(data=dat, aes(x=Region, y=total_root_length, color=Treatment)) +
  geom_boxplot() + xlab("Region") + ylab("Total Root Length")
p3 + theme_classic()

#### Analysis of variance
mod.2way <- lm(total_root_length ~ Region*Treatment, data=dat)
summary(mod.2way)

