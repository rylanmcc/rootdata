### Root Study R-Script - Angert Lab
### Directed Studies 

### set working directory
setwd("~/Documents/Root_Study")


### load in packages 
library(tidyverse)
library(ggplot2)

### load in csv 
root_data <- read.csv("RootReader_Data.csv") %>%
  select(plant_id, total_root_length, longest_root, plant_type, rhizome_present)
View(root_data)

metadata <- read.csv("Metadata.csv") %>% 
  select(-X)


### joining CSV's into a master data set
dat <- left_join(root_data, metadata, by = c("plant_id" = "Random.ID"))

View(dat)


### binning collection years into pre-drought (2010, 2011) and peak-drought (2014-16)

dat <- dat%>%
  mutate(pre_peak = ifelse(Year == "2010", "pre", ifelse(Year == "2011", "pre", ifelse(Year == "2014", "peak", ifelse(Year == "2015", "peak", ifelse(Year == "2016", "peak", NA))))))


### making region variable from sites 
dat <- dat%>%
  mutate(region = ifelse(Site == "S02", "South", ifelse(Site == "S07", "South", ifelse(Site == "S11", "South", ifelse(Site == "S15", "North", ifelse(Site == "S16", "North", ifelse(Site == "S36", "North", NA)))))))


### Histogram displaying raw data

hist(dat$total_root_length)

hist(dat$longest_root)

hist(log(dat$longest_root))



#### Exploratory graphing ##########################

### Total Root length X population
p1 <- dat %>% filter(is.na(Name) == FALSE) %>% 
  ggplot(aes(x=Name, y=total_root_length)) +
  geom_boxplot() + xlab("Population") + ylab("Total Root Length")

p1 + theme_classic()


### Total Root length X region
p2 <- dat %>% filter(is.na(region) == FALSE) %>% 
  ggplot(aes(x=region, y=total_root_length)) +
  geom_boxplot() + xlab("Region") + ylab("Total Root Length")

p2 + theme_classic()

### Total Root length X Treatment 
p3 <- dat %>% filter(is.na(Treatment) == FALSE) %>% 
  ggplot(aes(x=Treatment, y=total_root_length)) +
  geom_boxplot() + xlab("Treatment") + ylab("Total Root Length")

p3 + theme_classic()

### Does root length by year - pre vs peak drought 
p4 <- dat %>% filter(is.na(pre_peak) == FALSE) %>% 
  ggplot(aes(x=pre_peak, y=total_root_length)) +
  geom_boxplot() + xlab("Year of collection") + ylab("Total Root Length")

p4 + theme_classic()


### Do regions differ in response to drought (evolution across years)?
p5 <- dat %>% filter(is.na(pre_peak) == FALSE) %>% 
  ggplot(aes(x=pre_peak, y=total_root_length, color=region)) +
  geom_boxplot() + xlab("Year of collection") + ylab("Total Root Length")

p5 + theme_classic()

### Do regions differ in response to drought (plasticity across treatment)?
p6 <- dat %>% filter(is.na(Treatment) == FALSE) %>% 
  ggplot(aes(x=Treatment, y=total_root_length, color=region)) +
  geom_boxplot() + xlab("Watering treatment") + ylab("Total Root Length")

p6 + theme_classic()

### Do sites differ in response to drought (plasticity across treatment)?
p7 <- dat %>% filter(is.na(Treatment) == FALSE) %>% 
  ggplot(aes(x=Site, y=total_root_length, color=Treatment)) +
  geom_boxplot() + xlab("Site") + ylab("Total Root Length")

p7 + theme_classic()

### Did plasticity evolve?
p8 <- dat %>% filter(is.na(Treatment) == FALSE) %>% 
  ggplot(aes(x=pre_peak, y=total_root_length, color=Treatment)) +
  geom_boxplot() + xlab("Year of collection") + ylab("Total Root Length")

p8 + theme_classic()


#### Analysis of variance
library(lme4)
library(lmerTest)
library(lmtest)

mod.3way <- lmer(total_root_length ~ region*Treatment*pre_peak + (1|Site), data=dat)
summary(mod.3way)
# model gives singularity error - it is overly complex
# two options: drop random effect &/or drop interaction terms

# without random effect of site
mod.3way.fixed <- lm(total_root_length ~ region*Treatment*pre_peak, data=dat)
summary(mod.3way.fixed)

# retaining random effect but dropping interaction terms
mod.2ways <- lmer(total_root_length ~ region*Treatment + region*pre_peak + Treatment*pre_peak + (1|Site), data=dat)
summary(mod.2ways)

lrtest(mod.3way, mod.2ways) #  3-way model has significantly higher likelihood --> should retain 3-way interaction


