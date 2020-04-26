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


### changing years in numerical values 0 and 1 
### pre = 0 ----- peak = 1

dat <- dat%>%
  mutate(pre_peak = ifelse(Year == "2010", "0", ifelse(Year == "2011", "0", ifelse(Year == "2014", "1", ifelse(Year == "2015", "1", ifelse(Year == "2016", "1", NA))))))



#### changing treatments into 0 and 1 
#### wet = 0  ---- dry = 1

dat <- dat%>%
  mutate(num_treatment = ifelse(Treatment == "wet", "0", ifelse(Treatment == "dry", "1", NA)))


### Changing sites to 1-5 (numerical values)




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


### Total Root length X Treatment 
p2 <- dat %>% filter(is.na(Treatment) == FALSE) %>% 
  ggplot(aes(x=Treatment, y=total_root_length)) +
  geom_boxplot() + xlab("Treatment") + ylab("Total Root Length")

p2 + theme_classic()


### Does root length by year - pre vs peak drought 
p3 <- dat %>% filter(is.na(pre_peak) == FALSE) %>% 
  ggplot(aes(x=pre_peak, y=total_root_length)) +
  geom_boxplot() + xlab("Pre-drought                                                   Peak-drought") + ylab("Total Root Length")

p3 + theme_classic()



### Does root length differ by region?


### Total Root Length X Year of Collection










