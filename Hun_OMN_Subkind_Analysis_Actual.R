### Load packages

library(tidyverse)
library(effsize)
library(lme4)

# import data
data <- read_csv('Hun_OMN_Subkinds_data.csv')

# reformat data
##transpose data frame
data2 <- t(data) 
##get list of IDs
ID <- data2[2,5:34] 
##get judgments of first item
Judgment <- data2[3,5:34] 
##make frame with labels
final <- data.frame()

for (i in 3:102) {
	temp <- data.frame(ID=data2[2,5:34],Class=rep(data2[i,2], times=30),Countability=rep(data2[i,3],times=30),Condition=rep(data2[i,4],times=30),Noun=rep(data2[i,1],times=30),Judgment=data2[i,5:34])
	print(temp)
	final <- rbind(final,temp)
	}
	
## converting character type column to numeric
final <- transform(final, Judgment = as.numeric(Judgment))
## convert back to tibble
final <- as_tibble(final)

# create subsets for analayses
## all test items
test <- filter(final, Class %in% c('object', 'substance'))
## object denoting nouns
objects <- filter(final, Class %in% c('object'))
## mass nouns ~ subkinds 
masssk <- filter(final, Condition %in% c('subkind'))
masssk <- filter(masssk, Countability %in% c('uncountable'))
## object mass nouns 
om <- filter(objects, Countability %in% c('uncountable'))
## count nouns counting objects
oco <- filter(objects, Condition %in% c('object'))
oco <- filter(oco, Countability %in% c('countable'))
## object mass nouns counting objects
omo <- filter(om, Condition %in% c('object'))
## object mass nouns counting subkinds
omsk <- filter(om, Condition %in% c('subkind'))
## substance mass nouns counting subkinds
smsk <- filter(masssk, Class %in% c('substance'))
## count nouns counting objects and object mass nouns counting subkinds
ocoomsk <- rbind(oco,omsk)

#Draw graphs
masssk %>% ggplot(aes(x = Class, y = Judgment, fill = Class)) + geom_boxplot() + theme_minimal() + scale_fill_brewer(palette = 'PuOr')

objects %>% ggplot(aes(y = Judgment, x = Countability, fill = Condition)) +
+   geom_boxplot() +
+   labs(x = "Countability", y = "Judgment")

# ANOVA
# To see if there is an interaction of of Countability and Condition

null <- lmer(Judgment ~ Condition + (1|ID) + (1|Noun), data = objects, REML=FALSE)
model <- lmer(Judgment ~ Countability + Condition + (1|ID) + (1|Noun), data = objects, REML=FALSE)
anova(null, model)

# Paired t-Test

t.test(smsk$Judgment,omsk$Judgment,paired=FALSE)
t.test(omsk$Judgment,omo$Judgment,paired=FALSE)
t.test(omsk$Judgment,oco$Judgment,paired=FALSE)

# Power analysis
# To determine if the number of participants will result in a large enough effect size

cohen.d(Judgment ~ Countability, data = objects)
cohen.d(Judgment ~ Class, data = masssk)
cohen.d(Judgment ~ Condition, data = om)
cohen.d(Judgment ~ Countability, data = ocoomsk)