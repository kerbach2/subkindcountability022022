### Load packages

library(tidyverse)
library(effsize)
library(lmerTest)
library(emmeans)

# import data
data <- read_csv('Hun_OMN_Questionnaire_Data.csv')

# create subsets for analayses
## object denoting nouns
objects <- filter(data, class %in% c('object'))
## mass nouns ~ subkinds 
masssk <- filter(data, `counted-entity` %in% c('subkind'))
masssk <- filter(masssk, `object-countability` %in% c('uncountable'))
## object mass nouns counting subkinds
om <- filter(objects, `object-countability` %in% c('uncountable'))
omsk <- filter(om, Condition %in% c('subkind'))
## substance mass nouns counting subkinds
smsk <- filter(masssk, Class %in% c('substance'))

#Draw graphs
masssk %>% ggplot(aes(x = class, y = rating, fill = Class)) + geom_boxplot() + theme_minimal() + scale_fill_brewer(palette = 'PuOr')

objects %>% ggplot(aes(y = rating, x = `object-countability`, fill = Condition)) + geom_boxplot() + labs(x = "Countability", y = "Judgment") + scale_fill_brewer(palette = 'PuOr')

# Unpaired t-test of substance and object mass nouns counting subkinds
t.test(smsk$Judgment,omsk$Judgment,paired=FALSE)
cohen.d(rating ~ class, data = masssk)

# ANOVA
# To see if there is an interaction of of Countability and Condition

M<-lmer(rating ~ (`object-countability` * `counted-entities`) + (1|subject) + (1|pair), data=objects)
anova(M)

# Paired t-Test
emmeans(M, list(pairwise ~ (`object-countability` * `counted-entities`)))
