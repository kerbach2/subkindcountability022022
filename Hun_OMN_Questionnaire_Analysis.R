### Load packages

library(tidyverse)
library(effsize)
library(lmerTest)
library(multcomp)
library(emmeans)

# import data
data <- read_csv('Hun_OMN_Questionnaire_Data.csv')

# create subsets for analayses
## object denoting nouns


objects <- filter(data, class %in% c('object'))
## mass nouns ~ subkinds 
masssk <- filter(data, `counted-entities` %in% c('subkind'))
masssk <- filter(masssk, `object-countability` %in% c('mass'))
## object mass nouns counting subkinds
om <- filter(objects, `object-countability` %in% c('mass'))
omsk <- filter(om, `counted-entities` %in% c('subkind'))
## substance mass nouns counting subkinds
smsk <- filter(masssk, class %in% c('substance'))

#Draw graphs
masssk %>% ggplot(aes(x = class, y = rating, fill = class)) + geom_boxplot() + theme_minimal() + scale_fill_brewer(palette = 'Greys')

objects %>% ggplot(aes(y = rating, x = `object-countability`, fill = `counted-entities`)) + geom_boxplot() + labs(x = "Countability", y = "Judgment") + scale_fill_brewer(palette = 'Greys')

# Unpaired t-test of substance and object mass nouns counting subkinds
t.test(smsk$rating,omsk$rating,paired=FALSE)
cohen.d(rating ~ class, data = masssk)

# ANOVA
# To see if there is an interaction of of Countability and Condition

M<-lmer(rating ~ (`object-countability` * `counted-entities`) + (1|subject) + (0 + `object-countability` * `counted-entities`|subject) + (1|`object-countability` * `counted-entities`|pair), data=objects)
summary(M)
anova(M)

# Clean data; remove problematic elements and corresponding pairs

clean <-data[!(data$pair=="bullet-ammunition" | data$pair=="container-plasticware" | data$pair=="good-merchandise" | data$pair=="blanket-bedding" | data$pair=="dish-china"),]

# ANOVA on clean data
# To see if there is an interaction of of Countability and Condition

Mc<-lmer(rating ~ (`object-countability` * `counted-entities`) + (1|subject) + (0 + `object-countability` * `counted-entities`|subject) + (1|`object-countability` * `counted-entities`|pair), data=clean)
summary(Mc)
anova(Mc)

