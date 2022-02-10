

### Load packages

library(tidyverse)
library(effsize)
library(lme4)

### Load data 

data <- read_csv('Hun_OMN_Subkinds_Study.csv')

### Create subsets for analysis

objects <- data[1:1201,] # to compare Subkind and Object judgments of Count nouns and Object Mass Nouns
massnouns <- data[1:1201,] # to compare Subkind judgments of Substance Mass Nouns and Object Mass Nouns
substancemass <- massnouns$Judgment[296:590]
objectmass <- massnouns$Judgment[1:295]

### Power analysis
### To determine if the number of participants will result in a large enough effect size

cohen.d(Judgment ~ Countability, data = objects)
cohen.d(Judgment ~ Class, data = massnouns)

### ANOVA
### To see if there is an interaction of of Countability and Condition

null <- lmer(Judgment ~ Condition + (1|ID) + (1|Noun), data = objects, REML=FALSE)
model <- lmer(Judgment ~ Countability + Condition + (1|ID) + (1|Noun), data = objects, REML=FALSE)
anova(null, model)

### Paired t-Test

t.test(substancemass,objectmass,paired=TRUE)
