# LOAD ORIGINAL DATA
alldata <- haven::read_sav('data/data.sav')

# SELECT VARIABLES
library(dplyr)
data %>%
  select(qa1, qa2a, qa2b, qb1a, qb1b, qb1c, qb2a, qb2b, qb2c, qb2d,
         qb20, qb21, qb22, qb30, qb31, marital, hisp, racecmb, racethn,
         qe1, qe2, born, DENOM, FAMILY, RELTRAD, attend, qf2, qf5, qg1,
         qg1b, qg1c, qg5, qg6, qg7, qg7b, qh1, qh2, qh4, qi1, qi2a,
         qi2b, qi2c, qi2d, qi3, qi3b, qi4a, qi4b, qi4c, qi4d, qj1, qj2,
         chborn, CHDENOM, CHFAMILY, CHRELTRAD, CHPROTFAM, qk1, qk2, 
         spyearrec, sphisp, spracecmb, spracethn, spborn, SPDENOM, 
         SPFAMILY, SPRELTRAD, SPPROTFAM, childrenrec, chage1rec,
         chageoldrec, chageyngrec,
         qk10a, qk10b, qk10c, qk10d, qm5a, qm5b, qm5d, qm5e,
         qm5f, qm5g, agerec, cohort5cat, educ, respondent_birthregion,
         qp3rec, qp4rec, income, reg, party, partyln, ideo, hh1rec,
         hh3rec, fertrec, SEX, qp99)

data <- alldata[,c('qa1','qa2a','qa2b','qb1a','qb1b','qb1c','qb2a','qb2b', 
                   'qb2c','qb2d','qb20','qb21','qb22','qb30','qb31','marital',
                   'hisp','racecmb','racethn','qe1','qe2','born','DENOM','FAMILY',
                   'RELTRAD','attend','qf2','qf5','qg1','qg1b','qg1c','qg5','qg6',
                   'qg7','qg7b','qh1','qh2','qh4','qi1','qi2a','qi2b','qi2c','qi2d',
                   'qi3','qi3b','qi4a','qi4b','qi4c','qi4d','qj1','qj2','chborn',
                   'CHDENOM','CHFAMILY','CHRELTRAD','CHPROTFAM','qk1','qk2', 
                   'spyearrec','sphisp','spracecmb','spracethn','spborn','SPDENOM', 
                   'SPFAMILY','SPRELTRAD','SPPROTFAM','childrenrec','chage1rec',
                   'chageoldrec','chageyngrec','qk10a','qk10b','qk10c','qk10d','qm5a',
                   'qm5b','qm5d','qm5e','qm5f','qm5g','agerec','cohort5cat','educ',
                   'respondent_birthregion','qp3rec','qp4rec','income','reg','party',
                   'partyln','ideo','hh1rec','hh3rec','fertrec','SEX','qp99')]

# RECODES
data$tongues <- as.factor(ifelse(data$qi2d < 4, 1, 0))

summary(data)

library(tidyverse)
library(dplyr)
library(tidyr)



#create histogram of values for price
ggplot(data=data, aes(x=qi2d)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Price Values")

# FREQUENCES OF SPEAKING IN TONGUES
tonguecounts <- table(data$qi2d)
labs <- c("1 At least once a week", "2 Once or twice a month", "3 Several times a year", "4 Seldom", "5 Never")
par(mfrow=c(2,2))
barplot(tonguecounts, main="Frequency of Tongues", horiz=FALSE, col='red', ylim = c(0,200),
         names.arg=labs)
dev.off()

library(ggplot2)
# Basic barplot
p <- ggplot(data=data, aes(x=qi2d)) +
  geom_bar()  
p 

#produce crosstab 
data %>%
  group_by(cohort5cat, qi2d) %>%
  tally() %>%
  spread(cohort5cat, n)