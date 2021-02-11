library(tidyverse)

oeko <- read.csv("http://staff.pubhealth.ku.dk/~sr/BasicStatistics/datasets/oeko.txt", sep="")
oeko <- as_tibble(oeko)

# 1
oeko <- oeko %>%
  mutate(
    abstid = as_factor(abstid),
    logkonc = log10(konc)
  )
  
ggplot(oeko, aes(konclog)) +
  geom_histogram()

# 2a

sumStat <- function(x){
  mean=mean(x)
  median=median(x)
  Q1=quantile(x, probs=.25)
  Q3=quantile(x, probs=.75)
  SD=sd(x)
  min=min(x)
  max=max(x)
  samlet <- data.frame(mean,median,SD, Q1,Q3,min,max )
  row.names(samlet) <- NULL
  return( samlet ) 
}

sumStatLog <- function(x){
  logmean=mean(log10(x))
  logSD=sd(log10(x))
  logulimit=logmean + 2 * logSD/sqrt(length(x))
  logllimit=logmean - 2 * logSD/sqrt(length(x))
  mean = 10^logmean
  ulimit = 10^logulimit
  llimit = 10^logllimit
  samlet <- data.frame(mean,llimit,ulimit)
  row.names(samlet) <- NULL
  return( samlet )
}


do.call('rbind', tapply( oeko$konc, oeko$sas_ansat, sumStatLog))

# 2b
# Forskel? Ja


t.test( konclog ~ sas_ansat, data=oeko , var = T)


# 3
do.call('rbind', tapply( oeko$konc, oeko$abstid, sumStatLog))

oeko %>%
  group_by(abstid, sas_ansat) %>%
  summarize(
    mean = mean(konc)
    )

oeko %>%
  group_by(abstid, sas_ansat) %>%
  summarize(
    mean = 10 ^ mean(log10(konc)),
    lower = 10 ^ (mean(log10(konc)) - 2 * sd(log10(konc)) / sqrt(n())),
    upper = 10 ^ (mean(log10(konc)) + 2 * sd(log10(konc)) / sqrt(n()))
  )

oeko %>%
  ggplot(aes(abstid, konc)) +
  geom_boxplot() +
  facet_wrap(~ sas_ansat)

oeko %>%
  ggplot(aes(abstid)) +
  geom_histogram(stat = 'count') +
  facet_wrap(~ sas_ansat)

# 4
anova2 = lm(logkonc ~ sas_ansat + abstid, data=oeko)
anova(anova2)
sum <-  summary(model.twoway)
10^sum$coefficients
10^confint(anova2)

# interaktion
anova3 = lm(logkonc ~ sas_ansat * abstid, data=oeko)
anova(anova3)
summary(anova3)
confint(anova3)
