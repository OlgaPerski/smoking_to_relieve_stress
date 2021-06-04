library(foreign)
library(tidyverse)
library(survey)
library(arsenal)
library(ggplot2)
library(interactions)
library(gtsummary)
library(here)
library(flextable)

variables <- c("weight0","xwave","cigsmok","qimw139_016","qmotiv3","trylyb","tq3","basecpd","numkid2","agez","sgz","sexz","tenure")
names(variables) <- c("weight0","wave","cigsmok","stress_motive","mtss","trylyb","tq3","cpd","children","age","sep","sex","tenure")

if(!file.exists(here("data","stress_relief_data.rds"))) {
  data <- read.spss(here("data","STS_dec_2020.sav"), to.data.frame=TRUE)
  data<-data %>%
    select(!!! variables)
  
  write_rds(data, path=here("data","stress_relief_data.rds"))
}

data <- read_rds(here("data","stress_relief_data.rds"))

#restrict sample to relevant waves and cigarette smokers at baseline

data_clean <- data %>%
  filter(wave>=139 & wave<=142) %>%
  filter(cigsmok == 1)

#recode variables

data_clean$mtss <- as.factor(data_clean$mtss)
data_clean$trylyb <- as.factor(data_clean$trylyb)
data_clean$sex <- droplevels(data_clean$sex)
data_clean$tq3 <- droplevels(data_clean$tq3)
data_clean$age <- droplevels(data_clean$age)

data_clean <- data_clean %>%
mutate(sep = forcats::fct_collapse(sep, "Higher" = c("AB", "C1"), "Lower" = c("C2", "D", "E")),
       tq3 = forcats::fct_collapse(tq3, "No" = c("0"), "Yes" = c("1", "2", "3", "4+")),
       tenure = forcats::fct_collapse(tenure, "Lower" = c("BELONGS TO HOUSING ASSOCIATION", "RENTED FROM LOCAL AUTHORITY"), 
                                      "Higher" = c("BEING BOUGHT ON A MORTGAGE", "OWNED OUTRIGHT BY HOUSEHOLD", 
                                                   "RENTED FROM A PRIVATE LANDLORD", "OTHER"),
                                      "NA" = "REFUSED"),
       trylyb = forcats::fct_collapse(trylyb, "4+" = c("4","5","6","8","10","12","15","20")))

data_clean$children <- factor(data_clean$children, levels = c("YES", "NO"), 
                                labels = c("Yes", "No"))

data_clean$mtss <- factor(data_clean$mtss, levels = c(0, 1), 
                              labels = c("Low", "High"))

data_clean$stress_motive <- factor(data_clean$stress_motive, levels = c("no It helps me cope with stress or anxiety", "It helps me cope with stress or anxiety"), 
                          labels = c("No", "Yes"))

#remove respondents with missing data on baseline variables (baseline analytic sample)

data_clean <- data_clean %>%
  filter(!is.na(mtss)) %>%
  filter(!is.na(trylyb)) %>%
  filter(!is.na(cpd)) %>%
  filter(!is.na(tenure)) %>%
  filter(tenure=="Higher"|tenure=="Lower")
  
data_clean$tenure <- droplevels(data_clean$tenure)

write.csv(data_clean, "/Users/olgaperski/Desktop/Post-Doc/Research Projects/Submitted/Smoking to relieve stress in STS - Submitted/Data & R Code/data/clean_data.csv", row.names = T)

#descriptives (baseline analytic sample)

desc.stat.unweighted <- data_clean %>%
  select(stress_motive, mtss, trylyb, tq3, cpd, children, age, sex, sep, tenure) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"), digits = all_continuous() ~ 2) %>%
  bold_labels()

weighted <- svydesign(ids = ~1, weights = ~weight0, data = data_clean)

desc.stat_weighted <- weighted %>%
  tbl_svysummary(include = c(stress_motive, mtss, trylyb, tq3, cpd, children, age, sex, sep, tenure),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), digits = all_continuous() ~ 2) %>%
  bold_labels()

#descriptives (follow-up sample)

data_fu <- data_clean %>%
  filter(!is.na(tq3))

desc.stat.fu.unweighted <- data_fu %>%
  select(stress_motive, mtss, trylyb, tq3, cpd, children, age, sex, sep, tenure) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"), digits = all_continuous() ~ 2) %>%
  bold_labels()

weighted_fu <- svydesign(ids = ~1, weights = ~weight0, data = data_fu)

desc.stat.fu.weighted <- weighted_fu %>%
  tbl_svysummary(include = c(stress_motive, mtss, trylyb, tq3, cpd, children, age, sex, sep, tenure),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), digits = all_continuous() ~ 2) %>%
  bold_labels()

#comparing baseline and follow-up samples

t.test(data_clean$cpd, data_fu$cpd, paired=FALSE)

data_clean <- data_clean %>%
  mutate(fu_sample = as_factor(ifelse(is.na(tq3), "No", "Yes")))

options(scipen = 999)

a <- chisq.test(table(data_clean$age, data_clean$fu_sample))
round(a$p.value,5)

b <- chisq.test(table(data_clean$sex, data_clean$fu_sample))
round(b$p.value,5)

c <- chisq.test(table(data_clean$sep, data_clean$fu_sample))
round(c$p.value,5)

d <- chisq.test(table(data_clean$children, data_clean$fu_sample))
round(d$p.value,5)

e <- chisq.test(table(data_clean$trylyb, data_clean$fu_sample))
round(e$p.value,5)

f <- chisq.test(table(data_clean$stress_motive, data_clean$fu_sample))
round(f$p.value,5)

g <- chisq.test(table(data_clean$mtss, data_clean$fu_sample))
round(g$p.value,5)

h <- chisq.test(table(data_clean$tenure, data_clean$fu_sample))
round(h$p.value,5)


# main analyses -----------------------------------------------------------

data_clean$stress_motive <- relevel(data_clean$stress_motive, ref="No")
data_clean$mtss <- relevel(data_clean$mtss, ref="Low")
data_clean$tq3 <- relevel(data_clean$tq3, ref="No")
data_clean$children <- relevel(data_clean$children, ref="No")
data_clean$sep <- relevel(data_clean$sep, ref="Higher")
data_clean$tenure <- relevel(data_clean$tenure, ref="Higher")
data_clean$sex <- relevel(data_clean$sex, ref="Women")

data_fu$stress_motive <- relevel(data_fu$stress_motive, ref="No")
data_fu$mtss <- relevel(data_fu$mtss, ref="Low")
data_fu$tq3 <- relevel(data_fu$tq3, ref="No")
data_fu$children <- relevel(data_fu$children, ref="No")
data_fu$sep <- relevel(data_fu$sep, ref="Higher")
data_fu$tenure <- relevel(data_fu$tenure, ref="Higher")
data_fu$sex <- relevel(data_fu$sex, ref="Women")

#RQ1

model1 <- glm(mtss ~  stress_motive,
              data = data_clean, family = binomial(link = 'logit'))

t1 <- tbl_regression(model1, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model2 <- glm(mtss ~ stress_motive + age + sex + sep + children + cpd + trylyb,
              data = data_clean, family = binomial(link = 'logit'))

t2 <- tbl_regression(model2, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#RQ2

model3 <- glm(tq3 ~  stress_motive,
              data = data_fu, family = binomial(link = 'logit'))

t3 <- tbl_regression(model3, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model4 <- glm(tq3 ~  stress_motive + age + sex + sep + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t4 <- tbl_regression(model4, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#RQ3

model5 <- glm(mtss ~  stress_motive*sep,
              data = data_fu, family = binomial(link = 'logit'))

t5 <- tbl_regression(model5, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model6 <- glm(mtss ~  stress_motive*sep + stress_motive + age + sex + sep + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t6 <- tbl_regression(model6, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model7 <- glm(tq3 ~  stress_motive*sep,
              data = data_fu, family = binomial(link = 'logit'))

t7 <- tbl_regression(model7, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model8 <- glm(tq3 ~  stress_motive*sep + stress_motive + age + sex + sep + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t8 <- tbl_regression(model8, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#sensitivity analyses

model2_sa <- glm(mtss ~  stress_motive + age + sex + tenure + children + cpd + trylyb,
              data = data_clean, family = binomial(link = 'logit'))

t2_sa <- tbl_regression(model2_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model4_sa <- glm(tq3 ~  stress_motive + age + sex + tenure + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t4_sa <- tbl_regression(model4_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model5_sa <- glm(mtss ~  stress_motive*tenure,
              data = data_fu, family = binomial(link = 'logit'))

t5_sa <- tbl_regression(model5_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model6_sa <- glm(mtss ~  stress_motive*tenure + stress_motive + age + sex + tenure + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t6_sa <- tbl_regression(model6_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model7_sa <- glm(tq3 ~  stress_motive*tenure,
              data = data_fu, family = binomial(link = 'logit'))

t7_sa <- tbl_regression(model7_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

model8_sa <- glm(tq3 ~  stress_motive*tenure + stress_motive + age + sex + sep + children + cpd + trylyb,
              data = data_fu, family = binomial(link = 'logit'))

t8_sa <- tbl_regression(model8_sa, exponentiate = T) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#smoking to relieve stress by SEP

a<-chisq.test(table(data_clean$stress_motive, data_clean$sep))
round(a$p.value,5)

table(data_clean$stress_motive, data_clean$sep)

binom.test(293,678) #lower SEP
binom.test(185,457) #higher SEP