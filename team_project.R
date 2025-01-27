# Main Team Project Script
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Date: Aug. 12, 2024
# to run: set wd to main repo page (Solivana_DS1_Final folder)

eda <- function(sleep_data){
  #summarize mean age by gender
  sleep_data.age <- sleep_data %>% 
    group_by(Gender) %>%
    summarise(mean_age = mean(Age, na.rm = T))
  
  ggplot(data = sleep_data.age, mapping = aes(x = Gender, y= mean_age))+
    geom_col()
  
  #summarize BMI by gender
  sleep_data.BMI <- sleep_data %>% 
    group_by(Gender) %>%
    summarise(mean_BMI = mean(BMI, na.rm = T))
  
  ggplot(data = sleep_data.BMI, mapping = aes(x = Gender, y= mean_BMI))+
    geom_col()
}

#initialize libaries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(misty)
library(mice)

#import the data
sleep_data <- read.csv("datasets/project_data.csv", header = T)

names(sleep_data)

#clean the data

#select relevant columns
sleep_data <- sleep_data %>% 
  select(Subject,
         Gender,
         Age,
         BMI,
         Time.from.transplant,
         Liver.Diagnosis,
         Recurrence.of.disease,
         Rejection.graft.dysfunction,
         Any.fibrosis,
         Renal.Failure,
         Depression,
         Corticoid,
         Epworth.Sleepiness.Scale,
         Pittsburgh.Sleep.Quality.Index.Score,
         Athens.Insomnia.Scale,
         Berlin.Sleepiness.Scale,
         SF36.PCS,
         SF36.MCS)

#Quick glimplse of the data
summary(sleep_data)

#identify cutoffs for sleepiness scales, so we can binarize and fit glm models to predict yes or no outcomes

# Athens cutoff - 5.5 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7730071/#:~:text=A%20sum%20score%20is%20calculated,or%20more)%20%5B8%5D.

#ESS cutoff - 10 from midterm instructions

#BSS is alrealdy binary

#add columns and assign binary input
sleep_data <- sleep_data %>% 
  mutate(
    ESS = case_when(
    Epworth.Sleepiness.Scale > 10 ~ 1,
    Epworth.Sleepiness.Scale <= 10 ~ 0
    ),
    AthensSS = case_when(
      Athens.Insomnia.Scale > 5.5 ~ 1,
      Athens.Insomnia.Scale <= 5.5 ~ 0
    )
  )

#convert categorical data to factors
cols <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression",  "Corticoid", "Berlin.Sleepiness.Scale", "ESS", "AthensSS")



#convert the specified columns to factors
sleep_data[cols] <- lapply(sleep_data[cols], as.factor)

#look at the unique values in each categorical field
# make sure there is nothing there that shouldnt be there
apply(sleep_data[cols], 2, unique)

#identify missingness in the data
tab <- apply(ifelse(is.na(sleep_data), "Missing", "Not Missing"), 2, table)

#proportion tables for missingness (True = not missing, false = missing)
lapply(tab, prop.table)

# PSS has 30% missingness, may make sense to exclude from the analysis
sleep_data <- sleep_data %>% 
  select(!Pittsburgh.Sleep.Quality.Index.Score)

#See if we can identify a correlation between missing data
# i.e if some data is missing are we likely to be missing other data?

#convert NAs to true and non-NAs to false
NA_matrix <- is.na(sleep_data)

#calculate correlation matrix and p-value matrix
cor_matrix <- Hmisc::rcorr(NA_matrix)

diag(cor_matrix$r) <- 0
diag(cor_matrix$P) <- 0

cor_matrix$r[NA] <- 0
cor_matrix$P[NA] <- 0


#plot a heatmap
# Note alot of NA values in rows where SD = 0, i.e all values are complete
corrplot(cor_matrix$r, method = "square", p.mat = cor_matrix$P, insig = "blank", type = "lower", sig.level = 0.05, na.label = " ", tl.cex = 0.5)

# it seems that alot of people who did not fill out one sleep quality/QOL score, also tended not to fill out the rest

#missing data pattern, doesnt seem to have monotone pattern
md.pattern(sleep_data, rotate.names = T)
fluxplot(sleep_data, labels = F)

#Description of relevant data
eda(sleep_data)


#===============Prevalence of sleep disturbance===================#
# to estimate the prevalence, identify cutoffs in the data
# identify percentage of patients that have sleep disturbance according to each metric


#===============Imputation===================#
# we need to do something to deal with the missing data
# multiple imputation

#we cant do it all at once, since some data are binary, some are unordered and some are cont. 
#impute continuous data with linear regression + stochastic, logistic regression for binary, and polytomous logistic regression for unordered factors
imputed_sleep_data <- mice(sleep_data,
                           defaultMethod = c("norm.nob", "logreg", "polyreg", "polyr"),
                           m = 20,
                           print = FALSE,
                           maxit = 5,
                           seed = 5)

#check one of the imputed datasets to ensure there are no missing values
first_imputation <- complete(imputed_sleep_data, action = 1)

#NO NAs, negative values to be discussed in the discussion section
summary(first_imputation)


#===============Creating Models===================#

#Create models that predict sleep disturbance based on predictor variables
# Research which variables are effective predictors of each model

#####Logistic regression model for ESS######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_ESS <- as.integer(sleep_data %>%  count(ESS) %>% filter(ESS == 1) %>% pull(n)/15)

#initial model with all predictors, we will research and figure out what to include/not to include
ESS_model <- with(imputed_sleep_data,
                  glm(ESS ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid, family = "binomial"))


#extract models from the mice object 
ESS_models <- getfit(ESS_model)
#calculate AICs for each model
ESS_AICs <- t(sapply(ESS_models, extractAIC))

#get the average AIC
ESS_mean_AIC <- mean(ESS_AICs[,2])

#summarize the models
summary(pool(ESS_model))

#####Logistic regression model for BSS######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_BSS <- as.integer(sleep_data %>%  count(Berlin.Sleepiness.Scale) %>% filter(Berlin.Sleepiness.Scale == 1) %>% pull(n)/15)

#initial model with all predictors, we will research and figure out what to include/not to include
BSS_model <- with(imputed_sleep_data,
                  glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid, family = "binomial"))


#extract models from the mice object 
BSS_models <- getfit(BSS_model)
#calculate AICs for each model
BSS_AICs <- t(sapply(BSS_models, extractAIC))

#get the average AIC
BSS_mean_AIC <- mean(BSS_AICs[,2])

#summarize the models
summary(pool(BSS_model))


#####Logistic regression model for AthensSS######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_AthensSS <- as.integer(sleep_data %>%  count(AthensSS) %>% filter(AthensSS == 1) %>% pull(n)/15)

#initial model with all predictors, we will research and figure out what to include/not to include
AthensSS_model <- with(imputed_sleep_data,
                  glm(AthensSS ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid, family = "binomial"))


#extract models from the mice object 
AthensSS_models <- getfit(AthensSS_model)
#calculate AICs for each model
AthensSS_AICs <- t(sapply(AthensSS_models, extractAIC))

#get the average AIC
AthensSS_mean_AIC <- mean(AthensSS_AICs[,2])

#summarize the models
summary(pool(AthensSS_model))


#===============Create Models for PCS and MCS===================#

# use lm of AIS, BSS, ESS to predict PCS/MCS
# Analysis: create an lm %>% effect_plot from jtools?


