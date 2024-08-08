# Main Team Project Script
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Date: Aug. 12, 2024
# to run: set wd to main repo page (Solivana_DS1_Final folder)

#initialize libaries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(misty)
library(mice)
library(glue)

#load functions
source("functions.R")

#import the data
sleep_data <- read.csv("datasets/project_data.csv", header = T)

names(sleep_data)

#### Data Cleaning ####

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

#convert categorical data to factors
cols <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression",  "Corticoid", "Berlin.Sleepiness.Scale")

#convert the specified columns to factors
sleep_data[cols] <- lapply(sleep_data[cols], as.factor)

#look at the unique values in each categorical field
# make sure there is nothing there that shouldnt be there
apply(sleep_data[cols], 2, unique)

#Scale goes from 0-24, max should not be 26
summary(sleep_data$Epworth.Sleepiness.Scale)
#remove any values greater than 24
sleep_data$Epworth.Sleepiness.Scale[sleep_data$Epworth.Sleepiness.Scale > 24]<- NA

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

#### Prevalence of sleep disturbance ####
# to estimate the prevalence, identify cutoffs in the data
# identify percentage of patients that have sleep disturbance according to each metric

#create a binary metric and add to the table based on cutoff values
prevalence <- sleep_data %>% mutate(
  ESS = case_when(
    Epworth.Sleepiness.Scale > 10 ~ 1,
    Epworth.Sleepiness.Scale <= 10 ~ 0
  ),
  AthensSS = case_when(
    Athens.Insomnia.Scale > 5.5 ~ 1,
    Athens.Insomnia.Scale <= 5.5 ~ 0
  ),
  BSS = as.integer(Berlin.Sleepiness.Scale)
) %>% 
  select(ESS, BSS, AthensSS)

#determine the prevalence

#create empty data frame
prevalences <- data.frame()
rownames(prevalences) <- NULL

#loop through the metrics, summarize them and append to empty df


for (name in names(prevalence)) {
  prevalence_temp <- prevalence %>%
    select(all_of(name)) %>% 
    na.omit()
  
  prevalence_temp <- prevalence_temp %>%
    summarise(insomnia = sum(prevalence_temp[[all_of(name)]]),
              total = n(),
              percent = insomnia/total*100)
  
  prevalences <- base::rbind(prevalences, prevalence_temp)
  
}

#set the row names of the df
row.names(prevalences) <- names(prevalence)
prevalences

#### Imputation ####
# we need to do something to deal with the missing data
# single imputation

#we cant do it all at once, since some data are binary, some are unordered and some are cont. 
#impute continuous data with linear regression + stochastic, logistic regression + stochasticity for binary
imputed_sleep_data <- mice(sleep_data,
                           defaultMethod = c("norm.nob", "logreg", "polyreg", "polyr"),
                           m = 1,
                           print = FALSE,
                           maxit = 1,
                           seed = 10)


#check one of the imputed datasets to ensure there are no missing values
imputed_sleep_data <- complete(imputed_sleep_data, action = 1)

#NO NAs, negative values to be discussed in the discussion section
summary(imputed_sleep_data)

#===============Creating Models===================#

#Create models that predict sleep disturbance based on predictor variables
# Research which variables are effective predictors of each model

##### Linear regression model for ESS######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:

#ask which dataset to use (imputed vs not)
max_predictors_ESS <- as.integer(length(sleep_data$Epworth.Sleepiness.Scale)/15)

#initial model with all predictors, we will research and figure out what to include/not to include
ESS_model <-lm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid,
               data = imputed_sleep_data)

stepAIC(ESS_model, direction = "both")


summary(ESS_model)

ESS_predictions <- fitted(ESS_model)

#plot against each of the predictors, hold all other variables the same, show plots to show how the predictor influences the response

##### Logistic regression model for BSS ######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_BSS <- as.integer(sleep_data %>%  count(Berlin.Sleepiness.Scale) %>% filter(Berlin.Sleepiness.Scale == 1) %>% pull(n)/15)


#initial model with all predictors, we will research and figure out what to include/not to include
BSS_model <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid,
                 family = "binomial",
                 data = imputed_sleep_data)


#Summary
summary(BSS_model)

##### Linear regression model for AthensSS ######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_AthensSS <- as.integer(length(sleep_data$Athens.Insomnia.Scale)/15)

#initial model with all predictors, we will research and figure out what to include/not to include
AthensSS_model <-lm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction+ Any.fibrosis + Renal.Failure + Depression + Corticoid,
               data = imputed_sleep_data)

summary(AthensSS_model)

#### Models for PCS and MCS ####

# use lm of AIS, BSS, ESS to predict PCS/MCS
PCS_model <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Berlin.Sleepiness.Scale + Athens.Insomnia.Scale,
                data = imputed_sleep_data)

summary(PCS_model)


MCS_model <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Berlin.Sleepiness.Scale + Athens.Insomnia.Scale,
                data = imputed_sleep_data)

summary(PCS_model)

#Create a dataset with relevant metrics for the analysis
Q4_sleep_data <- imputed_sleep_data %>% 
  select(SF36.MCS, SF36.MCS, Epworth.Sleepiness.Scale, Berlin.Sleepiness.Scale, Athens.Insomnia.Scale)


### Analysis: plot fitted values, compared to original values, plot fitted compared to each predictor

# Create new data: for each predictor, set all other predictors to the mean/reference

#### Analysis for ESS_model ####
Age_data <- generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "Age")

ggplot(data = Age_data, mapping = aes(x = Age, y = fit))+
  geom_smooth(method = lm, se = TRUE)

